library(hash)
library(hpds)
library(stringr)

global_paths = list(
  consent = "\\_consents\\",
  harmonized = "\\_harmonized_consent\\",
  topmed = "\\_topmed_consents\\",
  dcc_harmonized = "\\DCC Harmonized data set"
)

# ======================
#    ADAPTER CODE
# ======================

#' @title PicSureHpdsResourceConnectionBdc
#' @description 
#' R6 class that allows access to the data dictionary and query services of a selected HPDS-hosted resources on a PIC-SURE network.
#' 
#' For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access the dictionary and query services via the objects it returns.
PicSureHpdsResourceConnectionBdc <- R6::R6Class(
  "PicSureHpdsResourceConnectionBdc",
  inherit = hpds::PicSureHpdsResourceConnection,
  portable = FALSE,
  lock_objects = FALSE,
  public = list(
    #' @description This method is used to create new object of this class which uses the passed PicSureConnection object for communication with the PIC-SURE network along with a UUID to identify a HPDS-hosted resource.
    #' @param connection A \code{\link{picsure::PicSureConnection}} PIC-SURE connection object.
    #' @param resource_uuid The UUID identity of a Resource hosted via the PIC-SURE connection.
    initialize = function(connection, resource_uuid) {
      self$connection_reference <- connection
      if (missing(resource_uuid)) {
        if (length(self$connection_reference$resource_uuids) > 1) {  
          print(self$connection_reference$resource_uuids)
          stop("ERROR: You must specify a valid Resource UUID")
        } else {
          self$resourceUUID <- self$connection_reference$resource_uuids[[1]]
        }
      } else {
        if (resource_uuid %in% self$connection_reference$resource_uuids) {
          self$resourceUUID <- resource_uuid
        } else {
          stop("ERROR: You must specify a valid Resource UUID")
        }
      }

      self$profile_info = jsonlite::fromJSON(connection$INTERNAL_api_obj()$profile())
      self$dict_instance <- PicSureHpdsDictionaryBdc$new(self)
    },
    #' @description This method returns a data frame of consents and their status as harmonized or topmed.
    #' @return A \code{\link{data.frame}} object containing consents and if they are topmed or harmonized.
    consents = function() {
      template <- if (is.null(self$profile_info$queryTemplate)) '{}' else self$profile_info$queryTemplate
      template <- jsonlite::fromJSON(template, simplifyVector=FALSE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
      consent_paths = template$categoryFilters[[global_paths$consent]]
      consent_table = list()
      for(consent in consent_paths) {
        consent_table[[(length(consent_table) + 1)]] = list(
          consent = consent,
          harmonized = consent %in% template$categoryFilters[[global_paths$harmonized]],
          topmed = consent %in% template$categoryFilters[[global_paths$topmed]]
        )
      }
      return(data.frame(do.call(rbind.data.frame, consent_table)))
    },
    #' @description This method returns a new \code{\link{bdc::PicSureHpdsQueryBdc}} object configured to run all commands against the previously specified HPDS-hosted resource.
    #' @return A \code{\link{bdc::PicSureHpdsQueryBdc}} object.
    query = function() {
      return(PicSureHpdsQueryBdc$new(self))
    }
  )
)

# ========================
#    DICTIONARY CODE
# ========================

#' @title PicSureHpdsDictionaryBdc
#' @description
#' R6 class that runs searches against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!
#' 
#' For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import stringr
#' @import hash
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
PicSureHpdsDictionaryBdc <- R6::R6Class(
  "PicSureHpdsDictionaryBdc",
  portable = FALSE,
  lock_objects = FALSE,
  private <- list(
    lazyLoadConcepts = function() {
      if(!is.null(self$concept_dictionary)) return()

      # Initial query is blank to get all phenotype and info/genotype data
      query = "{\"query\":\"\"}"
      results = self$INTERNAL_API_OBJ$search(self$connection$resourceUUID, query)
      results = jsonlite::fromJSON(results, simplifyVector=FALSE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)

      self$concept_dictionary = hash()
      self$annotations_paths = if (is.null(results$results$info)) c() else names(results$results$info)
      types = names(results$results)
      for (type_index in 1:length(results$results)){
        type = types[[type_index]]
        HpdsDataType = gsub("phenotypes", "phenotype", type)
        rows = results$results[[type_index]]
        row_names = names(rows)
        for (row_index in 1:length(rows)){
          row = rows[[row_index]]
          name = row_names[[row_index]]
          categorical = (isTRUE(row$categorical) | isFALSE(row$continuous))
          values = if (type == "phenotypes") row$categoryValues else row$values
          entry = list(
            name = name,
            HpdsDataType = HpdsDataType,
            categorical = categorical,
            categoryValues = if (categorical) values else list(),
            description = if (type != "info") "" else str_replace_all(str_replace(row$description, "Description=", ""), "^\"|\"$", "")
          )
          self$concept_dictionary[name] = entry
        }
      }
    }
  ),
  public = list(
    #' @description This method is used to create new PicSureHpdsDictionaryBdc object. DO NOT CREATE THIS OBJECT DIRECTLY!
    #' @param resourceConnection A \code{\link{bdc::PicSureHpdsResourceConnectionBdc}} object.
    initialize = function(resourceConnection) {
      self$connection <- resourceConnection
      self$resourceUUID <- resourceConnection$resourceUUID
      self$INTERNAL_API_OBJ <- resourceConnection$connection_reference$INTERNAL_api_obj()
      scopes = self$connection$profile_info$queryScopes
      private$included_studies <-  if (is.null(scopes)) c() else str_replace_all(scopes[str_detect(scopes, "^\\\\")], "\\\\", "")
    },
    #' @description Helper method to return key/path information from phenotype concept paths.
    #' @param key The concept path key to look up.
    #' @return Concept path information or FALSE.
    getKeyInfo = function(key) {
      private$lazyLoadConcepts()
      if (has.key(key, self$concept_dictionary)) {
        return(get(key, self$concept_dictionary)) 
      }
      return(FALSE)
    },
    #' @description Returns genotype annotations from info concept paths.
    #' @return a \code{\link{data.frame}} object containing a table of genotype annotations.
    genotypeAnnotations = function() {
      private$lazyLoadConcepts()
      concepts = self$annotations_paths
      annotations = list()
      for (concept_name in concepts) {
        concept = get(concept_name, self$concept_dictionary)
        annotations[[(length(annotations) + 1)]] = list(
          genomic_annotation = concept$name,
          description = concept$description,
          values = toString(concept$categoryValues),
          continuous = !concept$categorical
        )
      }
      return(data.frame(do.call(rbind.data.frame, annotations)))
    },
    #' @description This method returns a PicSureHpdsDictionaryBdcResult object containing the results of the search on the HPDS resource's data dictionary.
    #' @param term Term to find in the dictionary.
    #' @param limit Number of items to return.
    #' @param offset The offset record to start returning from.
    #' @param showAll Show all studies, not just ones within connection query scopes.
    #' @return A \code{\link{bdc::PicSureHpdsDictionaryBdcResult}} object containing results from the dictionary.
    find = function(term, limit = 0, offset = 0, showAll=FALSE) {
      print("Loading data dictionary... (takes a minute)")
      flush.console() # print loading message while we wait for json result and processing

      searchQuery <- jsonlite::toJSON(list(query = list(
        searchTerm = term,
        includedTags = list(),
        excludedTags = list(),
        returnTags = TRUE,
        offset = offset,
        limit = if(limit == 0) 10000 else limit
      )), auto_unbox=TRUE)
      results <- self$INTERNAL_API_OBJ$search(self$resourceUUID, searchQuery)
      results <- gsub("\\xef\\xbb\\xbf", "", results, useBytes = T) # strip BOM characters that are in the json data
      results <- jsonlite::fromJSON(results, simplifyVector=FALSE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
      
      return(PicSureHpdsDictionaryBdcResult$new(results$results, private$included_studies, showAll))
    }
  )
)


#' @title PicSureHpdsDictionaryBdcResult
#' @description
#' R6 class contain the results of a search against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!
#' 
#' For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import stringr
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
PicSureHpdsDictionaryBdcResult <- R6::R6Class(
  "PicSureHpdsDictionaryBdcResult",
  portable = FALSE,
  lock_objects = FALSE,
  private = list(
    concept_paths = c(),
    results = list(),
    projectAndFilterResults = function(results, scopes, showAll) {
      in_scope = function(study) Reduce(function(acc, scope) (acc | str_detect(study, fixed(scope))), scopes, init=FALSE)
      include_list <- c()
      paths <- c()
      
      if (length(results) < 1) return(list(results = list(), paths = paths))

      for (index in 1:length(results)) {
        result <- results[[index]]$result$metadata
        categorical = results[[index]]$result$is_categorical
        if (!(showAll | in_scope(result$columnmeta_HPDS_PATH))) next

        paths <- c(paths, result$columnmeta_HPDS_PATH)
        results[[index]] <- list(
          var_name = result$derived_var_name,
          var_description = result$derived_var_description,
          data_type = result$columnmeta_data_type,
          group_id = result$derived_group_id,
          group_name = result$derived_group_name,
          group_description = result$derived_group_description,
          study_id = result$derived_study_id,
          study_description = result$derived_study_description,
          is_stigmatized = result$is_stigmatized,
          HPDS_PATH = result$columnmeta_HPDS_PATH,
          min = if (categorical) NA else result$columnmeta_min,
          max = if (categorical) NA else result$columnmeta_max,
          values = toString(result$values)
        )
        include_list <- c(include_list, index)
      }
      return(list(results=results[include_list], paths=paths))
    }
  ),
  public = list(
    #' @description This method is used to create a PicSureHpdsDictionaryBdcResult object. DO NOT CREATE THIS OBJECT DIRECTLY!
    #' @param results A list of results to process and return.
    #' @param scopes A list of query scopes for filter by.
    #' @param showAll Show all studies, not just ones within connection query scopes.
    initialize = function(results, scopes, showAll = FALSE) {
      projected <- private$projectAndFilterResults(results$searchResults, scopes, showAll)
      private$concept_paths <- projected$paths
      private$results <- projected$results
    },
    #' @description This method returns a integer of how many terms were returned by the data dictionary search.
    #' @return Return the number of returned results.
    count = function() {
      return(length(private$results))
    },
    #' @description This method returns information about the terms discovered by the data dictionary search in a data frame format.
    #' @return Return all results.
    entries = function() {
      return(private$results)
    },
    #' @description Display all information available on a particular HPDS_PATH corresponding to a search result.
    #' @param path A path to look for in the results.
    #' @return A \code{\link{data.frame}} containing the fields for the given path.
    varInfo = function(path) {
      result <- private$results[[match(path, private$concept_paths)]]
      if (is.null(result)) return(paste0("No results found for '", path, "'"))

      info <- data.frame(unlist(result, use.names=FALSE), row.names=names(result))
      names(info) <- path
      return(info)
    },
    #' @description Extract the paths of all results into a list.
    #' @return A list of paths returned from the results.
    paths = function() {
      return(private$concept_paths)
    },
    #' @description Flatten the results into a \code{\link{data.frame}}.
    #' @return A \code{\link{data.frame}} of all results.
    dataframe = function() {
      return(data.frame(do.call(rbind.data.frame, private$results)))
    }
  )
)

# ===================
#     QUERY CODE
# ===================

#' @title PicSureHpdsQueryBdc
#' @description
#' R6 class used to build a multi-use query to search against a HPDS resource's data - DO NOT CREATE THIS OBJECT DIRECTLY!
#'  
#' For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import stringr
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
PicSureHpdsQueryBdc <- R6::R6Class(
  "PicSureHpdsQueryBdc",
  inherit = hpds::PicSureHpdsQuery,
  portable = FALSE,
  lock_objects = FALSE,
  private = list(
    harmonized_consents = c(),
    topmed_consents = c()
  ),
  public = list(
    #' @description This method is used to create new PicSureHpdsQueryBdc object. DO NOT CREATE THIS OBJECT DIRECTLY!
    #' @param connection A \code{\link{bdc::PicSureHpdsResourceConnectionBdc}} object.
    initialize = function(connection) {
      super$initialize(connection)
      template = jsonlite::fromJSON(connection$profile_info$queryTemplate)
      private$harmonized_consents = template$categoryFilters[[global_paths$harmonized]]
      private$topmed_consents = template$categoryFilters[[global_paths$topmed]]
    },
    #' @description Generate a query object and modify consents for harmonized and topmed concept paths.
    #' @param resultType The type of result that should be returned from the api.
    #' @return An object to use for the query.
    buildQuery = function(resultType="COUNT") {
      filters = self$listFilter$getQueryValues()
      query = list(
        fields = self$listSelect$getQueryValues(),
        crossCountFields = self$listCrossCounts$getQueryValues(),
        requiredFields = self$listRequire$getQueryValues(),
        anyRecordOf = self$listAnyOf$getQueryValues(),
        numericFilters = filters$numericFilters,
        categoryFilters = filters$categoryFilters,
        variantInfoFilters = list(filters$variantInfoFilters) # Hack to make jsonlite work correctly for variant info filters
      )

      keys = c(
        query$fields, 
        query$crossCountFields, 
        query$requiredFields,
        query$anyRecordOf,
        names(query$numericFilters),
        names(query$categoryFilters)
      )

      # see if query needs harmonized consents
      if (length(keys[str_detect(keys, global_paths$dcc_harmonized)]) > 0) {
        # add harmonized consents to filter
        query$categoryFilters[[global_paths$harmonized]] = private$harmonized_consents
      } else {
        # remove harmonized consents from filter
        query$categoryFilters[[global_paths$harmonized]] = NULL
      }

      # see if query needs topmed consents
      if(length(filters$variantInfoFilters$categoryVariantInfoFilters) > 0 || length(filters$variantInfoFilters$numericVariantInfoFilters) > 0) {
        # add topmed consents to filter
        query$categoryFilters[[global_paths$topmed]] = private$topmed_consents
      } else {
        # remove topmed consents from filter
        query$categoryFilters[[global_paths$topmed]] = NULL
      }

      search = list(query = query)
      if (!(isFALSE(self$resourceUUID))) {
        search[['resourceUUID']] <- self$resourceUUID
      }
      search$query[['expectedResultType']] <- resultType
      return(search)
    }
  )
)
