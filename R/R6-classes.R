library(hash)
library(hpds)

global_paths = list(
  consent = "\\_consents\\",
  harmonized = "\\_harmonized_consent\\",
  topmed = "\\_topmed_consents\\",
  dcc_harmonized = "\\DCC Harmonized data set"
)

# ======================
#    ADAPTER CODE
# ======================

#' R6 class that allows access to the data dictionary and query services of a selected HPDS-hosted resources on a PIC-SURE network.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access the dictionary and query services via the objects it returns.
#' @format \code{\link{PicSureHpdsResourceConnection}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(connection, resource_uuid)}}{This method is used to create new object of this class which uses the passed PicSureConnection object for communication with the PIC-SURE network along with a UUID to identify a HPDS-hosted resource.}
#'
#'   \item{\code{dictionary()}}{This method returns a \code{PicSureHpdsDictionary} object which is used to run lookups against a resources data dictionary.}
#'   \item{\code{consents()}}{This method returns a data frame of consents and their status as harmonized or topmed. }
#'   \item{\code{query()}}{This method returns a new \code{PicSureHpdsQuery} object configured to run all commands against the previously specified HPDS-hosted resource.}}
PicSureHpdsResourceConnectionBdc <- R6::R6Class(
  "PicSureHpdsResourceConnectionBdc",
  inherit = hpds::PicSureHpdsResourceConnection,
  portable = FALSE,
  lock_objects = FALSE,
  public = list(
    initialize = function(connection, resource_uuid) {
      self$connection_reference <- connection
      if (missing(resource_uuid)) {
        if (length(self$connection_reference$self$resource_uuids) > 1) {
          print(self$connection_reference$self$resource_uuids)
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
    query = function() {
      return(PicSureHpdsQueryBdc$new(self))
    }
  )
)

# ========================
#    DICTIONARY CODE
# ========================

#' R6 class that runs searches against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{PicSureHpdsDictionary}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(refHpdsResourceConnection)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{find(term='', limit, offset, showAll)}}{This method returns a \code{PicSureHpdsDictionaryResult} object containing the results of the search on the HPDS resource's data dictionary.}}
PicSureHpdsDictionaryBdc <- R6::R6Class(
  "PicSureHpdsDictionaryBdc",
  portable = FALSE,
  lock_objects = FALSE,
  private <- list(
    # Populate a hash/dictionary of phenotype and genotype concepts
    lazyLoadConcepts = function() {
      if(!is.null(self$concept_dictionary)) return()

      # Initial query is blank to get all phenotype and info/genotype data
      query = jsonlite::toJSON(list(query=""), auto_unbox=TRUE)
      results = self$INTERNAL_API_OBJ$search(self$connection$resourceUUID, query)
      results = jsonlite::fromJSON(results, simplifyVector=FALSE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)

      self$concept_dictionary = hash()
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
          self$concept_dictionary[paste0(type, "_", name)] = entry
        }
      }
    }
  ),
  public = list(
    initialize = function(refHpdsResourceConnection, writeJson=FALSE) {
      self$connection <- refHpdsResourceConnection
      self$resourceUUID <- refHpdsResourceConnection$resourceUUID
      self$INTERNAL_API_OBJ <- refHpdsResourceConnection$connection_reference$INTERNAL_api_obj()
      scopes = self$connection$profile_info$queryScopes
      private$included_studies <-  if (is.null(scopes)) c() else str_replace_all(scopes[str_detect(scopes, "^\\\\")], "\\\\", "")
    },
    getKeyInfo = function(key) {
      private$lazyLoadConcepts()
      index = paste0("phenotypes_", key)
      if (has.key(index, self$concept_dictionary)) {
        return(get(index, self$concept_dictionary)) 
      }
      return(FALSE)
    },
    genotypeAnnotations = function() {
      private$lazyLoadConcepts()
      concepts = keys(self$concept_dictionary)
      concepts = concepts[sapply(concepts, function(concept) str_detect(concept, 'info_'))]
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
    find = function(term, limit, offset, showAll=FALSE) {
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


#' R6 class contain the results of a search against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{PicSureHpdsDictionaryResult}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(results)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{count()}}{This method returns a integer of how many terms were returned by the data dictionary search.}
#'   \item{\code{entries()}}{This method returns information about the terms discovered by the data dictionary search in a data frame format.}
#'   \item{\code{varInfo(path)}}{Display all information available on a particular HPDS_PATH corresponding to a search result.}
#'   \item{\code{listPaths()}}{Extract the paths of all results into a list.}
#'   \item{\code{dataframe()}}{Flatten the results into a dataframe.}
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
          values = result$values
        )
        include_list <- c(include_list, index)
      }
      return(list(results=results[include_list], paths=paths))
    }
  ),
  public = list(
    initialize = function(results, scopes, showAll = FALSE) {
      projected <- private$projectAndFilterResults(results$searchResults, scopes, showAll)
      private$concept_paths <- projected$paths
      private$results <- projected$results
    },
    count = function() {
      return(length(private$results))
    },
    entries = function() {
      return(private$results)
    },
    varInfo = function(path) {
      result <- private$results[[match(path, private$concept_paths)]]
      if (is.null(result)) return(paste0("No results found for '", path, "'"))

      info <- data.frame(unlist(result, use.names=FALSE), row.names=names(result))
      names(info) <- path
      return(info)
    },
    paths = function() {
      return(private$concept_paths)
    },
    dataframe = function() {
      return(data.frame(do.call(rbind.data.frame, private$results)))
    }
  )
)

# ===================
#     QUERY CODE
# ===================

#' R6 class used to build a multi-use query to search against a HPDS resource's data - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import stringr
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{PicSureHpdsQuery}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(connection)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{show()}}{This method displays a list of all settings specified for the query.}
#'   \item{\code{select()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{crosscounts()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{require()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{anyof()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{filter()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{getCount()}}{This method returns a count of how many records are discovered by the query.}
#'   \item{\code{getResults()}}{This method returns the records discovered by the query.}
#'   \item{\code{getResultsDataFrame()}}{This method returns the discovered records in a dataframe format.}
#'   \item{\code{getRunDetails()}}{This method returns information the performance of the query.}}
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
    initialize = function(connection) {
      super$initialize(connection)
      template = jsonlite::fromJSON(connection$profile_info$queryTemplate)
      private$harmonized_consents = template$categoryFilters[[global_paths$harmonized]]
      private$topmed_consents = template$categoryFilters[[global_paths$topmed]]
    },
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
