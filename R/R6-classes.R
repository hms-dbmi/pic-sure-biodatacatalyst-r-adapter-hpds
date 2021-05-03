library(hash)
library(hpds)


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
#'   \item{\code{query()}}{This method returns a new \code{PicSureHpdsQuery} object configured to run all commands against the previously specified HPDS-hosted resource.}}
PicSureHpdsResourceConnectionBdc <- R6::R6Class("PicSureHpdsResourceConnectionBdc",
                                             inherit = hpds::PicSureHpdsResourceConnection,
                                             portable = FALSE,
                                             lock_objects = FALSE,
                                             public = list(
                                               initialize = function(connection, resource_uuid) {
                                                 self$connection_reference <- connection
                                                 self$profile_info = jsonlite::fromJSON("{}")
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

                                                 # cache the profile information on startup
                                                 api = connection$INTERNAL_api_obj()
                                                 self$profile_info = jsonlite::fromJSON(api$profile())
                                                 # use singleton dictionary instance
                                                 print("Loading data dictionary... (takes a minute)")
                                                 flush.console()
                                                 self$dict_instance <- PicSureHpdsDictionary$new(self)
                                               },
                                               query = function(loadQuery=NA) {
                                                 if (is.na(loadQuery)) {
                                                   return(PicSureHpdsQueryBdc$new(self))
                                                 } else {
                                                   return(PicSureHpdsQueryBdc$new(self, loadQuery=loadQuery))
                                                 }
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
PicSureHpdsQueryBdc <- R6::R6Class("PicSureHpdsQueryBdc",
                                inherit = hpds::PicSureHpdsQuery,
                                portable = FALSE,
                                lock_objects = FALSE,
                                private = list(
                                  query_template = "",
                                  harmonized_path = "\\DCC Harmonized data set",
                                  consent_path_harmonized = "\\_harmonized_consent\\",
                                  consent_path_topmed = "\\_topmed_consents\\",
                                  harmonized_consents = c(),
                                  topmed_consents = c()
                                ),
                                public = list(
                                  initialize = function(connection) {
                                    super$initialize(connection)
                                    # Save the consents from the default queryTemplate
                                    private$query_template = jsonlite::fromJSON(connection$profile_info$queryTemplate)
                                    private$harmonized_consents = private$query_template$categoryFilters[[private$consent_path_harmonized]]
                                    private$topmed_consents = private$query_template$categoryFilters[[private$consent_path_topmed]]
                                  },
                                  buildQuery = function(resultType="COUNT") {
                                    ret <- jsonlite::fromJSON('{"query": {
                                                              "fields":[],
                                                              "crossCountFields":[],
                                                              "requiredFields":[],
                                                              "anyRecordOf": [],
                                                              "numericFilters":{},
                                                              "categoryFilters":{},
                                                              "variantInfoFilters": []
                                                              }}')
                                    ret$query$fields = self$listSelect$getQueryValues()
                                    ret$query$crossCountFields = self$listCrossCounts$getQueryValues()
                                    ret$query$requiredFields = self$listRequire$getQueryValues()
                                    ret$query$anyRecordOf = self$listAnyOf$getQueryValues()
                                    temp = self$listFilter$getQueryValues()
                                    ret$query$numericFilters = temp$numericFilters
                                    ret$query$categoryFilters = temp$categoryFilters
                                    # Hack to make jsonlite work correctly for variant info filters
                                    ret$query$variantInfoFilters = list(temp$variantInfoFilters)

                                    # see if query needs harmonized consents
                                    temp_name = c()
                                    temp_name = c(temp_name, ret$query$fields)
                                    temp_name = c(temp_name, ret$query$crossCountFields)
                                    temp_name = c(temp_name, ret$query$requiredFields)
                                    temp_name = c(temp_name, ret$query$anyRecordOf)
                                    temp_name = c(temp_name, names(ret$query$numericFilters))
                                    temp_name = c(temp_name, names(ret$query$categoryFilters))
                                    if (length(temp_name[str_detect(temp_name, private$harmonized_path)]) > 0) {
                                      # add harmonized consents to filter
                                      ret$query$categoryFilters[[private$consent_path_harmonized]] = private$harmonized_consents
                                    } else {
                                      # remove harmonized consents from filter
                                      ret$query$categoryFilters[[private$consent_path_harmonized]] = NULL
                                    }

                                    # see if query needs topmed consents
                                    if(length(ret$query$variantInfoFilters[[1]]$categoryVariantInfoFilters) > 0 || length(ret$query$variantInfoFilters[[1]]$numericVariantInfoFilters) > 0) {
                                      # add topmed consents to filter
                                      ret$query$categoryFilters[[private$consent_path_topmed]] = private$topmed_consents
                                    } else {
                                      # remove topmed consents from filter
                                      ret$query$categoryFilters[[private$consent_path_topmed]] = NULL
                                    }


                                    if (!(isFALSE(self$resourceUUID))) {
                                      ret[['resourceUUID']] <- self$resourceUUID
                                    }
                                    ret$query[['expectedResultType']] <- resultType
                                    return(ret)
                                  }
                                )
)
