expect_class = function(obj, type) expect_equal(class(obj)[[1]], type)
expect_no_message = function(method) expect_message(method, regex = NA)

resource_uuid_list = list(
  "36363664-6231-6134-2d38-6538652d3131",
  "70c837be-5ffc-11eb-ae93-0242ac130002"
)

MockConnection <- R6::R6Class(
  "MockConnection",
  lock_objects = FALSE,
  public = list(
    initialize = function(profile = '{}', search = function(uuid, query) '{}', resources = resource_uuid_list) {
      self$resource_uuids = resources
      self$mockProfile = profile
      self$mockSearch = search
    },
    INTERNAL_api_obj = function() list(
      profile = function() self$mockProfile,
      search = self$mockSearch
    )
  )
)