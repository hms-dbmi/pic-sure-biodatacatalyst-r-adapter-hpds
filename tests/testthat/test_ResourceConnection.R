library(jsonlite)

describe("PicSureHpdsResourceConnectionBdc", {
  uuid = resource_uuid_list[[1]]
  describe("new()", {
    api_user_profile = "{}"
    it("should return an error if resource is not in valid returned resources", {
      connection = MockConnection$new(api_user_profile)
      expect_error(PicSureHpdsResourceConnectionBdc$new(connection, 'bad-uuid'))
    })
    it("should return an error if more than one resouce is returned from api and passed in uuid is blank", {
      connection = MockConnection$new(api_user_profile)
      expect_error(capture.output(PicSureHpdsResourceConnectionBdc$new(connection)))
    })
    it("should use the only resource available if no uuid is given", {
      connection = MockConnection$new(api_user_profile, resources = list(uuid))
      resource = PicSureHpdsResourceConnectionBdc$new(connection)
      expect_equal(resource$resourceUUID, uuid)
    })
  })
  describe("consents()", {
    generateProfile = function(consents, harmonized, topmed) jsonlite::toJSON(list(
      queryTemplate = jsonlite::toJSON(list(
        categoryFilters = list(
          `\\_consents\\` = consents,
          `\\_harmonized_consent\\` = harmonized,
          `\\_topmed_consents\\` = topmed
        )
      ))
    ))
    it("should return a data.frame object", {
      api_user_profile = generateProfile(c(), c(), c())
      connection = MockConnection$new(api_user_profile)

      resource = PicSureHpdsResourceConnectionBdc$new(connection, uuid)
      consents = resource$consents()

      expect_class(consents, 'data.frame')
    })
    it("should return a list of concents using api provided user profile query template", {
      consent = 'phs001001.c1'
      api_user_profile = generateProfile(c(consent), c(), c(consent))
      connection = MockConnection$new(api_user_profile)

      resource = PicSureHpdsResourceConnectionBdc$new(connection, uuid)
      consents = resource$consents()

      expect_snapshot(consents)
    })
  })
  describe("query()", {
    api_user_profile = "{ \"queryTemplate\": \"{}\" }"
    it("should return a query object", {
      connection = MockConnection$new(api_user_profile)
      resource = PicSureHpdsResourceConnectionBdc$new(connection, uuid)

      query = resource$query()
      
      expect_class(query, 'PicSureHpdsQueryBdc')
    })
  })
})