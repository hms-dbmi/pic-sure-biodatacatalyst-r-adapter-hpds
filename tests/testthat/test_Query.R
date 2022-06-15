library(jsonlite)

describe("PicSureHpdsQueryBdc", {
  uuid = resource_uuid_list[[1]]
  api_user_profile = jsonlite::toJSON(list(
    queryScopes = list("\\DCC Harmonized data set\\", "\\phs000001\\"),
    queryTemplate = jsonlite::toJSON(list(
      categoryFilters = list(
        `\\_consents\\` = list("phs000001.c1", "phs000002.c1"),
        `\\_harmonized_consent\\` = list("phs000001.c1"),
        `\\_topmed_consents\\` = list("phs000001.c1")
      )
    ), auto_unbox=TRUE)
  ), auto_unbox=TRUE)
  api_results = function(uuid, query) jsonlite::toJSON(list(
    results = list(
      phenotypes = list(
        `\\DCC Harmonized data set\\unit_test\\test_variable\\` = list(
          categoryValues = list('water', 'air', 'fire'),
          categorical = TRUE,
          name = "\\DCC Harmonized data set\\unit_test\\test_variable\\"
        ),
        `\\phs000001\\unit_test\\test_variable\\` = list(
          min = 0,
          max = 5,
          categorical = FALSE,
          name = "\\phs000001\\unit_test\\test_variable\\"
        )
      ),
      info = list(
        Gene_with_variant = list(
          description = "Description=\"The official symbol for a gene affected by a variant.\"",
          values = c("HTR6", "HTR7", "BBX", "RN7SL563P", "AL035696.3", "AL035696.2", "DHDH", "CHD8"),
          continuous = FALSE
        )
      )
    )
  ), auto_unbox=TRUE)

  describe("buildQuery()", {
    it("should not add harmonized or topmed consent filters if a field does not have a harmonized or genotype path", {
      connection = MockConnection$new(api_user_profile, api_results)
      resource = PicSureHpdsResourceConnectionBdc$new(connection, uuid)
      query = resource$query()

      query$select()$add(key="\\phs000001\\unit_test\\test_variable\\")
      expect_snapshot(query$buildQuery())
    })
    it("should add harmonized consent filters if a field has a harmonized path", {
      connection = MockConnection$new(api_user_profile, api_results)
      resource = PicSureHpdsResourceConnectionBdc$new(connection, uuid)
      query = resource$query()

      query$select()$add(key="\\DCC Harmonized data set\\unit_test\\test_variable\\")
      expect_snapshot(query$buildQuery())
    })
    it("should add topmed consent filters if a field has a genotpye path", {
      connection = MockConnection$new(api_user_profile, api_results)
      resource = PicSureHpdsResourceConnectionBdc$new(connection, uuid)
      query = resource$query()

      query$filter()$add("Gene_with_variant", "CHD8")
      expect_snapshot(query$buildQuery())
    })
  })
})