describe("bdc", {
  describe('query.filter.add', {
    api_user_profile = "{\"queryTemplate\":\"{}\"}"
    api_results = function(uuid, query) jsonlite::toJSON(list(
      results = list(
        phenotypes = list(
          `\\phs000001\\unit_test\\test_variable_1\\` = list(
            min = 0,
            max = 5,
            categorical = FALSE,
            name = "\\phs000001\\unit_test\\test_variable_1\\"
          ),
          `\\phs000002\\unit_test\\test_variable_2\\` = list(
            min = 3,
            max = 9,
            categorical = FALSE,
            name = "\\phs000002\\unit_test\\test_variable_2\\"
          )
        )
      )
    ), auto_unbox=TRUE)
    connection = MockConnection$new(api_user_profile, api_results)
    resource = PicSureHpdsResourceConnectionBdc$new(connection, resource_uuid_list[[1]])
    newQuery = function() {
      query = resource$query()
      class(query) <- 'Hpds_Query'
      return(query)
    }

    path = "\\phs000001\\unit_test\\test_variable_1\\"
    it('should return error message when min is specified without max', {
      query = newQuery()
      expect_message(bdc::query.filter.add(query, path, min=1))
    })
    it('should return error message when max is specified without min', {
      query = newQuery()

      expect_message(bdc::query.filter.add(query, path, max=1))
    })
    it('should not give an error message when min and max are given', {
      query = newQuery()

      expect_no_message(bdc::query.filter.add(query, path, min=1, max=5))
    })
    it('should output error message matching snapshot when max is specified without min', {
      query = newQuery()

      expect_snapshot(bdc::query.filter.add(query, path, max=8))
    })
    it('should output error message matching snapshot when min is specified without max', {
      query = newQuery()

      expect_snapshot(bdc::query.filter.add(query, path, min=1))
    })
    it('should output error message with a list of ranges from dictionary using the list of keys provided', {
      query = newQuery()

      expect_snapshot(bdc::query.filter.add(query, list(path, "\\phs000002\\unit_test\\test_variable_2\\"), min=1))
    })
    it('should not give an error message when using one unnamed parameter, assumed to be minimum', {
      query = newQuery()

      expect_no_message(bdc::query.filter.add(query, path, 1))
    })
  })
})