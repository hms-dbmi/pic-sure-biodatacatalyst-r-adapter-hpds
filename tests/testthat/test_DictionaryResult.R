expect_class = function(obj, type) expect_equal(class(obj)[[1]], type)
describe("PicSureHpdsDictionaryBdcResult", {
  api_results = list(
    searchResults = list(
      list(
        result = list(
          metadata = list(
            derived_var_id = 'phv00000001.v1',
            derived_var_name = 'test_variable_cat',
            derived_var_description = 'This is a categorical test variable',
            columnmeta_data_type = 'categorical',
            derived_group_id = 'categorical group id',
            derived_group_name = 'categorical group name',
            derived_group_description = 'categorical group description',
            derived_study_id = 'categorical study id',
            derived_study_description = 'categorical study description',
            is_stigmatized = FALSE,
            columnmeta_HPDS_PATH = '\\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\',
            values = list(ABC="ABC", DEF="DEF", GHI="GHI")
          ),
          is_categorical = TRUE
        )
      ),
      list(
        result = list(
          metadata = list(
            derived_var_id = 'phv00000002.v1',
            derived_var_name = 'test_variable_cont',
            derived_var_description = 'This is a continuous test variable',
            columnmeta_data_type = 'continuous',
            derived_group_id = 'continuous group id',
            derived_group_name = 'continuous group name',
            derived_group_description = 'continuous group description',
            derived_study_id = 'continuous study id',
            derived_study_description = 'continuous study description',
            is_stigmatized = FALSE,
            columnmeta_HPDS_PATH = '\\phs000002\\pht0000001\\phv00000001\\test_variable_cont\\',
            columnmeta_min = 1,
            columnmeta_max = 5,
            values = list()
          ),
          is_categorical = FALSE
        )
      )
    )
  )
  describe("count()", {
    it("should return the correct count of results within scope", {
      testScopes = c('phs000001')
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes)
      expect_equal(dictionaryResults$count(), 1)
    })
    it("should return 0 if no search results were found", {
      testScopes = c()
      api_results = list(searchResults = list())
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, scopes)
      expect_equal(dictionaryResults$count(), 0)
    })
  })
  describe("entries()", {
    it("should return correctly formatted categorical results", {
      testScopes = c('phs000001')
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes)
      expect_snapshot(dictionaryResults$entries())
    })
    it("should return correctly formatted continuous results", {
      testScopes = c('phs000002')
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes)
      expect_snapshot(dictionaryResults$entries())
    })
    it("should return an empty list if no search results were found", {
      testScopes = c('phs000002')
      api_results = list(searchResults = list())
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes)
      expect_equal(dictionaryResults$entries(), list())
    })
  })
  describe("varInfo()", {
    testScopes = c('phs000001', 'phs000002')
    it("should return a data.frame object", {
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes) 
      info = dictionaryResults$varInfo("\\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\")
      expect_class(info, 'data.frame')
    })
    it("should return correctly formatted variable information for matching path", {
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes) 
      info = dictionaryResults$varInfo("\\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\")
      expect_snapshot(info)
    })
    it("should return a note suggesting no results were found for invalid path", {
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes) 
      info = dictionaryResults$varInfo("bad_path")
      expect_class(info, 'character')
    })
  })
  describe("paths()", {
    testScopes = c('phs000001')
    it("should return all paths in scope", {
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes) 
      expect_snapshot(dictionaryResults$paths())
    })
  })
  describe("dataframe()", {
    testScopes = c('phs000001', 'phs000002')
    it("should return a data.frame object", {
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes) 
      df = dictionaryResults$dataframe()
      expect_class(df, 'data.frame')
    })
    it("should return correctly formatted results", {
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes) 
      expect_snapshot(dictionaryResults$dataframe())
    })
    it("should return an empty frame if no search results were found", {
      testScopes = c('phs000002')
      api_results = list(searchResults = list())
      dictionaryResults = PicSureHpdsDictionaryBdcResult$new(api_results, testScopes)
      expect_snapshot(dictionaryResults$dataframe())
    })
  })
})