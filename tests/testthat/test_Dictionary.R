library(jsonlite)

describe("PicSureHpdsDictionaryBdc", {
  testProfile = jsonlite::toJSON(list(
    queryScopes = list("\\DCC Harmonized data set\\", "\\phs000001\\")
  ), auto_unbox=TRUE)
  loadDictionary = function(profile, search) {
    connection = MockConnection$new(profile, search)
    resource = PicSureHpdsResourceConnectionBdc$new(connection, resource_uuid_list[[1]])
    return(resource$dictionary())
  }
  describe("getKeyInfo()", {
    api_results = function(uuid, query) jsonlite::toJSON(list(
      results = list(
        phenotypes = list(
          `\\phs000001\\pht000001\\phv00000001\\test\\` = list(
            min = 0,
            max = 5,
            categorical = FALSE,
            name = "\\phs000001\\pht000001\\phv00000001\\test\\"
          )
        )
      )
    ), auto_unbox=TRUE)
    it("should return FALSE if path does not exist", {
      dictionary = loadDictionary(testProfile, api_results)
      result = dictionary$getKeyInfo("unknown_path")
      expect_class(result, 'logical')
    })
    it("should return a list of data if path exists", {
      dictionary = loadDictionary(testProfile, api_results)
      result = dictionary$getKeyInfo("\\phs000001\\pht000001\\phv00000001\\test\\")
      expect_class(result, 'list')
    })
    it("should return correctly formatted list of phenotype data", {
      dictionary = loadDictionary(testProfile, api_results)
      result = dictionary$getKeyInfo("\\phs000001\\pht000001\\phv00000001\\test\\")
      expect_snapshot(result)
    })
  })
  describe("genotypeAnnotations()", {
    api_results = function(uuid, query) jsonlite::toJSON(list(
      results = list(
        info = list(
          A_Variant = list(
            description = "Description=\"A proper testing description.\"",
            values = c("ABC", "DEF"),
            continuous = FALSE
          )
        )
      )
    ), auto_unbox=TRUE)
    it("should return a data.frame object", {
      dictionary = loadDictionary(testProfile, api_results)
      annotations = dictionary$genotypeAnnotations()
      expect_class(annotations, 'data.frame')
    })
    it("should return correctly formatted genomic annotations", {
      dictionary = loadDictionary(testProfile, api_results)
      annotations = dictionary$genotypeAnnotations()
      expect_snapshot(annotations)
    })
    it("should send api call with blank query", {
      api_results = function(uuid, query) {
        expect_snapshot(toString(query))
        return("{}")
      }
      dictionary = loadDictionary(testProfile, api_results)
      expect_success(dictionary$genotypeAnnotations())
    })
  })
  describe("find()", {
    it("should return a dictionary result object", {
      api_results = function(uuid, query) "{}"
      dictionary = loadDictionary(testProfile, api_results)
      capture.output(result <- dictionary$find('clouds'))
      expect_class(result, 'PicSureHpdsDictionaryBdcResult')
    })
    it("should send correctly formatted json api call", {
      api_results = function(uuid, query) {
        expect_snapshot(toString(query))
        return("{}")
      }
      dictionary = loadDictionary(testProfile, api_results)
      expect_success(capture.output(dictionary$find('clouds', 10)))
    })
  })
})