# getKeyInfo(): should return correctly formatted list of phenotype data

    Code
      result
    Output
      $name
      [1] "\\phs000001\\pht000001\\phv00000001\\test\\"
      
      $HpdsDataType
      [1] "phenotype"
      
      $categorical
      [1] FALSE
      
      $categoryValues
      list()
      
      $description
      [1] ""
      

# genotypeAnnotations(): should return correctly formatted genomic annotations

    Code
      annotations
    Output
        genomic_annotation                   description   values continuous
      1          A_Variant A proper testing description. ABC, DEF      FALSE

# genotypeAnnotations(): should send api call with blank query

    Code
      toString(query)
    Output
      [1] "{\"query\":\"\"}"

# find(): should send correctly formatted json api call

    Code
      toString(query)
    Output
      [1] "{\"query\":{\"searchTerm\":\"clouds\",\"includedTags\":[],\"excludedTags\":[],\"returnTags\":true,\"offset\":0,\"limit\":10}}"
