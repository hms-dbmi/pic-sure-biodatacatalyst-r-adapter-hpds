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
      
      $min
      [1] 0
      
      $max
      [1] 5
      
      $description
      [1] ""
      

# getKeyInfo(): should return correctly formatted list of genotype data

    Code
      result
    Output
      $name
      [1] "A_Variant"
      
      $HpdsDataType
      [1] "info"
      
      $categorical
      [1] TRUE
      
      $categoryValues
      $categoryValues[[1]]
      [1] "ABC"
      
      $categoryValues[[2]]
      [1] "DEF"
      
      
      $min
      [1] NA
      
      $max
      [1] NA
      
      $description
      [1] "A proper testing description."
      

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

