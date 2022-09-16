# buildQuery(): should not add harmonized or topmed consent filters if a field does not have a harmonized or genotype path

    Code
      query$buildQuery()
    Output
      $query
      $query$fields
      $query$fields[[1]]
      [1] "\\phs000001\\unit_test\\test_variable\\"
      
      
      $query$crossCountFields
      list()
      
      $query$requiredFields
      list()
      
      $query$anyRecordOf
      list()
      
      $query$numericFilters
      list()
      
      $query$categoryFilters
      $query$categoryFilters$`\\_consents\\`
      $query$categoryFilters$`\\_consents\\`[[1]]
      [1] "phs000001.c1"
      
      $query$categoryFilters$`\\_consents\\`[[2]]
      [1] "phs000002.c1"
      
      
      
      $query$variantInfoFilters
      $query$variantInfoFilters[[1]]
      $query$variantInfoFilters[[1]]$categoryVariantInfoFilters
      list()
      
      $query$variantInfoFilters[[1]]$numericVariantInfoFilters
      list()
      
      
      
      $query$expectedResultType
      [1] "COUNT"
      
      
      $resourceUUID
      [1] "36363664-6231-6134-2d38-6538652d3131"
      

# buildQuery(): should add harmonized consent filters if a field has a harmonized path

    Code
      query$buildQuery()
    Output
      $query
      $query$fields
      $query$fields[[1]]
      [1] "\\DCC Harmonized data set\\unit_test\\test_variable\\"
      
      
      $query$crossCountFields
      list()
      
      $query$requiredFields
      list()
      
      $query$anyRecordOf
      list()
      
      $query$numericFilters
      list()
      
      $query$categoryFilters
      $query$categoryFilters$`\\_consents\\`
      $query$categoryFilters$`\\_consents\\`[[1]]
      [1] "phs000001.c1"
      
      $query$categoryFilters$`\\_consents\\`[[2]]
      [1] "phs000002.c1"
      
      
      $query$categoryFilters$`\\_harmonized_consent\\`
      [1] "phs000001.c1"
      
      
      $query$variantInfoFilters
      $query$variantInfoFilters[[1]]
      $query$variantInfoFilters[[1]]$categoryVariantInfoFilters
      list()
      
      $query$variantInfoFilters[[1]]$numericVariantInfoFilters
      list()
      
      
      
      $query$expectedResultType
      [1] "COUNT"
      
      
      $resourceUUID
      [1] "36363664-6231-6134-2d38-6538652d3131"
      

# buildQuery(): should add topmed consent filters if a field has a genotpye path

    Code
      query$buildQuery()
    Output
      $query
      $query$fields
      list()
      
      $query$crossCountFields
      list()
      
      $query$requiredFields
      list()
      
      $query$anyRecordOf
      list()
      
      $query$numericFilters
      list()
      
      $query$categoryFilters
      $query$categoryFilters$`\\_consents\\`
      $query$categoryFilters$`\\_consents\\`[[1]]
      [1] "phs000001.c1"
      
      $query$categoryFilters$`\\_consents\\`[[2]]
      [1] "phs000002.c1"
      
      
      $query$categoryFilters$`\\_topmed_consents\\`
      [1] "phs000001.c1"
      
      
      $query$variantInfoFilters
      $query$variantInfoFilters[[1]]
      $query$variantInfoFilters[[1]]$categoryVariantInfoFilters
      $query$variantInfoFilters[[1]]$categoryVariantInfoFilters$Gene_with_variant
      $query$variantInfoFilters[[1]]$categoryVariantInfoFilters$Gene_with_variant[[1]]
      [1] "CHD8"
      
      
      
      $query$variantInfoFilters[[1]]$numericVariantInfoFilters
      list()
      
      
      
      $query$expectedResultType
      [1] "COUNT"
      
      
      $resourceUUID
      [1] "36363664-6231-6134-2d38-6538652d3131"
      

