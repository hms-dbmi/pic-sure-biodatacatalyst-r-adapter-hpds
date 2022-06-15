# entries(): should return correctly formatted categorical results

    Code
      dictionaryResults$entries()
    Output
      [[1]]
      [[1]]$var_name
      [1] "test_variable_cat"
      
      [[1]]$var_description
      [1] "This is a categorical test variable"
      
      [[1]]$data_type
      [1] "categorical"
      
      [[1]]$group_id
      [1] "categorical group id"
      
      [[1]]$group_name
      [1] "categorical group name"
      
      [[1]]$group_description
      [1] "categorical group description"
      
      [[1]]$study_id
      [1] "categorical study id"
      
      [[1]]$study_description
      [1] "categorical study description"
      
      [[1]]$is_stigmatized
      [1] FALSE
      
      [[1]]$HPDS_PATH
      [1] "\\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\"
      
      [[1]]$min
      [1] NA
      
      [[1]]$max
      [1] NA
      
      [[1]]$values
      [1] "ABC, DEF, GHI"
      
      

# entries(): should return correctly formatted continuous results

    Code
      dictionaryResults$entries()
    Output
      [[1]]
      [[1]]$var_name
      [1] "test_variable_cont"
      
      [[1]]$var_description
      [1] "This is a continuous test variable"
      
      [[1]]$data_type
      [1] "continuous"
      
      [[1]]$group_id
      [1] "continuous group id"
      
      [[1]]$group_name
      [1] "continuous group name"
      
      [[1]]$group_description
      [1] "continuous group description"
      
      [[1]]$study_id
      [1] "continuous study id"
      
      [[1]]$study_description
      [1] "continuous study description"
      
      [[1]]$is_stigmatized
      [1] FALSE
      
      [[1]]$HPDS_PATH
      [1] "\\phs000002\\pht0000001\\phv00000001\\test_variable_cont\\"
      
      [[1]]$min
      [1] 1
      
      [[1]]$max
      [1] 5
      
      [[1]]$values
      [1] ""
      
      

# varInfo(): should return correctly formatted variable information for matching path

    Code
      info
    Output
                        \\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\
      var_name                                                  test_variable_cat
      var_description                         This is a categorical test variable
      data_type                                                       categorical
      group_id                                               categorical group id
      group_name                                           categorical group name
      group_description                             categorical group description
      study_id                                               categorical study id
      study_description                             categorical study description
      is_stigmatized                                                        FALSE
      HPDS_PATH         \\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\
      min                                                                    <NA>
      max                                                                    <NA>
      values                                                        ABC, DEF, GHI

# paths(): should return all paths in scope

    Code
      dictionaryResults$paths()
    Output
      [1] "\\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\"

# dataframe(): should return correctly formatted results

    Code
      dictionaryResults$dataframe()
    Output
                  var_name                     var_description   data_type
      1  test_variable_cat This is a categorical test variable categorical
      2 test_variable_cont  This is a continuous test variable  continuous
                    group_id             group_name             group_description
      1 categorical group id categorical group name categorical group description
      2  continuous group id  continuous group name  continuous group description
                    study_id             study_description is_stigmatized
      1 categorical study id categorical study description          FALSE
      2  continuous study id  continuous study description          FALSE
                                                         HPDS_PATH min max
      1  \\phs000001\\pht0000001\\phv00000001\\test_variable_cat\\  NA  NA
      2 \\phs000002\\pht0000001\\phv00000001\\test_variable_cont\\   1   5
               values
      1 ABC, DEF, GHI
      2              

# dataframe(): should return an empty frame if no search results were found

    Code
      dictionaryResults$dataframe()
    Output
      data frame with 0 columns and 0 rows

