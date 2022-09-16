# query.filter.add: should output error message matching snapshot when max is specified without min

    Code
      bdc::query.filter.add(query, path, max = 8)
    Message <simpleMessage>
      Please specify a minimum value.
      
      Min and max ranges for keys provided:
      - \phs000001\unit_test\test_variable_1\ min: 0 max: 5

# query.filter.add: should output error message matching snapshot when min is specified without max

    Code
      bdc::query.filter.add(query, path, min = 1)
    Message <simpleMessage>
      Please specify a maximum value.
      
      Min and max ranges for keys provided:
      - \phs000001\unit_test\test_variable_1\ min: 0 max: 5

# query.filter.add: should output error message with a list of ranges from dictionary using the list of keys provided

    Code
      bdc::query.filter.add(query, list(path,
        "\\phs000002\\unit_test\\test_variable_2\\"), min = 1)
    Message <simpleMessage>
      Please specify a maximum value.
      
      Min and max ranges for keys provided:
      - \phs000001\unit_test\test_variable_1\ min: 0 max: 5
      - \phs000002\unit_test\test_variable_2\ min: 3 max: 9

