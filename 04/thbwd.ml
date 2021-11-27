open Assert

let prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/04/thbwd/"

let simple_tests = 
  [ (prefix ^ "nested_return_array.oat", "", "0")
  ; (prefix ^ "nested_return_bool.oat", "", "0")
  ]

let tests = (Gradedtests.executed_oat_file simple_tests)
