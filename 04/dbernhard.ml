open Assert

let prefix = "./compiler-design-eth-tests/04/dbernhard/"

let simple_tests = [
  (prefix ^ "simple_while.oat", "", "10");
]

let dbernhard_tests = (Gradedtests.executed_oat_file simple_tests)
