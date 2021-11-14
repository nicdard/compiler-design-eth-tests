open Assert

let prefix = "./compiler-design-eth-tests/04/dbernhard/"

let simple_tests = [
  (prefix ^ "simple_while.oat", "", "10")
  ; (prefix ^ "array_indexing.oat", "", "205") (* various indices *)
  ; (prefix ^ "length.oat", "asdf", "4") (* returns length of the first argument *)
  ; (prefix ^ "arr_of_string.oat", "abc", "98") (* returns second character as integer *)
  ; (prefix ^ "simple_global_update.oat", "", "11")
  ; (prefix ^ "simple_global_update2.oat", "", "11") (* update of a global variable *)
  ; (prefix ^ "advanced_while.oat", "", "5")
]

let dbernhard_tests = (Gradedtests.executed_oat_file simple_tests)
