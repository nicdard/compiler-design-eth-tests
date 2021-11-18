open Assert

let prefix = "./compiler-design-eth-tests/04/dbernhard/"

let simple_tests = [
  (prefix ^ "simple_while.oat", "", "10")
  ; (prefix ^ "simple_while2.oat", "", "50") (* function return value as condition *)
  ; (prefix ^ "array_indexing.oat", "", "205") (* various indices *)
  ; (prefix ^ "length.oat", "asdf", "4") (* returns length of the first argument *)
  ; (prefix ^ "arr_of_string.oat", "abc", "98") (* returns second character as integer *)
  ; (prefix ^ "simple_global_update.oat", "", "11")
  ; (prefix ^ "simple_global_update2.oat", "", "11") (* update of a global variable *)
  ; (prefix ^ "tests_if.oat", "", "1")
  ; (prefix ^ "tests_if2.oat", "", "1")
  ; (prefix ^ "tests_if3.oat", "", "110")
  ; (prefix ^ "tests_if4.oat", "", "60")
  ; (prefix ^ "tests_if5.oat", "", "60")
  ; (prefix ^ "tests_if6.oat", "", "65")
  ; (prefix ^ "count_primes_less_100.oat", "", "25")
  ; (prefix ^ "for_cond_fun.oat", "", "10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 10")
  ; (prefix ^ "for_cond_fun2.oat", "", "1, 3, 5, 7, 9, 11, b:50")
  ; (prefix ^ "mat_mult.oat", "", "11 7 13 14 3 22 4 1 5 0")
  ; (prefix ^ "mat_mult2.oat", "", "11 7 13 14 3 22 4 1 5 0")
  ; (prefix ^ "null_update.oat", "", "12")
]

let dbernhard_tests = (Gradedtests.executed_oat_file simple_tests)
