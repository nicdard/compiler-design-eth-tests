open Assert

let hw4_prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/04/dbernhard/"
let hw5_prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/05/dbernhard/"

let incorrect_types = [
    hw5_prefix ^ "err_subtype.oat"
  ; hw5_prefix ^ "err_subtype2.oat"
  ; hw5_prefix ^ "err_subtype3.oat"
  ; hw5_prefix ^ "err_array_subtype.oat"
  ; hw5_prefix ^ "err_array_subtype2.oat"
  ; hw5_prefix ^ "err_branching.oat"
  ; hw5_prefix ^ "err_branching2.oat"
  ; hw5_prefix ^ "err_builtin.oat"
  ; hw5_prefix ^ "err_call_retvoid.oat"
  ; hw5_prefix ^ "err_cast.oat"
  ; hw5_prefix ^ "err_cast2.oat"
  ; hw5_prefix ^ "err_function_return_type.oat"
  ; hw5_prefix ^ "err_return.oat"
  ; hw5_prefix ^ "err_return2.oat"
  ; hw5_prefix ^ "err_return3.oat"
  ; hw5_prefix ^ "err_return4.oat"
  ; hw5_prefix ^ "err_return5.oat"
  ; hw5_prefix ^ "err_redefined.oat"
  ; hw5_prefix ^ "err_redefined2.oat"
  ; hw5_prefix ^ "err_redefined3.oat"
  ; hw5_prefix ^ "err_redefined4.oat"
  ; hw5_prefix ^ "err_redefined5.oat"
  ; hw5_prefix ^ "err_redefined6.oat"
  ; hw5_prefix ^ "err_redefined7.oat"
  ; hw5_prefix ^ "err_type_mismatch.oat"
]

let simple_tests = [
  (hw4_prefix ^ "simple_while.oat", "", "10")
  ; (hw4_prefix ^ "simple_while2.oat", "", "50") (* function return value as condition *)
  ; (hw4_prefix ^ "while_false.oat", "", "4")
  ; (hw4_prefix ^ "while_false2.oat", "", "4")
  ; (hw4_prefix ^ "array_indexing.oat", "", "205") (* various indices *)
  ; (hw4_prefix ^ "array_indexing2.oat", "", "105") (* 2d array *)
  ; (hw4_prefix ^ "length.oat", "asdf", "4") (* returns length of the first argument *)
  ; (hw4_prefix ^ "arr_of_string.oat", "abc", "98") (* returns second character as integer *)
  ; (hw4_prefix ^ "str_cat.oat", "", "hello42")
  ; (hw4_prefix ^ "str_of_arr.oat", "", "bcd0")
  ; (hw4_prefix ^ "print_bool.oat", "", "falsetruefalse5")
  ; (hw4_prefix ^ "simple_global_update.oat", "", "11")
  ; (hw4_prefix ^ "tests_if.oat", "", "1")
  ; (hw4_prefix ^ "tests_if2.oat", "", "1")
  ; (hw4_prefix ^ "tests_if3.oat", "", "110")
  ; (hw4_prefix ^ "tests_if5.oat", "", "60")
  ; (hw4_prefix ^ "tests_if6.oat", "", "65")
  ; (hw4_prefix ^ "tests_if7.oat", "", "53")
  ; (hw4_prefix ^ "tests_if8.oat", "", "50")
  ; (hw4_prefix ^ "advanced_add.oat", "", "4")
  ; (hw4_prefix ^ "for_cond_fun2.oat", "", "1, 3, 5, 7, 9, 11, b:50")
  ; (hw4_prefix ^ "empty.oat", "", "5")
  ; (hw4_prefix ^ "printing.oat", "", "hello42")
  ; (hw4_prefix ^ "update_global.oat", "", "12")
  ; (hw4_prefix ^ "argc.oat", "2", "20") (* character '2' is from amount of args and second character is the return code*)

  (* now come hw5 tests*)
  ; (hw5_prefix ^ "mat_mult.oat", "", "11 7 13 14 3 22 4 1 5 0")
  ; (hw5_prefix ^ "for_cond_fun.oat", "", "10,11,12,13,14,15,16,17,18,0")
  ; (hw5_prefix ^ "null_update_global2.oat", "", "12")
  ; (hw5_prefix ^ "equality.oat", "", "are equal0")
  ; (hw5_prefix ^ "non_equality.oat", "", "not equal0")
  ; (hw5_prefix ^ "count_primes_less_100.oat", "", "25")
  ; (hw5_prefix ^ "init_array_subtype.oat", "", "0")
  ; (hw5_prefix ^ "init_id.oat", "", "7")
  ; (hw5_prefix ^ "init_with_function.oat", "", "5")
  ; (hw5_prefix ^ "init_with_function2.oat", "", "10")
  ; (hw5_prefix ^ "runtime_ex_oob.oat", "", "Out of bounds index 4 for array length 31")
  ; (hw5_prefix ^ "sum_different_types.oat", "", "6")
]

let dbernhard_tests = (
    Gradedtests.executed_oat_file simple_tests 
  @ Gradedtests.typecheck_file_error incorrect_types
)
