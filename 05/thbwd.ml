open Assert
open Gradedtests

let prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/05/thbwd/"

let tests = (
  executed_oat_file [
    ("thbwd/compile_struct_order.oat", "", "123412340");
    ("thbwd/polymorphic_array.oat", "", "1230");
  ] @
  typecheck_file_error [
    "thbwd/dead_code1.oat";
    "thbwd/dead_code2.oat";
    "thbwd/dead_code3.oat";
    "thbwd/assign_global_function.oat";
    "thbwd/array_init_shadowing.oat";
  ] @
  typecheck_file_correct [
    "thbwd/undead_code1.oat";
    "thbwd/undead_code2.oat";
  ]
)
