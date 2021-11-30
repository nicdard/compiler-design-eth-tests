open Assert
open Gradedtests

let prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/05/thbwd/"

let tests = (
  executed_oat_file [
    (prefix ^ "compile_struct_order.oat", "", "123412340");
    (prefix ^ "polymorphic_array.oat", "", "1230");
  ] @
  typecheck_file_error [
    prefix ^ "dead_code1.oat";
    prefix ^ "dead_code2.oat";
    prefix ^ "illegal_for_stmt.oat";
    prefix ^ "assign_global_function.oat";
    prefix ^ "array_init_shadowing.oat";
  ] @
  typecheck_file_correct [
    prefix ^ "undead_code1.oat";
    prefix ^ "undead_code2.oat";
  ]
)
