open Assert

let prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/03/haenniro/"

let my_tests =
(* Warning 1: The first test works only on Linux as it contains raw assembly with invalid labels. To make it work on mac, change the labels in stacktest_asm.s to have an underscore in front of them.*)
(* Warning 2: The first test checks, whether your implementation can handle begin called from both an aligned stack (16-Bytes) as well as from an UNaligned stack. This is something that should never happen in practice and therefore a correct implementation could fail this test. *)
  [ [prefix ^ "stacktest_asm.s"; prefix ^ "stacktest.c"], prefix ^ "stacktest_ll.ll", [], 0L
  ; [prefix ^ "arg_loc_test.c"], prefix ^ "arg_loc_test.ll", [], 1L
  ]
