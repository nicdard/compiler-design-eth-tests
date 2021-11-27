
let prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/03/dbernhard/"

let dbernhard_tests = [
    prefix ^ "load_simple.ll", 23L
  ; prefix ^ "square.ll", 144L
  ; prefix ^ "square_ptr.ll", 144L
  ; prefix ^ "return_pointer.ll", 42L
  ; prefix ^ "bitcast_1.ll", 255L
  ; prefix ^ "bitcast_2.ll", 24L
  ; prefix ^ "swap_none.ll", 42L (* performs NO swap *)
  ; prefix ^ "swap_1.ll", 10L (* swaps once*)
  ; prefix ^ "swap_2.ll", 42L (* swaps twice (same as no swap)*)
  ; prefix ^ "arguments_1.ll", 100L (* tests if all 10 arguments are taken into account *)
  ; prefix ^ "arguments_2.ll", 55L (* unique 10 arguments get summed up *)
  ; prefix ^ "arguments_3.ll", 65L (* test if the last argument is correct *)
  ; prefix ^ "update_global_value.ll", 44L (* updating of a global variable *)
  ; prefix ^ "gep_inside_struct.ll", 42L (* index inside a struct which contains an array *)
  ; prefix ^ "gep_inside_struct_array.ll", 11L (* index inside an array which is inside a struct *)
  ; prefix ^ "gep_twice.ll", 199L
]
