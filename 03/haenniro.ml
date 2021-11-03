open Assert

let prefix_fn fn = "./compiler-design-eth-tests/03/haenniro/" ^ fn

let exec_e2e_ast ll_ast extra_files =
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::extra_files) exec_file in
  let result = Driver.run_executable "" exec_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in
  let _ = Platform.verb @@ Printf.sprintf "** Executable exited with: %d\n" result in
  Int64.of_int result


let exec_e2e_file path extra_files =
  let ast = Driver.parse_ll_file path in
  exec_e2e_ast ast extra_files

let executed tests =
  List.map (fun (fn, extra_files, ans) ->
      fn, assert_eqf (fun () -> exec_e2e_file (prefix_fn fn) (List.map prefix_fn extra_files)) ans)
    tests

let my_tests =
(* Warning 1: The first test works only on Linux as it contains raw assembly with invalid labels. To make it work on mac, change the labels in stacktest_asm.s to have an underscore in front of them.*)
(* Warning 2: The first test checks, whether your implementation can handle begin called from both an aligned stack (16-Bytes) as well as from an UNaligned stack. This is something that should never happen in practice and therefore a correct implementation could fail this test. *)
  [ "stacktest_ll.ll", ["stacktest_asm.s";"stacktest.c"], 0L 
  ; "arg_loc_test.ll", ["arg_loc_test.c"], 1L
  ] 
