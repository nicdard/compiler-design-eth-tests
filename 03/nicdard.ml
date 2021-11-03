open Assert

(* Indent a string, apply one or more \t at each line. *)
let indent (n:int) (s:string) =
  let indent_chars = (String.make n '\t') in
  String.split_on_char '\n' s 
  |> String.concat @@ "\n" ^ indent_chars
  |> (^) indent_chars 

(* Equality assertion which explain expected value on failure. *)
let expect_eq (s:'a -> string) (v1:'a) (v2:'a): assertion =
  fun () -> if v1 <> v2 
    then failwith @@ String.concat "\n" ["\nexpected: "; s v2 |> indent 1; "actual: "; s v1 |> indent 1] 
    else ()

(* Equality assertion which explain expected value on failure. *)
let expect_eqf (s:'a -> string) (f:unit -> 'a) (v2:'a): assertion =
  fun () -> let v1 = f () in
    if v1 <> v2 
      then failwith @@ String.concat "\n" ["\nexpected: "; s v2 |> indent 1; "actual: "; s v1 |> indent 1] 
      else ()

let executed tests =
  List.map (fun (fn, ans) ->
      fn, expect_eqf Int64.to_string (fun () -> Gradedtests.exec_e2e_file fn "") ans)
    tests

(* Add prefix to tests *)
let prefix = List.map (fun (fname, expected_result) -> 
  ("./compiler-design-eth-tests/03/nicdard/" ^ fname, expected_result))

let gep_tests = prefix
  [ "gep0.ll", 1L
  ]

let local_names = prefix
  [ "local_names.ll", 29L
  ]

let return_tests = prefix
  [ "return42_2.ll", 42L 
  ]

(* The following tests files where already included in the project material, but they were not run in gradedtests.ml. *)
let arithmetic_missing_tests = prefix
  [ "add_twice.ll", 29L
  ; "arith_combo.ll", 4L
  ; "arith_combo_dce.ll", 4L
  ; "arith_combo_fold.ll", 4L
  ]

let gep_missing_tests = prefix
  [ "gep9.ll", 5L
  ; "gep10.ll", 3L
  ]

let analysis_missing_tests = prefix 
  [ "analysis10_cf_opt.ll", 60L 
  ; "analysis10_dce_opt.ll", 60L 
  ; "analysis10.ll", 60L 
  ; "analysis11_cf_opt.ll", 3L 
  ; "analysis11_dce_opt.ll", 3L 
  ; "analysis11.ll", 3L 
  ; "analysis12_cf_opt.ll", 14L 
  ; "analysis12_dce_opt.ll", 14L 
  ; "analysis12.ll", 14L 
  ; "analysis13_cf_opt.ll", 7L
  ; "analysis13_dce_opt.ll", 7L
  ; "analysis13.ll", 7L 
  ; "analysis14_cf_opt.ll", 42L 
  ; "analysis14_dce_opt.ll", 42L 
  ; "analysis14.ll", 42L 
  ; "analysis15_cf_opt.ll", 2L 
  ; "analysis15_dce_opt.ll", 2L 
  ; "analysis15.ll", 2L
  ; "analysis16.ll", 1L
  ; "analysis18_cf_opt.ll", 42L 
  ; "analysis18_dce_opt.ll", 42L 
  ; "analysis18.ll", 42L 
  ; "analysis19_cf_opt.ll", 5L
  ; "analysis19_dce_opt.ll", 5L
  ; "analysis19.ll", 5L 
  ; "analysis1_cf_opt.ll", 49L 
  ; "analysis1.ll", 49L 
  ; "analysis2_cf_opt.ll", 8L 
  ; "analysis2.ll", 8L
  ; "analysis3_cf_opt.ll", 188L 
  ; "analysis3_dce_opt.ll", 188L 
  ; "analysis3.ll", 188L 
  ; "analysis4_cf_opt.ll", 254L 
  ; "analysis4.ll", 254L 
  ; "analysis5_cf_opt.ll", 14L 
  ; "analysis5_dce_opt.ll", 14L 
  ; "analysis5.ll", 14L 
  ; "analysis6_cf_opt.ll", 2L 
  ; "analysis6_dce_opt.ll", 2L  
  ; "analysis7.ll", 10L 
  ; "analysis8_cf_opt.ll", 95L 
  ; "analysis8_dce_opt.ll", 95L 
  ; "analysis8.ll", 95L  
  ; "analysis9.ll", 0L
  (* The following tests are rejected by clang due to ssa numbering order problems, 
     but are accepted by llinterp. *)
  ; "analysis16_cf_opt.ll", 1L
  ; "analysis16_dce_opt.ll", 1L
  ; "analysis1_dce_opt.ll", 49L
  ; "analysis4_dce_opt.ll", 254L
  ; "analysis7_cf_opt.ll", 10L
  ; "analysis7_dce_opt.ll", 10L
  ; "analysis9_cf_opt.ll", 0L
  ; "analysis9_dce_opt.ll", 0L
  (* The following tests do not run with the provided llinterp due to i1 invalid operand 
     (althought accepted by clang). *)
  ; "analysis2_dce_opt.ll", 8L
  ]


let nicdard_tests = gep_tests
  @ local_names
  @ return_tests
  @ analysis_missing_tests
  @ gep_missing_tests
  @ arithmetic_missing_tests
