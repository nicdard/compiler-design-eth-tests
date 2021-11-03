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

let nicdard_tests = gep_tests
  @ local_names
  @ return_tests
