open Assert

let global_prefix = Test_config.global_prefix ^ "./compiler-design-eth-tests/05/nicdard/"

let subtype_unit_tests =
  let failtest (c : bool) = (fun () -> if c then failwith "should not succeed" else ()) in
  let passtest (c : bool) = (fun () -> if c then () else  failwith "should not fail") in
  let s1 : (Ast.id * Ast.field list) = "s1", [ { fieldName = "x1"; ftyp = TInt } ; { fieldName = "x2"; ftyp = TBool } ] in
  let s2 : (Ast.id * Ast.field list) = "s2", [ { fieldName = "x1"; ftyp = TInt } ; { fieldName = "x2"; ftyp = TRef RString } ] in
  let s3 : (Ast.id * Ast.field list) = "s3", [ { fieldName = "x1"; ftyp = TInt } ] in
  let s4 : (Ast.id * Ast.field list) = "s4", [ { fieldName = "x1"; ftyp = TBool } ; { fieldName = "x2"; ftyp = TBool } ] in
  let s5 : (Ast.id * Ast.field list) = "s5", [ { fieldName = "x2"; ftyp = TBool } ; { fieldName = "x1"; ftyp = TInt } ] in
  let s6 : (Ast.id * Ast.field list) = "s6", [ { fieldName = "x2"; ftyp = TBool } ] in
  let struct_ctxt (name1, s1) (name2, s2) = Tctxt.add_struct (Tctxt.add_struct Tctxt.empty name2 s2) name1 s1 in
  [ "subtype_sub_int", passtest @@ Typechecker.subtype Tctxt.empty TInt TInt 
  ; "subtype_sub_bool", passtest @@ Typechecker.subtype Tctxt.empty TBool TBool
  ; "subtype_sub_subarray", passtest @@ Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TInt))
  ; "subtype_sub_nref", passtest @@ Typechecker.subtype Tctxt.empty (TNullRef (RArray TInt)) (TNullRef (RArray TInt))
  ; "subtype_sub_nrref", passtest @@ Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TNullRef (RArray TInt))
  ; "subtype_subrstruct_eq1", passtest @@ Typechecker.subtype (struct_ctxt s1 s2) (TRef (RStruct (fst s1))) (TRef (RStruct (fst s1)))
  ; "subtype_subrstruct_eq2", passtest @@ Typechecker.subtype (struct_ctxt s1 s2) (TRef (RStruct (fst s2))) (TRef (RStruct (fst s2)))
  ; "subtype_subrstruct_sub1", passtest @@ Typechecker.subtype (struct_ctxt s3 s2) (TRef (RStruct (fst s2))) (TRef (RStruct (fst s3)))
  ; "subtype_subrstruct_no_sub1", failtest @@ Typechecker.subtype (struct_ctxt s1 s6) (TRef (RStruct (fst s1))) (TRef (RStruct (fst s6)))
  ; "subtype_subrstruct_no_sub2", failtest @@ Typechecker.subtype (struct_ctxt s3 s4) (TRef (RStruct (fst s4))) (TRef (RStruct (fst s3)))
  ; "subtype_subrstruct_no_sub3", failtest @@ Typechecker.subtype (struct_ctxt s5 s4) (TRef (RStruct (fst s4))) (TRef (RStruct (fst s5)))
  ; "subtype_subrfunt_eq", passtest @@ Typechecker.subtype Tctxt.empty (TRef (RFun ([TInt], RetVal TInt))) (TRef (RFun ([TInt], RetVal TInt)))
  ; "subtype_subrfunt_controvariant_void", passtest @@ Typechecker.subtype Tctxt.empty (TRef (RFun ([TNullRef RString], RetVoid))) (TRef (RFun ([TRef RString], RetVoid)))
  ; "subtype_subrfunt_covariant_void", failtest @@ Typechecker.subtype Tctxt.empty (TRef (RFun ([TRef RString], RetVoid))) (TRef (RFun ([TNullRef RString], RetVoid)))
  ; "subtype_subrfunt_contro_covariant", passtest @@ Typechecker.subtype Tctxt.empty (TRef (RFun ([TNullRef RString], RetVal (TRef RString)))) (TRef (RFun ([TRef RString], RetVal (TNullRef RString))))
  ; "subtype_subrfunt_contro_contro", failtest @@ Typechecker.subtype Tctxt.empty (TRef (RFun ([TNullRef RString], RetVal (TNullRef RString)))) (TRef (RFun ([TRef RString], RetVal (TRef RString))))
  ]

let typecheck_ty_unit_tests =
  let passtest (f : assertion) = (fun () -> try f () with Typechecker.TypeError error -> failwith @@ "Type error: " ^ error) in
  let failtest (f : assertion) = (fun () -> try f (); failwith "Should have raised TypeError." with Typechecker.TypeError error -> ()) in
  [ "wf_typokokint", passtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) Tctxt.empty TInt)
  ; "wf_typokokbool", passtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) Tctxt.empty TBool)
  ; "wf_typokokreft", passtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) Tctxt.empty (TRef RString)) 
  ; "wf_typokokreftq", passtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) Tctxt.empty (TNullRef RString))
  ; "array_of_undef_struct", failtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) Tctxt.empty (TNullRef (RArray (TRef (RStruct "Color")))))
  ; "array_of_def_struct", passtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) (Tctxt.add_struct Tctxt.empty "Color" []) (TNullRef (RArray (TRef (RStruct "Color")))))
  ; "fun_undef_struct", failtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) Tctxt.empty (TNullRef (RFun ([ TRef (RStruct "Color"); TInt; TBool ], RetVoid))))
  ; "fun_def_struct", passtest @@ (fun () -> Typechecker.typecheck_ty (Ast.no_loc ()) (Tctxt.add_struct Tctxt.empty "Color" []) ((TNullRef (RFun ([ TRef (RStruct "Color"); TInt; TBool ], RetVal TInt)))))
  ]

type context_builder = Tctxt.t -> Ast.prog -> Tctxt.t
let tctxt path (builder : context_builder) =
  let () = Platform.verb @@ Printf.sprintf "** Processing: %s\n" path in
  let oat_ast = Driver.parse_oat_file path in
  try builder Tctxt.empty oat_ast
  with Typechecker.TypeError s -> failwith @@ "Should not fail: " ^ s

let context_builder_test builder tests =
  List.map (fun (p, ctxt) -> (p, assert_eq (tctxt p builder) ctxt)) tests

let prefix = List.map (fun (path, a) -> global_prefix ^ path, a)
let prefix3 = List.map (fun (path, a, b) -> global_prefix ^ path, a, b)

let tctxt_builder (structs : Tctxt.struct_ctxt) (globals : Tctxt.global_ctxt) (locals : Tctxt.local_ctxt): Tctxt.t = 
  { locals; globals; structs }

let typecheck_program_correct_ctxt_tests = prefix
  [ "ctxt1.oat", tctxt_builder [] [ "f", TRef (RFun ([], RetVoid)) ] []
  ; "ctxt2.oat", tctxt_builder [] [ "g", TRef (RFun ([], RetVal TInt)); "f", TRef (RFun ([], RetVoid)); ] []
  ; "ctxt3.oat", tctxt_builder [] [ "b", TRef (RFun ([], RetVal TInt)); "l", TRef (RFun ([TInt], RetVal TInt)); "g", TRef (RFun ([TRef (RFun ([TInt], RetVal TInt))], RetVal TInt)); ] []
  ]

let frontend_tests = prefix3
  [ "length.oat", "", "3"
  ; "arrayinitializer.oat", "", "9 10 11 9"
  ; "ifq.oat", "", "v is null0"
  ; "struct1.oat", "", "10"
  ; "struct2.oat", "", "4"
  ; "struct3.oat", "", "10"
  ; "struct4.oat", "", "2"
  ; "struct5.oat", "", "9"
  ; "global_struct1.oat", "", "10"
  ; "global_struct2.oat", "", "5"
  ]

let nicdard_tests = subtype_unit_tests
  @ typecheck_ty_unit_tests
  @ context_builder_test Typechecker.create_function_ctxt typecheck_program_correct_ctxt_tests
  @ Gradedtests.executed_oat_file frontend_tests
