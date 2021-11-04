open Assert
open X86
open Simulator
open Asm

(* You can use this file for additional test cases to help your *)
(* implementation.                                              *)

(* some useful constructs *)
let redefinedsym_test (p:prog) () =
  try ignore (assemble p);
    failwith "Should have raised Redefined_sym"
  with
    | Redefined_sym _ -> ()
    | _ -> failwith "Should have raised Redefined_sym"

let prog_nothing = [ text "main"
    [ Retq, [] ]
]

let prog_int (n: int) = [ text "main"
    [ Movq, [~$n; ~%Rax] (* move immidiate to reg*)
    ; Retq, []
    ]
  ]

let prog_callq_ret (n: int) =
  [ text "main"
    [ Movq, [~$12; ~%Rax] (* move immidiate to reg*)
    ; Callq, [~$$"setblock" ]
    ; Retq, []
  ]
; text "setblock"
    [ Movq, [~$n; ~%Rax]
    ; Retq, []
    ]
]

let prog_double_symb = [ text "foo"
                  [ Xorq, [~%Rax; ~%Rax]
                  ; Movq, [~$100; ~%Rax]
                  ; Retq, []
                  ]
            ; text "main"
                  [ Xorq, [~%Rax; ~%Rax]
                  ; Movq, [Ind1 (Lbl "foo"); ~%Rax]
                  ; Retq, []
                  ]
            ;  data "foo"
                  [ Quad (Lit 99L)
                  ; Asciz "Hello, world!"
                  ]
            ]


let prog_placement =
  [ data "foo"
        [ Quad (Lit 42L)
        ; Quad (Lit 40L)
        ]
  ; text "main"
        [ Leaq, [Ind1 (Lbl "foo"); ~%Rax]
        ; Retq, []
        ]
  ]

let prog_placement_ind_lit =
  [ text "main"
        [ Leaq, [Ind1 (Lit 0x400001L); ~%Rax]
        ; Retq, []
        ]
  ]

let prog_leaq_ind2 =
  [ data "foo"
        [ Quad (Lit 42L)
        ; Quad (Lit 40L)
        ]
  ; text "main"
        [ Movq, [~$$"foo"; ~%Rax]
        ; Leaq, [Ind2 Rax; ~%Rax]
        ; Retq, [] (* should return mem_bot + 24 (or + 0x16) *)
        ]
  ]

let prog_shift = 
  [ text "main"
    [ Movq, [~$3; ~%Rcx]
    ; Movq, [~$233; ~%Rbx]
    ; Shrq, [~%Rcx; ~%Rbx]
    ; Movq, [~%Rbx; ~%Rax]
    ; Retq, []
  ]
]

let prog_mov_ind3 =
  [ data "foo"
        [ Quad (Lit 420L)
        ; Quad (Lit 39L)
        ]
  ; text "main"
        [ Movq, [(Imm (Lbl "foo")); ~%Rax]
        ; Movq, [(Ind3 (Lit 8L, Rax)); ~%Rax]
        ; Retq, []
        ]
  ]

let prog_dec_reg =
  [ data "foo"
        [ Quad (Lit 42L)
        ; Quad (Lit 40L)
        ]
  ; text "main"
        [ Movq, [~$12; ~%Rax] (* move immidiate to reg*)
        ; Decq, [~%Rax]
        ; Retq, []
        ]
  ]

(* Function prologue and epilogue. *)
let function_prologue : ins list =
  Asm.([ 
      Pushq, [~%Rbp]
    ; Movq, [~%Rsp; ~%Rbp]
  ])

let function_epilogue : ins list =
  Asm.([ 
      Popq, [~%Rbp]
    ; Retq, []
  ])

let empty_main = text "main" @@ function_prologue @ function_epilogue

(* A driver main, just call some other function *)
let main_driver = text "main" @@ function_prologue @ [  
  Callq, [ Imm (Lbl ("fun")) ]
] @ function_epilogue

let function_builder (name:string) (body:ins list) = 
  text name (function_prologue @ body @ function_epilogue)

let default_fun_builder = function_builder "fun" 

let negq = [ 
  default_fun_builder [ Movq, [ Ind1 (Lbl "data1"); ~%Rax ]
                      ; Negq, [ ~%Rax ]
                      ; Movq, [ ~%Rax; Ind1 (Lbl "data1") ] 
                      ] 
  ; data "data1" [ Quad(Lit 99L)
                 ; Asciz "Some other data" 
                 ]
]

let addq = [
  default_fun_builder [ Movq, [ ~$1; ~%Rax ]
                      ; Movq, [ ~$$"data1"; ~%R12 ]
                      ; Addq, [ Ind3 ( Lit 8L, R12 ); ~%Rax ] 
                      ]
  ; data "data1" [ Quad(Lit (-1L)) 
                 ; Quad(Lit 2L)
                 ; Quad(Lit Int64.max_int) 
                 ]
]

let fibonacci = [
  text "fibonacci" @@ function_prologue @ 
                   [ Movq, [ ~$1; ~%Rax ] (* Prepare the return value for base case. *)
                   ; Cmpq, [ ~$2; ~%Rdi ]
                   ; J Le, [ ~$$".exit" ]
                   ; Subq, [ ~$16; ~%Rsp ] (* Reserve stack space for recursive call argument and intermidiate result. *)
                   ; Subq, [ ~$2; ~%Rdi ]
                   ; Movq, [ ~%Rdi; Ind3 (Lit (-8L), Rbp) ] (* Save intermediate argument. *)
                   ; Addq, [ ~$1; ~%Rdi ] (* Prepare first recursive call argument. *)
                   ; Callq, [ ~$$"fibonacci" ] (* Recursively call with n - 1. *)
                   ; Movq, [ ~%Rax; Ind3 (Lit (-16L), Rbp) ] (* Store intermidiate result in stack. *)
                   ; Movq, [ Ind3 (Lit (-8L), Rbp); ~%Rdi ] (* Prepare second recursive call argument. *)
                   ; Callq, [ ~$$"fibonacci" ] (* Recursively call with n - 2. *)
                   ; Addq, [ Ind3 (Lit (-16L), Rbp); ~%Rax ] (* Prepare result. *)
                   ; Addq, [ ~$16; ~%Rsp ] (* Shrink the stack. *)
                   ]
  ; text ".exit" function_epilogue
  ; default_fun_builder [ Movq, [ ~$30; ~%Rdi ]
                        ; Callq, [ ~$$"fibonacci" ]
                        ]
]

let log (a: int64) = [
    text "log"
    [ Cmpq, [~$2; ~%Rdi]
    ; J Lt, [~$$"exit"]
    ; Pushq, [~%Rax]
    ; Movq, [~%Rdi; ~%Rax]
    ; Shrq, [~$63; ~%Rax]
    ; Addq, [~%Rdi; ~%Rax]
    ; Sarq, [~$1; ~%Rax]
    ; Movq, [~%Rax; ~%Rdi]
    ; Callq, [~$$"log"]
    ; Addq, [~$1; ~%Rax]
    ; Addq, [~$8; ~%Rsp]
    ; Retq, []];
    text "exit"
    [ Xorq, [~%Rax; ~%Rax]
    ; Retq, []];
    text "main"
    [ Pushq, [~%Rax]
    ; Movq, [Imm (Lit a); ~%Rdi]
    ; Callq, [~$$"log"]
    ; Popq, [~%Rcx]
    ; Retq, []];
  ]

(* Parse a string to an integer using different base (<=16)                    *)
(* Inspired by https://github.com/JuliaLang/Microbenchmarks/blob/master/perf.c *)
(* Assemble with Compiler Explorer                                             *)
(* Adapted by Zikai Liu                                                        *)
let parse_int (input: string) (base: int) = [
  text "_Z9parse_intPKcl" [
      Pushq, [ ~%Rbp ]
    ; Movq, [ ~%Rsp; ~%Rbp ]
    ; Movq, [ ~%Rdi; Ind3 (Lit (-8L), Rbp) ]
    ; Movq, [ ~%Rsi; Ind3 (Lit (-16L), Rbp) ]
    ; Movq, [ ~$0; Ind3 (Lit (-24L), Rbp) ]
  ]
  ; text ".LBB0_1" [
      Movq, [ Ind3 (Lit (-8L), Rbp); ~%Rax ]
    ; Movq, [ Ind2 Rax; ~%Rax ]
    ; Andq, [ ~$255; ~%Rax ]
    ; Cmpq, [ ~$0; ~%Rax ]
    ; J Eq, [ ~$$".LBB0_15" ]
    ; Movq, [ Ind3 (Lit (-8L), Rbp); ~%Rax ]
    ; Movq, [ Ind2 Rax; ~%Rax ]
    ; Andq, [ ~$255; ~%Rax ]
    ; Movq, [ ~%Rax; Ind3 (Lit (-32L), Rbp) ]
    ; Movq, [ ~$0; Ind3 (Lit (-40L), Rbp) ]
    ; Cmpq, [ ~$48; Ind3 (Lit (-32L), Rbp) ]
    ; J Lt, [ ~$$".LBB0_5" ]
    ; Cmpq, [ ~$57; Ind3 (Lit (-32L), Rbp) ]
    ; J Gt, [ ~$$".LBB0_5" ]
    ; Movq, [ Ind3 (Lit (-32L), Rbp); ~%Rax ]
    ; Subq, [ ~$48; ~%Rax ]
    ; Movq, [ ~%Rax; Ind3 (Lit (-40L), Rbp) ]
    ; Jmp, [ ~$$".LBB0_13" ]
  ]
  ; text ".LBB0_5" [
      Cmpq, [ ~$65; Ind3 (Lit (-32L), Rbp) ]
    ; J Lt, [ ~$$".LBB0_8" ]
    ; Cmpq, [ ~$90; Ind3 (Lit (-32L), Rbp) ]
    ; J Gt, [ ~$$".LBB0_8" ]
    ; Movq, [ Ind3 (Lit (-32L), Rbp); ~%Rax ]
    ; Subq, [ ~$65; ~%Rax ]
    ; Addq, [ ~$10; ~%Rax ]
    ; Movq, [ ~%Rax; Ind3 (Lit (-40L), Rbp) ]
    ; Jmp, [ ~$$".LBB0_12" ]
  ]
  ; text ".LBB0_8" [
      Cmpq, [ ~$97; Ind3 (Lit (-32L), Rbp) ]
    ; J Lt, [ ~$$".LBB0_11" ]
    ; Cmpq, [ ~$122; Ind3 (Lit (-32L), Rbp) ]
    ; J Gt, [ ~$$".LBB0_11" ]
    ; Movq, [ Ind3 (Lit (-32L), Rbp); ~%Rax ]
    ; Subq, [ ~$97; ~%Rax ]
    ; Addq, [ ~$10; ~%Rax ]
    ; Movq, [ ~%Rax; Ind3 (Lit (-40L), Rbp) ]
  ]
  ; text ".LBB0_11" [
      Jmp, [ ~$$".LBB0_12" ]
  ]
  ; text ".LBB0_12" [
      Jmp, [ ~$$".LBB0_13" ]
  ]
  ; text ".LBB0_13" [
      Movq, [ Ind3 (Lit (-24L), Rbp); ~%Rax ]
    ; Imulq, [ Ind3 (Lit (-16L), Rbp); ~%Rax ]
    ; Addq, [ Ind3 (Lit (-40L), Rbp); ~%Rax ]
    ; Movq, [ ~%Rax; Ind3 (Lit (-24L), Rbp) ]
    ; Movq, [ Ind3 (Lit (-8L), Rbp); ~%Rax ]
    ; Addq, [ ~$1; ~%Rax ]
    ; Movq, [ ~%Rax; Ind3 (Lit (-8L), Rbp) ]
    ; Jmp, [ ~$$".LBB0_1" ]
  ]
  ; text ".LBB0_15" [
      Movq, [ Ind3 (Lit (-24L), Rbp); ~%Rax ]
    ; Popq, [ ~%Rbp ]
    ; Retq, []
  ]
  ; gtext "main" [
      Pushq, [ ~%Rbp ]
    ; Movq, [ ~%Rsp; ~%Rbp ]
    ; Movq, [ ~$$".L.str"; ~%Rdi ]
    ; Movq, [ ~$base; ~%Rsi ]
    ; Callq, [ ~$$"_Z9parse_intPKcl" ]
    ; Popq, [ ~%Rbp ]
    ; Retq, []
  ]
  ; data ".L.str" [
    Asciz input
  ]
]

let cc_not = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Notq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_not_negative = fun () -> Gradedtests.test_machine
  [InsB0 (Xorq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag (* sets all flags to false *)
  ;InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Notq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_and = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit 0x0F0F0F0FL); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Andq, [Imm (Lit 0xF0F0F0FFL); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_and_arg = fun (a:int) (b:int) -> Gradedtests.test_machine
  [InsB0 (Movq, [~$a; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Andq, [~$b; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_or = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit 0x0F0F0F0FL); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Orq,  [Imm (Lit 0xF0F0F0F0L); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_xor = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit 0x0F0F0F0FL); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Orq,  [Imm (Lit 0xF0F0F0F0L); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_sar = fun (n:int) (v:Int64.t) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit v); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Sarq, [~$n; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_shl = fun (n:int) (v:Int64.t) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit v); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Shlq, [~$n; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_shr = fun (n:int) (v:Int64.t) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit v); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Shrq, [~$n; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_incr = fun (src:Int64.t) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit src); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Incq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_decr = fun (src:Int64.t) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit src); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Decq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_sub = fun (dest:Int64.t) (src:Int64.t) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit dest); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Subq, [Imm (Lit src); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_imul = fun (a:int64) (b:int64) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit a); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Imulq, [Imm (Lit b); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_neg = fun (a:int64) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit a); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Negq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let condition_flag_set_tests = 
  [ (* Logic instruction. *)
    ("cc_and", Gradedtests.cs_test 2 (cc_and ()) (false, false, false))
  ; ("cc_or",  Gradedtests.cs_test 2 (cc_or  ()) (false, false, false))
  ; ("cc_xor", Gradedtests.cs_test 2 (cc_xor ()) (false, false, false))
  ; ("cc_not", Gradedtests.csi_test 2 (cc_not ()))
  ; ("cc_not_negative", Gradedtests.cs_test 3 (cc_not_negative ()) (false, false, false))
  ; ("cc_and_arg_eq_0", Gradedtests.cs_test 2 (cc_and_arg 0 12345) (false, false, true))
  ; ("cc_and_arg_eq_neg_1", Gradedtests.cs_test 2 (cc_and_arg (-1) (-1)) (false, true, false))

    (* Bit-manipulation instructions. *)
  ; ("cc_sar_0_a", Gradedtests.cc_test "OF:false SF:false ZF:false" 2 (cc_sar 0 0x0F0F0F0FL) (false, false, false)
      (fun m -> not (m.flags.fo || m.flags.fs || m.flags.fz)))
  ; ("cc_sar_0_b", Gradedtests.cc_test "OF:false SF:false ZF:false" 2 (cc_sar 0 0x0F0F0F0FL) (true, false, true)
      (fun m -> m.flags.fo && (not m.flags.fs) && m.flags.fz))
  ; ("cc_sar_0_c", Gradedtests.csi_test 2 (cc_sar 0 0x0F0F0F0FL))
  ; ("cc_sar_1", Gradedtests.cso_test 2 (cc_sar 1 0x0F0F0F0FL) false)
  ; ("cc_sar_1_neg_a", Gradedtests.cso_test 2 (cc_sar 1 Int64.min_int) false)
  ; ("cc_sar_1_neg_b", Gradedtests.cc_test "OF:false SF:true ZF:false" 2 (cc_sar 1 Int64.min_int) (true, false, true)
      (fun m -> not m.flags.fo && m.flags.fs && not m.flags.fz))
  ; ("cc_sar_max_a", Gradedtests.cc_test "OF:false SF:false ZF:true" 2 (cc_sar 63 Int64.max_int) (true, true, false)
      (fun m -> m.flags.fo && not (m.flags.fs) && m.flags.fz))
  ; ("cc_sar_max_b", Gradedtests.cc_test "OF:false SF:false ZF:true" 2 (cc_sar 63 Int64.max_int) (false, true, false)
      (fun m -> not m.flags.fo && not m.flags.fs && m.flags.fz))
  
  ; ("cc_shl_0_a", Gradedtests.cc_test "OF:false SF:false ZF:false" 2 (cc_shl 0 Int64.max_int) (false, false, false)
    (fun m -> not (m.flags.fo || m.flags.fs || m.flags.fz)))
  ; ("cc_shl_0_b", Gradedtests.cc_test "OF:false SF:false ZF:false" 2 (cc_shl 0 Int64.max_int) (true, false, true)
    (fun m -> m.flags.fo && (not m.flags.fs) && m.flags.fz))
  ; ("cc_shl_0_c", Gradedtests.csi_test 2 (cc_shl 0 Int64.max_int))
  ; ("cc_shl_1", Gradedtests.cso_test 2 (cc_shl 1 Int64.max_int) true)
  ; ("cc_shl_1_neg_a", Gradedtests.cso_test 2 (cc_shl 1 Int64.min_int) true)
  ; ("cc_shl_1_neg_b", Gradedtests.cc_test "OF:true SF:false ZF:true" 2 (cc_shl 1 Int64.min_int) (false, true, false)
    (fun m -> m.flags.fo && (not m.flags.fs) && m.flags.fz))
  ; ("cc_shl_1_top_equal", Gradedtests.cso_test 2 (cc_shl 1 (-1L)) false)
  ; ("cc_shl_n_a", Gradedtests.cc_test "OF:false SF:false ZF:false" 2 (cc_shl 1 12L) (false, true, true)
    (fun m -> not m.flags.fo && not m.flags.fs && not m.flags.fz))
  ; ("cc_shl_n_a", Gradedtests.cc_test "OF:false SF:true ZF:false" 2 (cc_shl 60 12L) (false, false, true)
    (fun m -> not m.flags.fo && m.flags.fs && not m.flags.fz))
  ; ("cc_shl_zero", Gradedtests.cc_test "OF:false SF:false ZF:true" 2 (cc_shl 1 0L) (false, true, false)
    (fun m -> (not m.flags.fo) && (not m.flags.fs) && m.flags.fz))

  ; ("cc_shr_0_a", Gradedtests.cc_test "OF:false SF:false ZF:false" 2 (cc_shr 0 Int64.min_int) (false, false, false)
    (fun m -> not (m.flags.fo || m.flags.fs || m.flags.fz)))
  ; ("cc_shr_0_b", Gradedtests.csi_test 2 (cc_shr 0 Int64.max_int))
  ; ("cc_shr_1", Gradedtests.cso_test 2 (cc_shr 1 0x0F0F0F0FL) false)
  ; ("cc_shr_1_neg", Gradedtests.cso_test 2 (cc_shr 1 Int64.min_int) true)
  ; ("cc_shr_max", Gradedtests.cc_test "OF:true SF:false ZF:false" 2 (cc_shr 3 Int64.max_int) (true, true, true)
      (fun m -> m.flags.fo && not m.flags.fs && not m.flags.fz))
  ; ("cc_shr_max", Gradedtests.cc_test "OF:true SF:false ZF:true" 2 (cc_shr 63 Int64.max_int) (true, true, false)
      (fun m -> m.flags.fo && not m.flags.fs && m.flags.fz))

  ; ("cc_imul", Gradedtests.cc_test "OF:false" 2 (cc_imul 2L 4L) (true, true, true)
      (fun m -> not m.flags.fo))
  ; ("cc_imul", Gradedtests.cc_test "OF:true" 2 (cc_imul 4L Int64.max_int) (false, true, true)
      (fun m -> m.flags.fo))
  ; ("cc_neg", Gradedtests.cs_test 2 (cc_neg Int64.min_int) (true, true, false))

    (* Arithmetic instructions. *)
  ; ("cc_sub_1", Gradedtests.cs_test 2 (cc_sub 0xFFFFFFFFFFFFFFFFL (-1L)) (false, false, true))
  ; ("cc_sub_2", Gradedtests.cs_test 2 (cc_sub 0xFFFFFFFFFFFFFFFFL 1L) (false, true, false))
  ; ("cc_sub_3", Gradedtests.cs_test 2 (cc_sub Int64.min_int 42L) (true, false, false))
  ; ("cc_sub_4", Gradedtests.cs_test 2 (cc_sub (-1233291893L) Int64.max_int) (true, false, false))

  ; ("cc_incr", Gradedtests.cs_test 2 (cc_incr Int64.max_int) (true, true, false))
  ; ("cc_incr", Gradedtests.cs_test 2 (cc_incr (-1L)) (false, false, true))
  ; ("cc_decr", Gradedtests.cs_test 2 (cc_decr Int64.min_int) (true, false, false))
  ; ("cc_decr", Gradedtests.cs_test 2 (cc_decr 1L) (false, false, true))
  ]


let leaq = fun () -> Gradedtests.test_machine
  [InsB0 (Leaq, [Ind1 (Lit (mem_bot)); ~%R12]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ;InsB0 (Movq, [Imm (Lit (Int64.add mem_bot @@ Int64.mul ins_size 4L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Leaq, [Ind2 Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Leaq, [Ind3 (Lit ins_size, Rbx); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let jmp = fun (src:operand) -> Gradedtests.test_machine
  [InsB0 (Jmp, [src]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let jmp_ind_1 = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit (Int64.add mem_bot 24L)); Ind1 (Lit (Int64.add mem_bot 24L))]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [Imm (Lit (Int64.add mem_bot 24L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Jmp, [Ind1 (Lit (Int64.add mem_bot 24L))]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let jmp_ind_2 = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit (Int64.add mem_bot 24L)); Ind1 (Lit (Int64.add mem_bot 24L))]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [Imm (Lit (Int64.add mem_bot 24L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Jmp, [Ind2 Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let jmp_ind_3 = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit (Int64.add mem_bot 24L)); Ind1 (Lit (Int64.add mem_bot 32L))]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [Imm (Lit (Int64.add mem_bot 24L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Jmp, [Ind3 (Lit 8L, Rax)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let callq = fun (src:quad) -> Gradedtests.test_machine
  [InsB0 (Movq,  [Imm (Lit src); Ind1 (Lit src)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Callq, [Ind1 (Lit src)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let ret = fun () -> Gradedtests.test_machine
  [InsB0 (Movq,  [Imm (Lit 4194328L); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Callq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Retq, []);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Retq, []);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let j = fun (cnd:cnd) (n:int) (src:quad) -> Gradedtests.test_machine
  [InsB0 (Addq, [~$n; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (J cnd, [Imm (Lit src)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let bin_log = fun (op:opcode) -> Gradedtests.test_machine
  [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$3; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$255; ~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$1; Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (op, [~%Rax; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (op, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (op, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (op, [Gradedtests.stack_offset 0L; ~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let notq = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [Imm (Lit (Int64.min_int)); Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Notq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Notq, [Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let shr = fun (op:opcode) -> Gradedtests.test_machine
  [InsB0 (Movq, [~$256; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [Imm (Lit (Int64.min_int)); Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$63; ~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (op, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (op, [~%Rcx; Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let setb = fun (cnd:cnd) -> Gradedtests.test_machine
  [InsB0 (Addq, [~$256; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Set cnd, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Addq, [Imm (Lit (Int64.max_int)); Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Set cnd, [Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let set_cmp = fun (a:int64) (b:int64) (cnd:cnd) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit a); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Cmpq, [Imm (Lit b); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [Imm (Lit Int64.max_int); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Set cnd, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [Imm (Lit (Int64.max_int)); Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Set cnd, [Gradedtests.stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let bin_aop = fun (op:opcode) -> Gradedtests.test_machine
  [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$22; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (op, [~%Rbx; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let leaq_simpl = fun (rax:quad) (src:operand) -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit rax); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ; InsB0 (Leaq, [src; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]

let instruction_tests = [
  ("leaq", Gradedtests.machine_test "r12=4194304L rbx=4194336L rax=4194344L" 4 (leaq ())
    (fun m -> m.regs.(rind R12) = mem_bot
           && m.regs.(rind Rbx) = (Int64.add mem_bot @@ Int64.mul ins_size 4L)
           && m.regs.(rind Rax) = (Int64.add mem_bot @@ Int64.mul ins_size 5L)
    )
  );
  ("jmp_lit", Gradedtests.machine_test "rip=0x400000L" 1 (jmp @@ Imm (Lit(mem_bot)))
    (fun m -> m.regs.(rind Rip) = mem_bot)
  );
  ("jmp_reg", Gradedtests.machine_test "rip=4259832L" 1 (jmp ~%Rsp)
    (fun m -> m.regs.(rind Rsp) = Int64.sub mem_top 8L
           && m.regs.(rind Rip) = Int64.sub mem_top 8L)
  );
  ("jmp_ind1", Gradedtests.machine_test "rip=4194328L" 3 (jmp_ind_1 ())
    (fun m -> m.regs.(rind Rip) = Int64.add mem_bot 24L)
  );
  ("jmp_ind2", Gradedtests.machine_test "rip=4194328L" 3 (jmp_ind_2 ())
    (fun m -> m.regs.(rind Rip) = Int64.add mem_bot 24L)
  );
  ("jmp_ind3", Gradedtests.machine_test "rip=4194328L" 3 (jmp_ind_3 ())
    (fun m -> m.regs.(rind Rip) = Int64.add mem_bot 24L)
  );
  ("call", Gradedtests.machine_test "rip=4194328L rsp=4259824L" 2 (callq 4194328L)
    (fun m -> m.regs.(rind Rip) = 4194328L
           && m.regs.(rind Rsp) = Int64.sub mem_top 16L
           (* Stack contains next instruction to be called in the caller. *)
           && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size - 16)) = Int64.add mem_bot 16L));
  ("ret_1", Gradedtests.machine_test "rip=4194320L" 3 (ret ()) 
    (fun m -> m.regs.(rind Rip) = 4194320L));
  ("ret_2", Gradedtests.machine_test "rip=0L" 4 (ret ())
    (* machine_test doesn't set the sentinel address. *)
    (fun m -> m.regs.(rind Rip) = 0L));
  ("j_1", Gradedtests.machine_test "rip=4259832L" 2 (j Eq 0 4259832L)
    (fun m -> m.flags.fz && m.regs.(rind Rip) = 4259832L));
  ("j_2", Gradedtests.machine_test "rip=4194320L" 2 (j Neq 0 4259832L)
    (fun m -> m.flags.fz && m.regs.(rind Rip) = 4194320L));
  ("j_3", Gradedtests.machine_test "rip=4194320L" 2 (j Lt 12 4259832L)
    (fun m -> not m.flags.fz && not m.flags.fs && m.regs.(rind Rip) = 4194320L));
  ("orq", Gradedtests.machine_test "rax=2 rbx=3 rcx=255 *65528=1" 8 (bin_log Orq)
    (fun m -> m.regs.(rind Rax) = 2L
           && m.regs.(rind Rbx) = 3L
           && m.regs.(rind Rcx) = 255L
           && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = 1L
    )
  );
  ("xorq", Gradedtests.machine_test "rax=2 rbx=1 rcx=254 *65528=1" 8 (bin_log Xorq)
    (fun m -> m.regs.(rind Rax) = 2L
           && m.regs.(rind Rbx) = 1L
           && m.regs.(rind Rcx) = 254L
           && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = 1L
    )
  );
  ("notq", Gradedtests.machine_test "rax=-3L *65528=Int64.max_int" 4 (notq ())
    (fun m -> m.regs.(rind Rax) = -3L
           && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = Int64.max_int
    )
  );
  ("shrq", Gradedtests.machine_test "rax=64L *65528=1L" 5 (shr (Shrq))
    (fun m -> m.regs.(rind Rax) = 64L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = 1L
    )
  );
  ("sraq", Gradedtests.machine_test "rax=64L *65528=-1L" 5 (shr (Sarq))
    (fun m -> m.regs.(rind Rax) = 64L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = -1L
    )
  );
  ("setb", Gradedtests.machine_test "rax=256L *65528=0x7FFFFFFFFFFFFF00L" 4 (setb (Lt))
    (fun m -> m.regs.(rind Rax) = 256L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = Int64.sub Int64.max_int 255L
    )
  );
  ("setb", Gradedtests.machine_test "rax=257L *65528=0x7FFFFFFFFFFFFF01L" 4 (setb (Gt))
    (fun m -> m.regs.(rind Rax) = 257L
      && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = Int64.sub Int64.max_int 254L
    )
  );
  ("setb_cmp", Gradedtests.machine_test "rax=*65528=0x7FFFFFFFFFFFFF01L" 6 (set_cmp 10L 10L Eq)
    (fun m ->
      let exp = Int64.sub Int64.max_int 254L in
        m.regs.(rind Rax) = exp
        && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = exp
    )
  );
  ("setb_cmp", Gradedtests.machine_test "rax=*65528=0x7FFFFFFFFFFFFF00L" 6 (set_cmp 11L 10L Eq)
    (fun m ->
      let exp = Int64.sub Int64.max_int 255L in
        m.regs.(rind Rax) = exp
        && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = exp
    )
  );
  ("setb_cmp", Gradedtests.machine_test "rax=*65528=0x7FFFFFFFFFFFFF01L" 6 (set_cmp 10L 10L Ge)
    (fun m ->
      let exp = Int64.sub Int64.max_int 254L in
        m.regs.(rind Rax) = exp
        && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = exp
    )
  );
  ("setb_cmp", Gradedtests.machine_test "rax=*65528=0x7FFFFFFFFFFFFF01L" 6 (set_cmp (-10L) 10L Le)
    (fun m ->
      let exp = Int64.sub Int64.max_int 254L in
        m.regs.(rind Rax) = exp
        && int64_of_sbytes (Gradedtests.sbyte_list m.mem (mem_size-8)) = exp
    )
  );
  ("addq", Gradedtests.machine_test "rax=24L" 3 (bin_aop Addq)
    (fun m -> m.regs.(rind Rax) = 24L));
  ("subq", Gradedtests.machine_test "rax=24L" 3 (bin_aop Subq)
    (fun m -> m.regs.(rind Rax) = -20L));
  ("leaq", Gradedtests.machine_test "90()" 2 (leaq_simpl 0L (Ind1 (Lit 90L)))
    (fun m -> m.regs.(rind Rax) = 90L));
  ("leaq", Gradedtests.machine_test "(%rax)" 2 (leaq_simpl 10L (Ind2 Rax))
    (fun m -> m.regs.(rind Rax) = 10L));
  ("leaq", Gradedtests.machine_test "10(%rax)" 2 (leaq_simpl 10L (Ind3 (Lit 10L, Rax)))
    (fun m -> m.regs.(rind Rax) = 20L));
]


let setq = Gradedtests.test_machine
    [InsB0 (Movq, [~$512; ~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$13; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$13; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Cmpq, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Set (Eq), [~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ]


let callq_retq = Gradedtests.test_machine
    [InsB0 (Callq, [~$(0x400000 + 24)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$25; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Addq, [~%Rbx; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$13; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$13; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Addq, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Retq, []);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ]

let georgr_insn_tests = [
  ("setq", Gradedtests.machine_test "rcx=513L" 5 setq
     (fun m -> m.regs.(rind Rcx) = 513L)
  );
  ("callq_retq", Gradedtests.machine_test "rax=51L, rbx=26L" 7 callq_retq
     (fun m -> (m.regs.(rind Rax) = 51L) && (m.regs.(rind Rbx) = 26L))
  )
]

let shared_suite : suite = [
  Test ("Debug: End-to-end Tests", [
    ("empty main", Gradedtests.program_test [empty_main] 0L)
    ; ("negq", Gradedtests.program_test (main_driver::negq) (Int64.neg 99L) )
    ; ("addq", Gradedtests.program_test (main_driver::addq) 3L )
    ; ("fibonacci", Gradedtests.program_test (main_driver::fibonacci) 832040L)
    ; ("parse_int_1", Gradedtests.program_test (parse_int "ECEB" 16) 0xECEBL)
    ; ("parse_int_2", Gradedtests.program_test (parse_int "42" 10) 42L)
    ; ("parse_int_3", Gradedtests.program_test (parse_int "0101" 16) 0x0101L)
    ; ("parse_int_4", Gradedtests.program_test (parse_int "0101" 2) 0b0101L)
    ; ("parse_int_5", Gradedtests.program_test (parse_int "6543" 8) 0o6543L)
  ]);
  Test ("Debug: Lookup Table Generation", [
    ("redefined sym", redefinedsym_test [text "main" [Retq,[]]; text "main" [Retq,[]]])
    ; ("redefined sym", redefinedsym_test [text "a" []; text "main" [Retq,[]]; text "a" [Retq,[]]])
  ]);
  Test ("Debug: Condition Flags Set Tests", condition_flag_set_tests);
  Test ("Debug: Instruction Tests", instruction_tests);
  Test ("Debug: Tests by dbernhard", [
    ("prog_exit", Gradedtests.program_test prog_nothing 0L)
    ; ("prog_movq_ret3", Gradedtests.program_test (prog_int 3) 3L)
    ; ("prog_callq_ret900", Gradedtests.program_test (prog_callq_ret 900) 900L)
    ; ("prog_double_symb", redefinedsym_test prog_double_symb)
    ; ("prog_placement", Gradedtests.program_test prog_placement 0x400010L)
    ; ("prog_placement_ind_lit", Gradedtests.program_test prog_placement_ind_lit 0x400001L)
    ; ("prog_leaq_ind2", Gradedtests.program_test prog_leaq_ind2 0x400018L)
    ; ("prog_mov_ind3", Gradedtests.program_test prog_mov_ind3 39L)
    ; ("prog_dec_reg", Gradedtests.program_test prog_dec_reg 11L)
    ; ("prog_shift by variable amount", Gradedtests.program_test prog_shift 29L)
  ]);
  Test ("Debug End-to-end Log Tests", [
    ("log 1", Gradedtests.program_test (log 1L) (0L))
    ; ("log 2", Gradedtests.program_test (log 2L) (1L))
    ; ("log 3", Gradedtests.program_test (log 3L) (1L))
    ; ("log 4419", Gradedtests.program_test (log 4419L) (12L))
    ; ("log 15630003", Gradedtests.program_test (log 15630003L) (23L))
    ; ("log 2^63-1", Gradedtests.program_test (log 0x7fffffffffffffffL) (62L))
  ]);
  Test ("Debug: georgr's redundant instruction tests", georgr_insn_tests)

]
