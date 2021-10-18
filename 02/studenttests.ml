open Assert
open X86
open Simulator
open Asm

(* You can use this file for additional test cases to help your *)
(* implementation.                                              *)

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

let cc_not = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Notq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let cc_and = fun () -> Gradedtests.test_machine
  [InsB0 (Movq, [Imm (Lit 0x0F0F0F0FL); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Andq, [Imm (Lit 0xF0F0F0FFL); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
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

let condition_flag_set_tests = 
  [ (* Logic instruction. *)
    ("cc_and", Gradedtests.cs_test 2 (cc_and ()) (false, false, false))
  ; ("cc_or",  Gradedtests.cs_test 2 (cc_or  ()) (false, false, false))
  ; ("cc_xor", Gradedtests.cs_test 2 (cc_xor ()) (false, false, false))
  ; ("cs_not", Gradedtests.csi_test 2 (cc_not ()))

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
  ; ("cc_shr_max", Gradedtests.cc_test "OF:true SF:false ZF:true" 2 (cc_shr 3 Int64.max_int) (true, true, true)
      (fun m -> m.flags.fo && not m.flags.fs && m.flags.fz))
  ]

let provided_tests : suite = [
  Test ("Debug: End-to-end Tests", [
    ("empty main", Gradedtests.program_test [empty_main] 0L)
    ; ("negq", Gradedtests.program_test (main_driver::negq) (Int64.neg 99L) )
    ; ("addq", Gradedtests.program_test (main_driver::addq) 3L )
  ]);
  Test ("Debug: Condition Flags Set Tests", condition_flag_set_tests)
]
