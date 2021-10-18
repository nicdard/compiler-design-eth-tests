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

let condition_flag_set_tests = 
  [ (* Logic instruction. *)
    ("cc_and", Gradedtests.cs_test 2 (cc_and ()) (false, false, false))
  ; ("cc_or",  Gradedtests.cs_test 2 (cc_or  ()) (false, false, false))
  ; ("cc_xor", Gradedtests.cs_test 2 (cc_xor ()) (false, false, false))
  ; ("cc_not", Gradedtests.csi_test 2 (cc_not ()))

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
  ; ("cc_shr_max", Gradedtests.cc_test "OF:true SF:false ZF:false" 2 (cc_shr 3 Int64.max_int) (true, true, true)
      (fun m -> m.flags.fo && not m.flags.fs && not m.flags.fz))

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

let instruction_tests = [
  ("leaq", Gradedtests.machine_test "r12=4194304L rbx=4194336L rax=4194344L" 4 (leaq ())
    (fun m -> print_endline @@ Int64.to_string m.regs.(rind Rax); m.regs.(rind R12) = mem_bot
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
]

let provided_tests : suite = [
  Test ("Debug: End-to-end Tests", [
    ("empty main", Gradedtests.program_test [empty_main] 0L)
    ; ("negq", Gradedtests.program_test (main_driver::negq) (Int64.neg 99L) )
    ; ("addq", Gradedtests.program_test (main_driver::addq) 3L )
  ]);
  Test ("Debug: Condition Flags Set Tests", condition_flag_set_tests);
  Test ("Debug: Instruction Tests", instruction_tests)
]
