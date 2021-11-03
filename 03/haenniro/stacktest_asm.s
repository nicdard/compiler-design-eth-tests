.text
.globl main

main:
    pushq %rax
    call ll_main
    pushq %rax
    call ll_main
    movq $0, %rax
    addq $16, %rsp
    retq