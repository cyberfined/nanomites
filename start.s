.text
.globl _start
.type _start,@function
.section .text._start
_start:
    # Call main
    movq 16(%rbp), %rdi
    movq %rbp, %rsi
    addq $24, %rsi
    callq main

    # Exit
    movq %rax, %rdi
    movq $60, %rax
    syscall
.size _start,.-_start
