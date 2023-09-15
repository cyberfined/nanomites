.text
.globl _start
_start:
    # Make text segment writable
    movq $10, %rax
    movq $0x400000, %rdi
    movq $0x1000, %rsi
    movq $7, %rdx
    syscall

    # Call main
    movq (%rsp), %rdi
    movq %rsp, %rsi
    addq $8, %rsi
    callq main

    # Exit
    movq %rax, %rdi
    movq $60, %rax
    syscall
