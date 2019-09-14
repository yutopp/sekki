    bits 64
    org 0x00400000

    jmp 10000

    jmp m
m:                              ; 1. assume 8

    align 256
b:  equ 5000                    ; 1. assume 128 + 2 + 8 = 130
    jmp b
    jmp m

    align 0x2000
    je .next
    resb 0x1000
.next:
