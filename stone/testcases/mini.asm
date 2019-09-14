    ;; mini
    bits 64
    org     0x00400000

    mov qword [rbp-48], cc
    dq shdrs - $$
cc:
    call cc

    dq f
    align 0x200
f:
;    db 0xff
;    dq e4


e:  equ $-$$
 ;   dq e4

    cmp rax, 0xff
    cmp rax, 8
    cmp rax, 0xffffffff

    db 1

    db e
    dq e4
    dq e3
    dq e4

    cmp rcx, rsi

local:
    jmp .e2
.e2:

local2:
    jmp .e2
.e2:
    align 16

    push qword [rbp]
    push rbp
    mov rbp, rsp
    pop rbp
    cmp rsi, 8
;
e3:  equ $-$$

code_segment_begin:
;
segment_align2:  equ 1+a
a:
    db segment_align2
    mov qword [rbp-32], 0

    mov rdx, 1
    mov r10, 2
    mov rax, $
    mov rax, b
    mov qword [rax], 1

;   mov [rax], 0
                                ;
g_asm_inst_sizes:   resd 0x10000

foo:
    call foo

b:
e4:  equ $-$$
    leave
    ret

    align 16
shdrs:
