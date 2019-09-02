    bits 64
    db 1

    org     0x01

hoge:  equ 1+a
    db hoge
    dw hoge
    dd hoge
    dq hoge
    dq a - $$
a:

;    db 0, "abc", 12
;    mov [rbp-8], rbp
;;    call sexp_alloc_cons
;;    mov [rbp-56], rax           ; (value, nil)
;;.loop:
;;    mov rdi, [rbp+8]
;;b:
;;    dq $
;    mov rax, rcx
;    mov [rax-40], rbp
