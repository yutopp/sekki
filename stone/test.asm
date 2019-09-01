    bits 64
a:
    db 0, "abc", 12
;    mov [rbp-8], rbp
;    call sexp_alloc_cons
;    mov [rbp-56], rax           ; (value, nil)
;.loop:
;    mov rdi, [rbp+8]
;b:
;    dq $
    mov rax, rcx
