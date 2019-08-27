a:
    db 0, "abc", 12
    mov [rbp-8], rax
.loop:
    mov rdi, [rbp-8]
b:
    dq $
