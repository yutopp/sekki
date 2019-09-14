    bits 64

    sub rsp, 8
    sub rsp, 128

    sub rdi, r10
    sub al, 10
    sub ax, 10
    sub rdx, rcx
    sub rax, 32
    sub [rbp-40], rax           ;
    sub rdi, [rbp-72]
    sub eax, dword [rdi+44]
    sub dword [rdi+44], 1
    sub r10, rcx
