    bits 64

    add rdi, r10
    add al, 10
    add ax, 10
    add rdx, rcx
    add rax, 32
    add [rbp-40], rax           ;
    add rdi, [rbp-72]
    add eax, dword [rdi+44]
    add dword [rdi+44], 1
    add r10, rcx
    add byte [rcx], al
