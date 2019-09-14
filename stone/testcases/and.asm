    bits 64

    and rax, 0x0f
    and rax, 0x00000000000000ff
    and rax, 0xfffffffffffffffe
    and rax, 0x00000002
    and rcx, 0x0f
    and rcx, 0x00000000000000ff
    and rcx, 0xfffffffffffffffe
    and rcx, 0x00000002

    and byte [rax], 0x0f
    and byte [rax], 0x00000000000000ff
    and byte [rax], 0xfffffffffffffffe
    and byte [rax], 0x00000002

    and word [rax], 0x0f
    and word [rax], 0x00000000000000ff
    and word [rax], 0xfffffffffffffffe
    and word [rax], 0x00000002

    and dword [rax], 0x0f
    and dword [rax], 0x00000000000000ff
    and dword [rax], 0xfffffffffffffffe
    and dword [rax], 0x00000002

    and qword [rax], 0x0f
    and qword [rax], 0x00000000000000ff
    and qword [rax], 0xfffffffffffffffe
    and qword [rax], 0x00000002

    and rax, rcx
    and rdi, 0xff
    and al, 0x0f

    and rdi, rax
    and rax, rcx
    and rax, 0x0000000000000001
    and rax, 0x00000002
    and ax, cx
    and cl, al
