    bits 64

    or rax, 0x0f
    or rax, 0x00000000000000ff
    or rax, 0xfffffffffffffffe
    or rax, 0x00000002
    or rcx, 0x0f
    or rcx, 0x00000000000000ff
    or rcx, 0xfffffffffffffffe
    or rcx, 0x00000002

    or byte [rax], 0x0f
    or byte [rax], 0x00000000000000ff
    or byte [rax], 0xfffffffffffffffe
    or byte [rax], 0x00000002

    or word [rax], 0x0f
    or word [rax], 0x00000000000000ff
    or word [rax], 0xfffffffffffffffe
    or word [rax], 0x00000002

    or dword [rax], 0x0f
    or dword [rax], 0x00000000000000ff
    or dword [rax], 0xfffffffffffffffe
    or dword [rax], 0x00000002

    or qword [rax], 0x0f
    or qword [rax], 0x00000000000000ff
    or qword [rax], 0xfffffffffffffffe
    or qword [rax], 0x00000002

    or rax, rcx
    or rdi, 0xff
    or al, 0x0f

    or rdi, rax
    or rax, rcx
    or rax, 0x0000000000000001
    or rax, 0x00000002
    or ax, cx
    or cl, al
