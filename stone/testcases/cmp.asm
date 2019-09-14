    bits 64

    cmp cl, 0x0
    cmp rcx, [rbp-32]
    cmp dl, al
    cmp rax, 0xff
    cmp rax, 8
    cmp rax, 0xffffffff
    cmp rcx, rsi

    cmp rax, -1
    cmp    rax,0x80

    cmp al, byte [rbp-0x20]
