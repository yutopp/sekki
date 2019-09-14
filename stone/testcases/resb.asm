con: equ 0x40000

    bits 64
    resb 10

    dq a
    dq b

a:  resb con
    resd 10
b:
