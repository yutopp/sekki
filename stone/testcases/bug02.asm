    bits 64
    org     0x00400000

    je .rhs_bits8
    resb 123
.rhs_bits8:
