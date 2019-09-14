local:
    jmp .e2
.e2:

local2:
    jmp .e2
    jmp .a
.e2:
    ja local
    je local
    jg local
    jge local
    jl local
    jle local
    jne local
.a:
.b:
