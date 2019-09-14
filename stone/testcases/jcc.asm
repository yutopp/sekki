    bits 64
    dq b
m:                              ; 1. assume 8
    ja m                        ; 1. assume 2

    ;; 1
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2

    ;; 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2

    ;; 3
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2

    ;; 4
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2

    ;; 5
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2

    ;; 6
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2

    ;; 7
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
    ja b                        ; 1. assume 2
 ;   times 64 ja b               ; 1. assume 2 * 64
b:  equ 5000                            ; 1. assume 128 + 2 + 8 = 130

    ja b
    jb b
    jbe b
    je b
    jg b
    jge b
    jl b
    jle b
    jne b
