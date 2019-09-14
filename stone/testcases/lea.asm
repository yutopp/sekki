    bits 64

a:
.updated_args_offset: equ 20

    lea rdi, [.updated_args_offset]
    lea rdi, [rbp+.updated_args_offset]
    lea rdi, [rbp-.updated_args_offset]

    lea rdi, [rbp-48]
    lea rdx, [rbp-88]
    lea rdx, [rbp-56]
    lea rdi, [rdi+56]

    lea rax, [rax+rcx*4]
    lea rdi, [rbp-0xa0]
    lea rdi, [rbp+0xa0]

    lea rax, [rbp-160]
    lea rax, [rbp-168]
