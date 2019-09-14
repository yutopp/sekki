    bits 64

a:
    mov rax, a
    mov rax, b
    mov rax, $


    mov byte [rdi],0x66
    mov byte [rdi+0x0],0x66

;    mov rax, 1
;    mov rax, -1

;v:  equ -1
;    mov rax, v
    mov rcx, 0xff00

    mov al, -0x80
    mov al, -0x81
    mov ax, -0x80
    mov ecx, 0x80000000
    mov rcx, 0x80000000
    mov rcx, 0x80000001
    mov rcx, -0x80000001


    mov qword [rbp-0x80],0x0
    mov qword [rbp+0x80],0x0

app_max_code_buffer_size:   equ 0x100000
    mov rdx, app_max_code_buffer_size

    mov al, 0
    mov ax, 0
    mov eax, 0
    mov rax, 0

    mov cl, 0
    mov cx, 0
    mov ecx, 0
    mov rcx, 0

    mov al, byte [rdi+44]
    mov byte [rdi+44], 0
    mov byte [rdi+44], al
    mov byte [rcx], al
    mov eax, dword [rcx+0x40]
    mov eax,0x2


    mov ax, word [rdi+44]

    mov word [rdi+44], 0
    mov word [rdi+44], ax

    mov eax, dword [rdi+44]
    mov dword [rdi+44], 0
    mov dword [rdi+44], eax

    mov eax, [rdx+rcx*4]
    mov eax, [rcx*4+rdi]
    mov cl, [rdi+rax]
    mov cl, [rax+rdi+10]

    mov byte [rsi], dl
    mov byte [rsi], 0
    mov byte [rax], dl
    mov [rcx*8+rdx], rax
cc:
    mov qword [rbp-48], cc
    mov dl, [r10]
    mov [r10], dl
    mov eax, [rbp-32]           ; byte*
    mov rdx, 1
    mov rbp, rsp
    mov qword [rbp-32], 0
    mov rdx, 1
    mov r10, 2

    mov qword [rax], 1
b:
    mov rax, [cc]
