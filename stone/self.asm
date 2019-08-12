    bits    64
    org     0x00400000          ; このあたりにマッピングしておきたい


;;; ELF Header
    ;; e_ident
    db 0x7f, "ELF"              ; Magic number
    db 0x02                     ; EI_CLASS, ELFCLASS64
    db 0x01                     ; EI_DATA, ELFDATA2LSB
    db 0x01                     ; EI_VERSION, EV_CURRENT
    db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ; padding
    ;; e_type, ET_EXEC
    dw 0x0002
    ;; e_machine, EM_X86_64
    dw 0x003e
    ;; e_version, EV_CURRENT
    dd 0x00000001
    ;; e_entry
    dq _start
    ;; e_phoff
    dq phdrs - $$
    ;; e_shoff,
    dq shdrs - $$
    ;; e_flags
    dd 0x00000000
    ;; e_ehsize, (64)
    dw 0x0040
    ;; e_phentsize, (56)
    dw 0x0038
    ;; e_phnum
    dw 0x0001
    ;; e_shentsize, (64)
    dw 0x0040
    ;; e_shnum
    dw 0x0000
    ;; e_shstrndx
    dw 0x0000



;;; Program headers
phdrs:
    ;; p_type, PT_LOAD
    dd 0x00000001
    ;; p_flags, R(4)X(1)
    dd 0x00000005
    ;; p_offset
    dq 0x0000000000000000
    ;; p_vaddr,
    dq $$
    ;; p_paddr
    dq 0
    ;; p_filesz
    dq 0x0000000000000010
    ;; p_memsz
    dq 0x0000000000000010
    ;; p_align
    dq 0x0000000000001000



;;; Segments
_start:
    mov rax, 60
    mov rdi, 42
    syscall


;;; Section headers
shdrs:
