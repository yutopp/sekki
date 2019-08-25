    bits    64
    org     0x00400000          ; このあたりにマッピングしておきたい

segment_align:  equ 0x1000

app_max_code_buffer_size:   equ 0x8000
app_max_out_buffer_size:    equ 0x8000
app_max_sexp_objects_count: equ 2000

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
    dw 0x0003
    ;; e_shentsize, (64)
    dw 0x0040
    ;; e_shnum
    dw 0x0005
    ;; e_shstrndx
    dw 0x0001



;;; Program headers
phdrs:
    ;; [0]
    ;; p_type, PT_LOAD
    dd 0x00000001
    ;; p_flags, R(4)
    dd 0x00000004
    ;; p_offset
    dq 0x0000000000000000
    ;; p_vaddr,
    dq $$
    ;; p_paddr
    dq 0
    ;; p_filesz
    dq rest_size
    ;; p_memsz
    dq rest_size
    ;; p_align
    dq segment_align

    ;; [1]
    ;; p_type, PT_LOAD
    dd 0x00000001
    ;; p_flags, R(4)X(1)
    dd 0x00000005
    ;; p_offset
    dq code_segment_begin-$$
    ;; p_vaddr,
    dq code_segment_begin
    ;; p_paddr
    dq 0
    ;; p_filesz
    dq code_segment_rest_size
    ;; p_memsz
    dq code_segment_rest_size
    ;; p_align
    dq segment_align

    ;; [2]
    ;; p_type, PT_LOAD
    dd 0x00000001
    ;; p_flags, R(4)W(2)
    dd 0x00000006
    ;; p_offset
    dq data_segment_begin-$$
    ;; p_vaddr,
    dq data_segment_begin
    ;; p_paddr
    dq 0
    ;; p_filesz
    dq data_segment_rest_size
    ;; p_memsz
    dq data_segment_rest_size
    ;; p_align
    dq segment_align


;;; Segments
    ;; Rest
section_strtab_begin:
sym_null:   db 0
sym_name_sym:   db ".sym", 0
sym_name_text:  db ".text", 0
sym_name_bss:   db ".bss", 0
sym_name_rodata:   db ".rodata", 0
section_strtab_end:

rest_size:  equ ($-$$)
    align segment_align
initial_segment_end:


    ;; Code
code_segment_begin:

    ;; --> text
section_text_begin:
_start:
    call load_code_from_stdin
    ;; call print_loaded_code_to_stdout

    call parse_code

    mov rax, 60
    mov rdi, [ret_code]
    syscall

load_code_from_stdin:
    xor rax, rax                ; 0 - read
    xor rdi, rdi                ; fd 0 - stdin
    mov rsi, g_code_buffer
    mov rdx, app_max_code_buffer_size
    syscall
    ret

print_loaded_code_to_stdout:
    mov rax, 1                  ; 1 - write
    mov rdi, 1                  ; fd 1 - stdout
    mov rsi, g_code_buffer
    mov rdx, app_max_code_buffer_size
    syscall
    ret

parse_code:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-40], 0       ; return
    ;; parser {
    ;;   char* buffer
    ;;   u64   offset
    ;;   bool  is_failed
    ;; }
    ;; rbp-32: char*, buffer
    mov qword [rbp-32], g_code_buffer
    ;; rbp-24: u64, offset
    mov qword [rbp-24], 0
    ;; rbp-16: bool, is_failed
    mov qword [rbp-16], 0
    ;; rbp-8: *parser
    lea rax, [rbp-32]
    mov [rbp-8], rax

    mov rdi, [rbp-8]
    call start_parse_code
    mov [rbp-40], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    mov rdi, [rbp-40]
    call sexp_print

    mov rax, [rbp-40]
    jmp .ok

.failed:
    mov rdi, text_error_failed_to_parse
    call runtime_print_string

    mov rdi, [rbp-8]
    call parser_get_index
    mov rdi, rax
    call runtime_print_uint64

    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.ok:
    leave
    ret

;;; rdi: *parser
start_parse_code:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    mov rdi, [rbp-8]
    call parser_skip_space

    mov rdi, [rbp-8]
    call parse_symbol
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    mov rdi, [rbp-24]
    call sexp_print

.loop_args:
    mov rdi, [rbp-8]
    call parser_skip_space

    mov rdi, [rbp-8]
    call parse_newline
    cmp rax, 0
    jne .succeeded

    ;; Reset failure of newline parsing
    mov rdi, [rbp-8]
    mov rsi, 0
    call parser_change_failed

    mov rdi, [rbp-8]
    call parse_expr
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    jmp .loop_args

.succeeded:
    mov rax, [rbp-24]
    jmp .ok

.failed:
    ;; revert
    ;;mov rdi, [rbp-8]
    ;;mov rsi, [rbp-16]
    ;;call parser_move_offset

.ok:
    leave
    ret

;;; rdi: *parser
parse_expr:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    mov rdi, [rbp-8]
    call parse_integer
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    mov rdi, [rbp-24]
    call sexp_print

.succeeded:
    mov rax, [rbp-24]
    jmp .ok

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

.ok:
    leave
    ret

;;;
parse_newline:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    mov rdi, [rbp-8]
    call parser_get_char

    cmp al, 0x0a                ; \n
    jne .failed

    ;; succeeded
    mov rdi, [rbp-8]
    call parser_step_char

    mov rax, 1
    jmp .finalize

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    xor rax, rax

.finalize:
    leave
    ret

;;;
parse_symbol:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov byte [rbp-24], 0        ; second-letter flag
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    mov rdi, [rbp-8]
    call parser_get_index
    mov r10, rax

.loop:
    mov rdi, [rbp-8]
    call parser_get_char

    ;; al >= 'A' && al <= 'Z'
    ;; al-'A' <= 'Z'-'A'(25)
    mov cl, al
    sub cl, 65                  ; 'A'
    cmp cl, 25
    jbe .ok

    ;; al >= 'a' && al <= 'z'
    ;; al-'a' <= 'z'-'a'(25)
    mov cl, al
    sub cl, 97                  ; 'a'
    cmp cl, 25
    jbe .ok

    ;; check if this character is the second character or later
    mov cl, [rbp-24]
    cmp cl, 0
    je .out_of_char

    ;; al >= '0' && al <= '9'
    ;; al-'0' <= '9'-'0'(9)
    mov cl, al
    sub cl, 48                  ; '0'
    cmp cl, 9
    jbe .ok

    jmp .out_of_char

.ok:
    mov rdi, [rbp-8]
    call parser_step_char

    mov byte [rbp-24], 1        ; second-flag
    jmp .loop

.out_of_char:
    ;; if current_offset - initial_offset == 0
    mov rdi, [rbp-8]
    call parser_get_index
    mov rcx, rax
    sub rcx, r10
    cmp rcx, 0
    je .not_matched

    mov rdi, [rbp-8]
    call parser_get_buffer
    mov rdi, rax
    add rdi, r10
    mov rsi, rcx
    call sexp_alloc_string

    jmp .finalize

.not_matched:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    mov rsi, 1
    call parser_change_failed

.finalize:
    leave
    ret

;;;
parse_integer:
    push rbp
    mov rbp, rsp
    sub rsp, 72

    mov qword [rbp-72], 1       ; radix^counter
    mov qword [rbp-64], 0       ; counter
    mov qword [rbp-56], 0       ; digits, u8 * 16
    mov qword [rbp-48], 0
    mov qword [rbp-40], 0       ; result
    mov qword [rbp-32], 10      ; radix
    mov byte [rbp-24], 0        ; second-letter flag
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;; read head
    mov rdi, [rbp-8]
    call parser_get_char

    ;; if al != '0'
    cmp al, 48                  ; '0'
    jne .loop

    mov rdi, [rbp-8]
    call parser_step_char

    mov rdi, [rbp-8]
    call parser_get_char

    ;; if al != 'x'
    cmp al, 120                 ; 'x'
    je .hex_digit

    jmp .not_matched

.hex_digit:
    mov qword [rbp-32], 16       ; radix

.loop:
    mov rdi, [rbp-8]
    call parser_get_char

    mov r10, 0                  ; last hex (NOT)
    ;; al >= '0' && al <= '9'
    ;; al-'0' <= '9'-'0'(9)
    mov cl, al
    sub cl, 48                  ; '0'
    cmp cl, 9
    jbe .ok

    mov r10, 1                  ; last hex

    jmp .out_of_char

.ok:
    mov al, cl

    mov rcx, [rbp-64]           ; counter
    cmp rcx, 16                 ; MAX_LENGTH of integer format
    je .not_matched

    cmp r10, 0
    je .ok_num

.ok_num:
    lea rdx, [rbp-56]           ; digits
    mov rcx, [rbp-64]           ; counter
    add rdx, rcx                ; &digits[counter]
    mov byte [rdx], al          ; digits[counter] = [0, 16)

    ;;
    mov rdi, [rbp-8]
    call parser_step_char

    ;; counter++
    mov rcx, [rbp-64]
    inc rcx
    mov [rbp-64], rcx

    jmp .loop

.out_of_char:
    ;; if counter == 0
    mov rcx, [rbp-64]
    cmp rcx, 0
    je .not_matched

    ;; calc sum of per digits
    mov rcx, [rbp-64]           ; counter
    dec rcx                     ; --counter

.sum_loop:
    lea rdx, [rbp-56]           ; digits
    add rdx, rcx

    xor rax, rax
    mov al, byte [rdx]          ; digits[counter]

    mov rdx, [rbp-72]           ; radix^counter
    mul rdx                     ; rax *= rdx

    mov rdx, [rbp-40]           ; result
    add rdx, rax
    mov [rbp-40], rdx

    mov rax, [rbp-72]           ; radix^counter
    mov rdx, [rbp-32]           ; radix
    mul rdx                     ; rax *= rdx
    mov [rbp-72], rax           ; radix^counter

    cmp rcx, 0
    je .sum_finish

    dec rcx
    jmp .sum_loop

.sum_finish
    mov rdi, [rbp-40]
    call sexp_alloc_int

    jmp .finalize

.not_matched:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    mov rsi, 1
    call parser_change_failed

.finalize:
    leave
    ret

;;; rdi: *parser
parser_get_index:
    mov rax, [rdi+8]            ; offset
    ret

;;; rdi: *parser
;;; rsi: u64
parser_move_offset:
    mov [rdi+8], rsi
    ret

;;; rdi: *parser
parser_get_buffer:
    mov rax, [rdi]              ; buffer
    ret

;;; rdi: *parser
parser_get_char:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; parser*

    ;; debug
    mov rdi, [rbp-8]
    mov rax, [rdi]
    add rax, [rdi+8]
    mov rdi, rax
    mov rsi, 1
    call runtime_print_string_view
    ;;

    mov rdi, [rbp-8]
    mov rcx, [rdi]              ; buffer
    call parser_get_index
    add rcx, rax
    mov al, [rcx]

    leave
    ret

;;; rdi: *parser
parser_is_failed:
    mov rax, [rdi+16]           ; failed
    and rax, 0x00000001
    ret

;;; rdi: *parser
;;; rsi: bool
parser_change_failed:
    mov [rdi+16], rsi
    ret

;;; rdi: *parser
parser_step_char:
    call parser_get_index
    mov rsi, rax
    inc rsi
    call parser_move_offset
    ret

;;; rdi: *parser
parser_skip_space:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; parser*

.loop:
    mov rdi, [rbp-8]
    call parser_get_char

    cmp al, 0x00
    je .break

    cmp al, 0x20                ; ' '
    je .found_space

    cmp al, 0x3b                ; ';'
    je .found_comment

    jmp .break

.found_space:
    mov rdi, [rbp-8]
    call parser_step_char

    jmp .loop

.found_comment:
    mov rdi, [rbp-8]
    call parser_skip_unlil_lf

    jmp .break

.break:
    leave
    ret


;;; rdi: *parser
parser_skip_unlil_lf:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; parser*

.loop:
    mov rdi, [rbp-8]
    call parser_get_char

    cmp al, 0x00
    je .break

    cmp al, 0x0a                ; '\n'
    je .break

    mov rdi, [rbp-8]
    call parser_step_char

    jmp .loop

.break:
    leave
    ret

;;;
;;; struct value {
;;;   u64       tag; (0: int, 1: string)
;;;   union {
;;;     u64 unum;
;;;     struct {
;;;       value* car
;;;       value* cdr
;;;     }
;;;   }
;;; }
;;; sizeof: 24
;;;
sexp_alloc:
    push rbp
    mov rbp, rsp

    mov rcx, [g_sexp_objects_count]
    cmp rcx, app_max_sexp_objects_count
    je sexp_cannot_alloc

    mov rax, rcx
    mov rdx, 24
    mul rdx
    mov rcx, g_sexp_objects
    add rax, rcx

    mov rcx, [g_sexp_objects_count]
    inc rcx
    mov [g_sexp_objects_count], rcx

    leave
    ret

sexp_cannot_alloc:
    ;; TODO
    mov rax, 60
    mov rdi, 2
    syscall

;;; rdi: int
sexp_alloc_int:
    call sexp_alloc
    mov rcx, rax

    mov qword [rcx], 0          ; tag
    mov [rcx+8], rdi

    ret

;;; rdi: *char
;;; rsi: u64
sexp_alloc_string:
    call sexp_alloc
    mov rcx, rax

    mov qword [rcx], 1          ; tag
    mov [rcx+8], rdi
    mov [rcx+16], rsi

    ret

sexp_print:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; *value

    mov rax, [rbp-8]
    cmp rax, 0
    je .print_nil

    mov rax, [rax]
    cmp rax, 0
    je .print_int

    cmp rax, 1
    je .print_string

    jmp .print_unknown

.print_nil:
    mov rdi, text_sexp_nil
    call runtime_print_string
    jmp .finalize

.print_unknown:
    mov rdi, text_sexp_unknown
    call runtime_print_string
    jmp .finalize

.print_int:
    mov rdi, text_sexp_int
    call runtime_print_string

    mov rax, [rbp-8]
    mov rdi, [rax+8]
    call runtime_print_uint64
    jmp .finalize

.print_string:
    mov rdi, text_sexp_string
    call runtime_print_string

    mov rax, [rbp-8]
    mov rdi, [rax+8]
    mov rsi, [rax+16]
    call runtime_print_string_view
    jmp .finalize

.finalize:
    leave
    ret


;;; rdi: u64
runtime_print_uint64:
    push rbp
    mov rbp, rsp
    sub rsp, 128

    mov rax, rdi

    mov rsi, rbp
    sub rsi, 1
    mov byte [rsi], 0           ; null-terminate

.count_digit_and_update_buffer:
    mov rdx, 0
    mov rcx, 10
    div rcx

    add rdx, 48                 ; to ascii -> '0' + number(rdx)
    dec rsi
    mov byte [rsi], dl

    cmp rax, 0
    jne .count_digit_and_update_buffer

    mov rdi, rsi
    call runtime_print_string

    leave
    ret


;;;
runtime_print_string:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi

    call runtime_strlen

    mov rdi, [rbp-8]
    mov rsi, rax
    call runtime_print_string_view

    leave
    ret

runtime_print_string_view:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov [rbp-16], rdi
    mov [rbp-8], rsi

    mov rax, 1                  ; 1 - write
    mov rdi, 1                  ; fd 1 - stdout
    mov rsi, [rbp-16]           ; buffer
    mov rdx, [rbp-8]            ; count
    syscall

    leave
    ret

runtime_print_newline:
    mov rdi, text_lf
    call runtime_print_string
    ret


;;;
runtime_strlen:
    push rbp
    mov rbp, rsp

    mov rax, 0
.count:
    mov cl, [rdi+rax]
    cmp cl, 0
    je .finish
    inc rax
    jmp .count

.finish:
    leave
    ret


runtime_exit:
    mov rax, 60
    syscall

section_text_end:
    ;; --< bss

code_segment_rest_size: equ ($-code_segment_begin)
    align segment_align
code_segment_end:

    ;; Data
data_segment_begin:

    ;; --> bss
section_bss_begin:

ret_code:   dq 42

g_code_buffer:  times app_max_code_buffer_size db 0

g_out_buffer:  times app_max_out_buffer_size db 0
g_out_buffer_size:  dq 0

g_sexp_objects:  resb 24 * app_max_sexp_objects_count
g_sexp_objects_count:  dq 0

section_bss_end:
    ;; --< bss

    ;; --> rodata
section_rodata_begin:

text_sexp_nil:  db "nil", 0
text_sexp_unknown:  db "unknown", 0
text_sexp_int:  db "int", 0
text_sexp_string:  db "string", 0
text_error_failed_to_parse: db "Failed to parse", 0
text_lf:     db 0x0a, 0

section_rodata_end:
    ;; --< rodata

data_segment_rest_size: equ ($-data_segment_begin)
    align segment_align
data_segment_end:


;;; Section headers
shdrs:
    ;; [0] NULL
    ;; sh_name
    dd 0x00000000
    ;; sh_type
    dd 0x00000000
    ;; sh_flags
    dq 0x0000000000000000
    ;; sh_addr
    dq 0x0000000000000000
    ;; sh_offset
    dq 0x0000000000000000
    ;; sh_size
    dq 0x0000000000000000
    ;; sh_link
    dd 0x00000000
    ;; sh_info
    dd 0x00000000
    ;; sh_addralign
    dq 0x0000000000000000
    ;; sh_entsize
    dq 0x0000000000000000

    ;; [1] .sym
    ;; sh_name
    dd sym_name_sym-section_strtab_begin
    ;; sh_type, SHT_STRTAB
    dd 0x00000003
    ;; sh_flags
    dq 0x0000000000000000
    ;; sh_addr
    dq 0x0000000000000000
    ;; sh_offset
    dq section_strtab_begin-$$
    ;; sh_size
    dq section_strtab_end-section_strtab_begin
    ;; sh_link
    dd 0x00000000
    ;; sh_info
    dd 0x00000000
    ;; sh_addralign
    dq 0x0000000000000001
    ;; sh_entsize
    dq 0x0000000000000000

    ;; [2] .text
    ;; sh_name
    dd sym_name_text-section_strtab_begin
    ;; sh_type, SHT_STRTAB
    dd 0x00000001
    ;; sh_flags, X(4)A(2)
    dq 0x0000000000000006
    ;; sh_addr
    dq section_text_begin
    ;; sh_offset
    dq section_text_begin-$$
    ;; sh_size
    dq section_text_end-section_text_begin
    ;; sh_link
    dd 0x00000000
    ;; sh_info
    dd 0x00000000
    ;; sh_addralign, 16
    dq 0x0000000000000010
    ;; sh_entsize
    dq 0x0000000000000000

    ;; [3] .bss
    ;; sh_name
    dd sym_name_bss-section_strtab_begin
    ;; sh_type, SHT_NOBITS
    dd 0x00000008
    ;; sh_flags, A(2)W(1)
    dq 0x0000000000000003
    ;; sh_addr
    dq section_bss_begin
    ;; sh_offset
    dq section_bss_begin-$$
    ;; sh_size
    dq section_bss_end-section_bss_begin
    ;; sh_link
    dd 0x00000000
    ;; sh_info
    dd 0x00000000
    ;; sh_addralign, 1
    dq 0x0000000000000001
    ;; sh_entsize
    dq 0x0000000000000000

    ;; [4] .rodata
    ;; sh_name
    dd sym_name_rodata-section_strtab_begin
    ;; sh_type, SHT_NOBITS
    dd 0x00000008
    ;; sh_flags, A(2)
    dq 0x0000000000000002
    ;; sh_addr
    dq section_rodata_begin
    ;; sh_offset
    dq section_rodata_begin-$$
    ;; sh_size
    dq section_rodata_end-section_rodata_begin
    ;; sh_link
    dd 0x00000000
    ;; sh_info
    dd 0x00000000
    ;; sh_addralign, 1
    dq 0x0000000000000001
    ;; sh_entsize
    dq 0x0000000000000000
