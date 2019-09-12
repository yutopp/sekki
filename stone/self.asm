    bits    64
    org     0x00400000          ; このあたりにマッピングしておきたい

segment_align:  equ 0x1000

app_max_code_buffer_size:   equ 0x10000
app_max_asm_buffer_size:    equ 0xf00000
app_max_sexp_objects_count: equ 0x80000
app_max_sexp_objects_size:  equ 0xc00000 ; 24 *
app_max_heap:   equ 0x8000

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

rest_size:  equ $-$$
    align segment_align
initial_segment_end:


    ;; Code
code_segment_begin:

    ;; --> text
section_text_begin:
_start:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    ;; parser {
    ;;   char* buffer                , 0
    ;;   u64   offset                , 8
    ;;   u64   status = 2  finished  , 16
    ;:                | 1  failed
    ;:                | 0  running
    ;;   sexp* last-label            , 24
    ;; }
    mov qword [rbp-48], g_code_buffer ; char*, buffer
    mov qword [rbp-40], 0       ; u64, offset
    mov qword [rbp-32], 0       ; u64, status
    mov qword [rbp-24], 0       ; sexp*, last-label

    mov qword [rbp-8], 0        ; statements

    call load_code_from_stdin
    ;; call print_loaded_code_to_stdout

    lea rdi, [rbp-48]           ; parser*
    call parse_code
    mov [rbp-8], rax

    mov rdi, [rbp-8]
    call sexp_print
    call runtime_print_newline

    ;; initialize pre-defined symbols

    ;; $
    mov rdi, str_g_symbol_doller
    call sexp_alloc_string
    mov [g_sexp_symbol_doller], rax

    ;; $$
    mov rdi, str_g_symbol_doller_doller
    call sexp_alloc_string
    mov [g_sexp_symbol_doller_doller], rax

    ;;
    mov rdi, [rbp-8]
    call asm_process_statements

    mov rdi, 0
    call runtime_exit

load_code_from_stdin:
    xor rax, rax                ; 0 - read
    xor rdi, rdi                ; fd 0 - stdin
    mov rsi, g_code_buffer
    mov rdx, app_max_code_buffer_size
    syscall

    mov [g_code_size], rax

    ret

print_loaded_code_to_stdout:
    mov rax, 1                  ; 1 - write
    mov rdi, 1                  ; fd 1 - stdout
    mov rsi, g_code_buffer
    mov rdx, [g_code_size]
    syscall
    ret


;;; rdi: parser*
parse_code:
    push rbp
    mov rbp, rsp
    sub rsp, 80

    mov qword [rbp-56], 0       ; statements     : (new-statement . nil)
    mov qword [rbp-48], 0       ; last-statement : (new-statement . nil)
    mov qword [rbp-40], 0       ; statement

    mov [rbp-8], rdi            ; parser*

.loop:
    mov rdi, [rbp-8]
    call parse_statement
    mov [rbp-40], rax           ; statement

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    mov rax, [rbp-40]           ; statement
    cmp rax, 0                  ; nil-statement
    je .loop_skip_updated

    ;; update statements-list
    mov rdi, [rbp-40]           ; statement
    mov rsi, 0                  ; nil
    call sexp_alloc_cons        ; (new-statement . nil)

    cmp qword [rbp-56], 0       ; statements
    jne .loop_skip_init

    mov [rbp-56], rax           ; statements     : (new-statement . nil)
    mov [rbp-48], rax           ; last-statement : (new-statement . nil)

    jmp .loop_skip_updated

.loop_skip_init:
    ;; chain
    mov rdi, [rbp-48]           ; last-statement : (last-statement . nil)
    mov rsi, rax                ; (new-statement . nil)
    call sexp_update_cdr        ; (last-statement . (new-statement . nil))
    mov [rbp-48], rax           ; last-statement : (new-statement . nil)

.loop_skip_updated:
    ;; debug, -> expr
    call runtime_print_newline
    mov rdi, str_debug_arrow_r
    call runtime_print_string
    mov rdi, [rbp-40]
    call sexp_print
    call runtime_print_newline
    ;;

    mov rdi, [rbp-8]
    call parser_is_finished
    cmp rax, 0
    jne .finished

    jmp .loop

.failed:
    mov rdi, text_error_failed_to_parse
    call runtime_print_string

    mov rdi, [rbp-8]
    call parser_get_index
    mov rdi, rax
    call runtime_print_uint64

    call runtime_print_newline

    mov rdi, 2
    call runtime_exit

.finished:
    mov rax, [rbp-56]           ; statements

.ok:
    leave
    ret

;;; rdi: *parser
parse_statement:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov qword [rbp-56], 0       ; tmp-arg
    mov qword [rbp-48], 0       ; last-args
    mov qword [rbp-40], 0       ; args
    mov qword [rbp-32], 0       ; second-or-later-flag
    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    ;;
    call parser_get_index
    mov [rbp-16], rax

    ;;
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; empty-statement
    mov rdi, [rbp-8]
    call parse_newline_or_term
    cmp rax, 0
    jne .succeeded

    ;; label-statement
    mov rdi, [rbp-8]
    call parse_label_statement
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .label_found

    ;; operation-statement
    ;; Reset failure of label-parsing
    mov rdi, [rbp-8]
    call parser_reset_failed

    mov rdi, [rbp-8]
    call parse_symbol
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

;    mov rdi, [rbp-24]
;    call sexp_print

.loop_args:
    mov rdi, [rbp-8]
    call parser_skip_space

    mov rdi, [rbp-8]
    call parse_newline_or_term
    cmp rax, 0
    jne .inst_found

    ;; Reset failure of newline parsing
    mov rdi, [rbp-8]
    call parser_reset_failed

    mov rdi, [rbp-32]           ; second-or-later-flag
    cmp rdi, 0
    je .loop_args_skip_comma

    ;;
    mov rdi, [rbp-8]
    call parse_comma
    cmp rax, 0
    je .failed

.loop_args_skip_comma:
    mov rdi, [rbp-8]
    call parser_skip_space

    mov qword [rbp-32], 1

    mov rdi, [rbp-8]
    call parse_expr
    mov [rbp-56], rax           ; tmp-arg

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    ;; append to args
    mov rdi, [rbp-56]
    mov rsi, 0
    call sexp_alloc_cons
    mov [rbp-56], rax           ; (value, nil)

    mov rax, [rbp-40]           ; args
    cmp rax, 0
    jne .update_args

    mov rax, [rbp-56]           ; (value, nil)
    mov [rbp-40], rax           ; args
    mov [rbp-48], rax           ; last-args

    jmp .loop_args

.update_args:
    mov rdi, [rbp-48]           ; last-args
    mov rsi, [rbp-56]           ; (value, nil)
    call sexp_update_cdr

    mov rax, [rbp-56]           ; (value, nil)
    mov [rbp-48], rax           ; last-args

    jmp .loop_args

.inst_found:
    ;; ((1, value), (args...) | nil)
    mov rdi, 1
    call sexp_alloc_int
    mov rdi, rax                ; 1
    mov rsi, [rbp-24]           ; value
    call sexp_alloc_cons
    mov rdi, rax                ; (1, value)
    mov rsi, [rbp-40]           ; (args...) | nil
    call sexp_alloc_cons
    mov [rbp-24], rax           ; ((1, value), (args...) | nil)
    jmp .succeeded

.label_found:
    mov rdi, [rbp-24]           ; (type . value)
    call tnode_label_is_local
    cmp rax, 0
    jne .skip_set_last_label

    mov rdi, [rbp-24]           ; (type . value)
    call tnode_value
    mov rdi, [rbp-8]            ; parser*
    mov rsi, rax
    call parser_set_last_label

.skip_set_last_label:
    mov rdi, [rbp-24]           ; (type . value)
    call tnode_value
    mov [rbp-24], rax           ; value

    ;; ((0, value), nil)
    mov rdi, 0
    call sexp_alloc_int
    mov rdi, rax                ; 0
    mov rsi, [rbp-24]           ; value
    call sexp_alloc_cons
    mov rdi, rax                ; (0, value)
    mov rsi, 0                  ; nil
    call sexp_alloc_cons
    mov [rbp-24], rax           ; ((0, value), nil)

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
    sub rsp, 32

    mov qword [rbp-32], 0       ; tmp_return
    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    mov rdi, [rbp-8]
    call parser_skip_space

    ;; addressing
    mov rdi, [rbp-8]
    call parse_addressing
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .break

    ;; reset
    mov rdi, [rbp-8]
    call parser_reset_failed

    ;; constant-expr
    mov rdi, [rbp-8]
    call parse_expr_add_sub
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .break

    ;; failed...
    jne .failed

.break:
;    mov rdi, [rbp-24]
;    call sexp_print

    mov rax, [rbp-24]
    jmp .ok

.failed:
    ;; revert
    ;mov rdi, [rbp-8]
    ;mov rsi, [rbp-16]
    ;call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.ok:
    leave
    ret


    ;; [REG] | [DISP]

    ;; [PAT*N] | [PAT+REG] | [REG+DISP]
    ;; [PAT*N+REG] | [PAT+REG+DISP]
    ;; [PAT*N+REG+DISP]
const_addr_pat_0_0_0_r:        equ 0x01
const_addr_pat_0_0_0_d:        equ 0x02
const_addr_pat_0_0_r_d:        equ 0x03
const_addr_pat_0_0_r_d_signed: equ 0x13
const_addr_pat_0_r_r_d:        equ 0x04
const_addr_pat_0_r_r_d_signed: equ 0x14
const_addr_pat_r_i_r_d:        equ 0x05
const_addr_pat_r_i_r_d_signed:  equ 0x15
const_addr_pat_0_r_i_r: equ 0x06
const_addr_pat_0_0_r_r:        equ 0x07

g_parse_addressing_pattern_size: equ 17

g_parse_addressing_pattern:
    ;; [REG]
    db 1, const_addr_pat_0_0_0_r
    db 0, 1 ; REG
    db 0, 0
    db 0, 0
    db 0, 0

    ;; [DISP]
    db 1, const_addr_pat_0_0_0_d
    db 0, 4 ; IMM
    db 0, 0
    db 0, 0
    db 0, 0

    db 1, const_addr_pat_0_0_0_d
    db 0, 2 ; LABEL
    db 0, 0
    db 0, 0
    db 0, 0

    ;; [PAT+REG]
    db 2, const_addr_pat_0_0_r_r
    db 0, 1 ; REG
    db 0, 1 ; + REG
    db 0, 0
    db 0, 0

    ;; [REG+DISP]
    db 2, const_addr_pat_0_0_r_d
    db 0, 1 ; REG
    db 0, 4 ; + IMM
    db 0, 0
    db 0, 0

    db 2, const_addr_pat_0_0_r_d_signed
    db 0, 1 ; REG
    db 1, 4 ; - IMM
    db 0, 0
    db 0, 0

    db 2, const_addr_pat_0_0_r_d
    db 0, 1 ; REG
    db 0, 2 ; + LABEL
    db 0, 0
    db 0, 0

    db 2, const_addr_pat_0_0_r_d_signed
    db 0, 1 ; REG
    db 1, 2 ; - LABEL
    db 0, 0
    db 0, 0

    ;; [PAT+REG+DISP]
    db 3, const_addr_pat_0_r_r_d
    db 0, 1 ; REG
    db 0, 1 ; + REG
    db 0, 4 ; + IMM
    db 0, 0

    db 3, const_addr_pat_0_r_r_d_signed
    db 0, 1 ; REG
    db 0, 1 ; + REG
    db 1, 4 ; - IMM
    db 0, 0

    db 3, const_addr_pat_0_r_r_d
    db 0, 1 ; REG
    db 0, 1 ; + REG
    db 0, 2 ; + LABEL
    db 0, 0

    db 3, const_addr_pat_0_r_r_d_signed
    db 0, 1 ; REG
    db 0, 1 ; + REG
    db 1, 2 ; - LABEL
    db 0, 0

    ;; [PAT*N+REG]
    db 3, const_addr_pat_0_r_i_r
    db 0, 1 ; REG
    db 2, 4 ; * IMM
    db 0, 1 ; + REG
    db 0, 0

    ;; [PAT*N+REG+DISP]
    db 4, const_addr_pat_r_i_r_d
    db 0, 1 ; REG
    db 2, 4 ; * IMM
    db 0, 1 ; + REG
    db 0, 4 ; + IMM

    db 4, const_addr_pat_r_i_r_d_signed
    db 0, 1 ; REG
    db 2, 4 ; * IMM
    db 0, 1 ; + REG
    db 1, 4 ; - IMM

    db 4, const_addr_pat_r_i_r_d
    db 0, 1 ; REG
    db 2, 4 ; * IMM
    db 0, 1 ; + REG
    db 0, 2 ; + IMM

    db 4, const_addr_pat_r_i_r_d_signed
    db 0, 1 ; REG
    db 2, 4 ; * IMM
    db 0, 1 ; + REG
    db 1, 2 ; - IMM

;;; rdi:
;;; rsi:
;;; rcx: counter
parse_check_addressing_pattern:
    push rbp
    mov rbp, rsp
    sub rsp, 144

    mov byte [rbp-48], 0        ; result
    mov qword [rbp-40], 0       ; counter

    mov [rbp-32], rcx           ; number
    mov [rbp-24], rdx           ; pattern*
    mov [rbp-16], rsi           ; op*
    mov [rbp-8], rdi            ; sexp**

    ;; check length
    mov rdi, [rbp-24]           ; pattern*
    mov al, [rdi]               ; length
    inc qword [rbp-24]          ; pattern* ++

    mov rcx, [rbp-32]           ; number
    cmp al, cl
    jne .failed

    ;; get pattern-enum
    mov rdi, [rbp-24]           ; pattern*
    mov al, [rdi]               ; length
    inc qword [rbp-24]          ; pattern* ++

    mov [rbp-48], al            ; result

.loop:
    mov rax, [rbp-40]           ; counter
    cmp rax, [rbp-32]           ; number
    je .matched

    ;; op-loaded
    mov rax, [rbp-16]
    mov rcx, [rbp-40]           ; counter
    mov cl, [rcx+rax]           ; op[]

    ;; pattern-op
    mov rdi, [rbp-24]           ; pattern*
    mov dl, [rdi]               ; length
    inc qword [rbp-24]          ; pattern* ++

    cmp dl, cl                  ; pattern-op != op
    jne .failed

    mov rax, [rbp-8]
    mov rcx, [rbp-40]           ; counter
    mov rdi, [rcx*8+rax]        ; sexp[]
    call tnode_print
    call runtime_print_newline

    ;; sexp*-loaded
    mov rax, [rbp-8]
    mov rcx, [rbp-40]           ; counter
    mov rdi, [rcx*8+rax]        ; sexp[]
    call tnode_type_tag

    ;; pattern-type
    mov rdi, [rbp-24]           ; pattern*
    mov dl, [rdi]               ; length
    inc qword [rbp-24]          ; pattern* ++

    cmp dl, al                  ; pattern-op != op
    jne .failed

    inc qword [rbp-40]          ; counter

    jmp .loop

.matched:
    xor rax, rax
    mov al, [rbp-48]            ; result
    jmp .break

.failed:
    xor rax, rax

.break:
    leave
    ret


;;; rdi: *parser
parse_addressing:
    push rbp
    mov rbp, rsp
    sub rsp, 144

    mov qword [rbp-144], 0      ; table-offset
    mov qword [rbp-136], 0      ; pattern-index
    mov qword [rbp-128], 0      ; kind

    ;; [scale-reg * scale-N + base-reg +/- disp]
    mov qword [rbp-120], 0      ; sexp*, scale-reg
    mov qword [rbp-112], 0      ; sexp*, scale-N
    mov qword [rbp-104], 0      ; sexp*, base-reg
    mov qword [rbp-96], 0       ; sexp*, disp

    mov qword [rbp-88], 0       ; sexp*, exprs[0]
    mov qword [rbp-80], 0       ; sexp*, exprs[1]
    mov qword [rbp-72], 0       ; sexp*, exprs[2]
    mov qword [rbp-64], 0       ; sexp*, exprs[3]

    ;; 0 = plus, 1 = minus, 2 = multiply
    mov byte [rbp-56], 0        ; ops[0], not-used
    mov byte [rbp-55], 0        ; ops[1]
    mov byte [rbp-54], 0        ; ops[2]
    mov byte [rbp-53], 0        ; ops[3]

    mov qword [rbp-48], 0       ; counter

    mov qword [rbp-40], 0       ; expected-size. 0 is unknown
    mov qword [rbp-32], 0
    mov qword [rbp-24], 0       ; sexp*, candidate
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;; addressing

    ;; normal register
    mov rdi, [rbp-8]
    call parse_reg_value
    mov [rbp-24], rax           ; candidate

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded

    ;;

    ;; reset failure
    mov rdi, [rbp-8]
    call parser_reset_failed

    ;; size-prefix
    mov rdi, [rbp-8]
    call parse_size
    mov qword [rbp-40], rax     ; size = .

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    ;;
    mov rdi, [rbp-8]
    call parser_skip_space

    mov qword [rbp-48], 0       ; count = 0

    ;; [
    mov rdi, [rbp-8]
    call parse_bracket_l
    cmp rax, 0
    je .failed

    ;; constant / reg
    mov rdi, [rbp-8]
    call parse_reg_or_constant
    mov rcx, [rbp-48]           ; counter
    lea rdx, [rbp-88]           ; sexp**
    mov [rcx*8+rdx], rax        ; exprs[counter] = expr

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    inc qword [rbp-48]          ; counter++

.loop:
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; + / - / *
    mov rdi, [rbp-8]
    call parse_plus
    cmp rax, 0
    jne .op_plus

    ;; + / - / *
    mov rdi, [rbp-8]
    call parse_minus
    cmp rax, 0
    jne .op_minus

    ;; + / - / *
    mov rdi, [rbp-8]
    call parse_multiply
    cmp rax, 0
    jne .op_multiply

    jmp .enclose

.op_plus:
    mov rcx, [rbp-48]           ; counter
    lea rdx, [rbp-56]           ; ops*
    mov byte [rcx+rdx], 0       ; ops[counter] = plus
    jmp .next_value

.op_minus:
    mov rcx, [rbp-48]           ; counter
    lea rdx, [rbp-56]           ; ops*
    mov byte [rcx+rdx], 1       ; ops[counter] = minus
    jmp .next_value

.op_multiply:
    mov rcx, [rbp-48]           ; counter
    lea rdx, [rbp-56]           ; ops*
    mov byte [rcx+rdx], 2       ; ops[counter] = multiply
    jmp .next_value

.next_value:
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; constant / reg
    mov rdi, [rbp-8]
    call parse_reg_or_constant
    mov rcx, [rbp-48]           ; counter
    lea rdx, [rbp-88]           ; sexp**
    mov [rcx*8+rdx], rax        ; exprs[counter] = expr

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    inc qword [rbp-48]          ; counter++

    jmp .loop

.enclose:
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; ]
    mov rdi, [rbp-8]
    call parse_bracket_r
    cmp rax, 0
    je .failed

.parsed:
;    ;; <--
;    mov rdi, str_debug_parse_addressing
;    call runtime_print_string
;    call runtime_print_newline
;
;    mov rdi, [rbp-88]           ; 0
;    call sexp_print
;    call runtime_print_newline
;
;    mov rdi, [rbp-80]           ; 1
;    call sexp_print
;    call runtime_print_newline
;
;    mov rdi, [rbp-72]           ; 2
;    call sexp_print
;    call runtime_print_newline
;
;    mov rdi, [rbp-64]           ; 3
;    call sexp_print
;    call runtime_print_newline
;    ;; -->

    mov qword [rbp-144], g_parse_addressing_pattern
    mov qword [rbp-136], 0      ; match-index

.match_loop:
    cmp qword [rbp-136], g_parse_addressing_pattern_size
    je .failed

    lea rdi, [rbp-88]
    lea rsi, [rbp-56]
    mov rdx, [rbp-144]
    mov rcx, [rbp-48]
    call parse_check_addressing_pattern
    cmp rax, 0
    jne .pattern_matched

    add qword [rbp-144], 10     ; template += 10
    inc qword [rbp-136]

    jmp .match_loop

.pattern_matched:
    mov [rbp-128], rax        ; kind
    and rax, 0x0f             ; kind.type-mask

    cmp rax, const_addr_pat_0_0_0_r
    je .pat_0_0_0_r

    cmp rax, const_addr_pat_0_0_0_d
    je .pat_0_0_0_d

    cmp rax, const_addr_pat_0_0_r_r
    je .pat_0_0_r_r

    cmp rax, const_addr_pat_0_0_r_d
    je .pat_0_0_r_d

    cmp rax, const_addr_pat_0_r_r_d
    je .pat_0_r_r_d

    cmp rax, const_addr_pat_0_r_i_r
    je .pat_0_r_i_r

    ;; ICE
    mov rdi, 101
    call runtime_exit

.pat_0_0_0_r:
    mov rax, [rbp-88]           ; exprs[0]
    mov [rbp-104], rax          ; base-reg

    jmp .matched

.pat_0_0_0_d:
    mov rax, [rbp-88]           ; exprs[0]
    mov [rbp-96], rax           ; disp

    jmp .matched

.pat_0_0_r_r:
    mov rax, [rbp-88]           ; exprs[0]
    mov [rbp-120], rax          ; scale-reg
    mov rax, [rbp-80]           ; exprs[1]
    mov [rbp-104], rax          ; base-reg

    jmp .matched

.pat_0_0_r_d:
    mov rax, [rbp-88]           ; exprs[0]
    mov [rbp-104], rax          ; base-reg
    mov rax, [rbp-80]           ; exprs[1]
    mov [rbp-96], rax           ; disp

    jmp .matched

.pat_0_r_r_d:
    mov rax, [rbp-88]           ; exprs[0]
    mov [rbp-120], rax          ; scale-reg
    mov rax, [rbp-80]           ; exprs[1]
    mov [rbp-104], rax          ; base-reg
    mov rax, [rbp-72]           ; exprs[2]
    mov [rbp-96], rax           ; disp

    jmp .matched

.pat_0_r_i_r:
    mov rax, [rbp-88]           ; exprs[0]
    mov [rbp-120], rax          ; scale-reg
    mov rax, [rbp-80]           ; exprs[1]
    mov [rbp-112], rax          ; scale-N
    mov rax, [rbp-72]           ; exprs[2]
    mov [rbp-104], rax          ; base-reg

    jmp .matched

.matched:
    ;; <--
    mov rdi, str_debug_parse_addressing
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [rbp-120]           ; 0
    call sexp_print
    call runtime_print_newline

    mov rdi, [rbp-112]           ; 1
    call sexp_print
    call runtime_print_newline

    mov rdi, [rbp-104]           ; 2
    call sexp_print
    call runtime_print_newline

    mov rdi, [rbp-96]           ; 3
    call sexp_print
    call runtime_print_newline
    ;; -->

    mov rdi, [rbp-96]         ; args[3]
    mov rsi, 0                ; nil
    call sexp_alloc_cons
    mov rdi, [rbp-104]        ; args[2]
    mov rsi, rax
    call sexp_alloc_cons
    mov rdi, [rbp-112]        ; args[1]
    mov rsi, rax              ; ( . )
    call sexp_alloc_cons
    mov rdi, [rbp-120]        ; args[0]
    mov rsi, rax              ; ( . )
    call sexp_alloc_cons
    mov [rbp-24], rax         ; tmp

    ;; value-tag
    ;;         ------4|1|1|1|1|
    ;;                    | | 5 = addressing
    ;;                    | ?   = size
    ;;                    ?     = kind
    mov rdi, 0x0000000000000005

    mov rax, [rbp-40]           ; size
    and rax, 0x00000000000000ff
    shl rax, 8                  ; 1 * 8 bits
    or rdi, rax                 ; set size to value-tag

    mov rax, [rbp-128]          ; kind
    and rax, 0x00000000000000ff
    shl rax, 16                 ; 2 * 8 bits
    or rdi, rax                 ; set kind to value-tag

    call sexp_alloc_int

    mov rdi, rax
    mov rsi, [rbp-24]           ; tmp
    call sexp_alloc_cons
    mov [rbp-24], rax

    mov rdi, rax
    call tnode_print
    call runtime_print_newline

    jmp .succeeded

.succeeded:
    mov rax, [rbp-24]
    jmp .break

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.break:
    leave
    ret


;;; rdi: *parser
;;; expr (+|- expr)*
parse_expr_add_sub:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov byte [rbp-40], 0        ; op, add = 0, sub = 1
    mov qword [rbp-32], 0       ; tmp-return-value
    mov qword [rbp-24], 0       ; return-value
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;;
    mov rdi, [rbp-8]
    call parse_expr_primitive
    mov [rbp-24], rax           ; return-value

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

.loop:
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; -
    mov rdi, [rbp-8]
    call parse_minus
    cmp rax, 0
    jne .found_mid_op_minus

    ;; +
    mov rdi, [rbp-8]
    call parse_plus
    cmp rax, 0
    jne .found_mid_op_plus

    jmp .break

.found_mid_op_minus:
    mov byte [rbp-40], 1        ; op, sub
    jmp .found_mid_op

.found_mid_op_plus:
    mov byte [rbp-40], 0        ; op, add

.found_mid_op:
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; expr
    mov rdi, [rbp-8]
    call parse_expr
    mov [rbp-32], rax           ; tmp-return-value

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    ;; (lhs . rhs)
    mov rdi, [rbp-24]           ; lhs = return-value
    mov rsi, [rbp-32]           ; rhs = tmp-return-value
    call sexp_alloc_cons
    mov [rbp-24], rax           ; expr-cons

    ;; value-tag
    ;;         |-----4||-2||-2|
    ;;         |      ||  ||  |
    ;;                        8 = expr
    ;;                    ?     = op: 0 = add, 1 = sub
    mov rdi, 0x0000000000000008

    xor rax, rax
    mov al, [rbp-40]            ; op
    shl rax, 16                 ; 2 * 8 bits
    or rdi, rax                 ; set kind to value-tag

    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; expr-cons
    call sexp_alloc_cons
    mov [rbp-24], rax

    jmp .loop

.break:
;    mov rdi, [rbp-24]
;    call sexp_print

    mov rax, [rbp-24]
    jmp .ok

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.ok:
    leave
    ret

;;;
parse_expr_primitive:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax



    ;; constant-value
    ;; Reset failure of register parsing
    mov rdi, [rbp-8]
    call parser_reset_failed

    mov rdi, [rbp-8]
    call parse_constant_value
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded

    ;; failed...
    jmp .failed

.succeeded:
;    mov rdi, [rbp-24]
;    call sexp_print

    mov rax, [rbp-24]
    jmp .ok

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.ok:
    leave
    ret


;;;
parse_reg_or_constant:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; parser*

    call parser_get_index
    mov [rbp-16], rax

    ;; register
    mov rdi, [rbp-8]
    call parse_reg_value
    mov [rbp-24], rax           ; return

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded

    mov rdi, [rbp-8]
    call parser_reset_failed

    ;; constant
    mov rdi, [rbp-8]
    call parse_constant_value
    mov [rbp-24], rax           ; return

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded

    ;; failed...
    jmp .failed

.succeeded:
    mov rax, [rbp-24]
    jmp .break

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.break:
    leave
    ret

;;;
;;; (N, value) where S(32bits):N(32bits) == 0
;;;                                       |
parse_constant_value:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;; string
    mov rdi, [rbp-8]
    call parse_string
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded_string

    ;; label
    mov rdi, [rbp-8]
    call parser_reset_failed

    mov rdi, [rbp-8]
    call parse_label
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded_label

    ;; integer
    mov rdi, [rbp-8]
    call parser_reset_failed

    mov rdi, [rbp-8]
    call parse_integer
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded_integer

    ;; failed...
    jmp .failed

.succeeded_label:
    jmp .succeeded

.succeeded_string:
    ;; value-tag
    ;;         |-----4||-2||-2|
    ;;         |      ||  ||  |
    ;;                        3 = string
    mov rdi, 0x0000000000000003
    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; string
    call sexp_alloc_cons
    mov [rbp-24], rax

    jmp .succeeded

.succeeded_integer:
    mov rdi, [rbp-24]           ; sexp*, int
    mov rsi, 0
    call tnode_alloc_uint_node
    mov [rbp-24], rax

    jmp .succeeded

.succeeded:
;    mov rdi, [rbp-24]
;    call sexp_print

    mov rax, [rbp-24]
    jmp .break

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.break:
    leave
    ret


;;; rdi:
;;; rsi:
parse_one_char:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], rsi     ; u64,
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    mov rdi, [rbp-8]
    call parser_get_char

    mov rdx, [rbp-24]
    cmp al, dl
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

;;; rdi:
parse_newline:
    mov rsi, 0x0a               ; '\n'
    call parse_one_char
    ret

;;; rdi:
parse_newline_or_term:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    mov qword [rbp-16], 0             ;
    mov [rbp-8], rdi            ; *parser

    mov rdi, [rbp-8]
    call parse_newline
    mov [rbp-16], rax
    cmp rax, 0
    jne .break                  ; found

    mov rdi, [rbp-8]
    mov rsi, 0x00               ; '\0'
    call parse_one_char
    mov [rbp-16], rax
    cmp rax, 0
    je .break                   ; not-found

    ;;
    mov rdi, [rbp-8]
    call parser_set_finished

.break:
    mov rax, [rbp-16]
    leave
    ret

;;; rdi:
parse_colon:
    mov rsi, 0x3a               ; ':'
    call parse_one_char
    ret

;;; rdi:
parse_dot:
    mov rsi, 0x2e               ; '.'
    call parse_one_char
    ret

;;; rdi:
parse_comma:
    mov rsi, 0x2c               ; ','
    call parse_one_char
    ret

;;; rdi:
parse_double_quote:
    mov rsi, 0x22               ; '\"'
    call parse_one_char
    ret

;;; rdi:
parse_minus:
    mov rsi, 0x2d               ; '-'
    call parse_one_char
    ret

;;; rdi:
parse_plus:
    mov rsi, 0x2b               ; '+'
    call parse_one_char
    ret

;;; rdi:
parse_multiply:
    mov rsi, 0x2a               ; '*'
    call parse_one_char
    ret

;;; rdi:
parse_bracket_l:
    mov rsi, 0x5b               ; '['
    call parse_one_char
    ret

;;; rdi:
parse_bracket_r:
    mov rsi, 0x5d               ; ']'
    call parse_one_char
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
    mov [rbp-16], rax
    mov r10, rax

.loop:
    mov rdi, [rbp-8]
    call parser_get_char

    cmp al, 0x5f                ; '_'
    je .ok

    cmp al, 0x24                ; '$'
    je .ok

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
    call sexp_alloc_string_view

    jmp .finalize

.not_matched:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.finalize:
    leave
    ret

;;;
parse_string:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; result
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;;
    mov rdi, [rbp-8]
    call parse_double_quote
    cmp rax, 0
    je .not_matched             ;

    ;; "abc"
    ;;  ^
    mov rdi, [rbp-8]
    call parser_get_index
    mov r10, rax

.loop:
    ;;
    mov rdi, [rbp-8]
    call parse_double_quote
    cmp rax, 0
    jne .matched

    ;; step contents
    mov rdi, [rbp-8]
    call parser_step_char

    jmp .loop

.not_matched:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

    jmp .finalize

.matched:
    mov rdi, [rbp-8]
    call parser_get_index
    mov rcx, rax
    dec rcx                     ; "

    sub rcx, r10                ; begin - end

    mov rdi, [rbp-8]
    call parser_get_buffer
    mov rdi, rax
    add rdi, r10
    mov rsi, rcx
    call sexp_alloc_string_view
    mov qword [rbp-24], rax

    mov rax, qword [rbp-24]

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

    ;; only-zero
    mov qword [rbp-64], 1
    jmp .out_of_char

.hex_digit:
    mov qword [rbp-32], 16       ; radix

    mov rdi, [rbp-8]
    call parser_step_char

.loop:
    mov rdi, [rbp-8]
    call parser_get_char

    mov r10, 0                  ; last hex (NOT)
    ;; al >= '0' && al <= '9'
    ;; al-'0' <= 9('9'-'0')
    mov cl, al
    sub cl, 48                  ; '0'
    cmp cl, 9
    jbe .ok

    ;;
    mov rdi, qword [rbp-32]
    cmp rdi, 16
    jne .out_of_char

    mov r10, 1                  ; last hex
    ;; al >= 'a' && al <= 'f'
    ;; al-'a' <= 5('f'-'a')
    mov cl, al
    sub cl, 0x61                  ; 'a'
    cmp cl, 5
    jbe .ok

    jmp .out_of_char

.ok:
    mov al, cl

    mov rcx, [rbp-64]           ; counter
    cmp rcx, 16                 ; MAX_LENGTH of integer format
    je .not_matched

    cmp r10, 0
    je .ok_num

    ;; Add 10 for hex-num, because base are stored as zero
    add al, 10

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

.sum_finish:
    mov rdi, [rbp-40]
    call sexp_alloc_int

    jmp .finalize

.not_matched:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

.finalize:
    leave
    ret


;;;
parse_size:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; node | hash
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;;
    mov rdi, [rbp-8]
    call parser_skip_space

    ;;
    mov rdi, [rbp-8]
    call parse_symbol
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .skip

    ;;
    mov rdi, [rbp-24]           ; symbol
    mov rsi, str_size_byte
    call sexp_internal_string_cmp
    cmp rax, 0
    jne .size_byte

    ;;
    mov rdi, [rbp-24]           ; symbol
    mov rsi, str_size_word
    call sexp_internal_string_cmp
    cmp rax, 0
    jne .size_word

    ;;
    mov rdi, [rbp-24]           ; symbol
    mov rsi, str_size_dword
    call sexp_internal_string_cmp
    cmp rax, 0
    jne .size_dword

    ;;
    mov rdi, [rbp-24]           ; symbol
    mov rsi, str_size_qword
    call sexp_internal_string_cmp
    cmp rax, 0
    jne .size_qword

    ;; failed...
    jmp .failed

.size_byte:
    mov rax, 1                  ; sizeof(byte) = 1
    jmp .break

.size_word:
    mov rax, 2                  ; sizeof(word) = 2
    jmp .break

.size_dword:
    mov rax, 4                  ; sizeof(dword) = 4
    jmp .break

.size_qword:
    mov rax, 8                  ; sizeof(qword) = 8
    jmp .break

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

    jmp .break

.skip:
    ;; Reset failure of symbol parsing
    mov rdi, [rbp-8]
    call parser_reset_failed

    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    xor rax, rax                ; 0, unknown-size

.break:
    leave
    ret


;;;
parse_reg_value:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-40], 0       ; register-index
    mov qword [rbp-32], 0       ; hash
    mov qword [rbp-24], 0       ; symbol, return-value
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;;
    mov rdi, [rbp-8]
    call parser_skip_space

    ;;
    mov rdi, [rbp-8]
    call parse_symbol
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    ;; find register
    mov rax, g_register_table
    mov [rbp-40], rax

.find_register_loop:
    mov rax, [rbp-40]           ; table*
    mov rax, [rax]
    cmp rax, 0
    je .failed                  ; not-found

    mov rdi, [rbp-24]           ; symbol
    mov rsi, rax                ; char*, register-name
    call sexp_internal_string_cmp
    cmp rax, 0
    jne .found_register

    ;; step table-entries
    mov rax, [rbp-40]           ; table*
    add rax, 32                 ; 8 * 4
    mov [rbp-40], rax

    jmp .find_register_loop

.found_register:
    mov rax, [rbp-40]           ; table*
    mov rdi, [rax+8]            ; register-index
    mov rsi, [rax+16]           ; register-size
    mov rdx, [rax+24]           ; extended

    ;; value-tag
    ;;         ------4|1|1|1|1|
    ;;                  | | | 1 = register
    ;;                  | | ?   = size
    ;;                  | ?     = register-index
    ;;                  ?       = extended
    mov rax, 0x0000000000000001

    mov rcx, rsi                ; size
    and rcx, 0x00000000000000ff
    shl rcx, 8                  ; 1 * 8 bits
    or rax, rcx

    mov rcx, rdi                ; register-index
    and rcx, 0x00000000000000ff
    shl rcx, 16                 ; 2 * 8 bits
    or rax, rcx                 ; set kind to value-tag

    mov rcx, rdx
    and rcx, 0x00000000000000ff
    shl rcx, 24                 ; 3 * 8 bits
    or rax, rcx                 ; set extended to value-tag

    mov rdi, rax
    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; symbol
    call sexp_alloc_cons
    mov [rbp-24], rax

    jmp .ok

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

    jmp .break

.ok:
    mov rax, [rbp-24]           ; return-value

.break:
    leave
    ret

;;;
parse_label_statement:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; node
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;; label
    mov rdi, [rbp-8]
    call parse_label
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    ;;
    mov rdi, [rbp-8]
    call parser_skip_space

    ;;
    mov rdi, [rbp-8]
    call parse_colon
    cmp rax, 0
    je .failed

    jmp .ok

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

    jmp .break

.ok:
    mov rax, [rbp-24]

.break:
    leave
    ret


;;;
parse_label:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov qword [rbp-56], 0       ; char*, new-label-ptr
    mov qword [rbp-48], 0       ; last-label
    mov qword [rbp-40], 0       ; label-size
    mov qword [rbp-32], 0       ; is_local
    mov qword [rbp-24], 0       ; node
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;; .
    mov rdi, [rbp-8]
    call parse_dot
    cmp rax, 0
    je .skip

    ;; local-label
    mov qword [rbp-32], 1       ; is_local

.skip:
    ;; symbol
    mov rdi, [rbp-8]
    call parse_symbol
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    cmp qword [rbp-32], 0       ; is_local
    je .ok

    ;; make local-name
    mov rdi, [rbp-8]            ; parser*
    call parser_get_last_label
    cmp rax, 0
    jne .has_last_label

    mov rdi, 0                  ; nil
    mov rsi, 0                  ; len = 0
    call sexp_alloc_string_view

.has_last_label:
    mov [rbp-48], rax           ; last-label

    mov rdi, [rbp-24]           ; symbol
    call sexp_internal_string_length
    add [rbp-40], rax           ; label-size += len(symbol)

    mov rdi, [rbp-48]           ; last-label
    call sexp_internal_string_length
    add [rbp-40], rax           ; label-size += len(last-label)

    add qword [rbp-40], 2       ; label-size += len(".") & null-term

    mov rdi, [rbp-40]           ; label-size
    call runtime_malloc
    mov [rbp-56], rax           ; new-label-ptr

    ;; last
    mov rdi, [rbp-48]           ; last-label
    call sexp_internal_string_ptr
    mov [rbp-64], rax           ; last-label-ptr

    mov rdi, [rbp-48]           ; last-label
    call sexp_internal_string_length
    mov [rbp-72], rax           ; last-label-length

    ;; current
    mov rdi, [rbp-24]           ; current-label
    call sexp_internal_string_ptr
    mov [rbp-80], rax           ; current-label-ptr

    mov rdi, [rbp-24]           ; current-label
    call sexp_internal_string_length
    mov [rbp-88], rax           ; current-label-length

    ;;
    mov rdi, [rbp-56]           ; new-label-ptr
    mov rsi, [rbp-40]           ; label-size
    sub rsi, 1                  ; null-term
    mov rdx, [rbp-64]           ; last-label-ptr
    mov rcx, [rbp-72]           ; last-label-length
    call runtime_strcpy

    ;;
    mov rdi, [rbp-56]           ; new-label-ptr
    add rdi, [rbp-72]
    mov rsi, [rbp-40]           ; label-size
    sub rsi, 1                  ; null-term
    sub rsi, [rbp-72]           ; last-label-length
    mov rdx, str_dot
    mov rcx, 1
    call runtime_strcpy

    ;;
    mov rdi, [rbp-56]           ; new-label-ptr
    add rdi, [rbp-72]
    add rdi, 1
    mov rsi, [rbp-40]           ; label-size
    sub rsi, 1                  ; null-term
    sub rsi, [rbp-72]           ; last-label-length
    sub rsi, 1
    mov rdx, [rbp-80]
    mov rcx, [rbp-88]
    call runtime_strcpy

    mov rdi, [rbp-56]           ; new-label-ptr
    call sexp_alloc_string
    mov [rbp-24], rax

    jmp .ok

.failed:
    ;; revert
    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call parser_move_offset

    mov rdi, [rbp-8]
    call parser_set_failed

    jmp .break

.ok:
    ;; value-tag
    ;;         ------4|1|1|1|1|
    ;;         |          | | |
    ;;                    | | 2 = label
    ;;                    | 0   = size-unknown
    ;;                    ?     = is-local
    mov rdi, 0x0000000000000002

    mov rax, qword [rbp-32]     ; is_local
    and rax, 0x00000000000000ff
    shl rax, 16                 ; 2 * 8 bits
    or rdi, rax

    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; symbol
    call sexp_alloc_cons

.break:
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
;    mov rdi, [rbp-8]
;    mov rax, [rdi]
;    add rax, [rdi+8]
;    mov rdi, rax
;    mov rsi, 1
;    call runtime_print_string_view
    ;;

    mov rdi, [rbp-8]
    mov rcx, [rdi]              ; buffer
    call parser_get_index
    add rcx, rax
    mov rax, [rcx]

    leave
    ret

;;; rdi: *parser
parser_is_failed:
    mov rax, [rdi+16]           ; status
    and rax, 0x0000000000000001
    ret

;;; rdi: *parser
parser_reset_failed:
    mov rax, [rdi+16]           ; status
    and rax, 0xfffffffffffffffe ; mask failed
    mov [rdi+16], rax
    ret

;;; rdi: *parser
parser_set_failed:
    mov rax, [rdi+16]           ; status
    or rax, 0x0000000000000001
    mov [rdi+16], rax
    ret

;;; rdi: *parser
parser_is_finished:
    mov rax, [rdi+16]           ; status
    and rax, 0x00000002
    ret

;;; rdi: *parser
parser_set_finished:
    mov rax, [rdi+16]           ; status
    or rax, 0x00000002
    mov [rdi+16], rax
    ret

;;; rdi: parser*
;;; rsi: sexp*, symbol
parser_set_last_label:
    mov [rdi+24], rsi           ; last-symbol
    ret

;;; rdi: parser*
parser_get_last_label:
    mov rax, [rdi+24]           ; last-symbol
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


;;; rdi, sexp*
;;; rsi: u64, expected-size
tnode_alloc_uint_node:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov qword [rbp-32], 0       ; not-shrinkable
    mov [rbp-16], rsi           ; size
    mov [rbp-8], rdi            ; sexp*, value

    cmp qword [rbp-16], 0       ; size
    jne .skip_not_shrinkable

    mov qword [rbp-32], 1       ; not-shrinkable

.skip_not_shrinkable:
;    call sexp_internal_int_value ; value
;    cmp rax, 0x7f
;    jle .size_1                 ; rax <= 0x7f, sizeof(1)
;
;    cmp rax, 0x7fff
;    jle .size_2                 ; rax <= 0x7fff, sizeof(2)
;
;    cmp rax, 0x7fffffff
;    jle .size_4                 ; rax <= 0x7fffffff, sizeof(4)
;
;    jmp .size_8                 ; otherwise, sizeof(2)
;
;.size_1:
;    mov qword [rbp-16], 1       ; size
;    jmp .alloc
;
;.size_2:
;    mov qword [rbp-16], 2       ; size
;    jmp .alloc
;
;.size_4:
;    mov qword [rbp-16], 4       ; size
;    jmp .alloc
;
;.size_8:
;    mov qword [rbp-16], 8       ; size
;    jmp .alloc
;
;.alloc:
    ;; value-tag
    ;;         ------4|--2|1|1|
    ;;                        4 = int
    ;;                      ?   = size
    mov rdi, 0x0000000000000004

    mov rax, [rbp-16]           ; size
    and rax, 0x00000000000000ff
    shl rax, 8                  ; 8 * 1 bits
    or rdi, rax                 ; size

    ;;         ------4|--------
    mov rax, [rbp-32]           ; not-shrinkable
    and rax, 0x00000000000000ff
    shl rax, 32                 ; 8 * 4 bits
    or rdi, rax

    ;; tnode
    call sexp_alloc_int

    mov rdi, rax                ; value-tag
    mov rsi, [rbp-8]            ; value
    call sexp_alloc_cons

    leave
    ret


;;; rdi: sexp** args*
tnode_step_args:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov qword [rbp-16], 0       ; return-value
    mov [rbp-8], rdi            ; (type . value)*: sexp**

    mov rdi, [rbp-8]            ; sexp**
    mov rdi, [rdi]              ; sexp*, (hd . rest)
    call sexp_car               ; hd
    mov [rbp-16], rax           ; return-value

    mov rdi, [rbp-8]            ; sexp**
    mov rdi, [rdi]              ; sexp*, (hd . rest)
    call sexp_cdr               ; rest
    mov rdi, [rbp-8]
    mov [rdi], rax              ; update sexp* in sexp**

    mov rax, [rbp-16]           ; return-value

    leave
    ret


;;; rdi, sexp*
tnode_print:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov [rbp-8], rdi            ; value

    mov rdi, [rbp-8]
    call sexp_print

    mov rdi, [rbp-8]
    call tnode_calc_imm_size
    mov rdi, rax
    call runtime_print_uint64

    mov rdi, str_comma
    call runtime_print_string

    mov rdi, [rbp-8]
    call tnode_type_tag
    mov rdi, rax
    call runtime_print_uint64

    leave
    ret

;;; rdi: sexp*, (type . value)
;;; -> sexp*
tnode_type_value:
    call sexp_car
    ret

;;; rdi: sexp*, (type . value)
;;; -> u64, type
tnode_type:
    call tnode_type_value
    mov rdi, rax
    call sexp_internal_int_value ; value-tag
    ret

;;; rdi: sexp*, (type . value)
;;; -> u64, type-tag
tnode_type_tag:
    call tnode_type
    and rax, 0x00000000000000ff ; type-tag
    ret

;;; rdi: sexp*, (type . value)
;;; -> u64, type-size
tnode_type_size:
    call tnode_type
    shr rax, 8                  ; 8 * 1 bits
    and rax, 0x00000000000000ff ; type-size
    ret

;;; rdi: sexp*, (type . value)
;;; -> u64, type-size
tnode_type_not_shrinkable:
    call tnode_type
    shr rax, 32                 ; 8 * 4 bits
    and rax, 0x00000000000000ff ;
    ret

;;; rdi: sexp*, (type . value)
;;; -> sexp*, value
tnode_value:
    call sexp_cdr
    ret


;;; rdi: sexp*, (type . value)
;;; rsi: allow 64bits for imm
tnode_calc_imm_size:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; size

    mov [rbp-16], rsi           ; allow 64bits for imm
    mov [rbp-8], rdi            ; value

    mov rdi, [rbp-8]            ; value
    call tnode_type_size
    cmp rax, 0
    jne .break

    mov [rbp-24], rax           ; size

    ;; inspect size if value is number
    mov rdi, [rbp-8]
    call tnode_type_tag
    cmp rax, 4                  ; integer
    je .calc_integer_size

    ;; fallback
    mov rax, [rbp-24]           ; size
    jmp .break

.calc_integer_size:
    mov rdi, [rbp-8]
    call tnode_value            ; value
    mov rdi, rax
    call sexp_internal_int_value ; N

    cmp rax, 0                  ; rax >= 0
    jge .compare

    neg rax                     ; abs

.compare:
    cmp rax, 0x7f
    jle .size_1                 ; rax <= 0x7f, sizeof(1)

    cmp rax, 0x7fff
    jle .size_2                 ; rax <= 0x7fff, sizeof(2)

    cmp rax, 0x7fffffff
    jle .size_4                 ; rax <= 0x7fffffff, sizeof(4)

    jmp .size_8

.size_1:
    mov rax, 1
    jmp .break

.size_2:
    mov rax, 2
    jmp .break

.size_4:
    mov rax, 4
    jmp .break

.size_8:
    mov rax, 8
    jmp .break

.break:
    leave
    ret

;;; rdi: sexp*, (type . value)
;;; -> u64, kind
tnode_addr_kind:
    call tnode_type
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x00000000000000ff ; kind
    ret

;;; rdi: sexp*, (type . value)
;;; -> u64, index
tnode_reg_index:
    call tnode_type
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x00000000000000ff ; register-index
    ret

;;; rdi: sexp*, (type . value)
;;; -> u64, index
tnode_reg_extended:
    call tnode_type
    shr rax, 24                 ; 3 * 8 bits
    and rax, 0x00000000000000ff ; register-index
    ret

;;; rdi: sexp*, (type . value)
tnode_label_is_local:
    call tnode_type
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x00000000000000ff ; is-local
    ret

;;; rdi: sexp*
;;; rsi: u64, size
tnode_size_updated:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-16], 0       ; sexp*, type
    mov [rbp-8], rdi            ; sexp*

    call tnode_type
    mov rcx, 0x000000000000ff00
    not rcx
    and rax, rcx                ; clear type-size
    mov rcx, rsi
    shl rcx, 8                  ; 8 * 1 bits
    or rax, rcx                 ; type-size

    mov rdi, rax                ; type-size
    call sexp_alloc_int
    mov [rbp-16], rax

    mov rdi, [rbp-8]            ; sexp*, (type . value)
    call sexp_cdr               ; value

    mov rdi, [rbp-16]           ; type
    mov rsi, rax
    call sexp_alloc_cons

    leave
    ret

;;; rdi: sexp* statements
asm_process_statements:
    push rbp
    mov rbp, rsp
    sub rsp, 160

    ;; asm {
    ;;   sexp* labels
    ;;   sexp* replacements
    ;;   sexp* last-label
    ;;   u64   segment-base
    ;;   u64   inst-index
    ;;   byte  phase
    ;;   u32   current-inst-size
    ;;   ..pads..
    ;;   sexp* consts
    ;;   inst  inst
    ;;   u64
    ;;   char*
    ;;   u64
    ;; }
    mov qword [rbp-160], 0       ; sexp*, nil, asm.labels           0
    mov qword [rbp-152], 0       ; sexp*, nil, asm.replacements     8
    mov qword [rbp-144], 0       ; sexp*, nil, last-label           16
    mov qword [rbp-136], 0       ; u64, 0    , segment-base         24
    mov qword [rbp-128], 0       ; u64, 0    , inst-index           32
    mov byte [rbp-120], 0        ; u8, 0     , phase                40
    mov byte [rbp-116], 0        ; u8, 0     , current-inst-size    44
    mov qword [rbp-112], 0       ; sexp*, nil, asm.consts           48
    mov qword [rbp-104], 0       ; inst(sizeof(32)), inst           56
    mov qword [rbp-72], 0        ; latest-inst-cost                 88
    mov qword [rbp-64], 0        ; buffer*                          96
    mov qword [rbp-56], 0        ; buffer-size                      104

    mov qword [rbp-32], 0       ; fd
    lea rax, [rbp-160]
    mov [rbp-16], rax           ; asm*
    mov [rbp-8], rdi            ; statements*

    mov rdi, str_dev_zero       ; filename
    mov rsi, 0                  ; READ
    call runtime_open
    mov [rbp-32], rax           ; fd

    mov rdi, 0x0                ; addr
    mov rsi, app_max_asm_buffer_size ; length
    mov rdx, 0x03               ; 0x0011 = WRITE | READ
    mov rcx, 0x2                ; flags = MAP_PRIVATE
    mov r8, [rbp-32]            ; fd
    call runtime_mmap

    cmp rax, -1
    je .failed_to_allocate_buffer

    mov rdi, [rbp-16]           ; asm*
    mov [rdi+96], rax           ; asm.buffer

    mov rdi, [rbp-16]           ; asm*
    mov rsi, [rbp-8]            ; statements*
    call asm_process_statements_phase0

.loop:
    mov rdi, [rbp-16]           ; asm*
    mov rsi, [rbp-8]            ; statements*
    call asm_process_statements_phase1
    cmp rax, 0
    je .break

    jmp .loop

.failed_to_allocate_buffer:
    mov rdi, 2
    call runtime_exit

.break:
    ;; TODO
    ;;mov rdi, [rbp-88]           ; asm.labels
    ;;call sexp_print
    ;;call runtime_print_newline
    ;;
    ;;mov rdi, [rbp-80]           ; asm.replacements
    ;;call sexp_print   ;
    ;;call runtime_print_newline
    ;;
    ;;mov rdi, [rbp-16]           ; asm*
    ;;call asm_fill_replacements

    mov rdi, [rbp-16]           ; asm*
    call asm_print_to_stderr

    leave
    ret

;;; rdi: asm*
asm_print_to_stderr:
    mov rax, rdi
    mov rdi, 2                  ; fd = strerr
    mov rsi, [rax+96]           ; char* = asm.buffer
    mov rdx, [rax+104]          ; length = asm.buffer-size
    call runtime_write
    ret

;;; rdi: asm*
;;; rsi: sexp* statements
asm_process_statements_phase0:
    push rbp
    mov rbp, rsp
    sub rsp, 72

    mov [rbp-16], rsi           ; sexp* statements
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-8]            ; asm*
    mov byte [rdi+40], 0        ; asm.phase = 0

    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call asm_process_statements_loop

    leave
    ret

;;; rdi: asm*
;;; rsi: sexp* statements
asm_process_statements_phase1:
    push rbp
    mov rbp, rsp
    sub rsp, 72

    mov [rbp-16], rsi           ; sexp* statements
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-8]            ; asm*
    mov byte [rdi+40], 1        ; asm.phase = 0

    mov rdi, [rbp-8]
    mov rsi, [rbp-16]
    call asm_process_statements_loop

    leave
    ret

;;; rdi: asm*
;;; rsi: sexp* statements
asm_process_statements_loop:
    push rbp
    mov rbp, rsp
    sub rsp, 72

    mov [rbp-16], rsi           ; sexp* statements
    mov [rbp-8], rdi            ; asm*

    ;; init
    mov rdi, [rbp-8]            ; asm*
    mov qword [rdi+104], 0      ; asm.buffer-size

    mov rdi, [rbp-8]            ; asm*
    mov qword [rdi+16], 0       ; asm.last-label
    mov qword [rdi+32], 0       ; asm.inst-index

.loop:
    mov rdi, [rbp-16]           ; statements
    cmp rdi, 0
    je .finish

    mov rdi, [rbp-8]            ; asm*
    call asm_inst_init

    mov rdi, [rbp-8]            ; asm*
    mov qword [rdi+88], 0xff    ; asm.latest-inst-cost

    mov rdi, [rbp-8]            ; asm*
    mov dword [rdi+44], 0       ; clear asm.current-inst-size

    mov rdi, [rbp-16]           ; statements
    call sexp_car

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; statement
    call asm_process_statement

    ;; write inst
    mov rdi, [rbp-8]           ; asm*
    call asm_inst_write

    ;; write inst-size
    mov rdi, [rbp-8]            ; asm*
    mov eax, [rdi+44]           ; asm.current-inst-size

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; size
    call asm_set_inst_size
    cmp rax, 0
    jne .break

    ;; step statements
    mov rdi, [rbp-16]           ; statements
    call sexp_cdr
    mov [rbp-16], rax           ; statements

    ;; step inst-size
    mov rdi, [rbp-8]            ; asm*
    call asm_step_inst_index

    jmp .loop

.finish:
    xor rax, rax

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: sexp* statement
asm_process_statement:
    push rbp
    mov rbp, rsp
    sub rsp, 72

    mov qword [rbp-40], 0       ; tmp
    mov qword [rbp-32], 0       ; args
    mov qword [rbp-24], 0       ; symbol (OR hash)
    mov [rbp-16], rsi           ; sexp* statement
    mov [rbp-8], rdi            ; asm*

    ;; debug, -> expr
    call runtime_print_newline
    mov rdi, str_debug_arrow_r
    call runtime_print_string
    mov rdi, [rbp-16]
    call sexp_print
    call runtime_print_newline
    ;;

    ;; nil
    ;; ((0, value), nil)
    ;; ((1, value), (args...) | nil)

    mov rdi, [rbp-16]
    cmp rdi, 0
    je .break

    call sexp_car               ; (N, value) as car
    mov [rbp-40], rax           ;

    mov rdi, [rbp-16]
    call sexp_cdr               ; nil | (args...) as cdr
    mov [rbp-32], rax

    mov rdi, [rbp-40]           ; (N, value)
    call sexp_cdr               ; value
    mov [rbp-24], rax

    mov rdi, [rbp-40]           ; (N, value)
    call sexp_car               ; N
    mov rdi, rax
    call sexp_internal_int_value

    cmp rax, 0
    je .label

    cmp rax, 1
    je .inst

.failed:
    mov rdi, str_ice_invalid_statement
    call runtime_print_string

    mov rdi, 2
    call runtime_exit

.label:
    mov rdi, [rbp-8]             ; asm*
    call asm_current_inst_index

    mov rdi, rax                ; inst-index
    call sexp_alloc_int
    mov rdi, rax
    mov rsi, 0                  ; not-shrinkable
    call tnode_alloc_uint_node

    mov rdi, [rbp-8]            ; asm*
    mov cl, [rdi+40]            ; asm.phase != 0
    cmp cl, 0
    jne .label_skip_updation

    ;; update labels
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; symbol
    mov rdx, rax                ; (type . inst-index)
    call asm_add_labels

.label_skip_updation:
    ;; set last-label
    mov rdi, [rbp-24]           ; symbol
    mov rcx, [rbp-8]            ; asm*
    mov [rcx+16], rdi           ; asm.last_label

    jmp .break

.inst:
    mov qword [rbp-48], g_asm_inst_table ; table.entry*

.inst_find_loop:
    mov rax, [rbp-48]           ; table.entry*
    mov rax, [rax]              ; char*, inst_str_table
    cmp rax, 0
    je .inst_invalid_inst

    ;; find inst
    mov rdi, [rbp-24]           ; symbol
    mov rsi, rax                ; inst_str_table
    call sexp_internal_string_cmp
    cmp rax, 0
    jne .inst_found

    ;; step entries
    mov rax, [rbp-48]           ; table.entry*
    add rax, 24                 ; 8 * 3
    mov [rbp-48], rax

    jmp .inst_find_loop

.inst_found:
    mov rax, [rbp-48]           ; table.entry*

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; args
    mov rdx, [rax+16]           ; aux args
    mov rcx, [rax+8]            ; inst-addr
    call rcx

    jmp .break

.inst_invalid_inst:
    mov rdi, str_ice_invalid_inst
    call runtime_print_string

    call runtime_print_newline

    mov rdi, 2
    call runtime_exit

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: inst*
;;; rdx: cost
asm_update_inst_if_efficient:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov [rbp-32], rdx           ; inst-cost
    mov qword [rbp-24], 0       ; inst-size
    mov [rbp-16], rsi           ; inst*
    mov [rbp-8], rdi            ; asm*

    ;; debug
    call runtime_print_newline
    call runtime_print_newline
    mov rdi, [rbp-8]            ; asm*
    call asm_inst_current_size
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-16]           ; inst*
    call inst_current_size
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-8]            ; asm*
    mov rdi, [rdi+88]           ; asm.latest-inst-cost
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-32]           ; inst-cost
    call runtime_print_uint64
    call runtime_print_newline
    call runtime_print_newline
    ;; debug

    mov rax, [rbp-32]           ; inst-cost
    mov rdi, [rbp-8]            ; asm*
    cmp rax, [rdi+88]           ; inst-cost > asm.latest-inst-cost
    jg .break

    mov [rdi+88], rax           ; asm.latest-inst-cost = inst-cost

    ;; check-inst
    mov rdi, [rbp-8]            ; asm*
    call asm_inst_current_size
    mov [rbp-24], rax
    cmp qword [rbp-24], 0
    je .update

    mov rdi, [rbp-16]           ; inst*
    call inst_current_size
    cmp rax, [rbp-24]           ; size(inst) < size(asm.inst)
    jl .update

    jmp .break

.update:
    ;; [0, 8)
    mov rcx, [rbp-16]           ; inst*
    mov rcx, [rcx]              ; inst[0, 8)
    mov rax, [rbp-8]            ; asm*
    mov [rax+56], rcx           ; asm.inst[0, 8), 56

    ;; [8, 16)
    mov rcx, [rbp-16]           ; inst*
    mov rcx, [rcx+8]            ; inst[0, 8)
    mov rax, [rbp-8]            ; asm*
    mov [rax+64], rcx           ; asm.inst[0, 8), 56

    ;; [16, 24)
    mov rcx, [rbp-16]           ; inst*
    mov rcx, [rcx+16]           ; inst[0, 8)
    mov rax, [rbp-8]            ; asm*
    mov [rax+72], rcx           ; asm.inst[0, 8), 56

    ;; [24, 32)
    mov rcx, [rbp-16]           ; inst*
    mov rcx, [rcx+24]           ; inst[0, 8)
    mov rax, [rbp-8]            ; asm*
    mov [rax+80], rcx           ; asm.inst[24, 32), [80, 88)

.break:
    leave
    ret


;;; rdi: sexp*(tnode), lhs, dst
;;; rsi: sexp*(tnode), rhs, src
asm_infer_size_ds:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-40], 0       ; size

    mov byte [rbp-32], 0        ; src.size
    mov [rbp-24], rsi           ; src

    mov byte [rbp-16], 0        ; dst.size
    mov [rbp-8], rdi            ; dst

    ;; dst
    mov rdi, [rbp-8]            ; dst
    call tnode_type_size
    mov byte [rbp-16], al       ; dst.size

    ;; src
    mov rdi, [rbp-24]           ; src
    call tnode_type_size
    mov byte [rbp-32], al       ; src.size

    mov al, byte [rbp-16]       ; dst.size
    cmp al, [rbp-32]            ; dst.size <= src.size
    jle .overflow

    xor rax, rax
    mov al, [rbp-32]            ; src.size
    jmp .break

.overflow:
    mov rax, 1                  ; maybe overflow, set dummy-size

.break:
    leave
    ret


;;; rdi: sexp*(tnode), lhs
;;; rsi: sexp*(tnode), rhs
asm_infer_size:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-40], 0       ; size

    mov byte [rbp-32], 0        ; rhs.size
    mov byte [rbp-31], 0        ; rhs.type_tag
    mov [rbp-24], rsi           ; rhs

    mov byte [rbp-16], 0        ; lhs.size
    mov byte [rbp-15], 0        ; lhs.type_tag
    mov [rbp-8], rdi            ; lhs

    ;; lhs
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_tag
    mov byte [rbp-15], al       ; lhs.type_tag

    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    mov byte [rbp-16], al       ; lhs.size

    ;; rhs
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_tag
    mov byte [rbp-31], al       ; rhs.type_tag

    mov rdi, [rbp-24]           ; rhs
    call tnode_type_size
    mov byte [rbp-32], al       ; rhs.size




    ;; size
    xor rax, rax
    mov al, [rbp-16]            ; lhs.size
    shl rax, 8                  ; 0xLL00
    xor rcx, rcx
    mov cl, [rbp-32]            ; rhs.size
    or ax, cx                   ; 0xLLRR
    mov [rbp-40], rax           ; size

    ;; check
    ;; lhs
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_not_shrinkable
    cmp rax, 0
    jne .not_shrinkable

    mov al, [rbp-15]            ; lhs.type_tag
    cmp al, 5                   ; addressing
    je .not_shrinkable

    ;; rhs
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_not_shrinkable
    cmp rax, 0
    jne .not_shrinkable

    mov al, [rbp-31]            ; rhs.type_tag
    cmp al, 5                   ; addressing
    je .not_shrinkable

.shrinkable:
    ;; size-matching
    mov rax, [rbp-40]           ; size

    cmp ax, 0x0808              ; 8, 8 -> 8
    je .bits64

    cmp ax, 0x0800              ; 8, 0 -> 8
    je .bits64

    cmp ax, 0x0804              ; 8, 4 -> 4
    je .bits32

    cmp ax, 0x0802              ; 8, 2 -> 4
    je .bits32

    cmp ax, 0x0801              ; 8, 1 -> 4
    je .bits32

    cmp ax, 0x0101              ; 1, 1 -> 1
    je .bits8

    jmp .size_undecidable

.not_shrinkable:
    ;; size-matching
    mov rax, [rbp-40]           ; size

    cmp ax, 0x0808              ; 8, 8 -> 8
    je .bits64

    cmp ax, 0x0800              ; 8, 0 -> 8
    je .bits64

    cmp ax, 0x0804              ; 8, 4 -> 8
    je .bits64

    cmp ax, 0x0802              ; 8, 2 -> 8
    je .bits64

    cmp ax, 0x0801              ; 8, 1 -> 8
    je .bits64

    cmp ax, 0x0101              ; 1, 1 -> 1
    je .bits8

    jmp .size_undecidable

.bits8:
    mov rax, 1
    jmp .break

.bits32:
    mov rax, 4
    jmp .break

.bits64:
    mov rax, 8
    jmp .break

.size_undecidable:
    mov rdi, str_error_unsupported_oprands_size
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [rbp-40]           ; size
    shr rdi, 8
    and rdi, 0xff
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-40]           ; size
    and rdi, 0xff
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-40]           ; size
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, 2                  ; debug
    call runtime_exit

.break:
    leave
    ret


;;; rdi
;;; rsi
asm_calc_inst_size:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-16], 0       ; size

    ;; size
    mov rax, rdi                ; lhs.size
    and rax, 0xff
    shl rax, 8                  ; 0xLL00
    mov rcx, rsi                ; rhs.size
    and rcx, 0xff
    or ax, cx                   ; 0xLLRR
    mov [rbp-16], rax           ; size

    ;;
    mov rax, [rbp-16]           ; size

    cmp ax, 0x0808              ; 8, 8 -> 8
    je .bits64

    cmp ax, 0x0800              ; 8, 0 -> 8
    je .bits64

    cmp ax, 0x0804              ; 8, 4 -> 4
    je .bits32

    cmp ax, 0x0802              ; 8, 2 -> 4
    je .bits32

    cmp ax, 0x0801              ; 8, 1 -> 4
    je .bits32

    cmp ax, 0x0101              ; 1, 1 -> 1
    je .bits8

    jmp .size_undecidable

.bits8:
    mov rax, 1
    jmp .break

.bits32:
    mov rax, 4
    jmp .break

.bits64:
    mov rax, 8
    jmp .break

.size_undecidable:
    mov rdi, str_error_unsupported_oprands_size
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [rbp-40]           ; size
    shr rdi, 8
    and rdi, 0xff
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-40]           ; size
    and rdi, 0xff
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-40]           ; size
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, 2                  ; debug
    call runtime_exit

.break:
    leave
    ret


;;; rdi: sexp*(tnode), lhs
;;; rsi: sexp*(tnode), rhs

;;; reg, N    -> rax, sizeof(8) = 8
;;;           -> r8 , sizeof(4) = 8
;;;           -> rax, sizeof(4) = 4
;;; reg, []   ->
asm_infer_size_r_i:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-40], 0       ; size

    mov byte [rbp-32], 0        ; rhs.size
    mov byte [rbp-31], 0        ; rhs.type_tag
    mov [rbp-24], rsi           ; rhs

    mov byte [rbp-16], 0        ; lhs.size
    mov byte [rbp-15], 0        ; lhs.type_tag
    mov [rbp-8], rdi            ; lhs

    ;; lhs
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_tag
    mov byte [rbp-15], al       ; lhs.type_tag

    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    mov byte [rbp-16], al       ; lhs.size

    ;; rhs
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_tag
    mov byte [rbp-31], al       ; rhs.type_tag

    mov rdi, [rbp-24]           ; rhs
    call tnode_type_size
    mov byte [rbp-32], al       ; rhs.size

    ;; lhs-type
    cmp byte [rbp-15], 1        ; register
    je .lhs_reg

    cmp byte [rbp-15], 5        ; addressing
    je .lhs_addressing

    ;;
    mov rdi, 50
    call runtime_exit

.lhs_reg:
    ;; recalc-rhs-size
    mov rdi, [rbp-24]           ; rhs
    call tnode_calc_imm_size
    mov byte [rbp-32], al       ; rhs.size

    xor rax, rax
    mov al, byte [rbp-16]       ; lhs.size
    mov rdi, rax
    xor rax, rax
    mov al, byte [rbp-32]       ; rhs.size
    mov rsi, rax
    call asm_calc_inst_size

    jmp .break

.lhs_addressing:


.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_mov:
    mov rdx, g_asm_inst_template_mov
    call asm_write_inst_from_template
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_xor:
    mov rdx, g_asm_inst_template_xor
    call asm_write_inst_from_template
    ret

;;; rdi: asm*
;;; rsi: args
asm_write_inst_push:
    mov rdx, g_asm_inst_template_push
    call asm_write_inst_from_template
    ret

;;; rdi: asm*
;;; rsi: args
asm_write_inst_pop:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov qword [rbp-48], 0       ; size

    mov qword [rbp-24], 0       ; args[1], sexp*(tnode)

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; args[1]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-24], rax           ; args[1]

    ;; args[1] inspect
    mov rdi, [rbp-24]
    call tnode_type_tag

    cmp rax, 5                  ; addressing
    je .encode_m

    cmp rax, 1                  ; register
    je .encode_o

    mov rdi, 10                 ; debug
    call runtime_exit

    ;; POP r/m
.encode_m:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0x8f               ; POP r/m
    call asm_inst_append_opcode

    mov rdi, [rbp-8]            ; asm*
    mov rsi, 6                  ; mod r/m, /r-degit
    mov rdx, [rbp-24]           ; args[1], effective-reg
    call asm_inst_set_r_digit_operands

    jmp .break

;; POP r
.encode_o:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0x58               ; POP r
    call asm_inst_append_opcode

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1]
    call asm_inst_add_reg_to_opcode

    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_cmp:
    mov rdx, g_asm_inst_template_cmp
    call asm_write_inst_from_template
    ret


;;; inst_pattern { type, size, value, aux } = sizeof(8)

;;; rdi: pattern*
inst_pattern_print:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; pattern*

    ;; type
    mov rdi, [rbp-8]
    call inst_pattern_get_type
    mov rdi, rax
    call runtime_print_uint64
    mov rdi, str_comma
    call runtime_print_string

    ;; type-narrow
    mov rdi, [rbp-8]
    call inst_pattern_get_type_narrow
    mov rdi, rax
    call runtime_print_uint64
    mov rdi, str_comma
    call runtime_print_string

    ;; size
    mov rdi, [rbp-8]
    call inst_pattern_get_size
    mov rdi, rax
    call runtime_print_uint64
    mov rdi, str_comma
    call runtime_print_string

    ;; value
    mov rdi, [rbp-8]
    call inst_pattern_get_value
    mov rdi, rax
    call runtime_print_uint64
    mov rdi, str_comma
    call runtime_print_string

    ;; aux
    mov rdi, [rbp-8]
    call inst_pattern_get_aux
    mov rdi, rax
    call runtime_print_uint64

    call runtime_print_newline

    leave
    ret

;;; rdi: pattern*
;;; rsi:
inst_pattern_set_type:
    mov rax, rsi                ; value
    mov byte [rdi+3], al
    ret

;;; rdi: pattern*
inst_pattern_get_type:
    mov cl, [rdi+3]
    xor rax, rax
    mov al, cl
    and al, 0x0f
    ret

;;; rdi: pattern*
inst_pattern_get_type_narrow:
    mov cl, [rdi+3]
    xor rax, rax
    mov al, cl
    and al, 0xf0
    ret

;;; rdi: pattern*
;;; rsi:
inst_pattern_set_size:
    mov rax, rsi                ; value
    mov byte [rdi+2], al
    ret

;;; rdi: pattern*
inst_pattern_get_size:
    mov cl, [rdi+2]
    xor rax, rax
    mov al, cl
    ret

;;; rdi: pattern*
;;; rsi:
inst_pattern_set_value:
    mov rax, rsi                ; value
    mov byte [rdi+1], al
    ret

;;; rdi: pattern*
inst_pattern_get_value:
    mov cl, [rdi+1]
    xor rax, rax
    mov al, cl
    ret

;;; rdi: pattern*
;;; rsi:
inst_pattern_set_aux:
    mov rax, rsi                ; value
    mov byte [rdi], al
    ret

;;; rdi: pattern*
inst_pattern_get_aux:
    mov cl, [rdi]
    xor rax, rax
    mov al, cl
    ret

;;; rdi: sexp*(tnode)
;;; rsi: pattern*
inst_pattern_updated_tnode:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; sexp*

    mov rdi, rsi                ; pattern*
    call inst_pattern_get_size

    mov rdi, [rbp-8]            ; sexp*
    mov rsi, rax                ; size
    call tnode_size_updated

    leave
    ret

;;; rdi: byte*
;;; rsi: pattern*
inst_pattern_read:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-16], rsi           ; pattern*
    mov [rbp-8], rdi            ; byte*

    ;;
    mov rax, [rbp-8]            ; byte*
    mov al, byte [rax]
    mov rdi, [rbp-16]           ; pattern*
    mov rsi, rax
    call inst_pattern_set_type

    ;;
    mov rax, [rbp-8]            ; byte*
    mov al, byte [rax+1]
    mov rdi, [rbp-16]           ; pattern*
    mov rsi, rax
    call inst_pattern_set_size

    ;;
    mov rax, [rbp-8]            ; byte*
    mov al, byte [rax+2]
    mov rdi, [rbp-16]           ; pattern*
    mov rsi, rax
    call inst_pattern_set_value

    ;;
    mov rax, [rbp-8]            ; byte*
    mov al, byte [rax+3]
    mov rdi, [rbp-16]           ; pattern*
    mov rsi, rax
    call inst_pattern_set_aux

    leave
    ret


;;; rdi: pattern*
;;; rsi: pattern*
;;; rdx: template-metadata
inst_pattern_is_matched:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-40], rdx           ; template-metadata

    mov byte [rbp-24], 0        ; actual-type
    mov byte [rbp-23], 0        ; expected-type

    mov byte [rbp-22], 0        ; actual-size
    mov byte [rbp-21], 0        ; expected-size

    mov [rbp-16], rsi           ; pattern*, actual
    mov [rbp-8], rdi            ; pattern*, expected

    ;; check type

    mov rdi, [rbp-8]            ; expected-pattern
    call inst_pattern_get_type
    mov [rbp-23], al            ; expected-type

    mov rdi, [rbp-16]           ; actual-pattern
    call inst_pattern_get_type
    mov [rbp-24], al            ; actual-type

    cmp byte [rbp-23], 1        ; expected = register
    je .expect_register

    cmp byte [rbp-23], 5        ; expected = addressing
    je .expect_addressing

    cmp byte [rbp-23], 4        ; expected = imm
    je .expect_imm

    jmp .unexpected

.expect_register:
    cmp byte [rbp-24], 1        ; actual = register
    je .type_matched

    jmp .not_matched

.expect_addressing:
    cmp byte [rbp-24], 5        ; actual = addressing
    je .type_matched

    cmp byte [rbp-24], 1        ; actual = register
    je .type_matched

    jmp .not_matched

.expect_imm:
    cmp byte [rbp-24], 4        ; actual = imm
    je .type_matched

    jmp .not_matched

.type_matched:
    ;; check size

    mov rdi, [rbp-8]            ; expected-pattern
    call inst_pattern_get_size
    mov [rbp-21], al            ; expected-size

    mov rdi, [rbp-16]           ; actual-pattern
    call inst_pattern_get_size
    mov [rbp-22], al            ; actual-size

    cmp qword [rbp-40], 0       ; template-metadata == 0
    je .skip_optimizable_size_check

    cmp byte [rbp-23], 1        ; expected-type = register
    je .size_check_32bits_included

;    cmp byte [rbp-23], 4        ; expected = imm
;    je .size_check_32bits_included

.skip_optimizable_size_check:
    cmp byte [rbp-23], 4        ; expected = imm
    je .size_check_included

.size_check_exact:
    mov al, [rbp-21]            ; expected
    cmp al, [rbp-22]            ; expected == actual
    je .size_matched

    jmp .not_matched

.size_check_32bits_included:
    mov al, [rbp-21]            ; expected-size
    cmp al, [rbp-22]            ; expected-size == actual-size
    je .size_matched

    mov al, [rbp-21]            ; expected-size
    cmp al, 4                   ; expected-size == 4
    je .size_check_32bits_included_allow_64bits

    jmp .not_matched

.size_check_32bits_included_allow_64bits:
    mov al, [rbp-22]            ; actual
    cmp al, 8                   ; actual == 8
    je .matched

    jmp .not_matched

.size_check_included:
    mov al, [rbp-21]            ; expected
    cmp al, [rbp-22]            ; expected >= actual
    jge .size_matched

    jmp .not_matched

.size_matched:
    jmp .matched

.unexpected:
    mov rdi, 32
    call runtime_exit

.matched:
    mov rax, 1
    jmp .break

.not_matched:
    xor rax, rax

.break:
    leave
    ret


;;; reg, N    -> rax, sizeof(8) = 8
;;;           -> r8 , sizeof(4) = 8
;;;           -> rax, sizeof(4) = 4
;;; reg, []   ->

;;; rdi: sexp*(tnode)
;;; rsi: sexp*(tnode)
;;; rdx: pattern*
;;; rcx: template-metadata
asm_infer_size_x_x:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov [rbp-48], rcx           ; template-metadata
    mov [rbp-40], rdx           ; pattern*
    mov [rbp-24], rsi           ; rhs
    mov byte [rbp-15], 0        ; lhs.type_tag
    mov [rbp-8], rdi            ; lhs

    ;; lhs
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_tag
    mov byte [rbp-15], al       ; lhs.type_tag

    ;; rhs
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_size

    ;; update

    ;; set-lhs-type
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_tag         ;
    mov rdi, [rbp-40]           ; pattern*
    mov rsi, rax                ; type
    call inst_pattern_set_type

;    ;; set-lhs-size
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    mov rdi, [rbp-40]           ; pattern*
    mov rsi, rax                ; size
    call inst_pattern_set_size

    ;; type-check

    ;; lhs-type
    cmp byte [rbp-15], 1        ; register
    je .lhs_reg

    cmp byte [rbp-15], 5        ; addressing
    je .lhs_addressing

    ;;
    mov rdi, 52
    call runtime_exit

.lhs_reg:
    ;; set-lhs-register-index
    mov rdi, [rbp-8]            ; lhs
    call tnode_reg_index
    mov rdi, [rbp-40]           ; pattern**
    mov rsi, rax                ; reg-index
    call inst_pattern_set_value

    ;; reg, x
    mov rdi, [rbp-8]            ; lhs
    mov rsi, [rbp-24]           ; rhs
    mov rdx, [rbp-40]           ; pattern*
    mov rcx, [rbp-48]           ; template-metadata
    call asm_infer_size_r_x

    jmp .break

.lhs_addressing:
    mov rdi, [rbp-8]            ; lhs
    mov rsi, [rbp-24]           ; rhs
    mov rdx, [rbp-40]           ; pattern*
    mov rcx, [rbp-48]           ; template-metadata
    call asm_infer_size_m_x

    jmp .break

.break:
    leave
    ret


;;; rdi: lhs
;;; rsi: rhs
;;; rdx: pattern*
;;; rcx: template-metadata, 1 if 64bits is allowed for imm
asm_infer_size_r_x:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov [rbp-48], rcx           ; template-metadata
    mov [rbp-40], rdx           ; pattern*

    mov qword [rbp-32], 0       ; size or rhs-size

    mov [rbp-24], rsi           ; rhs
    mov [rbp-8], rdi            ; lhs

    ;; recalc-rhs-size
    mov rdi, [rbp-24]           ; rhs
    mov rsi, [rbp-48]           ; template-metadata
    call tnode_calc_imm_size
    and rax, 0x00ff             ; 0x00RR
    mov [rbp-32], rax           ; size

    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    and rax, 0x00ff             ; 0x00RR
    shl rax, 8                  ; 1 * 8, 0xRR00
    or [rbp-32], rax            ; size, 0xRRLL

    ;;
    mov rax, [rbp-32]           ; size

    ;; 8
    cmp ax, 0x0800              ; 8, 0 -> 8
    je .rhs_bits64
    cmp ax, 0x0808              ; 8, 8 -> 8
    je .rhs_bits64
    cmp ax, 0x0804              ; 8, 4 -> 8 | rhs = 4, 4
;   je .rhs_bits64_or_lhs_and_rhs_bits32
    je .rhs_bits64
    cmp ax, 0x0802              ; 8, 2 -> 2
    je .rhs_bits16
    cmp ax, 0x0801              ; 8, 1 -> 1
    je .rhs_bits8

    ;; 4
    cmp ax, 0x0400              ; 4, 0 -> 0
    je .rhs_bits32
    cmp ax, 0x0404              ; 4, 4 -> 4
    je .rhs_bits32
    cmp ax, 0x0402              ; 4, 2 -> 2
    je .rhs_bits16
    cmp ax, 0x0401              ; 4, 1 -> 1
    je .rhs_bits16

    ;; 2
    cmp ax, 0x0200              ; 2, 0 -> 0
    je .rhs_bits8
    cmp ax, 0x0201              ; 2, 1 -> 1
    je .rhs_bits8

    ;; 1
    cmp ax, 0x0100              ; 1, 0 -> 0
    je .rhs_bits8
    cmp ax, 0x0101              ; 1, 1 -> 1
    je .rhs_bits8

    jmp .size_undecidable

.rhs_bits64:
    mov qword [rbp-32], 8       ; rhs-size
    jmp .update_rhs

.rhs_bits64_or_lhs_and_rhs_bits32:
    mov rdi, [rbp-8]            ; lhs
    call tnode_reg_extended
    cmp rax, 0
    jne .rhs_bits64

    ;; set-lhs-size
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    mov rdi, [rbp-40]           ; pattern*
    mov rsi, 4                  ; size
    call inst_pattern_set_size

    jmp .rhs_bits32

.rhs_bits32:
    mov qword [rbp-32], 4       ; rhs-size
    jmp .update_rhs

.rhs_bits16:
    mov qword [rbp-32], 2       ; rhs-size
    jmp .update_rhs

.rhs_bits8:
    mov qword [rbp-32], 1       ; rhs-size
    jmp .update_rhs

.update_rhs:
    ;; set-rhs-expr
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_tag
    mov rdi, [rbp-40]           ; pattern*
    add rdi, 8                  ; pattern[1]*
    mov rsi, rax                ; type
    call inst_pattern_set_type

    ;; set-rhs-size
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_size
    mov rdi, [rbp-40]           ; pattern*
    add rdi, 8                  ; pattern[1]*
    mov rsi, [rbp-32]           ; rhs-size
    call inst_pattern_set_size

    jmp .break

.size_undecidable:
    ;;
    mov rdi, [rbp-32]           ; size
    call asm_exit_with_error_undecidable_op_size

.break:
    mov rax, [rbp-40]           ; pattern

    leave
    ret


;;; rdi: lhs
;;; rsi: rhs
;;; rdx: pattern*
;;; rcx: template-metadata, 1 if 64bits is allowed for imm
asm_infer_size_m_x:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov [rbp-48], rcx           ; template-metadata
    mov [rbp-40], rdx           ; pattern

    mov qword [rbp-32], 0       ; size or rhs-size

    mov [rbp-24], rsi           ; rhs
    mov [rbp-8], rdi            ; lhs

    ;; recalc-rhs-size
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_size
    and rax, 0x00ff             ; 0x00RR
    mov [rbp-32], rax           ; size

    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    and rax, 0x00ff             ; 0x00LL
    shl rax, 8                  ; 1 * 8, 0xLL00
    or [rbp-32], rax            ; size, 0xLLRR

    cmp qword [rbp-32], 0       ; size
    je .size_undecidable

    ;; recalc-rhs-size
    mov rdi, [rbp-24]           ; rhs
    mov rsi, [rbp-48]           ; template-metadata
    call tnode_calc_imm_size
    and rax, 0x00ff             ; 0x00RR
    mov [rbp-32], rax           ; size

    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    cmp rax, 0
    jne .skip_lhs_updation

.update_lhs_size:
    ;; set-lhs-size, same as rhs
    mov rdi, [rbp-40]           ; pattern*
    mov rsi, [rbp-32]           ; size
    call inst_pattern_set_size

    mov rax, [rbp-32]           ; rhs-size

.skip_lhs_updation:
    and rax, 0x00ff             ; 0x00LL
    shl rax, 8                  ; 1 * 8, 0xLL00
    or [rbp-32], rax            ; size, 0xLLRR

    ;;
    mov rax, [rbp-32]           ; size

    cmp ax, 0x0808              ; 8, 8 -> 8
    je .rhs_bits64

    cmp ax, 0x0800              ; 8, 0 -> 8
    je .rhs_bits64

    cmp ax, 0x0804              ; 8, 4 -> 4
    je .rhs_bits32

    cmp ax, 0x0802              ; 8, 2 -> 2
    je .rhs_bits16

    cmp ax, 0x0801              ; 8, 1 -> 1
    je .rhs_bits8

    cmp ax, 0x0404              ; 4, 4 -> 4
    je .rhs_bits32

    cmp ax, 0x0402              ; 4, 2 -> 2
    je .rhs_bits16

    cmp ax, 0x0401              ; 4, 1 -> 1
    je .rhs_bits8

    cmp ax, 0x0201              ; 2, 1 -> 1
    je .rhs_bits8

    cmp ax, 0x0101              ; 1, 1 -> 1
    je .rhs_bits8

    jmp .size_undecidable

.rhs_bits64:
    mov qword [rbp-32], 8       ; rhs-size
    jmp .update_rhs

.rhs_bits32:
    mov qword [rbp-32], 4       ; rhs-size
    jmp .update_rhs

.rhs_bits16:
    mov qword [rbp-32], 2       ; rhs-size
    jmp .update_rhs

.rhs_bits8:
    mov qword [rbp-32], 1       ; rhs-size
    jmp .update_rhs

.update_rhs:
    ;; set-rhs-expr
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_tag
    mov rdi, [rbp-40]           ; pattern*
    add rdi, 8                  ; pattern[1]*
    mov rsi, rax                ; type
    call inst_pattern_set_type

    ;; set-rhs-size
    mov rdi, [rbp-24]           ; rhs
    call tnode_type_size
    mov rdi, [rbp-40]           ; pattern*
    add rdi, 8                  ; pattern[1]*
    mov rsi, [rbp-32]           ; rhs-size
    call inst_pattern_set_size

    jmp .break

.size_undecidable:
    ;;
    mov rdi, [rbp-32]           ; size
    call asm_exit_with_error_undecidable_op_size

.break:
    mov rax, [rbp-40]           ; pattern

    leave
    ret


;;; rdi: lhs
;;; rsi: pattern*
;;; rdx: template-metadata, 1 if 64bits is allowed for imm
asm_infer_size_x:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov [rbp-48], rdx           ; template-metadata
    mov [rbp-40], rsi           ; pattern

    mov qword [rbp-32], 0       ; size or rhs-size

    mov [rbp-8], rdi            ; lhs

    ;; calc-lhs-size
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    mov [rbp-32], rax           ; size

    cmp qword [rbp-32], 0       ; size
    je .size_undecidable

    ;; set-lhs-type
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_tag         ;
    mov rdi, [rbp-40]           ; pattern*
    mov rsi, rax                ; type
    call inst_pattern_set_type

;    ;; set-lhs-size
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    mov rdi, [rbp-40]           ; pattern*
    mov rsi, [rbp-32]           ; size
    call inst_pattern_set_size

    ;; TODO: register-check

    jmp .break

.size_undecidable:
    ;;
    mov rdi, [rbp-32]           ; size
    call asm_exit_with_error_undecidable_op_size

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: inst*
;;; rdx: byte*
;;; rcx: args[1]
;;; r8:  args[2]
inst_pattern_to_inst:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov byte [rbp-56], 0        ; op-counter

    mov [rbp-48], r8            ; args[2]
    mov [rbp-40], rcx           ; args[1]

    mov [rbp-32], rdx           ; byte*, template
    mov [rbp-16], rsi           ; inst*
    mov [rbp-8], rdi            ; asm*

    ;; --> debug
    mov rdi, [rbp-40]
    call tnode_type_size
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    cmp qword [rbp-48], 0
    je .skip_debug_0
    mov rdi, [rbp-48]
    call tnode_type_size
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline
.skip_debug_0:
    ;; <-- debug

    ;; REX
    mov eax, [rbp-32]           ; byte*
    xor rcx, rcx
    mov cl, [rax]               ; byte[n], REX
    inc qword [rbp-32]          ; step-iter, n

    cmp cl, 0
    je .skip_rex

    mov rdi, [rbp-16]           ; inst*
    mov rsi, rcx
    call inst_set_rex_value

.skip_rex:
    ;; op-code
    mov rax, [rbp-32]           ; byte*
    mov al, [rax]               ; byte[n], op-length
    inc qword [rbp-32]          ; step-iter, n

    mov [rbp-56], al            ; op-length

.op_loop:
    mov al, [rbp-56]            ; op-length
    cmp al, 0
    je .op_loop_break

    mov rax, [rbp-32]           ; byte*
    mov al, [rax]               ; byte[n], op-code
    inc qword [rbp-32]          ; step-iter, n

    mov rdi, [rbp-16]           ; inst*
    mov rsi, rax                ; op-code
    call inst_append_opcode

    dec byte [rbp-56]           ; op-length
    jmp .op_loop

.op_loop_break:
    ;; operands
    mov rax, [rbp-32]           ; byte*
    mov al, [rax]               ; byte[n], op-length
    inc qword [rbp-32]          ; step-iter, n

    cmp al, const_asm_inst_type_rm ; r, r/m
    je .operand_rm

    cmp al, const_asm_inst_type_mr ; r/m, r
    je .operand_mr

    cmp al, const_asm_inst_type_i ; imm
    je .operand_i

    cmp al, const_asm_inst_type_mi ; r/m, imm
    je .operand_mi

    cmp al, const_asm_inst_type_oi ; r, imm
    je .operand_oi

    cmp al, const_asm_inst_type_m ; r/m
    je .operand_m

    cmp al, const_asm_inst_type_o ; r
    je .operand_o

    cmp al, const_inst_enc_d_0 ; rel
    je .operand_d_0

    jmp .not_supported

    ;; r, r/m
.operand_rm:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1], mod r/m, /r-degit
    mov rdx, [rbp-48]           ; args[2], effective-reg
    call inst_set_operands

    jmp .break

    ;; r/m, r
.operand_mr:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-48]           ; args[2], mod r/m, /r-degit
    mov rdx, [rbp-40]           ; args[1], effective-reg
    call inst_set_operands

    jmp .break

    ;; r, imm
.operand_i:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_set_prefix_if

    mov rdi, [rbp-48]           ; args[2]
    call tnode_type_size

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-48]           ; arg[2]
    mov rdx, rax
    call inst_set_imm_sign_ext

    jmp .break

    ;; r/m
.operand_m:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_set_prefix_if

    mov rax, [rbp-32]           ; byte*
    xor rcx, rcx
    mov cl, [rax]               ; byte[n], /r
    inc qword [rbp-32]          ; step-iter, n

    mov rdi, [rbp-16]           ; inst*
    mov rsi, rcx                ; mod r/m, /r-degit
    mov rdx, [rbp-40]           ; args[1], effective-reg
    call inst_set_r_digit_operands

    jmp .break

    ;; r/m, imm
.operand_mi:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_set_prefix_if

    mov rax, [rbp-32]           ; byte*
    xor rcx, rcx
    mov cl, [rax]               ; byte[n], /r
    inc qword [rbp-32]          ; step-iter, n

    mov rdi, [rbp-16]           ; inst*
    mov rsi, rcx                ; mod r/m, /r-degit
    mov rdx, [rbp-40]           ; args[1], effective-reg
    call inst_set_r_digit_operands

    mov rdi, [rbp-48]           ; args[2]
    call tnode_type_size

    ;; imm
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-48]           ; arg[2]
    mov rdx, rax
    call inst_set_imm_sign_ext

    jmp .break

    ;; r
.operand_o:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_set_prefix_if

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_add_reg_to_opcode

    jmp .break

    ;; r, imm
.operand_oi:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_set_prefix_if

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_add_reg_to_opcode

    mov rdi, [rbp-48]           ; args[2]
    call tnode_type_size

    ;; imm
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-48]           ; arg[2]
    mov rdx, rax
    call inst_set_imm

    jmp .break

    ;; d
.operand_d_0:
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; args[1]
    call inst_set_prefix_if

    ;; imm
    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-40]           ; arg[1]
    mov rdx, rax
    call inst_set_imm

    jmp .break

.not_supported:
    mov rdi, 42
    call runtime_exit

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_add:
    mov rdx, g_asm_inst_template_add
    call asm_write_inst_from_template
    ret

;;; rdi: asm*
;;; rsi: args
asm_write_inst_sub:
    mov rdx, g_asm_inst_template_sub
    call asm_write_inst_from_template
    ret

;;; rdi: asm*
;;; rsi: args
asm_write_inst_div:
    mov rdx, g_asm_inst_template_div
    call asm_write_inst_from_template
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_inc:
    mov rdx, g_asm_inst_template_inc
    call asm_write_inst_from_template
    ret

;;; rdi: asm*
;;; rsi: args
asm_write_inst_dec:
    mov rdx, g_asm_inst_template_dec
    call asm_write_inst_from_template
    ret


;;; rdi: asm*
;;; rsi: args
;;; rdx: template**
asm_write_inst_from_template:
    push rbp
    mov rbp, rsp
    sub rsp, 120

    mov qword [rbp-120], 0       ; args[0], sexp*(tnode)
    mov qword [rbp-112], 0       ; args[1], sexp*(tnode)
    mov qword [rbp-104], 0       ; args[2], sexp*(tnode)
    mov qword [rbp-96], 0        ; args[3], sexp*(tnode)

    mov qword [rbp-88], 0       ; args*, head

    mov qword [rbp-48], 0       ; template-metadata
    mov qword [rbp-40], 0       ; args-num
    mov qword [rbp-32], 0       ; matched
    mov [rbp-24], rdx           ; template**
    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    mov rax, [rbp-16]           ; args*
    mov [rbp-88], rax           ; args*, head

.take_args_loop:
    cmp qword [rbp-88], 0
    je .generate_inst           ; all args are read

    ;; args[n]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-88]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd

    mov rdx, [rbp-40]           ; args-num
    shl rdx, 3                  ; args-num * 8
    lea rcx, [rbp-120]          ; args*
    add rcx, rdx                ; args[args-num]*
    mov [rcx], rax              ; args[args-num] = evaled-arg

    inc qword [rbp-40]          ; args-num

    jmp .take_args_loop

.generate_inst:
    ;; read template metadata
    mov rax, [rbp-24]           ; template**
    mov rax, [rax]              ; template[0, 8), template-metadata
    add qword [rbp-24], 8       ; template* += sizeof(template), 8

    mov [rbp-48], rax           ; template-metadata

.gen_loop:
    ;; -> debug
    mov rdi, [rbp-16]
    call sexp_print
    call runtime_print_newline
    ;; <- debug

    mov rax, [rbp-24]           ; template**
    mov rax, [rax]              ; template*
    cmp rax, 0                  ; null-terminated
    je .finish

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; template*
    mov rdx, [rbp-40]           ; args-num
    lea rcx, [rbp-120]          ; args*, sexp**
    mov r8, [rbp-48]            ; template-metadata
    call gen
    or [rbp-32], rax            ; matched |= result

    add qword [rbp-24], 8       ; template* += sizeof(template), 8

    jmp .gen_loop

.finish:
    cmp qword [rbp-32], 0       ; matched
    jne .break

.failed:
    mov rdi, str_error_unsupported_inst_form
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [rbp-16]
    call sexp_print
    call runtime_print_newline

    mov rdi, 2
    call runtime_exit

.break:
    leave
    ret

;;; rdi: pattern*
;;; rsi: index
inst_helper_calc_pattern_offset:
    shl rsi, 3                     ; index * 8
    add rdx, rsi                   ; pattern[index]
    mov rax, rdx
    ret


;;; rdi: asm*
;;; rsi: template*
;;; rdx: args-num
;;; rcx: args, sexp**
;;; r8: template-metadata
gen:
    push rbp
    mov rbp, rsp
    sub rsp, 168

    lea rax, [rbp-168]          ; inst: sizeof(32), [168, 136)
    mov qword [rbp-128], rax    ; inst*

    mov qword [rbp-48], 0       ; expected-size
    mov [rbp-40], r8            ; template-metadata
    mov [rbp-32], rcx           ; args*, sexp**
    mov [rbp-24], rdx           ; args-num
    mov [rbp-16], rsi           ; template*
    mov [rbp-8], rdi            ; asm*

    ;; read-length
    mov rax, [rbp-16]           ; template*
    mov eax, dword [rax]        ; template[n]
    add qword [rbp-16], 4       ; step template

    mov [rbp-48], rax           ; expected-size

    ;; check-length
    mov rax, [rbp-48]           ; expected-size
    cmp rax, [rbp-24]           ; template-arg-size != args-num
    jne .not_matched

    mov rdi, [rbp-128]          ; inst*
    call inst_init

    mov rax, [rbp-48]           ; expected-size
    cmp rax, 0
    je .gen_inst_0

    cmp rax, 1
    je .gen_inst_1

    cmp rax, 2
    je .gen_inst_2

    jmp .not_matched

.gen_inst_0:
;    call gen_inst_0
;    jmp .break
    jmp .not_matched

.gen_inst_1:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; template*
    mov rdx, [rbp-128]          ; inst*
    mov rcx, [rbp-32]           ; args*, sexp**
    mov r8, 1
    call asm_gen_inst

    jmp .finish

.gen_inst_2:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; template*
    mov rdx, [rbp-128]          ; inst*
    mov rcx, [rbp-32]           ; args*, sexp**
    mov r8, 2
    call asm_gen_inst

    jmp .finish

.finish:
    cmp rax, 0
    je .not_matched

    sub rax, 100                ; total-cost + 100

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-128]          ; inst*
    mov rdx, rax
    call asm_update_inst_if_efficient

    mov rax, 1
    jmp .break

.not_matched:
    xor rax, rax

.break:
    leave
    ret

;;; rdi: pattern*
;;; rsi: expected-pattern*
;;; rdx: template**
;;; rcx: args-num
;;; r8:  template-metadata
match:
    push rbp
    mov rbp, rsp
    sub rsp, 176

    mov qword [rbp-96], 0       ; expected-pattern*
    mov qword [rbp-88], 0       ; pattern*
    mov qword [rbp-80], 0       ; counter

    mov [rbp-40], r8            ; template-metadata
    mov [rbp-32], rcx           ; args-num
    mov [rbp-24], rdx           ; template**
    mov [rbp-16], rsi           ; expected-pattern*
    mov [rbp-8], rdi            ; pattern*

.loop:
    ;; debug
    mov rdi, qword [rbp-80]     ; counter
    add rdi, 100
    call runtime_print_uint64
    call runtime_print_newline

    mov rcx, [rbp-80]           ; counter
    cmp rcx, [rbp-32]           ; counter == args-num
    je .matched

    ;;
    mov rdx, [rbp-8]            ; pattern*
    mov rsi, [rbp-80]           ; counter
    call inst_helper_calc_pattern_offset
    mov [rbp-88], rax           ; current-pattern[counter]*

    mov rdx, [rbp-16]           ; expected-pattern*
    mov rsi, [rbp-80]           ; counter
    call inst_helper_calc_pattern_offset
    mov [rbp-96], rax           ; current-expected-pattern[counter]*

    ;;
    mov rdi, [rbp-24]           ; template**
    mov rdi, [rdi]              ; template*
    mov rsi, [rbp-96]           ; current-expected-pattern[counter]*
    call inst_pattern_read

    ;; --> debug
    mov rdi, [rbp-96]           ; pattern*, expected
    call inst_pattern_print

    mov rdi, [rbp-88]           ; pattern[n]
    call inst_pattern_print
    ;; <--

    mov rdi, [rbp-96]           ; pattern*, expected
    mov rsi, [rbp-88]           ; pattern[counter]
    mov rdx, [rbp-40]           ; template-metadata
    call inst_pattern_is_matched
    cmp rax, 0
    je .not_matched

    ;; step
    mov rax, [rbp-24]           ; template**
    mov rcx, [rax]              ; template*
    add rcx, 4                  ; step template
    mov [rax], rcx              ;
    inc qword [rbp-80]          ; counter

    jmp .loop

.matched:
    ;;
    mov rdi, .matched_str
    call runtime_print_string
    call runtime_print_newline
    ;;

    mov rax, 1
    jmp .break

.matched_str:
    db "matched", 0

.not_matched:
    xor rax, rax

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: expected-pattern*
;;; rdx: sexp*(tnode*)
;;; rcx: u64*, cost
asm_param_compat:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov [rbp-32], rcx           ; cost, u64*
    mov [rbp-24], rdx           ; arg, sexp*
    mov [rbp-16], rsi           ; expected-pattern*
    mov [rbp-8], rdi            ; asm*

    mov rax, [rbp-32]           ; cost, u64*
    mov qword [rax], 0          ; cost == 0

    ;; REMOVE
    ;; --
    mov rdi, str_debug_compat
    call runtime_print_string
    call runtime_print_newline

    mov rdi, str_debug_arrow_r
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [rbp-16]           ; pattern
    call inst_pattern_print

    mov rdi, [rbp-24]           ; arg
    call sexp_print
    call runtime_print_newline

    mov rdi, [rbp-24]           ; arg
    call tnode_calc_imm_size
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, str_debug_arrow_r
    call runtime_print_string
    ;; --

    ;;
    mov rdi, [rbp-16]           ; pattern
    call inst_pattern_get_type

    cmp rax, 1                  ; register
    je .register

    cmp rax, 5                  ; addressing
    je .addressing

    cmp rax, 4                  ; imm
    je .imm

    cmp rax, 0x0f               ; rel
    je .rel

    ;; ICE
    mov rdi, 10
    call runtime_exit

.register:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; expected-pattern*
    mov rdx, [rbp-24]           ; arg, sexp*
    mov rcx, [rbp-32]           ; cost, u64*
    call asm_param_compat_to_reg
    jmp .break

.addressing:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; expected-pattern*
    mov rdx, [rbp-24]           ; arg, sexp*
    mov rcx, [rbp-32]           ; cost, u64*
    call asm_param_compat_to_mod
    jmp .break

.imm:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; expected-pattern*
    mov rdx, [rbp-24]           ; arg, sexp*
    mov rcx, [rbp-32]           ; cost, u64*
    call asm_param_compat_to_imm
    jmp .break

.rel:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; expected-pattern*
    mov rdx, [rbp-24]           ; arg, sexp*
    mov rcx, [rbp-32]           ; cost, u64*
    call asm_param_compat_to_rel
    jmp .break

.break:
    mov [rbp-40], rax

    ;;
    mov rdi, rax
    call sexp_print

    mov rcx, [rbp-32]           ; cost, u64*
    mov rdi, [rcx]
    call runtime_print_uint64

    call runtime_print_newline
    call runtime_print_newline
    ;;

    mov rax, [rbp-40]

    leave
    ret


;;; rdi: asm*
;;; rsi: expected-pattern*
;;; rdx: sexp*(tnode*)
asm_param_compat_to_reg:
    push rbp
    mov rbp, rsp
    sub rsp, 104

    mov byte [rbp-96], 0        ; expected-reg-index
    mov qword [rbp-88], 0       ; result, sexp*
    mov qword [rbp-80], 0       ; arg.size

    mov [rbp-32], rcx           ; cost, u64*
    mov [rbp-24], rdx           ; arg, sexp*
    mov [rbp-16], rsi           ; expected-pattern*
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-24]           ; arg
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .can_conv

    jmp .failed

.can_conv:
    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_value
    cmp rax, 0xff
    je .can_conv_normal

    ;; check register index
    mov [rbp-96], rax           ; expected-reg-index

    mov rdi, [rbp-24]           ; arg
    call tnode_reg_index

    cmp byte [rbp-96], al       ; expected-reg-index != reg-index
    jne .failed

.can_conv_normal:
    mov rdi, [rbp-24]           ; arg
    call tnode_type_size
    mov [rbp-80], rax           ; arg.size

    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_type_narrow
    cmp rax, 0
    jne .narrow_size_check

.exact_size_check:
    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_size

    cmp rax, [rbp-80]           ; expected == arg.size
    je .matched

    jmp .failed

.narrow_size_check:
    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_size

    cmp rax, [rbp-80]           ; expected == arg.size
    je .matched

    cmp rax, 4                  ; expected == 8
    je .narrow_size_check_allow_reg32

    jmp .failed

.narrow_size_check_allow_reg32:
;    mov rdi, [rbp-24]           ; arg
;    call tnode_reg_extended
;    cmp rax, 0
;    jne .failed

    cmp qword [rbp-80], 8       ; arg.size == 4
    je .matched

    jmp .failed

.matched:
    mov rdi, [rbp-24]           ; sexp*
    mov rsi, [rbp-16]           ; expected-pattern*
    call inst_pattern_updated_tnode
    mov [rbp-88], rax           ; result, sexp*

    mov rax, [rbp-88]           ; result, sexp*
    jmp .break

.failed:
    xor rax, rax

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: expected-pattern*
;;; rdx: sexp*(tnode*)
asm_param_compat_to_mod:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov qword [rbp-88], 0       ; result, sexp*
    mov qword [rbp-80], 0       ; arg.size

    mov [rbp-32], rcx           ; cost, u64*
    mov [rbp-24], rdx           ; arg, sexp*
    mov [rbp-16], rsi           ; expected-pattern*
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-24]           ; arg
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .can_conv
    cmp rax, 5                  ; addressing
    je .can_conv

    jmp .failed

.can_conv:
    mov rdi, [rbp-24]           ; arg
    call tnode_type_size
    mov [rbp-80], rax           ; arg.size

.size_check:
    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_size

    cmp rax, [rbp-80]           ; expected == arg.size
    je .matched

    jmp .failed

.matched:
    mov rax, [rbp-24]           ; sexp*

    jmp .break

.failed:
    xor rax, rax
    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: expected-pattern*
;;; rdx: sexp*(tnode*)
asm_param_compat_to_imm:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov qword [rbp-88], 0       ; result, sexp*
    mov qword [rbp-80], 0       ; arg.size

    mov [rbp-32], rcx           ; cost, u64*
    mov [rbp-24], rdx           ; arg, sexp*
    mov [rbp-16], rsi           ; expected-pattern*
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-24]           ; arg
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .can_conv

    jmp .failed

.can_conv:
    mov rdi, [rbp-24]           ; arg
    call tnode_calc_imm_size
    mov [rbp-80], rax           ; arg.size

    cmp qword [rbp-80], 8       ; arg.size == 8
    jne .narrow_size_check

.exact_size_check:              ; imm64 not signed only
    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_type_narrow
    cmp rax, 0
    je .overflow

    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_size

    cmp rax, [rbp-80]           ; expected == arg.size
    je .matched

    jmp .failed

.overflow:
    ;; maybe overflow, set size 1
    mov qword [rbp-80], 1       ; arg.size
    jmp .matched_with_cost

.narrow_size_check:
    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_size

    cmp rax, [rbp-80]           ; expected >= arg.size
    jge .matched_with_cost

    jmp .failed

.matched_with_cost:
    mov rdi, [rbp-32]           ; cost
    inc qword [rdi]

.matched:
    mov rdi, [rbp-24]           ; sexp*
    mov rsi, [rbp-16]           ; expected-pattern*
    call inst_pattern_updated_tnode
    mov [rbp-88], rax           ; result, sexp*

    mov rax, [rbp-88]           ; result, sexp*
    jmp .break

.failed:
    xor rax, rax

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: expected-pattern*
;;; rdx: sexp*(tnode*)
asm_param_compat_to_rel:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov qword [rbp-88], 0       ; result, sexp*
    mov qword [rbp-80], 0       ; arg.size

    mov [rbp-32], rcx           ; cost, u64*
    mov [rbp-24], rdx           ; arg, sexp*
    mov [rbp-16], rsi           ; expected-pattern*
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-24]           ; arg
    call tnode_type_tag
    cmp rax, 4                  ; integer
    je .can_conv

    jmp .failed

.can_conv:
    ;; -->
    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_aux

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; arg, sexp*
    mov rdx, rax
    call asm_calc_rel_sample

    mov rdi, rax
    call tnode_print

    ;; <--

    mov rdi, [rbp-16]           ; expected-pattern*
    call inst_pattern_get_aux

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; arg, sexp*
    mov rdx, rax
    call asm_calc_rel_sample

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; expected-pattern*
    mov rdx, rax
    call asm_param_compat_to_imm

    jmp .break

.failed:
    xor rax, rax

.break:
    leave
    ret


;;; rdi: expected-pattern*
;;; rsi: args*
;;; rdx: updated-args*
;;; rcx: args-num
match2:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov qword [rbp-88], 0       ; expected-pattern*
    mov qword [rbp-80], 0       ; counter

    mov [rbp-32], rcx           ; args-num
    mov [rbp-24], rdx           ; updated-args*
    mov [rbp-16], rsi           ; args*
    mov [rbp-8], rdi            ; expected-pattern*

.loop:
    mov rcx, [rbp-80]           ; counter
    cmp rcx, [rbp-32]           ; counter == args-num
    je .break

    mov rdx, [rbp-8]            ; expected-pattern*
    mov rsi, [rbp-80]           ; counter
    call inst_helper_calc_pattern_offset
    mov [rbp-88], rax           ; current-expected-pattern[counter]*
;
    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    mov rdi, [rbp-16]           ; args*(sexp**)
    add rdi, rcx                ; args[counter]*(sexp**)
    mov rdi, [rdi]              ; sexp*
    mov rsi, [rbp-88]           ; current-expected-pattern[counter]*
    call inst_pattern_updated_tnode

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    mov rdi, [rbp-24]           ; updated-args*
    add rdi, rcx                ; updated-args[counter]*
    mov [rdi], rax              ; updated-args <- result

    ;; step
    inc qword [rbp-80]          ; counter

    jmp .loop

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: template*
;;; rdx: inst*
;;; rcx: args, sexp**
;;; r8:  args-num
asm_gen_inst:
    push rbp
    mov rbp, rsp
    sub rsp, 176

.args_num: equ 2

.pattern_offset: equ 176
    mov qword [rbp-176], 0       ; pattern[0] = pattern(args[1])
    mov qword [rbp-168], 0       ; pattern[1] = pattern(args[2])

.updated_args_offset: equ 160
    mov qword [rbp-160], 0       ; updated-sexp*(tnode)[0]
    mov qword [rbp-152], 0       ; updated-sexp*(tnode)[1]

.expected_pattern_offset: equ 144
    mov qword [rbp-144], 0       ; expected-pattern[0]
    mov qword [rbp-136], 0       ; expected-pattern[1]

    mov qword [rbp-112], 0      ; pattern*
    mov qword [rbp-104], 0      ; pattern*

    mov qword [rbp-96], 0       ; pattern*
    mov qword [rbp-88], 0       ; expected-pattern*
    mov qword [rbp-80], 0       ; counter

    mov [rbp-48], r8            ; args-num
    mov [rbp-40], rcx           ; args*, sexp**
    mov [rbp-24], rdx           ; inst*
    mov [rbp-16], rsi           ; template*
    mov [rbp-8], rdi            ; asm*

;;;-;;;
    mov qword [rbp-80], 0       ; counter
    mov qword [rbp-88], 0       ; max-size

.loop_a:
    ;; debug
    mov rdi, qword [rbp-80]     ; counter
    add rdi, 100
    call runtime_print_uint64
    call runtime_print_newline
    ;;

    mov rax, [rbp-48]           ; args-num
    cmp qword [rbp-80], rax     ; counter == args-num
    je .size_checked

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    mov rdi, [rbp-40]           ; args*, sexp**
    add rdi, rcx                ; args[n]**
    mov rdi, [rdi]              ; sexp*
    call tnode_type_size

    cmp rax, qword [rbp-88]     ; size < max-size
    jl .skip

    mov qword [rbp-88], rax     ; max-size

.skip:
    inc qword [rbp-80]          ; counter++

    jmp .loop_a

;;; ;;;;
.size_checked:
    mov rdi, [rbp-88]
    call runtime_print_uint64
    call runtime_print_newline

    ;; TODO: fix
    mov rax, [rbp-48]           ; args-num
    cmp rax, 1
    jle .skip_c

    cmp qword [rbp-88], 0       ; max-size == 0
    je .unsolvable_size

 .skip_c:
    ;;
    mov qword [rbp-80], 0       ; counter

.loop_b:
    ;; debug
    mov rdi, qword [rbp-80]     ; counter
    add rdi, 100
    call runtime_print_uint64
    call runtime_print_newline
    ;;

    mov rax, [rbp-48]           ; args-num
    cmp qword [rbp-80], rax     ; counter == args-num
    je .size_updated

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    mov rdi, [rbp-40]           ; args*, sexp**
    add rdi, rcx                ; args[n]**
    mov rdi, [rdi]              ; sexp*
    call tnode_calc_imm_size

    cmp rax, 0
    je .skip_b

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    mov rdi, [rbp-40]           ; args*, sexp**
    add rdi, rcx                ; args[n]**
    mov rax, [rdi]              ; sexp*

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    lea rdi, [rbp-.updated_args_offset]
    add rdi, rcx
    mov [rdi], rax

    jmp .skip_b_2

.skip_b:
    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    mov rdi, [rbp-40]           ; args*, sexp**
    add rdi, rcx                ; args[n]**

    mov rdi, [rdi]              ; sexp*
    mov rsi, [rbp-88]           ; max-size
    call tnode_size_updated

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    lea rdi, [rbp-.updated_args_offset]
    add rdi, rcx
    mov [rdi], rax

.skip_b_2:
    inc qword [rbp-80]          ; counter++

    jmp .loop_b

    ;;
.size_updated:
    mov qword [rbp-80], 0       ; counter
    mov qword [rbp-112], 0      ; total-cost

.loop:
    ;; debug
    mov rdi, qword [rbp-80]     ; counter
    add rdi, 100
    call runtime_print_uint64
    call runtime_print_newline
    ;;

    mov rax, [rbp-48]           ; args-num
    cmp qword [rbp-80], rax     ; counter == args-num
    je .matched

    mov rdi, [rbp-16]           ; template*
    lea rsi, [rbp-96]           ; expected-pattern*
    call inst_pattern_read
    add qword [rbp-16], 4       ; step template

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    lea rax, [rbp-.updated_args_offset]
    add rax, rcx                ; args[n], sexp*
    mov rdi, [rax]
    call tnode_calc_imm_size
    mov [rbp-104], rax          ; cost

    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    lea rax, [rbp-.updated_args_offset]
    add rax, rcx                ; args[n], sexp*

    ;; convert
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-96]           ; expected-pattern*
    mov rdx, [rax]              ; args[n], sexp*
    lea rcx, [rbp-104]          ; cost*
    call asm_param_compat

    cmp rax, 0                  ; result == nil
    je .not_matched

    ;; update typed-node
    mov rcx, [rbp-80]           ; counter
    shl rcx, 3                  ; counter * 8
    lea rdi, [rbp-.updated_args_offset]
    add rdi, rcx
    mov [rdi], rax

    mov rax, [rbp-104]          ; cost
    add [rbp-112], rax          ; total-cost

    inc qword [rbp-80]          ; counter++

    jmp .loop

.matched:
    mov rax, [rbp-48]           ; args-num

    cmp rax, 1
    je .matched_1

    cmp rax, 2
    je .matched_2

    ;; ICE
    mov rdi, 50
    call runtime_exit

.matched_1:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; inst*
    mov rdx, [rbp-16]           ; template*
    lea rax, [rbp-.updated_args_offset]
    mov rcx, [rax]              ; args[0]
    mov r8, 0                   ; args[1] = nil
    call inst_pattern_to_inst

    jmp .finish

.matched_2:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; inst*
    mov rdx, [rbp-16]           ; template*
    lea rax, [rbp-.updated_args_offset]
    mov rcx, [rax]              ; args[0]
    mov r8, [rax+8]             ; args[1]
    call inst_pattern_to_inst

    jmp .finish

.finish:
    mov rax, [rbp-112]          ; total-cost
    add rax, 100                ; total-cost + 100
    jmp .break

.not_matched:
    xor rax, rax
    jmp .break

.unsolvable_size:
    mov rdi, 3
    call runtime_exit
    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
;;; rdx: u64, cond
asm_write_inst_jcc:
    push rbp
    mov rbp, rsp
    sub rsp, 64

    mov [rbp-56], rdx           ; cond
    mov qword [rbp-48], 0       ; size

    mov qword [rbp-24], 0       ; args[1], sexp*(tnode)

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; args[1]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-24], rax           ; args[1]

    ;; args[1] inspect
    mov rdi, [rbp-24]
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .encode_d

    cmp rax, 2                  ; label
    je .encode_d

    mov rdi, 10                 ; debug
    call runtime_exit

    ;; rel
.encode_d:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], sexp*(tnode)
    call asm_calc_rel_sample
    mov rdi, rax
    call tnode_calc_imm_size
    mov [rbp-48], rax           ; size

    cmp qword [rbp-48], 1
    je .skip_jcc_prefix

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x0f              ; JCC
    call asm_inst_append_opcode

.skip_jcc_prefix:
    mov rax, [rbp-56]           ; cond
    cmp rax, 0
    je .encode_d_ja

    cmp rax, 8
    je .encode_d_je

    cmp rax, 9
    je .encode_d_jg

    cmp rax, 10
    je .encode_d_jge

    cmp rax, 11
    je .encode_d_jl

    cmp rax, 12
    je .encode_d_jle

    cmp rax, 18
    je .encode_d_jne

    mov rdi, 11                 ; debug
    call runtime_exit

.encode_d_ja:
    cmp qword [rbp-48], 1
    je .encode_d_ja_rel8

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x87              ; JA
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_ja_rel8:
    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x77              ; JA
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_je:
    cmp qword [rbp-48], 1
    je .encode_d_je_rel8

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x84              ; JE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_je_rel8:
    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x74              ; JE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jg:
    cmp qword [rbp-48], 1
    je .encode_d_jg_rel8

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x8f              ; JG
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jg_rel8:
    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x7f              ; JG
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jge:
    cmp qword [rbp-48], 1
    je .encode_d_jge_rel8

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x8d              ; JGE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jge_rel8:
    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x7d              ; JGE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jl:
    cmp qword [rbp-48], 1
    je .encode_d_jl_rel8

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x8c              ; JL
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jl_rel8:
    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x7c              ; JL
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jle:
    cmp qword [rbp-48], 1
    je .encode_d_jle_rel8

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x8e              ; JLE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jle_rel8:
    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x7e              ; JLE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jne:
    cmp qword [rbp-48], 1
    je .encode_d_jne_rel8

    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x85              ; JNE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_jne_rel8:
    mov rdi, [rbp-8]           ; asm*
    mov rsi, 0x75              ; JNE
    call asm_inst_append_opcode

    jmp .encode_d_body

.encode_d_body:
    mov rdi, [rbp-8]            ; asm*
    mov rdi, [rdi]              ; asm.labels
    call sexp_print
    call runtime_print_newline

    ;; imm
    mov rdi, [rbp-8]           ; asm*
    mov rsi, [rbp-24]          ; arg[1]
    mov rdx, [rbp-48]          ; size
    call asm_inst_set_rel_sign_ext_8_32

    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_jmp:
    mov rdx, g_asm_inst_template_jmp
    call asm_write_inst_from_template
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_call:
    mov rdx, g_asm_inst_template_call
    call asm_write_inst_from_template
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_syscall:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0x0f               ; SYSCALL
    call asm_inst_append_opcode

    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0x05
    call asm_inst_append_opcode

    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_leave:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0xc9               ; LEAVE
    call asm_inst_append_opcode

    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_ret:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0xc3               ; RET (near)
    call asm_inst_append_opcode

    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_interp_inst_bits:
    ret


;;; rdi: asm*
;;; rsi: args
asm_interp_inst_org:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov qword [rbp-24], 0       ; sexp*(tnode), args[1]

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; args[1]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-24], rax           ; args[1]

    ;; args[1] inspect
    mov rdi, [rbp-24]
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .arg_1_integer

    mov rdi, 20                 ; debug
    call runtime_exit

.arg_1_integer:
    mov rdi, [rbp-24]           ; args[1]
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value

    mov rdi, [rbp-8]            ; asm*
    mov [rdi+24], rax           ; asm.segment-base

    leave
    ret


;;; rdi: asm*
;;; rsi: args
;;; rdx: u32, size
asm_interp_inst_dump:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

.loop:
    mov rdi, [rbp-16]           ; args, (hd . rest) | nil
    cmp rdi, 0
    je .break

    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; (type . value): sexp* as hd
    mov rdx, [rbp-24]           ; size
    call asm_write_node

    jmp .loop

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: args
;;; rdx: u32, size
asm_interp_inst_res:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov qword [rbp-32], 0       ; args[0]

    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check length

    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-32], rax            ; args[0]

    mov rdi, [rbp-32]           ; args[0]
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .res

    ;; failed
    mov rdi, 100
    call runtime_exit

.res:
    mov rdi, [rbp-32]           ; args[0]
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; count
    mov rdx, 0                  ; imm
    mov rcx, [rbp-24]           ; byte-size
    call asm_interp_inst_times

    leave
    ret

;;; rdi: asm*
;;; rsi: u64, count
;;; rdx: value imm
;;; rcx: byte-size
asm_interp_inst_times:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-32], rcx           ; byte-size
    mov [rbp-24], rdx           ; imm
    mov [rbp-16], rsi           ; count
    mov [rbp-8], rdi            ; asm*

.loop:
    mov rcx, [rbp-16]           ; count
    cmp rcx, 0
    je .break

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; imm
    mov rdx, [rbp-32]           ; byte-size
    call asm_write_value

    dec qword [rbp-16]

    jmp .loop

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: args
asm_interp_inst_equ:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov qword [rbp-24], 0       ; sexp*(tnode), args[1]

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; 1st
    lea rdi, [rbp-16]           ; (hd . rest)*
    call tnode_step_args
    mov [rbp-24], rax           ; args[1]

    ;;
    mov rdi, [rbp-8]            ; asm*
    call asm_state_snapshot_save
    mov rcx, rax

    ;; last-symbol
    mov rdi, [rbp-8]            ; asm*
    mov rax, [rdi+16]           ; asm.last-symbol

    ;; update labels
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; symbol
    mov rdx, [rbp-24]           ; (type . raw-expr)
    ;; mov rcx, rcx             ; snapshot
    call asm_add_consts

    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_interp_inst_align:
    push rbp
    mov rbp, rsp
    sub rsp, 64

    mov qword [rbp-56], 0       ; offset
    mov qword [rbp-48], 0       ; align
    mov qword [rbp-40], 0       ; pad

    mov qword [rbp-24], 0       ; sexp*(tnode), args[1]

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; args[1]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-24], rax           ; args[1]

    ;; args[1] inspect
    mov rdi, [rbp-24]
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .arg_1_integer

    mov rdi, [rbp-8]            ; asm*
    mov cl, [rdi+40]            ; asm.phase = 0
    cmp cl, 0
    je .break                   ; ignore evaluation when phase == 0

    mov rdi, 11                 ; debug
    call runtime_exit

.arg_1_integer:
    mov rdi, [rbp-24]           ; args[1]
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value
    mov [rbp-48], rax           ; align
    cmp rax, 0
    je .div0

    mov rdi, [rbp-8]            ; asm*
    mov rax, [rdi+104]          ; asm.buffer-size
    mov [rbp-56], rax           ; offset

    ;; -----    : offset 5
    ;; -------- : align 8
    ;;      --- : pad 3

    mov rax, [rbp-56]           ; offset
    mov rcx, [rbp-48]           ; align
    mov rdx, 0
    div rcx                     ; rax <- offset / align, rdx <- offset % align

    mov rax, [rbp-48]           ; pad = (align
    sub rax, rdx                ;              - (offset % align))
    mov rdx, 0
    div rcx                     ;
    mov [rbp-40], rdx           ;                                  % align

    ;; TODO: implment times and use it
.loop:
    mov rcx, [rbp-40]
    cmp rcx, 0
    je .break

    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0x90               ; fill by 0x90
    call asm_write_u8

    mov rcx, [rbp-40]
    dec rcx
    mov [rbp-40], rcx

    jmp .loop

.div0:
    mov rdi, 20                 ; debug
    call runtime_exit

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi, sexp** args*
asm_step_args_with_eval:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov [rbp-8], rdi            ; asm*

    mov rdi, rsi
    call tnode_step_args

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_eval_expr

    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*(tnode)
;;; rdx: expected-diff
asm_calc_rel_sample:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-32], 0       ; offset

    mov [rbp-24], rdx           ; expected-diff
    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value
    mov [rbp-32], rax           ; offset = target

    mov rdi, [rbp-8]            ; asm*
    call asm_current_loc
    sub [rbp-32], rax           ; offset -= $

    mov rax, [rbp-24]           ; expected-offset
    sub [rbp-32], rax           ; offset -= expected-offset

    mov rdi, [rbp-32]
    call sexp_alloc_int
    mov rdi, rax
    mov rsi, 0
    call tnode_alloc_uint_node

    leave
    ret


;;; inst {
;;;   l-prefix  : u8   , 0
;;;   rex-prefix: u8   , 1
;;;   opcode    : u8[4], 2
;;;   mod-rm    : u8   , 6
;;;   sib       : u8   , 7
;;;   disp      : u8[8], 8
;;;   imm       : u8[8], 16
;;;
;;;   l-prefix-size  : u8, 24
;;;   rex-prefix-size: u8, 25
;;;   opcode-size    : u8, 26
;;;   mod-rm-size    : u8, 27
;;;   sib-size       : u8, 28
;;;   disp-size      : u8, 29
;;;   imm-size       : u8, 30
;;; } = sizeof(32)

;;; rdi: inst*
inst_init:
    mov qword [rdi], 0
    mov qword [rdi+8], 0
    mov qword [rdi+16], 0
    mov qword [rdi+24], 0
    ret

;;; rdi: asm*
asm_inst_init:
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_init
    ret

;;; rdi: inst*
inst_current_size:
    xor rax, rax
    xor rcx, rcx

    mov cl, [rdi+24]            ; inst.l-prefix-size
    add rax, rcx

    mov cl, [rdi+25]            ; inst.rex-prefix-size
    add rax, rcx

    mov cl, [rdi+26]            ; inst.opcode-size
    add rax, rcx

    mov cl, [rdi+27]            ; inst.mod-rm-size
    add rax, rcx

    mov cl, [rdi+28]            ; inst.sib-size
    add rax, rcx

    mov cl, [rdi+29]            ; inst.disp-size
    add rax, rcx

    mov cl, [rdi+30]            ; inst.imm-size
    add rax, rcx

    ret

;;; rdi: asm*
asm_inst_current_size:
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_current_size
    ret

;;; rdi: inst*
;;; rsi: sexp*
inst_set_prefix_if:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; inst*

    mov rdi, rsi                ; value
    call tnode_type_size
    cmp rax, 2
    jne .skip

    mov rdi, [rbp-8]            ; inst*
    mov byte [rdi+24], 1        ; inst.l-prefix-size

    mov byte [rdi+0], 0x66      ; inst.l-prefix-size

.skip:
    leave
    ret

;;; rdi: inst*
;;; rsi: asm*
inst_write_prefix:
    xor rax, rax
    mov al, [rdi]               ; inst.rex-prefix
    cmp rax, 0
    je .break

    mov rdi, rsi                ; asm*
    mov rsi, rax
    call asm_write_u8

.break:
    ret

;;; rdi: inst*
inst_set_rex:
    mov byte [rdi+25], 1        ; inst.rex-prefix-size
                                ;                   0b    0000
    or byte [rdi+1], 0x40       ; inst.rex-prefix = 0b01000000
    ret

;;; rdi: inst*
inst_set_rex_r:
    mov byte [rdi+25], 1        ; inst.rex-prefix-size
                                ;                   0b    0R00
    or byte [rdi+1], 0x44       ; inst.rex-prefix = 0b01000100
    ret

;;; rdi: inst*
inst_set_rex_b:
    mov byte [rdi+25], 1        ; inst.rex-prefix-size
                                ;                   0b    000B
    or byte [rdi+1], 0x41       ; inst.rex-prefix = 0b01000001
    ret

;;; rdi: inst*
inst_set_rex_w:
    mov byte [rdi+25], 1        ; inst.rex-prefix-size
                                ;                   0b    W000
    or byte [rdi+1], 0x48       ; inst.rex-prefix = 0b01001000
    ret

;;; rdi: inst*
;;; rsi: rex
inst_set_rex_value:
    mov byte [rdi+25], 1        ; inst.rex-prefix-size
    mov rax, rsi
    or byte [rdi+1], al         ; inst.rex-prefix
    ret

;;; rdi: inst*
;;; rsi: asm*
inst_write_rex:
    xor rax, rax
    mov al, [rdi+1]             ; inst.rex-prefix
    cmp rax, 0
    je .break

    mov rdi, rsi                ; asm*
    mov rsi, rax
    call asm_write_u8

.break:
    ret

;;; rdi: inst*
;;; rsi: op
inst_append_opcode:
    xor rax, rax
    mov al, [rdi+26]            ; inst.opcode-size

    mov rcx, rdi                ; inst*
    add rcx, 2                  ; inst.opcode
    add rcx, rax                ; opcode-size offset

    mov rax, rsi                ; op
    mov byte [rcx], al          ; inst.opcode[inst.opcode-size] = (byte)op

    inc byte [rdi+26]           ; inst.opcode-size++
    ret

;;; rdi: asm*
;;; rsi: op
asm_inst_append_opcode:
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_append_opcode
    ret

;;; rdi: inst*
;;; rsi: sexp*(tnode)
inst_add_reg_to_opcode:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-16], rsi           ; sexp*
    mov [rbp-8], rdi            ; inst*

    mov rdi, [rbp-16]           ; sexp*
    call tnode_reg_extended
    cmp rax, 0
    je .skip_rex

    mov rdi, [rbp-8]            ; inst
    call inst_set_rex_b

.skip_rex:
    mov rdi, [rbp-16]           ; sexp*
    call tnode_reg_index        ; /r-digit

    mov rcx, [rbp-8]            ; inst*
    add rcx, 2                  ; inst.opcode
    add byte [rcx], al          ; inst.opcode[0] += /r-digit

    leave
    ret

;;; rdi: inst*
;;; rsi: sexp*(tnode)
asm_inst_add_reg_to_opcode:
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_add_reg_to_opcode
    ret

;;; rdi: inst*
;;; rsi: asm*
inst_write_opcode:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; counter
    mov [rbp-16], rsi           ; asm*
    mov [rbp-8], rdi            ; inst*

    mov qword [rbp-24], 0       ; counter = 0
.loop:
    mov rdi, [rbp-8]            ; inst*
    xor rax, rax
    mov al, [rdi+26]            ; inst.opcode-size
    mov rcx, [rbp-24]           ; counter
    cmp rax, rcx
    je .break

    mov rdi, [rbp-8]            ; inst*
    add rdi, 2                  ; inst.opcode
    add rdi, rcx                ; inst.opcode[i]*

    xor rax, rax
    mov al, [rdi]               ; inst.opcode[i]

    mov rdi, [rbp-16]           ; asm*
    mov rsi, rax
    call asm_write_u8

    inc qword [rbp-24]          ; counter = 0
    jmp .loop

.break:
    leave
    ret


;;; rdi: inst*
;;; rsi: u64, value
;;; rdx: u64, size
;;; rcx: u64, value-offset
;;; r8 : u64, size-offset
inst_set_value:
    mov rax, rdi                ; inst*
    add rax, rcx                ; inst.(value-offset)*

    mov rcx, rsi                ; value

    cmp rdx, 1
    je .u8

    cmp rdx, 2
    je .u16

    cmp rdx, 4
    je .u32

    cmp rdx, 8
    je .u64

    ;; debug
    mov rdi, rdx
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, 53
    call runtime_exit

.u8:
    mov byte [rax], cl          ; value
    jmp .break

.u16:
    mov word [rax], cx          ; value
    jmp .break

.u32:
    mov dword [rax], ecx        ; value
    jmp .break

.u64:
    mov qword [rax], rcx        ; value
    jmp .break

.break:
    mov rax, rdi                ; inst*
    add rax, r8                 ; inst.(size-offset)*
    mov byte [rax], dl          ; size

    ret


;;; rdi: inst*
;;; rsi: asm*
;;; rdx: u64, value-offset
;;; rcx: u64, size-offset
inst_write_value:
    mov rax, rdi                ; inst*
    add rax, rcx                ; inst.(size-offset)*

    xor rcx, rcx
    mov cl, [rax]               ; inst.(size-offset)

    mov rax, rdi                ; inst*
    add rax, rdx                ; inst.(value-offset)*

    cmp rcx, 0
    je .break

    cmp rcx, 1
    je .u8

    cmp rcx, 2
    je .u16

    cmp rcx, 4
    je .u32

    cmp rcx, 8
    je .u64

    ;; debug
    mov rdi, rcx
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, 54
    call runtime_exit

.u8:
    mov rdi, rsi                ; asm*
    mov al, [rax]               ; inst.(value-offset)
    mov rsi, rax
    call asm_write_u8

    jmp .break

.u16:
    mov rdi, rsi                ; asm*
    mov ax, [rax]               ; inst.(value-offset)
    mov rsi, rax
    call asm_write_u16

    jmp .break

.u32:
    mov rdi, rsi                ; asm*
    mov eax, [rax]              ; inst.(value-offset)
    mov rsi, rax
    call asm_write_u32

    jmp .break

.u64:
    mov rdi, rsi                ; asm*
    mov rax, [rax]              ; inst.(value-offset)
    mov rsi, rax
    call asm_write_u64

    jmp .break

.break:
    ret


;;; rdi: inst*
;;; rsi: u64, value
;;; rdx: u64, size
inst_set_disp_value:
    mov rcx, 8                  ; inst.disp
    mov r8, 29                  ; inst.disp-size
    call inst_set_value
    ret

;;; rdi: inst*
;;; rsi: asm*
inst_write_disp_value:
    mov rdx, 8                  ; inst.disp
    mov rcx, 29                 ; inst.disp-size
    call inst_write_value
    ret

;;; rdi: inst*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
;;; rcx: u64, signed
inst_set_disp:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-32], rcx           ; signed
    mov [rbp-24], rdx           ; size
    mov [rbp-16], rsi           ; sexp*, disp
    mov [rbp-8], rdi            ; inst*

    mov rdi, [rbp-16]           ; disp
    call tnode_type_tag

    cmp rax, 2                  ; label
    je .unsupported_operand     ; assume 32bits, TODO: fix

    cmp rax, 4                  ; integer
    jne .unsupported_operand

    ;;
    mov rdi, [rbp-16]           ; disp
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value ;
    mov rcx, rax                ; move disp value to rcx

    mov rax, [rbp-24]           ; size
    cmp rax, 1
    je .disp_1

    cmp rax, 2
    je .disp_2

    mov rax, [rbp-24]           ; size
    cmp rax, 4
    je .disp_4

    ;; ICE
    mov rdi, 180                ; debug
    call runtime_exit

.disp_1:
    mov rax, [rbp-32]           ; signed
    cmp rax, 0
    je .disp_set
    neg cl
    jmp .disp_set

.disp_2:
    mov rax, [rbp-32]           ; signed
    cmp rax, 0
    je .disp_set
    neg cx
    jmp .disp_set

.disp_4:
    mov rax, [rbp-32]           ; signed
    cmp rax, 0
    je .disp_set
    neg ecx
    jmp .disp_set

.disp_set:
    mov rdi, [rbp-8]            ; inst*
    mov rsi, rcx                ; value
    mov rdx, [rbp-24]           ; size
    call inst_set_disp_value

    jmp .break

.unsupported_operand:
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-16]           ; disp
    call tnode_print
    call runtime_print_newline

    ;; ICE
    mov rdi, 161                 ; debug
    call runtime_exit

.break:
    leave
    ret


;;; rdi: inst*
;;; rsi: u64, value
;;; rdx: u64, size
inst_set_imm_value:
    mov rcx, 16                 ; inst.imm
    mov r8, 30                  ; inst.imm-size
    call inst_set_value
    ret

;;; rdi: inst*
;;; rsi: asm*
inst_write_imm_value:
    mov rdx, 16                 ; inst.imm
    mov rcx, 30                 ; inst.imm-size
    call inst_write_value
    ret

;;; rdi: inst*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
inst_set_imm:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; inst*

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_type_tag

    cmp rax, 4                  ; int
    je .write_imm

.failed:
    mov rdi, str_debug_set_imm
    call runtime_print_string
    call runtime_print_newline

    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call sexp_print
    call runtime_print_newline

    mov rdi, str_ice_invalid_node
    call runtime_print_string
    call runtime_print_newline

    mov rdi, 3
    call runtime_exit

.write_imm:
    mov rax, [rbp-24]           ; size
    cmp rax, 8
    jne .skip_rex

    mov rdi, [rbp-8]            ; inst*
    call inst_set_rex_w

.skip_rex:
    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value

    mov rdi, [rbp-8]            ; inst*
    mov rsi, rax
    mov rdx, [rbp-24]           ; byte-size
    call inst_set_imm_value

    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
asm_inst_set_imm:
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_set_imm
    ret

;;; rdi: inst*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
inst_set_imm_sign_ext:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; sexp*(node)
    mov [rbp-8], rdi            ; inst*

    cmp qword [rbp-24], 8       ; size
    jne .skip_adjust

    mov rdi, [rbp-8]            ; inst*
    call inst_set_rex_w

    ;; TODO: warning
    mov qword [rbp-24], 4       ; size from imm64 to imm32

.skip_adjust:
    mov rdx, [rbp-24]           ; byte-size
    mov rsi, [rbp-16]           ; sexp*(node)
    mov rdi, [rbp-8]            ; inst*
    call inst_set_imm

    leave
    ret

;;; rdi: asm*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
asm_inst_set_imm_sign_ext:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; sexp*(node)
    mov [rbp-8], rdi            ; asm*

    cmp qword [rbp-24], 8       ; size
    jne .skip_adjust

    mov rdi, [rbp-8]            ; asm*
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_set_rex_w

    ;; TODO: warning
    mov qword [rbp-24], 4       ; size from imm64 to imm32

.skip_adjust:
    mov rdx, [rbp-24]           ; byte-size
    mov rsi, [rbp-16]           ; sexp*(node)
    mov rdi, [rbp-8]            ; asm*
    call asm_inst_set_imm

    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
asm_inst_set_rel:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-32], 0       ; offset
    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; asm*

    mov rdx, [rbp-24]           ; byte-size
    cmp rdx, 0                  ; byte-size
    je .failed


;
;.skip_adjust_rel8:
    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_type_tag

    cmp rax, 4                  ; int
    je .write_rel

.failed:
    mov rdi, str_debug_set_rel
    call runtime_print_string
    call runtime_print_newline

    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call sexp_print
    call runtime_print_newline

    mov rdi, str_ice_invalid_node
    call runtime_print_string
    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.write_rel:
    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value
    mov [rbp-32], rax           ; offset = target

    mov rdi, [rbp-8]            ; asm*
    call asm_current_loc
    sub [rbp-32], rax           ; offset -= $

    mov rdi, [rbp-8]            ; asm*
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_current_size
    sub [rbp-32], rax           ; offset -= inst-size

    mov rax, [rbp-24]           ; byte-size
    sub qword [rbp-32], rax     ; offset -= byte-size (maybe size of imm)

    mov rdi, [rbp-8]            ; asm*
    lea rdi, [rdi+56]           ; asm.inst*
    mov rsi, [rbp-32]           ; offset
    mov rdx, [rbp-24]           ; byte-size
    call inst_set_imm_value

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
asm_inst_set_rel_sign_ext:
    cmp rdx, 8                  ; size
    jne .skip_adjust

    ;; TODO: warning
    mov rdx, 4                  ; size from imm64 to imm32

.skip_adjust:
    call asm_inst_set_rel

    ret


;;; rdi: asm*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
asm_inst_set_rel_sign_ext_8_32:
    cmp rdx, 1                  ; size == 1
    je .skip_adjust

    cmp rdx, 8                  ; size >= 8
    jge .skip_adjust

    mov rdx, 4                  ; size imm16 to imm32

.skip_adjust:
    call asm_inst_set_rel_sign_ext

    ret

;;; rdi: asm*
;;; rsi: sexp*(tnode)
;;; rdx: u64, size
asm_inst_set_rel_sign_ext_32:
    cmp rdx, 8                  ; size >= 8
    jge .skip_adjust

    mov rdx, 4                  ; size imm8 or imm16 to imm32

.skip_adjust:
    call asm_inst_set_rel_sign_ext

    ret


;;; rdi: inst*
;;; rsi: u64, value
inst_set_mod_rm_value:
    mov rdx, 1                  ; size = 1
    mov rcx, 6                  ; inst.mod-rm
    mov r8, 27                  ; inst.mod-rm-size
    call inst_set_value
    ret

;;; rdi: inst*
;;; rsi: asm*
inst_write_mod_rm_value:
    mov rdx, 6                  ; inst.mod-rm
    mov rcx, 27                 ; inst.mod-rm-size
    call inst_write_value
    ret


;;; rdi: inst*
;;; rsi: u64, value
inst_set_sib_value:
    mov rdx, 1                  ; size = 1
    mov rcx, 7                  ; inst.sib
    mov r8, 28                  ; inst.sib-size
    call inst_set_value
    ret

;;; rdi: inst*
;;; rsi: asm*
inst_write_sib_value:
    mov rdx, 7                  ; inst.sib
    mov rcx, 28                 ; inst.sib-size
    call inst_write_value
    ret


;;; rdi: inst*
;;; rsi: asm*
inst_write:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; counter
    mov [rbp-16], rdi           ; inst*
    mov [rbp-8], rsi            ; asm*

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-8]            ; asm*
    call inst_write_prefix

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-8]            ; asm*
    call inst_write_rex

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-8]            ; asm*
    call inst_write_opcode

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-8]            ; asm*
    call inst_write_mod_rm_value

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-8]            ; asm*
    call inst_write_sib_value

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-8]            ; asm*
    call inst_write_disp_value

    mov rdi, [rbp-16]           ; inst*
    mov rsi, [rbp-8]            ; asm*
    call inst_write_imm_value

    leave
    ret

;;; rdi: asm*
asm_inst_write:
    mov rsi, rdi                ; asm*
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_write
    ret

;;; rdi: inst*
;;; rsi: sexp*(tnode), /r-digit
;;; rdx: sexp*(tnode), effective-reg
inst_set_operands:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-24], rdx           ; sexp*(tnode), effective-reg
    mov [rbp-16], rsi           ; sexp*(tnode), /r-digit
    mov [rbp-8], rdi            ; inst*

    mov rdi, [rbp-16]           ; sexp*(tnode), /r-digit
    call tnode_reg_extended
    cmp rax, 0
    je .skip_rex

    mov rdi, [rbp-8]            ; inst*
    call inst_set_rex_r

.skip_rex:
    mov rdi, [rbp-16]           ; sexp*(tnode), /r-digit
    call tnode_reg_index        ; /r-digit

    mov rdi, [rbp-8]            ; inst*
    mov rsi, rax                ; /r-digit
    mov rdx, [rbp-24]           ; sexp*(tnode), effective-reg
    call inst_set_r_digit_operands

    leave
    ret

;;; rdi: asm*
;;; rsi: sexp*(tnode), /r-digit
;;; rdx: sexp*(tnode), effective-reg
asm_inst_set_operands:
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_set_operands
    ret

;;; rdi: inst*
;;; rsi: u64, /r
;;; rdx: sexp*(tnode), effective-reg
inst_set_r_digit_operands:
    push rbp
    mov rbp, rsp
    sub rsp, 120

    mov qword [rbp-120], 0      ; args.scale-reg
    mov qword [rbp-112], 0      ; args.scale-N
    mov qword [rbp-104], 0      ; args.base-reg
    mov qword [rbp-96], 0       ; args.disp

    mov byte [rbp-80], 0        ; has-sib
    mov byte [rbp-79], 0        ; sib-ss (scaled index)
    mov byte [rbp-78], 0        ; sib-index
    mov byte [rbp-77], 0        ; sib-r32

    mov byte [rbp-72], 0        ; mod
    mov byte [rbp-71], 0        ; disp-signed
    mov byte [rbp-70], 0        ; r/m
    mov byte [rbp-69], 0        ; disp-size

    mov qword [rbp-56], 0       ; sexp*(tnode), disp
    mov qword [rbp-48], 0       ; sexp*(tnode), index-reg
    mov qword [rbp-40], 0       ; sexp*(tnode), inner-expr in [addressing]

    mov [rbp-24], rdx           ; sexp*(tnode), effective-reg
    mov [rbp-16], rsi           ; u64, /r-digit
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-24]           ; effective-reg
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .mod3

    cmp rax, 5                  ; addressing
    je .maybe_mod_0_1_2

    ;; ICE
    mov rdi, 12                 ; debug
    call runtime_exit

.maybe_mod_0_1_2:
    mov rdi, [rbp-24]           ; effective-reg
    call tnode_value
    mov [rbp-40], rax

    ;;
    mov rdi, [rbp-40]
    call sexp_car
    mov [rbp-120], rax          ; args.scale-reg
    mov rdi, [rbp-40]
    call sexp_cdr
    mov [rbp-40], rax

    ;;
    mov rdi, [rbp-40]
    call sexp_car
    mov [rbp-112], rax          ; args.scale-N
    mov rdi, [rbp-40]
    call sexp_cdr
    mov [rbp-40], rax

    ;;
    mov rdi, [rbp-40]
    call sexp_car
    mov [rbp-104], rax          ; args.base-reg
    mov rdi, [rbp-40]
    call sexp_cdr
    mov [rbp-40], rax

    ;;
    mov rdi, [rbp-40]
    call sexp_car
    mov [rbp-96], rax           ; args.disp
    mov rdi, [rbp-40]
    call sexp_cdr
    mov [rbp-40], rax

    ;;
    mov rdi, [rbp-24]           ; effective-reg
    call tnode_addr_kind
    and rax, 0xf0               ; kind.signed-mask
    mov byte [rbp-71], al       ; disp-signed

    ;;
    cmp qword [rbp-96], 0       ; args.disp != nil
    jne .maybe_mod_1_2

    ;; [REG]
.maybe_mod_0:
    mov byte [rbp-72], 0        ; mod

    mov rdi, [rbp-104]          ; args.base-reg
    call tnode_reg_index
    mov byte [rbp-70], al       ; r/m

    cmp al, 5                   ; 0b101, EBP
    je .maybe_mod_0_to_mod_1_disp

;    ;; SIB
;    cmp qword [rbp-120], 0      ; args.sib == 0
;    je .maybe_mod_0_not_sib
;
;    mov byte [rbp-70], 0x04     ; r/m = 0b100
;
;    mov byte [rbp-80], 1        ; has-sib
;    mov byte [rbp-79], 0        ; sib-ss = 0b00
;    mov byte [rbp-77], al       ; sib-r32 = base-reg
;    mov rdi, [rbp-120]          ; args.base-reg
;    call tnode_reg_index
;    mov byte [rbp-78], al       ; sib-index = none, 0b100
;
;    jmp .mod
;
;.maybe_mod_0_not_sib:


    jmp .mod

.mod_0_disp:
    mov byte [rbp-72], 0        ; mod
    mov byte [rbp-70], 4        ; r/m, 0x100
    mov byte [rbp-69], 4        ; disp-size, disp32

    mov byte [rbp-80], 1        ; has-sib
    mov byte [rbp-79], 0        ; sib-ss = 0b00
    mov byte [rbp-78], 4        ; sib-index = none, 0b100
    mov byte [rbp-77], 5        ; sib-r32 = [*], 0b101

    jmp .mod

.maybe_mod_0_to_mod_1_disp:
    mov byte [rbp-72], 1        ; mod
    mov byte [rbp-69], 1        ; disp-size

    mov rdi, 0
    call sexp_alloc_int
    mov rdi, rax
    call tnode_alloc_uint_node
    mov [rbp-96], rax           ; args.disp = 0

    jmp .mod

.maybe_mod_1_2:
    mov rdi, [rbp-104]          ; args.base-reg
    cmp rdi, 0
    je .mod_0_disp

    mov rdi, [rbp-96]           ; disp
    call tnode_calc_imm_size
    cmp rax, 1                  ; sizeof(disp) > 1
    jg .maybe_mod_2

    ;; [REG] + disp8
.maybe_mod_1:
    mov byte [rbp-72], 1        ; mod
    mov byte [rbp-69], 1        ; disp-size

    mov rdi, [rbp-104]          ; args.base-reg
    call tnode_reg_index
    mov byte [rbp-70], al       ; r/m

    jmp .mod

    ;; [REG] + disp32
.maybe_mod_2:
	mov rdi, 44
    call runtime_exit


    mov rdi, [rbp-24]

    ;;
    mov byte [rbp-72], 2        ; mod

    mov rdi, [rbp-104]          ; args.base-reg
    call tnode_reg_index
    mov byte [rbp-70], al       ; r/m

    mov rdi, [rbp-24]           ; effective-reg
    call tnode_addr_kind

    mov rdi, [rbp-96]           ; args.disp
    mov [rbp-56], rdi           ; disp

    mov rdi, [rbp-96]           ; disp
    call tnode_calc_imm_size
    cmp rax, 1
    je .disp8

    mov byte [rbp-69], 4        ; disp-size = 4
    jmp .disp_set

.disp8:
    mov byte [rbp-69], 1        ; disp-size = 1

.disp_set:
    jmp .mod

    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline
    call runtime_exit

    mov [rbp-40], rax           ; sexp*, value([addressing])

    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .mod0_no_disp

    cmp rax, 6                  ; expr (addressing)
    je .maybe_mod_1_2

    cmp rax, 4                  ; integer
    je .mod0_disp

    ;; ICE
    mov rdi, rax
    call runtime_print_uint64
    mov rdi, 19                 ; debug
    call runtime_exit

;.maybe_mod_1_2:
    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_type
    shr rax, 16                 ; 2 * 8  bits, op
    and rax, 0x000000000000ffff
    mov byte [rbp-71], al       ; disp-signed

    ;; lhs (register)
    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_value
    mov rdi, rax                ; inner-value, value(expr)
    call sexp_car               ; lhs (type . value)
    mov [rbp-48], rax           ; index-reg
    ;; TODO: check type is register

    ;; rhs (disp)
    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_value
    mov rdi, rax
    call sexp_cdr               ; rhs (type . value)
    mov [rbp-56], rax           ; disp

    mov rdi, [rbp-56]           ; disp
    call tnode_type_tag

    cmp rax, 2                  ; label
    je .mod2                    ; assume 32bits

    cmp rax, 4                  ; integer
    jne .unsupported_operand

    mov rdi, [rbp-56]           ; disp
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value ; value

    cmp rax, 128
    jge .mod2

    jmp .mod1

    ;; [REG]
.mod0_no_disp:
    mov rdi, [rbp-40]           ; inner-arg (register)
    call tnode_reg_index
    cmp rax, 5                  ; rbp, r13
    je .mod_1_with_disp0        ; special treatment

    mov byte [rbp-72], 0        ; mod

    mov rdi, [rbp-40]           ; inner-arg (register)
    call tnode_reg_extended
    cmp rax, 0
    je .mod0_no_disp_skip_rex

    mov rdi, [rbp-8]            ; inst*
    call inst_set_rex_b

.mod0_no_disp_skip_rex:
    mov rdi, [rbp-40]           ; index-reg
    call tnode_reg_index
    mov byte [rbp-70], al       ; r/m

    mov byte [rbp-69], 0        ; disp-size

    jmp .mod

    ;; [disp32]
.mod0_disp:
    mov byte [rbp-72], 0        ; mod
    mov byte [rbp-70], 4        ; r/m, 0x100
    mov byte [rbp-69], 4        ; disp-size, disp32

    mov rax, [rbp-40]           ; inner-arg (disp)
    mov [rbp-56], rax           ; disp

    mov byte [rbp-80], 1        ; has-sib
    mov byte [rbp-79], 0        ; sib-ss = 0b00
    mov byte [rbp-78], 4        ; sib-index = none, 0b100
    mov byte [rbp-77], 5        ; sib-r32 = [*], 0b101

    jmp .mod

.mod_1_with_disp0:
    mov rdi, [rbp-40]           ; inner-arg (register)
    mov [rbp-48], rdi           ; index-reg

    mov rdi, 0
    call sexp_alloc_int
    mov rdi, rax
    mov rsi, 0
    call tnode_alloc_uint_node
    mov [rbp-56], rax           ; disp

    ;; [][] + disp8 or [REG + disp8]
.mod1:


    mov byte [rbp-72], 1        ; mod

    mov rdi, [rbp-48]           ; index-reg
    call tnode_reg_extended
    cmp rax, 0
    je .mod1_skip_rex

    mov rdi, [rbp-8]            ; inst*
    call inst_set_rex_b

.mod1_skip_rex:
    mov rdi, [rbp-48]           ; index-reg
    call tnode_reg_index
    mov byte [rbp-70], al       ; r/m

    mov byte [rbp-69], 1        ; disp-size

    jmp .mod

    ;; [][] + disp32 or [REG + disp32]
.mod2:
    ;; NOTE: currently not supported
    mov rdi, 16                 ; debug
    call runtime_exit

    jmp .mod

    ;; REG
.mod3:
    mov byte [rbp-72], 3        ; mod

    mov rax, [rbp-24]           ; effective-reg
    mov qword [rbp-104], rax    ; args.base-reg

    mov rdi, [rbp-104]          ; args.base-reg
    call tnode_reg_index
    mov byte [rbp-70], al       ; r/m

    jmp .mod

.mod:
    ;; SIB
    cmp qword [rbp-120], 0      ; args.sib == 0
    je .mod_not_sib

    mov al, [rbp-70]            ; r/m
    mov byte [rbp-70], 0x04     ; r/m = 0b100

    mov byte [rbp-80], 1        ; has-sib


;
;    mov byte [rbp-79], 0        ; sib-ss = 0b00
;    mov byte [rbp-77], al       ; sib-r32 = base-reg
;    mov rdi, [rbp-120]          ; args.scale-reg
;    call tnode_reg_index
;    mov byte [rbp-78], al       ; sib-index
;
;    mov rdi, [rbp-120]
;    call tnode_print
;    mov rdi, [rbp-104]
;    call tnode_print
;
;    cmp qword [rbp-112], 0      ; args.scale-N
;    je .skip_set_scale
;
;    mov rdi, qword [rbp-112]    ; args.scale-N
;    call tnode_value
;    mov rdi, rax
;    call sexp_internal_int_value
;
;    cmp rax, 1
;    je .mod_sib_1
;
;    cmp rax, 2
;    je .mod_sib_2
;
;    cmp rax, 4
;    je .mod_sib_4
;
;    cmp rax, 8
;    je .mod_sib_8
;
;    ;; debug
;    mov rdi, 104
;    call runtime_exit
;
;.mod_sib_1:
;    mov byte [rbp-79], 0        ; sib-ss
;    jmp .mod_not_sib
;
;.mod_sib_2:
;    mov byte [rbp-79], 1        ; sib-ss
;    jmp .mod_not_sib
;
;.mod_sib_4:
;    mov byte [rbp-79], 2        ; sib-ss
;    jmp .mod_not_sib
;
;.mod_sib_8:
;    mov byte [rbp-79], 3        ; sib-ss
;    jmp .mod_not_sib

.skip_set_scale:
.mod_not_sib:
    mov rdi, [rbp-104]          ; args.base-reg
    cmp rdi, 0
    je .mod_skip_rex
    call tnode_reg_extended
    cmp rax, 0
    je .mod_skip_rex

    mov rdi, [rbp-8]            ; inst*
    call inst_set_rex_b

.mod_skip_rex:
    ;; mod r/m
    xor rcx, rcx

    mov al, [rbp-72]            ; Mod
    shl al, 6                   ; 0b11000000, Mod
    or cl, al

    mov al, [rbp-16]            ; Reg
    shl al, 3                   ; 0b00111000, Reg
    or cl, al

    mov al, [rbp-70]            ; R/M
    or cl, al                   ; 0b00000111, R/M

    mov rdi, [rbp-8]            ; inst*
    mov rsi, rcx                ; mod r/m
    call inst_set_mod_rm_value

    ;; write SIB
    cmp byte [rbp-80], 0        ; has-sib == 0
    je .mod_set_disp            ; skip SIB, -> disp

    ;; sib base
.set_sib_base_reg:
    mov rdi, [rbp-104]          ; args.base-reg
    cmp rdi, 0
    jne .set_sib_with_base_reg  ; has base-register

.set_sib_without_base_reg:
    mov byte [rbp-77], 5        ; sib-r32 = [*], 0b101
    jmp .set_sib_scale_reg

.set_sib_with_base_reg:
    mov rdi, [rbp-104]          ; args.base-reg
    call tnode_reg_index
    mov byte [rbp-77], al       ; r32 = base-reg
    jmp .set_sib_scale_reg

    ;; sib index
.set_sib_scale_reg:
    mov rdi, [rbp-120]          ; args.scale-reg
    cmp rdi, 0
    jne .set_sib_with_scale_reg ; has base-register

.set_sib_without_scale_reg:
    mov byte [rbp-78], 4        ; sib-index = none, 0b100
    jmp .set_sib_scale

.set_sib_with_scale_reg:
    mov rdi, [rbp-120]          ; args.scale-reg
    call tnode_reg_index
    mov byte [rbp-78], al       ; sib-index = scale-reg
    jmp .set_sib_scale

    ;; sib scale
.set_sib_scale:
    mov byte [rbp-79], 0        ; sib-ss = 0b00 (default)

    cmp qword [rbp-112], 0      ; args.scale-N
    je .set_sib                 ; use default scale

    mov rdi, qword [rbp-112]    ; args.scale-N
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value

    cmp rax, 1
    je .set_sib_scale_1

    cmp rax, 2
    je .set_sib_scale_2

    cmp rax, 4
    je .set_sib_scale_4

    cmp rax, 8
    je .set_sib_scale_8

    ;; debug
    mov rdi, 104
    call runtime_exit

.set_sib_scale_1:
    mov byte [rbp-79], 0        ; sib-ss
    jmp .set_sib

.set_sib_scale_2:
    mov byte [rbp-79], 1        ; sib-ss
    jmp .set_sib

.set_sib_scale_4:
    mov byte [rbp-79], 2        ; sib-ss
    jmp .set_sib

.set_sib_scale_8:
    mov byte [rbp-79], 3        ; sib-ss
    jmp .set_sib

.set_sib:
    ;; SIB
    xor rcx, rcx

    mov al, [rbp-79]            ; SS
    shl al, 6                   ; 0b11000000, SS
    or cl, al

    mov al, [rbp-78]            ; index
    shl al, 3                   ; 0b00111000, index
    or cl, al

    mov al, [rbp-77]            ; r32
    or cl, al                   ; 0b00000111, r32

    mov rdi, [rbp-8]            ; inst*
    mov rsi, rcx                ; SIB
    call inst_set_sib_value

;    call runtime_print_newline
;    xor rax, rax
;    mov al, [rbp-77]            ; r32
;    mov rdi, rax
;    call runtime_print_uint64
;    call runtime_print_newline
;    call runtime_exit

;    ;; rex
;    mov rdi, [rbp-24]           ; effective-reg
;    call tnode_type_size
;    cmp rax, 8
;    jne .mod_skip_rex
;
;    mov rdi, [rbp-8]            ; inst*
;    call inst_set_rex_w
;
;.mod_skip_rex:
.mod_set_disp:
    ;; disp
    mov rax, [rbp-96]           ; disp
    cmp rax, 0
    je .mod_skip_disp

.mod_set_disp_do:
    mov rdi, [rbp-8]            ; inst*
    mov rsi, [rbp-96]           ; disp
    xor rdx, rdx
    mov dl, byte [rbp-69]       ; disp-size
    xor rcx, rcx
    mov cl, byte [rbp-71]       ; disp-signed
    call inst_set_disp

.mod_skip_disp:
    jmp .break

.unsupported_operand:
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    ;; ICE
    mov rdi, 160                ; debug
    call runtime_exit

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: u64, /r
;;; rdx: sexp*(tnode), effective-reg
asm_inst_set_r_digit_operands:
    lea rdi, [rdi+56]           ; asm.inst*
    call inst_set_r_digit_operands
    ret


;;; rdi: asm*
;;; rsi: sexp*(tnode), effective-reg
;;; rdx: u64, /r
asm_write_operands:
    push rbp
    mov rbp, rsp
    sub rsp, 72

    mov byte [rbp-72], 0        ; mod
    mov byte [rbp-71], 0        ; disp-signed
    mov byte [rbp-70], 0        ; r/m
    mov byte [rbp-69], 0        ; disp-size

    mov qword [rbp-56], 0       ; sexp*(tnode), disp
    mov qword [rbp-48], 0       ; sexp*(tnode), index-reg
    mov qword [rbp-40], 0       ; sexp*(tnode), inner-expr in [addressing]

    mov [rbp-24], rdx           ; u64, /r-digit
    mov [rbp-16], rsi           ; sexp*(tnode), effective-reg
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-16]           ; effective-reg
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .mod3

    cmp rax, 5                  ; addressing
    je .mod0to2

    ;; ICE
    mov rdi, 12                 ; debug
    call runtime_exit

.mod0to2:
    mov rdi, [rbp-16]           ; effective-reg
    call tnode_value
    mov [rbp-40], rax           ; sexp*, value([addressing])

    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .mod0_no_disp

    cmp rax, 6                  ; expr (addressing)
    je .mod1to2

    ;; ICE
    mov rdi, 28                 ; debug
    call runtime_exit

.mod0_no_disp:
    ;; ICE
    mov rdi, 14                 ; debug
    call runtime_exit

.mod1to2:
    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_type
    shr rax, 16                 ; 2 * 8  bits, op
    and rax, 0x000000000000ffff
    mov byte [rbp-71], al       ; disp-signed

    ;; lhs (register)
    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_value
    mov rdi, rax                ; inner-value, value(expr)
    call sexp_car               ; lhs (type . value)
    mov [rbp-48], rax           ; index-reg
    ;; TODO: check type is register

    ;; rhs (disp)
    mov rdi, [rbp-40]           ; inner-arg, sexp*(tnode)
    call tnode_value
    mov rdi, rax
    call sexp_cdr               ; rhs (type . value)
    mov [rbp-56], rax           ; disp

    mov rdi, [rbp-56]           ; disp
    call tnode_type_tag

    cmp rax, 2                  ; label
    je .mod2                    ; assume 32bits

    cmp rax, 4                  ; integer
    jne .unsupported_operand

    mov rdi, [rbp-56]           ; disp
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value ; value

    cmp rax, 128
    jge .mod2

    ;; [REG + disp8]
.mod1:
    mov byte [rbp-72], 1         ; mod

    mov rdi, [rbp-48]            ; index-reg
    call tnode_reg_index

    mov byte [rbp-70], al        ; r/m

    mov byte [rbp-69], 1         ; disp-size

    jmp .mod

    ;; [REG + disp32]
.mod2:
    ;; ICE, not-supported
    mov rdi, 162                 ; debug
    call runtime_exit

    ;; REG
.mod3:
    mov byte [rbp-72], 3        ; mod

    mov rdi, [rbp-16]           ; effective-reg
    call tnode_reg_index

    mov byte [rbp-70], al       ; r/m

.mod:
    ;; Mod R/M
    xor rcx, rcx

    xor rax, rax
    mov al, [rbp-72]            ; Mod
    shl al, 6                   ; 0b11000000, Mod
    or cl, al

    mov al, [rbp-24]            ; Reg
    shl al, 3                   ; 0b00111000, Reg
    or cl, al

    mov al, [rbp-70]            ; R/M
    or cl, al                   ; 0b00000111, R/M

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rcx
    call asm_write_u8

    ;; Disp
    mov rax, [rbp-56]           ; disp
    cmp rax, 0
    jne .disp

.mod_after_disp:
    jmp .break

.disp:
    mov rdi, [rbp-56]           ; disp
    call tnode_type_tag

    cmp rax, 2                  ; label
    je .unsupported_operand     ; assume 32bits, TODO: fix

    cmp rax, 4                  ; integer
    jne .unsupported_operand

    mov rdi, [rbp-56]           ; disp
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value ; value-tag
    mov rcx, rax

    mov al, byte [rbp-69]       ; disp-size
    cmp al, 1
    je .disp_1

    ;; ICE
    mov rdi, 17                 ; debug
    call runtime_exit

.disp_1:
    mov al, [rbp-71]            ; disp-signed
    cmp al, 0
    je .disp_1_not_signed

    neg cl

.disp_1_not_signed:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rcx
    call asm_write_u8

    jmp .mod_after_disp

.unsupported_operand:
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    ;; ICE
    mov rdi, 163                 ; debug
    call runtime_exit

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: arg*, imm
;;; rdx: u64, size
asm_write_imm_operand:
    push rbp
    mov rbp, rsp

;    mov rdi, rdi                ; asm*
;    mov rsi, rsi                ; (type . value): sexp* as hd
 ;   mov rdx, rdx                ; size
    call asm_write_node

    leave
    ret

;;; rdi: asm*
;;; rsi: arg*, imm
;;; rdx: u64, size
asm_write_sign_ext_imm_operand:
    cmp rdx, 8                  ; size
    jne .skip_adjust

    ;; TODO: warning
    mov rdx, 4                  ; size to imm32

.skip_adjust:
    call asm_write_imm_operand
    ret


;;; rdi: asm*
;;; rsi: arg*, imm
;;; rdx: u64, size
asm_write_rel_operand:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-32], 0       ; offset
    mov qword [rbp-24], rdx     ; size
    mov qword [rbp-16], rsi     ; arg*
    mov qword [rbp-8], rdi      ; asm*

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_type_tag

    cmp rax, 4                  ; int
    je .write_rel

    mov rdi, [rbp-8]            ; asm*
    mov cl, [rdi+40]            ; asm.phase = 0
    cmp cl, 0
    je .write_dummy             ; ignore evaluation when phase == 0

    ;; failed
    mov rdi, str_debug_set_rel_operand
    call runtime_print_string
    call runtime_print_newline

    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call sexp_print
    call runtime_print_newline

    mov rdi, str_ice_invalid_node
    call runtime_print_string
    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.write_rel:
    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value
    mov [rbp-32], rax           ; offset

    mov rdi, [rbp-8]            ; asm*
    call asm_current_loc

    sub [rbp-32], rax           ; offset

    mov rax, [rbp-24]           ; byte-size
    sub qword [rbp-32], rax     ; offset -= byte-size

    mov rdi, [rbp-8]            ; asm*
    add eax, dword [rdi+44]     ; asm.current-inst-size
    sub qword [rbp-32], rax     ; offset -= inst-size

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; int-value, offset
    mov rdx, [rbp-24]           ; byte-size
    call asm_write_value

    jmp .break

.write_dummy:
    cmp qword [rbp-24], 0       ; byte-size
    jne .skip_size_adjustment

    mov qword [rbp-24], 1       ; adjust byte-size

.skip_size_adjustment:
    ;; fill space temporary by using dummy-value
    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0                  ; int-value
    mov rdx, [rbp-24]           ; byte-size
    call asm_write_value

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: arg*, imm
;;; rdx: u64, size
asm_write_sign_ext_rel_operand:
    cmp rdx, 8                  ; size
    jne .skip_adjust

    ;; TODO: warning
    mov rdx, 4                  ; size to imm32

.skip_adjust:
    cmp rdx, 1                  ; size
    je .skip_adjust_rel8

    mov rdx, 4                  ; rel32

.skip_adjust_rel8:
    call asm_write_rel_operand
    ret


;;; rdi: sexp*, (type . value)
;;; rsi: arg*
asm_decode_arg:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov [rbp-16], rsi           ; arg*
    mov [rbp-8], rdi            ; arg: sexp*

    ;; type, sexp* -> u64 then store
    mov rdi, [rbp-8]
    call sexp_car
    mov rdi, rax
    call sexp_internal_int_value ; value-tag

    mov rdi, [rbp-16]
    mov qword [rdi], rax        ; arg* + 0 = type(u64)

    ;; value, sexp* then store
    mov rdi, [rbp-8]
    call sexp_cdr

    mov rdi, [rbp-16]
    mov qword [rdi+8], rax      ; arg* + 8 = value(sexp*)

    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*, tnode
;;; -> sexp*, tnode
asm_eval_expr_addressing:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; cdr
    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; asm*

    cmp qword [rbp-16], 0
    je .nil

    ;; eval cdr
    mov rdi, [rbp-16]
    call sexp_cdr
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_eval_expr_addressing
    mov [rbp-24], rax           ; cdr

    ;; eval car
    mov rdi, [rbp-16]
    call sexp_car
    cmp rax, 0
    je .skip_eval
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_eval_expr

.skip_eval:
    mov rdi, rax                ; car
    mov rsi, [rbp-24]           ; cdr
    call sexp_alloc_cons

    jmp .break

.nil:
    xor rax, rax

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: sexp*, tnode
;;; -> sexp*, tnode
asm_eval_expr:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov qword [rbp-88], 0       ; calc

    mov qword [rbp-80], 0       ; (type . value): sexp*: lhs
    mov qword [rbp-72], 0       ; arg* : lhs.type : u64
    mov qword [rbp-64], 0       ;      : lhs.value: sexp*

    mov qword [rbp-56], 0       ; (type . value): sexp*: rhs
    mov qword [rbp-48], 0       ; arg* : rhs.type : u64
    mov qword [rbp-40], 0       ;      : rhs.value: sexp*

    mov qword [rbp-48], 0       ; (type . value): sexp*: lhs
    mov qword [rbp-40], 0       ; lhs.tmp

    mov qword [rbp-32], 0       ; (type . value): sexp*: rhs
    mov qword [rbp-24], 0       ; rhs.tmp

    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; asm*

    ;; arg
    mov rdi, [rbp-16]           ; sexp*(tnode)
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .arg_integer

    cmp rax, 3                  ; string
    je .arg_string

    cmp rax, 1                  ; register
    je .arg_register

    cmp rax, 2                  ; label
    je .arg_label

    cmp rax, 8                  ; expr
    je .arg_expr

    cmp rax, 5                  ; addressing
    je .arg_addressing

    ;; failed

    mov rdi, 29                 ; debug
    call runtime_exit

.arg_expr:
    ;; phase0 never solves all-nested exprs
    mov rdi, [rbp-8]            ; asm*
    mov cl, [rdi+40]            ; asm.phase = 0
    cmp cl, 0
    je .dummy_int_node          ; ignore evaluation when phase == 0

    ;; eval-lhs
    mov rdi, [rbp-16]           ; sexp*(tnode)
    call tnode_value
    mov rdi, rax                ; (lhs . rhs)
    call sexp_car               ; lhs
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_eval_expr
    mov qword [rbp-48], rax     ; evaluated-lhs

    ;; eval-rhs
    mov rdi, [rbp-16]           ; sexp*(tnode)
    call tnode_value
    mov rdi, rax                ; (lhs . rhs)
    call sexp_cdr               ; rhs
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_eval_expr
    mov qword [rbp-32], rax     ; evaluated-rhs

    ;; type check
    mov rdi, [rbp-48]           ; lhs
    call tnode_type_tag
    mov [rbp-40], rax           ; lhs.tmp = type

    mov rdi, [rbp-32]           ; rhs
    call tnode_type_tag
    mov [rbp-24], rax           ; rhs.tmp = type

    mov rax, [rbp-40]
    mov rcx, [rbp-24]
    cmp rax, rcx                ; lhs.type ? rhs.type
    jne .arg_expr_cannot_resolve

    cmp rax, 2                  ; label
    je .arg_expr_cannot_resolve

    cmp rax, 4                  ; int
    jne .invalid_op

    ;; both of integer, evaluate
    ;; lhs + rhs => rhs + lhs
    ;; lhs - rhs => neg(rhs) + lhs
    mov rdi, [rbp-32]           ; rhs
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value
    mov [rbp-88], rax           ; calc

    mov rdi, [rbp-16]           ; sexp*(tnode)
    call tnode_type
    shr rax, 16                 ; 8 * 2 bits
    and rax, 0x000000000000ffff ; op

    cmp rax, 0                  ; add
    je .eval_add

    ;; to substruct values in add inst, nagate operand
    mov rax, [rbp-88]           ; calc
    neg rax
    mov [rbp-88], rax           ; calc

.eval_add:
    mov rdi, [rbp-48]           ; lhs
    call tnode_value
    mov rdi, rax
    call sexp_internal_int_value

    add [rbp-88], rax           ; calc + lhs

    mov rdi, [rbp-88]           ; calculated
    call sexp_alloc_int

    mov rdi, rax
    mov rsi, 0                  ; TODO
    call tnode_alloc_uint_node  ; TODO

    jmp .break

.arg_expr_cannot_resolve:
    ;; re-construct
    mov rdi, qword [rbp-48]     ; evaluated-lhs
    mov rsi, qword [rbp-32]     ; evaluated-rhs
    call sexp_alloc_cons
    mov [rbp-88], rax           ; tmp, value (lhs . rhs)

    mov rdi, [rbp-16]           ; (type . value): sexp*
    call sexp_car

    mov rdi, rax                ; type
    mov rsi, [rbp-88]           ;
    call sexp_alloc_cons
    mov [rbp-16], rax           ;

    mov rax, [rbp-16]           ;

    jmp .break

.arg_integer:
.arg_string:
.arg_register:
    jmp .pass_through

.pass_through:
    mov rax, [rbp-16]           ; pass-through
    jmp .break

.dummy_int_node:
    mov rdi, 0x80               ; dummy-value
    call sexp_alloc_int
    mov rdi, rax
    mov rsi, 1                  ; expected-size
    call tnode_alloc_uint_node
    jmp .break

.arg_label:
    ;; phase0 never solves all-nested exprs
    mov rdi, [rbp-8]            ; asm*
    mov cl, [rdi+40]            ; asm.phase = 0
    cmp cl, 0
    je .dummy_int_node          ; ignore evaluation when phase == 0

    ;; $
    mov rdi, [rbp-16]           ; (type . value): sexp*
    call tnode_value            ; value as symbol
    mov rdi, [g_sexp_symbol_doller]
    mov rsi, rax
    call sexp_equals
    cmp rax, 0
    jne .arg_label_doller

    ;; $$
    mov rdi, [rbp-16]           ; (type . value): sexp*
    call tnode_value            ; value as symbol
    mov rdi, [g_sexp_symbol_doller_doller]
    mov rsi, rax
    call sexp_equals
    cmp rax, 0
    jne .arg_label_doller_doller

    ;; other
    mov rdi, [rbp-16]           ; (type . value): sexp*
    call tnode_value            ; value as symbol
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_find_label_value

    cmp rax, 0
    jne .break                  ; found

    jmp .invalid_op

.arg_label_doller:
    ;; current location
    mov rdi, [rbp-8]            ; asm*
    call asm_current_loc

    mov rdi, rax
    call sexp_alloc_int
    mov rdi, rax
    mov rsi, 8                  ; not-shrinkable
    call tnode_alloc_uint_node

    jmp .break

.arg_label_doller_doller:
    ;; current segment-base
    mov rdi, [rbp-8]            ; asm*
    call asm_segment_base

    mov rdi, rax
    call sexp_alloc_int
    mov rdi, rax
    mov rsi, 8                  ; not-shrinkable
    call tnode_alloc_uint_node

    jmp .break

.arg_addressing:
    mov rdi, [rbp-16]           ; sexp*(tnode)
    call tnode_value
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_eval_expr_addressing
    mov [rbp-88], rax           ; tmp, value

    ;; re-construct
    mov rdi, [rbp-16]           ; sexp*(tnode)
    call tnode_type_value
    mov rdi, rax                ; type
    mov rsi, [rbp-88]           ; tmp, value
    call sexp_alloc_cons

    jmp .break

.invalid_op:
    mov rdi, str_ice_invalid_expr
    call runtime_print_string
    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.break:
    leave
    ret


;;; rdi: asm*
asm_segment_base:
    mov rax, [rdi+24]           ; asm.segment_base
    ret


;;; rdi: asm*
;;; rsi: u64, inst-index
asm_inst_loc:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; loc
    mov [rbp-16], rsi           ; inst-index
    mov [rbp-8], rdi            ; asm*

    mov rdi, 0                  ; NOTE: not used-now
    mov rsi, [rbp-16]           ; inst-index
    call asm_accum_inst_size
    mov [rbp-24], rax           ; loc

    ;; base
    mov rdi, [rbp-8]            ; asm*
    call asm_segment_base
    add [rbp-24], rax           ; loc += base

    ;; return
    mov rax, [rbp-24]

    leave
    ret

;;; rdi: asm*
asm_current_loc:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-8]            ; asm*
    call asm_current_inst_index

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    call asm_inst_loc

    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*, tnode
;;; rdx: u64 byte-size
asm_write_node:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_type_tag

    cmp rax, 3                  ; string
    je .write_sexp

    cmp rax, 4                  ; int
    je .write_sexp

    mov rdi, [rbp-8]            ; asm*
    mov cl, [rdi+40]            ; asm.phase = 0
    cmp cl, 0
    je .write_dummy             ; ignore evaluation when phase == 0

    ;; Failed
    mov rdi, str_debug_set_write_node
    call runtime_print_string
    call runtime_print_newline

    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call sexp_print
    call runtime_print_newline

    mov rdi, str_ice_invalid_node
    call runtime_print_string
    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.write_sexp:
    mov rdi, [rbp-16]           ; (type . value): sexp*
    call tnode_value

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; value
    mov rdx, [rbp-24]           ; byte-size
    call asm_write_sexp

    jmp .break

.write_dummy:
    cmp qword [rbp-24], 0       ; byte-size
    jne .skip_size_adjustment

    mov qword [rbp-24], 1       ; adjust byte-size

.skip_size_adjustment:
    ;; fill space temporary by using dummy-value
    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0                  ; int-value
    mov rdx, [rbp-24]           ; byte-size
    call asm_write_value

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*
;;; rdx: u64 byte-size
asm_write_sexp:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov [rbp-48], rdi           ; asm*
    mov qword [rbp-40], 0       ; total-size
    mov qword [rbp-32], 0       ; counter
    mov qword [rbp-24], 0       ; char*
    mov [rbp-16], rdx           ; byte-size
    mov [rbp-8], rsi            ; sexp*

    mov rax, [rsi]              ; sexp.tag

    cmp rax, 0
    je .write_integer

    cmp rax, 1
    je .write_string

    ;; Failed
    mov rdi, str_ice_invalid_type
    call runtime_print_string

    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.write_integer:
    mov rdi, [rbp-8]
    call sexp_internal_int_value

    mov rdi, [rbp-48]           ; asm*
    mov rsi, rax                ; int-value
    mov rdx, [rbp-16]           ; byte-size
    call asm_write_value
    add [rbp-40], rax           ; total-size

    jmp .break

.write_string:
    mov rdi, [rbp-8]
    call sexp_internal_string_ptr
    mov [rbp-24], rax           ; char*

    mov rdi, [rbp-8]
    call sexp_internal_string_length
    mov  [rbp-32], rax          ; counter

.write_string_loop:
    mov rcx, [rbp-32]           ; counter
    cmp rcx, 0
    je .break

    mov rdi, [rbp-48]           ; asm*
    mov rsi, [rbp-24]           ; char*
    mov rsi, [rsi]              ; *(char*)
    and rsi, 0xff
    mov rdx, [rbp-16]           ; byte-size
    call asm_write_value
    add [rbp-40], rax           ; total-size

    mov rax, [rbp-24]           ; char*
    inc rax
    mov [rbp-24], rax

    mov rcx, [rbp-32]           ; counter
    dec rcx
    mov [rbp-32], rcx           ; counter

    jmp .write_string_loop

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: u64, value
;;; rdx: u64, byte-size
asm_lazy_write_sexp:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov qword [rbp-32], 0       ; cursor
    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; sexp*, tnode
    mov [rbp-8], rdi            ; asm*

    mov rax, [g_asm_buffer_cursor]
    mov [rbp-32], rax

    ;; fill space temporary by using dummy-value
    mov rdi, [rbp-8]            ; asm*
    mov rsi, 0                  ; int-value
    mov rdx, [rbp-24]           ; byte-size
    call asm_write_value

    ;; save lazy information
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; sexp*
    mov rdx, [rbp-32]           ; loc
    mov rcx, [rbp-24]           ; byte-size
    call asm_add_replacements

    leave
    ret


;;; rdi: asm*
;;; rsi: u64, value
;;; rdx: u64, size
asm_write_value:
    cmp rdx, 1
    je .u8

    cmp rdx, 2
    je .u16

    cmp rdx, 4
    je .u32

    cmp rdx, 8
    je .u64

    ;; Failed
    mov rdi, rdx
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, str_ice_unsupported_size
    call runtime_print_string
    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.u8:
    call asm_write_u8
    jmp .break

.u16:
    call asm_write_u16
    jmp .break

.u32:
    call asm_write_u32
    jmp .break

.u64:
    call asm_write_u64

.break:
    ret

;;; rdi: asm*
;;; rsi: u8
asm_write_u8:
    mov rax, [rdi+96]           ; asm.buffer
    mov rcx, [rdi+104]          ; asm.buffer-size
    add rax, rcx

    ;; write
    mov rdx, rsi
    mov byte [rax], dl

    add dword [rdi+44], 1       ; asm.current-inst-size
    add qword [rdi+104], 1      ; asm.buffer-size

    ret

;;; rdi: asm*
;;; rsi: u16
asm_write_u16:
    mov rax, [rdi+96]           ; asm.buffer
    mov rcx, [rdi+104]          ; asm.buffer-size
    add rax, rcx

    ;; write
    mov rdx, rsi
    mov word [rax], dx

    add dword [rdi+44], 2       ; asm.current-inst-size
    add qword [rdi+104], 2      ; asm.buffer-size

    ret

;;; rdi: asm*
;;; rsi: u32
asm_write_u32:
    mov rax, [rdi+96]           ; asm.buffer
    mov rcx, [rdi+104]          ; asm.buffer-size
    add rax, rcx

    ;; write
    mov rdx, rsi
    mov dword [rax], edx

    add dword [rdi+44], 4       ; asm.current-inst-size
    add qword [rdi+104], 4      ; asm.buffer-size

    ret

;;; rdi: asm*
;;; rsi: u64
asm_write_u64:
    mov rax, [rdi+96]           ; asm.buffer
    mov rcx, [rdi+104]          ; asm.buffer-size
    add rax, rcx

    ;; write
    mov rdx, rsi
    mov qword [rax], rdx

    add dword [rdi+44], 8       ; asm.current-inst-size
    add qword [rdi+104], 8      ; asm.buffer-size

    ret


;;; rdi: asm*
;;; rsi: sexp*, symbol
;;; rdx: sexp*, tnode, (type . inst-index)
asm_add_labels:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-24], rdx           ;
    mov [rbp-16], rsi           ;
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-16]           ; symbol
    mov rsi, [rbp-24]           ; tnode
    call sexp_alloc_cons        ; (symbol, (type . value))

    ;; prepend the label. e.g. ((b, (ty0 . 10)), ((a, (ty1 . 0)), nil))
    mov rdi, rax                ; (symbol, (type . value))
    mov rcx, [rbp-8]            ; asm*
    mov rsi, [rcx]              ; asm.labels
    call sexp_alloc_cons        ; ((symbol, (type . value)), asm.labels)

    ;; update labels
    mov rcx, [rbp-8]            ; asm*
    mov [rcx], rax              ; asm.labels <- ((symbol, (type . value)), asm.labels)

    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*, symbol
asm_find_labels:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-32], rsi           ; sexp*, finding-symbol
    mov qword [rbp-24], 0       ; sexp*, current
    mov qword [rbp-16], 0       ; sexp*, labels
    mov [rbp-8], rdi            ; asm*

    mov rax, [rdi]              ; asm.labels
    mov [rbp-16], rax           ;

.loop:
    mov rax, [rbp-16]           ; labels
    cmp rax, 0
    je .break                   ; not-found

    mov rdi, [rbp-16]           ; labels
    call sexp_car
    mov [rbp-24], rax           ; current

    mov rdi, [rbp-24]           ; current
    call sexp_car               ; symbol
    mov rdi, rax
    mov rsi, [rbp-32]           ; finding-symbol
    call sexp_equals

    cmp rax, 1
    je .found

    ;; step
    mov rdi, [rbp-16]           ; labels
    call sexp_cdr
    mov [rbp-16], rax

    jmp .loop

.found:
    mov rdi, [rbp-24]           ; current, (symbol . (type . value))
    call sexp_cdr               ; (type . value) and value must be integer
    mov [rbp-24], rax           ; over-write

    mov rdi, [rbp-24]           ; (type . value)
    call tnode_value            ; value
    mov rdi, rax
    call sexp_internal_int_value ; N

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; inst-index
    call asm_inst_loc

    ;; re-construct
    mov rdi, rax                ; accumurated inst-index
    call sexp_alloc_int
    mov rdi, rax
    mov rsi, 8                  ; not-shrinkable
    call tnode_alloc_uint_node

    jmp .break

.invalid:
    ;; debug
    mov rdi, 40
    call runtime_exit

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*, symbol
;;; rdx: sexp*, tnode, (type . expr)
;;; sexp*, snapshot
asm_add_consts:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-32], rcx           ; sexp*, snapshot
    mov [rbp-24], rdx           ; sexp*(tnode), value
    mov [rbp-16], rsi           ; sexp*, symbol
    mov [rbp-8], rdi            ; asm*

    mov cl, [rdi+40]            ; asm.phase != 0
    cmp cl, 0
    jne .break

    mov rdi, [rbp-24]           ; tnode
    mov rsi, [rbp-32]           ; snapshot
    call sexp_alloc_cons        ; ((type . value) . snapshot)

    mov rdi, [rbp-16]           ; symbol
    mov rsi, rax                ; ((type . value) . snapshot)
    call sexp_alloc_cons        ; (symbol, ((type . value) . snapshot))

    ;; prepend the label. e.g. ((b, ((ty0 . 10) . s)), ((a, ((ty1 . 0) . s)), nil))
    mov rdi, rax                ; (symbol, ((type . value) . snapshot))
    mov rcx, [rbp-8]            ; asm*
    mov rsi, [rcx+48]           ; asm.consts
    call sexp_alloc_cons        ; ((symbol, ((type . value) . snapshot)), asm.consts)

    ;; update labels
    mov rcx, [rbp-8]            ; asm*
    mov [rcx+48], rax           ; asm.consts <- ((symbol, ((type . value) . snapshot)), asm.consts)

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*, symbol
asm_find_consts:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov qword [rbp-56], 0       ; return-value
    mov qword [rbp-48], 0       ; snapshot
    mov [rbp-40], rsi           ; sexp*, finding-symbol
    mov qword [rbp-32], 0       ; sexp*, current
    mov qword [rbp-16], 0       ; sexp*, consts
    mov [rbp-8], rdi            ; asm*

    mov rax, [rdi+48]           ; asm.consts
    mov [rbp-16], rax           ;

.loop:
    mov rax, [rbp-16]           ; consts
    cmp rax, 0
    je .break                   ; not-found

    mov rdi, [rbp-16]           ; consts
    call sexp_car
    mov [rbp-32], rax           ; current

    mov rdi, [rbp-32]           ; current
    call sexp_car               ; symbol
    mov rdi, rax
    mov rsi, [rbp-40]           ; finding-symbol
    call sexp_equals

    cmp rax, 1
    je .found

    ;; step
    mov rdi, [rbp-16]           ; consts
    call sexp_cdr
    mov [rbp-16], rax

    jmp .loop

.found:
    mov rdi, [rbp-32]           ; current, (symbol . ((type . value) , snapshot))
    call sexp_cdr
    mov [rbp-32], rax           ; ((type . value) , snapshot)

    mov rdi, [rbp-8]            ; asm*
    call asm_state_snapshot_save
    mov [rbp-48], rax           ; snapshot-before

    mov rdi, [rbp-32]           ; ((type . value) , snapshot)
    call sexp_cdr
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; snapshot
    call asm_state_snapshot_load

    mov rdi, [rbp-32]           ; ((type . value) , snapshot)
    call sexp_car
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; (type . value)
    call asm_eval_expr
    mov [rbp-56], rax           ; return-value

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-48]           ; snapshot-before
    call asm_state_snapshot_load

    mov rax, [rbp-56]           ; return-value

    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*, symbol
asm_find_label_value:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-16], rsi           ; sexp*, symbol
    mov [rbp-8], rdi            ; asm*

    ;; REMOVE
    call runtime_print_newline
    call runtime_print_newline
    mov rdi, [rbp-16]           ; (type . value)
    call sexp_print
    call runtime_print_newline
    call runtime_print_newline
    call runtime_print_newline


    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; symbol
    call asm_find_consts
    cmp rax, 0
    jne .break                  ; found

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; symbol
    call asm_find_labels

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: sexp*, tnode
;;; rdx: u64, loc
;;; rcx: u64, byte-size
asm_add_replacements:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov qword [rbp-48], 0       ;
    mov [rbp-40], rcx           ; byte-size
    mov [rbp-32], rdx           ; loc
    mov [rbp-16], rsi           ; tnode
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-32]           ; loc
    call sexp_alloc_int
    mov [rbp-48], rax

    mov rdi, [rbp-40]           ; byte-size
    call sexp_alloc_int

    mov rdi, [rbp-48]           ; loc-node
    mov rsi, rax                ; byte-size-node
    call sexp_alloc_cons        ; (loc . byte-size)

    mov rdi, [rbp-16]           ; tnode
    mov rsi, rax                ; loc, node
    call sexp_alloc_cons        ; (tnode . (loc . byte-size))

    ;; prepend the replacement
    mov rdi, rax                ; (tnode . (loc . byte-size))
    mov rcx, [rbp-8]            ; asm*
    mov rsi, [rcx+8]            ; asm.replacements
    call sexp_alloc_cons        ; ((tnode . (loc . byte-size)), asm.replacements)

    ;; update labels
    mov rcx, [rbp-8]            ; asm*
    mov [rcx+8], rax            ; asm.replacements <- ((tnode . (loc . byte-size)), asm.replacements)

    leave
    ret


;;; rdi: asm*
asm_fill_replacements:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov qword [rbp-48], 0       ; byte-size
    mov qword [rbp-40], 0       ; loc
    mov qword [rbp-32], 0       ;
    mov qword [rbp-24], 0       ;
    mov qword [rbp-16], 0       ; sexp*, current
    mov [rbp-8], rdi            ; asm*

    mov rax, [rbp-8]            ; asm*
    mov rax, [rax+8]            ; asm.replacements
    mov [rbp-16], rax

.loop:
    mov rdi, [rbp-16]           ; replacements
    cmp rdi, 0
    je .break

    call sexp_car               ; (tnode . (loc . byte-size)
    mov [rbp-24], rax

    mov rdi, [rbp-24]
    call sexp_car
    mov [rbp-32], rax           ; tnode

    mov rdi, [rbp-24]
    call sexp_cdr               ; (loc . byte-size)
    mov [rbp-24], rax

    mov rdi, [rbp-24]
    call sexp_car               ; loc
    mov rdi, rax
    call sexp_internal_int_value
    mov [rbp-40], rax

    mov rdi, [rbp-24]
    call sexp_cdr               ; byte-size
    mov rdi, rax
    call sexp_internal_int_value
    mov [rbp-48], rax

    mov rax, [rbp-40]           ; loc
    mov [g_asm_buffer_cursor], rax

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; tnode
    call asm_eval_expr

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax
    mov rdx, [rbp-48]           ; byte-size
    call asm_write_node

    ;; step
    mov rdi, [rbp-16]           ; replacements
    call sexp_cdr
    mov [rbp-16], rax           ; replacements

    jmp .loop

.break:
    leave
    ret

;;; rdi: asm*
asm_state_snapshot_save:
    mov rdi, [rdi+32]           ; asm.inst_index
    call sexp_alloc_int
    ret

;;; rdi: asm*
;;; rsi: sexp*, state
asm_state_snapshot_load:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov [rbp-16], rsi           ; sexp*, state
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-16]
    call sexp_internal_int_value

    mov rdi, [rbp-8]            ; asm*
    mov [rdi+32], rax           ; asm.inst_index

    leave
    ret

;;; rdi: asm*
asm_step_inst_index:
    inc qword [rdi+32]          ; asm.inst_index
    ret

;;; rdi: asm*
asm_current_inst_index:
    mov rax, [rdi+32]           ; asm.inst_index
    ret

;;; rdi: asm*
;;; rsi: u64, size
asm_set_inst_size:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-16], 0       ; changed
    mov [rbp-8], rdi            ; asm*

    mov rax, g_asm_inst_sizes
    mov rcx, [rdi+32]           ; asm.inst_index
    lea rax, [rax+rcx*4]
    mov ecx, dword [rax]        ; (u32)g_asm_inst_sizes[asm.inst_index]
    cmp rcx, rsi
    je .skip_size_changed

    mov qword [rbp-16], 1       ; changed

.skip_size_changed:
    mov rcx, rsi
    mov dword [rax], ecx        ; g_asm_inst_sizes[asm.inst_index] = (u32)size

    ;; debug
    mov rdi, rsi                ; size
    call runtime_print_uint64
    call runtime_print_newline

    xor rax, rax

    ;;
    mov rdi, [rbp-8]            ; asm*
    mov cl, [rdi+40]            ; asm.phase == 0
    cmp cl, 0
    je .break

    mov rax, [rbp-16]           ; returns 0 if inst-size is not changed

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: offset
asm_accum_inst_size:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov qword [rbp-8], 0        ; size

    ;; accumurale [0, offset)
    mov rcx, 0                  ; counter
.loop:
    cmp rcx, rsi
    je .break

    mov rdx, g_asm_inst_sizes   ; offset
    mov eax, [rdx+rcx*4]        ; (byte)offset[counter] (inst-size)
    add [rbp-8], rax            ; size += inst-size

    inc rcx                     ; counter

    jmp .loop

.break:
    mov rax, [rbp-8]            ; size

    leave
    ret


;;; rdi: asm*
;;; rsi: sexp*(tnode)
asm_exit_with_error_inst_arg_1:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-16], rsi
    mov [rbp-8], rdi

    mov rdi, str_error_unsupported_op_arg1
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [rbp-16]
    call sexp_print
    call runtime_print_newline

    mov rdi, 10                 ; debug
    call runtime_exit

;;; rdi: size
asm_exit_with_error_undecidable_op_size:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-8], rdi            ; size

    mov rdi, str_error_unsupported_oprands_size
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [rbp-8]            ; size
    shr rdi, 8
    and rdi, 0xff
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-8]            ; size
    and rdi, 0xff
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, [rbp-8]            ; size
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, 1                  ; debug
    call runtime_exit

;;;
;;; struct sexp {
;;;   u64       tag; (0: int, 1: string, 2: cons)
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
    mov rdi, str_ice_nomore_sexps
    call runtime_print_string
    call runtime_print_newline

    mov rdi, [g_sexp_objects_count]
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, 3
    call runtime_exit

;;; rdi: int
sexp_alloc_int:
    call sexp_alloc
    mov rcx, rax

    mov qword [rcx], 0          ; tag
    mov [rcx+8], rdi

    ret


;;; rdi: *char
sexp_alloc_string:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi

    call runtime_strlen

    mov rdi, [rbp-8]
    mov rsi, rax
    call sexp_alloc_string_view

    leave
    ret


;;; rdi: *char
;;; rsi: u64
sexp_alloc_string_view:
    call sexp_alloc
    mov rcx, rax

    mov qword [rcx], 1          ; tag
    mov [rcx+8], rdi
    mov [rcx+16], rsi

    ret


;;; rdi: *value car
;;; rsi: *value cdr
sexp_alloc_cons:
    call sexp_alloc
    mov rcx, rax

    mov qword [rcx], 2          ; tag
    mov [rcx+8], rdi
    mov [rcx+16], rsi

    ret

;;; rdi: *value
sexp_internal_int_value:
    mov rax, [rdi+8]
    ret

;;; rdi: *value
sexp_internal_string_ptr:
    mov rax, [rdi+8]
    ret

;;; rdi: *value
sexp_internal_string_length:
    mov rax, [rdi+16]
    ret

;;; rdi: *value
sexp_value_string_as_hash:
    push rbp
    mov rbp, rsp

    mov rax, rdi
    mov rdi, [rax+8]
    mov rsi, [rax+16]
    call runtime_string_view_hash

    leave
    ret

;;; rdi: *value cons
sexp_car:
    mov rax, [rdi+8]
    ret

;;; rdi: *value cons
sexp_cdr:
    mov rax, [rdi+16]
    ret


;;; rdi: *value cons
;;; rsi: *value cdr
sexp_update_cdr:
    mov [rdi+16], rsi

    ret


;;; rdi
;;; rsi
sexp_equals:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov qword [rbp-24], 0       ; tmp
    mov [rbp-16], rsi           ; value*, A
    mov [rbp-8], rdi            ; value*, B

    mov rax, [rbp-16]           ; A
    mov rax, [rax]              ; A.type

    mov rcx, [rbp-8]            ; B
    mov rcx, [rcx]              ; B.type

    cmp rax, rcx
    jne .bad

    cmp rax, 0                  ; int
    je .cmp_int

    cmp rax, 1                  ; string
    je .cmp_string

    ;; failed...
    mov rdi, 162
    call runtime_exit

.cmp_int:
    mov rdi, [rbp-16]           ; A
    call sexp_internal_int_value
    mov [rbp-24], rax           ; tmp

    mov rdi, [rbp-8]            ; B
    call sexp_internal_int_value

    mov rcx, [rbp-24]           ; tmp
    cmp rax, rcx
    jne .bad

    mov rax, 1
    jmp .break

.cmp_string:
    mov rdi, [rbp-16]           ; A
    call sexp_internal_string_ptr
    mov [rbp-24], rax           ; tmp

    mov rdi, [rbp-16]           ; A
    call sexp_internal_string_length
    mov rsi, rax

    mov rdi, [rbp-8]            ; B
    call sexp_internal_string_ptr
    mov rdx, rax                ; tmp

    mov rdi, [rbp-8]            ; B
    call sexp_internal_string_length
    mov rcx, rax

    mov rdi, [rbp-24]
    call runtime_strcmp

    jmp .break

.bad:
    xor rax, rax
    jmp .break

.break:
    leave
    ret


;;; rdi: sexp*, lhs
;;; rsi: char*, rhs
sexp_internal_string_cmp:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-32], rsi           ; rhs

    mov qword [rbp-24], 0       ; lhs, length
    mov qword [rbp-16], 0       ; lhs, char*
    mov [rbp-8], rdi            ; lhs

    mov rdi, [rbp-8]            ; lhs
    call sexp_internal_string_ptr
    mov [rbp-16], rax           ; lhs, char*

    mov rdi, [rbp-8]            ; lhs
    call sexp_internal_string_length
    mov [rbp-24], rax           ; lhs, length

    mov rdi, [rbp-32]           ; rhs
    call runtime_strlen

    mov rdi, [rbp-32]           ; rhs
    mov rsi, rax                ; rhs.length
    mov rdx, [rbp-16]           ; lhs, char*
    mov rcx, [rbp-24]           ; lhs, length
    call runtime_strcmp

    leave
    ret

;;; rdi*, value
;;; rsi,
sexp_list_map:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; *value

;;;
sexp_print:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi            ; *value

    mov rdi, str_paren_l
    call runtime_print_string

    mov rax, [rbp-8]
    cmp rax, 0
    je .print_nil

    mov rax, [rax]
    cmp rax, 0
    je .print_int

    cmp rax, 1
    je .print_string

    cmp rax, 2
    je .print_cons

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
    mov rax, [rbp-8]
    mov rdi, [rax+8]
    call runtime_print_uint64

    mov rdi, str_colon
    call runtime_print_string

    mov rdi, text_sexp_int
    call runtime_print_string

    jmp .finalize

.print_string:
    mov rax, [rbp-8]
    mov rdi, [rax+8]
    mov rsi, [rax+16]
    call runtime_print_string_view

    mov rdi, str_colon
    call runtime_print_string

    mov rdi, text_sexp_string
    call runtime_print_string

    mov rax, [rbp-8]
    mov rdi, [rax+16]
    call runtime_print_uint64

    jmp .finalize

.print_cons:
    mov rax, [rbp-8]
    mov rdi, [rax+8]            ; car
    call sexp_print

    mov rdi, str_comma
    call runtime_print_string

    mov rax, [rbp-8]
    mov rdi, [rax+16]            ; cdr
    call sexp_print

    jmp .finalize

.finalize:
    mov rdi, str_paren_r
    call runtime_print_string

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


;;; rdi: char*
;;; rsi: u64
;;; rdx: char*
;;; rcx: u64
runtime_strcmp:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-24], rdx
    mov [rbp-16], rdi
    mov [rbp-8], rcx            ; length

    cmp rsi, rcx
    jne .bad

    mov rcx, 0

.loop:
    mov rax, [rbp-8]
    cmp rax, rcx
    je .matched

    mov rdx, [rbp-24]
    add rdx, rcx
    mov dl, [rdx]

    mov rax, [rbp-16]
    add rax, rcx
    mov al, [rax]

    cmp dl, al
    jne .bad

    inc rcx
    jmp .loop

.matched:
    mov rax, 1
    jmp .break

.bad:
    xor rax, rax

.break:
    leave
    ret


;;; rdi: char*, dst
;;; rsi: u64  , dst
;;; rdx: char*, src
;;; rcx: u64  , src
runtime_strcpy:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-32], rcx           ; src-len
    mov [rbp-24], rdx           ; char*, src
    mov [rbp-16], rsi           ; dst-len
    mov [rbp-8], rdi            ; char*, dst

    mov rcx, 0

.loop:
    cmp rcx, [rbp-32]           ; i >= src-len
    jge .break
    cmp rcx, [rbp-16]           ; i >= dst-len
    jge .break

    mov rdx, [rbp-24]           ; src[i]*
    mov dl, [rdx]               ; src[i]
    mov rax, [rbp-8]            ; dst[i]*
    mov [rax], dl               ; dst[i] = src[i]

    inc qword [rbp-24]
    inc qword [rbp-8]

    inc rcx
    jmp .loop

.matched:
    mov rax, 1
    jmp .break

.bad:
    xor rax, rax

.break:
    leave
    ret


;;; rdi: char*
runtime_string_hash:
    push rbp
    mov rbp, rsp
    sub rsp, 8

    mov [rbp-8], rdi

    call runtime_strlen

    mov rdi, [rbp-8]
    mov rsi, rax
    call runtime_string_view_hash

    leave
    ret

;;; rdi: char*
;;; rsi: u64 length
runtime_string_view_hash:
    push rbp
    mov rbp, rsp

    cmp rsi, 8
    jg .failed

    mov rax, 0
    mov rcx, 0

.loop:
    cmp rcx, rsi
    je .break

    mov r10, rdi
    add r10, rcx
    xor rdx, rdx
    mov dl, [r10]

    ;; TODO: fix
    add rax, rdx

    inc rcx
    jmp .loop

.failed:
    mov rdi, 2
    call runtime_exit
    jmp .break

.break:
    leave
    ret


;;;
runtime_exit:
    mov rax, 60
    syscall
    ret


;;; rdi: filename
;;; rsi: flags
;;; rdx: mode
runtime_open:
    mov rax, 2                  ; open
    syscall
    ret


runtime_write:
    mov rax, 1                  ; write
    syscall
    ret


;;; rdi: addr
;;; rsi: length
;;; rdx: prot
;;; rcx: flags
runtime_mmap:
    mov r10, rcx                ; flags
;   mov r8, r8                  ; fd
    mov r9, 0                   ; offset = 0
    mov rax, 9                  ; mmap
    syscall
    ret


;;; rdi: u64, size
runtime_malloc:
    mov rax, [g_heap_size]
    mov rcx, app_max_heap
    sub rcx, rdi
    cmp rax, rcx       ; heap_size > max - size
    jg .no_memory

    mov rax, g_heap
    add rax, [g_heap_size]      ; g_heap + offset

    add qword [g_heap_size], rdi

    jmp .break

.no_memory:
    mov rdi, 60
    call runtime_exit
    jmp .break

.break:
    ret


section_text_end:
    ;; --< text

code_segment_rest_size: equ $-code_segment_begin
    align segment_align
code_segment_end:

    ;; Data
data_segment_begin:

    ;; --> bss
section_bss_begin:

ret_code:   dq 42

g_sexp_symbol_doller:   dq 0
g_sexp_symbol_doller_doller:   dq 0

g_code_buffer:  resb app_max_code_buffer_size
g_code_size:    dq 0

g_asm_buffer_size:  dq 0
g_asm_buffer_cursor:dq 0

g_asm_inst_sizes:   resd 0x100000

g_sexp_objects:  resb app_max_sexp_objects_size
g_sexp_objects_count:  dq 0

g_heap: resb app_max_heap
g_heap_size:     dq 0

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

str_dev_zero:   db "/dev/zero", 0

str_paren_l:    db "(", 0
str_paren_r:    db ")", 0
str_colon:      db ":", 0
str_comma:      db ",", 0
str_dot:        db ".", 0

str_g_symbol_doller:    db "$", 0
str_g_symbol_doller_doller: db "$$", 0

str_reg_al:    db "al", 0
str_reg_cl:    db "cl", 0
str_reg_dl:    db "dl", 0
str_reg_bl:    db "bl", 0
str_reg_ah:    db "ah", 0
str_reg_ch:    db "ch", 0
str_reg_dh:    db "dh", 0
str_reg_bh:    db "bh", 0

str_reg_ax:    db "ax", 0
str_reg_cx:    db "cx", 0
str_reg_dx:    db "dx", 0
str_reg_bx:    db "bx", 0
str_reg_sp:    db "sp", 0
str_reg_bp:    db "bp", 0
str_reg_si:    db "si", 0
str_reg_di:    db "di", 0

str_reg_eax:    db "eax", 0
str_reg_ecx:    db "ecx", 0
str_reg_edx:    db "edx", 0
str_reg_ebx:    db "ebx", 0
str_reg_esp:    db "esp", 0
str_reg_ebp:    db "ebp", 0
str_reg_esi:    db "esi", 0
str_reg_edi:    db "edi", 0

str_reg_rax:    db "rax", 0
str_reg_rcx:    db "rcx", 0
str_reg_rdx:    db "rdx", 0
str_reg_rbx:    db "rbx", 0
str_reg_rsp:    db "rsp", 0
str_reg_rbp:    db "rbp", 0
str_reg_rsi:    db "rsi", 0
str_reg_rdi:    db "rdi", 0

str_reg_r8:     db "r8", 0
str_reg_r9:     db "r9", 0
str_reg_r10:    db "r10", 0
str_reg_r11:    db "r11", 0
str_reg_r12:    db "r12", 0
str_reg_r13:    db "r13", 0
str_reg_r14:    db "r14", 0
str_reg_r15:    db "r15", 0

g_register_table:
    dq str_reg_al
    dq 0                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_cl
    dq 1                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_dl
    dq 2                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_bl
    dq 3                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_ah
    dq 4                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_ch
    dq 5                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_dh
    dq 6                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_bh
    dq 7                        ; register-index
    dq 1                        ; size
    dq 0                        ; extended

    dq str_reg_ax
    dq 0                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_cx
    dq 1                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_dx
    dq 2                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_bx
    dq 3                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_sp
    dq 4                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_bp
    dq 5                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_si
    dq 6                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_di
    dq 7                        ; register-index
    dq 2                        ; size
    dq 0                        ; extended

    dq str_reg_eax
    dq 0                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_ecx
    dq 1                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_edx
    dq 2                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_ebx
    dq 3                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_esp
    dq 4                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_ebp
    dq 5                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_esi
    dq 6                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_edi
    dq 7                        ; register-index
    dq 4                        ; size
    dq 0                        ; extended

    dq str_reg_rax
    dq 0                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_rcx
    dq 1                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_rdx
    dq 2                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_rbx
    dq 3                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_rsp
    dq 4                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_rbp
    dq 5                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_rsi
    dq 6                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_rdi
    dq 7                        ; register-index
    dq 8                        ; size
    dq 0                        ; extended

    dq str_reg_r8
    dq 0                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq str_reg_r9
    dq 1                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq str_reg_r10
    dq 2                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq str_reg_r11
    dq 3                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq str_reg_r12
    dq 4                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq str_reg_r13
    dq 5                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq str_reg_r14
    dq 6                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq str_reg_r15
    dq 7                        ; register-index
    dq 8                        ; size
    dq 1                        ; extended

    dq 0                        ; null-termination

str_size_byte:  db "byte", 0
str_size_word:  db "word", 0
str_size_dword: db "dword", 0
str_size_qword: db "qword", 0

str_ice_invalid_statement:  db "ICE: Invalid statement", 0
str_ice_invalid_inst:       db "ICE: Invalid inst", 0
str_ice_invalid_type:       db "ICE: Invalid type", 0
str_ice_invalid_node:       db "ICE: Invalid node", 0
str_ice_invalid_expr:       db "ICE: Invalid op", 0
str_ice_unsupported_size:   db "ICE: Unsupported size", 0
str_ice_nomore_sexps:       db "ICE: Cannot allocate sexp nodes", 0

str_error_unsupported_inst_form:    db "ERROR: Unsupported inst form", 0
str_error_unsupported_op_arg1:  db "ERROR: ", 0
str_error_unsupported_oprands_size: db "ERROR: Unsupported operands size", 0

str_debug_set_imm:  db "imm", 0
str_debug_set_rel:  db "rel", 0
str_debug_set_rel_operand:  db "rel_op", 0
str_debug_set_write_node:  db "w_node", 0

str_inst_bits:  db "bits", 0
str_inst_org:   db "org", 0
str_inst_equ:   db "equ", 0
str_inst_align: db "align", 0
str_inst_resb:  db "resb", 0
str_inst_resd:  db "resd", 0
str_inst_db:    db "db", 0
str_inst_dw:    db "dw", 0
str_inst_dd:    db "dd", 0
str_inst_dq:    db "dq", 0

str_inst_mov:   db "mov", 0
str_inst_xor:   db "xor", 0
str_inst_push:  db "push", 0
str_inst_pop:   db "pop", 0
str_inst_cmp:   db "cmp", 0
str_inst_add:   db "add", 0
str_inst_sub:   db "sub", 0
str_inst_div:   db "div", 0
str_inst_inc:   db "inc", 0
str_inst_dec:   db "dec", 0
str_inst_ja:    db "ja", 0
str_inst_je:    db "je", 0
str_inst_jg:    db "jg", 0
str_inst_jge:   db "jge", 0
str_inst_jl:    db "jl", 0
str_inst_jle:   db "jle", 0
str_inst_jne:   db "jne", 0
str_inst_jmp:   db "jmp", 0
str_inst_call:  db "call", 0
str_inst_syscall:   db "syscall", 0
str_inst_leave: db "leave", 0
str_inst_ret:   db "ret", 0

str_debug_arrow_r:  db "->", 0
str_debug_finished: db "[DEBUG] finished", 0
str_debug_failed_to_parse_addr: db "[DEBUG] Failed to parse addr", 0
str_debug_compat: db "[DEBUG] compat", 0
str_debug_parse_addressing: db "[DEBUG] parse.addressing", 0

g_asm_inst_table:
    dq str_inst_bits
    dq asm_interp_inst_bits
    dq 0

    dq str_inst_org
    dq asm_interp_inst_org
    dq 0

    dq str_inst_equ
    dq asm_interp_inst_equ
    dq 0

    dq str_inst_align
    dq asm_interp_inst_align
    dq 0

    dq str_inst_db
    dq asm_interp_inst_dump
    dq 1                        ; size

    dq str_inst_dw
    dq asm_interp_inst_dump
    dq 2                        ; size

    dq str_inst_dd
    dq asm_interp_inst_dump
    dq 4                        ; size

    dq str_inst_dq
    dq asm_interp_inst_dump
    dq 8                        ; size

    dq str_inst_db
    dq asm_interp_inst_dump
    dq 1                        ; size

    dq str_inst_dw
    dq asm_interp_inst_dump
    dq 2                        ; size

    dq str_inst_dd
    dq asm_interp_inst_dump
    dq 4                        ; size

    dq str_inst_resb
    dq asm_interp_inst_res
    dq 1

    dq str_inst_resd
    dq asm_interp_inst_res
    dq 4

    dq str_inst_mov
    dq asm_write_inst_mov
    dq 0

    dq str_inst_xor
    dq asm_write_inst_xor
    dq 0

    dq str_inst_push
    dq asm_write_inst_push
    dq 0

    dq str_inst_pop
    dq asm_write_inst_pop
    dq 0

    dq str_inst_cmp
    dq asm_write_inst_cmp
    dq 0

    dq str_inst_add
    dq asm_write_inst_add
    dq 0

    dq str_inst_sub
    dq asm_write_inst_sub
    dq 0

    dq str_inst_div
    dq asm_write_inst_div
    dq 0

    dq str_inst_inc
    dq asm_write_inst_inc
    dq 0

    dq str_inst_dec
    dq asm_write_inst_dec
    dq 0

    dq str_inst_ja
    dq asm_write_inst_jcc
    dq 0                        ; ja

    dq str_inst_je
    dq asm_write_inst_jcc
    dq 8                        ; je

    dq str_inst_jg
    dq asm_write_inst_jcc
    dq 9                        ; jg

    dq str_inst_jge
    dq asm_write_inst_jcc
    dq 10                       ; jge

    dq str_inst_jl
    dq asm_write_inst_jcc
    dq 11                       ; jl

    dq str_inst_jle
    dq asm_write_inst_jcc
    dq 12                       ; jle

    dq str_inst_jne
    dq asm_write_inst_jcc
    dq 18                       ; jne

    dq str_inst_jmp
    dq asm_write_inst_jmp
    dq 0

    dq str_inst_call
    dq asm_write_inst_call
    dq 0

    dq str_inst_syscall
    dq asm_write_inst_syscall
    dq 0

    dq str_inst_leave
    dq asm_write_inst_leave
    dq 0

    dq str_inst_ret
    dq asm_write_inst_ret
    dq 0

    dq 0                        ; termination

;;; OPCODE-patterns
const_asm_inst_type_rm: equ 0
const_asm_inst_type_mr: equ 1
const_asm_inst_type_i:  equ 2
const_asm_inst_type_mi: equ 3
const_asm_inst_type_oi: equ 4
const_asm_inst_type_m:  equ 5
const_asm_inst_type_o:  equ 6

const_inst_enc_0_i: equ 2
const_inst_enc_r_m: equ 0
const_inst_enc_m_r: equ 1
const_inst_enc_m_i: equ 3
const_inst_enc_m_0: equ 5
const_inst_enc_o_0: equ 6
const_inst_enc_d_0: equ 8

const_inst_rex:     equ 0x40    ; REX
const_inst_rex_w:   equ 0x48    ; REX.W
const_inst_rex_r:   equ 0x44    ; REX.R
const_inst_rex_b:   equ 0x41    ; REX.B


;;;
;;; MOV
;;;
g_asm_inst_template_mov:
    dq 1                        ; optimizable
    ;; MR
    dq .rm8_r8
    dq .rm64_r64
    ;; RM
    dq .r8_rm8
    dq .r32_rm32
    dq .r64_rm64
    ;; OI
    dq .r32_imm32
    dq .r64_imm64
    ;; MI
    dq .rm8_imm8
    dq .rm64_imm32
    dq 0

.rm8_r8:
    dd 2
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x01, 0x01, 0xff, 0x00   ; r8
    db 0x00
    db 0x01                     ; op-length
    db 0x88
    db const_asm_inst_type_mr

.rm64_r64:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x01, 0x08, 0xff, 0x00   ; r64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x89
    db const_asm_inst_type_mr

.r8_rm8:
    dd 2
    db 0x01, 0x01, 0xff, 0x00   ; r8
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x00
    db 0x01                     ; op-length
    db 0x8a
    db const_asm_inst_type_rm

.r32_rm32:
    dd 2
    db 0x01, 0x04, 0xff, 0x00   ; r32
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x00
    db 0x01                     ; op-length
    db 0x8b
    db const_asm_inst_type_rm

.r64_rm64:
    dd 2
    db 0x01, 0x08, 0xff, 0x00   ; r64
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x8b
    db const_asm_inst_type_rm

.r32_imm32:
    dd 2
    db 0x11, 0x04, 0xff, 0x00   ; r32, can convert
    db 0x14, 0x04, 0xff, 0x00   ; imm32
    db 0x00
    db 0x01                     ; op-length
    db 0xb8                     ;
    db const_asm_inst_type_oi

.r64_imm64:
    dd 2
    db 0x01, 0x08, 0xff, 0x00   ; r64
    db 0x14, 0x08, 0xff, 0x00   ; imm64
    db 0x00
    db 0x01                     ; op-length
    db 0xb8                     ;
    db const_asm_inst_type_oi

.rm8_imm8:
    dd 2
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0xc6
    db const_inst_enc_m_i, 0

.rm64_imm32:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x04, 0x04, 0xff, 0x00   ; imm32
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0xc7
    db const_asm_inst_type_mi, 0


;;;
;;; XOR
;;;
g_asm_inst_template_xor:
    dq 1                        ; optimizable
    ;; MR
    dq .rm64_r64
    dq 0

.rm64_r64:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x01, 0x08, 0xff, 0x00   ; r64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x31
    db const_asm_inst_type_mr


;;;
;;; PUSH
;;;
g_asm_inst_template_push:
    dq 0                        ; normal
    ;; M
    dq g_asm_inst_push_rm64
    ;; O
    dq g_asm_inst_push_r64
    dq 0

g_asm_inst_push_rm64:
    dd 1
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x00
    db 0x01                     ; op-length
    db 0xff
    db const_asm_inst_type_m, 6

g_asm_inst_push_r64:
    dd 1
    db 0x01, 0x08, 0xff, 0x00   ; r64
    db 0x00
    db 0x01                     ; op-length
    db 0x50
    db const_asm_inst_type_o

;;;
;;; CMP
;;;
g_asm_inst_template_cmp:
    dq 0                        ; normal
    ;; I
    dq .al_imm8
    dq .ax_imm16
    dq .eax_imm32
    dq .rax_imm32
    ;; MI
    dq .rm8_imm8
    dq .rm64_imm32
    dq .rm64_imm8
    ;; MR
    dq .rm8_r8
    dq .rm64_r64
    ;; RM
    dq .r8_rm8
    dq .r64_rm64
    dq 0

.al_imm8:
    dd 2
    db 0x01, 0x01, 0x00, 0x00   ; al
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x3c
    db const_asm_inst_type_i

.ax_imm16:
    dd 2
    db 0x01, 0x02, 0x00, 0x00   ; ax
    db 0x04, 0x02, 0xff, 0x00   ; imm16
    db 0x00
    db 0x01                     ; op-length
    db 0x3d
    db const_asm_inst_type_i

.eax_imm32:
    dd 2
    db 0x01, 0x04, 0x00, 0x00   ; eax
    db 0x04, 0x04, 0xff, 0x00   ; imm32
    db 0x00
    db 0x01                     ; op-length
    db 0x3d
    db const_asm_inst_type_i

.rax_imm32:
    dd 2
    db 0x01, 0x08, 0x00, 0x00   ; rax
    db 0x04, 0x04, 0xff, 0x00   ; imm32
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x3d
    db const_asm_inst_type_i

.rm8_imm8:
    dd 2
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x80
    db const_asm_inst_type_mi, 7

.rm64_imm32:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x04, 0x04, 0xff, 0x00   ; imm32
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x81
    db const_asm_inst_type_mi, 7

.rm64_imm8:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x83
    db const_asm_inst_type_mi, 7

.rm8_r8:                        ; MR
    dd 2
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x01, 0x01, 0xff, 0x00   ; r8
    db 0x00
    db 0x01                     ; op-length
    db 0x38
    db const_inst_enc_m_r

.rm64_r64:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x01, 0x08, 0xff, 0x00   ; r64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x39
    db const_asm_inst_type_mr

.r8_rm8:                        ; MR
    dd 2
    db 0x01, 0x01, 0xff, 0x00   ; r8
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db const_inst_rex
    db 0x01                     ; op-length
    db 0x3a
    db const_inst_enc_r_m

.r64_rm64:                      ; MR
    dd 2
    db 0x01, 0x08, 0xff, 0x00   ; r64
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x3b
    db const_inst_enc_r_m

;;;
;;; ADD
;;;
g_asm_inst_template_add:
    dq 0                        ; normal
    ;; I
    dq g_asm_inst_add_al_imm8
    dq g_asm_inst_add_ax_imm16
    ;; MI
    dq g_asm_inst_add_rm16_imm8
    dq g_asm_inst_add_rm32_imm8
    dq g_asm_inst_add_rm64_imm8
    ;; MR
    dq g_asm_inst_add_rm64_r64
    ;; RM
    dq g_asm_inst_add_r32_rm32
    dq g_asm_inst_add_r64_rm64
    dq 0

g_asm_inst_add_al_imm8:
    dd 2
    db 0x01, 0x01, 0x00, 0x00   ; al
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x04                     ;
    db const_inst_enc_0_i

g_asm_inst_add_ax_imm16:
    dd 2
    db 0x01, 0x02, 0x00, 0x00   ; ax
    db 0x04, 0x02, 0xff, 0x00   ; imm16
    db 0x00
    db 0x01                     ; op-length
    db 0x05                     ;
    db const_inst_enc_0_i

g_asm_inst_add_rm16_imm8:
    dd 2
    db 0x05, 0x02, 0xff, 0x00   ; r/m16
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x83                     ;
    db const_inst_enc_m_i, 0    ; /r = 0

g_asm_inst_add_rm32_imm8:
    dd 2
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x83
    db const_inst_enc_m_i, 0    ; /r = 0

g_asm_inst_add_rm64_imm8:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x83
    db const_inst_enc_m_i, 0    ; /r = 0

g_asm_inst_add_rm64_r64:
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x01, 0x08, 0xff, 0x00   ; reg64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x01
    db const_inst_enc_m_r

g_asm_inst_add_r32_rm32:
    dd 2
    db 0x01, 0x04, 0xff, 0x00   ; reg32
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x00                     ; REX
    db 0x01                     ; op-length
    db 0x03                     ;
    db const_inst_enc_r_m

g_asm_inst_add_r64_rm64:
    dd 2
    db 0x01, 0x08, 0xff, 0x00   ; reg64
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x03                     ;
    db const_inst_enc_r_m


;;;
;;; SUB
;;;
g_asm_inst_template_sub:
    dq 0                        ; normal
    ;; I
    dq .al_imm8
    dq .ax_imm16
    ;; MI
    dq .rm8_imm8
    dq .rm16_imm16
    dq .rm32_imm32
    dq .rm64_imm32
    dq .rm16_imm8
    dq .rm32_imm8
    dq .rm64_imm8
    ;; MR
    dq .rm64_r64
    ;; RM
    dq .r32_rm32
    dq .r64_rm64
    dq 0

.al_imm8:                       ; I
    dd 2
    db 0x01, 0x01, 0x00, 0x00   ; al
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x2c
    db const_inst_enc_0_i

.ax_imm16:                      ; I
    dd 2
    db 0x01, 0x02, 0x00, 0x00   ; ax
    db 0x04, 0x02, 0xff, 0x00   ; imm16
    db 0x00
    db 0x01                     ; op-length
    db 0x2d
    db const_inst_enc_0_i

.rm8_imm8:                      ; MI
    dd 2
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x80                     ;
    db const_inst_enc_m_i, 5    ; /r

.rm16_imm16:                    ; MI
    dd 2
    db 0x05, 0x02, 0xff, 0x00   ; r/m16
    db 0x04, 0x02, 0xff, 0x00   ; imm16
    db 0x00
    db 0x01                     ; op-length
    db 0x81                     ;
    db const_inst_enc_m_i, 5    ; /r

.rm32_imm32:                     ; MI
    dd 2
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x04, 0x04, 0xff, 0x00   ; imm32
    db 0x00
    db 0x01                     ; op-length
    db 0x81                     ;
    db const_inst_enc_m_i, 5    ; /r

.rm64_imm32:                    ; MI
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x04, 0x04, 0xff, 0x00   ; imm32
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x81                     ;
    db const_inst_enc_m_i, 5    ; /r

.rm16_imm8:                     ; MI
    dd 2
    db 0x05, 0x02, 0xff, 0x00   ; r/m16
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x83                     ;
    db const_inst_enc_m_i, 5    ; /r

.rm32_imm8:                     ; MI
    dd 2
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db 0x00
    db 0x01                     ; op-length
    db 0x83
    db const_inst_enc_m_i, 5    ; /r

.rm64_imm8:                     ; MI
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x04, 0x01, 0xff, 0x00   ; imm8
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x83
    db const_inst_enc_m_i, 5    ; /r

.rm64_r64:                      ; MR
    dd 2
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db 0x01, 0x08, 0xff, 0x00   ; reg64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x29
    db const_inst_enc_m_r

.r32_rm32:                      ; RM
    dd 2
    db 0x01, 0x04, 0xff, 0x00   ; reg32
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x00
    db 0x01                     ; op-length
    db 0x2b
    db const_inst_enc_r_m

.r64_rm64:                      ; RM
    dd 2
    db 0x01, 0x08, 0xff, 0x00   ; reg64
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0x2b
    db const_inst_enc_r_m


;;;
;;; DIV
;;;
g_asm_inst_template_div:
    dq 0                        ; normal
    ;; M
    dq .rm8
    dq .rm16
    dq .rm32
    dq .rm64
    dq 0

.rm8:                           ; M
    dd 1
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x00
    db 0x01                     ; op-length
    db 0xf6
    db const_inst_enc_m_0, 6    ; /r

.rm16:                          ; M
    dd 1
    db 0x05, 0x02, 0xff, 0x00   ; r/m16
    db 0x00
    db 0x01                     ; op-length
    db 0xf7
    db const_inst_enc_m_0, 6    ; /r

.rm32:                          ; M
    dd 1
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x00
    db 0x01                     ; op-length
    db 0xf7
    db const_inst_enc_m_0, 6    ; /r

.rm64:                          ; M
    dd 1
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0xf7
    db const_inst_enc_m_0, 6    ; /r


;;;
;;; INC
;;;
g_asm_inst_template_inc:
    dq 0                        ; normal
    ;; M
    dq .rm8
    dq .rm16
    dq .rm32
    dq .rm64
    ;; O
    dq .r16
    dq .r32
    dq 0

.rm8:                           ; M
    dd 1
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x00
    db 0x01                     ; op-length
    db 0xfe
    db const_inst_enc_m_0, 0    ; /r

.rm16:                          ; M
    dd 1
    db 0x05, 0x02, 0xff, 0x00   ; r/m16
    db 0x00
    db 0x01                     ; op-length
    db 0xff
    db const_inst_enc_m_0, 0    ; /r

.rm32:                          ; M
    dd 1
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x00
    db 0x01                     ; op-length
    db 0xff
    db const_inst_enc_m_0, 0    ; /r

.rm64:                          ; M
    dd 1
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0xff
    db const_inst_enc_m_0, 0    ; /r

.r16:                           ; O
    dd 1
    db 0x01, 0x02, 0xff, 0x00   ; reg16
    db 0x00
    db 0x01                     ; op-length
    db 0x40
    db const_inst_enc_o_0

.r32:                           ; O
    dd 1
    db 0x01, 0x04, 0xff, 0x00   ; reg16
    db 0x00
    db 0x01                     ; op-length
    db 0x40
    db const_inst_enc_o_0


;;;
;;; DEC
;;;
g_asm_inst_template_dec:
    dq 0                        ; normal
    ;; M
    dq .rm8
    dq .rm16
    dq .rm32
    dq .rm64
    ;; O
    dq .r16
    dq .r32
    dq 0

.rm8:                           ; M
    dd 1
    db 0x05, 0x01, 0xff, 0x00   ; r/m8
    db 0x00
    db 0x01                     ; op-length
    db 0xfe
    db const_inst_enc_m_0, 1    ; /r

.rm16:                          ; M
    dd 1
    db 0x05, 0x02, 0xff, 0x00   ; r/m16
    db 0x00
    db 0x01                     ; op-length
    db 0xff
    db const_inst_enc_m_0, 1    ; /r

.rm32:                          ; M
    dd 1
    db 0x05, 0x04, 0xff, 0x00   ; r/m32
    db 0x00
    db 0x01                     ; op-length
    db 0xff
    db const_inst_enc_m_0, 1    ; /r

.rm64:                          ; M
    dd 1
    db 0x05, 0x08, 0xff, 0x00   ; r/m64
    db const_inst_rex_w
    db 0x01                     ; op-length
    db 0xff
    db const_inst_enc_m_0, 1    ; /r

.r16:                           ; O
    dd 1
    db 0x01, 0x02, 0xff, 0x00   ; reg16
    db 0x00
    db 0x01                     ; op-length
    db 0x48
    db const_inst_enc_o_0

.r32:                           ; O
    dd 1
    db 0x01, 0x04, 0xff, 0x00   ; reg16
    db 0x00
    db 0x01                     ; op-length
    db 0x48
    db const_inst_enc_o_0

;;;
;;; JMP
;;;
g_asm_inst_template_jmp:
    dq 0                        ; normal
    ;; D
    dq .rel8
    dq .rel32
    dq 0

.rel8:                          ; D
    dd 1
    db 0x0f, 0x01, 0xff, 0x02   ; rel8, op(1)
    db 0x00
    db 0x01                     ; op-length
    db 0xeb
    db const_inst_enc_d_0

.rel32:                         ; D
    dd 1
    db 0x0f, 0x04, 0xff, 0x05   ; rel32, op(1)
    db 0x00
    db 0x01                     ; op-length
    db 0xe9
    db const_inst_enc_d_0


g_asm_inst_template_call:
    dq 0                        ; normal
    ;; D
    dq .rel32
    dq 0

.rel32:                         ; D
    dd 1
    db 0x0f, 0x04, 0xff, 0x05   ; rel32, op(1)
    db 0x00
    db 0x01                     ; op-length
    db 0xe8
    db const_inst_enc_d_0

section_rodata_end:
    ;; --< rodata

data_segment_rest_size: equ $-data_segment_begin
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
