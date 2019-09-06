    bits    64
    org     0x00400000          ; このあたりにマッピングしておきたい

segment_align:  equ 0x1000

app_max_code_buffer_size:   equ 0x10000
app_max_asm_buffer_size:    equ 0x8000
app_max_sexp_objects_count: equ 0x8000

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
    sub rsp, 8

    mov qword [rbp-8], 0        ; statements

    call load_code_from_stdin
    ;; call print_loaded_code_to_stdout

    call parse_code
    mov [rbp-8], rax

    mov rdi, [rbp-8]
    call sexp_print

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

    call print_asm_to_stderr

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

print_asm_to_stderr:
    mov rax, 1                  ; 1 - write
    mov rdi, 2                  ; fd 2 - stderr
    mov rsi, g_asm_buffer
    mov rdx, [g_asm_buffer_size]
    syscall
    ret

parse_code:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    mov qword [rbp-56], 0       ; sexp*, statements
    mov qword [rbp-48], 0       ; sexp*, last-statement
    mov qword [rbp-40], 0       ; sexp*, statement

    ;; parser {
    ;;   char* buffer
    ;;   u64   offset
    ;;   u64   status = 2  finished
    ;:                | 1  failed
    ;:                | 0  running
    ;; }
    ;; rbp-32: char*, buffer
    mov qword [rbp-32], g_code_buffer
    ;; rbp-24: u64, offset
    mov qword [rbp-24], 0
    ;; rbp-16: u64, status
    mov qword [rbp-16], 0
    ;; rbp-8: *parser
    lea rax, [rbp-32]
    mov [rbp-8], rax

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

    mov rdi, 1
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


;;; rdi: *parser
parse_addressing:
    push rbp
    mov rbp, rsp
    sub rsp, 56

    mov qword [rbp-56], 0       ; op, 0 = add, 1 = minus
    mov qword [rbp-48], 0       ; tmp-node
    mov qword [rbp-40], 0       ; expected-size. 0 is unknown
    mov qword [rbp-32], 0       ; kind, 0 = reg, 1 = reg +|- disp, 2 = disp-only
    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;; addressing

    ;; normal register
    mov rdi, [rbp-8]
    call parse_reg_value
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded

    ;; Reset failure
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

    ;; [
    mov rdi, [rbp-8]
    call parse_bracket_l
    cmp rax, 0
    je .failed

.reg:
    ;; reg
    mov rdi, [rbp-8]
    call parse_reg_value
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .expect_only_disp       ; skip

    mov rdi, [rbp-8]
    call parser_skip_space

    ;; reg +|- disp
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

    ;; reg
    mov qword [rbp-32], 0       ; kind = disp
    jmp .enclose

.found_mid_op_minus:
    mov qword [rbp-56], 1       ; op

.found_mid_op_plus:
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; disp
    mov rdi, [rbp-8]
    call parse_constant_value
    mov [rbp-48], rax           ; tmp-node

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    ;; (lhs . rhs)
    mov rdi, [rbp-24]           ; lhs = reg
    mov rsi, [rbp-48]           ; rhs = disp
    call sexp_alloc_cons
    mov [rbp-24], rax           ; expr-cons

    ;; value-tag
    ;;         ------4|--2|1|1|
    ;;                        6 = expr (addressing)
    ;;                      0   = size-unknown
    ;;                    ?     = op: 0 = add, 1 = sub
    mov rdi, 0x0000000000000006

    mov rax, [rbp-56]           ; op
    and rax, 0x000000000000ffff
    shl rax, 16                 ; 2 * 8 bits
    or rdi, rax                 ; set kind to value-tag

    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; expr-cons
    call sexp_alloc_cons
    mov [rbp-24], rax

    mov qword [rbp-32], 1       ; kind = reg+disp
    jmp .enclose

.expect_only_disp:
    ;; Reset failure
    mov rdi, [rbp-8]
    call parser_reset_failed

    ;; disp
    mov rdi, [rbp-8]
    call parse_constant_value
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    mov qword [rbp-32], 2       ; kind = disp

.enclose:
    ;;
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; ]
    mov rdi, [rbp-8]
    call parse_bracket_r
    cmp rax, 0
    je .failed

.succeeded_addressing:
;    mov rdi, [rbp-24]
;    call sexp_print

    ;; value-tag
    ;;         ------4|--2|1|1|
    ;;                        5 = addressing
    ;;                      ?   = size(default: 0, unknown)
    ;;                    ?     = kind (reg, reg+disp, disp)
    mov rdi, 0x0000000000000005

    mov rax, [rbp-40]           ; size
    and rax, 0x00000000000000ff
    shl rax, 8                  ; 1 * 8 bits
    or rdi, rax                 ; set size to value-tag

    mov rax, [rbp-32]           ; kind
    and rax, 0x000000000000ffff
    shl rax, 16                 ; 2 * 8 bits
    or rdi, rax                 ; set kind to value-tag

    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; addressing expr
    call sexp_alloc_cons
    mov [rbp-24], rax

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
    je .succeeded_symbol

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

.succeeded_symbol:
    ;; value-tag
    ;;         |-----4||-2||-2|
    ;;         |      ||  ||  |
    ;;                        2 = label
    mov rdi, 0x0000000000000002
    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; symbol
    call sexp_alloc_cons
    mov [rbp-24], rax

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
    mov rdx, [rax+24]           ; REX

    ;; value-tag
    ;;         ------4|--2|1|1|
    ;;                    | | 1 = register
    ;;                    | ?   = size
    ;;                    ?     = register-index
    mov rax, 0x0000000000000001

    mov rcx, rsi                ; size
    and rcx, 0x00000000000000ff
    shl rcx, 8                  ; 1 * 8 bits
    or rax, rcx

    mov rcx, rdi                ; register-index
    and rcx, 0x00000000000000ff
    shl rcx, 16                 ; 2 * 8 bits
    or rax, rcx                 ; set kind to value-tag

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
    sub rsp, 24

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

.skip:
    ;; symbol
    mov rdi, [rbp-8]
    call parse_symbol
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

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
tnode_alloc_uint_node:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov qword [rbp-16], 0       ; size
    mov [rbp-8], rdi            ; sexp*, value

    call sexp_internal_int_value ; value
    cmp rax, 0xff
    jle .size_1                 ; rax <= 0x7f, sizeof(1)

    cmp rax, 0xffff
    jle .size_2                 ; rax <= 0x7fff, sizeof(2)

    cmp rax, 0xffffffff
    jle .size_4                 ; rax <= 0x7fffffff, sizeof(4)

    jmp .size_8                 ; otherwise, sizeof(2)

.size_1:
    mov qword [rbp-16], 1       ; size
    jmp .alloc

.size_2:
    mov qword [rbp-16], 2       ; size
    jmp .alloc

.size_4:
    mov qword [rbp-16], 4       ; size
    jmp .alloc

.size_8:
    mov qword [rbp-16], 8       ; size
    jmp .alloc

.alloc:
    ;; value-tag
    ;;         ------4|--2|1|1|
    ;;                        4 = int
    ;;                      ?   = size
    mov rdi, 0x0000000000000004

    mov rax, [rbp-16]           ; size
    and rax, 0x00000000000000ff
    shl rax, 8                  ; 8 * 1 bits
    or rdi, rax                 ; size

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
    mov [rdi], rax            ; update sexp* in sexp**

    mov rax, [rbp-16]           ; return-value

    leave
    ret


;;; rdi: sexp*, (type . value)
;;; -> u64, type
tnode_type:
    call sexp_car
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
;;; -> sexp*, value
tnode_value:
    call sexp_cdr
    ret


;;; rdi: sexp*, (type . value)
;;; -> u64, index
tnode_reg_index:
    call tnode_type
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x000000000000ffff ; register-index
    ret


;;; rdi: sexp* statements
asm_process_statements:
    push rbp
    mov rbp, rsp
    sub rsp, 88

    ;; asm {
    ;;   sexp* labels
    ;;   sexp* replacements
    ;;   sexp* last-label
    ;;   u64   segment-base
    ;; }
    mov qword [rbp-88], 0       ; sexp*, nil, asm.labels       0
    mov qword [rbp-80], 0       ; sexp*, nil, asm.replacements 8
    mov qword [rbp-72], 0       ; sexp*, nil, last-label       16
    mov qword [rbp-64], 0       ; u64, 0    , segment-base     24

    lea rax, [rbp-88]
    mov [rbp-16], rax           ; asm*
    mov [rbp-8], rdi            ; statements*

    mov rdi, [rbp-16]           ; asm*
    mov rsi, [rbp-8]            ; statements*
    call asm_process_statements_phase1

    ;; TODO
    ;;mov rdi, [rbp-88]           ; asm.labels
    ;;call sexp_print
    ;;call runtime_print_newline
    ;;
    ;;mov rdi, [rbp-80]           ; asm.replacements
    ;;call sexp_print   ;
    ;;call runtime_print_newline
    ;;
    mov rdi, [rbp-16]           ; asm*
    call asm_fill_replacements

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

.loop:
    mov rdi, [rbp-16]           ; statements
    cmp rdi, 0
    je .break

    call sexp_car

    mov rdi, [rbp-8]            ; asm
    mov rsi, rax                ; statement
    call asm_process_statement

    ;; step
    mov rdi, [rbp-16]           ; statements
    call sexp_cdr
    mov [rbp-16], rax           ; statements

    jmp .loop

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

    mov rdi, str_ice_invalid_statement
    call runtime_print_string

    mov rdi, 1
    call runtime_exit

.label:
    mov rdi, [rbp-8]             ; asm*
    call asm_current_loc

    mov rdi, rax                 ; loc
    call sexp_alloc_int
    mov rdi, rax
    call tnode_alloc_uint_node

    ;; update labels
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; symbol
    mov rdx, rax                ; (type . loc)
    call asm_add_labels

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

    mov rdi, 1
    call runtime_exit

.break:
    leave
    ret





;;; rdi: sexp*(tnode), lhs
;;; rsi: sexp*(tnode), rhs
asm_infer_size:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov qword [rbp-32], 0       ; size

    mov byte [rbp-24], 0        ; rhs.size
    mov byte [rbp-23], 0        ; lhs.size

    mov byte [rbp-22], 0        ; rhs.type_tag
    mov byte [rbp-21], 0        ; lhs.type_tag

    mov [rbp-16], rsi           ; rhs
    mov [rbp-8], rdi            ; lhs

    ;; lhs
    mov rdi, [rbp-8]            ; lhs
    call tnode_type_tag
    mov byte [rbp-21], al       ; lhs.type_tag

    mov rdi, [rbp-8]            ; lhs
    call tnode_type_size
    mov byte [rbp-23], al       ; lhs.size

    ;; rhs
    mov rdi, [rbp-16]           ; rhs
    call tnode_type_tag
    mov byte [rbp-22], al       ; rhs.type_tag

    mov rdi, [rbp-16]           ; rhs
    call tnode_type_size
    mov byte [rbp-24], al       ; rhs.size

    ;; size
    xor rax, rax
    mov al, [rbp-23]            ; lhs.size
    shl rax, 8                  ; 0xLL00
    xor rcx, rcx
    mov cl, [rbp-24]            ; rhs.size
    or ax, cx                   ; 0xLLRR
    mov [rbp-32], rax

    ;; check
    mov al, [rbp-21]            ; lhs.type_tag
    cmp al, 5                   ; addressing
    je .not_shrinkable


    mov al, [rbp-22]            ; rhs.type_tag
    cmp al, 5                   ; addressing
    je .not_shrinkable

.shrinkable:
    ;; size-matching
    mov rax, [rbp-32]           ; size

    cmp ax, 0x0808              ; 8, 8 -> 8
    je .bits64
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

    jmp .size_undecidable

.not_shrinkable:
    ;; size-matching
    mov rax, [rbp-32]           ; size

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

    jmp .size_undecidable

.bits32:
    mov rax, 4
    jmp .break

.bits64:
    mov rax, 8
    jmp .break

.size_undecidable:
    mov rdi, [rbp-32]           ; size
    call runtime_print_uint64
    call runtime_print_newline

    mov rdi, 32                 ; debug
    call runtime_exit

.break:
    leave
    ret

;;; rdi: asm*
;;; rsi: args
asm_write_inst_mov:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov qword [rbp-48], 0       ; size

    mov qword [rbp-32], 0       ; args[2], sexp*(tnode)
    mov qword [rbp-24], 0       ; args[1], sexp*(tnode)

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; args[1]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-24], rax           ; args[1]

    ;; args[2]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-32], rax           ; args[2]

    ;; args[1] inspect
    mov rdi, [rbp-24]
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .arg_1_reg

    cmp rax, 5                  ; addressing
    je .arg_1_mod_rm

    mov rdi, 10                 ; debug
    call runtime_exit

.arg_1_reg:
    ;; args[2] inspect
    mov rdi, [rbp-32]
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .encode_mr

    cmp rax, 4                  ; integer
    je .encode_oi

    cmp rax, 2                  ; label
    je .encode_oi

    mov rdi, 11                 ; debug
    call runtime_exit

.arg_1_mod_rm:
    ;; args[2] inspect
    mov rdi, [rbp-32]
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .encode_mr

    cmp rax, 4                  ; integer
    je .encode_mi

    cmp rax, 2                  ; label
    je .encode_mi

    mov rdi, 13                 ; debug
    call runtime_exit

    ;; MOV r/m, r
.encode_mr:
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    mov rsi, [rbp-32]           ; args[2], sexp*(tnode)
    call asm_infer_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, 0x89               ; MOV r/m, r
    call asm_write_u8

    mov rdi, [rbp-32]           ; args[2]
    call tnode_reg_index        ; /r-digit

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, rax                ; ModR/M, /r-digit
    call asm_write_operands

    jmp .break

;; MOV r, imm
.encode_oi:
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    mov rsi, [rbp-32]           ; args[2], sexp*(tnode)
    call asm_infer_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, [rbp-24]           ; args[1]
    call tnode_reg_index        ; /r-digit

    mov rdi, 0xb8               ; MOV r, imm
    add rdi, rax                ;            + rd
    call asm_write_u8

    ;; disp
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; arg[2]
    mov rdx, [rbp-48]           ; size
    call asm_write_imm_operand

    jmp .break

;; MOV r/m, imm
.encode_mi:
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    mov rsi, [rbp-32]           ; args[2], sexp*(tnode)
    call asm_infer_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, 0xc7               ; MOV r/m, imm
    call asm_write_u8

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, 0                  ; ModR/M, /r-digit
    call asm_write_operands

    ;; disp
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; arg[2]
    mov rdx, [rbp-48]           ; size
    call asm_write_sign_ext_imm_operand

    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_push:
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

    ;; PUSH r/m
.encode_m:
    mov rdi, 0xff               ; PUSH r/m
    call asm_write_u8

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, 6                  ; ModR/M, /r-digit
    call asm_write_operands

    jmp .break

;; PUSH r
.encode_o:
    mov rdi, [rbp-24]           ; arg[1]
    call tnode_type
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x000000000000ffff ; register-index

    mov rdi, 0x50               ; PUSH r
    add rdi, rax                ;        + rd
    call asm_write_u8

    jmp .break

;; PUSH imm
.encode_i:
    jmp .break

.break:
    leave
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
    mov rdi, 0x8f               ; POP r/m
    call asm_write_u8

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, 0                  ; ModR/M, /r-digit
    call asm_write_operands

    jmp .break

;; POP r
.encode_o:
    mov rdi, [rbp-24]           ; arg[1]
    call tnode_type
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x000000000000ffff ; register-index

    mov rdi, 0x58               ; POP r
    add rdi, rax                ;       + rd
    call asm_write_u8

    jmp .break

;; PUSH imm
.encode_i:
    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_cmp:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov qword [rbp-48], 0       ; size

    mov qword [rbp-32], 0       ; args[2], sexp*(tnode)
    mov qword [rbp-24], 0       ; args[1], sexp*(tnode)

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; args[1]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-24], rax           ; args[1]

    ;; args[2]
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval ; hd
    mov [rbp-32], rax           ; args[2]

    ;; args[1] inspect
    mov rdi, [rbp-24]
    call tnode_type_tag

    cmp rax, 1                  ; register
    je .arg_1_reg

    cmp rax, 5                  ; addressing
    je .arg_1_mod_rm

    mov rdi, 10                 ; debug
    call runtime_exit

.arg_1_reg:
    mov rdi, [rbp-24]
    call tnode_reg_index
    cmp rax, 0
    je .arg_1_reg_a

    ;; args[2] inspect
    mov rdi, [rbp-32]
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .encode_mi

    cmp rax, 2                  ; label
    je .encode_mi

    cmp rax, 1                  ; register
    je .encode_rm

    mov rdi, 11                 ; debug
    call runtime_exit

.arg_1_reg_a:
    ;; args[2] inspect
    mov rdi, [rbp-32]
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .encode_i

    cmp rax, 2                  ; label
    je .encode_i

    jmp .encode_rm

.arg_1_mod_rm:
    ;; args[2] inspect
    mov rdi, [rbp-32]
    call tnode_type_tag

    cmp rax, 4                  ; integer
    je .encode_mi

    cmp rax, 2                  ; label
    je .encode_mi

    cmp rax, 1                  ; register
    je .encode_mr

    mov rdi, 13                 ; debug
    call runtime_exit

    ;; (R|E)?A(X|L), imm
.encode_i:
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    mov rsi, [rbp-32]           ; args[2], sexp*(tnode)
    call asm_infer_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, 0x3d               ; CMP
    call asm_write_u8

    ;; disp
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; arg[2]
    mov rdx, [rbp-48]           ; size
    call asm_write_sign_ext_imm_operand

    jmp .break

    ;; r/m, imm
.encode_mi:
    mov rdi, [rbp-32]           ; args[2], sexp*(tnode)
    call tnode_type_size
    cmp rax, 1
    je .encode_mi_imm8

    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    mov rsi, [rbp-32]           ; args[2], sexp*(tnode)
    call asm_infer_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, 0x81               ; CMP
    call asm_write_u8

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, 7                  ; ModR/M, /r-digit
    call asm_write_operands

    ;; disp
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; arg[2]
    mov rdx, [rbp-48]           ; size
    call asm_write_sign_ext_imm_operand

    jmp .break

.encode_mi_imm8:
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    call tnode_type_size
    mov [rbp-48], rax           ; update-size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, 0x83               ; CMP
    call asm_write_u8

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, 7                  ; ModR/M, /r-digit
    call asm_write_operands

    ;; disp
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-32]           ; arg[2]
    mov rdx, 1                  ; size
    call asm_write_sign_ext_imm_operand

    jmp .break

    ;; r/m, r
.encode_mr:
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    mov rsi, [rbp-32]           ; args[2], sexp*(tnode)
    call asm_infer_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, 0x38               ; CMP
    call asm_write_u8

    mov rdi, [rbp-32]           ; args[2]
    call tnode_reg_index        ; /r-digit

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, rax                ; ModR/M, /r-digit
    call asm_write_operands

    jmp .break

   ;; r, r/m
.encode_rm:
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    mov rsi, [rbp-32]           ; args[2], sexp*(tnode)
    call asm_infer_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

    mov rdi, 0x3b               ; CMP
    call asm_write_u8

    mov rdi, [rbp-24]           ; args[1]
    call tnode_reg_index        ; /r-digit

    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-24]           ; args[1], effective-reg
    mov rdx, rax                ; ModR/M, /r-digit
    call asm_write_operands

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
    mov rdi, [rbp-24]           ; args[1], sexp*(tnode)
    call tnode_type_size
    mov [rbp-48], rax           ; size

    mov rdi, [rbp-48]           ; size
    call asm_write_rex_w_if_needed

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
    jmp .encode_d_body

.encode_d_je:
    jmp .encode_d_body

.encode_d_jg:
    mov rdi, 0x7f               ; JG
    call asm_write_u8

    jmp .encode_d_body

.encode_d_jge:
    jmp .encode_d_body

.encode_d_jl:
    jmp .encode_d_body

.encode_d_jle:
    jmp .encode_d_body

.encode_d_jne:
    jmp .encode_d_body

.encode_d_body:
    mov rdi, 0x3d               ; CMP
    call asm_write_u8

    ;; disp
;    mov rdi, [rbp-8]            ; asm*
;    mov rsi, [rbp-32]           ; arg[2]
;    mov rdx, [rbp-48]           ; size
;    call asm_write_sign_ext_imm_operand

    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_call:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    leave
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

    ;; syscall
    mov rdi, 0x0f
    call asm_write_u8

    mov rdi, 0x05
    call asm_write_u8

    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_leave:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_ret:
    push rbp
    mov rbp, rsp
    sub rsp, 48

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
    mov rcx, 0                  ; NOT disallow
    call asm_write_node

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
    mov rdi, [rbp-8]            ; asm*
    lea rsi, [rbp-16]           ; (hd . rest)*
    call asm_step_args_with_eval

    mov rdi, [rbp-8]            ; asm*
    mov rcx, [rdi+16]           ; asm.last-symbol

    ;; update labels
    mov rdi, [rbp-8]            ; asm*
    mov rsi, rcx                ; symbol
    mov rdx, rax                ; (type . loc)
    call asm_add_labels

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
    mov rax, [g_asm_buffer_cursor]
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

    mov rdi, 0x90               ; fill by 0x90
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


;;; rdi: u64, size
asm_write_rex_w_if_needed:
    cmp rdi, 8
    jne .break

    mov rdi, 0x48               ; REX.W
    call asm_write_u8

.break:
    ret


;;; rdi: asm*
;;; rsi, sexp** args*
asm_step_args_with_eval:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov [rbp-8], rdi            ; asm*

    mov rdi, rsi                ; args*
    call tnode_step_args

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; arg
    call asm_eval_expr

    leave
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
    mov rdi, 13                 ; debug
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
    mov byte [rbp-72], 1        ; mod

    mov rdi, [rbp-48]           ; index-reg
    call tnode_type

    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x000000000000ffff ; register-index

    mov byte [rbp-70], al        ; r/m

    mov byte [rbp-69], 1         ; disp-size

    jmp .mod

    ;; [REG + disp32]
.mod2:
    ;; ICE, not-supported
    mov rdi, 16                 ; debug
    call runtime_exit

    ;; REG
.mod3:
    mov byte [rbp-72], 3        ; mod

    mov rdi, [rbp-16]           ; effective-reg
    call tnode_type
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x000000000000ffff ; register-index

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

    mov rdi, rcx
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
    mov rdi, rcx
    call asm_write_u8

    jmp .mod_after_disp

.unsupported_operand:
    mov rdi, rax
    call runtime_print_uint64
    call runtime_print_newline

    ;; ICE
    mov rdi, 16                 ; debug
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
    mov rcx, 0                  ; NOT disallow
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

    cmp rax, 8                  ; expr
    je .arg_expr

    cmp rax, 6
    je .arg_addressing_expr

    cmp rax, 5
    je .arg_addressing

    cmp rax, 4                  ; integer
    je .arg_integer

    cmp rax, 3                  ; string
    je .arg_string

    cmp rax, 2                  ; label
    je .arg_label

    cmp rax, 1
    je .arg_register

    ;; failed
    mov rdi, 19                 ; debug
    call runtime_exit

.arg_expr:
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
.arg_addressing:
.arg_addressing_expr:
    mov rax, [rbp-16]           ; pass-through
    jmp .break

.arg_label:
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
    call asm_find_labels

    cmp rax, 0
    jne .break

    mov rax, [rbp-16]           ; pass-through
    jmp .break

.arg_label_doller:
    ;; current location
    mov rdi, [rbp-8]            ; asm*
    call asm_current_loc

    mov rdi, rax
    call sexp_alloc_int
    mov rdi, rax
    call tnode_alloc_uint_node

    jmp .break

.arg_label_doller_doller:
    ;; current segment-base
    mov rdi, [rbp-8]            ; asm*
    call asm_segment_base

    mov rdi, rax
    call sexp_alloc_int
    mov rdi, rax
    call tnode_alloc_uint_node

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
asm_current_loc:
    mov rcx, [g_asm_buffer_cursor]
    call asm_segment_base
    add rcx, rax
    mov rax, rcx
    ret


;;; rdi: asm*
;;; rsi: sexp*, tnode
;;; rdx: u64 byte-size
;;; rcx: u64 disallow-laxy
asm_write_node:
    push rbp
    mov rbp, rsp
    sub rsp, 40

    mov [rbp-32], rcx           ; disallow-lazy
    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; asm*

    mov rdi, [rbp-16]           ; (type . value): sexp*(tnode)
    call tnode_type_tag

    cmp rax, 3                  ; string
    je .write_sexp

    cmp rax, 4                  ; int
    je .write_sexp

    mov rcx, [rbp-32]           ; disallow
    cmp rcx, 1
    je .failed

    cmp rax, 8                  ; expr
    je .lazy_write_sexp

    cmp rax, 2                  ; label
    je .lazy_write_sexp

.failed:
    ;; Failed
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

    mov rdi, rax
    mov rsi, [rbp-24]           ; byte-size
    call asm_write_sexp

    jmp .break

.lazy_write_sexp:
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; (type . value): sexp*
    mov rdx, [rbp-24]           ; byte-size
    call asm_lazy_write_sexp

.break:
    leave
    ret


;;; rdi: sexp*
;;; rsi: u64 byte-size
asm_write_sexp:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov qword [rbp-40], 0       ; total-size
    mov qword [rbp-32], 0       ; counter
    mov qword [rbp-24], 0       ; char*
    mov [rbp-16], rsi           ; byte-size
    mov [rbp-8], rdi            ; sexp*

    mov rax, [rdi]              ; sexp.tag

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

    mov rdi, rax                ; int-value
    mov rsi, [rbp-16]           ; byte-size
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

    mov rdi, [rbp-24]           ; char*
    mov rdi, [rdi]              ; *(char*)
    and rdi, 0xff
    mov rsi, [rbp-16]           ; byte-size
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


;;; rdi
;;; rsi
;;; rdx
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
    mov rdi, 0                  ; int-value
    mov rsi, [rbp-24]           ; byte-size
    call asm_write_value

    ;; save lazy information
    mov rdi, [rbp-8]            ; asm*
    mov rsi, [rbp-16]           ; sexp*
    mov rdx, [rbp-32]           ; loc
    mov rcx, [rbp-24]           ; byte-size
    call asm_add_replacements

    leave
    ret


;;; rdi
;;; rsi
asm_write_value:
    cmp rsi, 1
    je .u8

    cmp rsi, 2
    je .u16

    cmp rsi, 4
    je .u32

    cmp rsi, 8
    je .u64

    ;; Failed
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

;;; rdi: u8
asm_write_u8:
    mov rax, g_asm_buffer
    mov rcx, [g_asm_buffer_cursor]

    add rax, rcx
    mov rdx, rdi
    mov byte [rax], dl

    add rcx, 1
    mov [g_asm_buffer_cursor], rcx

    mov rax, [g_asm_buffer_size]
    cmp rcx, rax
    jl .break                   ; if rcx < rax

    mov [g_asm_buffer_size], rcx

.break:
    mov rax, 1
    ret

;;; rdi: u16
asm_write_u16:
    mov rax, g_asm_buffer
    mov rcx, [g_asm_buffer_cursor]

    add rax, rcx
    mov rdx, rdi
    mov word [rax], dx

    add rcx, 2
    mov [g_asm_buffer_cursor], rcx

    mov rax, [g_asm_buffer_size]
    cmp rcx, rax
    jl .break                   ; if rcx < rax

    mov [g_asm_buffer_size], rcx

.break:
    mov rax, 2
    ret

;;; rdi: u32
asm_write_u32:
    mov rax, g_asm_buffer
    mov rcx, [g_asm_buffer_cursor]

    add rax, rcx
    mov rdx, rdi
    mov dword [rax], edx

    add rcx, 4
    mov [g_asm_buffer_cursor], rcx

    mov rax, [g_asm_buffer_size]
    cmp rcx, rax
    jl .break                   ; if rcx < rax

    mov [g_asm_buffer_size], rcx

.break:
    mov rax, 4
    ret

;;; rdi: u64
asm_write_u64:
    mov rax, g_asm_buffer
    mov rcx, [g_asm_buffer_cursor]

    add rax, rcx
    mov rdx, rdi
    mov qword [rax], rdx

    add rcx, 8
    mov [g_asm_buffer_cursor], rcx

    mov rax, [g_asm_buffer_size]
    cmp rcx, rax
    jl .break                   ; if rcx < rax

    mov [g_asm_buffer_size], rcx

.break:
    mov rax, 8
    ret


;;; rdi: asm*
;;; rsi: sexp*, symbol
;;; rdx: sexp*, tnode
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
    sub rsp, 24

    mov [rbp-24], rsi           ; sexp*, finding-symbol
    mov qword [rbp-16], 0       ; sexp*, current
    mov qword [rbp-8], 0        ; sexp*, labels

    mov rax, [rdi]              ; asm.labels
    mov [rbp-8], rax            ;

.loop:
    mov rax, [rbp-8]            ; labels
    cmp rax, 0
    je .break

    mov rdi, [rbp-8]            ; labels
    call sexp_car
    mov [rbp-16], rax           ; current

    mov rdi, [rbp-16]           ; current
    call sexp_car               ; symbol
    mov rdi, rax
    mov rsi, [rbp-24]           ; finding-symbol
    call sexp_equals

    cmp rax, 1
    je .found

    ;; step
    mov rdi, [rbp-8]            ; labels
    call sexp_cdr
    mov [rbp-8], rax

    jmp .loop

.found:
    mov rdi, [rbp-16]           ; current
    call sexp_cdr

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
    mov rcx, 1                  ; disallow
    call asm_write_node

    ;; step
    mov rdi, [rbp-16]           ; replacements
    call sexp_cdr
    mov [rbp-16], rax           ; replacements

    jmp .loop

.break:
    leave
    ret


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
    mov rdi, 16
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

g_code_buffer:  times app_max_code_buffer_size db 0
g_code_size:    dq 0

g_asm_buffer:  times app_max_asm_buffer_size db 0
g_asm_buffer_size:  dq 0
g_asm_buffer_cursor:dq 0

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

str_paren_l:    db "(", 0
str_paren_r:    db ")", 0
str_colon:      db ":", 0
str_comma:      db ",", 0

str_g_symbol_doller:    db "$", 0
str_g_symbol_doller_doller: db "$$", 0

str_reg_rax:    db "rax", 0
str_reg_rcx:    db "rcx", 0
str_reg_rdx:    db "rdx", 0
str_reg_rbx:    db "rbx", 0
str_reg_rsp:    db "rsp", 0
str_reg_rbp:    db "rbp", 0
str_reg_rsi:    db "rsi", 0
str_reg_rdi:    db "rdi", 0

str_size_byte:  db "byte", 0
str_size_word:  db "word", 0
str_size_dword: db "dword", 0
str_size_qword: db "qword", 0

g_register_table:
    dq str_reg_rax
    dq 0                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq str_reg_rcx
    dq 1                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq str_reg_rdx
    dq 2                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq str_reg_rbx
    dq 3                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq str_reg_rsp
    dq 4                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq str_reg_rbp
    dq 5                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq str_reg_rsi
    dq 6                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq str_reg_rdi
    dq 7                        ; register-index
    dq 8                        ; size
    dq 0                        ; TODO: rex

    dq 0                        ; null-termination

str_ice_invalid_statement:  db "ICE: Invalid statement", 0
str_ice_invalid_inst:       db "ICE: Invalid inst", 0
str_ice_invalid_type:       db "ICE: Invalid type", 0
str_ice_invalid_node:       db "ICE: Invalid node", 0
str_ice_invalid_expr:       db "ICE: Invalid op", 0
str_ice_unsupported_size:   db "ICE: Unsupported size", 0
str_ice_nomore_sexps:   db "ICE: Cannot allocate sexp nodes", 0

str_inst_bits:  db "bits", 0
str_inst_org:   db "org", 0
str_inst_equ:   db "equ", 0
str_inst_align: db "align", 0
str_inst_resb:  db "resb", 0
str_inst_db:    db "db", 0
str_inst_dw:    db "dw", 0
str_inst_dd:    db "dd", 0
str_inst_dq:    db "dq", 0

str_inst_mov:   db "mov", 0
str_inst_push:  db "push", 0
str_inst_pop:   db "pop", 0
str_inst_cmp:   db "cmp", 0
str_inst_ja:    db "ja", 0
str_inst_je:    db "je", 0
str_inst_jg:    db "jg", 0
str_inst_jge:   db "jge", 0
str_inst_jl:    db "jl", 0
str_inst_jle:   db "jle", 0
str_inst_jne:   db "jne", 0
str_inst_call:  db "call", 0
str_inst_syscall:   db "syscall", 0
str_inst_leave: db "leave", 0
str_inst_ret:   db "ret", 0

str_debug_arrow_r:  db "->", 0
str_debug_finished: db "[DEBUG] finished", 0
str_debug_failed_to_parse_addr: db "[DEBUG] Failed to parse addr", 0

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

    dq str_inst_mov
    dq asm_write_inst_mov
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
