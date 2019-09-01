    bits    64
    org     0x00400000          ; このあたりにマッピングしておきたい

segment_align:  equ 0x1000

app_max_code_buffer_size:   equ 0x10000
app_max_asm_buffer_size:    equ 0x8000
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

rest_size:  equ $-$$
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
    sub rsp, 56

    ;; asm {
    ;;   sexp* labels
    ;;   sexp* replacements
    ;; }
    mov qword [rbp-56], 0       ; nil
    mov qword [rbp-48], 0       ; nil
    mov qword [rbp-40], 0       ; return
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
    mov [rbp-40], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    ;; debug
    call runtime_print_newline
    mov rdi, str_debug_arrow_r
    call runtime_print_string
    mov rdi, [rbp-40]
    call sexp_print
    call runtime_print_newline

    lea rdi, [rbp-56]           ; asm
    mov rsi, [rbp-40]           ; statement
    call asm_process_statement

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
    ;; TODO
    mov rdi, [rbp-56]           ; labels
    call sexp_print

    call print_asm_to_stderr

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

    mov rdi, [rbp-24]
    call sexp_print

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

    ;; reg or constant-expr
    mov rdi, [rbp-8]
    call parser_reset_failed

    mov rdi, [rbp-8]
    call parse_expr_add_sub
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .break

    jne .failed

.break:
    mov rdi, [rbp-24]
    call sexp_print

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
    ;;         |-----4||-2||-2|
    ;;         |      ||  ||  |
    ;;                        6 = expr
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

.succeeded:
    mov rdi, [rbp-24]
    call sexp_print

    ;; value-tag
    ;;         |-----4||-2||-2|
    ;;         |      ||  ||  |
    ;;                        5 = addressing
    ;;                    ?     = kind
    ;;                ?         = size
    mov rdi, 0x0000000000000005

    mov rax, [rbp-32]           ; kind
    and rax, 0x000000000000ffff
    shl rax, 16                 ; 2 * 8 bits
    or rdi, rax                 ; set kind to value-tag

    mov rax, [rbp-40]           ; size
    and rax, 0x00000000ffffffff
    shl rax, 32                 ; 4 * 8 bits
    or rdi, rax                 ; set size to value-tag

    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; addressing expr
    call sexp_alloc_cons
    mov [rbp-24], rax

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
    sub rsp, 32

    mov qword [rbp-32], 0       ; tmp_return
    mov qword [rbp-24], 0       ; return
    mov qword [rbp-16], 0       ; u64, initial offset
    mov [rbp-8], rdi            ; *parser

    call parser_get_index
    mov [rbp-16], rax

    ;;
    mov rdi, [rbp-8]
    call parse_expr_primitive
    mov [rbp-24], rax

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
.found_mid_op_plus:
.found_mid_op:
    mov rdi, [rbp-8]
    call parser_skip_space

    ;; expr
    mov rdi, [rbp-8]
    call parse_expr
    mov [rbp-32], rax           ; tmp_return

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    jne .failed

    jmp .loop

.break:
    mov rdi, [rbp-24]
    call sexp_print

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

    ;; register
    mov rdi, [rbp-8]
    call parse_reg_value
    mov [rbp-24], rax

    mov rdi, [rbp-8]
    call parser_is_failed
    cmp rax, 0
    je .succeeded

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
    mov rdi, [rbp-24]
    call sexp_print

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
    ;; value-tag
    ;;         |-----4||-2||-2|
    ;;         |      ||  ||  |
    ;;                        4 = int
    mov rdi, 0x0000000000000004
    call sexp_alloc_int
    mov rdi, rax                ; value-tag
    mov rsi, [rbp-24]           ; int
    call sexp_alloc_cons
    mov [rbp-24], rax

    jmp .succeeded

.succeeded:
    mov rdi, [rbp-24]
    call sexp_print

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
    call sexp_alloc_string

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
    call sexp_alloc_string
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
    call sexp_internal_string_length
    cmp rax, 8
    jg .failed

    ;;
    mov rdi, [rbp-24]           ; symbol
    call sexp_value_string_as_hash
    mov [rbp-24], rax           ; hash

    ;; qword
    mov rdi, str_size_qword
    call runtime_string_hash
    mov rdi, [rbp-24]
    cmp rdi, rax
    je .qword

    ;; failed...
    jmp .failed

.qword:
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

    ;;
    mov rdi, [rbp-24]           ; symbol
    call sexp_internal_string_length
    cmp rax, 8
    jg .failed

    ;;
    mov rdi, [rbp-24]           ; symbol
    call sexp_value_string_as_hash
    mov [rbp-32], rax           ; hash

    ;; rbp
    mov rdi, str_reg_rbp
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rbp

    ;; rax
    mov rdi, str_reg_rax
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rax

    ;; rcx
    mov rdi, str_reg_rcx
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rcx

    ;; rdx
    mov rdi, str_reg_rdx
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rdx

    ;; rbx
    mov rdi, str_reg_rbx
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rbx

    ;; rsp
    mov rdi, str_reg_rsp
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rsp

    ;; rbp
    mov rdi, str_reg_rbp
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rbp

    ;; rsi
    mov rdi, str_reg_rsi
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rsi

    ;; rdi
    mov rdi, str_reg_rdi
    call runtime_string_hash
    mov rdi, [rbp-32]           ; hash
    cmp rdi, rax
    je .succeeded_rdi

    ;; failed...
    jmp .failed

.succeeded_rax:
    mov qword [rbp-40], 0       ; register-index
    jmp .succeeded_reg64

.succeeded_rcx:
    mov qword [rbp-40], 1       ; register-index
    jmp .succeeded_reg64

.succeeded_rdx:
    mov qword [rbp-40], 2       ; register-index
    jmp .succeeded_reg64

.succeeded_rbx:
    mov qword [rbp-40], 3       ; register-index
    jmp .succeeded_reg64

.succeeded_rsp:
    mov qword [rbp-40], 4       ; register-index
    jmp .succeeded_reg64

.succeeded_rbp:
    mov qword [rbp-40], 5       ; register-index
    jmp .succeeded_reg64

.succeeded_rsi:
    mov qword [rbp-40], 6       ; register-index
    jmp .succeeded_reg64

.succeeded_rdi:
    mov qword [rbp-40], 7       ; register-index
    jmp .succeeded_reg64

.succeeded_reg64:
    ;; value-tag
    ;;         |-----4||-2||-2|
    ;;         |      ||  ||  |
    ;;                        1 = register
    ;;                    ?     = register-index
    ;;                8         = size
    mov rdi, 0x0000000800000001

    mov rax, [rbp-40]           ; register-index
    and rax, 0x000000000000ffff
    shl rax, 16                 ; 2 * 8 bits
    or rdi, rax                 ; set kind to value-tag

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


;;; rdi: asm*
;;; rsi: sexp* statement
asm_process_statement:
    push rbp
    mov rbp, rsp
    sub rsp, 72

    mov byte [rbp-72], 0        ; op
    mov byte [rbp-71], 0        ; encoding
    mov byte [rbp-70], 0        ; r
    mov byte [rbp-69], 0        ; rex
    mov byte [rbp-68], 0        ; rex.w


    mov qword [rbp-56], 0       ; 2nd-arg
    mov qword [rbp-48], 0       ; 1st-arg
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
    mov [rbp-40], rax

    mov rdi, [rbp-16]
    call sexp_cdr               ; nil | (args...) as cdr
    mov [rbp-32], rax

    mov rdi, [rbp-40]
    call sexp_cdr               ; value
    mov [rbp-24], rax

    mov rdi, [rbp-40]
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
    mov rdi, [g_asm_buffer_size] ; loc
    call sexp_alloc_int

    mov rdi, [rbp-24]           ; symbol
    mov rsi, rax
    call sexp_alloc_cons        ; (value, loc)

    ;; prepend the label. e.g. ((b, 10), ((a, 0), nil))
    mov rdi, rax                ; (value, loc)
    mov rcx, [rbp-8]            ; asm*
    mov rcx, [rcx]              ; asm.labels
    mov rsi, rcx                ; asm.labels
    call sexp_alloc_cons        ; ((value, loc), asm.labels)

    ;; update labels
    mov rcx, [rbp-8]            ; asm*
    mov [rcx], rax              ; asm.labels <- ((value, loc), asm.labels)

    jmp .break

.inst:
    mov rdi, [rbp-24]           ; symbol
    call sexp_print

    mov rdi, [rbp-24]           ; symbol
    call sexp_value_string_as_hash
    mov [rbp-24], rax           ; hash

    ;; bits
    mov rdi, str_inst_bits
    call runtime_string_hash
    mov rdi, [rbp-24]
    cmp rdi, rax
    je .inst_bits

    ;; db
    mov rdi, str_inst_db
    call runtime_string_hash
    mov rdi, [rbp-24]
    cmp rdi, rax
    je .inst_db

    ;; dq
    mov rdi, str_inst_dq
    call runtime_string_hash
    mov rdi, [rbp-24]
    cmp rdi, rax
    je .inst_dq

    ;; mov
    mov rdi, str_inst_mov
    call runtime_string_hash
    mov rdi, [rbp-24]
    cmp rdi, rax
    je .inst_mov

    mov rdi, str_ice_invalid_inst
    call runtime_print_string

    call runtime_print_newline

    mov rdi, 1
    call runtime_exit


.inst_bits:
    jmp .break


.inst_db:
    mov rax, [rbp-32]           ; args
    mov [rbp-40], rax

.inst_db_loop:
    mov rax, [rbp-40]           ; args, (hd . rest) | nil
    cmp rax, 0
    je .break

    mov rdi, [rbp-40]           ; (hd . rest)
    call sexp_car               ; hd

    mov rdi, [rbp-8]            ; asm*
    mov rsi, rax                ; (type . value): sexp* as hd
    mov rdx, 1                  ; size
    call asm_write_node

    mov rdi, [rbp-40]           ; (hd . rest)
    call sexp_cdr               ; rest
    mov [rbp-40], rax

    jmp .inst_db_loop


.inst_dq:
    jmp .break


.inst_mov:
    mov rdi, [rbp-8]            ; asm
    mov rsi, [rbp-32]           ; args
    call asm_write_inst_mov
    jmp .break

.break:
    leave
    ret


;;; rdi: asm*
;;; rsi: args
asm_write_inst_mov:
    push rbp
    mov rbp, rsp
    sub rsp, 48

    mov qword [rbp-48], 0       ; 2nd-arg: type : u64
    mov qword [rbp-40], 0       ;        : value: sexp*
    mov qword [rbp-32], 0       ; 1st-arg: type : u64
    mov qword [rbp-24], 0       ;        : value: sexp*

    mov [rbp-16], rsi           ; args: sexp*
    mov [rbp-8], rdi            ; asm*

    ;; TODO: check-length

    ;; 1st
    mov rdi, [rbp-16]           ; (hd . rest)
    call sexp_car               ; hd: (type . value)
    mov rdi, rax
    lea rsi, [rbp-32]           ; 1st-arg
    call asm_decode_arg

    mov rdi, [rbp-16]           ; (hd . rest)
    call sexp_cdr               ; rest
    mov [rbp-16], rax

    ;; 2nd
    mov rdi, [rbp-16]           ; (hd . rest)
    call sexp_car               ; hd
    mov rdi, rax
    lea rsi, [rbp-48]           ; 2nd-arg
    call asm_decode_arg

    ;; args[1] inspect
    mov rax, [rbp-32]
    and rax, 0x000000000000ffff ; type-tag

    cmp rax, 1                  ; register
    jmp .arg_1_reg

    mov rdi, 10                 ; debug
    call runtime_exit

.arg_1_reg:
    ;; args[2] inspect
    mov rax, [rbp-48]
    and rax, 0x000000000000ffff ; type-tag

    cmp rax, 1                  ; register
    jmp .encode_mr

    mov rdi, 11                 ; debug
    call runtime_exit

    ;; MOV r/m, r
.encode_mr:
    mov rdi, 0x48               ; REX.W
    call asm_write_u8

    mov rdi, 0x89               ; MOV r/m, r
    call asm_write_u8

    mov rax, [rbp-32]           ; args[1]
    mov rax, 16                 ; 2 * 8 bits
    and rax, 0x000000000000ffff ; register-index
    mov rdi, rax                ; ModR/M, effective-reg

    mov rax, [rbp-48]           ; args[2]
    shr rax, 16                 ; 2 * 8 bits
    and rax, 0x000000000000ffff ; register-index
    mov rsi, rax                ; ModR/M, /r-digit

    call asm_write_mod_rm_reg

    leave
    ret



;;; rdi: effective-reg
;;; rsi: /r
asm_write_mod_rm_reg:
    and rdi, 0x07               ; 0b00000111, RM
    or rax, rdi

    and rsi, 0x07               ; 0b00000111, REG
    shl rsi, 3                  ; 3bits
    mov rax, rsi                ; 0b00111000, REG

    or rax, 0xc0                ; 0b11000000, Mod

    mov rdi, rax
    call asm_write_u8

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
;;; rsi: sexp*
;;; rdx: u64 byte-size
asm_write_node:
    push rbp
    mov rbp, rsp
    sub rsp, 24

    mov [rbp-24], rdx           ; byte-size
    mov [rbp-16], rsi           ; (type . value): sexp*
    mov [rbp-8], rdi            ; asm*

    mov rdi, rsi                ; (type . value): sexp*
    call sexp_car               ; type

    mov rdi, rax
    call sexp_internal_int_value ; value-tag
    and rax, 0x000000000000ffff

    cmp rax, 3                  ; string
    je .write_sexp

    cmp rax, 4                  ; int
    je .write_sexp

    ;; Failed
    mov rdi, str_ice_invalid_node
    call runtime_print_string

    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.write_sexp:
    mov rdi, [rbp-16]           ; (type . value): sexp*
    call sexp_cdr               ; value

    mov rdi, rax
    mov rsi, [rbp-24]           ; byte-size
    call asm_write_sexp

.break:
    leave
    ret

;;; rdi: sexp*
;;; rsi: u64 byte-size
asm_write_sexp:
    push rbp
    mov rbp, rsp
    sub rsp, 32

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
;
    mov rdi, rax                ; int-value
    mov rsi, [rbp-16]           ; byte-size
    call asm_write_value
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
asm_write_value:
    cmp rsi, 1
    je .u8

    ;; Failed
    mov rdi, str_ice_unsupported_size
    call runtime_print_string

    call runtime_print_newline

    mov rdi, 1
    call runtime_exit

.u8:
    call asm_write_u8
    ret


;;; rdi: u8
asm_write_u8:
    mov rax, g_asm_buffer
    mov rcx, [g_asm_buffer_size]
;
    add rax, rcx
    mov rdx, rdi
    mov byte [rax], dl

    add rcx, 1
    mov [g_asm_buffer_size], rcx

    ret

;;;
;;; struct value {
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
    ;; --< bss

code_segment_rest_size: equ $-code_segment_begin
    align segment_align
code_segment_end:

    ;; Data
data_segment_begin:

    ;; --> bss
section_bss_begin:

ret_code:   dq 42

g_code_buffer:  times app_max_code_buffer_size db 0
g_code_size:    dq 0

g_asm_buffer:  times app_max_asm_buffer_size db 0
g_asm_buffer_size:  dq 0

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

str_reg_rax:    db "rax", 0
str_reg_rcx:    db "rcx", 0
str_reg_rdx:    db "rdx", 0
str_reg_rbx:    db "rbx", 0
str_reg_rsp:    db "rsp", 0
str_reg_rbp:    db "rbp", 0
str_reg_rsi:    db "rsi", 0
str_reg_rdi:    db "rdi", 0

str_size_qword: db "qword", 0

str_ice_invalid_statement:  db "ICE: Invalid statement", 0
str_ice_invalid_inst:       db "ICE: Invalid inst", 0
str_ice_invalid_type:       db "ICE: Invalid type", 0
str_ice_invalid_node:       db "ICE: Invalid node", 0
str_ice_unsupported_size:   db "ICE: Unsupported size", 0

str_inst_bits:  db "bits", 0
str_inst_db:    db "db", 0
str_inst_dq:    db "dq", 0
str_inst_mov:   db "mov", 0

str_debug_arrow_r:  db "->", 0
str_debug_finished: db "[DEBUG] finished", 0
str_debug_failed_to_parse_addr: db "[DEBUG] Failed to parse addr", 0

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
