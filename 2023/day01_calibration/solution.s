    .global main

    .text

main:
    push %rbp
    mov %rsp,%rbp

    # dump input on stack
    sub $0x8000,%rsp
    mov $0,%rdi                         # stdin
    mov %rsp,%rsi                       # buf
    mov $0x8000,%rdx                    # count
    call read                           # rax <- input len

    mov %rsp,%r13
    add %rax,%r13                       # r13 <- end of input

    mov %rsp,%r12                       # start of input
    call calibration_values

    mov $0, %rax
    leave
    ret

calibration_values:
    mov $0,%r14d                        # r14d: sum

new_line:
    mov $0x0,%r8                        # r8: first digit <- 0
    mov $0x0,%r9                        # r9: last digit <- 0

next_char:
    cmp %r12, %r13
    je eof

    mov (%r12),%sil                     # sil <- current char
    add $1,%r12

    cmp $0xa,%sil
    jne continue_char

eol:
    mov %r8w,%ax
    mov $10,%dl
    mul %dl
    add %eax,%r14d                      # sum += 10 * first digit
    add %r9d,%r14d                      # sum += last digit
    jmp new_line
continue_char:

    # skip non-digits
    cmp $0x31,%sil
    jl next_char
    cmp $0x39,%sil
    jg next_char
    sub $0x30,%sil

save_digit:
    cmp $0x0,%r8b
    cmove %si,%r8w                      # save first digit if unset
    mov %sil,%r9b                       # save last digit

    jmp next_char

eof:
    lea fmt(%rip),%rdi
    mov %r14d,%esi
    xor %eax,%eax                       # no vec varargs
    call printf
    ret

    .section .rodata

fmt:
    .asciz "%u\n"
