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
    mov $0,%r15                         # ignore spelled digits
    call calibration_values

    mov %rsp,%r12                       # start of input
    mov $1,%r15                         # handle spelled digits
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

    cmp $1,%r15
    jne break_check_spelled

    # check for spelled out digits
    mov $digits,%r11                    # r11: current digit ptr
check_spelled_digit:
    mov %r12,%r10                       # r10: current src ptr
    sub $1,%r10
check_spelled_char:
    cmp %r10,%r13
    je next_digit                       # eof

    mov (%r11),%dl                      # dl <- current digit char
    add $1,%r11

    cmp $0,%dl
    jne continue_digit_char
    sub $digits,%r11
    shr $3,%r11
    mov %r11b,%sil
    add $1,%sil
    jmp save_digit
continue_digit_char:

    mov (%r10),%cl                      # cl <- current src char
    add $1,%r10

    cmp %cl,%dl
    je check_spelled_char

next_digit:
    # go to next digit, aligned by 8
    and $0xf8,%r11b
    add $0x8,%r11

    cmp $digits_end,%r11
    je break_check_spelled

    jmp check_spelled_digit
break_check_spelled:

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

.align 8
digits:
    .ascii "one\0    "
    .ascii "two\0    "
    .ascii "three\0  "
    .ascii "four\0   "
    .ascii "five\0   "
    .ascii "six\0    "
    .ascii "seven\0  "
    .ascii "eight\0  "
    .ascii "nine\0   "
digits_end:
