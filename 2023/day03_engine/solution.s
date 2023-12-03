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
    call read

    mov %rsp,%r12                       # r12 <- start of input
    mov %rsp,%r13
    add %rax,%r13                       # r13 <- end of input

    # determine schematic width
    mov %r12,%r14
schematic_width:
    movb (%r14),%al
    add $1,%r14
    cmp $0xa,%al
    jne schematic_width
    sub %r12,%r14                       # r14 <- row length

    call part_numbers

    mov $0, %rax
    leave
    ret

part_numbers:
    mov %r12,%r8                        # r8: current ptr
    mov $0,%rbx                         # rbx: current char
    mov $0,%r9                          # r9 <- part number sum

next_char:
    cmp %r8,%r13
    je eof

    movb (%r8),%bl
    add $1,%r8

    # ignore non-digits
    cmp $0x30,%bl
    jl next_char
    cmp $0x39,%bl
    jg next_char

    mov $0,%rax                         # rax: number value
    mov $0,%r11                         # r11: has symbol?

digit_found:
    # add to number value (num = 10*num + digit)
    imul $10,%eax
    sub $0x30,%bl
    add %rbx,%rax

    # check for symbol nearby
    call symbol_nearby

    cmp %r8,%r13
    je eof
    movb (%r8),%bl
    add $1,%r8

    # abort on non-digits
    cmp $0x30,%bl
    jl end_of_num
    cmp $0x39,%bl
    jg end_of_num

    # look for next digit
    jmp digit_found

end_of_num:
    # add to part number if symbol nearby
    cmp $0,%r11
    je next_char
    add %rax,%r9
    jmp next_char

eof:
    lea fmt(%rip),%rdi
    mov %r9d,%esi
    xor %eax,%eax                       # no vec varargs
    call printf
    ret

symbol_nearby:
    cmp $1,%r11
    je symbol_nearby_done               # symbol already found

top_row:
    # rcx: current ptr, now on char after current digit
    mov %r8,%rcx
    sub %r14,%rcx
    cmp %r12,%rcx
    jl middle_row

    # top right
    call is_symbol

    # top
    sub $1,%rcx
    call is_symbol

    # top left
    sub $1,%rcx
    call is_symbol

middle_row:
    mov %r8,%rcx

    # right
    call is_symbol

    # ignore middle, is digit

    # left, if not on first cell
    sub $2,%rcx
    cmp %r12,%rcx
    jl bottom_row
    call is_symbol

bottom_row:
    mov %r8,%rcx
    add %r14,%rcx
    cmp %r13,%rcx
    jg symbol_nearby_done

    # bottom right
    call is_symbol

    # bottom
    sub $1,%rcx
    call is_symbol

    # bottom left
    sub $1,%rcx
    call is_symbol

    jmp symbol_nearby_done

symbol_nearby_done:
    ret

is_symbol:
    mov (%rcx),%dl

    cmp $0x2e,%dl                       # '.'
    je not_symbol

    cmp $0xa,%dl
    je not_symbol

    # digits
    cmp $0x30,%dl
    jl not_digit
    cmp $0x39,%dl
    jng not_symbol

not_digit:
    mov $1,%r11

not_symbol:
    ret

    .section .rodata

fmt:
    .asciz "%u\n"
