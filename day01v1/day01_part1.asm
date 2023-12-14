section .text
global find_number
find_number:
    ; Input (rdi) is pointer to the string

    ; Zero the return, rax
    mov rax, 0

    ; Use register rbx for index
    mov rbx, 0
    
    ; Other registers for use
    mov r12, 0
    mov r13, 0

.start_search_1_loop:
    ; load the next character
    mov al, byte [rdi + rbx]
    cmp al, 0
    je .begin_second_search

    cmp al, '0'
    jl .continue_search_1
    cmp al, '9'
    jg .continue_search_1

    ; Subtract '0' to get actual digit
    sub al, '0'

    ; Keep a copy for the second digit (in case there is only one digit in the string)
    mov r12b, al

    ; Multiply by ten, result will be left in rax register
    ; Multiplication requires additional register
    mov edx, 10
    mul edx
    jmp .begin_second_search

 .continue_search_1:
    inc rbx
    jmp .start_search_1_loop

.begin_second_search:
    inc rbx

.start_search_2_loop:

    ; search for the second digit
    mov r13b, byte [rdi + rbx]
    cmp r13b, 0
    je .end_search_2

    cmp r13b, '0'
    jl .continue_search_2
    cmp r13b, '9'
    jg .continue_search_2

    ; Found the second digit.
    sub r13b, '0'
    
    ; Copy to r12 and continue looking through the string
    mov r12b, r13b

.continue_search_2:
    inc rbx
    jmp .start_search_2_loop

.end_search_2:
    ; Here means there is only one digit in the string
    add rax, r12
    ret

.error:
    mov rax, 99
    ret

global first_of_last:
first_of_last:
    xor rax, rax

.start_loop:
    mov r12, [rdi]

    test r12, r12
    jz .end_loop

    movzx rax, byte [r12]

    add rdi, 8
    jmp .start_loop

.end_loop:
    ret

global sum_numbers
sum_numbers:
    ; Push a zero value to enable summation
    push 0

.start_sum:
    mov r14, [rdi]
    test r14, r14
    jz .end_sum

    ; Push copy of start
    push rdi

    ; Now load the start of the next string
    mov rdi, r14

    call find_number

    ; Restore rdi
    pop rdi

    ; Update sum
    pop r15
    add rax, r15
    push rax

    add rdi, 8
    jmp .start_sum

.end_sum:
    pop rax
    ret


%ifidn __OUTPUT_FORMAT__,elf64
section .note.GNU-stack noalloc noexec nowrite progbits
%endif
