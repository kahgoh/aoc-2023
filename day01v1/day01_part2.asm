default rel

section .rodata
    one: db "one", 0
    two: db "two", 0
    three: db "three", 0
    four: db "four", 0
    five: db "five", 0
    six: db "six", 0
    seven: db "seven", 0
    eight: db "eight", 0
    nine: db "nine", 0

section .text
global copy_to_buf
copy_to_buf:
    ; Argument 1 (rdi) is the source
    ; Argument 2 (rsi) is the destination buffer

    ; Register rbx as the index
    xor rbx, rbx

.start_loop:
    mov al, byte [rdi + rbx]
    
    test al, al
    jz .end_loop

    mov byte [rsi + rbx], al
    inc rbx
    jmp .start_loop

.end_loop:
    mov byte [rsi + rbx], 0
    ret

global sub_number
sub_number:
    ; Argument 1 (rdi) points to source string (also the destination)
    ; Argument 2 (rsi) points to the string to look for buffer
    ; Argument 3 (rdx/dl) contains the replacement byte
    
    ; Use rbx as index (set to 0)
    xor rbx, rbx

    ; Use r12 to store index of target letter
    ; Zero r12
    xor r12, r12

    ; Use r13 to load the char from the source string
    xor r13, r13

    ; Use r14 to store the character of interest
    xor r14, r14

    ; Want always start the matching from the first char
    mov r14b, byte[rsi]

.start_loop:
    ; Load next byte from the source string
    mov r13b, byte [rdi + rbx]

    ; Null, end of string
    test r13b, r13b
    jz .exit_loop

    ; Check the byte to see if still matching the string
    cmp r13b, r14b
    jne .no_match

    ; If here, found a match
    ; Check if entire string has been found
    inc r12
    mov r14b, byte [rsi + r12]
    test r14b, r14b
    jnz .continue_loop

    ; If here, the entire word has matched
    ; Replace the byte
    mov byte [rdi + rbx - 1], dl

    ; Reset the r12 index
    xor r12, r12
    mov r14b, byte [rsi]

    jmp .continue_loop

.no_match:
    ; Look for the first byte again
    xor r12, r12
    mov r14b, byte [rsi]
    jmp .continue_loop

.continue_loop:
    inc rbx
    jmp .start_loop

.exit_loop:
    ret

global find_number2
%macro replace_word 2
    lea rsi, %1
    mov dl, %2
    call sub_number
%endmacro
find_number2:
    ; Argument 1 (rdi) is the source string
    ; Argument 2 (rsi) is pointer to buffer, for temporary workspace

    ; Copy initial contents to the buffer
    call copy_to_buf
    
    ; Now move rsi to rdi (the rest works on the same buffer)
    ; This makes the buffer the first argument for calls to sub_number
    mov rdi, rsi

    replace_word one, '1'
    replace_word two, '2'
    replace_word three, '3'
    replace_word four, '4'
    replace_word five, '5'
    replace_word six, '6'
    replace_word seven, '7'
    replace_word eight, '8'
    replace_word nine, '9'

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

global sum_numbers2
sum_numbers2:
    ; Argument 1 (rdi) is the array of strings
    ; Argument 2 (rsi) is a temporary work buffer

    ; Push a zero value to enable summation
    push 0

.start_sum:
    mov r14, [rdi]
    test r14, r14
    jz .end_sum

    ; Push copy of start to free up the register for
    ; making a further call
    push rdi

    ; Also a copy of the second argument?
    push rsi

    ; Now load the start of the next string in the array
    mov rdi, r14

    call find_number2

    ; Restore the rsi and rdi to get back where we were up to
    ; in the array
    pop rsi
    pop rdi

    ; Update sum
    pop r15
    add rax, r15
    jo .overflow

    test rax, rax
    js .overflow

    push rax

    add rdi, 8
    jmp .start_sum

.end_sum:
    pop rax
    ret

.overflow:
    mov rax, -1
    ret

%ifidn __OUTPUT_FORMAT__,elf64
section .note.GNU-stack noalloc noexec nowrite progbits
%endif
