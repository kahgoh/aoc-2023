default rel

section .rodata
    w_one: db "one", 0
    d_one: db "1", 0
    w_two: db "two", 0
    d_two: db "2", 0
    w_three: db "three", 0
    d_three: db "3", 0
    w_four: db "four", 0
    d_four: db "4", 0
    w_five: db "five", 0
    d_five: db "5", 0
    w_six: db "six", 0
    d_six: db "6", 0
    w_seven: db "seven", 0
    d_seven: db "7", 0
    w_eight: db "eight", 0
    d_eight: db "8", 0
    w_nine: db "nine", 0
    d_nine: db "9", 0

section .text
global find_digit
find_digit:
    ; Argument 1 (rdi) points to source string (also the destination)
    ; Argument 2 (rsi) points to the string to look for buffer
    
    ; Initialize the return register rax
    xor rax, rax

    ; Use rbx as index (set to 0)
    xor rbx, rbx

    ; Use r12 to store index of target letter
    ; Zero r12
    xor r12, r12

    ; Use r13 to load the char from the source string
    xor r13, r13

    ; Use r14 to store the current character we are looking for
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
    ; Use bit 16 as flag to store whether the value was encountered.
    bts rax, 16
    ; bts copies the bit to the carry flag register
    jc .copy_upper
 
    ; No low index just yet
    mov ah, bl

.copy_upper:
    ; Copy upper part to al
    mov al, bl
    ; Reset the r12 index
    xor r12, r12
    mov r14b, byte [rsi]

    jmp .continue_loop

.no_match:
    ; Annoying special case - what if we are at double letter for the first letter?
    cmp r12, 1
    jne .not_double_first

    ; Is the letter still the same as the first letter we are trying to find?
    cmp r13b, byte [rsi]
    jne .not_double_first

    ; If this is the case, we are still in the same spot.
    jmp .continue_loop

.not_double_first:
    ; Look for the first byte again
    xor r12, r12
    mov r14b, byte [rsi]
    jmp .continue_loop

.continue_loop:
    inc rbx
    jmp .start_loop

.exit_loop:
    ret

global find_number
%macro check_digit 2
    ; Store rbx and rcx in case of overwrite
    ; rbx tracks the position
    ; r12b tracks the more significant digit
    ; r13b tracks the less significant digit
    push rbx
    push r12
    push r13

    ; Start by finding  positions
    lea rsi, [%1]
    call find_digit

    ; Restore values for reading back
    pop r13
    pop r12
    pop rbx

    bt rax, 16
    ; If carry not set, then number was not found
    jnc .end_check_%1

    ; if ah < bh, then we want to update bh to the new ah

    cmp ah, bh
    jge .check_upper_%1

    ; If we are here, then the digit appears earlier
    ; this is the higher significant digit
    ; Update the first digit, taking care to preserve
    ; the values
    mov bh, ah
    mov r12, %2

.check_upper_%1:
    ; is al >= bl, then we want to update bl to the new al
    ; Please note we also need to it if al == bl to handle the
    ; special case where there is only one digit at the start
    ; of the string (bl is initialized to 0 and al would also be 0)
    cmp al, bl
    jl .end_check_%1

    ; Here, the digit appears later
    ; this is the lower significant digit
    mov bl, al
    mov r13, %2

    mov rax, rbx
    jmp .end_check_%1
.end_check_%1:
%endmacro

find_number:
    ; Argument 1 (rdi) is the source string

    ; b registers for positions
    ; r12b for the more significant digit
    ; r13b for the less significant digit
    xor rbx, rbx
    xor r12, r12
    xor r13, r13

    ; Set bh to max, as that must be the earliest.
    mov bh, 0x7f

    ; Zero bh
    xor bl, bl

    check_digit w_one, 1
    check_digit d_one, 1
    
    check_digit w_two, 2
    check_digit d_two, 2

    check_digit w_three, 3
    check_digit d_three, 3

    check_digit w_four, 4
    check_digit d_four, 4

    check_digit w_five, 5
    check_digit d_five, 5

    check_digit w_six, 6
    check_digit d_six, 6

    check_digit w_seven, 7
    check_digit d_seven, 7

    check_digit w_eight, 8
    check_digit d_eight, 8

    check_digit w_nine, 9
    check_digit d_nine, 9

    ; Work out the "10s" digit
    ; The mul instruction stores the result in rax register
    mov rax, r12

    ; Mul instruction requires a register as argument
    mov r12, 10
    mul r12

    add rax, r13
    ret
 
global sum_numbers
sum_numbers:
    ; Argument 1 (rdi) is the array of strings

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
    ; push rsi

    ; Now load the start of the next string in the array
    mov rdi, r14

    call find_number

    ; Restore the rsi and rdi to get back where we were up to
    ; in the array
    ; pop rsi
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
