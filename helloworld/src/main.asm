org 0x7C00 ;set initial memory offset 0x7C00
bits 16 ;tell nasm to output 16-bit instructions

%define ENDLINE 0x0D, 0x0A

start:
    jmp main        ; this ensures main is still the program entry point as whatever code is first will be the first code executed
                    ; I'm doing this as I decided to place the "puts" function above the main function
                    ; and we want the main function executing first

; Prints string to screen
; Params: ds:si points to a string
puts:
    ; save registers we wanna modify, basically function prologue
    push si
    push ax

.loop:
    lodsb           ; lodsb/lodsw/lodsd load a byte, word or dword from ds:si into al/ax/eax then increments si by number of bytes loaded
    or al, al       ; check if next char is null by performing bitwise OR
    jz .done        ; if result of previous OR operation is zero, jump to ".done"

    mov ah, 0x0e
    mov bh, 0x0     ; set "page" number - see https://stackoverflow.com/questions/9591893/page-number-in-bios-interrupts
    int 0x10
    jmp .loop       ; else loop again

.done:
    ; pop values back into registers, basically function epilogue
    pop ax
    pop si
    ret

main:
    ;setup data segment
    mov ax, 0       ; can't write constant to ds/es directly
    mov ds, ax      ; set ds (data segment reg) to 0
    mov es, ax      ; set es (extra segment reg) to 0
    
    ;setup stack segment
    mov ss, ax      ; set ss (stack segment reg) to 0
    mov sp, 0x7C00  ; stack will grow downward from where we are in memory
                    ; we set the stack to the start of our Operating System because it grows downwards -
                    ; so we don't overwrite our code

    mov si, msg_hello
    call puts
    
    hlt

.halt:
    jmp .halt

msg_hello: db 'Hello World', ENDLINE, 0

times 510-($-$$) db 0 ;fill remainder of bytes with 0x00
dw 0AA55h ;delcare last two bytes to be AA55