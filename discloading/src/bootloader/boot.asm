org 0x7C00 ;set initial memory offset 0x7C00
bits 16 ;tell nasm to output 16-bit instructions

%define ENDLINE 0x0D, 0x0A

;
; FAT12 header
;

; jump over disk format information (BPB, BIOS parameter block, and EBPB) see https://wiki.osdev.org/FAT
; more on FAT https://www.win.tue.nl/~aeb/linux/fs/fat/fat-1.html

jmp short start
nop                                     ; 3 bytes

;fat bpb

bpb_oem:                  db 'MSWIN4.1' ; 8 bytes
bpb_bytes_per_sector:     dw 512        ; 2 bytes
bpb_secs_per_cluster:     db 0x1        ; 1 byte
bpb_reserved_secs:        dw 0x1        ; 2 bytes
bpb_fat_count:            db 0x2        ; 1 byte
bpb_root_dir_count:       dw 0xe0       ; 2 bytes
bpb_total_sec_count:      dw 2880       ; 2 bytes (512 bytes per sector, 1.44mb floppy size)
bpb_media_descriptor:     db 0xf0       ; 1 byte, set media type to F0 for 3.5" floppy
bpb_sectors_per_fat:      dw 0x9        ; 2 bytes
bpb_sectors_per_track:    dw 0x12       ; 2 bytes, 18 sectors per track
bpb_head_count:           dw 0x2        ; 2 bytes, number of heads "sides" on storage media
bpb_hidden_sec_count:     dd 0x0        ; 4 bytes
bpb_large_sec_count:      dd 0x0        ; 4 bytes
  
;fat ebdb

ebpb_drive_number:        db 0x0        ; 1 byte
                          db 0x0        ; 1 byte, reserved byte
ebpb_signature:           db 0x29       ; 1 byte
ebpb_volume_id:           db 0xbe, 0xba, 0xfe, 0xca ; 4 bytes, "serial number", can be anything
ebpb_volume_label:        db 'HarmonyOS  ' ; 11 byte field, can be anything, pad with spaces if too short
ebpb_system_id:           db 'FAT12   ' ; 8 byte FAT file system type, spec says never trust this field

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
    ;s etup data segment
    mov ax, 0       ; can't write constant to ds/es directly
    mov ds, ax      ; set ds (data segment reg) to 0
    ; mov es, ax      ; set es (extra segment reg) to 0
    
    ; setup stack segment
    mov ss, ax      ; set ss (stack segment reg) to 0
    mov sp, 0x7C00  ; stack will grow downward from where we are in memory
                    ; we set the stack to the start of our Operating System because it grows downwards -
                    ; so we don't overwrite our code

    ; read something from disk
    ; BIOS should set DL to drive number

    mov [ebpb_drive_number], dl ; move value of dl into ebpb_drive_number
    
    mov ax, 1                  ; LBA 1 - read second sector of disk
    mov cl, 1                  ; 1 sector to read
    mov bx, 0x7E00             ; store data read from disk at first address after boot sector (0x7c00 + 512 bytes = 0x7E00)
    call disk_read

    mov si, msg_hello
    call puts
    
    hlt

;
; Display an error if the disk read operation fails
;

floppy_error:
    mov si, msg_read_failed
    call puts
    jmp wait_key_and_reboot

;
; Wait for a key to be pressed and reboot system if that occurs
;

wait_key_and_reboot:
    mov ah, 0
    int 16h                     ; wait for keypress
    jmp 0xFFFF:0x0000           ; jump to beginning of bios - should reboot system

.halt:
    cli                         ; cli disables interrupts - not sure why we do this....
    hlt

;
; Convert LBA (Logical Block Addressing) to CHS (Cylinder, Head, Sector) address
; Parameters:
;   - ax: LBA address
; Returns:
;   - cx (bits 0-5): sector number
;   - cx (bits 6-15): cylinder
;   - dh: head
;

lba_to_chs:

    push ax
    push dx
    
    ; get sector

    xor dx, dx                        ; set dx to 0
    
    ; div - number to be divided (dividend) stored in ax, remainder stored in dx
    div word [bpb_sectors_per_track]  ; ax = LBA / Sectors per track
                                      ; dx = LBA % Sectors per track
    inc dx                            ; dx = LBA % Sectors per track + 1 (lba value is index 0, but sectors start at 1, so for example, if a cylinder has 63 sectors, and the LBA value is 63, you do 63 % 63 which gives you remainder of 0, but you need to add 1 as you are now on second head, head "1")
    mov cx, dx                        ; move remainder into cx

    ; get head

    xor dx, dx
    div word [bpb_head_count]         ; ax = (LBA / Sectors per track) / Heads = cylinder
                                      ; dx = (LBA / Sectors per track) % Heads = head                       
    ; move results into correct registers
    
    mov dh, dl                        ; BIOS expects head in dh, upper 8 bits of dx
    mov ch, al                        ; ch = lower 8 bits of cylinder
    shl ah, 6                         ; shift ah left 6 bits
    or  cl, ah                        ; combine upper 2 bits of cylinder with sector (which is 6 bits)

    pop ax                            ; restore value of dx into ax (using ax as an intermeditary to restore only dl in next instruction)
    mov dl, al                        ; upper 8 bits of dx, dh, contains head, so only restore dl                      
    pop ax                            ; restore ax to original value before function was called
    ret                               ; return

;
; Reads sectors from a disk
; Parameters:
;   - ax: LBA address
;   - cl: number of sectors to read (max 128)
;   - dl: drive number
;   - es:bx: memory address where to store read data
;
; More info here - https://www.stanislavs.org/helppc/int_13.html
;

disk_read:

    push ax                           ; save values of registers that will be modified by the disk read operation
    push bx
    push dx
    push di

    ; lba_to_chs() modifys value of cx, so we push cx onto the stack
    push cx                           ; cl contains number of sectors we want to read, so we're also pushing cx to save this value (limitation of x86 is that we cannot do push/pop operations on 8-bit registers)
    call lba_to_chs                   ; compute CHS address
    pop ax                            ; interrupt handler expects al to be set to the number of sectors we want to read, so here we pop cx into ax - al will now be the value of cl 

    mov ah, 0x2                       ; set disk interrupt service "read sectors"
    mov di, 3                         ; retry count

.retry:
    pusha                             ; pusha pushes value of all registers - we don't know what registers BIOS will modify
    stc                               ; stc = "set carry" - not all BIOS's set carry flag so we set it here
    int 0x13                          ; carry flag cleared = success (interrupt handler sets this)
    jc .done                         ; "jump no carry"

    ;read failed
    popa
    call disk_reset                   ; reset disk controller

    dec di                            ; decrement di   
    test di, di                       ; check di
    jnz .retry                        ; if di not yet zero retry operation

.fail:
    ; all attempts to read from disk are exhausted, boot failed :(
    jmp floppy_error

.done:
    popa                              ; restore all values

    pop di
    pop dx
    pop bx
    pop ax
    ret

;
; Reset disk controller
; Parameters:
;   - dl: drive number
;
disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret

msg_hello:              db 'Hello World', ENDLINE, 0
msg_read_failed:        db 'Error - the disk could not be read...', ENDLINE, 0

times 510-($-$$) db 0 ;fill remainder of bytes with 0x00
dw 0AA55h ;delcare last two bytes to be AA55 