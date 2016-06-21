; Copyright (C) 2016  Piotr Janczyk
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.


[bits 16]
[org 0x0]

%define START_SEGMENT 0x07c0
%define SCREEN_WIDTH 80
%define SIZE 31 ; size of the spiral

%define CHAR_HORIZONTAL 0
%define CHAR_VERTICAL 1
%define CHAR_CORNER 2
%define CHAR_CENTER 3
%define CHAR_P 4
%define CHAR_J 5

; Table that maps n'th element of the spiral
;   with its offset in video memory
; 434 words, real adresses: [0x8C00 - 0x8F63]
%define lookup 0x1000 

aaa ; AAAAAA!

jmp word START_SEGMENT:start

;--------;
;  MAIN  ;
;--------;
start:

SETUP_SEGMENTS:
    ; DATA SEGMENT, EXTRA SEGMENT
    ; from 0x7c00
    mov ax, START_SEGMENT
    mov ds, ax
    mov es, ax
      
    ; STACK SEGMENT
    ; from 0x1F000
    mov ax, 0x1F00
    mov ss, ax
    xor sp, sp ; sp := 0
  
SETUP_GRAPHIC:
    ; SWITCH GRAPHIC MODE - Int 10 / AH=00h
    ;   al=0x03 - mode 3
    mov ax, 0x0003
    int 0x10
      
    ; SET 9/8 DOT MODE - 8 dots per character
    mov ax, 0x0101
    mov dx, 0x03C4
    out dx, ax
    
    ; SET CURSOR POSITION - Int 10 / AH=02h
    ;   bh=0x0  - page number
    ;   dh=0xff - row (outside of the screen)
    ;   dl=0xff - column
    mov ah, 0x02
    xor bx, bx
    mov dx, 0xffff
    int 0x10
    
    ; LOAD DEFAULT ROM 8x8 FONT - Int 10 / AX=1112h
    mov ax, 0x1112
    int 0x10
        
    ; LOAD FONT - Int 10 / AX=1110h
    ;   ES:BP=0x07c0:font - pointer to font source
    ;   BH=08h - bytes per character
    ;   BL=00h - table in character generator
    ;   CX=0006h - number of characters to load
    ;   DX=0000h - offset into RAM font area
    mov al, 0x10 ; AH is already set to 11h
    mov bp, font
    mov bh, 0x08 ; BL is already set to 00h
    mov cx, 0x0006
    xor dx, dx
    int 0x10
    
    ; SET PALETTE REGISTER - Int 10 / AX=1000h
    ;   BH - color value
    ;   BL - paletter register
    ; set palette registers to 0x0..0xF
    mov ax, 0x1000
    mov bx, 0x0f0f
    .loop:
    int 0x10
    sub bx, 0x0101
    jge .loop
    
    ; SET BLOCK OF DAC COLOR REGISTERS - Int 10 / AX=1012h
    ;   BX=0x0000 - first color register to set
    ;   CX=0x0010 - number of color register to set
    ;   ES:DX=0x07C0:colors - pointer to source
    mov al, 0x12 ; AH is already set to 10h
    xor bx, bx
    mov cl, 0x10 ; CH is already set to 00h
    mov dx, colors
    int 0x10

SETUP_SEGMENTS_2:
    ; VGA SEGMENT
    ; from 0xB8000
    %define VGA es
    mov ax, 0xB800
    mov VGA, ax

WRITE_TEXT:
    mov cx, (text_end-text)
    mov si, text
    mov di, 7804
    mov al, 0x05
    .loop:
    movsb
    stosb
    loop .loop
   
GENERATE_SPIRAL:
    ; al - DIRECTION (0..3)
    %define INDEX cx ; - INDEX (0..433)
    %define X si ; - X coordinate (0..SIZE-1)
    %define Y di ; - Y coordinate (0..SIZE-1)

    xor X, X ; X := 0
    xor Y, Y ; Y := 0
    mov al, byte CHAR_CORNER
    call $set_character ; set character at (0,0) to CHAR_CORNER
    
    xor ax, ax ; DIRECTION := 0
    xor INDEX, INDEX ; INDEX := 0

    loop:
    ;call $wait
      
    pusha ; push X and Y, it will be poped:
          ;  - at the beginning of .continue, OR
          ;  - at the beginning of .next_dir
    
    cmp al, byte 1
    jl .right ; DIRECTION al == 0
    je .down ; DIRECTION == 1
    cmp al, byte 3
    je .up ; DIRECTION == 3
    ; else: DIRECTION == 2

    .left:
        dec X
        dec X ; newX = X-2
        ; if X-2 < 0
        jl .next_dir 
                
        call $cmp_char_space
        ; if char at (X-2, Y) != 0x20
        jne .next_dir 
        
        inc X ; newX = X-1
        jmp .continue
  
    .right:
        inc X
        inc X ; newX = X+2
        cmp X, (SIZE - 1)
        ; if X+2 == SIZE-1
        je .next_dir 
        
        call $cmp_char_space
        ; if char at (X+2, Y) != 0x20
        jne .next_dir
        
        dec X ; newX = X+1
        jmp .continue
  
    .down:
        inc Y
        inc Y ; newY = Y+2
        cmp Y, (SIZE - 1)
        ; if Y+2 == SIZE-1
        je .next_dir 
        
        call $cmp_char_space
        ; if char at (X, Y+2) != 0x20
        jne .next_dir
        
        dec Y ; newY = Y+1
        jmp .continue
  
    .up:
        dec Y
        dec Y ; newY = Y-2
        ; if Y-2 < 0
        jl .next_dir 
                
        call $cmp_char_space
        ; if char at (X, Y-2) != 0x20
        jne .next_dir 
        
        inc Y ; newY = Y-1
        jmp .continue
  
  
    .next_dir:
        popa ; restore previous X and Y = X, dx = Y
        
        push ax
        mov al, byte CHAR_CORNER
        call $set_character ; char at (X, Y) = CHAR_CORNER
        pop ax ; restore al = DIRECTION
        
        inc al ; DIRECTION++
        and al, 11b ; DIRECTION %= 4
               
        jmp loop
  
  
    .continue:
        add sp, 16 ; undo pusha
        
        push ax
        and al, 01b
        call $set_character ; char at (X, Y) = DIRECTION mod 2
        pop ax
        
        call $set_lookup
        
        inc INDEX ; INDEX++
        cmp INDEX, 434
        jl loop ; INDEX < 434
            
    mov al, CHAR_CENTER
    call $set_character ; set the center character
        
ANIMATION:
    mov dx, 0x0f01
    .animation_begin:
    xor cx, cx ; cx - index
    .loop:
        call $wait
        call $get_offset_of ; bx = offset
        mov byte [VGA:bx+1], 0x7F ; set background to 0x7 and foreground to 0xF
        
        push cx
        mov ax, cx
        sub ax, 2*7 ; dx = index - 14
        .loop2:
            sub cx, 2
            js .loop2_end ; cx < 0, don't do anything
            call $get_offset_of ; bx = offset
            sub byte [VGA:bx+1], 0x11
            cmp cx, ax
            jne .loop2
        .loop2_end:
        
        .change_background:
            ; SET DAC COLOR REGISTER
            mov ax, 0x1010
            mov bx, 8 ; color register        
            xor cx, cx ; green, blue value
            int 0x10
            
            add dx, si
            
            cmp dh, 15
            jg .greater
            mov si, 0x40          
            .greater:
            cmp dh, 30
            jl .less
            mov si, (-0x40)
            .less:
        
        pop cx
        inc cx
        cmp cx, 450
        jl .loop
        
jmp .animation_begin
        

;-------------;
;  FUNCTIONS  ;
;-------------;


; Loads into bx an offset of cx'th element of the spiral
; If index is out of range (index < 0 or index > 434) returns offset out of range
;
; INPUT:
;   cx - index of an element of spiral
; OUTPUT:
;   bx - offset of the element
$get_offset_of:
    cmp cx, 0
    jl .out_of_range
    cmp cx, 434
    jge .out_of_range
    mov bx, cx ; bx = index
    shl bx, 1 ; bx = 2*index
    mov bx, word [lookup+bx] ; bx = [lookup+2*index] = offset
    ret
    .out_of_range:
    mov bx, word 8000 ; 80x40*2 - outside of the screen
    ret


; Calculates offset in VRAM of character at (X, Y)
;
; INPUT:
;   si - X
;   di - Y
; OUTPUT:
;   bx - calculated offset
$calculate_char_offset:
    push ax
    mov ax, Y ; ax = Y
    mov bx, SCREEN_WIDTH ; bx = SCREEN_WIDTH
    mul bx ; ax = Y * SCREEN_WIDTH
    add ax, X ; ax = Y * SCREEN_WIDTH + X
    add ax, 10 * SCREEN_WIDTH + 25 ; center the spiral
    shl ax, 1 ; each character consist of 2 bytes
    mov bx, ax ; bx = offset
    pop ax
    ret
    

; Compares character at (X, Y) with 0x20.
; 
; INPUT:
;   si - X
;   di - Y
$cmp_char_space:
    pusha ; push bx
    call $calculate_char_offset ; bx = offset
    cmp [VGA:bx], byte 0x20
    popa ; pop bx
    ret

; Sets character at (X, Y) to AL, and its color to 0x08
;
; INPUT:
;   al - character to be set 
;   si - X
;   di - Y
$set_character:
    pusha ; push bx
    call $calculate_char_offset ; bx = offset
    mov byte [VGA:bx], al
    mov byte [VGA:bx+1], 0x08
    popa ; restore bx
    ret

; INPUT:
;   si - X
;   di - Y
;   cx - INDEX
$set_lookup:
    pusha
    call $calculate_char_offset ; bx = offset
    mov ax, bx ; ax = offset
    mov bx, INDEX ; bx = index
    shl bx, 1
    mov word [lookup+bx], ax ; [lookup+index] = offset
    popa ; restore ax, bx
    ret

; Waits approximately 56 ms (1/18.2 s)
$wait:
    pusha
    mov ah, 0x0
    int 0x1A ; cx - high order word of tick count
	    ; dx - low order word of tick count
    mov bx, dx
    ;shr bx, 1
    .loop:
    int 0x1A
    ;shr dx, 1
    cmp bx, dx
    je .loop
    popa
    ret


;-------------;
;  RESOURCES  ;
;-------------;
font:
    ; CHAR_HORIZONTAL - 0
    db 00000000b
    db 00000000b
    db 11111111b
    db 11111111b
    db 11111111b
    db 11111111b
    db 00000000b
    db 00000000b
    ; CHAR_VERTICAL - 1
    db 00111100b
    db 00111100b
    db 00111100b
    db 00111100b
    db 00111100b
    db 00111100b
    db 00111100b
    db 00111100b
    ; CHAR_CORNER 2
    db 00111100b
    db 01111110b
    db 11111111b
    db 11111111b
    db 11111111b
    db 11111111b
    db 01111110b
    db 00111100b
    ; CHAR_CENTER 3
    db 00000000b
    db 00000000b
    db 11111110b
    db 11111111b
    db 11111111b
    db 11111110b
    db 00000000b
    db 00000000b
    ; CHAR_P - 4
    db 11111110b
    db 10000011b
    db 10011001b
    db 10011001b
    db 10000011b
    db 10011110b
    db 10010000b
    db 11110000b
    ; CHAR_J - 5
    db 00011111b
    db 00010001b
    db 00011001b
    db 11111001b
    db 10011001b
    db 10011001b
    db 11000011b
    db 01111110b
.end:

colors:
    ;   R   G   B
    db  8,  0,  0 ; 0
    db 12,  0,  0 ; 1
    db 16,  0,  0 ; 2
    db 20,  0,  0 ; 3
    db 25,  0,  0 ; 4
    db 30,  0,  0 ; 5
    db 35,  0,  0 ; 6
    db 39,  0,  0 ; 7
  
    db 19,  0,  0 ; 8
    db 26,  2,  1 ; 9
    db 33,  4,  2 ; 10
    db 39,  6,  3 ; 11
    db 45,  8,  3 ; 12
    db 51,  9,  4 ; 13
    db 57, 10,  4 ; 14
    db 63, 11,  5 ; 15
    
text:
    db "(C) "
    db CHAR_P
    db "iotr "
    db CHAR_J
    db "anczyk"
text_end:

_BOOT_SIGNATURE:
db 0x55
db 0xAA




