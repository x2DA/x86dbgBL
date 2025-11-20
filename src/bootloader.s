org 0x7c00
bits 16

; ----
; CONSTANTS
; ----

VGATmem equ 0xb800 ; VGA Text Mode Buffer Start
scrdim equ 4000d ; (24*80)*2(bytes)
scrwidth equ 80d
bgclear equ 8f20h

; ----
; SETUP
; ----

mov dx, VGATmem
mov es, dx ; Do NOT change ES in any function

xor dx, dx

mov ds, dx
mov ss, dx
mov sp, 0x7c00

; ----
; MAIN
; ----

main:
	; ---- Clear Screen ----
	mov bx, 0h
	mov ax, bgclear
	.loop:
		mov [es:bx], ax
		add bx, 2
	cmp bx, scrdim
	jl .loop
	; ---- Clear Screen ----


	; ---- Draw Cursor ----
	mov bx, [cursor] ; At
	mov ah, [cursor+2] ; Color
	call xy2dto1d
	mov al, '=' ; Char
	shl bx, 1
	mov [es:bx], ax
	shr bx, 1
	; ---- Draw Cursor ----


	; ---- Draw Buffer ----
	mov si, savebuffer+2
	mov bx, [savebuffer]
	call xy2dto1d
	call write_line
	; ---- Draw Buffer ----

	
	; ---- Eval & Draw Buffer Result ----
	mov bx, 0h ; First 4 digit hex
	call dump_to_dx
	mov ax, dx

	mov bx, 6h
	mov cl, [savebuffer+bx] ; Sign

	push cx
	mov bx, 5h ; Second 4 digit hex (+1 for sign)
	call dump_to_dx
	mov bx, dx
	pop cx


	cmp cl, '*'
	je .op_multiply
	cmp cl, '/'
	je .op_divide
	cmp cl, '+'
	je .op_add
	cmp cl, '-'
	je .op_sub
	jmp .op_done

	.op_multiply:
		mul bx
	jmp .op_done
	
	.op_divide:
		div bx
	jmp .op_done
	
	.op_add:
		add ax, bx
	jmp .op_done

	.op_sub:
		sub ax, bx
	jmp .op_done


	.op_done:
	mov bx, 0h

	push dx
	mov dx, ax
	call dump_dx
	pop dx
	inc bx
	call dump_dx
	; ---- Eval & Draw Buffer Result ----

	call handle_user_input
jmp main



; ----
; FUNCTIONS
; ----


; IN [BH]: X hex
; IN [BL]: Y hex
; OUT[BX]: Offset from 00
xy2dto1d:
	push ax
	xor ax, ax
	
	mov al, bl
	mov bl, scrwidth
	mul bl ; AX = y*width

	xor bl, bl
	xchg bl, bh
	add ax, bx ; AX = y*width+x
	mov bx, ax

	pop ax
ret

;messes ax and bx
handle_user_input:
	xor ah, ah
	int 16h ; Read key into AL

	cmp al, '`'
	je .togglemode

	mov ah, [cursor+3]
	cmp ah, 1h ; Jump to writemode if the mode byte is set.
	je .key_writemode

	cmp al, 'h'
	je .key_left
	cmp al, 'j'
	je .key_down
	cmp al, 'k'
	je .key_up
	cmp al, 'l'
	je .key_right
	jmp .key_handled


	.togglemode:
		mov ax, [cursor+2] ; toggle: ah mode, al color
		xor ax, 0117h
		mov [cursor+2], ax

		call .loadcursor
		inc ah
		mov [savebuffer], ax
	jmp .key_handled

	.key_writemode:
		mov bx, [bufferwritten]
		cmp bx, 0ah ; buffersize+1
		jle .inbuffer
			mov bx, 2 ; We're out of the buffer, re-set
		.inbuffer:
		mov [savebuffer+bx], al
		add bx, 1
		mov [bufferwritten], bx
	jmp .key_handled



	.loadcursor:
		mov ax, [cursor]
	ret

	.key_left:
		call .loadcursor
		add ah, 0ffh
	jmp .key_move_handled
	.key_down:
		call .loadcursor
		add al, 01h
	jmp .key_move_handled
	.key_up:
		call .loadcursor
		add al, 0ffh
	jmp .key_move_handled
	.key_right:
		call .loadcursor
		add ah, 01h
	jmp .key_move_handled


	.key_move_handled:
		mov [cursor], ax
	.key_handled:
ret


; IN [SI]: Zero terminated string.
; OUT[SI]: End of zero terminated string.
; IN [BX]: Offset from top left.
; OUT[BX]: Offset strlen+1 of initial offset.
; OUT[AL]: Zero, probably.
write_line:
	shl bx, 1
	.loop:
		mov al, [si]
		cmp al, 0h
		je .done ; Exit if we're at the end

		mov [es:bx], al
		inc si
		add bx, 2h
	jmp .loop
	.done:
	shr bx, 1h

ret


; IN [DHl]: Hex nibble to convert to ASCII.
; IN [DHh]: Zero
; OUT[DH]: ASCII output
nibble2asciihexbyte:
	add dh, 30h
	cmp dh, 3ah
	jl .done
	add dh, 07h
	.done:
ret

; Inverse of the above, OUT's become IN's and vice versa
asciihexbyte2nibble:
	sub dh, 30h
	cmp dh, 0fh
	jl .done
	sub dh, 07h
	.done:
ret


; IN [BX]: Offset from top left.
; OUT[BX]: End of string offset + 1
dump_dx:
	shl bx, 1
	jmp .start

	.dump_nibble:
		call nibble2asciihexbyte

		mov [es:bx], dh
		add bx, 2
	ret

	.start:
	; High DH
	push dx
	and dh, 0xf0
	shr dh, 4 ; dh = 0000xxxx
	call .dump_nibble
	pop dx

	; Low DH
	push dx
	and dh, 0x0f
	call .dump_nibble
	pop dx

	; High DL
	push dx
	and dl, 0xf0
	shr dl, 4
	xchg dh, dl
	call .dump_nibble
	pop dx

	; Low DL
	push dx
	and dl, 0x0f
	xchg dh, dl
	call .dump_nibble
	pop dx

	shr bx, 1
ret



; ou [DHl]: Hex nibble to convert to ASCII.
; ou [DHh]: Zero
; in [DH]: ASCII output

; in, bx, offset from base of first buffer
dump_to_dx:
	mov cx, 0h
	add bx, 2h
	jmp .start

	.dump_ascii:
		mov dh, [savebuffer+bx]
		call asciihexbyte2nibble
		inc bx
	ret


	.start:
	call .dump_ascii
	shl dh, 4
	or ch, dh

	call .dump_ascii
	or ch, dh

	call .dump_ascii
	shl dh, 4
	or cl, dh

	call .dump_ascii
	or cl, dh

	mov dx, cx

ret



; ----
; VARIABLES
; ----

cursor db 00h, 09h, 70h, 00h ; Y,X, Color, Mode,Modifier
; Modes: 0 Normal, 1 Write

savebuffer:
	db 00h
	db 0ah
	times 09h db 2eh
	db 00h
	bufferwritten db 02h

times 510 - ($-$$) db 0 ; Pad rest of sector
dw 0xaa55

