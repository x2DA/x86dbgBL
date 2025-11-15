org 0x200
VGATmem equ 0xb800 ; VGA text mode buffer start
screendim equ 77fh ; 24*80-1

xor ax, ax
xor di, di
xor si, si

mov ds, ax
mov es, ax
mov ss, ax

mov ax, 0x200
mov sp, ax
mov bp, sp
xor ax, ax

main:
	
	 ; mov si, teststr
	 ; mov ah, 47h
	 ; mov bx, 3h
	 ; call write_line

	 ; mov al, 40h ; @
	 ; mov ah, 1ch
	 ; mov bx, 0002h
	 ; call write_char

	mov al, 20h ; ' '
	mov ah, 07h
	call fill_screen

	mov dh, 00h
	mov dl, 00h
	call set_cursor

	jmp force_shutdown



jmp main


; Expects si(strZ address), bx(offset from top left), ah(bg|fg)
; Offset shouldn't be a power of two
write_line:
	push ax
	push bx
	push si

	call set_es_to_vidmem

	shl bx, 1
	.loop:
		mov al, [si]
		cmp al, 0
		je .done
		mov [es:bx], ax

		inc si
		add bx, 2
	jmp .loop

	.done:
	pop si
	pop bx
	pop ax
ret


; Expects ax([bg|fg],[char])
fill_screen:
	push bx
	push cx
	push es

	call set_es_to_vidmem

	mov bx, 0
	.loop:
		cmp bx, screendim
		jge .done

		mov [es:bx], ax
		add bx, 2
	jmp .loop

	.done:
	pop es
	pop cx
	pop bx
ret


; Expects ah(bg|fg) al(char), bx(offset from top left)
; The offset should not be a power of two, we account for that :)
write_char:
	push bx

	call set_es_to_vidmem

	shl bx, 1

	mov [es:bx], ax
	pop bx
ret


; Expects: dh(row) dl(col) (starting top left)
set_cursor:
	push ax
	push bx

	mov ah, 0x02 ; Set cursor position
	mov bh, 0 ; Page number
	int 0x10

	pop bx
	pop ax
ret


set_es_to_vidmem:
	push ax
	mov ax, VGATmem
	mov es, ax
	pop ax
ret



jmp main

teststr db "Teststr", 0

force_shutdown:
	; int 15, ax 5307h (apm state)
	; bx (Device ID) cx (System State ID)

	mov ax, 5307h
	mov bx, 0001h ; All
	mov cx, 0003h ; Off
	int 15

	; Support for APM 1.0, where SysID OFF not supporting DevID ALL

	; Re-set ah since the err code is written there, al and cx shouldn't be modified
	mov ah, 53h;
	mov bx, 02ffh ; storage (all secondary)
	int 15

	mov ah, 53h
	mov bx, 01ffh ; display (all)
	int 15

	mov ah, 53h
	mov bx, 04ffh ; serial ports (all)
	int 15

	mov ah, 53h
	mov bx, 03ffh ; parallel ports (all)
	int 15

	cli
	halt_cpu:
		hlt
	jmp halt_cpu

jmp force_shutdown


times 9216 - ($-$$) db 0 ; Pad out remaining 18 sectors

