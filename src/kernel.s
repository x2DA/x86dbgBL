org 0x200
VGATmem equ 0xb800 ; VGA text mode buffer start
screendim equ 77fh ; 24*80-1, starting from the VGATmem

mov ax, 0x200
mov sp, ax
mov bp, sp

xor di, di
xor si, si
xor ax, ax

mov ds, ax
mov es, ax
mov ss, ax
mov bx, ax
mov cx, ax
mov dx, ax

call set_cursor
jmp main

; Data goes here

welcome db "Welcome to 2Sos!", 0
cxtitle db "cx=", 0


jmp main

main:
	; Clear screen
	mov al, 20h ; ' '
	mov ah, 07h
	call fill_screen

	; TODO: Evaluate
	

	; Show
	mov si, welcome
	mov ah, 07h
	mov bx, 0h
	call write_line

	; Testing in prod, ignore.
	;mov si, cxtitle
	;mov bx, 20h ; 1919-10 (for reg+val), dec width times id of reg, where ax=1, bx=2...
	;call dump_cx

	; Wait
	push cx
	mov cx, 200d
	call ms2microseconds
	call sys_wait
	pop cx

	; Loop
jmp main


;	SI (Zero terminated string)
;	BX (Offset from top left, gets set to offset of the last char)
;	AH (BG|FG)
; The offset shouldn't be a power of two, it is accounted for :)
write_line:
	push ax
	push si
	push es

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
	shr bx, 1
	pop es
	pop si
	pop ax
ret


; TODO
;	SI (Zero terminated title)
;	BX (Offset from top left)
dump_cx:
	push ax
	push bx
	push cx
	push si
	push es

	call set_es_to_vidmem

	mov ah, 07h
	call write_line

	mov ah, 1ch
	mov al, 40h
	call write_char

	pop es
	pop si
	pop cx
	pop bx
	pop ax
ret


;	AX ( [(BG|FG)] [Char] )
; Where ( [AH] [AL] )
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


; FIXME: Doesn't work, reads nothing
;	ZF (Gets set if keystroke)
;	AH (Gets set with BIOS scan code)
;	AL (Gets set with ASCII character)
; Non-blocking, overwrites AX, sets ZF
get_key:
	mov ah, 01h
	int 16h
ret


;	AX ( [(BG|FG)] [Char] )
; Where ( [AH] [AL] )
;	BX (Offset from top left)
; The offset shouldn't be a power of two, it is accounted for :)
write_char:
	push bx
	push es
	call set_es_to_vidmem

	shl bx, 1

	mov [es:bx], ax
	pop es
	pop bx
ret


;	DH (row)
;	DL (col)
set_cursor:
	push ax
	push bx

	mov ah, 02h ; Set cursor position
	mov bh, 0h ; Page number
	int 10h

	pop bx
	pop ax
ret


set_es_to_vidmem:
	push ax
	mov ax, VGATmem
	mov es, ax
	pop ax
ret


;	CX (Expects time in ms)
;	[CX:DX] (Gets set with the result in microseconds)
ms2microseconds:
	push ax

	xchg ax, cx ; AX now holds the ms time
	mov cx, 1000d ; 1ms = 1k microseconds
	mul cx ; [DX:AX] now holds the result, move to [CX:DX]
	
	mov cx, dx
	mov dx, ax
	pop ax
ret


;	[CX:DX] (Time to wait in microseconds)
sys_wait:
	mov ah, 86h
	int 15h
ret



jmp main
sys_force_shutdown:
	; int 15, AX 5307h (APM state)
	; BX (Device ID), CX (System State ID)

	mov ax, 5307h
	mov bx, 0001h ; All
	mov cx, 0003h ; Off
	int 15


	; Support for APM 1.0, where SysID OFF not supporting DevID ALL

	; Re-set AH since the error code is written there, AL and CX shouldn't be modified
	mov ah, 53h;
	mov bx, 02ffh ; Storage (all secondary)
	int 15

	mov ah, 53h
	mov bx, 01ffh ; Display (all)
	int 15

	mov ah, 53h
	mov bx, 04ffh ; Serial ports (all)
	int 15

	mov ah, 53h
	mov bx, 03ffh ; Parallel ports (all)
	int 15

	cli
	halt_cpu:
		hlt
	jmp halt_cpu

jmp sys_force_shutdown


times 9216 - ($-$$) db 0 ; Pad out remaining 18 sectors

