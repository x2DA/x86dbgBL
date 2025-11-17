org 0x200
VGATmem equ 0xb800 ; VGA text mode buffer start
screendim equ 77fh ; 24*80-1, starting from the VGATmem
regdumpoffs equ 641h ; 20 * 80 +1
scrwidth equ 80

mov bp, stack_base
mov sp, stack_top
mov ax, cs
mov ss, ax

xor di, di
xor si, si
xor ax, ax

mov ds, ax
mov es, ax
mov bx, ax
mov cx, ax
mov dx, ax

;call set_cursor
jmp main

; Data goes here

welcome db "Welcome!", 0
axtitle db "ax=", 0
bxtitle db "bx=", 0
cxtitle db "cx=", 0
dxtitle db "dx=", 0
bptitle db "BP=", 0
sptitle db "SP=", 0


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

	mov si, cxtitle
	mov bx, 20h ; 1919-10 (for reg+val), dec width times id of reg, where ax=1, bx=2...
	
	mov ax, 0h
	mov bx, 0h
	mov cx, 0h
	mov dx, 0h

	call get_key
	call dump_regs

	; Wait
	push cx
	mov cx, 200d
	call ms2microseconds
	call sys_wait
	pop cx

	; Loop
jmp main


;	SI (Zero terminated string)
;	BX (Offset from top left, gets set to offset after the last char)
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


dump_regs:
	push si
	push bx
	push cx
	push cx
	push bx

	mov bx, regdumpoffs

	mov si, axtitle
	mov cx, ax
	call dump_cx
	inc bx

	mov si, bxtitle
	pop cx ; cx holds bx
	call dump_cx
	inc bx

	add bx, scrwidth
	sub bx, 10h ; account for being set at the next label + 1
				; and that we're splitting newlines every 2
	
	mov si, cxtitle
	pop cx ; bx
	call dump_cx
	inc bx
	
	mov si, dxtitle
	mov cx, dx
	call dump_cx
	inc bx

	add bx, scrwidth
	sub bx, 10h

	mov si, bptitle
	mov cx, bp
	call dump_cx
	inc bx

	mov si, sptitle
	mov cx, sp
	add cx, 8h ; Account for 4 pushes
	call dump_cx
	pop cx
	
	pop bx
	pop si
ret


;	SI (Zero terminated title)
;	BX (Offset from top left)
dump_cx:
	push ax
	push cx
	push si
	push es
	
	mov ah, 07h
	call write_line

	; Show cx contents
	mov ah, 1ch

	; higher nibble of ch

	push cx
	and ch, 0xf0 ; wipe cl
	shr ch, 4 ; cl = 0000xxxx
	; TODO: Write this as a subroutine v
	call nibble2asciihex

	mov al, ch
	call write_char
	pop cx
	; TODO: Write this as a subroutine ^

	; lower nibble of ch

	push cx
	and ch, 0x0f
	call nibble2asciihex

	mov al, ch
	call write_char
	pop cx

	; higher nibble of cl
	push cx
	and cl, 0xf0
	shr cl, 4
	xchg ch, cl
	call nibble2asciihex
	
	mov al, ch
	call write_char
	pop cx

	; lower nibble of cl
	push cx
	and cl, 0x0f
	xchg ch, cl
	call nibble2asciihex
	
	mov al, ch
	call write_char
	pop cx


	pop es
	pop si
	pop cx
	;pop bx
	pop ax
ret


;	CHlower (Hex nibble to attempt to turn into ascii)
;	CHhigher (Must be zero'd out)
;	CH (Gets the ascii value)
; ex: 0x00f[4] -> 0x0034
nibble2asciihex:
	add ch, 30h
	cmp ch, 3ah
	jl .done
	add ch, 07h
	.done:
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
	mov ah, 01h ; Get key, non-blocking
	int 16h
ret


;	AX ( [(BG|FG)] [Char] )
; Where ( [AH] [AL] )
;	BX (Offset from top left, gets set to offset after the last char)
; The offset shouldn't be a power of two, it is accounted for :)
write_char:
	push es
	call set_es_to_vidmem

	shl bx, 1

	mov [es:bx], ax
	pop es
	add bx, 2
	shr bx, 1
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

stack_base:
	times 200 db 58h
stack_top:

times 9216 - ($-$$) db 0 ; Pad out remaining 18 sectors

