org $008000
arch spc700

base $3e20

mov	x,#$00
incw	$2c
mov	a,($2c+x)
mov	y,#$06
mul	ya
mov	$d2,#$8f
mov	$d3,#$23
addw	ya,$d2
movw	$d2,ya
mov	y,#$00
mov	a,$03c2
or	 a,#$04
mov	x,a
mov	$12,#$04
mov	a,($d2)+y
push	y
push	x
pop	y
call	$060d
push	y
pop	x
pop	y
inc	x
inc	y
dbnz	$12,$3e3e
mov	a,($d2)+y
mov	x,$03c0
mov	$0221+x,a
inc	y
mov	a,($d2)+y
mov	$0220+x,a
jmp	$2b27
set7	$13
mov	a,#$60
mov	y,#$03
dec.b	$a0+x
call	$0cc4
mov	a,$0361+x
mov	y,a
mov	a,$0360+x
movw	$10,ya
mov	$47,#$00
jmp	$0582
push	a
mov	y,#$5c
mov	a,#$00
call	$060d
pop	a
mov	y,#$4c
jmp	$060d
mov	a,$03f1
bne	$3ea5
mov.b	a,$59
mov	$03f1,a
mov	a,#$88
mov.b	$59,a
ret

mov	a,$03f1
beq	$3ea5
mov	a,$03f1
mov.b	$59,a
mov	a,#$00
mov	$03f1,a
ret

mov	a,$03c1
and.b	a,$4a
beq	$3eba
mov.b	a,$4a
setc
sbc	a,$03c1
mov.b	$4a,a
mov	y,#$4d
call	$060d
ret

warnpc $3ebb
