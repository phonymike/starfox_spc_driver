org $008000
arch spc700

base $3ee8

clr1	$65
reti
adc	$cb,#$b2
mov	a,$19fc
clr1	$4c
cmp	a,$7f72
dec	$a598
clr5	$bf
mov.b	$d8,y
mov	a,$fcf2

warnpc $3f00
