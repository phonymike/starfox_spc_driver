org $008000
arch spc700

base $3ee8

gate:
	db $32, $65, $7F, $98, $B2, $CB, $E5, $FC

volt:
	db $19, $32, $4C, $65, $72, $7F, $8C, $98
	db $A5, $B2, $BF, $CB, $D8, $E5, $F2, $FC

warnpc $3f00
