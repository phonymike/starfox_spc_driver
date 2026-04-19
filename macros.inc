macro warnpc(address)
	if !check_space == 1
		assert pc() <= <address>, "Used too much space"
	endif
endmacro

; Sound effect VCMDs

; VCMD $00 - Phrase termination/return from subroutine
macro rts()
	db	$00
endmacro

; VCMDs $01-$7F: note duration
macro dur(val)
	if <val> < $01
		error "Duration must be between $01-$7F"
	endif
	if <val> > $7F
		error "Duration must be between $01-$7F"
	endif
	db	<val>
endmacro

; VCMD $00-$7F: Set volume (must follow duration)
; if byte following dur() < $80, it's volume
macro vol(vol)
	if <vol> > $7F
		error "invalid volume value, must be between $0-$7F"
	endif
	db	<vol>
endmacro

; VCMDs $80-$C7: notes $80-$C7
macro pitch(val)
	if <val> < $80
		error "Invalid pitch, must be between $80-$C7"
	endif
	if <val> > $C7
		error "Invalid pitch, must be between $80-$C7"
	endif
	db	<val>
endmacro

; VCMD $E0: Set instrument (must be hex)
macro inst(val)
	db	$E0,$<val>
endmacro

; VCMD $F1: Note Pitch Envelope To
; $F1 xx yy zz
; - xx is the number of ticks to delay before applying the pitch envelope... at least in theory.
;   In reality, this never works, and the pitch slide executes instantly in all cases.
;   This byte is (almost) always zero because of this and is left out of the macro parameters.
; - yy is the length of the pitch slide in tempo ticks.
;   If this is equal or greater than the overall note length, the note won't key off at the end,
;   thus allowing for multiple pitch envelopes to be chained at once.
; - zz is the target note.
macro pitchenv(len,target)
	if <target> < $80
		error "Invalid target pitch, must be between $80-$C7"
	endif
	if <target> > $C7
		error "Invalid target pitch, must be between $80-$C7"
	endif
	db	$F1,$00,<len>,<target>
endmacro

; VCMD $F9: Pitch slide to note
; $F9 xx yy zz aa
; - xx is the starting note.
; - yy defines the number of tempo ticks to delay the slide for... at least in theory.
;   In reality, this never works, and the pitch slide executes instantly in all cases.
;   This byte is (almost) always zero because of this and is left out of the macro parameters.
; - zz defines the length of the pitch slide in tempo ticks.
;   If this is equal or greater than the overall note length, the note won't key off at the end,
;   thus allowing for multiple pitch envelopes to be chained at once.
; - aa is the target note.
macro pitchslide(start,len,target)
	if <start> < $80
		error "Invalid starting pitch, must be between $80-$C7"
	endif
	if <start> > $C7
		error "Invalid starting pitch, must be between $80-$C7"
	endif

	if <target> < $80
		error "Invalid target pitch, must be between $80-$C7"
	endif
	if <target> > $C7
		error "Invalid target pitch, must be between $80-$C7"
	endif
	db	$F9,<start>,$00,<len>,<target>
endmacro