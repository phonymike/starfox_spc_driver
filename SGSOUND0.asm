org $008000

; ===============================================
dw prog_code_00_end-prog_code_00_start		; calculate size in bytes
dw $3ee8					; spc destination

	incsrc PROG_CODE_00.asm


; ===============================================
dw prog_code_01_end-prog_code_01_start		; calculate size in bytes
dw $400						; spc destination

	incsrc PROG_CODE_01.asm


; ===============================================
dw prog_code_02_end-prog_code_02_start		; calculate size in bytes
dw $3e20					; spc destination

	incsrc PROG_CODE_02.asm


; ============================
; end of data, start execution
; ============================
dw $0000
dw $0400					; start execution here
