org $008000

; ===============================================
dw prog_code_00_end-prog_code_00_start		; calculate size in bytes
dw $3ee8					; spc destination

prog_code_00_start:

	incbin PROG_CODE_00.bin

prog_code_00_end:



; ===============================================
dw prog_code_01_end-prog_code_01_start		; calculate size in bytes
dw $400						; spc destination

prog_code_01_start:

	incbin PROG_CODE_01.bin

prog_code_01_end:


; ===============================================
dw prog_code_02_end-prog_code_02_start		; calculate size in bytes
dw $3e20					; spc destination

prog_code_02_start:

	incbin PROG_CODE_02.bin

prog_code_02_end:


; ============================
; end of data, start execution
; ============================
dw $0000
dw $0400					; start execution here
