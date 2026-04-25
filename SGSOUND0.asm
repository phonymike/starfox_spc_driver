; Asar 1.91
org $008000
arch spc700

; Optimizations/fixes
; 0 = off, 1 = on
!opt_enginesnd = 0	; optimize engine sound generation code (requires patches to PSGSND2, SGSOUND2, and SGSOUND3)
					; saves 19 bytes
!opt_f1_f9 = 0		; remove unnecessary broken delay operand from $F1/$F9 VCMDs
					; saves 361 bytes
!opt_misc = 0		; various small optimizations

incsrc macros.inc
!check_space = 1			; 1 will warn if data chunks are too large

; ===============================================
; PROG_CODE_00
spcblock $3ee8 nspc

; note dur%'s
gate:
	db $32, $65, $7F, $98, $B2, $CB, $E5, $FC

; per-note velocity values
volt:
	db $19, $32, $4C, $65, $72, $7F, $8C, $98
	db $A5, $B2, $BF, $CB, $D8, $E5, $F2, $FC

%warnpc($3f00)

endspcblock


; ===============================================
; PROG_CODE_01
spcblock $400 nspc

incsrc defines.asm
incsrc KAN.asm

	clrp					; clear direct page flag
;................................................
	mov	x,#$cf				; stack pointer ; set SP to (01)cf
	mov	sp,x				;
;
	mov	a,#$00				; clear RAM 000h-0dfh
	mov	x,a
;
start10:
	mov	(x+),a
	cmp	x,#$df+1			; zero 00-e0
	bne	start10
;........................................
	mov	x,#$00
-
	mov	!ngs+x,a
	inc	x
	bne	-					; zero 0200-02ff

-
	mov	!pvodw+x,a
	inc	x					; zero 0300-03ff
	bne	-
;........................................
	inc	a
	call	esaset			; EDL & ESA set ; set echo delay to 1 (16ms)
;
	set5	!flgs			; echo off
;................................................
	mov	a,#$96
	mov	!_03c6,a
	mov	a,#$bb
	mov	!_03cb,a
	call	create_engine_sound_brr
;................................................
	mov	a,#$60
	mov	y,#$0c				; MVOL
	call	apus			; master vol L = $60
;
	mov	y,#$1c				; MVOR
	call	apus			; master vol R = $60
;
	mov	a,#!sampl_dir>>8	; source dir = !sampl_dir<<8
	mov	y,#$5d				; DIR
	call	apus			; 19 byte
;........................................
	mov	a,#$f0				; inputport reset
	mov	!cont,a				; timer stop ; reset ports, disable timers
;
	mov	a,#$10				; 2mS
	mov	!tmset,a			; timer data set ; set timer0 latch to #$10 (500 Hz, 2ms)
	mov.b	!tmp,a
;
	mov	a,#$01				; timer start ; start timer0
	mov	!cont,a
;++++++++++++++++++++++++++++++++++++++++++++++++
start20:					; 2mS
;........................................
;	mov	a,ffk				; !! test !!
;	bne	start55				; !! test !!
;........................................
	mov	y,#10				; 10 data set ; set DSP regs from shadow:
start24:
	cmp	y,#05
	beq	start25
	bcs	start26
;
	cmp	!ekin,!eclr			; echo clear chu ?
	bne	start28				; EON EFB EVOR EVOL
;
start25:
	bbs7	!ekin,start28	; echo kinshi chu ? (FLG)
;
start26:
	mov	a,dseta-1+y
	mov	!apuadd,a			; write address
	mov	a,dsetd-1+y
	mov	x,a
	mov	a,(x)
	mov	!apudt,a			; data write ; write to DSP reg
start28:
	dbnz	y,start24		; 18 byte ; loop for each reg
;
	mov.b	!keyons,y
	mov.b	!keyoffs,y
;...................
	mov.b	a,!rdm			; random keisan
	eor.b	a,!rdm+1
	lsr	a
	lsr	a
	notc					; d1 check
	ror.b	!rdm			; wait for counter0 increment
	ror.b	!rdm+1
;........................................
start40:
	mov	y,!tmdt				; timer read
	beq	start40
;
	push	y				; 2mS goto
;................................................
	mov	a,#14*4				; 14 count
	mul	ya
;
	clrc
	adc.b	a,!cnt
	mov.b	!cnt,a
	bcc	start50
;.......................................; 16mS
	call	_257c
	mov	x,#$01
	call	flset
	call	_2749
;
	mov	x,#$02				; fl2 & port2 check
	call	flset			; x = fl?
	call	chd				; sound effect
	call	_2981
;........................................
	cmp	(!ekin),(!eclr)
	beq	start50
	inc	!_03c7
;................................................
	mov	a,!_03c7
	lsr	a
	bcs	start50
	inc.b	!ekin
;................................................
start50:
	mov.b	a,!tmp			; tmp = 20h (normal)
	pop	y					; timer count 
	mul	ya					;
;
	clrc
	adc.b	a,!tmpd
	mov.b	!tmpd,a
	bcc	start60
;...................
	mov	a,!_03f8
	bne	+
start55:
	call	cha				; music
;
	mov	x,#$00				; fl0 & port0 check
	call	_4FE			; x = fl?
+
	jmp	start20
;................................................
start60:
	mov.b	a,!sf0
	beq	start20x
;...
	mov	x,#$00				; hokan routin (8 ch.)
	mov	!keyd,#$01			; key data set
;
start62:
	mov.b	a,!add+1+x
	beq	start64				; kami = 0
;
	call	trry			; pan move & sweep & vib check
;
start64:
	inc	x
	inc	x
	asl.b	!keyd			;
	bne	start62				; channel end ? (8ch)
;
start20x:
	call	generate_engine_sound_noise
	jmp	start20				; channel end
;************************************************
flset:
	mov.b	a,!sf0+x		; flag set flx
	mov	!port0+x,a			; flag return
;
flset02:
	mov	a,!port0+x			; flag read
	cmp	a,!port0+x			; 2 kai check
	bne	flset02				;
;
	mov	y,a
	mov	!fl0+x,y			; new data
;
dssr:
	ret
;************************************************
_4FE:
	mov.b	a,!sf0+x		; flag set flx
	mov	!port0+x,a			; flag return
-
	mov	a,!port0+x			; flag read
	cmp	a,!port0+x			; 2 kai check
	bne	-					;
;
	mov	y,a
	mov.b	a,!fl0s+x
	mov	!fl0s+x,y
	cbne	!fl0s+x,+
	mov	y,#$00
+
	mov	!fl0+x,y
	ret
;************************************************
; 
;
;************************************************ 
;		Freq. data set
;************************************************ 
; handle a note vcmd (80-df)
dss:
	cmp	y,#!dd0				; drums check
	bcc	dss0
;************************************************ 
;		drums set    ; x=channel  a=sno 
;************************************************ 
; vcmds ca-df - percussion note
dds:
	call	snoset			; sno data set ; set sample
	mov	y,#!c30				; (takasa) ; dispatch as note $a4
;................................................
; vcmds 80-c7,c8,c9 - note/tie/rest
dss0:
	cmp	y,#!xxx				; tai or yyy ? 
	bcs	dssr				; skip if tie/rest
;................................................
	mov.b	a,!fkin			; kinshi flag check
	and.b	a,!keyd
	bne	dssr
;......
; vcmds 80-c7 - note (note number in Y)
	mov	a,y
	and	a,#$7f				; fre. set & flag set
	clrc					; key trans. add.
	adc.b	a,!ktps			; add global transpose
	clrc
	adc	a,!ptps+x			; add per-voice transpose
	mov	!swpd+x,a			; ontei store
;
	mov	a,!tund+x
	mov	!swpdw+x,a			; sweep shosuten ika
;................................................
	mov	a,!vibcs+x			;
	lsr	a
	mov	a,#$00
	ror	a
	mov	!vibc+x,a			; count data (00h or 80h)
;
	mov	a,#$00
	mov.b	!vibhc+x,a		; vib hold
	mov	!vibcc+x,a			; vib change
	mov	!trec+x,a			; tre count = 0
	mov.b	!trehc+x,a		; tre hold
;
	or	(!vols),(!keyd)		; vol set flag ; set volume changed flg
	or	(!keyons),(!keyd)	; keyon ; set key on shadow bit
;................................................
	mov	a,!swsc+x			; sweep check ; pitch envelope counter
	mov.b	!swpc+x,a		; sweep (counter) ; portamento counter
	beq	dss6
;................................................
	mov	a,!swshc+x
	mov.b	!swphc+x,a		; sweep (hold)
;
	mov	a,!swsk+x			; sws or swk ? ; pitch envelope mode (0:attack / 1:release)
	bne	dss3
;......
	mov	a,!swpd+x			; (sws)
	setc					;
	sbc	a,!swss+x			;
	mov	!swpd+x,a			;
;......	
dss3:
	mov	a,!swss+x			; + ? (swk)
	clrc
; set DSP pitch from $10/1
	adc	a,!swpd+x			; now + @
;......
	call	swpadset		; sweep data set
;........................................ from kokaon
dss6:
	call	swpdset			; kkk sss <-- swpd swpdw
;************************************************
;		fre. data set   kkk & sss  x=channel  bls set
;************************************************
dssx:
	mov	y,#$00				; S curve hosei
	mov.b	a,!kkk
	setc
	sbc	a,#52				; e40 = 52
	bcs	dssx04				; e40 ijo add
;...
dssx02:
	mov.b	a,!kkk
	setc
	sbc	a,#19				; g10 = 19
	bcs	dssx10
;
	dec	y					; y = 0ffh
	asl	a
dssx04:
	addw	ya,!sss
	movw	!sss,ya
;................................................
dssx10:
	push	x			; ontei store (kkk,sss) 
	mov.b	a,!kkk
; get pitch from note number in A (with octave correction)
	asl	a
	mov	y,#00
	mov	x,#24				; decimal
	div	ya,x				; ya/x = a ... y
	mov	x,a					; x = oct.
;...
	mov	a,gfd+1+y			; high
	mov.b	!adx+1,a
	mov	a,gfd+y				; low
	mov.b	!adx,a			; set $14/5 from pitch table
;
	mov	a,gfd+3+y			; high
	push	a
	mov	a,gfd+2+y			; low
	pop	y
	subw	ya,!adx			; ya - adx
;...						; ( 0.sss x ya ) + adx  = adx
	mov.b	y,!sss
	mul	ya					; shimo x 0.???
	mov	a,y
	mov	y,#00
	addw	ya,!adx
	mov.b	!adx+1,y
;
	asl	a
	rol.b	!adx+1
	mov.b	!adx,a
	bra	dssx14
;...
dssx12:
	lsr.b	!adx+1
	ror	a
	inc	x
dssx14:
	cmp	x,#06				; x = oct.
	bne	dssx12
	mov.b	!adx,a
;
	pop	x
;........................................
	mov	a,!bls+x			; 0. block su
	mov.b	y,!adx+1		; fre.(high)
	mul	ya			;
	movw	!adx+2,ya		;
;
	mov	a,!bls+x			; 0. block su
	mov.b	y,!adx			; fre.(low)
	mul	ya			;
	push	y				; --> low
;
	mov	a,!bls+1+x			; block su
	mov.b	y,!adx			; fre.(low)
	mul	ya			;
	addw	ya,!adx+2
	movw	!adx+2,ya		;
;
	mov	a,!bls+1+x			; block su
	mov.b	y,!adx+1		; fre.(high)
	mul	ya
	mov	y,a
	pop	a					; <-- low
	addw	ya,!adx+2
	movw	!adx+2,ya		; freq. set
;................................................
	mov	a,x					; apunch ; set voice X pitch DSP reg from $16/7
	xcn	a					;  (if vbit clear in $1a)
	lsr	a
	or	a,#$02				; pl1 = 2
	mov	y,a					; write address ; Y = voice X pitch DSP reg
;
	mov.b	a,!adx+2		; shimo
	call	apusx			; a=data  y=address
;
	inc	y
	mov.b	a,!adx+3		; kami
;************************************************
;		APU data out   acc = write data   y = write add
;************************************************
; write A to DSP reg Y if vbit clear in $1a
apusx:
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin			; kinshi flag check
	pop	a
	bne	apusr
;................................................
; write A to DSP reg Y
apus:
	mov	!apuadd,y			; write address
	mov	!apudt,a			; data write
apusr:
	ret
;................................................
; 
; 
;************************************************
;************************************************
;
;
;................................................
; Generate random noise used for Arwing's engine sound
generate_engine_sound_noise:
	dec	!_d0
	mov.b	a,!_d0
	and	a,#$03
	mov	y,#$3f
	mul	ya
	mov	y,a
	mov	!ttt,#$07
_621:
	inc	y
	mov	x,#$04
-
	mov1	c,!rdm+1.6
	eor1	c,!rdm+1.5
	rol.b	!rdm
	rol.b	!rdm+1
	mov.b	a,!rdm+1
	and	a,!_03cb
	or	a,#$11
	mov	!engine_snd+y,a
	inc	y
	mov.b	a,!rdm
	or	a,#$11
	mov	!engine_snd+y,a
	inc	y
	dec	x
	bne	-
	dbnz	!ttt,_621
	ret

; Create the BRR sample for the Arwing's engine sound and insert its address into the sample directory
; Also initialize the RNG seed if it is zero
create_engine_sound_brr:
	if !opt_enginesnd == 0
	mov	y,#$00
	mov	x,#$1b
	else
	mov	x,#$00				; Swapped X and Y so a dbnz could be used instead of a dex+bne below
	mov	y,#$1b
	endif
	mov	a,!_03c6
-
	if !opt_enginesnd == 0
	mov	!engine_snd+y,a
	inc	y
	inc	y
	inc	y
	inc	y
	inc	y
	inc	y
	inc	y
	inc	y
	inc	y
	dec	x
	bne	-
	else
	mov	!engine_snd+x,a
	push	a		; 4 cycles ; Increase X by 9
	mov	a,x			; 2 cycles
	clrc			; 2 cycles
	adc	a,#9		; 2 cycles
	mov	x,a			; 2 cycles
	pop	a			; 4 cycles
; new code to increase X by 9 takes 7 bytes, 16 cycles
; old code (iny 9x) took 9 bytes, 18 cycles
	dbnz	y,-		; saves 1 byte, combines dey+bne into 1 instruction
	endif
	inc	a
	if !opt_enginesnd == 0
	mov	!engine_snd+y,a
	mov.b	y,#!engine_snd>>8	; Overwrite sample directory index $20 with location of Arwing's engine sound in ARAM
	mov.b	a,#!engine_snd
	mov	!sampl_dir+($20*4),a
	mov	!sampl_dir+($20*4)+1,y
	mov	!sampl_dir+($20*4)+2,a
	mov	!sampl_dir+($20*4)+3,y
	else
	mov	!engine_snd+x,a
	; 16 bytes saved by removing code to insert the address of engine sound into the sample directory
	; this requires PSGSND2, SGSOUND2, SGSOUND3 to be patched to hardcode the engine sound address
	endif
	mov.b	a,!rdm
	or.b	a,!rdm+1		; Check if RNG seed is zero by ORing each byte against each other
	bne	+					; If OR result is nonzero, RNG is already initialized, skip the next line
	inc.b	!rdm			; If OR result is zero, Increment random seed by 1 to initialize RNG
+
	ret
;................................................
_67B:
	mov	a,#$00
	mov	y,#$2c
	call	apus
	mov	y,#$3c
	call	apus
	mov	a,#$ff
	mov	y,#$5c
	call	apus
	call	ten00
	mov	a,#$00
	mov	!_03ca,a
	mov.b	!sf0,a
	mov	!sf1,a
	mov	!sf2,a
	mov	!sf3,a
	mov.b	!fkin,a
	mov	y,#$10
-
	mov	!_039f+y,a
	dbnz	y,-
	mov	a,#$96
	mov	!_03c6,a
	mov	a,#$bb
	mov	!_03cb,a
	call	create_engine_sound_brr
;................................................
_6B7:
	cmp	!sf0,#$11
	beq	_6CF
	mov	x,#$a0
	mov.b	!mvoc,x
	mov	!_03ca,x
	mov	a,#$00
	mov.b	!mvom,a
	setc
	sbc.b	a,!mvo
	call	divx
	movw	!mvoadw,ya
_6CF:
	jmp	cha02				; finished with command
;................................................
_6D2:
	mov	a,!_03f1
	bne	_6F5
	mov.b	a,!mvo
	mov	!_03f1,a
	mov	a,#$70
	mov.b	!mvo,a
	jmp	cha02				; finished with command
;................................................
_6E3:
	mov	a,!_03f1
	beq	_6F5
	mov	a,!_03f1
	mov.b	!mvo,a
	mov	a,#$00
	mov	!_03f1,a
	jmp	cha02				; finished with command
_6F5:
	ret
;................................................
decode_commands:
	cmp	a,#$ff
	beq	_67B
	cmp	a,#$f1				; fade song volume
	beq	_6B7
	cmp	a,#$f2				; restore song volume
	beq	_6D2
	cmp	a,#$f3
	beq	_6E3
	cmp	a,#$f4				; transpose voices 0-2 to -2 semitones and fade tempo
	beq	_71D
	cmp	a,#$f5				; transpose voices 0-2 to 3 semitones and fade tempo
	beq	_717
	cmp	a,#$f0				; stop music
	beq	_744
	cmp	a,#$14
	bcc	_766
	ret

_717:
	mov	x,#3				; transpose voices 0-2 to 3 semitones
	mov	a,#$30				; fade tempo to $30
	bne	+
_71D:
	mov	x,#-2				; transpose voices 0-2 to -2 semitones
	mov	a,#$09				; fade tempo to $09
+
	mov	!tmpc,#$8f			; fade tempo over 143 tempo ticks
	mov	!ptps,x
	mov	!ptps+2,x
	mov	!ptps+4,x
	mov.b	!tmpm,a
	setc
	sbc.b	a,!tmp
	mov.b	x,!tmpc
	call	divx
	movw	!tmpadw,ya
	jmp	cha02				; finished with command
;................................................
_73C:
	dec	!_03ca
	beq	_744
	jmp	_7E8
_744:
	mov.b	a,!fkin
	eor	a,#$ff
	tset	!keyoffs,a
	mov	!sf0,#$00
	mov	!keyd,#$00
	mov	!mvo,#!main_vol		; main volume set
	mov	!tmp,#!tempo		; tempo data set
	ret
;................................................
adset:
; read $ads/1 into YA with advancing the ptr
	mov	y,#00				; block address set
	mov	a,(!ads)+y
	incw	!ads
	push	a				; shimo
	mov	a,(!ads)+y
	incw	!ads
	mov	y,a					; kami
	pop	a
	ret
;................................................
; play song in A
_766:
	clrc
	mov	x,#$00
	mov	!_03ca,x
	mov	!_03f1,x
	mov.b	!sf0,a
	asl	a
;	beq	ks04				; 000h = end
;......
	mov	x,a					; shoki data set
	mov	a,!gft-2+1+x		; block add. shoki set
	mov	y,a
	bne	+
	mov.b	!sf0,a
	ret
+
	mov	a,!gft-2+x
	movw	!ads,ya
;......
	mov	!sf0c,#$02			; count
;...................
ks04:
	mov.b	a,!fkin			; key off
	eor	a,#$ff
	tset	!keyoffs,a		; keyoff set
	ret
;................................................ 
; reset song params
ks10:
	mov	x,#14				; shoki data set
	mov	!keyd,#$80			; last voice

ks12:
	mov.b	a,!keyd
	and.b	a,!fkin
	and	a,#$c0
	bne	_7BC
;
	mov	a,#!voice_vol		;voice volume = $ff
	mov	!pvod+x,a			; part vol
;
	mov	a,#10				; pan data set ; pan = $0a.00
	call	panx			; pand & panf  set    (a=0) ; zero instrument
;
	mov	!snos+x,a			; sound number
	mov	!tund+x,a			; tun shoki set
	mov	!ptps+x,a			; part tran. set
	mov	!swsc+x,a			; sweep count
	mov	!_03e1+x,a
	mov	!_03e0+x,a
	mov	!_03d0+x,a
	mov.b	!vibd+x,a		; vib depth
	mov.b	!tred+x,a		; tre depth
;
_7BC:
	dec	x
	dec	x					; - 2
	lsr.b	!keyd
	bne	ks12				;loop for each voice
;......
	mov.b	!mvoc,a			; mvol count (a=0) ; zero master vol fade counter
	mov.b	!evoc,a			; evol count ; zero echo vol fade counter
	mov.b	!tmpc,a			; tempo count set ; zero tempo fade counter
	mov.b	!ktps,a			; key trans. set ; zero global transpose
	mov.b	!blc,a			; block count ; zero block repeat count
	mov.b	!wavs,a			; source
;
	mov	!mvo,#!main_vol		; main volume set ; master vol
	mov	!tmp,#!tempo		; tempo data set ; tempo
char:
	ret
;************************************************
;		music enso routin
;************************************************
cha:
	mov.b	a,!fl0
	beq	cha02
	jmp	decode_commands
cha02:
	mov.b	a,!sf0			; play chu ?
	beq	char				;
	mov	a,!_03ca
	beq	_7E8
	jmp	_73C
;........................................
_7E8:
	mov.b	a,!sf0c			;
	beq	txh
;
	dbnz	!sf0c,ks10		; wait count (dec & bne)
;................................................
ks20:
	call	adset			; block address set (Z=kami) ; read block addr from $40/1, advance ptr
;......
	bne	ks40				; load start addresses, if hi-byte is non zero
;......
	mov	y,a					; shimo = 0 ? ; refetch lo-byte
	bne	ks24				; music end ? ; set/dec repeat count
;........................................
;************************************************
	jmp	_744				; key off, return if also zero
;************************************************
; set/dec repeat count
ks24:
	dec.b	!blc
	bpl	ks26
;
	mov.b	!blc,a			; blc=0 or 129 ijo
;
ks26:
	call	adset			; kurikaeshi ? ; read next word as well
;
	mov.b	x,!blc			; blc = 0 ?
	beq	ks20				;
;
	movw	!ads,ya			; kurikaeshi ads set ; "goto" that address
	bra	ks20				; continue
;........................................
; load start addresses - hi-byte not zero
ks40:
	movw	!adx+2,ya		; adx+2,+3 set
	mov	y,#15				; shoki address set (8ch)
;
ks42:
	mov	a,(!adx+2)+y		; part sento add. set
	mov	!add+y,a			; add.
	dec	y
	bpl	ks42				; set all reading ptrs
;........................................
	mov	x,#$00				; shoki data set
	mov	!keyd,#$01			; first voice
;
ks44:
	mov.b	a,!add+1+x
	beq	ks46				; if vptr hi != 0
;
	mov	a,!snos+x
	bne	ks46
;
	mov	a,#$00
	call	snoset			; sno data set ; set instrument #0 if not set
;
ks46:
	mov	a,#$00
	mov.b	!ptc+x,a		; pt  count = 0	; zero subroutine repeat counter
;
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin
	and	a,#$c0
	pop	a
	bne	+
;
	mov.b	!panc+x,a		; pan move count ; zero subroutine repeat counter
	mov.b	!pvoc+x,a		; vol move count ; zero voice vol fade counter
+
	inc	a
	mov.b	!ngc+x,a		; Nagasa count set (ngo) ; set duration counter to 1
;
	inc	x
	inc	x					; + 2
	asl.b	!keyd			; next voice
	bne	ks44				; for each voice
;************************************************
txh:
	mov	x,#$00				; channel count
	mov.b	!vols,x			; vols reset
	mov	!keyd,#$01			; key data set ; first voice
;................................................
tx00:
	mov.b	!chn,x
	mov.b	a,!add+1+x
	beq	tx60				; kami = 0 (no use channel) ; next if vptr hi zero
;................................................
	dec.b	!ngc+x			; dec duration counter
	bne	tx22				; if not zero, skip to voice readahead
;......
tx10:
	call	data_in			; data in & inc add ; read vcmd into A and Y
	bne	tx15				; block end ?
; vcmd 00 - end repeat/return
	mov.b	a,!ptc+x		; pattern chu ?
	beq	ks20				; read next block if loop has been done
;................................................ 
; repeat / return from subroutine
	call	addset			; pattern start add set ; jump to loop start addr
;......
	dec.b	!ptc+x			; dec repeat count
	bne	tx10				; if the loop has been done
;......
	mov	a,!adt+x			; add restore (pattern end)
	mov.b	!add+x,a
	mov	a,!adt+1+x
	mov.b	!add+1+x,a		; back to return addr instead
	bra	tx10				; then continue
;................................................
; vcmd branches
tx15:
	bmi	tx16				; d7 = 1 ? ; vcmds 01-7f - note info:
;
	mov	!ngs+x,a			; Nagasa Store ; set cmd as duration
;......
	call	data_in			; data in & inc add	;read next byte
	bmi	tx16				; d7 = 1 ? ; if note note then
;......
	push	a				; % & vol
	xcn	a					; kami
	and	a,#$07
	mov	y,a
	mov	a,gate+y			; Gate off (%) set
	mov	!ngg+x,a			; set dur% from high nybble
;
	pop	a					; shimo
	and	a,#$0f
	mov	y,a
	mov	a,volt+y
	mov	!vol+x,a			; vol set ; set per-note vol from low nybble
;...................
;	mov	!kkk,a				; X 2.5
;	lsr	!kkk
;	asl	a
;	adc	a,!kkk
;	mov	!ngg+x,a			; Gate off (%) set
;... 
;	call	data_in			; data in & inc add
;	bmi	tx16				; $
;...
;	asl	a					; X 2
;	mov	!vol+x,a			; Gain set
;...................
	call	data_in			; data in & inc add ; read vcmd into A and Y
; vcmd branches 80-ff
tx16:
	cmp	a,#!sno				; special flag ?
	bcc	tx17
;
	call	spfx			; special flag ; vcmds e0-ff
	bra	tx10
; vcmds 80-df - note
tx17:
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin
	pop	a
	bne	tx18
	call	dss				; freq. data set ; handle note cmd if vbit $1a clear
;...................
tx18:
	mov	a,!ngs+x			; set duration counter from duration
	mov.b	!ngc+x,a
	mov	y,a
;
	mov	a,!ngg+x			; gate off (step) set
	mul	ya
	mov	a,y
	bne	tx19
	inc	a					; a = 1
tx19:
	mov.b	!ngo+x,a		; set actual key-off dur counter
	bra	tx40
;................................................
tx22:
	call	keych			; keyoff & sweep & vib check ; do readahead
;................................................
tx40:
	call	swpch			; sweep check (next data)
;................................................
tx60:
	inc	x					;
	inc	x					;
	asl.b	!keyd			;
	beq	tmpy				; channel end ? (8ch)
;************************************************
;		tempo move  tmp mvol pan move keisan & gain set
;************************************************
	jmp	tx00
tmpy:
	mov.b	a,!tmpc			; tmp move chu ? ; tempo fade counter
	beq	evoy
;
	movw	ya,!tmpadw		; move keisan
	addw	ya,!tmpw		; add tempo fade to tempo
	dbnz	!tmpc,tmp20		; dec & bne
;							; tmpc = 0 (move end)
	movw	ya,!tmpc		; y <- tmpm , a <- 00	; last time: move tempo target to tempo
tmp20:
	movw	!tmpw,ya
;************************************************
;		evol move
;************************************************
evoy:
	mov.b	a,!evoc			; evo move chu ? ; echo vol fade counter
	beq	_8f1
;
	movw	ya,!evoladw		; move keisan
	addw	ya,!evolw
	movw	!evolw,ya		; add echo L delta to echo L vol
;
	movw	ya,!evoradw		; move keisan
	addw	ya,!evorw		; add echo R delta to echo R vol
	dbnz	!evoc,evo12		; dec & bne
;
	movw	ya,!evoc		; y <- evolm , a <- 00
	movw	!evolw,ya
	mov.b	y,!evorm		;
evo12:
	movw	!evorw,ya
;************************************************
;		mvol move
;************************************************
_8f1:
	mov.b	a,!mvoc			; mvol move chu ? ; master vol fade counter
	beq	mvo40
;
	movw	ya,!mvoadw		; move keisan
	addw	ya,!mvow		; add master vol delta to value
	dbnz	!mvoc,+			; dec & bne
;							; mvoc = 0 (move end)
	movw	ya,!mvoc		; y <- mvom , a <- 00
+
	movw	!mvow,ya
	mov	!vols,#$ff			; mvo set ; set all vol chg flags
;................................................
mvo40:
	mov	x,#$00				; vol set keyon & end
	mov	!keyd,#$01			; key data set (8ch) ; first voice
;
mvo42:
	mov.b	a,!add+1+x
	beq	mvo46				; kami = 0
;
	call	voly			; tre pan move & vol set ; do per-voice fades
;
mvo46:
	inc	x
	inc	x
	asl.b	!keyd
	bne	mvo42
;
mvo48:
	ret
;................................................
;
;
;************************************************
;		special flag check
;************************************************
; dispatch vcmd in A (e0-ff)
spfx:
	asl	a					; e0-ff => c0-fe (8 bit)
	mov	y,a
!_spft = !sno*2-256			; = $C0
	mov	a,spft+1-(!_spft)+y	; high
	push	a
	mov	a,spft-(!_spft)+y	; low
	push	a				; push jump address from table
;
	mov	a,y
	lsr	a
	mov	y,a
	mov	a,spfp-!sno+128+y	;mov	a,$0bb0+y ; vcmd length
	beq	data_inr			; if non zero
;************************************************
;		data in  &  inc address
;************************************************
; read new argument to A and Y
data_in:
	mov	a,(!add+x)			; data in
; advance reading ptr
add_inc:
	inc.b	!add+x
	bne	data_inr
	inc.b	!add+1+x		; inc reading ptr
data_inr:
	mov	y,a					; flag set
	ret						; jump to vcmd
;................................................
;
;
;************************************************
;		sound no.
;************************************************
; vcmd e0 - set instrument
snox:
	;call	data_in			; data in & inc add
;************************************************
;		Sound No. data set
;************************************************
snoset:
	mov	!snos+x,a			; sno store
snoset0:
	mov	y,a					; d7 check
	bpl	snoset1				; if percussion note:
;......
	setc
	sbc	a,#$ca				; ca-dd => 00-15
;
	clrc
	adc.b	a,!wavs			; bias add. ; add perc patch base
;......
snoset1:
	mov	y,#$06				; x=channel a=sno
	mul	ya
	movw	!adx,ya
	clrc
	adc	!adx,#!patch_tab		; #low sod
	adc	!adx+1,#!patch_tab>>8	; #high sod
;...
	mov.b	a,!fkin			; kinshi flag check
	and.b	a,!keyd
	bne	snosetr
;...
	push	x
	mov	a,x					; apuch
	xcn	a
	lsr	a
	or	a,#$04				; write address ; voice X SRCN
	mov	x,a
;
	mov	y,#$00				; 1st data in
	mov	a,(!adx)+y			; sound data set
	bpl	snoset4				; noise ?
;......
snoset2:
	and	a,#$1f				; noise clock store ; sample > 80: noise, freq in low bits
	and	!flgs,#$20			; keep echo bit from FLG
	tset	!flgs,a			; noise clock store ; OR in noise freq
;
	or	(!nons),(!keyd)		; noise channel store ; set vbit in noise enable
;
	mov	a,y					; y = 0 (=dd0) ; set SRCN to 0
	bra	snoset8				; else
;......
snoset4:
	mov.b	a,!keyd			; normal sno
	tclr	!nons,a			; noise channel clear ; clear noise vbit
;...................
snoset6:
	mov	a,(!adx)+y			; sound data set ; set SRCN from table
snoset8:
	mov	!apuadd,x			; write address
	mov	!apudt,a			; data write
;
	inc	x
	inc	y
	cmp	y,#$04
	bne	snoset6				; tensou data 4 ; set SRCN, ADSR1/2, GAIN from table
;
	pop	x
	mov	a,(!adx)+y			; 5 ban me (block su)
	mov	!bls+1+x,a			; block su store ; set pitch multiplier
	inc	y
	mov	a,(!adx)+y			; 6 ban me (block su)
	mov	!bls+x,a			; block su store
;
snosetr:
	ret
;************************************************
;		pan data set
;************************************************
; vcmd e1 - pan
panx:
	;call	data_in			; data in & inc add
	mov	!panf+x,a			; pan flag store
;
	and	a,#$1f
	mov	!pand+x,a			; pan data ; voice pan value
	mov	a,#$00
	mov	!pandw+x,a
;
	ret
;************************************************
;		pan move
;************************************************
; vcmd e2 - pan fade
pamx:
	;call	data_in			; data in & inc add
	mov.b	!panc+x,a		; pan (count)
	push	a				; count --> x
;
	call	data_in			; data in & inc add
	mov	!panm+x,a			; pan (mokuteki)
;......
	setc
	sbc	a,!pand+x			; pan (now data) ; current pan value
	pop	x					; count --> x
;
	call	divx			; x=count a=sa c=+,- ; delta = pan value / steps
;......
	mov	!panadw+x,a			; + shimo
	mov	a,y					; kami
	mov	!panad+x,a			; + kami
	ret
;************************************************
;		vibrate
;************************************************
; vcmd e3 - vibrato on
vibx:
	;call	data_in			; data in & inc add
	mov	!vibhs+x,a			; vib hold
;
	call	data_in			; data in & inc add
	mov	!vibcad+x,a			; vib speed (+@)
;
	call	data_in			; data in & inc add
;************************************************
;		vibrate off
;************************************************
; vcmd e4 - vibrato off
vofx:
	mov.b	!vibd+x,a		; vib depth
	mov	!vibdm+x,a			; vib depth mokuteki
;
	mov	a,#$00
	mov	!vibcs+x,a			; vib change count
	ret
;************************************************
;		vibrate change
;************************************************
; vcmd f0 - vibrato fade
vchx:
	;call	data_in			; data in & inc add
	mov	!vibcs+x,a			; vib change count
	push	a
;
	mov	y,#$00
	mov.b	a,!vibd+x		;
	pop	x
	div	ya,x				; ya/x = a ... y
;
	mov.b	x,!chn
	mov	!vibad+x,a
;
	ret
;************************************************
;		main volume change
;************************************************
; vcmd e5 - master volume
mv1x:
	;call	data_in			;;00
	mov	a,!_03ca
	bne	+
	mov	a,!_03f1
	bne	+
	mov	a,#$00
	movw	!mvow,ya		; main vol
+
	ret
;************************************************
;		main volume move
;************************************************
; vcmd e6 - master volume fade
mv2x:
	;call	data_in			; data in & inc add
	mov.b	!mvoc,a			; mvol (count)
;
	call	data_in			; data in & inc add
	mov.b	!mvom,a			; mvol (mokuteki)
;......
	setc
	sbc.b	a,!mvo			; mvol (now data)
	mov.b	x,!mvoc			; count --> x
;
	call	divx			; x=count a=sa c=+,-
;
	movw	!mvoadw,ya		; + @
	ret
;************************************************ 
;               tempo change   
;************************************************ 
; vcmd e7 - tempo
tp1x:
	;call	data_in			;;00   
	mov	a,#$00
	movw	!tmpw,ya		; tempo
	ret
;************************************************ 
;               tempo move     
;************************************************ 
; vcmd e8 - tempo fade
tp2x:
	;call	data_in			; data in & inc add
	mov.b	!tmpc,a			; tmp (count)
;
	call	data_in			; data in & inc add
	mov.b	!tmpm,a			; tmp (mokuteki)
;
	setc
	sbc.b	a,!tmp			; tmp (now data)
	mov.b	x,!tmpc			; count --> x
;
	call	divx			; x=count a=sa c=+,-
;
	movw	!tmpadw,ya		; + @
	ret
;************************************************
;		key tras.
;************************************************
; vcmd e9 - global transpose
ktpx:
	;call	data_in			;; 0  
	mov.b	!ktps,a
	ret
;************************************************
;		part key tras.
;************************************************
; vcmd ea - per-voice transpose
ptpx:
	;call	data_in			;; x
	mov	!_03d0+x,a
	mov	a,!_03a0+x
	bne	+
	mov	a,!_03d0+x
	mov	!ptps+x,a			; key trans. store
+
	ret
;************************************************
;		tremolo
;************************************************
; vcmd eb - tremolo on
trex:
	;call	data_in			; data in & inc add
	mov	!trehs+x,a			; tre hold
;
	call	data_in			; data in & inc add
	mov	!trecad+x,a			; tre speed (+@)
;
	call	data_in			; data in & inc add
;************************************************
;		tremolo off		; a = 0
;************************************************
; vcmd ec - tremolo off
tofx:
; vcmd ec -ff
	mov.b	!tred+x,a		; tre depth
	ret
;************************************************
;		sweep kurikaeshi
;************************************************
; vcmd f1 - pitch envelope (release)
swkx:
	mov	a,#$01				;
	bra	swsx0
;................................................
; vcmd f2 - pitch envelope (attack)
swsx:
	mov	a,#$00
swsx0:
	mov	!swsk+x,a			; a = 0
;
	;call	data_in			; data in & inc add
	mov	a,y
	mov	!swshc+x,a			; hold
;
	call	data_in			; data in & inc add

	mov	!_03e1+x,a
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin
	pop	a
	beq	+
	mov	a,#$00
+
	mov	!swsc+x,a			; count
;
	call	data_in			; data in & inc add
	mov	!swss+x,a			; + @
	ret
;................................................
;		sweep off		; a = 0
;................................................
; vcmd f3 - pitch envelope off
sofx:
	mov	!swsc+x,a			;
	mov	!_03e1+x,a
	ret
;************************************************
;		part vol set
;************************************************
; vcmd ed - volume
pv1x:
	;call	data_in			;;0x
	mov	!pvod+x,a			; vol set
	mov	a,#$00
	mov	!pvodw+x,a
	ret
;************************************************
;		part vol move
;************************************************
; vcmd ee - volume fade
pv2x:
	;call	data_in			; data in & inc add
	mov.b	!pvoc+x,a		; vol (count)
	push	a				; count --> x
;
	call	data_in			; data in & inc add
	mov	!pvom+x,a			; vol (mokuteki)
;......
	setc
	sbc	a,!pvod+x			; vol (now data)
	pop	x					; count --> x
;
	call	divx			; x=count a=sa c=+,-
;......
	mov	!pvoadw+x,a			; + shimo
	mov	a,y					; kami
	mov	!pvoad+x,a			; + kami
	ret
;************************************************
;		part tune
;************************************************
; vcmd f4 - tuning
tunx:
	mov	!_03e0+x,a
	mov	a,!_03a0+x
	bne	+
	mov	a,!_03e0+x
	mov	!tund+x,a
+
	ret
;************************************************
;		rythm pattern
;************************************************
; vcmd ef - call subroutine
patx:
	;call	data_in			; data in & inc add
	mov	!adp+x,a			; pattern add. (low)
	call	data_in			; data in & inc add
	mov	!adp+1+x,a			; pattern add. (high) ; $0240/1+X - destination (arg1/2)
;
	call	data_in			; data in & inc add
	mov.b	!ptc+x,a		; rythm pattern count ; repeat count from arg3
;
	mov.b	a,!add+x		; add taihi
	mov	!adt+x,a
	mov.b	a,!add+1+x
	mov	!adt+1+x,a			; $0230/1+X - return addr
; jump to $loop destination
addset:
	mov	a,!adp+x			; pattern add. (low)
	mov.b	!add+x,a
	mov	a,!adp+1+x			; pattern add. (high)
	mov.b	!add+1+x,a
	ret
;************************************************
;		echo on channel & volume
;************************************************
; vcmd f5 - echo vbits/volume
ecvx:
	;call	data_in			; data in & inc add
	mov	!_03c3,a
	mov.b	!eons,a			; echo channel set
;
	call	data_in			; data in & inc add
	mov	a,#$00
	movw	!evolw,ya
;
	call	data_in			; data in & inc add
	mov	a,#$00
	movw	!evorw,ya
;
	clr5	!flgs			; write enable
;
	ret
;************************************************
;		echo volume move
;************************************************
; vcmd f8 - echo volume fade
ev2x:
	;call	data_in			; data in & inc add
	mov.b	!evoc,a			; evol (count)
;......
	call	data_in			; data in & inc add
	mov.b	!evolm,a		; evol (mokuteki)
;...
	setc
	sbc.b	a,!evol			; evol (now data)
	mov.b	x,!evoc			; count --> x
;
	call	divx			; x=count a=sa c=+,-
	movw	!evoladw,ya		; + @
;...................
	call	data_in			; data in & inc add
	mov.b	!evorm,a		; evor (mokuteki)
;...
	setc
	sbc.b	a,!evor			; evor (now data)
	mov.b	x,!evoc			; count --> x
;
	call	divx			; x=count a=sa c=+,-
	movw	!evoradw,ya		; + @
	ret
;************************************************
;		echo off
;************************************************
; vcmd f6 - disable echo
eofx:
	movw	!evolw,ya		; ya = 00 ; zero echo vol L shadow
	movw	!evorw,ya		; EVOL "00" set ; zero echo vol R shadow
;
	set5	!flgs			; write disable ; disable echo write
	ret
;************************************************
;		echo delay time & feed back
;************************************************
; vcmd f7 - set echo params
edlx:
	;call	data_in			; data in & inc add
	call	esaset			; EDL & ESA set ; set echo delay from arg1
;
	call	data_in			; data in & inc add
	mov.b	!efbs,a			; EFB = feed back ; set echo feedback shadow from arg2
;
	call	data_in			; data in & inc add
;
filset:
	mov	y,#$08				; a = fil no.
	mul	ya
	mov	x,a					; table add.
	mov	y,#$0f				; tenso address set
;
filset2:
	mov	a,fild+x			; filter table
	call	apus			; a=data  y= address 
;
	inc	x
	mov	a,y
	clrc
	adc	a,#$10
	mov	y,a
	bpl	filset2				; until 07fh ; set echo filter from table index arg3
;
	mov.b	x,!chn
	ret
;........................................
; set echo delay to A
esaset:
	mov.b	!eclr,a			; echo delay time
;
	mov	y,#$7d				; EDL = delay time
	mov	!apuadd,y
	mov	a,!apudt			; set echo delay
	cmp.b	a,!eclr
	beq	esaset4				; same as $4d?
;......
	and	a,#$0f
	eor	a,#$ff
	bbc7	!ekin,esaset1	; kinshi chu ?
	clrc
	adc.b	a,!ekin
esaset1:
	mov.b	!ekin,a			; echo kinshi time
;
	mov	y,#$04
esaset2:
	mov	a,dseta-1+y			; EON EFB EVOL EVOR ; shadow reg DSP reg table
	mov	!apuadd,a			; write address 
	mov	a,#$00
	mov	!apudt,a			; data write
	dbnz	y,esaset2		; zero echo vol, feedback, vbit DSP regs
;
	mov.b	a,!flgs
	or	a,#$20
	mov	y,#$6c				; FLG echo off
	call	apus			; a=data  y=address ; set FLG from shadow but disable echo
;
	mov.b	a,!eclr
	mov	y,#$7d				; EDL = delay time
	call	apus			; a=data  y=address ; set echo delay from $4d
;......
esaset4:
	asl	a					; ESA set
	asl	a
	asl	a
	eor	a,#$ff
	setc
	adc	a,#$3c				; 0ffh = echo end add.  ** henko **
	mov	y,#$6d				; ESA = echo start add.
	jmp	apus				; a=data  y=address ; set echo region to $3c00-8*delay
;************************************************
;		source count
;************************************************
; vcmd fa - set perc patch base
wavx:	mov.b	!wavs,a		;
	ret						;
;************************************************
;		sel dammy
;************************************************
;selx:	call	add_inc		;	!! test !!
;	ret						;	!! test !!
;************************************************
;		sound cut
;************************************************
;cutx:	inc	a				;	!! test !!
;	mov	!cutk+x,a			;	!! test !!
;	ret						;	!! test !!
;************************************************
;		F.F. set
;************************************************
;fftx:		inc	a			;	!! test !!
;************************************************
;               F.F. clear
;************************************************
;plyx:		mov	!ffk,a		; 	 !! test !!
;		jmp	ks04			; keyoff !! test !!
; vcmd f9 - pitch slide
_B5D:
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin			; kinshi flag check
	pop	a
	beq	swpx
	mov	!sss,#$02
	bra	_B7D
;................................................
;************** sweep check (next data) *********
;................................................
swpch:
	mov.b	a,!swpc+x		;
	bne	swpadsetr
;
	mov	a,(!add+x)			; next data check
	cmp	a,#!swp
	bne	swpadsetr			; not [swp] ?
;......
	mov.b	a,!keyd
	and.b	a,!fkin
	beq	+
	mov	!sss,#$04
_B7D:
	call	add_inc			; inc add
	dbnz	!sss,_B7D
	bra	swpadsetr
;......
+
	call	add_inc			; inc add
	call	data_in			; data in & inc add
swpx:
	mov.b	!swphc+x,a		; sweep (hold)
;
	call	data_in			; data in & inc add
	mov.b	!swpc+x,a		; sweep (counter)
;
	call	data_in			; data in & inc add
	clrc					; key trans. add.
	adc.b	a,!ktps			; add global transpose
	adc	a,!ptps+x			; per-voice transpose
;................................................
; calculate portamento delta
swpadset:
	and	a,#$7f				; $
	mov	!swpm+x,a			; sweep (mokuteki) ; final portamento value
;......
	setc					;
	sbc	a,!swpd+x			; moku - now ; note number
;
	mov.b	y,!swpc+x		; sweep count ; portamento steps
	push	y
	pop	x					; count --> x
;
	call	divx			; x=count a=sa c=+,-
	mov	!swpadw+x,a			; + shimo
	mov	a,y
	mov	!swpad+x,a			; + kami ; portamento delta
swpadsetr:
	ret
;........................................
swpdset:
	mov	a,!swpd+x			; kkk sss <-- swpd swpdw
	mov.b	!kkk,a
	mov	a,!swpdw+x			;
	mov.b	!sss,a
	ret
;................................................
;************** div keisan  from tp2 & mv2 & pam & swp (x=count a=sa)
;................................................
; signed 16 bit division
divx:
	notc					; c=1 plus
	ror.b	!ttt			; data store
	bpl	div10				; lpus ?
;......
	eor	a,#$ff				; minus
	inc	a
;......
div10:
	mov	y,#$00				; sa --> 00 sa ( y a )
	div	ya,x				; 00 sa / count --> a ... y
	push	a				; kami
;
	mov	a,#$00
	div	ya,x				; sa 00  / count --> a
	pop	y					; ya data set
	mov.b	x,!chn			;
;...................
minusc:
	bbc7	!ttt,divr		; ttt d7=1 ?
;
	movw	!adx,ya			; minus
	movw	ya,!t00
	subw	ya,!adx
divr:
	ret
;................................................
; vcmd dispatch table ($0a9c)
spft:
	dw snox					; e0 - set instrument
	dw panx					; e1 - pan
	dw pamx					; e2 - pan fade
	dw vibx					; e3 - vibrato on
	dw vofx					; e4 - vibrato off
	dw mv1x					; e5 - master volume
	dw mv2x					; e6 - master volume fade
	dw tp1x					; e7 - tempo
	dw tp2x					; e8 - tempo fade
	dw ktpx					; e9 - global transpose
	dw ptpx					; ea - per-voice transpose
	dw trex					; eb - tremolo on
	dw tofx					; ec - tremolo off
	dw pv1x					; ed - volume
	dw pv2x					; ee - volume fade
	dw patx					; ef - call subroutine
	dw vchx					; f0 - vibrato fade
	dw swkx					; f1 - pitch envelope (release)
	dw swsx					; f2 - pitch envelope (attack)
	dw sofx					; f3 - pitch envelope off
	dw tunx					; f4 - tuning
	dw ecvx					; f5 - echo vbits/volume
	dw eofx					; f6 - disable echo
	dw edlx					; f7 - set echo params
	dw ev2x					; f8 - echo volume fade
	dw _B5D					; f9 - pitch slide
	dw wavx					; fa - set perc patch base
	;dw tunx,ecvx,eofx,edlx,ev2x,swpx,wavx
	;dw selx,cutx,fftx,plyx	; !! test !!
; fb-ff undefined

; vcmd lengths ($0b32)
spfp:
	db $01, $01, $02, $03, $00, $01, $02, $01	; e0-e7
	db $02, $01, $01, $03, $00, $01, $02, $03	; e8-ef
	db $01, $03, $03, $00, $01, $03, $00, $03	; f0-f7
	db $03, $03, $01							; f8-fa
;	db $02, $00, $00, $00	; !! test !!
;................................................
;
;
;
;************************************************
;		part vol move
;************************************************
; do voice fades
voly:
	mov.b	a,!pvoc+x		; vol move chu ? ; voice volume fade counter
	beq	trey
;................................................
	mov	a,#$00
	mov	y,#$03
	dec.b	!pvoc+x			; dec voice vol fade counter
	call	_CC1

;************************************************
;               tremolo check                      
;************************************************
trey:
	mov.b	y,!tred+x		; tre chu ?
	beq	tre22
;
	mov	a,!trehs+x			; hold chu ?
	cbne	!trehc+x,tre20
;................................................
	or	(!vols),(!keyd)		; vol set flag  
;......
	mov	a,!trec+x			; trec = a
	bpl	tre02				; trec = 080h ijo ?

	inc	y					; tred = 0ffh ?
	bne	tre02

	mov	a,#$80
	bra	tre04
;......
tre02:
	clrc					; speed keisan
	adc	a,!trecad+x
tre04:
	mov	!trec+x,a			; count data
;
	call	treset			; volx set
	bra	pany
;................................................
tre20:
	inc.b	!trehc+x		; hold chu
;
tre22:
	mov	a,#$ff				; y = depth (tre)
	call	volxset			; volx set
;************************************************
;		pan move & gain set
;************************************************
pany:
	mov.b	a,!panc+x		; pan move chu ?
	beq	pan10
;..............................................
	mov	a,#$30
	mov	y,#$03
	dec.b	!panc+x
	call	_CC1
;..............................................
pan10:
	mov.b	a,!keyd
	and.b	a,!vols			; vol set ?
	beq	panr
;...
	mov	a,!pand+x			; kami
	mov	y,a
	mov	a,!pandw+x			; shimo
	movw	!sss,ya
;................................................
pan20:
	mov	a,x					; kkk sss --> pand set
	xcn	a
	lsr	a					; apuch
	mov.b	!ttt,a			; r.gain = 0
;................................................
pan30:
	mov.b	y,!kkk			; right gain keisan
	mov	a,pant+1+y			; next pan val from table
	setc
	sbc	a,pant+y			; sa --> a ; pan val
	mov.b	y,!sss			; shimo
	mul	ya					; sa x 0.???
	mov	a,y					; --> a
;
	mov.b	y,!kkk			; kami
	clrc
	adc	a,pant+y			; pan data --> a ; add integer part to pan val
	mov	y,a
;
	mov	!_0250+x,a			; volume
	mov	a,!volx+x			; gain data set
	mul	ya					;
;
	mov	a,!panf+x			; bits 7/6 will negate volume L/R
	asl	a
	bbc0	!ttt,pan32
	asl	a
pan32:
	mov	a,y
	bcc	pan34
;
	eor	a,#$ff
	inc	a
;
pan34:
	mov.b	y,!ttt			; write address
	call	apusx			; a=data  y=address
;................................................
	mov	y,#20				; left gain keisan
	mov	a,#$00
	subw	ya,!sss			; 20.00 - kkk sss 
	movw	!sss,ya
	inc.b	!ttt			; l.gain = 1
	bbc1	!ttt,pan30
;
panr:
	ret
;................................................
_CC1:
	or	(!vols),(!keyd)		; vol set flag 
_CC4:
	movw	!adx,ya
	movw	!adx+2,ya
	push	x
	pop	y
	clrc
	bne	_CD7
;...
	adc	!adx+2,#$1f
	mov	a,#$00
	mov	(!adx)+y,a
	inc	y
	bra	+
;...
_CD7:
	adc	!adx+2,#$10
	call	_CDE
	inc	y
;
_CDE:
	mov	a,(!adx)+y
+
	adc	a,(!adx+2)+y
	mov	(!adx)+y,a
	ret
;************************************************
;		keyoff check
;************************************************
keych:
; do readahead
	mov.b	a,!ngo+x		; key off ?
	beq	swpy
;
	dec.b	!ngo+x			; key off ?
	beq	key02
;
	mov	a,#$02
	cbne	!ngc+x,swpy
;...................
key02:
	mov.b	a,!ptc+x		; pattern count
	mov.b	!adx+3,a

	mov.b	a,!add+x		; address set
	mov.b	y,!add+1+x
key04:
	movw	!adx,ya
;
	mov	y,#$00
;......
key10:
	mov	a,(!adx)+y			; data in
	beq	key16				; block end ?
	bmi	key14
-
	inc	y
	bmi	key20
	mov	a,(!adx)+y
	bpl	-
;
key14:
	cmp	a,#!xxx				; xxx ?
	beq	swpy				; = tai
;
	cmp	a,#!pat
	beq	key18				; pat ?
;
	cmp	a,#!sno
	bcc	key20
;...
	push	y				; special flag
	mov	y,a
	pop	a
	adc	a,spfp-!sno+y		; c=1 ; vcmd lengths
	mov	y,a
	bra	key10
;...................
key16:
	mov.b	a,!adx+3		; pattern chu ?
	beq	key20
;......
	dec.b	!adx+3			; pattern end ?
	bne	key17
; read $0230/1+X into YA
	mov	a,!adt+1+x			; add restore (pattern end)
	push	a
	mov	a,!adt+x
	pop	y
	bra	key04				;
; read $0240/1+X into YA
key17:
	mov	a,!adp+1+x			; pattern add. (high)
	push	a
	mov	a,!adp+x			; pattern add. (low)
	pop	y
	bra	key04
;......
key18:
	inc	y					; pat
	mov	a,(!adx)+y			; data in
	push	a				; add. low
	inc	y					;
	mov	a,(!adx)+y			; data in
	mov	y,a					; add. high
	pop	a
	bra	key04				;
;...................
key20:
	mov.b	a,!keyd			; key off set
	mov	y,#!keyoff
	call	apusx			; keyoff set (a=keyd)
;************************************************
;		sweep check
;************************************************
swpy:
	clr7	!uuu			; sweep chu flag
;
	mov.b	a,!swpc+x		; sweep chu ?
	beq	viby
;................................................
	mov.b	a,!swphc+x		; hold chu ?
	beq	swp20
;
	dec.b	!swphc+x		; hold chu
	bra	viby
;................................................
swp20:
	mov.b	a,!fkin
	and.b	a,!keyd
	bne	viby
;
	set7	!uuu			; sweep chu flag
;........................................
	mov	a,#$60
	mov	y,#$03
;
	dec.b	!swpc+x			; sweep keisan
	call	_CC4
;************************************************
;		vib check
;************************************************
viby:
	call	swpdset			; kkk sss <-- swpd swpdw

	mov.b	a,!vibd+x		; vib chu ?
	beq	vib12
;
	mov	a,!vibhs+x
	cbne	!vibhc+x,vib11	; hold chu ?
;................................................
	mov	a,!vibcc+x			;
	cmp	a,!vibcs+x
	bne	vib15				; change chu ?
;...
	mov	a,!vibdm+x			; vib change end !
	bra	vib17
;......
vib15:
	setp					; change chu
	inc.b	!vibcc+x
	clrp
;
	mov	y,a					; !vibcc+x = 0 ?
	beq	vib16				; change begin (a=0)
;
	mov.b	a,!vibd+x		; change chu
vib16:
	clrc
	adc	a,!vibad+x			;
vib17:
	mov.b	!vibd+x,a
;................................................
vib18:
	mov	a,!vibc+x			; vib keisan
	clrc
	adc	a,!vibcad+x
	mov	!vibc+x,a			; count data
;................................................
vib20:
	mov.b	!ttt,a			; depth keisan
;
	asl	a
	asl	a
	bcc	vib21				; count data d6=0 ?
;
	eor	a,#$ff
;......
vib21:
	mov	y,a
	mov.b	a,!vibd+x		; vib depth (%)
	cmp	a,#$f1
	bcc	vib24
;
vib22:
	and	a,#$0f				;
	mul	ya
	bra	vib25
;
vib24:
	mul	ya
	mov	a,y					; shosuten ika
	mov	y,#$00				; kami
;
vib25:
	call	minusad			; if ttt(d7)=1 then minus + sss
;......
;	addw	ya,sss			; vib keisan
;	movw	sss,ya			; data set
;................................................
vib40:
	jmp	dssx				; fre. set (call)
;................................................
vib11:
	inc.b	!vibhc+x		; hold chu
vib12:
	bbs7	!uuu,vib40		; sweep chu ?
	ret
;................................................
;
;
;................................................
;************** tremolo check *******************
;................................................
trry:
	clr7	!uuu			; tre chu flag
;
	mov.b	a,!tred+x		; tre chu ?
	beq	pnny
;
	mov	a,!trehs+x			; holdchu ?
	cbne	!trehc+x,pnny
;................................................
	call	tresetx			; voice vol calculations
;................................................
;************** pan move check ******************
;................................................
pnny:
	mov	a,!pand+x			; kami
	mov	y,a
	mov	a,!pandw+x			; shimo
	movw	!sss,ya			; $10/1 = voice pan value
;...
	mov.b	a,!panc+x		; pan move chu ? ; voice pan fade counter
	beq	pnn04
;...
	mov	a,!panad+x
	mov	y,a
	mov	a,!panadw+x			; + @ keisan ; pan fade delta
;
	call	hokan			; kkk sss <-- data set ; add delta (with mutations)?
;...
pnn04:
	bbc7	!uuu,sppy
;......
	call	pan20			; vol data set
;................................................
;************** sweep check *********************
;................................................
sppy:
	clr7	!uuu			; sweep chu flag
;
	call	swpdset			; kkk sss <-- swpd swpdw
;...
	mov.b	a,!swpc+x		; sweep chu ?
	beq	vbby
;
	mov.b	a,!swphc+x		; hold chu?
	bne	vbby
;...................
	mov	a,!swpad+x
	mov	y,a
	mov	a,!swpadw+x
;
	call	hokan			; kkk sss <-- data set 
;................................................
;************** vib check ***********************
;................................................
vbby:
	mov.b	a,!vibd+x		; vib chu ?
	beq	vib12				; uuu d7 check & ret
;
	mov	a,!vibhs+x
	cbne	!vibhc+x,vib12	; hold chu ?
;................................................
vbb10:
	mov.b	y,!tmpd
	mov	a,!vibcad+x			; vib keisan
	mul	ya
	mov	a,y
	clrc
	adc	a,!vibc+x			; vib count
	jmp	vib20				; depth keisan & data set
;................................................
;************** hokan keisan ********************
;................................................
hokan:
	set7	!uuu			; from sppy
	mov.b	!ttt,y			; data store
;...
	call	minusc			; if ttt(d7)=1 then minus
;
	push	y
	mov.b	y,!tmpd			; X 0.???
	mul	ya					; a = shimo
	mov.b	!adx,y
	mov	!adx+1,#$00
;
	mov.b	y,!tmpd
	pop	a					; a = kami
	mul	ya
	addw	ya,!adx
;
minusad:
	call	minusc			; if ttt(d7)=1 then minus
;...
	addw	ya,!sss			; sweep keisan
	movw	!sss,ya			; data set
	ret
;................................................
;************** tremolo data set ****************
;................................................
tresetx:
	set7	!uuu			; call from trr
;
	mov.b	y,!tmpd
	mov	a,!trecad+x			; tre keisan

	mul	ya
	mov	a,y
	clrc
	adc	a,!trec+x
;................................................
treset:
	asl	a					; volx set
	bcc	treset2
;
	eor	a,#$ff
;
treset2:
	mov	y,!tred+x
	mul	ya					; tre depth x wave
;
	mov	a,y
	eor	a,#$ff				; = 1 - depth
;...................
volxset:
	mov.b	y,!mvo			; main vol x ( 1 - depth )
	mul	ya					;
;
	mov	a,!vol+x			; vol
	mul	ya
;
	mov	a,!pvod+x			; part vol x
	mul	ya
;
	mov	a,y					; dB
	mul	ya					; dB
;
	mov	a,y
	mov	!volx+x,a
;
	ret
;................................................
;
;
;..............................................
; pan table
pant:   ; 0 - 20
	db 000, 001, 003, 007, 013, 021, 030, 041, 052, 066
	db 081, 094, 103, 110, 115, 119, 122, 124, 125, 126, 127
;
;*	db 127, 126, 126, 125, 123, 120, 116, 111, 105, 098
;*	db 089, 080, 070, 060, 050, 040, 030, 022, 014, 006, 000
;..............................................
;
;
;................................................
; echo FIR presets
fild:  ;                                      ; Filter    (0xfh)
	db $7f, $00, $00, $00, $00, $00, $00, $00 ; no filter (x1.0)
	db $58, $bf, $db, $f0, $fe, $07, $0c, $0c ; high pass
	db $0c, $21, $2b, $2b, $13, $fe, $f3, $f9 ; low  pass
	db $34, $33, $00, $d9, $e5, $01, $fc, $eb ; band pass
;................................................ 
; EVOL(L),EVOL(R),EFB,EON,FLG,KOL,KOF,NON,PMON,KOF
dseta: ;   EVOL EVOR EFB  EON  FLG                   NOOF PMON
	db $2c, $3c, $0d, $4d, $6c, !keyon, !keyoff, $3d, $2d, !keyoff
; dsp shadow addrs ($0e27+1) for dsp regs ($0e1d+1)
dsetd: ;    1      2      3      4      5      6        7     8     9       10  
	db !evol, !evor, !efbs, !eons, !flgs, !keyons, !t00, !nons, !mons, !keyoffs
;................................................
;
;
;
;
;************************************************
; pitch table
gfd:	;c00  c01  d00  d01  e00  f00  f01  g00  g01  a00  a01  b00  1.0594631
; dw	0066,0070,0075,0079,0084,0089,0094,0100,0106,0112,0119,0126  ; c00
; dw	0133,0141,0150,0159,0168,0178,0189,0200,0212,0225,0238,0252  ; c10
; dw	0267,0283,0300,0318,0337,0357,0378,0401,0425,0450,0477,0505  ; c20
; dw	0535,0567,0601,0637,0675,0715,0757,0802,0850,0901,0954,1011  ; c30
; dw	1071,1135,1202,1274,1350,1430,1515,1605,1701,1802,1909,2022  ; c40
	dw 2143,2270,2405,2548,2700,2860,3030,3211,3402,3604,3818,4045  ; c50
	dw 4286;4541,4811,5097,5400,5721,6061,6422,6804,7208,7637,8091  ; c60
;
; 3=e60 , 4=b50 , 5=g50 , 6=e50 , 7=c51 , 8=b40 , 9=a40 , 10=g40
;************************************************	; 7.6560747 = 07.a8h
;
;************************************************
	db "*Ver S1.20*"		; ** version check **
;************************************************
;
;
;***************************************
;		tensou program
;***************************************
ten00:
	mov	a,#$aa				;
	mov	!port0,a			;
	mov	a,#$bb				;
	mov	!port1,a			;
;........................................
ten02:
	mov	a,!port0			; flag O.K. ?
	cmp	a,#$cc				;
	bne	ten02				;
	bra	ten40				;
;........................................
ten16:
	mov	y,!port0			;
	bne	ten16				;
;........................................
ten20:
	cmp	y,!port0			;
	bne	ten26
;......
	mov	a,!port1
	mov	!port0,y
	mov	(!adx)+y,a			;
;
	inc	y
	bne	ten20				;
;......
	inc.b	!adx+1			;
	bra	ten20				;
;........................................
ten26:
	bpl	ten20				;
;
	cmp	y,!port0			;
	bpl	ten20				;
;........................................
ten40:
	mov	a,!port2			;
	mov	y,!port3			;
	movw	!adx,ya			;
;
	mov	y,!port0			;
	mov	a,!port1			;
	mov	!port0,y			; flag return
	bne	ten16				; port1 = 0 ?
;........................................
	mov	x,#$31				; in port clear
	mov	!cont,x

	ret
;........................................

; 0F21-0FDF.bin
; Table of SFX IDs to trigger alongside the primary SFX ID that was called.
; Multiple SFX IDs can be chained together via this list.
; Zero means don't trigger another one.
sfx_chain_table:
	db $BF, $BF, $BE, $00, $0C, $0C, $00, $00 ; $01 - $08
	db $00, $00, $00, $00, $00, $AF, $B2, $B0 ; $09 - $10
	db $B1, $00, $AE, $00, $10, $10, $10, $10 ; $11 - $18
	db $00, $AB, $00, $00, $BD, $BC, $B8, $B6 ; $19 - $20
	db $BA, $00, $00, $00, $00, $00, $00, $00 ; $21 - $28
	db $00, $00, $00, $00, $00, $00, $00, $21 ; $29 - $30
	db $00, $B9, $00, $00, $00, $00, $00, $00 ; $31 - $38
	db $00, $00, $B3, $00, $00, $00, $00, $00 ; $39 - $40
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $41 - $48
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $49 - $50
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $51 - $58
	db $00, $00, $B4, $00, $00, $00, $00, $00 ; $59 - $60
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $61 - $68
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $69 - $70
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $71 - $78
	db $AA, $00, $00, $00, $00, $00, $00, $00 ; $79 - $80
	db $AD, $00, $AC, $00, $00, $00, $00, $00 ; $81 - $89
	db $00, $00, $00, $00, $A9, $A8, $00, $00 ; $89 - $90
	db $A7, $00, $00, $00, $00, $00, $00, $00 ; $91 - $98
	db $00, $00, $00, $00, $A6, $A6, $00, $00 ; $99 - $A0
	db $A2, $A3, $A4, $88, $00, $00, $00, $00 ; $A1 - $A8
	db $00, $00, $00, $00, $00, $2A, $00, $00 ; $A9 - $B0
	db $00, $00, $00, $00, $00, $B5, $00, $B7 ; $B1 - $B8
	db $00, $00, $79, $BB, $00, $00, $00      ; $B9 - $BF

; 0FE0-109E.bin
; This table defines the parameters of each SFX ID.
; Each byte consists of two nibbles ($xy).
; $x - voice ID to use.
; $y - SFX priority.
; Higher values have higher priority, and ties overwrite each other.
sfx_parameters:
	db $3F, $3F, $5D, $5C, $5C, $5C, $5A, $5A ; $01 - $08
	db $5A, $5A, $2D, $2D, $25, $3E, $3E, $3A ; $09 - $10
	db $10, $14, $59, $59, $2D, $2D, $2D, $5C ; $11 - $18
	db $5B, $59, $3E, $3E, $1F, $46, $46, $46 ; $19 - $20
	db $45, $44, $43, $42, $41, $40, $41, $40 ; $21 - $28
	db $40, $39, $45, $45, $45, $45, $35, $57 ; $29 - $30
	db $56, $58, $58, $50, $50, $50, $50, $32 ; $31 - $38
	db $45, $45, $45, $42, $41, $40, $42, $42 ; $39 - $40
	db $42, $41, $40, $42, $42, $42, $41, $40 ; $41 - $48
	db $42, $41, $40, $45, $45, $45, $40, $45 ; $49 - $50
	db $46, $45, $45, $45, $45, $51, $43, $12 ; $51 - $58
	db $45, $45, $45, $42, $42, $42, $39, $39 ; $59 - $60
	db $39, $39, $39, $39, $58, $45, $39, $43 ; $61 - $68
	db $43, $43, $43, $43, $31, $41, $41, $46 ; $69 - $70
	db $46, $46, $46, $43, $43, $43, $43, $43 ; $71 - $78
	db $1E, $39, $39, $39, $39, $39, $39, $45 ; $79 - $80
	db $46, $45, $45, $46, $35, $45, $46, $45 ; $81 - $89
	db $54, $30, $45, $39, $45, $46, $32, $45 ; $89 - $90
	db $39, $45, $42, $42, $46, $46, $32, $45 ; $91 - $98
	db $46, $44, $34, $41, $45, $45, $46, $46 ; $99 - $A0
	db $35, $25, $15, $03, $00, $15, $43, $36 ; $A1 - $A8
	db $35, $0E, $49, $35, $36, $2D, $4E, $44 ; $A9 - $B0
	db $40, $5A, $33, $25, $23, $33, $24, $34 ; $B1 - $B8
	db $36, $34, $25, $35, $0F, $2E, $2F      ; $B9 - $BF

; $109F SFX pointer table begins, 2 bytes, 191 pointers
; Sound labels taken from SOUNDEQU.INC and SOUND.ASM where available, otherwise based on descriptions.
; Sound descriptions taken from SFEX.
; SFX IDs past $AF aren't normally accessible, and are chained with other SFX IDs.
sfx_ptrs:
	dw se_pause						; $01 UNPAUSE
	dw se_pause						; $02 PAUSE
	dw se_playerdown				; $03 PLAYER DOWN
	dw se_playerdamage				; $04 PLAYER DAMAGE
	dw se_wingdestructleft			; $05 LEFT WING DAMAGED
	dw se_wingdestructright			; $06 RIGHT WING DAMAGED
	dw se_wingdamageleft			; $07 LEFT WING CRASH
	dw se_wingdamageright			; $08 RIGHT WING CRASH
	dw se_wingtouchleft				; $09 LEFT WING SCRATCH
	dw se_wingtouchright			; $0A RIGHT WING SCRATCH
	dw se_warning1					; $0B INCOMING ENEMY
	dw se_warning2					; $0C WING DAMAGED
	dw se_comeincorneria			; $0D COME IN CORNERIA
	dw se_bonuscredit				; $0E BONUS CREDIT
	dw se_gateofring				; $0F BIG SUPPORT RING
	dw se_itemcatch					; $10 1UP RING HIT
	dw se_cursor					; $11 CONTROLS SELECT
	dw se_percentagering			; $12 PERCENTAGE RING
	dw se_goodluck					; $13 GOOD LUCK
	dw se_conehit					; $14 CONE HIT
	dw se_twinblasterpowerup		; $15 TWIN BLASTER POWERUP
	dw se_shieldpowerup				; $16 SHIELD POWERUP
	dw se_wingrepairedpowerup		; $17 WING REPAIRED POWERUP
	dw se_bombpowerup				; $18 BOMB POWERUP
	dw se_winglessarwingcollision	; $19 WINGLESS ARWING COLLISION
	dw se_slotmachinecoin			; $1A SLOT MACHINE COIN
	dw se_smallarwingdamagealarm	; $1B SMALL ARWING DAMAGE ALARM
	dw se_bigarwingdamagealarm		; $1C BIG ARWING DAMAGE ALARM
	dw se_destructbosssmall			; $1D SMALL BOSS EXPLOSION
	dw se_destructbossnear			; $1E NEAR BIG BOSS EXPLOSION
	dw se_destructbossmid			; $1F MID BIG BOSS EXPLOSION
	dw se_destructbossfar			; $20 FAR BIG BOSS EXPLOSION
	dw se_destructenemynear			; $21 NEAR ENEMY EXPLOSION
	dw se_destructenemymid			; $22 MID ENEMY EXPLOSION
	dw se_destructenemyfar			; $23 FAR ENEMY EXPLOSION
	dw se_damageenemynear			; $24 NEAR ENEMY HIT
	dw se_damageenemymid			; $25 MID ENEMY HIT
	dw se_damageenemyfar			; $26 FAR ENEMY HIT
	dw se_hitwallnear				; $27 NEAR LASER DEFLECT
	dw se_hitwallmid				; $28 MID LASER DEFLECT
	dw se_hitwallfar				; $29 FAR LASER DEFLECT
	dw se_goodlucksubsubsfx			; $2A GOOD LUCK SUB-SUB-SFX
	dw se_enemywarpin				; $2B ENEMY WARP-IN
	dw se_conetriangle				; $2C CONE TRIANGLE
	dw se_bossshadowing				; $2D 1-6+2-3 BOSS SHADOWING
	dw se_plasmahydraarmhit			; $2E PLASMA HYDRA ARM HIT
	dw se_rockcrusherroll			; $2F ROCK CRUSHER ROLL
	dw se_specialweapon				; $30 PLAYER BOMB EXPLOSION
	dw se_abutton					; $31 PLAYER BOMB SHOT
	dw se_speedup					; $32 PLAYER BOOST
	dw se_speeddown					; $33 PLAYER BRAKE
	dw se_twinlaser					; $34 PLAYER TWIN BLASTER SHOT
	dw se_laser						; $35 PLAYER LASER SHOT
	dw se_dualbeam					; $36 DUAL BEAM SHOT
	dw se_dualbeam					; $37 DUAL BEAM SHOT
	dw se_birdscream				; $38 BIRD SCREAM
	dw se_enemyrocketplayerhit		; $39 ENEMY ROCKET PLAYER HIT
	dw se_dodoraeggcrackbird		; $3A DODORA EGG CRACK + BIRD
	dw se_dodorahit					; $3B DODORA HIT
	dw se_missilenear				; $3C NEAR ENEMY ROCKET SHOT
	dw se_missilemid				; $3D MID ENEMY ROCKET SHOT
	dw se_missilefar				; $3E FAR ENEMY ROCKET SHOT
	dw se_movingwallleft			; $3F LEFT GATE MOVING
	dw se_movingwallcentre			; $40 CENTRE GATE MOVING
	dw se_movingwallright			; $41 RIGHT GATE MOVING
	dw se_movingwallmid				; $42 MID GATE MOVING
	dw se_movingwallfar				; $43 FAR GATE MOVING
	dw se_laserleft					; $44 LEFT ENEMY LASER SHOT
	dw se_lasercentre				; $45 CENTRE ENEMY LASER SHOT
	dw se_laserright				; $46 RIGHT ENEMY LASER SHOT
	dw se_lasermid					; $47 MID ENEMY LASER SHOT
	dw se_laserfar					; $48 FAR ENEMY LASER SHOT
	dw se_enemybattrynear			; $49 NEAR ENEMY BATTERY
	dw se_enemybattrymid			; $4A MID ENEMY BATTERY
	dw se_enemybattryfar			; $4B FAR ENEMY BATTERY
	dw se_phantron2landing			; $4C PHANTRON 2 LANDING
	dw se_phantron2jump				; $4D PHANTRON 2 JUMP
	dw se_unused					; $4E -UNUSED-
	dw se_dancinginsectorpropelling	; $4F DANCING INSECTOR PROPELLING
	dw se_bladebarrierpostdrillatk	; $50 BLADE BARRIER POST-DRILL ATK
	dw se_bladebarrierplayerwebhit	; $51 BLADE BARRIER PLAYER WEB HIT
	dw se_doorclosenear				; $52 LAST BASE ENTRY 2.DOOR CLOSE
	dw se_doorclosemidfar			; $53 FAR LAST BASE ENTRY 2.DOOR CLOSE
	dw se_dooropennear				; $54 LAST BASE ENTRY 2.DOOR OPEN
	dw se_dooropenmidfar			; $55 FAR LAST BASE ENTRY 2.DOOR OPEN
	dw se_playeramoebahit			; $56 PLAYER AMOEBA HIT
	dw se_blockadedirectionchange	; $57 BLOCKADE DIRECTION CHANGE
	dw se_hovering					; $58 HOVERING
	dw se_doorclose					; $59 DOOR CLOSE
	dw se_dooropen					; $5A DOOR OPEN
	dw se_hovering2					; $5B HOVERING
	dw se_ringlasernear				; $5C NEAR ENEMY RING SHOT
	dw se_ringlasermid				; $5D MID ENEMY RING SHOT
	dw se_ringlaserfar				; $5E FAR ENEMY RING SHOT
	dw se_pepperradiochat			; $5F PEPPER RADIO CHAT
	dw se_foxradiochat				; $60 FOX RADIO CHAT
	dw se_falcoradiochat			; $61 FALCO RADIO CHAT
	dw se_peppyradiochat			; $62 PEPPY RADIO CHAT
	dw se_slippyradiochat			; $63 SLIPPY RADIO CHAT
	dw se_radiochatquit				; $64 RADIO CHAT QUIT
	dw se_playercamerachange		; $65 PLAYER CAMERA CHANGE
	dw se_destructorweapnheadattack	; $66 DESTRUCTOR WEAPON HEAD ATTACK
	dw se_continue					; $67 CONTINUE LET'S GO
	dw se_enemyupsealeft			; $68 LEFT WATER SPLASH OUT
	dw se_enemyupseacentre			; $69 CENTRE WATER SPLASH
	dw se_enemyupsearight			; $6A RIGHT WATER SPLASH
	dw se_midwatersplash			; $6B MID WATER SPLASH
	dw se_farwatersplash			; $6C FAR WATER SPLASH
	dw se_dopright					; $6D LEFT OBJECT FLY-BY
	dw se_dopcentre					; $6E CENTRE OBJECT FLY-BY
	dw se_dopleft					; $6F RIGHT OBJECT FLY-BY
	dw se_atomicbasepowersupplyoff	; $70 ATOMIC BASE POWER SUPPLY OFF
	dw se_atomicbasepowersupplyon	; $71 ATOMIC BASE POWER SUPPLY ON
	dw se_atomicbasecoreclose		; $72 ATOMIC BASE CORE CLOSE
	dw se_atomicbasecoreopen		; $73 ATOMIC BASE CORE OPEN
	dw se_enemydownsealeft			; $74 LEFT WATER SPLASH IN
	dw se_enemydownseacentre		; $75 CENTRE WATER SPLASH IN
	dw se_enemydownsearight			; $76 RIGHT WATER SPLASH IN
	dw se_midwatersplashin			; $77 MID WATER SPLASH IN
	dw se_farwatersplashin			; $78 FAR WATER SPLASH IN
	dw se_backgroundthunder			; $79 BACKGROUND THUNDER
	dw se_falcoradiochathit			; $7A FALCO RADIO CHAT HIT
	dw se_falcoradiochatdown		; $7B FALCO RADIO CHAT DOWN
	dw se_peppyradiochathit			; $7C PEPPY RADIO CHAT HIT
	dw se_peppyradiochatdown		; $7D PEPPY RADIO CHAT DOWN
	dw se_slippyradiochathit		; $7E SLIPPY RADIO CHAT HIT
	dw se_slippyradiochatdown		; $7F SLIPPY RADIO CHAT DOWN
	dw se_phantron2hit				; $80 PHANTRON 2 HIT
	dw se_phantron2scream			; $81 PHANTRON 2 SCREAM
	dw se_rockcrusherappears		; $82 ROCK CRUSHER APPEARS
	dw se_destructorengine			; $83 DESTRUCTOR ENGINE
	dw se_phantronappears			; $84 PHANTRON APPEARS
	dw se_rockcrusheruncover		; $85 ROCK CRUSHER UNCOVER
	dw se_pilontoground				; $86 PILON TO GROUND
	dw se_androssappears			; $87 ANDROSS APPEARS
	dw se_androsshit				; $88 ANDROSS HIT
	dw se_textting					; $89 TEXT TING
	dw se_silence					; $8A SILENCE
	dw se_prewingrepaired			; $8B PRE-WING REPAIRED
	dw se_androssradiochat			; $8C ANDROSS RADIO CHAT
	dw se_metalsmashersmashing		; $8D METAL SMASHER SMASHING
	dw se_metalsmasherclose			; $8E METAL SMASHER CLOSE
	dw se_bladebarrierwebattack		; $8F BLADE BARRIER WEB ATTACK
	dw se_bonusringbird				; $90 BONUS RING BIRD
	dw se_cometflyby				; $91 COMET FLY-BY
	dw se_whalescream				; $92 WHALE SCREAM
	dw se_stingrayhit				; $93 STINGRAY HIT
	dw se_squidhit					; $94 SQUID HIT
	dw se_spinningcorebgthunder		; $95 SPINNING CORE BG THUNDER
	dw se_phantron2scream			; $96 PHANTRON 2 SCREAM
	dw se_dancinginsectormovement	; $97 DANCING INSECTOR MOVEMENT
	dw se_dancinginsectorfireshot	; $98 DANCING INSECTOR FIRE SHOT
	dw se_dancinginsectorfireflyby	; $99 DANCING INSECTOR FIRE FLY-BY
	dw se_volcanofire				; $9A VOLCANO FIRE
	dw se_slotmachinehandledown		; $9B SLOT MACHINE HANDLE DOWN
	dw se_slotmachineslotspinning	; $9C SLOT MACHINE SLOT SPINNING
	dw se_professorhangerappears	; $9D PROFESSOR HANGER APPEARS
	dw se_professorhangerdisappears	; $9E PROFESSOR HANGER DISAPPEARS
	dw se_finalscorescreenflight	; $9F FINAL SCORE SCREEN FLIGHT
	dw se_androssshellexplosion		; $A0 ANDROSS SHELL EXPLOSION
	dw se_androssscream4			; $A1 ANDROSS SCREAM 4
	dw se_androssscream3			; $A2 ANDROSS SCREAM 3
	dw se_androssscream2			; $A3 ANDROSS SCREAM 2
	dw se_androssscream1			; $A4 ANDROSS SCREAM 1
	dw se_silence					; $A5 SILENCE
	dw se_enemyhovering				; $A6 ENEMY HOVERING
	dw se_shootingstar				; $A7 SHOOTING STAR
	dw se_objectimpact				; $A8 OBJECT IMPACT
	dw se_enemy						; $A9 ENEMY
	dw se_backgroundthunder2		; $AA BACKGROUND THUNDER
	dw se_slotmachinecoinsubsfx		; $AB SLOT MACHINE COIN SUB-SFX
	dw se_destructorengine2			; $AC DESTRUCTOR ENGINE
	dw se_phantron2screamsubsfx		; $AD PHANTRON 2 SCREAM SUB-SFX
	dw se_goodlucksubsfx			; $AE GOOD LUCK SUB-SFX
	dw se_bonuscreditsubsfx			; $AF BONUS CREDIT SUB-SFX
	dw se_itemcatchsubsfx			; $B0 1UP RING HIT SUB-SFX
	dw se_controlsselectsubsfx		; $B1 CONTROLS SELECT SUB-SFX
	dw se_gateofringsubsfx			; $B2 BIG SUPPORT RING SUB-SFX
	dw se_dodorahitsubsfx			; $B3 DODORA HIT SUB-SFX
	dw se_hovering2subsfx			; $B4 HOVERING SUB-SFX
	dw se_bigexplosionleft			; $B5 BIG EXPLOSION LEFT
	dw se_destructbossfarsubsfx		; $B6 FAR BIG BOSS EXPLOSION SUB-SFX
	dw se_destructbossmidsubsubsfx	; $B7 MID BIG BOSS EXPLOSION SUB-SUB-SFX
	dw se_destructbossmidsubsfx		; $B8 MID BIG BOSS EXPLOSION SUB-SFX
	dw se_speedupsubsfx				; $B9 BOOST SUB-SFX
	dw se_destructenemynearsubsfx	; $BA NEAR ENEMY EXPLOSION SUB-SFX
	dw se_destructbossnearsubsubsfx	; $BB NEAR BIG BOSS EXPLOSION SUB-SUB-SFX
	dw se_destructbossnearsubsfx	; $BC NEAR BIG BOSS EXPLOSION SUB-SFX
	dw se_destructbosssmallsubsfx	; $BD SMALL BOSS EXPLOSION SUB-SFX
	dw se_playerdownsubsfx			; $BE PLAYER DOWN SUB-SFX
	dw se_pausesubsfx				; $BF PAUSE SUB-SFX

; make sure pointer table isn't too big
%warnpc($121d)

; ===========================
; begin sound effect patterns
; ===========================

se_androssscream4: ; Andross scream 4
	%inst(2D)
	%dur($7F) : %vol($7D)
	%dur($64) : %pitch($81)

se_androssscream3: ; Andross scream 3
	%inst(2E)
	%dur($60) : %vol($7D) : %pitch($81)

se_androssscream2: ; Andross scream 2
	%inst(2D)
	%dur($5F) : %vol($64)
	%dur($7D) : %pitch($82)
	db $00

se_androssshellexplosion: ; Andross shell explosion
	%inst(06)
	%dur($0C) : %vol($64) : %pitch($C3)
	%pitch($BB)
	%dur($0E) : %pitch($A9)
	%pitch($B2)
	%dur($0F) : %pitch($BC)
	%pitch($B3)
	%dur($10) : %pitch($AB)
	%pitch($AD)
	%dur($12) : %pitch($9D)
	%pitch($A6)
	%pitch($A3)
	%dur($13) : %pitch($95)
	%pitch($90)
	%pitch($94)
	%dur($60) : %pitch($90)
	db $00

se_finalscorescreenflight: ; Final score screen flight
	%inst(03)
	%dur($5F) : %vol($3C)
	%pitchslide($97,$5F,$9A)
	%dur($7F) : %vol($46)
	%pitchenv($7F,$9D)
	%dur($7F) : %vol($50)
	%pitchenv($7F,$9F)
	%dur($7F) : %vol($50)
	%pitchenv($7F,$A0)
	%dur($7F) : %vol($5F)
	%pitchenv($7D,$A1)
	db $00

se_professorhangerappears: ; Professor Hanger appears
	%inst(2F)
	%dur($3F) : %vol($64)
	%pitchslide($BB,$3F,$BB)
	%dur($2F) : %vol($6E)
	%pitchenv($2F,$B7)
	%dur($2F) : %vol($7D)
	%pitchenv($2D,$B7)
	db $00

se_enemyhovering: ; Enemy hovering
	%inst(08)
	%dur($7F) : %vol($64) : %pitch($A3)
	db $00

se_professorhangerdisappears: ; Professor Hanger disappears
	%inst(2F)
	%dur($3F) : %vol($64)
	%pitchslide($BB,$3F,$BB)
	%dur($2F) : %vol($3C)
	%pitchenv($2F,$BE)
	%dur($2F) : %vol($1E)
	%pitchenv($2D,$BE)
	db $00

se_slotmachineslotspinning: ; Slot machine spinning
	%inst(0D)
	%dur($06) : %vol($14) : %pitch($A6)
	db $00

se_pepperradiochat: ; Pepper radio chat
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(2C)
	%dur($18) : %vol($7D) : %pitch($85)
	%dur($24) : %pitch($87)
	%inst(13)
	%dur($24) : %vol($6E) : %pitch($84)
	%inst(16)
	%dur($26)
	%pitchslide($84,$24,$80)
	db $00

se_androssradiochat: ; Andross radio chat
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)

se_androssscream1: ; Andross scream 1
	%inst(35)
	%dur($22) : %vol($7D) : %pitch($91)
	%dur($16) : %vol($64) : %pitch($91)
	%dur($14) : %vol($50) : %pitch($91)
	%dur($12) : %vol($3C) : %pitch($90)
	%dur($12) : %vol($28) : %pitch($8F)
	%dur($12) : %vol($14) : %pitch($8E)
	db $00

se_dancinginsectorfireflyby: ; Dancing Insector fly-by
	%inst(05)
	%dur($7F) : %vol($78)
	%pitchslide($91,$7C,$98)
	db $00

se_dancinginsectorfireshot: ; Dancing Insector fire shot
	%inst(1C)
	%dur($7F) : %vol($78)
	%pitchslide($97,$7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7D,$8C)
	db $00

se_dancinginsectormovement: ; Dancing Insector movement
	%inst(0A)
	%dur($06) : %vol($28) : %pitch($A2)
	%inst(05)
	%dur($08) : %vol($46) : %pitch($A9)
	db $00

se_squidhit: ; Squid hit
	%inst(23)
	%dur($12) : %vol($46)
	%pitchslide($A8,$10,$B0)
	db $00

se_stingrayhit: ; Stingray hit
	%inst(2D)
	%dur($18) : %vol($5A)
	%pitchslide($A8,$16,$BC)
	db $00

se_whalescream: ; Whale scream
	%inst(24)
	%dur($0C) : %vol($0A)
	%pitchslide($BC,$0C,$BE)
	%dur($06)
	%pitchenv($04,$C0)
	%dur($30) : %vol($0A)
	%pitchslide($C0,$30,$BE)
	%dur($60)
	%pitchenv($5E,$BC)
	%dur($24) : %vol($00) : %pitch($A4)
	%dur($0C) : %vol($14)
	%pitchslide($BC,$0C,$BE)
	%dur($06)
	%pitchenv($04,$C0)
	%dur($30) : %vol($14)
	%pitchslide($C0,$30,$BE)
	%dur($60)
	%pitchenv($5E,$BC)
	db $00

se_cometflyby: ; Comet fly-by
	%inst(10)
	%dur($1C) : %vol($00)
	%dur($0A) : %pitch($C7)
	%pitch($C5)
	%dur($0F) : %vol($05)
	%dur($14) : %pitch($C2)
	%pitch($C0)
	%dur($0A) : %vol($1E) : %pitch($C1)
	%pitch($BF)
	%dur($60) : %vol($28)
	db $00
	%pitch($BD)
	db $00

se_shootingstar: ; Shooting star
	%inst(10)
	%dur($08) : %vol($00) : %pitch($A4)
	%dur($1C) : %vol($0A) : %pitch($C6)
	%pitch($C4)
	%dur($0F) : %vol($14) : %pitch($C1)
	%pitch($BF)
	%dur($0A) : %vol($1E) : %pitch($C0)
	%pitch($BE)
	%dur($18) : %vol($14) : %pitch($BC)
	%dur($30) : %vol($0A) : %pitch($BC)
	db $00

se_bonusringbird: ; Bonus ring bird
	%inst(01)
	%dur($0C) : %vol($0A)
	%pitchslide($BE,$0C,$B4)
	%dur($06)
	%pitchenv($04,$C3)
	%dur($0C) : %vol($14)
	%pitchslide($BE,$0C,$B4)
	%dur($06)
	%pitchenv($04,$C3)
	%dur($18) : %vol($00) : %pitch($BE)
	%dur($0C) : %vol($1E)
	%pitchslide($BE,$0A,$C5)
	%dur($18) : %vol($00) : %pitch($BE)
	%dur($0C)
	%pitchslide($BE,$0C,$BB)
	%dur($06)
	%pitchenv($04,$C3)
	%dur($18) : %vol($00) : %pitch($BE)
	%dur($0C) : %vol($28)
	%pitchslide($BE,$0A,$C5)
	%dur($0C)
	%pitchslide($BE,$0C,$BB)
	%dur($06)
	%pitchenv($04,$C3)
	%dur($18) : %vol($00) : %pitch($BE)
	%dur($0C) : %vol($32)
	%pitchslide($BE,$0A,$C5)
	%dur($0C)
	%pitchslide($BE,$0C,$BB)
	%dur($06)
	%pitchenv($04,$C3)
	%dur($18) : %vol($00) : %pitch($BE)
	%dur($0C) : %vol($3C)
	%pitchslide($BE,$0A,$C5)
	db $00

se_comeincorneria: ; Come in Corneria
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(37)
	%dur($6F) : %pitch($87)
	%inst(15)
	%dur($48) : %vol($7D) : %pitch($98)
	%inst(0F)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(38)
	%dur($7F)
	%pitchslide($8A,$7F,$8A)
	%dur($1F)
	%pitchenv($1D,$8A)
	%inst(39)
	%dur($7F) : %pitch($8A)
	%inst(3A)
	%dur($7F)
	%pitchslide($8A,$7F,$8A)
	%dur($7F)
	%pitchenv($7F,$8A)
	%dur($2F)
	%pitchenv($2D,$8A)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(0F)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(3B)
	%dur($4F) : %pitch($87)
	%inst(3C)
	%dur($7F)
	%pitchslide($87,$7F,$87)
	%dur($40)
	%pitchenv($3E,$87)
	%inst(0F)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($48) : %vol($7D) : %pitch($98)
	db $00

se_bladebarrierwebattack: ; Blade barrier web attack
	%inst(1A)
	%dur($0C) : %vol($64) : %pitch($A3)
	%inst(23)
	%dur($60) : %vol($32)
	%pitchslide($8B,$60,$A3)
	%dur($60)
	%pitchenv($5E,$AF)
	db $00

se_metalsmasherclose: ; Metal smasher close
	%inst(1A)
	%dur($0C) : %vol($7D) : %pitch($A3)
	%dur($30) : %vol($7D) : %pitch($A3)
	db $00

se_objectimpact: ; Object impact
	%inst(0D)
	%dur($0C) : %vol($78) : %pitch($89)
	%dur($24) : %vol($78)
	%pitchslide($89,$22,$82)
	db $00

se_metalsmashersmashing: ; Metal smasher smashing
	%inst(36)
	%dur($24) : %vol($50)
	db $00
	%pitchslide($A3,$18,$9C)
	%dur($24)
	%pitchslide($A3,$18,$9C)
	%dur($24) : %vol($64)
	%dur($28)
	%pitchslide($A3,$18,$9C)
	%dur($24) : %vol($6E)
	%dur($3C)
	%pitchslide($A3,$18,$9C)
	%dur($24) : %vol($7D)
	%dur($50)
	%pitchslide($A3,$18,$9C)
	db $00

se_enemy: ; Enemy
	%inst(36)
	%dur($24) : %vol($00)
	%dur($50)
	%pitchslide($A2,$18,$9B)
	%dur($24)
	%pitchslide($A2,$18,$99)
	%dur($24) : %vol($28)
	%dur($64)
	%pitchslide($A2,$18,$9B)
	%dur($24) : %vol($3C)
	%dur($6E)
	%pitchslide($A2,$18,$9B)
	%dur($24) : %vol($50)
	%dur($7D)
	%pitchslide($A2,$18,$9B)
	db $00

se_prewingrepaired: ; Pre-wing repaired
	%inst(18)
	%dur($12) : %vol($0A)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$AD)
	%dur($12) : %vol($14)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$AD)
	%dur($12) : %vol($28)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$AD)
	%dur($12) : %vol($3C)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$AD)
	%dur($12) : %vol($50)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$A9)
	%dur($12) : %vol($50)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$A9)
	%dur($12) : %vol($3C)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$A9)
	%dur($12) : %vol($28)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$A9)
	%dur($12) : %vol($14)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$A9)
	%dur($12) : %vol($0A)
	%pitchslide($AB,$12,$BE)
	%dur($0C)
	%pitchenv($0A,$A9)
	db $00

se_silence: ; Silence
	db $00

se_textting: ; Text ting
	%inst(26)
	%dur($06) : %vol($32) : %pitch($BB)
	db $00

se_androsshit: ; Andross hit
	%inst(35)
	%dur($12) : %vol($7D) : %pitch($9A)
	%dur($12) : %vol($64) : %pitch($9A)
	%dur($12) : %vol($50) : %pitch($9A)
	%dur($12) : %vol($3C) : %pitch($9A)
	%dur($12) : %vol($28) : %pitch($9A)
	%dur($12) : %vol($14) : %pitch($9A)
	db $00

se_androssappears: ; Andross appears
	%inst(05)
	%dur($0C) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($28) : %pitch($B9)
	%inst(10)
	%dur($48) : %vol($46) : %pitch($C7)
	db $00

se_pilontoground: ; Pilon to ground
	%inst(0D)
	%dur($06) : %vol($78) : %pitch($89)
	%dur($04) : %vol($00) : %pitch($89)
	%dur($18) : %vol($78) : %pitch($89)
	db $00

se_destructorweapnheadattack: ; Destructor weapon head attack
	%inst(1C)
	%dur($20) : %vol($3C)
	%pitchslide($A4,$14,$AB)
	%dur($1C) : %vol($50)
	%pitchslide($A4,$10,$AB)
	%dur($1A) : %vol($64)
	%pitchslide($A4,$0E,$AB)
	%dur($18) : %vol($78)
	%pitchslide($A4,$0C,$AB)
	%dur($18)
	%pitchslide($A4,$0C,$AB)
	%dur($18) : %vol($64)
	%pitchslide($A4,$0C,$AB)
	%dur($18) : %vol($5A)
	%pitchslide($A4,$0C,$AB)
	%dur($18) : %vol($46)
	%pitchslide($A4,$0C,$AB)
	%dur($18) : %vol($28)
	%pitchslide($A4,$0C,$AB)
	%dur($18) : %vol($14)
	%pitchslide($A4,$0C,$AB)
	db $00

se_twinlaser: ; Player twin blaster shot
	%inst(20)
	%dur($0C) : %vol($3C) : %pitch($94)
	%dur($48) : %pitch($94)
	db $00

se_bigarwingdamagealarm: ; Big arwing damage alarm
	%inst(26)
	%dur($06) : %vol($46)
	%dur($14)
	%pitchslide($B4,$06,$B7)
	%dur($0C)
	if !opt_f1_f9 == 0
	db $F1, $B5, $00, $06 ; invalid pitch envelope command
	else
	db $F1, $00, $06 ; invalid pitch envelope command
	endif
	%pitch($B7)
	%dur($06)
	%pitchslide($B4,$06,$B7)
	%dur($0C)
	if !opt_f1_f9 == 0
	db $F1, $B5, $00, $06 ; invalid pitch envelope command
	else
	db $F1, $00, $06 ; invalid pitch envelope command
	endif
	%pitch($B7)
	%dur($06)
	%pitchslide($B4,$06,$B7)
	%dur($0C)
	if !opt_f1_f9 == 0
	db $F1, $B5, $00, $06 ; invalid pitch envelope command
	else
	db $F1, $00, $06 ; invalid pitch envelope command
	endif
	%pitch($B7)
	%dur($06)
	%pitchslide($B4,$06,$B7)
	%dur($0C)
	if !opt_f1_f9 == 0
	db $F1, $B5, $00, $06 ; invalid pitch envelope command
	else
	db $F1, $00, $06 ; invalid pitch envelope command
	endif
	%pitch($B7)
	db $00

se_smallarwingdamagealarm: ; Small arwing damage alarm
	%inst(26)
	%dur($0A) : %vol($3C)
	%dur($14)
	%pitchslide($B0,$0A,$B4)
	%dur($10)
	if !opt_f1_f9 == 0
	db $F1, $B2, $00, $0A ; invalid pitch envelope command
	else
	db $F1, $00, $0A ; invalid pitch envelope command
	endif
	%pitch($B4)
	%dur($0A)
	%pitchslide($B0,$0A,$B4)
	%dur($10)
	if !opt_f1_f9 == 0
	db $F1, $B2, $00, $0A ; invalid pitch envelope command
	else
	db $F1, $00, $0A ; invalid pitch envelope command
	endif
	%pitch($B4)
	db $00

se_slotmachinecoin: ; Slot machine coin
	%inst(01)
	%dur($08) : %vol($32) : %pitch($B0)
	%pitch($B2)
	%dur($08) : %vol($28) : %pitch($B5)
	%dur($0C) : %vol($1E) : %pitch($B7)
	%dur($08) : %vol($32) : %pitch($BC)
	%pitch($BE)
	%dur($08) : %vol($28) : %pitch($C1)
	%dur($18) : %vol($14)
	%dur($08) : %pitch($C3)
	%dur($30) : %vol($08)
	db $00
	%pitch($C3)
	db $00

se_slotmachinecoinsubsfx: ; Slot machine coin sub-sfx
	%inst(01)
	%dur($0C) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($32) : %pitch($B0)
	%pitch($B2)
	%dur($08) : %vol($28) : %pitch($B5)
	%dur($0C) : %vol($1E) : %pitch($B7)
	%dur($08) : %vol($32) : %pitch($BC)
	%pitch($BE)
	%dur($08) : %vol($28) : %pitch($C1)
	%dur($18) : %vol($14)
	%dur($08) : %pitch($C3)
	%dur($30) : %vol($08)
	db $00
	%pitch($C3)
	db $00

se_blockadedirectionchange: ; Blockade direction change
	%inst(05)
	%dur($06) : %vol($78) : %pitch($9E)
	%inst(00)
	%dur($08) : %vol($28)
	%pitchslide($A4,$06,$A6)
	%dur($18) : %pitch($A6)
	db $00

se_bombpowerup: ; Bomb powerup
	%inst(00)
	%dur($24) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($0A)
	%dur($28)
	%pitchslide($B7,$06,$B9)
	%dur($08) : %vol($00) : %pitch($B9)
	%dur($08) : %vol($0A)
	%dur($28)
	%pitchslide($B7,$06,$B9)
	%dur($08) : %vol($00) : %pitch($B9)
	%dur($08) : %vol($0A)
	%dur($28)
	%pitchslide($B7,$06,$B9)
	%dur($08) : %vol($00) : %pitch($B9)
	db $00

se_rockcrusheruncover: ; Rock crusher uncover
	%inst(32)
	%dur($7F) : %vol($7D)
	%pitchslide($A3,$7F,$A3)
	%dur($1F) : %vol($64)
	%pitchenv($1F,$A3)
	%dur($1F) : %vol($50)
	%pitchenv($1D,$A3)
	db $00

se_phantronappears: ; Phantron appears
	%inst(31)
	%dur($5F) : %vol($64)
	db $00
	%pitchslide($A3,$5F,$A3)
	%dur($5F) : %vol($64)
	%dur($1E)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($73)
	%dur($46)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($7D)
	%dur($64)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($50)
	%dur($69)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($32)
	%dur($50)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($1E)
	%dur($32)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($0A)
	%dur($1E)
	%pitchenv($5D,$A3)
	db $00

se_destructorengine: ; Destructor engine
	%inst(30)
	%dur($7F) : %vol($00)
	%dur($64)
	%pitchslide($A3,$7F,$A3)
	%dur($7F)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($0A)
	%dur($73)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($1E)
	%dur($7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($32)
	%dur($7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($46)
	%dur($7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($5A)
	%dur($7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($5A)
	%dur($7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($50)
	%dur($69)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($46)
	%dur($5A)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($3C)
	%dur($50)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($28)
	%dur($41)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($1E)
	%dur($32)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($14)
	%dur($1E)
	%pitchenv($7D,$A3)
	db $00

se_destructorengine2: ; Destructor engine
	%inst(30)
	%dur($7F) : %vol($00)
	%dur($64)
	%pitchslide($A1,$7F,$A1)
	%dur($7F) : %vol($0A)
	%dur($64)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($1E)
	%dur($73)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($32)
	%dur($7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($46)
	%dur($7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($5A)
	%dur($7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($5A)
	%dur($7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($7D)
	%dur($5A)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($69)
	%dur($50)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($5A)
	%dur($46)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($50)
	%dur($3C)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($41)
	%dur($28)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($32)
	%dur($1E)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($1E)
	%dur($14)
	%pitchenv($7D,$A1)
	db $00

se_rockcrusherappears: ; Rock crusher appears
	%inst(2F)
	%dur($5F) : %vol($64)
	db $00
	%pitchslide($A3,$5F,$A3)
	%dur($5F) : %vol($64)
	%dur($1E)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($73)
	%dur($46)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($7D)
	%dur($64)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($69)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($50)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($32)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($1E)
	%pitchenv($5D,$A3)
	db $00

se_phantron2hit: ; Phantron 2 hit
	%inst(12)
	%dur($0C) : %vol($78)
	%pitchslide($98,$0C,$97)
	%dur($0C)
	%pitchenv($0C,$A2)
	%dur($18)
	%pitchenv($15,$9F)
	db $00

se_phantron2screamsubsfx: ; Phantron scream
	%dur($10) : %vol($00) : %pitch($98)

se_phantron2scream: ; Phantron 2 scream
	%inst(12)
	%dur($12) : %vol($78)
	%pitchslide($98,$12,$97)
	%dur($18)
	%pitchenv($18,$A7)
	%dur($16)
	%pitchenv($13,$9F)
	%dur($0F) : %vol($64)
	%pitchslide($95,$0F,$93)
	%dur($15)
	%pitchenv($15,$A3)
	%dur($14)
	%pitchenv($11,$9C)
	%dur($0C) : %vol($50)
	%pitchslide($91,$0C,$90)
	%dur($12)
	%pitchenv($12,$A0)
	%dur($12)
	%pitchenv($0F,$98)
	%dur($09) : %vol($3C)
	%pitchslide($8E,$09,$8C)
	%dur($0F)
	%pitchenv($0F,$9C)
	%dur($0F)
	%pitchenv($0C,$95)
	%dur($06) : %vol($28)
	%pitchslide($8B,$06,$89)
	%dur($0C)
	%pitchenv($0C,$98)
	%dur($1E)
	%pitchenv($1B,$91)
	db $00

se_falcoradiochat: ; Falco radio chat
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(16)
	%dur($10) : %vol($7D) : %pitch($80)
	%pitch($82)
	%inst(14)
	%dur($12) : %vol($7D) : %pitch($84)
	%dur($18) : %vol($7D) : %pitch($85)
	%inst(16)
	%dur($24) : %vol($7D) : %pitch($85)
	%dur($18) : %vol($00) : %pitch($85)
	db $00

se_falcoradiochathit: ; Falco radio chat hit
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(16)
	%dur($18) : %vol($7D) : %pitch($85)
	%dur($0C) : %pitch($82)
	%inst(2E)
	%dur($0C) : %vol($7D) : %pitch($84)
	%pitch($82)
	%pitch($82)
	%inst(16)
	%dur($08) : %vol($7D) : %pitch($82)
	%dur($0C) : %vol($00) : %pitch($89)
	%inst(16)
	%dur($08) : %vol($7D) : %pitch($82)
	%inst(2C)
	%dur($18) : %vol($7D) : %pitch($87)
	%inst(14)
	%dur($18) : %vol($7D) : %pitch($85)
	db $00

se_falcoradiochatdown: ; Falco radio chat down
	%inst(0F)
	%dur($06) : %vol($14) : %pitch($C0)
	%dur($04) : %vol($00) : %pitch($C0)
	%dur($12) : %vol($14) : %pitch($C0)
	%dur($06) : %vol($00) : %pitch($C0)
	%dur($06) : %vol($14) : %pitch($C0)
	%dur($04) : %vol($00) : %pitch($C0)
	%dur($12) : %vol($14) : %pitch($C0)
	%dur($06) : %vol($00) : %pitch($C0)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(2D)
	%dur($24) : %vol($7D) : %pitch($85)
	%dur($0C) : %pitch($84)
	%dur($0C) : %vol($00) : %pitch($85)
	%inst(16)
	%dur($0C) : %vol($7D) : %pitch($89)
	%pitch($87)
	%pitch($85)
	%dur($0C) : %vol($00) : %pitch($89)
	%inst(16)
	%dur($08) : %vol($7D) : %pitch($85)
	%inst(14)
	%dur($18) : %vol($7D) : %pitch($85)
	%inst(16)
	%dur($18) : %vol($7D) : %pitch($84)
	db $00

se_peppyradiochat: ; Peppy radio chat
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(14)
	%dur($0C) : %vol($7D) : %pitch($9C)
	%pitch($95)
	%dur($24) : %vol($7D) : %pitch($9C)
	%inst(1F)
	%dur($0C) : %vol($7D) : %pitch($93)
	%dur($18) : %vol($7D) : %pitch($91)
	%inst(14)
	%dur($24) : %vol($7D) : %pitch($97)
	db $00

se_peppyradiochathit: ; Peppy radio chat hit
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(2C)
	%dur($0C) : %vol($7D) : %pitch($9C)
	%inst(14)
	%dur($24) : %vol($7D) : %pitch($9A)
	%inst(2E)
	%dur($08) : %vol($7D) : %pitch($93)
	%pitch($97)
	%dur($18) : %vol($7D) : %pitch($91)
	%inst(16)
	%dur($0C) : %vol($7D) : %pitch($97)
	%pitch($97)
	db $00

se_peppyradiochatdown: ; Peppy radio chat down
	%inst(0F)
	%dur($06) : %vol($14) : %pitch($C0)
	%dur($04) : %vol($00) : %pitch($C0)
	%dur($12) : %vol($14) : %pitch($C0)
	%dur($06) : %vol($00) : %pitch($C0)
	%dur($06) : %vol($14) : %pitch($C0)
	%dur($04) : %vol($00) : %pitch($C0)
	%dur($12) : %vol($14) : %pitch($C0)
	%dur($06) : %vol($00) : %pitch($C0)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(2D)
	%dur($18) : %vol($7D) : %pitch($A1)
	%pitch($9A)
	%dur($10) : %vol($00) : %pitch($A1)
	%inst(2E)
	%dur($0C) : %vol($7D) : %pitch($98)
	%dur($0C) : %vol($7D) : %pitch($97)
	%inst(1F)
	%dur($24) : %vol($7D) : %pitch($95)
	db $00

se_slippyradiochat: ; Slippy radio chat
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(16)
	%dur($08) : %vol($7D) : %pitch($89)
	%inst(14)
	%dur($08) : %pitch($85)
	%inst(16)
	%dur($08) : %pitch($89)
	%inst(14)
	%dur($08) : %pitch($8C)
	%inst(16)
	%dur($12) : %vol($00) : %pitch($87)
	%inst(16)
	%dur($08) : %vol($7D) : %pitch($89)
	%inst(14)
	%dur($08) : %pitch($85)
	%inst(16)
	%dur($08) : %pitch($89)
	%inst(14)
	%dur($08) : %pitch($8C)
	%inst(16)
	%dur($18) : %vol($00) : %pitch($87)
	db $00

se_slippyradiochathit: ; Slippy radio chat hit
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(14)
	%dur($06) : %vol($7D) : %pitch($8C)
	%pitch($90)
	%dur($06) : %pitch($8C)
	%pitch($90)
	%dur($0C) : %vol($00) : %pitch($85)
	%inst(16)
	%dur($06) : %vol($7D) : %pitch($89)
	%pitch($8C)
	%pitch($89)
	%pitch($8C)
	%dur($12) : %vol($00) : %pitch($85)
	%dur($06) : %vol($7D) : %pitch($87)
	%pitch($8B)
	%pitch($87)
	%pitch($8B)
	db $00

se_slippyradiochatdown: ; Slippy radio chat down
	%inst(0F)
	%dur($06) : %vol($14) : %pitch($C0)
	%dur($04) : %vol($00) : %pitch($C0)
	%dur($12) : %vol($14) : %pitch($C0)
	%dur($06) : %vol($00) : %pitch($C0)
	%dur($06) : %vol($14) : %pitch($C0)
	%dur($04) : %vol($00) : %pitch($C0)
	%dur($12) : %vol($14) : %pitch($C0)
	%dur($06) : %vol($00) : %pitch($C0)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(16)
	%dur($08) : %vol($7D) : %pitch($85)
	%pitch($89)
	%pitch($85)
	%pitch($89)
	%dur($12) : %vol($00) : %pitch($85)
	%dur($0A) : %vol($7D) : %pitch($84)
	%pitch($87)
	%dur($0B) : %pitch($84)
	%pitch($87)
	%dur($12) : %vol($00) : %pitch($85)
	%inst(16)
	%dur($0E) : %vol($7D) : %pitch($82)
	%pitch($85)
	%inst(2D)
	%dur($30) : %vol($7D) : %pitch($89)
	db $00

se_twinblasterpowerup: ; Twin blaster powerup
	%inst(1F)
	%dur($18) : %vol($00) : %pitch($8C)
	%dur($7F) : %vol($7D) : %pitch($8C)
	db $00

se_shieldpowerup: ; Shield powerup
	%inst(2C)
	%dur($18) : %vol($00) : %pitch($8C)
	%dur($30) : %vol($7D) : %pitch($8C)
	db $00

se_wingrepairedpowerup: ; Wing repaired powerup
	%inst(14)
	%dur($18) : %vol($00) : %pitch($8C)
	%dur($1C) : %vol($7D) : %pitch($8B)
	%inst(2E)
	%dur($60) : %vol($7D) : %pitch($8C)
	db $00

se_conehit: ; Cone hit
	%inst(05)
	%dur($06) : %vol($5A) : %pitch($B0)
	%inst(0A)
	%dur($24) : %vol($25) : %pitch($84)
	db $00

se_backgroundthunder: ; Background thunder
	%dur($7F) : %vol($00) : %pitch($90)
	%inst(04)
	%dur($7F) : %vol($7D)
	%pitchslide($8C,$7F,$90)
	%dur($7F)
	%pitchenv($7F,$8E)
	%dur($3F)
	%pitchenv($3D,$8E)
	%dur($7F)
	%pitchslide($8B,$7F,$8D)
	%dur($7F)
	%pitchenv($7D,$8C)

se_volcanofire: ; Volcano fire
	%inst(04)
	%dur($3F) : %vol($7D)
	%pitchslide($8B,$3F,$8D)
	%dur($7F)
	%pitchenv($7D,$8C)
	%dur($7F)
	%pitchslide($8B,$7F,$8D)
	%dur($7F)
	%pitchenv($7D,$8C)
	%dur($7F)
	%pitchslide($87,$7F,$8A)
	%dur($7F)
	%pitchenv($7F,$89)
	%dur($7F)
	%pitchenv($7D,$89)
	db $00

se_backgroundthunder2: ; Background thunder
	%dur($7F) : %vol($00) : %pitch($8D)

se_spinningcorebgthunder: ; Spinning core bg thunder
	%inst(0C)
	%dur($7F) : %vol($78)
	%pitchslide($8C,$7F,$8D)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7F,$8C)
	%dur($7F)
	%pitchenv($7D,$8C)
	db $00

se_goodluck: ; Jingle + good luck
	%inst(01)
	%dur($08) : %vol($1E) : %pitch($B7)
	%pitch($B9)
	%dur($08) : %vol($14) : %pitch($B9)
	%dur($0C) : %vol($14) : %pitch($B9)
	%dur($0C) : %vol($0A)
	%dur($08) : %pitch($B9)
	%dur($18) : %vol($08)
	db $00
	%pitch($B9)
	db $00

se_goodlucksubsfx: ; Good luck
	%inst(01)
	%dur($0C) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($1E) : %pitch($B7)
	%pitch($B9)
	%dur($08) : %vol($14) : %pitch($B9)
	%dur($0C) : %vol($14) : %pitch($B9)
	%dur($0C) : %vol($0A)
	%dur($08) : %pitch($B9)
	%dur($18) : %vol($08)
	db $00
	%pitch($B9)
	db $00

se_bonuscredit: ; Bonus credit
	%inst(01)
	%dur($08) : %vol($32) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($2E)
	%dur($1C) : %pitch($BE)
	%dur($18) : %pitch($C0)
	%dur($08) : %vol($24)
	%dur($16) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($1E)
	%dur($12) : %pitch($BE)
	%dur($18) : %pitch($C0)
	%dur($08) : %vol($16)
	%dur($0C) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($12)
	%dur($02) : %pitch($BE)
	%dur($18) : %pitch($C0)
	db $00

se_bonuscreditsubsfx: ; Bonus
	%inst(01)
	%dur($18) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($28) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($0C)
	%dur($1A) : %pitch($BE)
	%dur($0C) : %pitch($C0)
	%dur($08) : %vol($12)
	%dur($1E) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($0C)
	%dur($16) : %pitch($BE)
	%dur($18) : %pitch($C0)
	%dur($08) : %vol($02)
	%dur($12) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%pitch($BE)
	%dur($18) : %pitch($C0)
	db $00

se_playeramoebahit: ; Player amoeba hit
	%inst(2A)
	%dur($0C) : %vol($64) : %pitch($A1)
	db $00

se_doorclosenear: ; Last base entry 2.door close
	%inst(1C)
	%dur($24) : %vol($7D)
	%pitchslide($91,$24,$8C)
	%dur($18)
	%pitchenv($16,$8C)
	%inst(05)
	%dur($08) : %vol($64) : %pitch($99)
	db $00

se_doorclosemidfar: ; Far last base entry 2.door close
	%inst(1C)
	%dur($24) : %vol($46)
	%pitchslide($91,$24,$8C)
	%dur($18)
	%pitchenv($16,$8C)
	%inst(05)
	%dur($08) : %vol($3C) : %pitch($99)
	db $00

se_dooropennear: ; Last base entry 2.door open
	%inst(1C)
	%dur($24) : %vol($7D)
	%pitchslide($89,$24,$90)
	%dur($18)
	%pitchenv($16,$93)
	%inst(05)
	%dur($08) : %vol($64) : %pitch($99)
	db $00

se_dooropenmidfar: ; Far last base entry 2.door open
	%inst(1C)
	%dur($24) : %vol($46)
	%pitchslide($89,$24,$90)
	%dur($18)
	%pitchenv($16,$93)
	%inst(05)
	%dur($08) : %vol($3C) : %pitch($99)
	db $00

se_atomicbasecoreclose: ; Atomic base core close
	%inst(1C)
	%dur($24) : %vol($78)
	%pitchslide($9F,$18,$98)
	%dur($24)
	%pitchslide($9F,$18,$98)
	%dur($24) : %vol($64)
	%pitchslide($9F,$18,$98)
	%dur($24) : %vol($5A)
	%pitchslide($9F,$18,$98)
	%dur($24) : %vol($46)
	%pitchslide($9F,$18,$98)
	db $00

se_atomicbasecoreopen: ; Atomic base core open
	%inst(1C)
	%dur($24) : %vol($78)
	%pitchslide($98,$18,$9F)
	%dur($24)
	%pitchslide($98,$18,$9F)
	%dur($24) : %vol($64)
	%pitchslide($98,$18,$9F)
	%dur($24) : %vol($5A)
	%pitchslide($98,$18,$9F)
	%dur($24) : %vol($46)
	%pitchslide($98,$18,$9F)
	db $00

se_atomicbasepowersupplyon: ; Atomic base power supply on
	%inst(05)
	%dur($06) : %vol($78) : %pitch($9E)
	%inst(1C)
	%dur($18)
	%pitchslide($8C,$18,$93)
	%dur($24)
	%pitchenv($22,$98)
	db $00

se_atomicbasepowersupplyoff: ; Atomic base power supply off
	%inst(05)
	%dur($06) : %vol($78) : %pitch($9E)
	%inst(1C)
	%dur($18)
	%pitchslide($98,$18,$91)
	%dur($24)
	%pitchenv($22,$8C)
	db $00

se_hovering: ; Hovering
	%inst(29)
	%dur($7F) : %vol($64)
	%pitchslide($A3,$7F,$A3)
	%dur($7F)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($73)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($7D)
	%pitchenv($7F,$A3)
	%dur($7F)
	%pitchenv($7F,$A3)
	%dur($7F)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($69)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($50)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($32)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($1E)
	%pitchenv($7D,$A3)
	db $00

se_dooropen: ; Door open
	%inst(1C)
	%dur($18) : %vol($64)
	%pitchslide($85,$18,$8C)
	%dur($18)
	%pitchenv($16,$8C)
	db $00

se_doorclose: ; Door close
	%inst(1C)
	%dur($18) : %vol($64)
	%pitchslide($90,$18,$89)
	%dur($18)
	%pitchenv($16,$89)
	db $00

se_hovering2: ; Hovering
	%inst(29)
	%dur($7F) : %vol($5A)
	%pitchslide($A3,$7F,$A3)
	%dur($7F) : %vol($5F)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($64)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($6E)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($78)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($6E)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($64)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($5F)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($46)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($3C)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($28)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($1E)
	%pitchenv($7D,$A3)
	db $00

se_hovering2subsfx: ; Background hum
	%inst(29)
	%dur($70) : %vol($5A)
	%pitchslide($99,$70,$99)
	%dur($7F) : %vol($5F)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($64)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($6E)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($7D)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($7D)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($6E)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($69)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($5F)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($50)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($46)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($32)
	%pitchenv($7F,$99)
	%dur($7F) : %vol($1E)
	%pitchenv($7D,$99)
	db $00

se_ringlasernear: ; Near enemy ring shot
	%inst(27)
	%dur($12) : %vol($5A)
	%pitchslide($AD,$12,$B0)
	%dur($0C)
	%pitchenv($0C,$AF)
	%dur($0C)
	%pitchenv($0A,$B0)
	db $00

se_ringlasermid: ; Mid enemy ring shot
	%inst(27)
	%dur($12) : %vol($3C)
	%pitchslide($AD,$12,$B0)
	%dur($0C)
	%pitchenv($0C,$AF)
	%dur($0C)
	%pitchenv($0A,$B0)
	db $00

se_ringlaserfar: ; Far enemy ring shot
	%inst(27)
	%dur($12) : %vol($1E)
	%pitchslide($AD,$12,$B0)
	%dur($0C)
	%pitchenv($0C,$AF)
	%dur($0C)
	%pitchenv($0A,$B0)
	db $00

se_foxradiochat: ; Fox radio chat
	%inst(0F)
	%dur($12) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	%inst(16)
	%dur($08) : %vol($7D) : %pitch($8B)
	%dur($18) : %pitch($90)
	%dur($0C) : %pitch($8E)
	%dur($18) : %pitch($8C)
	%dur($0C) : %pitch($8E)
	%pitch($8D)
	%dur($18) : %vol($00) : %pitch($93)
	db $00

se_radiochatquit: ; Radio chat quit
	%inst(0F)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%dur($06) : %vol($14) : %pitch($BE)
	%dur($06) : %vol($00) : %pitch($BE)
	%inst(15)
	%dur($18) : %vol($7D) : %pitch($98)
	db $00

se_playercamerachange: ; Player camera change
	%inst(25)
	%dur($48) : %vol($3C) : %pitch($B6)
	db $00

se_goodlucksubsubsfx: ; Good luck sub-sub-sfx
	%inst(22)
	%dur($18) : %vol($00) : %pitch($A4)
	%dur($48) : %vol($78) : %pitch($A0)
	db $00

se_continue: ; Continue let's go
	%inst(33)
	%dur($48) : %vol($78) : %pitch($A2)
	db $00

se_enemyupsealeft: ; Left water splash out
	%inst(2B)
	%dur($30) : %vol($7D)
	%dur($0A)
	%pitchslide($A6,$28,$9A)
	db $00

se_enemyupseacentre: ; Centre water splash
	%inst(2B)
	%dur($30) : %vol($7D)
	%pitchslide($A6,$28,$9A)
	db $00

se_enemyupsearight: ; Right water splash
	%inst(2B)
	%dur($30) : %vol($0A)
	%dur($7D)
	%pitchslide($A6,$28,$9A)
	db $00

se_midwatersplash: ; Mid water splash
	%inst(2B)
	%dur($30) : %vol($6E)
	%pitchslide($A6,$28,$9A)
	db $00

se_farwatersplash: ; Far water splash
	%inst(2B)
	%dur($30) : %vol($5A)
	%pitchslide($A6,$28,$9A)
	db $00

se_enemydownsealeft: ; Left water splash in
	%inst(21)
	%dur($30) : %vol($7D)
	%dur($0A) : %pitch($A1)
	db $00

se_enemydownseacentre: ; Centre water splash in
	%inst(21)
	%dur($30) : %vol($7D) : %pitch($A1)
	db $00

se_enemydownsearight: ; Right water splash in
	%inst(21)
	%dur($30) : %vol($0A)
	%dur($7D) : %pitch($A1)
	db $00

se_midwatersplashin: ; Mid water splash in
	%inst(21)
	%dur($30) : %vol($6E) : %pitch($A1)
	db $00

se_farwatersplashin: ; Far water splash in
	%inst(21)
	%dur($30) : %vol($5A) : %pitch($A1)
	db $00

se_dancinginsectorpropelling: ; Dancing insector propelling
	%inst(1B)
	%dur($0C) : %vol($78)
	%pitchslide($A1,$0A,$9D)
	%dur($18) : %vol($64)
	%pitchslide($A1,$18,$A4)
	%dur($24)
	%pitchenv($18,$9D)
	db $00

se_dopleft: ; Right object fly-by
	%inst(1E)
	%dur($30) : %vol($00)
	%dur($78)
	%pitchslide($B9,$30,$B9)
	%dur($18)
	%pitchenv($16,$B0)
	db $00

se_dopcentre: ; Centre object fly-by
	%inst(1E)
	%dur($30) : %vol($64)
	%pitchslide($B9,$30,$B9)
	%dur($18)
	%pitchenv($16,$B0)
	db $00

se_dopright: ; Left object fly-by
	%inst(1E)
	%dur($30) : %vol($78)
	db $00
	%pitchslide($B9,$30,$B9)
	%dur($18)
	%pitchenv($16,$B0)
	db $00

se_bladebarrierplayerwebhit: ; Blade barrier player web hit
	%inst(19)
	%dur($0C) : %vol($78)
	%pitchslide($B9,$0C,$A4)
	%dur($0C)
	%pitchenv($0A,$9D)
	%inst(05)
	%dur($06) : %vol($78) : %pitch($9E)
	db $00

se_unused: ; -unused-
	%inst(05)
	%dur($06) : %vol($78) : %pitch($9E)
	%dur($0C) : %pitch($A1)
	db $00

se_bladebarrierpostdrillatk: ; Blade barrier post-drill attack
	%inst(1C)
	%dur($12) : %vol($78)
	%pitchslide($95,$12,$98)
	%dur($12)
	%pitchenv($12,$96)
	%dur($12)
	%pitchenv($12,$94)
	%dur($12)
	%pitchenv($10,$92)
	db $00

se_conetriangle: ; Cone triangle
	%inst(07)
	%dur($12) : %vol($78)
	%pitchslide($95,$12,$98)
	%dur($12)
	%pitchenv($12,$97)
	%dur($12)
	%pitchenv($12,$96)
	%dur($12)
	%pitchenv($12,$95)
	%dur($12)
	%pitchenv($12,$94)
	%dur($12)
	%pitchenv($12,$93)
	%dur($12)
	%pitchenv($10,$92)
	db $00

se_bossshadowing: ; 1-6+2-3 Boss shadowing
	%inst(07)
	%dur($08) : %vol($78) : %pitch($98)
	%dur($08) : %vol($00) : %pitch($98)
	%dur($08) : %vol($78) : %pitch($98)
	%dur($18) : %vol($64)
	%pitchslide($98,$16,$95)
	%dur($18) : %vol($50)
	%pitchslide($98,$16,$95)
	%dur($18) : %vol($3C)
	%pitchslide($98,$16,$95)
	%dur($18) : %vol($32)
	%pitchslide($98,$16,$95)
	%dur($18) : %vol($28)
	%pitchslide($98,$16,$95)
	db $00

se_enemywarpin: ; Enemy warp-in
	%inst(07)
	%dur($08) : %vol($78) : %pitch($95)
	%dur($18) : %vol($64)
	%pitchslide($89,$16,$98)
	%dur($18) : %vol($50)
	%pitchslide($8B,$16,$9A)
	db $00

se_rockcrusherroll: ; Rock crusher roll
	%inst(05)
	%dur($05) : %vol($78) : %pitch($8F)
	%pitch($8F)
	%pitch($90)
	%pitch($90)
	%pitch($91)
	%pitch($91)
	%pitch($92)
	%pitch($92)
	%pitch($93)
	%pitch($93)
	%pitch($94)
	%pitch($94)
	%pitch($95)
	%pitch($95)
	%pitch($96)
	%pitch($96)
	%pitch($97)
	%pitch($97)
	%pitch($98)
	%pitch($98)
	%pitch($99)
	%pitch($99)
	%pitch($9A)
	%pitch($9A)
	%pitch($9B)
	%pitch($9B)
	%pitch($9C)
	%pitch($9C)
	%pitch($9D)
	%pitch($9D)
	%pitch($9E)
	%pitch($9E)
	%pitch($9F)
	%pitch($9F)
	%pitch($A0)
	%pitch($A0)
	%pitch($A1)
	%pitch($A1)

se_slotmachinehandledown: ; Slot machine handle down
	%inst(05)
	%dur($05) : %vol($78) : %pitch($A2)
	%pitch($A2)
	%pitch($A2)
	%pitch($A3)
	%pitch($A3)
	%pitch($A4)
	%pitch($A4)
	%pitch($A5)
	%pitch($A5)
	%pitch($A6)
	%pitch($A6)
	%pitch($A7)
	%pitch($A7)
	%pitch($A8)
	%pitch($A8)
	%pitch($A9)
	%pitch($A9)
	%dur($06) : %vol($78) : %pitch($92)
	%dur($30) : %pitch($95)
	db $00

se_destructbosssmallsubsfx: ; Explosion circle
	%inst(05)
	%dur($0C) : %vol($78) : %pitch($9C)
	%dur($18) : %pitch($9C)
	%dur($30) : %vol($78)
	%pitchslide($8E,$30,$8F)
	%dur($30)
	%pitchenv($30,$90)
	%dur($30)
	%pitchenv($30,$91)
	%dur($30)
	%pitchenv($30,$92)
	%dur($48)
	%pitchenv($48,$91)
	%dur($48)
	%pitchenv($45,$90)
	db $00

se_destructbosssmall: ; Small boss explosion
	%inst(12)
	%dur($30) : %vol($78)
	%pitchslide($8C,$30,$8D)
	%dur($30)
	%pitchenv($30,$8E)
	%dur($30)
	%pitchenv($30,$8F)
	%dur($30)
	%pitchenv($30,$90)
	%dur($30)
	%pitchenv($30,$91)
	%dur($30)
	%pitchenv($30,$92)
	%dur($30)
	%pitchenv($2E,$93)
	db $00

se_plasmahydraarmhit: ; Plasma hydra arm hit
	%inst(02)
	%dur($0C) : %vol($78)
	%pitchslide($9C,$0C,$9B)
	%dur($0C)
	%pitchenv($0C,$A5)
	%dur($18)
	%pitchenv($15,$A3)
	db $00

se_enemyrocketplayerhit: ; Enemy rocket player hit
	%inst(06)
	%dur($12) : %vol($78)
	%pitchslide($93,$12,$8F)
	%dur($24)
	%pitchenv($24,$9B)
	%dur($24)
	%pitchenv($21,$98)
	db $00

se_dodorahit: ; Dodora hit
	%inst(02)
	%dur($12) : %vol($78)
	%pitchslide($91,$12,$96)
	%dur($0C)
	%pitchenv($0C,$A2)
	%dur($24)
	%pitchenv($24,$9F)
	%dur($24)
	%pitchenv($21,$97)
	db $00

se_dodorahitsubsfx: ; Dodora hit sub-sfx
	%inst(02)
	%dur($10) : %vol($00) : %pitch($8C)
	%dur($12) : %vol($3C)
	%dur($50)
	%pitchslide($91,$12,$96)
	%dur($0C)
	%pitchenv($0C,$A2)
	%dur($24)
	%pitchenv($24,$9F)
	%dur($24)
	%pitchenv($21,$97)
	db $00

se_dodoraeggcrackbird: ; Dodora egg crack + bird
	%inst(15)
	%dur($08) : %vol($78) : %pitch($B4)
	%pitch($B9)
	%dur($24) : %pitch($BE)

se_birdscream: ; Bird scream
	%inst(07)
	%dur($0C) : %vol($78)
	%pitchslide($A1,$0C,$A5)
	%dur($0C)
	%pitchenv($0C,$B1)
	%dur($24)
	%pitchenv($21,$AF)
	db $00

se_percentagering: ; Percentage ring
	%inst(00)
	%dur($03) : %vol($28) : %pitch($B7)
	%pitch($B9)
	%pitch($B7)
	%pitch($B9)
	%pitch($B7)
	%pitch($B9)
	%dur($08) : %pitch($BB)
	%dur($18) : %pitch($BE)
	db $00

se_gateofring: ; Big support ring
	%inst(01)
	%dur($0C) : %vol($1E) : %pitch($BC)
	%pitch($BE)
	%pitch($BC)
	%pitch($BE)
	%dur($0C) : %vol($1E)
	%dur($0F) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($14)
	%dur($08) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($0A)
	db $00
	%pitch($C1)
	%dur($24) : %pitch($C6)
	db $00

se_gateofringsubsfx: ; Big support ring sub-sfx
	%inst(01)
	%dur($15) : %vol($00) : %pitch($A4)
	%dur($0C) : %vol($1E) : %pitch($BC)
	%pitch($BE)
	%pitch($BC)
	%pitch($BE)
	%dur($0C) : %vol($0F)
	%dur($1E) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($08)
	%dur($14) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($00)
	%dur($0A) : %pitch($C1)
	%dur($24) : %pitch($C6)
	db $00

se_cursor: ; Controls select
	%inst(01)
	%dur($08) : %vol($3C) : %pitch($BE)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($00)
	%dur($0A) : %pitch($BE)
	%dur($18) : %pitch($C3)
	db $00

se_controlsselectsubsfx: ; Controls select sub-sfx
	%inst(01)
	%dur($18) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($14)
	db $00
	%pitch($BE)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($05)
	db $00
	%pitch($BE)
	%dur($18) : %pitch($C3)
	db $00

se_itemcatch: ; 1up ring hit
	%inst(00)
	%dur($08) : %vol($28) : %pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($00)
	%dur($0A) : %pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($0C) : %pitch($C3)
	db $00

se_itemcatchsubsfx: ; 1up ring hit sub-sfx
	%inst(00)
	%dur($18) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($14)
	db $00
	%pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($05)
	db $00
	%pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($0C) : %pitch($C3)
	db $00

se_movingwallleft: ; Left gate moving
	%inst(08)
	%dur($18) : %vol($78)
	db $00
	%pitchslide($98,$18,$9D)
	%dur($24)
	%pitchenv($21,$A1)
	db $00

se_movingwallcentre: ; Centre gate moving
	%inst(08)
	%dur($18) : %vol($64)
	%pitchslide($98,$18,$9D)
	%dur($24)
	%pitchenv($21,$A1)
	db $00

se_movingwallright: ; Right gate moving
	%inst(08)
	%dur($18) : %vol($00)
	%dur($78)
	%pitchslide($98,$18,$9D)
	%dur($24)
	%pitchenv($21,$A1)
	db $00

se_movingwallmid: ; Mid gate moving
	%inst(08)
	%dur($18) : %vol($3C)
	%pitchslide($98,$18,$9D)
	%dur($24)
	%pitchenv($21,$A1)
	db $00

se_movingwallfar: ; Far gate moving
	%inst(08)
	%dur($18) : %vol($1E)
	%pitchslide($98,$18,$9D)
	%dur($24)
	%pitchenv($21,$A1)
	db $00

se_speedup: ; Player boost
	%inst(05)
	%dur($48) : %vol($78)
	%pitchslide($95,$45,$A1)
	db $00

se_speedupsubsfx: ; Player boost sub-sfx
	%inst(11)
	%dur($60) : %vol($3C)
	%pitchslide($91,$5D,$AB)
	db $00

se_speeddown: ; Player brake
	%inst(03)
	%dur($30) : %vol($78)
	%pitchslide($A3,$2D,$95)
	db $00

se_warning1: ; Incoming enemy
	%inst(26)
	%dur($0C) : %vol($28)
	%pitchslide($B6,$0A,$B9)
	%dur($24)
	%pitchslide($B6,$10,$B9)
	%dur($0C) : %vol($28)
	%pitchslide($B6,$0A,$B9)
	%dur($24)
	%pitchslide($B6,$10,$B9)
	%inst(15)
	%dur($24) : %vol($7D) : %pitch($98)
	%inst(13)
	%dur($70) : %vol($7D) : %pitch($8C)
	%inst(15)
	%dur($0C) : %vol($7D) : %pitch($98)
	db $00

se_warning2: ; Wing damaged
	%inst(26)
	%dur($30) : %vol($00) : %pitch($A4)
	%dur($0C) : %vol($28)
	%pitchslide($B6,$0A,$B9)
	%dur($0C) : %vol($28)
	%pitchslide($B6,$0A,$B9)
	%dur($18) : %vol($00) : %pitch($A4)
	%inst(15)
	%dur($24) : %vol($7D) : %pitch($98)
	%inst(14)
	%dur($20) : %vol($7D) : %pitch($8C)
	%inst(16)
	%dur($40) : %vol($7D) : %pitch($8C)
	%inst(15)
	%dur($0C) : %vol($7D) : %pitch($98)
	db $00
	%inst(0E)
	%dur($30) : %vol($32) : %pitch($B7)
	db $00

se_phantron2landing: ; Phantron 2 landing
	%inst(0D)
	%dur($06) : %vol($78) : %pitch($89)
	%dur($04) : %vol($00) : %pitch($89)
	%dur($24) : %vol($78)
	%pitchslide($89,$22,$82)
	db $00

se_phantron2jump: ; Phantron 2 jump
	%inst(1D)
	%dur($0C) : %vol($78)
	%pitchslide($A9,$0A,$9D)
	%dur($24)
	%pitchslide($9D,$21,$AB)
	db $00

se_missilenear: ; Near enemy rocket shot
	%inst(0B)
	%dur($30) : %vol($64) : %pitch($95)
	db $00

se_missilemid: ; Mid enemy rocket shot
	%inst(0B)
	%dur($30) : %vol($3C) : %pitch($95)
	db $00

se_missilefar: ; Far enemy rocket shot
	%inst(0B)
	%dur($30) : %vol($1E) : %pitch($95)
	db $00

se_enemybattrynear: ; Near enemy battery
	%inst(19)
	%dur($08) : %vol($78) : %pitch($9A)
	%dur($48) : %pitch($98)
	db $00

se_enemybattrymid: ; Mid enemy battery
	%inst(19)
	%dur($08) : %vol($46) : %pitch($9A)
	%dur($48) : %pitch($98)
	db $00

se_enemybattryfar: ; Far enemy battery
	%inst(19)
	%dur($08) : %vol($28) : %pitch($9A)
	%dur($48) : %pitch($98)
	db $00

se_laserleft: ; Left enemy laser shot
	%inst(09)
	%dur($30) : %vol($5A)
	%dur($0A) : %pitch($95)
	db $00

se_lasercentre: ; Centre enemy laser shot
	%inst(09)
	%dur($30) : %vol($50) : %pitch($95)
	db $00

se_laserright: ; Right enemy laser shot
	%inst(09)
	%dur($30) : %vol($0A)
	%dur($5A) : %pitch($95)
	db $00

se_lasermid: ; Mid enemy laser shot
	%inst(09)
	%dur($30) : %vol($28) : %pitch($95)
	db $00

se_laserfar: ; Far enemy laser shot
	%inst(09)
	%dur($30) : %vol($1E) : %pitch($95)
	db $00

se_wingdestructright: ; Right wing damaged
	%inst(05)
	%dur($30) : %vol($0A)
	%dur($5A) : %pitch($A9)
	db $00

se_wingdestructleft: ; Left wing damaged
	%inst(05)
	%dur($30) : %vol($5A)
	%dur($0A) : %pitch($A9)
	db $00

se_winglessarwingcollision: ; Wingless arwing collision
	%inst(05)
	%dur($18) : %vol($5A) : %pitch($B5)
	db $00

se_wingdamageright: ; Right wing crash
	%inst(05)
	%dur($18) : %vol($0A)
	%dur($5A) : %pitch($B5)
	db $00

se_wingdamageleft: ; Left wing crash
	%inst(05)
	%dur($18) : %vol($5A)
	%dur($0A) : %pitch($B5)
	db $00

se_playerdamage: ; Player crash
	%inst(0D)
	%dur($06) : %vol($64) : %pitch($9D)
	%dur($60) : %vol($78) : %pitch($9D)
	db $00

se_damageenemynear: ; Near enemy hit
	%inst(0D)
	%dur($20) : %vol($46)
	%pitchslide($8E,$1E,$85)
	db $00

se_damageenemymid: ; Mid enemy hit
	%inst(0D)
	%dur($20) : %vol($28)
	%pitchslide($8E,$1E,$85)
	db $00

se_damageenemyfar: ; Far enemy hit
	%inst(0D)
	%dur($20) : %vol($1E)
	%pitchslide($8E,$1E,$85)
	db $00

se_destructenemynear: ; Near enemy explosion
	%inst(06)
	%dur($60) : %vol($78) : %pitch($98)
	db $00

se_destructenemynearsubsfx: ; Near enemy explosion sub-sfx
	%inst(06)
	%dur($18) : %vol($00) : %pitch($98)
	%dur($60) : %vol($50) : %pitch($95)
	db $00

se_destructenemymid: ; Mid enemy explosion
	%inst(06)
	%dur($70) : %vol($5A) : %pitch($95)
	db $00

se_destructenemyfar: ; Far enemy explosion
	%inst(06)
	%dur($70) : %vol($46) : %pitch($95)
	db $00

se_destructbossnear: ; Near big boss explosion
	%inst(0D)
	%dur($0C) : %vol($78) : %pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($64) : %pitch($95)
	%dur($12) : %pitch($95)
	%dur($0C) : %vol($50) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossnearsubsfx: ; Near big boss explosion sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($64)
	db $00
	%pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($50)
	db $00
	%pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($3C)
	db $00
	%pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossnearsubsubsfx: ; Near big boss explosion sub-sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($00)
	%dur($64) : %pitch($97)
	%dur($18) : %pitch($97)
	%dur($0C) : %vol($00)
	%dur($50) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($00)
	%dur($3C) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossmid: ; Mid big boss explosion
	%inst(0D)
	%dur($0C) : %vol($64) : %pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($50) : %pitch($95)
	%dur($12) : %pitch($95)
	%dur($0C) : %vol($3C) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossmidsubsubsfx: ; Mid big boss explosion sub-sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($50)
	db $00
	%pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($3C)
	db $00
	%pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($28)
	db $00
	%pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossmidsubsfx: ; Mid big boss explosion sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($00)
	%dur($50) : %pitch($97)
	%dur($18) : %pitch($97)
	%dur($0C) : %vol($00)
	%dur($3C) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($00)
	%dur($28) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossfar: ; Far big boss explosion
	%inst(0D)
	%dur($0C) : %vol($3C) : %pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($32) : %pitch($95)
	%dur($12) : %pitch($95)
	%dur($0C) : %vol($1E) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_bigexplosionleft: ; Big explosion left
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($32)
	db $00
	%pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($1E)
	db $00
	%pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($14)
	db $00
	%pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossfarsubsfx: ; Far big boss explosion sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($00)
	%dur($32) : %pitch($97)
	%dur($18) : %pitch($97)
	%dur($0C) : %vol($00)
	%dur($1E) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($00)
	%dur($14) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_playerdown: ; Player down
	%inst(0D)
	%dur($0C) : %vol($78) : %pitch($9C)
	%pitch($9C)
	%dur($60) : %pitch($9C)
	db $00

se_playerdownsubsfx: ; Player down sub-sfx
	%inst(0D)
	%dur($06) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($00)
	%dur($64) : %pitch($98)
	%dur($0C) : %vol($5A)
	db $00
	%pitch($98)
	%dur($0C) : %vol($00)
	%dur($50) : %pitch($98)
	%dur($48) : %vol($46)
	db $00
	%pitch($95)
	%dur($60) : %vol($00)
	%dur($28) : %pitch($95)
	db $00

se_laser: ; Player laser shot
	%inst(20)
	%dur($48) : %vol($32) : %pitch($92)
	db $00

se_specialweapon: ; Player bomb explosion
	%inst(05)
	%dur($0C) : %vol($64) : %pitch($9C)
	%dur($30) : %pitch($9C)
	%inst(12)
	%dur($60) : %vol($64)
	%pitchslide($85,$60,$86)
	%dur($60) : %vol($7D)
	%pitchenv($5E,$87)
	db $00

se_abutton: ; Player bomb shot
	%inst(0B)
	%dur($06) : %vol($78) : %pitch($98)
	%dur($18) : %vol($78) : %pitch($9D)
	db $00

se_dualbeam: ; Dual beam shot
	%inst(19)
	%dur($08) : %vol($5A) : %pitch($A1)
	%inst(17)
	%dur($24)
	%pitchslide($98,$22,$B0)
	db $00

se_hitwallnear: ; Near laser deflect
	%inst(0A)
	%dur($24) : %vol($21) : %pitch($B2)
	db $00

se_hitwallmid: ; Mid laser deflect
	%inst(0A)
	%dur($24) : %vol($11) : %pitch($B2)
	db $00

se_hitwallfar: ; Far laser deflect
	%inst(0A)
	%dur($24) : %vol($0D) : %pitch($B2)
	db $00

se_wingtouchleft: ; Left wing scratch
	%inst(05)
	%dur($06) : %vol($5A)
	db $00
	%pitch($BC)
	%pitch($BC)
	db $00

se_wingtouchright: ; Right wing scratch
	%inst(05)
	%dur($06) : %vol($00)
	%dur($5A) : %pitch($BC)
	%pitch($BC)
	db $00

se_pause: ; Pause
	%inst(00)
	%dur($10) : %vol($1E) : %pitch($B4)
	%dur($30) : %vol($1E) : %pitch($B7)
	db $00

se_pausesubsfx: ; Pause sub-sfx
	%inst(00)
	%dur($08) : %vol($00) : %pitch($B0)
	%dur($10) : %vol($1E) : %pitch($B0)
	%dur($30) : %vol($1E) : %pitch($BC)
	db $00

; make sure patterns aren't too big
if !opt_f1_f9 == 0
%warnpc($238f)
endif

; 238F-24FC.bin
sfx_patches:
incsrc patches.asm			; 61 sound effect patches

_24FD:
	mov	a,#$80
	mov	y,#$5c
	call	apus
	mov	a,!_03c3
	and	a,#$80
	beq	_2512
	set7	!eons
	mov	y,#$4d
	call	apus

_2512:
	mov	!sf1,#$00
	clr7	!fkin
	mov	x,#$0e
	mov	a,!_021f
	call	snox
	mov	a,#$00
	mov	!_03c9,a
	mov.b	!_d1,a
	mov.b	!_ae,a
	mov.b	!_9e,a
	mov	a,!_03ee
	mov	!_038f,a
	mov	a,!_03ef
	mov	!_028e,a
	ret

_2537:
	mov	x,#$60
	mov.b	!_9e,x
	mov	!_03c9,x
	mov	a,#$00
	mov	!_032e,a
	mov.b	x,!_9e
	setc
	sbc	a,!_030f
	call	divx
	mov	!_031e,a
	mov	a,y
	mov	!_031f,a
_2553:
	mov.b	a,!_9e
	beq	_24FD
	cmp	a,#$01
	beq	_24FD
	mov	a,#$00
	mov	y,#$03
	mov	x,#$0e
	dec	!_9e
	call	_CC4
	mov	a,!_030f
	mov	!_032f,a
	mov	a,#$0a
	mov	!_035f,a
	mov.b	!kkk,a
	mov.b	!sss,#$00
	mov	x,#$0e
	call	pan20
	ret

_257c:
	mov	a,!_03f8
	beq	+
	mov.b	!fl1,#$00
+
	mov.b	y,!fl1s
	cmp.b	y,!fl1
	beq	_25a1
	mov.b	a,!fl1
	mov.b	!sf1,a
	mov.b	!fl1s,a
	beq	_2537
	mov	a,y
	beq	_25b5
	eor.b	a,!fl1
	and	a,#$c0
	bne	_25b5
	mov.b	a,!_d1

	bne	_25cc
	bra	_2618

_25a1:
	mov.b	a,!fl1
	bne	_25ac
	mov	x,!_03c9
	beq	+
	bra	_2553
_25ac:
	mov.b	a,!_d1
	bne	_25cc
	mov.b	a,!sf1
	bne	_262f
+
	ret

_25b5:
	mov	!_d1,#$02
	mov	a,#$80
	mov	y,#$5c
	call	apus
	set7.b	!fkin
	mov	a,#$00
	mov	!_028e,a
	mov.b	!_ae,a
	mov	!_038f,a
-
	ret

_25cc:
	dbnz	!_d1,-
	call	_2671
	mov	a,#$80
	call	_3e79
	mov.b	a,!sf1
	bmi	_25ee
	bbs6	!sf1,_25e6
	mov	y,#$70
	mov	x,#$96
	mov	a,#$a0
	bra	_25ff

_25e6:
	mov	y,#$f7
	mov	x,#$b2
	mov	a,#$ff
	bra	_25ff

_25ee:
	bbs6	!sf1,_25f9
	mov	y,#$ff
	mov	x,#$b2
	mov	a,#$ff
	bra	_25ff

_25f9:
	mov	y,#$bb
	mov	x,#$96
	mov	a,#$e0

_25ff:
	mov	!_03cb,y
	mov	!_03c6,x
	mov	!_03fc,a
	call	create_engine_sound_brr
	clr7	!eons
	mov.b	a,!eons
	mov	y,#$4d
	call	apus
	mov	a,#$01
	bne	+
_2618:
	mov	a,#$30
+
; (CPUIO1 ($2141/$F5) Protocol)
; %xxyyyyyy - Voice 7 SFX Instance Control
; - %yyyyyy is an array ID to an engine frequency note to jump to briefly via a 48 SFX tempo tick pitch bend
; (it otherwise maintains a roughly consistent note, with some random variance for every 112 SFX tempo tick pitch bend).
; The highest two bits need to be cleared because of the internal array size: it is otherwise effectively valid.
	mov.b	!_ae,a
	mov	!_af,#$00
	mov.b	a,!sf1
	and	a,#%00111111
	mov	x,a
	mov	a,engine_freq_table+x
	mov	x,#$0e
	mov.b	!chn,x
	call	swpadset
	ret

_262f:
	clr7	!uuu
	mov.b	a,!_ae
	beq	_265c
	mov	x,#$0e
	call	_3e5f
	mov	a,!_03fc
	mov	!_032f,a
	mov	!_030f,a
	mov	a,#$0a
	mov	!_035f,a
	mov	!_033f,a
	mov	x,#$0e
	mov	a,!pand+x
	mov	y,a
	mov	a,!pandw+x
	movw	!sss,ya
	mov	a,#$0e
	call	pan20
	ret

_265c:
	mov	a,#$70
	mov.b	!_ae,a
	mov	!_af,#$00
	mov.b	a,!rdm
	and	a,#$03
	or	a,#$a4
	mov	x,#$0e
	mov.b	!chn,x
	call	swpadset
	ret

; (CPUIO1 ($2141/$F5) Protocol)
; %xxyyyyyy - Voice 7 SFX Instance Control
; - %xx is effectively an engine sound ID, utilizing the noise BRR generator and modifying how it is output, in addition to also acting as a standard SFX instrument ID.
_2671:
	mov.b	a,!sf1
	and	a,#%11000000
	clrc
	rol	a
	rol	a
	rol	a
	if !opt_misc == 0
	mov	x,a 				; X gets overwritten 2 instructions later, why?
	endif
	mov	y,#$06
	mul	ya
	mov	x,a
	mov	y,#$74
	mov	!ttt,#$04
-
	mov	a,engine_sound_id_table+x
	call	apus
	inc	x
	inc	y
	dbnz	!ttt,-
	mov	a,engine_sound_id_table+x
	mov	!_022f,a
	inc	x
	mov	a,engine_sound_id_table+x
	mov	!_022e,a
	ret

; related to determining wavering in pitch?
engine_sound_id_table:
	db $20, $00, $00, $E8, $04, $00, $20, $00
	db $00, $EF, $00, $60, $20, $00, $00, $E5
	db $00, $80, $20, $00, $00, $E8, $01, $C0

; table of engine frequency notes
engine_freq_table:
	db $A4, $A6, $A7, $A8, $A6, $A7, $A8, $A9
	db $B0, $B0, $B0, $B0, $98, $98, $98, $98

_26c4:
	mov	a,#$00
	mov.b	!sf2,a
	mov.b	!sf3c,a
	mov	!_03f6,a
	mov.b	!_ac,a
	mov.b	!_9c,a
	mov.b	!_9d,a
	mov	!_032d,a
	mov	!_030d,a
	mov	a,!_03ec
	mov	!_038d,a
	mov	a,!_03ed
	mov	!_028c,a
	clr6	!fkin
	mov	x,#$0c
	mov	a,!_021d
	call	snox
	mov	a,!_03c3
	and	a,#$40
	beq	_26fd
	set6	!eons
	mov	y,#$4d
	call	apus

_26fd:
	mov	a,#$40
	mov	y,#$5c
	jmp	apus

_2704:
	mov	x,!_03f6
	mov.b	!_9c,x
	mov	a,#$00
	mov	!_032c,a
	mov.b	x,!_9c
	setc
	sbc	a,!_030d
	call	divx
	mov	!_031c,a
	mov	a,y
	mov	!_031d,a

_271e:
	mov.b	a,!_9c
	bne	+
	ret

+
	cmp	a,#$01
	beq	_26c4
	mov	a,#$00
	mov	y,#$03
	mov	x,#$0c
	dec	!_9c
	call	_CC4
	mov	a,!_030d
	mov	!_032d,a
	mov	a,!_03fb
	mov	!_035d,a
	mov.b	!kkk,a
	mov.b	!sss,#$00
	mov	x,#$0c
	call	pan20
	ret

_2749:
	mov	a,!_03f8
	beq	+
	mov	!fl2,#$00

+
	mov.b	y,!fl2s
	cmp.b	y,!fl2
	beq	_277d
	mov.b	a,!fl2
	mov.b	!sf2,a
	mov.b	!fl2s,a
	and	a,#$c0
	beq	_2704
	mov	a,y
	eor.b	a,!fl2
	and	a,#$0f
	bne	_2779
	mov.b	a,!sf3c
	bne	_27a4
	mov	a,y
	eor.b	a,!fl2
	and	a,#$30
	beq	+
	jmp	_27ff
+
	jmp	_2839
_2779:
	mov.b	a,!fl2
	bne	_278D

_277d:
	mov.b	a,!fl2
	beq	_271e
	mov.b	a,!sf3c
	bne	_27a4
	mov.b	a,!sf2
	beq	_278C
	jmp	_285f
_278C:
	ret
_278D:
	mov	!sf3c,#$02
	mov	a,#$40
	mov	y,#$5c
	call	apus
	set6	!fkin
	mov	a,#$00
	mov	!_028c,a
	mov.b	!_ac,a
	mov	!_038d,a
	ret
_27a4:
	dbnz	!sf3c,_278C
	mov.b	a,!sf2
	and	a,#$0f
	setc
	sbc	a,#$01
	mov	x,a
	mov	a,_2921+x
	mov	!_03f6,a
	mov	a,_2930+x
	mov	!_03f9,a
	mov	a,x
	call	_289d
	mov.b	a,!sf2
	and	a,#$30
	bne	_27ca
	mov	y,!_03f9
	bne	_27cc
_27ca:
	mov	y,#$ad
_27cc:
	mov	!chn,#$0c
	mov	x,#$0c
	call	dss
	mov	a,#$40
	call	_3e79
	clr6	!eons
	mov.b	a,!eons
	mov	y,#$4d
	call	apus
	mov.b	a,!sf2
	and	a,#$30
	xcn	a
	mov	x,a
	mov	a,_291d+x
	mov	!_032d,a
	mov	!_030d,a
	mov.b	a,!sf2
	and	a,#$c0
	xcn	a
	lsr	a
	lsr	a
	mov	x,a
	mov	a,_2919+x
	mov	!_033d,a

_27ff:
	mov	x,!_03f6
	mov.b	!_9c,x
	mov.b	a,!sf2
	and	a,#$30
	bne	_280f
	mov	a,!_03f9
	bne	_2811
_280f:
	mov	a,#$ad
_2811:
	mov.b	!_ac,x
	mov	!_ad,#$00
	mov	x,#$0c
	mov.b	!chn,x
	call	swpadset
	mov.b	a,!sf2
	and	a,#$30
	xcn	a
	mov	x,a
	mov	a,_291d+x
	mov	!_032c,a
	mov.b	x,!_9c
	setc
	sbc	a,!_030d
	call	divx
	mov	!_031c,a
	mov	a,y
	mov	!_031d,a
_2839:
	mov.b	a,!sf2
	and	a,#$c0
	xcn	a
	lsr	a
	lsr	a
	mov	x,a
	mov	a,_2919+x
	mov	!_03fb,a
	mov	!_035c,a
	setc
	sbc	a,!_033d
	mov	x,!_03f6
	mov.b	!_9d,x
	call	divx
	movw	!sss,ya
	mov	!_034c,a
	mov	a,y
	mov	!_034d,a
_285f:
	mov.b	a,!_9c
	beq	_2874
	mov	a,#$00
	mov	y,#$03
	mov	x,#$0c
	dec	!_9c
	call	_CC4
	mov	a,!_030d
	mov	!_032d,a

_2874:
	clr7	!uuu
	mov.b	a,!_ac
	beq	+
	mov	x,#$0c
	call	_3e5f

+
	mov.b	a,!_9d
	beq	_289c
	mov	a,#$30
	mov	y,#$03
	mov	x,#$0c
	dec	!_9d
	call	_CC4
	mov	a,!_033d
	mov	y,a
	mov	a,!_033c
	movw	!sss,ya
	mov	x,#$0c
	call	pan20

_289c:
	ret

_289d:
	mov	y,#$06
	mul	ya
	mov	x,a
	mov	y,#$64
	mov	!ttt,#$04
_28a6:
	mov	a,special_patches+x
	call	apus
	inc	x
	inc	y
	dbnz	!ttt,_28a6
	mov	a,special_patches+x
	mov	!_022d,a
	inc	x
	mov	a,special_patches+x
	mov	!_022c,a
	ret

; $28BF
; Second table of sound effect patches
; Something to do with looping sounds that have stereo panning based
; on location relative to player
special_patches:
;	VxSRCN, VxADSR1, VxADSR2, VxGAIN, pitch mult base, pitch mult fractional (256ths)
	db $20, $0C, $E0, $70, $02, $80 ; $00 Arwing engine
	db $20, $0C, $E0, $60, $07, $00 ; $01 Arwing engine
	db $00, $0E, $E0, $70, $03, $00 ; $02
	db $0A, $0E, $E0, $70, $01, $80 ; $03
	db $01, $0E, $E0, $7F, $01, $00 ; $04
	db $01, $0E, $E0, $28, $07, $00 ; $05
	db $2D, $0E, $E0, $70, $01, $00 ; $06 Helicopter
	db $03, $0E, $E0, $7F, $01, $40 ; $07
	db $03, $0E, $E0, $70, $03, $00 ; $08
	db $13, $0E, $E0, $60, $00, $60 ; $09 
	db $0A, $0E, $E0, $7F, $00, $60 ; $0A
	db $0B, $0E, $E0, $60, $05, $00 ; $0B
	db $02, $0E, $E0, $7F, $01, $80 ; $0C
	db $13, $0E, $E0, $70, $01, $00 ; $0D
	db $02, $0E, $E0, $40, $08, $00 ; $0F

_2919:	
	db $0A, $14, $0A, $00

_291d:
	db $FF, $90, $60, $30

_2921:
	db $30, $18, $40, $40, $50, $28, $20, $60
	db $40, $40, $40, $40, $48, $20, $20

_2930:
	db $AB, $A1, $AD, $AD, $AD, $A9, $AC, $AD
	db $AD, $AD, $AD, $AD, $AD, $AD, $AB


_293f:
	mov.b	x,!fl3
	mov.b	!kkk,x
	mov	a,sfx_parameters-1+x			; get voice ID and SFX priority
	mov.b	!sss,a
	xcn	a								; swap nibbles
	and	a,#$0f							; get voice ID only
	asl	a
	mov	y,a
	mov	a,!_03a0+y
	beq	_2960
	mov	x,a
	mov	a,sfx_parameters-1+x			; get voice ID and SFX priority
	setc
	cmp.b	a,!sss
	beq	_2960
	bcc	_2960
	jmp	_3eba

_2960:
	mov.b	a,!kkk
	mov	!_03a0+y,a
	mov.b	!sss,y
	mov	a,#$01
	lsr.b	!sss
	beq	_2971
-
	asl	a
	dbnz	!sss,-

_2971:
	mov	!_03c1,a
	mov	!_03c0,y
	mov	a,!_03c1
	or.b	a,!fkin
	mov.b	!fkin,a
	jmp	_29c5

; function obtaining info from SPC<->5A22 register 3
_2981:
	mov	a,!port3
	cmp	a,!port3
	bne	_2981
	mov	!port3,a
	mov	y,a
	mov.b	a,!fl3s
	mov.b	!fl3s,y
	cbne	!fl3s,+
	mov	y,#$00
+
	mov.b	!fl3,y
	mov.b	a,!fl3
	beq	+
	cmp	a,#!max_sfx				; Sound FX ID overflow check, thanks KungFuFurby
	bcc	_29c2
+
	ret

_29A1:
	mov	a,!_03f8
	beq	_29be
	mov	a,#$00
	mov	!_03f8,a
	call	_3e96
	bra	_29be

_29b0:
	mov.b	a,!fkin
	and	a,#$c0
	eor	a,#$ff
	mov	!_03f8,a
	mov	y,#$5c
	call	apus

_29be:
	mov.b	a,!fl3
	bra	_29d5
_29c2:
	jmp	_293f
_29c5:
	call	_3ea6
	mov	x,!_03c0
	mov.b	a,!fl3
	cmp	a,#$01
	beq	_29A1
	cmp	a,#$02
	beq	_29b0
_29d5:
	mov	!_03a0+x,a
	cmp	a,#$0b
	bcc	_29eb
	cmp	a,#$0e
	bcc	+
	cmp	a,#$15
	bcc	_29eb
	cmp	a,#$18
	bcs	_29eb
+
	call	_3e87
_29eb:
	mov	a,#$03
	mov	!_03a1+x,a
	mov	a,#$00
	mov	!_0280+x,a
	mov.b	!swpc+x,a
	mov	!tund+x,a
	mov	!ptps+x,a
	mov	a,!_03c1
	or	a,!sf3
	mov	!sf3,a
	mov	a,!_03c1
	mov	y,#$5c
	call	apus
	mov	a,!_03a0+x
	mov	x,a
	mov	a,sfx_chain_table-1+x	;  get SFX ID to trigger alongside the SFX ID that was called
	mov.b	!fl3,a
	bne	_29c2
	ret

chd:
	mov	a,!sf3
	mov	!_03ce,a
	beq	_2a51
	mov	x,#$0a
	mov	a,#$20
	mov	!_03c1,a
	asl	!_03ce
	asl	!_03ce
_2a2f:
	asl	!_03ce
	bcc	_2a4a
	mov	!_03c0,x
	mov	a,x
	xcn	a
	lsr	a
	mov	!_03c2,a
	mov	a,!_03a1+x
	bne	_2a52
	mov	a,!_03a0+x
	beq	_2a4a
	jmp	_2b0c
_2a4a:
	lsr	!_03c1
	dec	x
	dec	x
	bpl	_2a2f
_2a51:
	ret

_2a52:
	mov	!_03c0,x
	mov	a,!_03a1+x
	dec	a
	mov	!_03a1+x,a
	beq	+
	if !opt_misc == 0
	jmp	_2a4a				; Why a jump instead of a branch?
	else
	bra	_2a4a				; saves 1 byte, but costs 1 more cycle
	endif
+
	mov	a,!_03a0+x			; get sound effect index number
	asl	a					; double it to get pointer
	mov	y,a					; check if over 255
	bcs	_2a7b
	mov	a,sfx_ptrs-1+y		; sound effect data pointer high
	mov	!_0391+x,a
	mov.b	!adk+1,a
	mov	a,sfx_ptrs-2+y		; sound effect data pointer low
	mov	!_0390+x,a
	mov.b	!adk,a
	jmp	_2b29
_2a7b:
	mov	a,sfx_ptrs-1+256+y	; sound effect data pointer high
	mov	!_0391+x,a
	mov.b	!adk+1,a
	mov	a,sfx_ptrs-2+256+y	; sound effect data pointer low
	mov	!_0390+x,a
	mov.b	!adk,a
	jmp	_2b29

_2a8e:
	mov	x,!_03c0
	mov	a,!_03a0+x
	cmp	a,#$0b
	bcc	_2aac
	cmp	a,#$0e
	bcc	+
	cmp	a,#$15
	bcc	_2aac
	cmp	a,#$18
	bcs	_2aac
+
	mov	a,!_03ca
	bne	_2aac
	call	_3e96

_2aac:
	mov	a,#$00
	mov	!_03a0+x,a
	mov.b	!swpc+x,a
	mov	a,!_03d0+x
	mov	!ptps+x,a
	mov	a,!_03e0+x
	mov	!tund+x,a
	mov	a,!_03e1+x
	mov	!_0280+x,a
	mov.b	a,!fkin
	setc
	sbc	a,!_03c1
	mov.b	!fkin,a
	mov	a,!sf3
	setc
	sbc	a,!_03c1
	mov	!sf3,a
	mov.b	!chn,x
	mov	a,!snos+x
	call	snox
	mov	a,!_03c1
	and	a,!_03c3
	beq	_2b02
	and.b	a,!eons
	bne	_2b02
	mov.b	a,!eons
	clrc
	adc	a,!_03c1
	mov.b	!eons,a
	mov	y,#$4d
	call	apus
	mov	a,!_03f3
	setc
	sbc	a,!_03c1
	mov	!_03f3,a
_2b02:
	mov	x,!_03c0
	ret

_2B06:
; End of sound effect data
	call	_2a8e
	jmp	_2a4a
_2b0c:
	call	_3ea6
	mov	!_03c0,x
	mov	a,!_0391+x
	mov	y,a
	mov	a,!_0390+x
	movw	!adk,ya
	mov	a,!_03b0+x
	dec	a
	mov	!_03b0+x,a
	beq	_2B27
	jmp	_2b94

_2B27:						; Jumped to from PROG_CODE_02.asm
	incw	!adk			; sound effect data pointer
_2b29:
	mov	a,!_03c0			; Get audio channel
	xcn	a
	lsr	a
	mov	!_03c2,a
	mov	x,#$00				; reset X
	mov	a,(!adk+x)			; Get next byte of sound effect data
	beq	_2B06				; 00 is end of sound effect data
	bmi	_2B6F				; Pitch and commands are 80 and over

_2B39:
; Sample duration
	mov	y,!_03c0			; Get audio channel
	mov	!_03b1+y,a			; store duration
	incw	!adk			; sound effect data pointer
	mov	a,(!adk+x)			; Get next byte of sound effect data
	mov.b	!sss,a			; Store it as volume
	bmi	_2B6F

; Value is under 80
	mov	y,!_03c2			; Get DSP register address
	call	apus			; Set DSP register (which?)
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)			; Get next byte of sound effect data
	bpl	_2B62				; Branch if byte is a volume setting (under $80)

; Set default volume
	mov	x,a					; store data byte
	mov.b	a,!sss			; Get last volume used
	mov	y,!_03c2			; Get DSP register address
	inc	y
	call	apus			; Set DSP register (volume)
	mov	a,x					; restore data byte
	bra	_2B6F

_2B62:
; Set new volume level
	mov	y,!_03c2			; Get DSP register address
	inc	y
	call	apus			; Set DSP register (volume)
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)

_2B6F:
; Pitch and commands
	cmp	a,#$e0				; Command: sample change
	bne	+
	jmp	_3e20				; PROG_CODE_02.asm, returns to _2B27

+
	cmp	a,#$f9				; Command Note + Pitch Slide To Note
	beq	_2BC1

	cmp	a,#$f1				; Command Note Pitch Envelope
	beq	_2BD6

; Pitch
	mov	x,!_03c0			; Get audio channel
	mov	y,a
	call	dss
	mov	a,!_03c1
	call	_3e79

_2B8B:
	mov	x,!_03c0
	mov	a,!_03b1+x
	mov	!_03b0+x,a
_2b94:
	clr7	!uuu
	mov	x,!_03c0
	mov.b	a,!swpc+x
	beq	+
	call	_3e5f
	bra	_2bb1
+
	mov	a,#$02
	cmp	a,!_03b0+x
	bne	_2bb1
	mov	a,!_03c1
	mov	y,#$5c
	call	apus
_2bb1:
	mov	x,!_03c0
	mov.b	a,!adk+1
	mov	!_0391+x,a
	mov.b	a,!adk
	mov	!_0390+x,a
	jmp	_2a4a

_2BC1:
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)			; Get next byte of sound effect data
	mov	x,!_03c0			; Get audio channel
	mov.b	!chn,x
	mov	y,a
	call	dss
	mov	a,!_03c1
	call	_3e79

_2BD6:
	if !opt_f1_f9 == 0
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)			; Get next byte of sound effect data
	else
	mov	a,#$00
	endif
	mov	x,!_03c0
	mov.b	!swphc+x,a		; Delay in ticks
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)			; Get next byte of sound effect data
	mov	x,!_03c0
	mov.b	!swpc+x,a		; Length of slide
	push	a
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)			; Get next byte of sound effect data
	pop	y
	mov	x,!_03c0
	mov.b	!chn,x			; Target note
	call	swpadset
	jmp	_2B8B

%warnpc($2bff)

endspcblock


; ===============================================
; PROG_CODE_02
spcblock $3e20 nspc

_3e20:
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)
	mov	y,#$06
	mul	ya
	mov	!_d2,#sfx_patches
	mov	!_d2+1,#sfx_patches>>8
	addw	ya,!_d2
	movw	!_d2,ya
	mov	y,#$00
	mov	a,!_03c2
	or	a,#$04
	mov	x,a
	mov	!ttt,#$04

_3e3e:
	mov	a,(!_d2)+y
	push	y
	push	x
	pop	y
	call	apus
	push	y
	pop	x
	pop	y
	inc	x
	inc	y
	dbnz	!ttt,_3e3e
	mov	a,(!_d2)+y
	mov	x,!_03c0
	mov	(!bls+1)+x,a
	inc	y
	mov	a,(!_d2)+y
	mov	!bls+x,a
	jmp	_2B27

_3e5f:
	set7	!uuu
	mov	a,#$60
	mov	y,#$03
	dec.b	!swpc+x
	call	_CC4
	mov	a,!swpd+x
	mov	y,a
	mov	a,!swpdw+x
	movw	!sss,ya
	mov	!keyd,#$00
	jmp	dssx

_3e79:
	push	a
	mov	y,#$5c
	mov	a,#$00
	call	apus
	pop	a
	mov	y,#$4c
	jmp	apus

_3e87:
	mov	a,!_03f1
	bne	_3ea5
	mov.b	a,!mvo
	mov	!_03f1,a
	mov	a,#$88
	mov.b	!mvo,a
	ret

_3e96:
	mov	a,!_03f1
	beq	_3ea5
	mov	a,!_03f1
	mov.b	!mvo,a
	mov	a,#$00
	mov	!_03f1,a
_3ea5:
	ret

_3ea6:
	mov	a,!_03c1
	and.b	a,!eons
	beq	_3eba
	mov.b	a,!eons
	setc
	sbc	a,!_03c1
	mov.b	!eons,a
	mov	y,#$4d
	call	apus

_3eba:
	ret

%warnpc($3ebb)

endspcblock execute $400
