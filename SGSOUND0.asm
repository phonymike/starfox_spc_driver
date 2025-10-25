; Asar 1.91
org $008000
arch spc700


; ===============================================
; PROG_CODE_00
spcblock $3ee8 nspc

gate:
	db $32, $65, $7F, $98, $B2, $CB, $E5, $FC

volt:
	db $19, $32, $4C, $65, $72, $7F, $8C, $98
	db $A5, $B2, $BF, $CB, $D8, $E5, $F2, $FC

assert pc() <= $3f00, "Used too much space"

endspcblock


; ===============================================
; PROG_CODE_01
spcblock $400 nspc

incsrc defines.asm
incsrc KAN.asm

	clrp				; clear direct page flag
;................................................
	mov	x,#$cf			; stack pointer 
	mov	sp,x			;
;
	mov	a,#$00			; clear RAM 000h-0dfh
	mov	x,a
;
start10:
	mov	(x+),a
	cmp	x,#$df+1
	bne	start10
;........................................
	mov	x,#$00
-
	mov	!ngs+x,a
	inc	x
	bne	-

-
	mov	!pvodw+x,a
	inc	x
	bne	-
;........................................
	inc	a
	call	esaset			; EDL & ESA set
;
	set5	!flgs			; echo off
;................................................
	mov	a,#$96
	mov	!_03c6,a
	mov	a,#$bb
	mov	!_03cb,a
	call	_648
;................................................
	mov	a,#$60
	mov	y,#$0c			; MVOL
	call	apus
;
	mov	y,#$1c			; MVOR
	call	apus
;
	mov	a,#!src_dir		; source dir = $3c00
	mov	y,#$5d			; DIR
	call	apus			; 19 byte
;........................................
	mov	a,#$f0			; inputport reset
	mov	!cont,a			; timer stop
;
	mov	a,#$10			; 2mS
	mov	!tmset,a		; timer data set
	mov.b	!tmp,a
;
	mov	a,#$01			; timer start
	mov	!cont,a
;++++++++++++++++++++++++++++++++++++++++++++++++
start20:				; 2mS
;........................................
;	mov	a,ffk			; !! test !!
;	bne	start55			; !! test !!
;........................................
	mov	y,#10			; 10 data set
start24:
	cmp	y,#05
	beq	start25
	bcs	start26
;
	cmp	!ekin,!eclr		; echo clear chu ?
	bne	start28			; EON EFB EVOR EVOL
;
start25:
	bbs7	!ekin,start28		; echo kinshi chu ? (FLG)
;
start26:
	mov	a,dseta-1+y
	mov	!apuadd,a		; write address
	mov	a,dsetd-1+y
	mov	x,a
	mov	a,(x)
	mov	!apudt,a		; data write
start28:
	dbnz	y,start24		; 18 byte
;
	mov.b	!keyons,y
	mov.b	!keyoffs,y
;...................
	mov.b	a,!rdm			; random keisan
	eor.b	a,!rdm+1
	lsr	a
	lsr	a
	notc				; d1 check
	ror.b	!rdm
	ror.b	!rdm+1
;........................................
start40:
	mov	y,!tmdt			; timer read
	beq	start40
;
	push	y			; 2mS goto
;................................................
	mov	a,#14*4			; 14 count
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
	mov	x,#$02			; fl2 & port2 check
	call	flset		; x = fl?
	call	chd			; sound effect
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
	pop	y			; timer count 
	mul	ya			;
;
	clrc
	adc.b	a,!tmpd
	mov.b	!tmpd,a
	bcc	start60
;...................
	mov	a,!_03f8
	bne	+
start55:
	call	cha			; music
;
	mov	x,#$00			; fl0 & port0 check
	call	_4FE			; x = fl?
+
	jmp	start20
;................................................
start60:
	mov.b	a,!sf0
	beq	start20x
;...
	mov	x,#$00			; hokan routin (8 ch.)
	mov	!keyd,#$01		; key data set
;
start62:
	mov.b	a,!add+1+x
	beq	start64			; kami = 0
;
	call	trry			; pan move & sweep & vib check
;
start64:
	inc	x
	inc	x
	asl.b	!keyd			;
	bne	start62			; channel end ? (8ch)
;
start20x:
	call	_614
	jmp	start20			; channel end
;************************************************
flset:
	mov.b	a,!sf0+x		; flag set flx
	mov	!port0+x,a		; flag return
;
flset02:
	mov	a,!port0+x		; flag read
	cmp	a,!port0+x		; 2 kai check
	bne	flset02			;
;
	mov	y,a
	mov	!fl0+x,y		; new data
;
dssr:
	ret
;************************************************
_4FE:
	mov.b	a,!sf0+x		; flag set flx
	mov	!port0+x,a		; flag return
-
	mov	a,!port0+x		; flag read
	cmp	a,!port0+x		; 2 kai check
	bne	-			;
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
	cmp	y,#!dd0			; drums check
	bcc	dss0
;************************************************ 
;		drums set    ; x=channel  a=sno 
;************************************************ 
; vcmds ca-df - percussion note
dds:
	call	snoset			; sno data set
	mov	y,#!c30			; (takasa)
;................................................
; vcmds 80-c7,c8,c9 - note/tie/rest
dss0:
	cmp	y,#!xxx			; tai or yyy ? 
	bcs	dssr
;................................................
	mov.b	a,!fkin			; kinshi flag check
	and.b	a,!keyd
	bne	dssr
;......
; vcmds 80-c7 - note (note number in Y)
	mov	a,y
	and	a,#$7f			; fre. set & flag set
	clrc				; key trans. add.
	adc.b	a,!ktps
	clrc
	adc	a,!ptps+x
	mov	!swpd+x,a		; ontei store
;
	mov	a,!tund+x
	mov	!swpdw+x,a		; sweep shosuten ika
;................................................
	mov	a,!vibcs+x		;
	lsr	a
	mov	a,#$00
	ror	a
	mov	!vibc+x,a		; count data (00h or 80h)
;
	mov	a,#$00
	mov.b	!vibhc+x,a		; vib hold
	mov	!vibcc+x,a		; vib change
	mov	!trec+x,a		; tre count = 0
	mov.b	!trehc+x,a		; tre hold
;
	or	(!vols),(!keyd)		; vol set flag
	or	(!keyons),(!keyd)	; keyon 
;................................................
	mov	a,!swsc+x		; sweep check
	mov.b	!swpc+x,a		; sweep (counter)
	beq	dss6
;................................................
	mov	a,!swshc+x
	mov.b	!swphc+x,a		; sweep (hold)
;
	mov	a,!swsk+x		; sws or swk ?
	bne	dss3
;......
	mov	a,!swpd+x		; (sws)
	setc				;
	sbc	a,!swss+x		;
	mov	!swpd+x,a		;
;......	
dss3:
	mov	a,!swss+x		; + ? (swk)
	clrc
; set DSP pitch from $10/1
	adc	a,!swpd+x		; now + @
;......
	call	swpadset		; sweep data set
;........................................ from kokaon
dss6:
	call	swpdset			; kkk sss <-- swpd swpdw
;************************************************
;		fre. data set   kkk & sss  x=channel  bls set
;************************************************
dssx:
	mov	y,#$00			; S curve hosei
	mov.b	a,!kkk
	setc
	sbc	a,#52			; e40 = 52
	bcs	dssx04			; e40 ijo add
;...
dssx02:
	mov.b	a,!kkk
	setc
	sbc	a,#19			; g10 = 19
	bcs	dssx10
;
	dec	y			; y = 0ffh
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
	mov	x,#24			; decimal
	div	ya,x			; ya/x = a ... y
	mov	x,a			; x = oct.
;...
	mov	a,gfd+1+y		; high
	mov.b	!adx+1,a
	mov	a,gfd+y			; low
	mov.b	!adx,a
;
	mov	a,gfd+3+y		; high
	push	a
	mov	a,gfd+2+y		; low
	pop	y
	subw	ya,!adx			; ya - adx
;...					; ( 0.sss x ya ) + adx  = adx
	mov.b	y,!sss
	mul	ya			; shimo x 0.???
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
	cmp	x,#06			; x = oct.
	bne	dssx12
	mov.b	!adx,a
;
	pop	x
;........................................
	mov	a,!bls+x		; 0. block su
	mov.b	y,!adx+1		; fre.(high)
	mul	ya			;
	movw	!adx+2,ya		;
;
	mov	a,!bls+x		; 0. block su
	mov.b	y,!adx			; fre.(low)
	mul	ya			;
	push	y			; --> low
;
	mov	a,!bls+1+x		; block su
	mov.b	y,!adx			; fre.(low)
	mul	ya			;
	addw	ya,!adx+2
	movw	!adx+2,ya		;
;
	mov	a,!bls+1+x		; block su
	mov.b	y,!adx+1		; fre.(high)
	mul	ya
	mov	y,a
	pop	a			; <-- low
	addw	ya,!adx+2
	movw	!adx+2,ya		; freq. set
;................................................
	mov	a,x			; apunch
	xcn	a
	lsr	a
	or	a,#$02			; pl1 = 2
	mov	y,a			; write address
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
	mov	!apuadd,y		; write address
	mov	!apudt,a		; data write
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
_614:
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
	mov	!_fe00+y,a
	inc	y
	mov.b	a,!rdm
	or	a,#$11
	mov	!_fe00+y,a
	inc	y
	dec	x
	bne	-
	dbnz	!ttt,_621
	ret
	_648:
	mov	y,#$00
	mov	x,#$1b
	mov	a,!_03c6
-
	mov	!_fe00+y,a
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
	inc	a
	mov	!_fe00+y,a
	mov	y,#$fe
	mov	a,#$00
	mov	!_3c80,a
	mov	!_3c81,y
	mov	!_3c82,a
	mov	!_3c83,y
	mov.b	a,!rdm
	or.b	a,!rdm+1
	bne	+
	inc.b	!rdm
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
	call	_648
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
	cmp	a,#$f4
	beq	_71D
	cmp	a,#$f5
	beq	_717
	cmp	a,#$f0				; stop music
	beq	_744
	cmp	a,#$14
	bcc	_766
	ret

_717:
	mov	x,#$03
	mov	a,#$30
	bne	+
_71D:
	mov	x,#$fe
	mov	a,#$09
+
	mov	!tmpc,#$8f
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
	mov	y,#00			; block address set
	mov	a,(!ads)+y
	incw	!ads
	push	a			; shimo
	mov	a,(!ads)+y
	incw	!ads
	mov	y,a			; kami
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
;	beq	ks04			; 000h = end
;......
	mov	x,a			; shoki data set
	mov	a,!gft-2+1+x		; block add. shoki set
	mov	y,a
	bne	+
	mov.b	!sf0,a
	ret
+
	mov	a,!gft-2+x
	movw	!ads,ya
;......
	mov	!sf0c,#$02		; count
;...................
ks04:
	mov.b	a,!fkin			; key off
	eor	a,#$ff
	tset	!keyoffs,a		; keyoff set
	ret
;................................................ 
; reset song params
ks10:
	mov	x,#14			; shoki data set
	mov	!keyd,#$80		; last voice

ks12:
	mov.b	a,!keyd
	and.b	a,!fkin
	and	a,#$c0
	bne	_7BC
;
	mov	a,#!voice_vol	;voice volume = $ff
	mov	!pvod+x,a		; part vol
;
	mov	a,#10			; pan data set
	call	panx			; pand & panf  set    (a=0)
;
	mov	!snos+x,a		; sound number
	mov	!tund+x,a		; tun shoki set
	mov	!ptps+x,a		; part tran. set
	mov	!swsc+x,a		; sweep count
	mov	!_03e1+x,a
	mov	!_03e0+x,a
	mov	!_03d0+x,a
	mov.b	!vibd+x,a		; vib depth
	mov.b	!tred+x,a		; tre depth
;
_7BC:
	dec	x
	dec	x			; - 2
	lsr.b	!keyd
	bne	ks12				;loop for each voice
;......
	mov.b	!mvoc,a			; mvol count (a=0)
	mov.b	!evoc,a			; evol count 
	mov.b	!tmpc,a			; tempo count set
	mov.b	!ktps,a			; key trans. set
	mov.b	!blc,a			; block count
	mov.b	!wavs,a			; source
;
	mov	!mvo,#!main_vol		; main volume set
	mov	!tmp,#!tempo		; tempo data set
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
	beq	char			;
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
	call	adset			; block address set (Z=kami)
;......
	bne	ks40
;......
	mov	y,a			; shimo = 0 ?
	bne	ks24			; music end ?
;........................................
;************************************************
	jmp	_744
;************************************************
; set/dec repeat count
ks24:
	dec.b	!blc
	bpl	ks26
;
	mov.b	!blc,a			; blc=0 or 129 ijo
;
ks26:
	call	adset			; kurikaeshi ?
;
	mov.b	x,!blc			; blc = 0 ?
	beq	ks20			;
;
	movw	!ads,ya			; kurikaeshi ads set
	bra	ks20
;........................................
; load start addresses - hi-byte not zero
ks40:
	movw	!adx+2,ya		; adx+2,+3 set
	mov	y,#15			; shoki address set (8ch)
;
ks42:
	mov	a,(!adx+2)+y		; part sento add. set
	mov	!add+y,a		; add.
	dec	y
	bpl	ks42	; set all reading ptrs
;........................................
	mov	x,#$00			; shoki data set
	mov	!keyd,#$01		; first voice
;
ks44:
	mov.b	a,!add+1+x
	beq	ks46			; if vptr hi != 0
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
	inc	x			; + 2
	asl.b	!keyd			; next voice
	bne	ks44				; for each voice
;************************************************
txh:
	mov	x,#$00			; channel count
	mov.b	!vols,x			; vols reset
	mov	!keyd,#$01		; key data set ; first voice
;................................................
tx00:
	mov.b	!chn,x
	mov.b	a,!add+1+x
	beq	tx60			; kami = 0 (no use channel) ; next if vptr hi zero
;................................................
	dec.b	!ngc+x		; dec duration counter
	bne	tx22			; if not zero, skip to voice readahead
;......
tx10:
	call	data_in			; data in & inc add ; read vcmd into A and Y
	bne	tx15			; block end ?
; vcmd 00 - end repeat/return
	mov.b	a,!ptc+x		; pattern chu ?
	beq	ks20			; read next block if loop has been done
;................................................ 
; repeat / return from subroutine
	call	addset			; pattern start add set ; jump to loop start addr
;......
	dec.b	!ptc+x			; dec repeat count
	bne	tx10				;  if the loop has been done
;......
	mov	a,!adt+x		; add restore (pattern end)
	mov.b	!add+x,a
	mov	a,!adt+1+x
	mov.b	!add+1+x,a	; back to return addr instead
	bra	tx10			; then continue
;................................................
; vcmd branches
tx15:
	bmi	tx16			; d7 = 1 ? 		; vcmds 01-7f - note info:
;
	mov	!ngs+x,a		; Nagasa Store	; set cmd as duration
;......
	call	data_in			; data in & inc add	;read next byte
	bmi	tx16			; d7 = 1 ? ; if note note then
;......
	push	a			; % & vol
	xcn	a			; kami
	and	a,#$07
	mov	y,a
	mov	a,gate+y		; Gate off (%) set
	mov	!ngg+x,a		; set dur% from high nybble
;
	pop	a			; shimo
	and	a,#$0f
	mov	y,a
	mov	a,volt+y
	mov	!vol+x,a		; vol set ; set per-note vol from low nybble
;...................
;	mov	kkk,a			; X 2.5
;	lsr	kkk
;	asl	a
;	adc	a,kkk
;	mov	!ngg+x,a		; Gate off (%) set
;... 
;	call	data_in			; data in & inc add
;	bmi	tx16			; $
;...
;	asl	a			; X 2
;	mov	!vol+x,a		; Gain set
;...................
	call	data_in			; data in & inc add ; read vcmd into A and Y
; vcmd branches 80-ff
tx16:
	cmp	a,#!sno			; special flag ?
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
	call	dss			; freq. data set ; handle note cmd if vbit $1a clear
;...................
tx18:
	mov	a,!ngs+x
	mov.b	!ngc+x,a
	mov	y,a
;
	mov	a,!ngg+x		; gate off (step) set ; set duration counter from duration
	mul	ya
	mov	a,y
	bne	tx19
	inc	a			; a = 1
tx19:
	mov.b	!ngo+x,a	; set actual key-off dur counter
	bra	tx40
;................................................
tx22:
	call	keych			; keyoff & sweep & vib check ; do readahead
;................................................
tx40:
	call	swpch			; sweep check (next data)
;................................................
tx60:
	inc	x			;
	inc	x			;
	asl.b	!keyd			;
	beq	tmpy			; channel end ? (8ch)
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
;					; tmpc = 0 (move end)
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
;					; mvoc = 0 (move end)
	movw	ya,!mvoc		; y <- mvom , a <- 00
+
	movw	!mvow,ya
	mov	!vols,#$ff		; mvo set ; set all vol chg flags
;................................................
mvo40:
	mov	x,#$00			; vol set keyon & end
	mov	!keyd,#$01		; key data set (8ch) ; first voice
;
mvo42:
	mov.b	a,!add+1+x
	beq	mvo46			; kami = 0
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
	mov	a,(!add+x)		; data in
; advance reading ptr
add_inc:
	inc.b	!add+x
	bne	data_inr
	inc.b	!add+1+x		; inc reading ptr
data_inr:
	mov	y,a			; flag set
	ret				; jump to vcmd
;................................................
;
;
;************************************************
;		sound no.
;************************************************
snox:
	;call	data_in			; data in & inc add
;************************************************
;		Sound No. data set
;************************************************
; vcmd e0 - set instrument
snoset:
	mov	!snos+x,a		; sno store
snoset0:
	mov	y,a			; d7 check
	bpl	snoset1			; if percussion note:
;......
	setc
	sbc	a,#$ca			; ca-dd => 00-15
;
	clrc
	adc.b	a,!wavs			; bias add. ; add perc patch base
;......
snoset1:
	mov	y,#$06			; x=channel a=sno
	mul	ya
	movw	!adx,ya
	clrc
	adc	!adx,#!patch_tab		; #low sod
	adc	!adx+1,#!patch_tab>>8		; #high sod
;...
	mov.b	a,!fkin			; kinshi flag check
	and.b	a,!keyd
	bne	snosetr
;...
	push	x
	mov	a,x			; apuch
	xcn	a
	lsr	a
	or	a,#$04			; write address ; voice X SRCN
	mov	x,a
;
	mov	y,#$00			; 1st data in
	mov	a,(!adx)+y		; sound data set
	bpl	snoset4			; noise ?
;......
snoset2:
	and	a,#$1f			; noise clock store ; sample > 80: noise, freq in low bits
	and	!flgs,#$20		; keep echo bit from FLG
	tset	!flgs,a			; noise clock store ;  ; OR in noise freq
;
	or	(!nons),(!keyd)		; noise channel store ; set vbit in noise enable
;
	mov	a,y			; y = 0 (=dd0) ; set SRCN to 0
	bra	snoset8
;......
snoset4:
	mov.b	a,!keyd			; normal sno
	tclr	!nons,a			; noise channel clear ; clear noise vbit
;...................
snoset6:
	mov	a,(!adx)+y		; sound data set ; set SRCN from table
snoset8:
	mov	!apuadd,x		; write address
	mov	!apudt,a		; data write
;
	inc	x
	inc	y
	cmp	y,#$04
	bne	snoset6			; tensou data 4 ; set SRCN, ADSR1/2, GAIN from table
;
	pop	x
	mov	a,(!adx)+y		; 5 ban me (block su)
	mov	!bls+1+x,a		; block su store ; set pitch multiplier
	inc	y
	mov	a,(!adx)+y		; 6 ban me (block su)
	mov	!bls+x,a		; block su store
;
snosetr:
	ret
;************************************************
;		pan data set
;************************************************
panx:
	;call	data_in			; data in & inc add
	mov	!panf+x,a		; pan flag store
;
	and	a,#$1f
	mov	!pand+x,a		; pan data ; voice pan value
	mov	a,#$00
	mov	!pandw+x,a
;
	ret
;************************************************
;		pan move
;************************************************
pamx:
	;call	data_in			; data in & inc add
	mov.b	!panc+x,a		; pan (count)
	push	a			; count --> x
;
	call	data_in			; data in & inc add
	mov	!panm+x,a		; pan (mokuteki)
;......
	setc
	sbc	a,!pand+x		; pan (now data) ; current pan value
	pop	x			; count --> x
;
	call	divx			; x=count a=sa c=+,- ; delta = pan value / steps
;......
	mov	!panadw+x,a		; + shimo
	mov	a,y			; kami
	mov	!panad+x,a		; + kami
	ret
;************************************************
;		vibrate
;************************************************
; vcmd e3 - vibrato on
vibx:
	;call	data_in			; data in & inc add
	mov	!vibhs+x,a		; vib hold
;
	call	data_in			; data in & inc add
	mov	!vibcad+x,a		; vib speed (+@)
;
	call	data_in			; data in & inc add
;************************************************
;		vibrate off
;************************************************
; vcmd e4 - vibrato off
vofx:
	mov.b	!vibd+x,a		; vib depth
	mov	!vibdm+x,a		; vib depth mokuteki
;
	mov	a,#$00
	mov	!vibcs+x,a		; vib change count
	ret
;************************************************
;		vibrate change
;************************************************
; vcmd f0 - vibrato fade
vchx:
	;call	data_in			; data in & inc add
	mov	!vibcs+x,a		; vib change count
	push	a
;
	mov	y,#$00
	mov.b	a,!vibd+x		;
	pop	x
	div	ya,x			; ya/x = a ... y
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
	mov	!ptps+x,a		; key trans. store
+
	ret
;************************************************
;		tremolo
;************************************************
; vcmd eb - tremolo on
trex:
	;call	data_in			; data in & inc add
	mov	!trehs+x,a		; tre hold
;
	call	data_in			; data in & inc add
	mov	!trecad+x,a		; tre speed (+@)
;
	call	data_in			; data in & inc add
;************************************************
;		tremolo off		; a = 0
;************************************************
tofx:
; vcmd ec -ff
	mov.b	!tred+x,a		; tre depth
	ret
;************************************************
;		sweep kurikaeshi
;************************************************
; vcmd f1 - pitch envelope (release)
swkx:
	mov	a,#$01			;
	bra	swsx0
;................................................
; vcmd f2 - pitch envelope (attack)
swsx:
	mov	a,#$00
swsx0:
	mov	!swsk+x,a		; a = 0
;
	;call	data_in			; data in & inc add
	mov	a,y
	mov	!swshc+x,a		; hold
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
	mov	!swsc+x,a		; count
;
	call	data_in			; data in & inc add
	mov	!swss+x,a		; + @
	ret
;................................................
;		sweep off		; a = 0
;................................................
; vcmd f3 - pitch envelope off
sofx:
	mov	!swsc+x,a		;
	mov	!_03e1+x,a
	ret
;************************************************
;		part vol set
;************************************************
; vcmd ed - volume
pv1x:
	;call	data_in			;;0x
	mov	!pvod+x,a		; vol set
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
	push	a			; count --> x
;
	call	data_in			; data in & inc add
	mov	!pvom+x,a		; vol (mokuteki)
;......
	setc
	sbc	a,!pvod+x		; vol (now data)
	pop	x			; count --> x
;
	call	divx			; x=count a=sa c=+,-
;......
	mov	!pvoadw+x,a		; + shimo
	mov	a,y			; kami
	mov	!pvoad+x,a		; + kami
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
	mov	!adp+x,a		; pattern add. (low)
	call	data_in			; data in & inc add
	mov	!adp+1+x,a		; pattern add. (high)
;
	call	data_in			; data in & inc add
	mov.b	!ptc+x,a		; rythm pattern count
;
	mov.b	a,!add+x		; add taihi
	mov	!adt+x,a
	mov.b	a,!add+1+x
	mov	!adt+1+x,a
; jump to $loop destination
addset:
	mov	a,!adp+x		; pattern add. (low)
	mov.b	!add+x,a
	mov	a,!adp+1+x		; pattern add. (high)
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
	;call	!data_in		; data in & inc add
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
	movw	!evolw,ya		; ya = 00
	movw	!evorw,ya		; EVOL "00" set
;
	set5	!flgs			; write disable
	ret
;************************************************
;		echo delay time & feed back
;************************************************
; vcmd f7 - set echo params
edlx:
	;call	data_in			; data in & inc add
	call	esaset			; EDL & ESA set
;
	call	data_in			; data in & inc add
	mov.b	!efbs,a			; EFB = feed back     
;
	call	data_in			; data in & inc add
;
filset:
	mov	y,#$08			; a = fil no.
	mul	ya
	mov	x,a			; table add.
	mov	y,#$0f			; tenso address set
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
	bpl	filset2			; until 07fh
;
	mov.b	x,!chn
	ret
;........................................
; set echo delay to A
esaset:
	mov.b	!eclr,a			; echo delay time
;
	mov	y,#$7d			; EDL = delay time
	mov	!apuadd,y
	mov	a,!apudt
	cmp.b	a,!eclr
	beq	esaset4
;......
	and	a,#$0f
	eor	a,#$ff
	bbc7	!ekin,esaset1		; kinshi chu ?
	clrc
	adc.b	a,!ekin
esaset1:
	mov.b	!ekin,a			; echo kinshi time
;
	mov	y,#$04
esaset2:
	mov	a,dseta-1+y		; EON EFB EVOL EVOR
	mov	!apuadd,a		; write address 
	mov	a,#$00
	mov	!apudt,a		; data write
	dbnz	y,esaset2		;
;
	mov.b	a,!flgs
	or	a,#$20
	mov	y,#$6c			; FLG echo off
	call	apus			; a=data  y=address
;
	mov.b	a,!eclr
	mov	y,#$7d			; EDL = delay time
	call	apus			; a=data  y=address
;......
esaset4:
	asl	a			; ESA set
	asl	a
	asl	a
	eor	a,#$ff
	setc
	adc	a,#$3c			; 0ffh = echo end add.  ** henko **
	mov	y,#$6d			; ESA = echo start add.
	jmp	apus			; a=data  y=address
;************************************************
;		source count
;************************************************
; vcmd fa - set perc patch base
wavx:	mov.b	!wavs,a			;
	ret				;
;************************************************
;		sel dammy
;************************************************
;selx:	call	add_inc			;	!! test !!
;	ret				;	!! test !!
;************************************************
;		sound cut
;************************************************
;cutx:	inc	a			;	!! test !!
;	mov	!cutk+x,a		;	!! test !!
;	ret				;	!! test !!
;************************************************
;		F.F. set
;************************************************
;fftx:		inc	a		;	!! test !!
;************************************************
;               F.F. clear
;************************************************
;plyx:		mov	!ffk,a		; 	 !! test !!
;		jmp	ks04		; keyoff !! test !!
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
	mov	a,(!add+x)		; next data check
	cmp	a,#!swp
	bne	swpadsetr		; not [swp] ?
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
	clrc				; key trans. add.
	adc.b	a,!ktps
	adc	a,!ptps+x
;................................................
; calculate portamento delta
swpadset:
	and	a,#$7f			; $
	mov	!swpm+x,a		; sweep (mokuteki)
;......
	setc				;
	sbc	a,!swpd+x		; moku - now
;
	mov.b	y,!swpc+x		; sweep count
	push	y
	pop	x			; count --> x
;
	call	divx			; x=count a=sa c=+,-
	mov	!swpadw+x,a		; + shimo
	mov	a,y
	mov	!swpad+x,a		; + kami
swpadsetr:
	ret
;........................................
swpdset:
	mov	a,!swpd+x		; kkk sss <-- swpd swpdw
	mov.b	!kkk,a
	mov	a,!swpdw+x		;
	mov.b	!sss,a
	ret
;................................................
;************** div keisan  from tp2 & mv2 & pam & swp (x=count a=sa)
;................................................
; signed 16 bit division
divx:
	notc				; c=1 plus
	ror.b	!ttt			; data store
	bpl	div10			; lpus ?
;......
	eor	a,#$ff			; minus
	inc	a
;......
div10:
	mov	y,#$00			; sa --> 00 sa ( y a )
	div	ya,x			; 00 sa / count --> a ... y
	push	a			; kami
;
	mov	a,#$00
	div	ya,x			; sa 00  / count --> a
	pop	y			; ya data set
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
	dw snox				; e0 - set instrument
	dw panx				; e1 - pan
	dw pamx				; e2 - pan fade
	dw vibx				; e3 - vibrato on
	dw vofx				; e4 - vibrato off
	dw mv1x				; e5 - master volume
	dw mv2x				; e6 - master volume fade
	dw tp1x				; e7 - tempo
	dw tp2x				; e8 - tempo fade
	dw ktpx				; e9 - global transpose
	dw ptpx				; ea - per-voice transpose
	dw trex				; eb - tremolo on
	dw tofx				; ec - tremolo off
	dw pv1x				; ed - volume
	dw pv2x				; ee - volume fade
	dw patx				; ef - call subroutine
	dw vchx				; f0 - vibrato fade
	dw swkx				; f1 - pitch envelope (release)
	dw swsx				; f2 - pitch envelope (attack)
	dw sofx				; f3 - pitch envelope off
	dw tunx				; f4 - tuning
	dw ecvx				; f5 - echo vbits/volume
	dw eofx				; f6 - disable echo
	dw edlx				; f7 - set echo params
	dw ev2x				; f8 - echo volume fade
	dw _B5D				; f9 - pitch slide
	dw wavx				; fa - set perc patch base
	;dw tunx,ecvx,eofx,edlx,ev2x,swpx,wavx
	;dw                                    selx,cutx,fftx,plyx	; !! test !!
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
	mov.b	a,!pvoc+x		; vol move chu ?
	beq	trey
;................................................
mov	a,#$00
mov	y,#$03
dec.b	!pvoc+x
call	_CC1

;************************************************
;               tremolo check                      
;************************************************
trey:
	mov.b	y,!tred+x		; tre chu ?
	beq	tre22
;
	mov	a,!trehs+x		; hold chu ?
	cbne	!trehc+x,tre20
;................................................
	or	(!vols),(!keyd)		; vol set flag  
;......
	mov	a,!trec+x		; trec = a
	bpl	tre02			; trec = 080h ijo ?

	inc	y			; tred = 0ffh ?
	bne	tre02

	mov	a,#$80
	bra	tre04
;......
tre02:
	clrc				; speed keisan
	adc	a,!trecad+x
tre04:
	mov	!trec+x,a		; count data
;
	call	treset			; volx set
	bra	pany
;................................................
tre20:
	inc.b	!trehc+x		; hold chu
;
tre22:
	mov	a,#$ff			; y = depth (tre)
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
	mov	a,!pand+x		; kami
	mov	y,a
	mov	a,!pandw+x		; shimo
	movw	!sss,ya
;................................................
pan20:
	mov	a,x			; kkk sss --> pand set
	xcn	a
	lsr	a			; apuch
	mov.b	!ttt,a			; r.gain = 0
;................................................
pan30:
	mov.b	y,!kkk			; right gain keisan
	mov	a,pant+1+y
	setc
	sbc	a,pant+y		; sa --> a
	mov.b	y,!sss			; shimo
	mul	ya			; sa x 0.???
	mov	a,y			;          --> a
;
	mov.b	y,!kkk			; kami
	clrc
	adc	a,pant+y		; pan data --> a
	mov	y,a
;
	mov	!_0250+x,a
	mov	a,!volx+x		; gain data set
	mul	ya			;
;
	mov	a,!panf+x
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
	mov	y,#20			; left gain keisan
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
	or	(!vols),(!keyd)			; vol set flag 
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
	mov	a,(!adx)+y		; data in
	beq	key16			; block end ?
	bmi	key14
-
	inc	y
	bmi	key20
	mov	a,(!adx)+y
	bpl	-
;
key14:
	cmp	a,#!xxx			; xxx ?
	beq	swpy			; = tai
;
	cmp	a,#!pat
	beq	key18			; pat ?
;
	cmp	a,#!sno
	bcc	key20
;...
	push	y			; special flag
	mov	y,a
	pop	a
	adc	a,spfp-!sno+y		; c=1
	mov	y,a
	bra	key10
;...................
key16:
	mov.b	a,!adx+3		; pattern chu ?
	beq	key20
;......
	dec.b	!adx+3			; pattern end ?
	bne	key17
;
	mov	a,!adt+1+x		; add restore (pattern end)
	push	a
	mov	a,!adt+x
	pop	y
	bra	key04			;
;
key17:
	mov	a,!adp+1+x		; pattern add. (high)
	push	a
	mov	a,!adp+x		; pattern add. (low)
	pop	y
	bra	key04
;......
key18:
	inc	y			; pat
	mov	a,(!adx)+y		; data in
	push	a			; add. low
	inc	y			;
	mov	a,(!adx)+y		; data in
	mov	y,a			; add. high
	pop	a
	bra	key04			;
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
	cbne	!vibhc+x,vib11		; hold chu ?
;................................................
	mov	a,!vibcc+x		;
	cmp	a,!vibcs+x
	bne	vib15			; change chu ?
;...
	mov	a,!vibdm+x		; vib change end !
	bra	vib17
;......
vib15:
	setp				; change chu
	inc.b	!vibcc+x
	clrp
;
	mov	y,a			; !vibcc+x = 0 ?
	beq	vib16			; change begin (a=0)
;
	mov.b	a,!vibd+x		; change chu
vib16:
	clrc
	adc	a,!vibad+x		;
vib17:
	mov.b	!vibd+x,a
;................................................
vib18:
	mov	a,!vibc+x		; vib keisan
	clrc
	adc	a,!vibcad+x
	mov	!vibc+x,a		; count data
;................................................
vib20:
	mov.b	!ttt,a			; depth keisan
;
	asl	a
	asl	a
	bcc	vib21			; count data d6=0 ?
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
	and	a,#$0f			;
	mul	ya
	bra	vib25
;
vib24:
	mul	ya
	mov	a,y			; shosuten ika
	mov	y,#$00			; kami
;
vib25:
	call	minusad			; if ttt(d7)=1 then minus + sss
;......
;	addw	ya,sss			; vib keisan
;	movw	sss,ya			; data set
;................................................
vib40:
	jmp	dssx			; fre. set (call)
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
	mov	a,!trehs+x		; holdchu ?
	cbne	!trehc+x,pnny
;................................................
	call	tresetx
;................................................
;************** pan move check ******************
;................................................
pnny:
	mov	a,!pand+x		; kami
	mov	y,a
	mov	a,!pandw+x		; shimo
	movw	!sss,ya
;...
	mov.b	a,!panc+x		; pan move chu ?
	beq	pnn04
;...
	mov	a,!panad+x
	mov	y,a
	mov	a,!panadw+x		; + @ keisan
;
	call	hokan			; kkk sss <-- data set
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
	beq	vib12			; uuu d7 check & ret
;
	mov	a,!vibhs+x
	cbne	!vibhc+x,vib12		; hold chu ?
;................................................
vbb10:
	mov.b	y,!tmpd
	mov	a,!vibcad+x		; vib keisan
	mul	ya
	mov	a,y
	clrc
	adc	a,!vibc+x		; vib count
	jmp	vib20			; depth keisan & data set
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
	mul	ya			; a = shimo
	mov.b	!adx,y
	mov	!adx+1,#$00
;
	mov.b	y,!tmpd
	pop	a			; a = kami
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
	mov	a,!trecad+x		; tre keisan

	mul	ya
	mov	a,y
	clrc
	adc	a,!trec+x
;................................................
treset:
	asl	a			; volx set
	bcc	treset2
;
	eor	a,#$ff
;
treset2:
	mov	y,!tred+x
	mul	ya			; tre depth x wave
;
	mov	a,y
	eor	a,#$ff			; = 1 - depth
;...................
volxset:
	mov.b	y,!mvo			; main vol x ( 1 - depth )
	mul	ya			;
;
	mov	a,!vol+x		; vol
	mul	ya
;
	mov	a,!pvod+x		; part vol x
	mul	ya
;
	mov	a,y			; dB
	mul	ya			; dB
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
fild:  ;                                        ; Filter    (0xfh)
	db $7f, $00, $00, $00, $00, $00, $00, $00 ; no filter (x1.0)
	db $58, $bf, $db, $f0, $fe, $07, $0c, $0c ; high pass
	db $0c, $21, $2b, $2b, $13, $fe, $f3, $f9 ; low  pass
	db $34, $33, $00, $d9, $e5, $01, $fc, $eb ; band pass
;................................................ 
dseta: ;   EVOL EVOR EFB  EON  FLG                   NOOF PMON
	db $2c, $3c, $0d, $4d, $6c, !keyon, !keyoff, $3d, $2d, !keyoff
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
	db "*Ver S1.20*"				; ** version check **
;************************************************
;
;
;***************************************
;		tensou program
;***************************************
ten00:
	mov	a,#$aa			;
	mov	!port0,a		;
	mov	a,#$bb			;
	mov	!port1,a		;
;........................................
ten02:
	mov	a,!port0			; flag O.K. ?
	cmp	a,#$cc			;
	bne	ten02			;
	bra	ten40			;
;........................................
ten16:
	mov	y,!port0		;
	bne	ten16			;
;........................................
ten20:
	cmp	y,!port0		;
	bne	ten26
;......
	mov	a,!port1
	mov	!port0,y
	mov	(!adx)+y,a		;
;
	inc	y
	bne	ten20			;
;......
	inc.b	!adx+1			;
	bra	ten20			;
;........................................
ten26:
	bpl	ten20			;
;
	cmp	y,!port0		;
	bpl	ten20			;
;........................................
ten40:
	mov	a,!port2		;
	mov	y,!port3		;
	movw	!adx,ya			;
;
	mov	y,!port0		;
	mov	a,!port1		;
	mov	!port0,y		; flag return
	bne	ten16			; port1 = 0 ?
;........................................
	mov	x,#$31			; in port clear
	mov	!cont,x

	ret
;........................................

; 0F21-0FDF.bin
; Table of SFX IDs to trigger alongside the primary SFX ID that was called.
; Multiple SFX IDs can be chained together via this list.
; Zero means don't trigger another one.
sfx_chain_table:
	db $BF ; SFX ID 0 is invalid, do not use
	db      $BF, $BE, $00, $0C, $0C, $00, $00 ; $01 - $07
	db $00, $00, $00, $00, $00, $AF, $B2, $B0 ; $08 - $0F
	db $B1, $00, $AE, $00, $10, $10, $10, $10 ; $10 - $17
	db $00, $AB, $00, $00, $BD, $BC, $B8, $B6 ; $18 - $1F
	db $BA, $00, $00, $00, $00, $00, $00, $00 ; $20 - $27
	db $00, $00, $00, $00, $00, $00, $00, $21 ; $28 - $2F
	db $00, $B9, $00, $00, $00, $00, $00, $00 ; $30 - $37
	db $00, $00, $B3, $00, $00, $00, $00, $00 ; $38 - $3F
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $40 - $47
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $48 - $4F
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $50 - $57
	db $00, $00, $B4, $00, $00, $00, $00, $00 ; $58 - $5F
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $60 - $67
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $68 - $6F
	db $00, $00, $00, $00, $00, $00, $00, $00 ; $70 - $77 
	db $AA, $00, $00, $00, $00, $00, $00, $00 ; $78 - $7F
	db $AD, $00, $AC, $00, $00, $00, $00, $00 ; $80 - $87
	db $00, $00, $00, $00, $A9, $A8, $00, $00 ; $88 - $8F
	db $A7, $00, $00, $00, $00, $00, $00, $00 ; $90 - $97
	db $00, $00, $00, $00, $A6, $A6, $00, $00 ; $98 - $9F
	db $A2, $A3, $A4, $88, $00, $00, $00, $00 ; $A0 - $A7
	db $00, $00, $00, $00, $00, $2A, $00, $00 ; $A8 - $AF
	db $00, $00, $00, $00, $00, $B5, $00, $B7 ; $B0 - $B7
	db $00, $00, $79, $BB, $00, $00, $00      ; $B8 - $BF

; 0FE0-109E.bin
; This table defines the parameters of each SFX ID.
; Each byte consists of two nibbles ($xy).
; $x - voice ID to use.
; $y - SFX priority.
; Higher values have higher priority, and ties overwrite each other.
sfx_parameters:
	db $3F ; SFX ID 0 is invalid, do not use
	db      $3F, $5D, $5C, $5C, $5C, $5A, $5A ; $01 - $07
	db $5A, $5A, $2D, $2D, $25, $3E, $3E, $3A ; $08 - $0F
	db $10, $14, $59, $59, $2D, $2D, $2D, $5C ; $10 - $17
	db $5B, $59, $3E, $3E, $1F, $46, $46, $46 ; $18 - $1F
	db $45, $44, $43, $42, $41, $40, $41, $40 ; $20 - $27
	db $40, $39, $45, $45, $45, $45, $35, $57 ; $28 - $2F
	db $56, $58, $58, $50, $50, $50, $50, $32 ; $30 - $37
	db $45, $45, $45, $42, $41, $40, $42, $42 ; $38 - $3F
	db $42, $41, $40, $42, $42, $42, $41, $40 ; $40 - $47
	db $42, $41, $40, $45, $45, $45, $40, $45 ; $48 - $4F
	db $46, $45, $45, $45, $45, $51, $43, $12 ; $50 - $57
	db $45, $45, $45, $42, $42, $42, $39, $39 ; $58 - $5F
	db $39, $39, $39, $39, $58, $45, $39, $43 ; $60 - $67
	db $43, $43, $43, $43, $31, $41, $41, $46 ; $68 - $6F
	db $46, $46, $46, $43, $43, $43, $43, $43 ; $70 - $77 
	db $1E, $39, $39, $39, $39, $39, $39, $45 ; $78 - $7F
	db $46, $45, $45, $46, $35, $45, $46, $45 ; $80 - $87
	db $54, $30, $45, $39, $45, $46, $32, $45 ; $88 - $8F
	db $39, $45, $42, $42, $46, $46, $32, $45 ; $90 - $97
	db $46, $44, $34, $41, $45, $45, $46, $46 ; $98 - $9F
	db $35, $25, $15, $03, $00, $15, $43, $36 ; $A0 - $A7
	db $35, $0E, $49, $35, $36, $2D, $4E, $44 ; $A8 - $AF
	db $40, $5A, $33, $25, $23, $33, $24, $34 ; $B0 - $B7
	db $36, $34, $25, $35, $0F, $2E, $2F      ; $B8 - $BF


; turn them into db statements and put them in rows of 8
; (excluding the first entry because SFX ID $00 is never valid, meaning the first row 
; should have a blank spot). 

; $109F table begins, 2 bytes, 191 pointers
; Names taken from SFEX 1.08.2
_109f:
	dw _237A	; $01 UNPAUSE
	dw _237A	; $02 PAUSE
	dw _2301	; $03 PLAYER DOWN
	dw _21F9	; $04 PLAYER DAMAGE
	dw _21DE	; $05 LEFT WING DAMAGED
	dw _21D7	; $06 RIGHT WING DAMAGED
	dw _21F2	; $07 LEFT WING CRASH
	dw _21EB	; $08 RIGHT WING CRASH
	dw _236A	; $09 LEFT WING SCRATCH
	dw _2372	; $0A RIGHT WING SCRATCH
	dw _2110	; $0B INCOMING ENEMY
	dw _213C	; $0C WING DAMAGED
	dw _13F8	; $0D COME IN CORNERIA
	dw _1B32	; $0E BONUS CREDIT
	dw _2024	; $0F BIG SUPPORT RING
	dw _207D	; $10 1UP RING HIT
	dw _205D	; $11 CONTROLS SELECT
	dw _2015	; $12 PERCENTAGE RING
	dw _1B05	; $13 JINGLE + GOOD LUCK
	dw _1A66	; $14 CONE HIT
	dw _1A46	; $15 TWIN BLASTER POWERUP
	dw _1A4F	; $16 SHIELD POWERUP
	dw _1A58	; $17 WING REPAIRED POWERUP
	dw _169C	; $18 BOMB POWERUP
	dw _21E5	; $19 WINGLESS ARWING COLLISION
	dw _1650	; $1A SLOT MACHINE COIN
	dw _1633	; $1B SMALL ARWING DAMAGE ALARM
	dw _15FE	; $1C BIG ARWING DAMAGE ALARM
	dw _1F73	; $1D SMALL BOSS EXPLOSION
	dw _223B	; $1E NEAR BIG BOSS EXPLOSION
	dw _227D	; $1F MID BIG BOSS EXPLOSION
	dw _22BF	; $20 FAR BIG BOSS EXPLOSION
	dw _2220	; $21 NEAR ENEMY EXPLOSION
	dw _222F	; $22 MID ENEMY EXPLOSION
	dw _2235	; $23 FAR ENEMY EXPLOSION
	dw _2202	; $24 NEAR ENEMY HIT
	dw _220C	; $25 MID ENEMY HIT
	dw _2216	; $26 FAR ENEMY HIT
	dw _2358	; $27 NEAR LASER DEFLECT
	dw _235E	; $28 MID LASER DEFLECT
	dw _2364	; $29 FAR LASER DEFLECT
	dw _1DBE	; $2A GOOD LUCK -BGM 4-
	dw _1EF2	; $2B ENEMY WARP-IN
	dw _1E9B	; $2C CONE TRIANGLE
	dw _1EC3	; $2D 1-6+2-3 BOSS SHADOWING
	dw _1F9B	; $2E PLASMA HYDRA ARM HIT
	dw _1F06	; $2F ROCK CRUSHER ROLL
	dw _232A	; $30 PLAYER BOMB EXPLOSION
	dw _2341	; $31 PLAYER BOMB SHOT
	dw _20F2	; $32 PLAYER BOOST
	dw _2106	; $33 PLAYER BRAKE
	dw _15F6	; $34 PLAYER TWIN BLASTER SHOT
	dw _2324	; $35 PLAYER LASER SHOT
	dw _234A	; $36 DUAL BEAM SHOT
	dw _234A	; $37 DUAL BEAM SHOT
	dw _2001	; $38 BIRD SCREAM
	dw _1FAF	; $39 ENEMY ROCKET PLAYER HIT
	dw _1FF9	; $3A DODORA EGG CRACK + BIRD
	dw _1FC3	; $3B DODORA HIT
	dw _218D	; $3C NEAR ENEMY ROCKET SHOT
	dw _2193	; $3D MID ENEMY ROCKET SHOT
	dw _2199	; $3E FAR ENEMY ROCKET SHOT
	dw _20A5	; $3F LEFT GATE MOVING
	dw _20B5	; $40 CENTRE GATE MOVING
	dw _20C4	; $41 RIGHT GATE MOVING
	dw _20D4	; $42 MID GATE MOVING
	dw _20E3	; $43 FAR GATE MOVING
	dw _21B7	; $44 LEFT ENEMY LASER SHOT
	dw _21BE	; $45 CENTRE ENEMY LASER SHOT
	dw _21C4	; $46 RIGHT ENEMY LASER SHOT
	dw _21CB	; $47 MID ENEMY LASER SHOT
	dw _21D1	; $48 FAR ENEMY LASER SHOT
	dw _219F	; $49 NEAR ENEMY BATTERY
	dw _21A7	; $4A MID ENEMY BATTERY
	dw _21AF	; $4B FAR ENEMY BATTERY
	dw _216D	; $4C PHATRON 2 LANDING
	dw _217D	; $4D PHATRON 2 JUMP
	dw _1E7A	; $4E -UNUSED-
	dw _1E21	; $4F DANCING INSECTOR PROPELLING
	dw _1E82	; $50 BLADE BARRIER POST-DRILL ATK
	dw _1E66	; $51 BLADE BARRIER PLAYER WEB HIT
	dw _1B8A	; $52 LAST BASE ENTRY 2.DOOR CLOSE
	dw _1B9E	; $53 FAR LAST BASE ENTRY 2.DOOR CLOSE
	dw _1BB2	; $54 LAST BASE ENTRY 2.DOOR OPEN
	dw _1BC6	; $55 FAR LAST BASE ENTRY 2.DOOR OPEN
	dw _1B84	; $56 PLAYER AMOEBA HIT
	dw _168B	; $57 BLOCKADE DIRECTION CHANGE
	dw _1C4A	; $58 HOVERING -UNUSED-
	dw _1C96	; $59 DOOR CLOSE
	dw _1C87	; $5A DOOR OPEN
	dw _1CA5	; $5B HOVERING -UNUSED-
	dw _1D49	; $5C NEAR ENEMY RING SHOT
	dw _1D5D	; $5D MID ENEMY RING SHOT
	dw _1D71	; $5E FAR ENEMY RING SHOT
	dw _12A3	; $5F PEPPER RADIO CHAT
	dw _1D85	; $60 FOX RADIO CHAT
	dw _1886	; $61 FALCO RADIO CHAT
	dw _1920	; $62 PEPPY RADIO CHAT
	dw _19A1	; $63 SLIPPY RADIO CHAT
	dw _1DA4	; $64 RADIO CHAT QUIT
	dw _1DB8	; $65 PLAYER CAMERA CHANGE
	dw _15AE	; $66 DESTRUCTOR WEAPON HEAD ATTACK
	dw _1DC7	; $67 CONTINUE LET'S GO -BGM 30-
	dw _1DCD	; $68 LEFT WATER SPLASH OUT
	dw _1DD8	; $69 CENTRE WATER SPLASH
	dw _1DE2	; $6A RIGHT WATER SPLASH
	dw _1DED	; $6B MID WATER SPLASH
	dw _1DF7	; $6C FAR WATER SPLASH
	dw _1E56	; $6D LEFT OBJECT FLY-BY
	dw _1E47	; $6E CENTRE OBJECT FLY-BY
	dw _1E37	; $6F RIGHT OBJECT FLY-BY
	dw _1C37	; $70 ATOMIC BASE POWER SUPPLY OFF
	dw _1C24	; $71 ATOMIC BASE POWER SUPPLY ON
	dw _1BDA	; $72 ATOMIC BASE CORE CLOSE
	dw _1BFF	; $73 ATOMIC BASE CORE OPEN
	dw _1E01	; $74 LEFT WATER SPLASH IN
	dw _1E08	; $75 CENTRE WATER SPLASH IN
	dw _1E0E	; $76 RIGHT WATER SPLASH IN
	dw _1E15	; $77 MID WATER SPLASH IN
	dw _1E1B	; $78 FAR WATER SPLASH IN
	dw _1A71	; $79 BACKGROUND THUNDER
	dw _18AA	; $7A FALCO RADIO CHAT HIT
	dw _18DD	; $7B FALCO RADIO CHAT DOWN
	dw _1944	; $7C PEPPY RADIO CHAT HIT
	dw _196B	; $7D PEPPY RADIO CHAT DOWN
	dw _19DB	; $7E SLIPPY RADIO CHAT HIT
	dw _1A06	; $7F SLIPPY RADIO CHAT DOWN
	dw _1817	; $80 PHANTRON 2 HIT
	dw _182E	; $81 PHANTRON 2 SCREAM
	dw _17DF	; $82 ROCK CRUSHER APPEARS
	dw _1715	; $83 DESTRUCTOR ENGINE
	dw _16D9	; $84 PHANTRON APPEARS
	dw _16C3	; $85 ROCK CRUSHER UNCOVER
	dw _15A2	; $86 PILON TO GROUND
	dw _1594	; $87 ANDROSS APPEARS
	dw _157F	; $88 ANDROSS HIT
	dw _1579	; $89 TEXT TING
	dw _1578	; $8A SILENCE -BGM 10-
	dw _14FD	; $8B PRE-WING REPAIRED
	dw _12C5	; $8C ANDROSS RADIO CHAT
	dw _14AB	; $8D METAL SMASHER SMASHING
	dw _1495	; $8E METAL SMASHER CLOSE
	dw _1481	; $8F BLADE BARRIER WEB ATTACK
	dw _1391	; $90 BONUS RING BIRD
	dw _1364	; $91 COMET FLY-BY
	dw _132E	; $92 WHALE SCREAM
	dw _1324	; $93 STINGRAY HIT
	dw _131A	; $94 SQUID HIT
	dw _1ABF	; $95 SPINNING CORE BG THUNDER
	dw _182E	; $96 PHANTRON 2 SCREAM
	dw _130F	; $97 DANCING INSECTOR MOVEMENT
	dw _12F1	; $98 DANCING INSECTOR FIRE SHOT
	dw _12E7	; $99 DANCING INSECTOR FIRE FLY-BY
	dw _1A92	; $9A VOLCANO FIRE
	dw _1F30	; $9B SLOT MACHINE HANDLE DOWN
	dw _129D	; $9C SLOT MACHINE SLOT SPINNING
	dw _126B	; $9D PROFESSOR HANGER APPEARS
	dw _1287	; $9E PROFESSOR HANGER DISAPPEARS
	dw _1249	; $9F FINAL SCORE SCREEN FLIGHT
	dw _122F	; $A0 ANDROSS SHELL EXPLOSION
	dw _121D	; $A1 ANDROSS SCREAM 4
	dw _1223	; $A2 ANDROSS SCREAM 3
	dw _1228	; $A3 ANDROSS SCREAM 2
	dw _12D2	; $A4 ANDROSS SCREAM 1
	dw _1578	; $A5 SILENCE -BGM 10-
	dw _1281	; $A6 ENEMY HOVERING -unused-
	dw _1379	; $A7 SHOOTING STAR -unused-
	dw _149E	; $A8 OBJECT IMPACT -unused-
	dw _14D4	; $A9 ENEMY -unused-
	dw _1ABC	; $AA BACKGROUND THUNDER -unused-
	dw _166C	; $AB BONUS -unused-
	dw _1779	; $AC DESTRUCTOR ENGINE -unused-
	dw _182B	; $AD PHANTRON SCREAM
	dw _1B1A	; $AE GOOD LUCK -BGM 4- -unused-
	dw _1B5B	; $AF BONUS -unused-
	dw _208F	; $B0 UNKNOWN
	dw _206B	; $B1 UNKNOWN
	dw _203F	; $B2 UNKNOWN
	dw _1FDC	; $B3 UNKNOWN
	dw _1CF7	; $B4 UNKNOWN
	dw _22D1	; $B5 UNKNOWN
	dw _22E9	; $B6 UNKNOWN
	dw _228F	; $B7 UNKNOWN
	dw _22A7	; $B8 UNKNOWN
	dw _20FC	; $B9 UNKNOWN
	dw _2226	; $BA UNKNOWN
	dw _2265	; $BB UNKNOWN
	dw _224D	; $BC UNKNOWN
	dw _1F4B	; $BD UNKNOWN
	dw _230A	; $BE PLAYER DOWN
	dw _2383	; $BF PAUSE

; make sure pointer table isn't too big
assert pc() <= $121d, "Used too much space"

; ===========================
; begin sound effect patterns
; ===========================

_121D: ; Andross scream 4
	db $E0, $2D, $7F, $7D, $64, $81

_1223: ; Andross scream 3
	db $E0, $2E, $60, $7D, $81

_1228: ; Andross scream 2
	db $E0, $2D, $5F, $64, $7D, $82, $00

_122F: ; Andross shell explosion
	db $E0, $06, $0C, $64, $C3, $BB, $0E, $A9
	db $B2, $0F, $BC, $B3, $10, $AB, $AD, $12
	db $9D, $A6, $A3, $13, $95, $90, $94, $60
	db $90, $00

_1249: ; Final score screen flight
	db $E0, $03, $5F, $3C, $F9, $97, $00, $5F
	db $9A, $7F, $46, $F1, $00, $7F, $9D, $7F
	db $50, $F1, $00, $7F, $9F, $7F, $50, $F1
	db $00, $7F, $A0, $7F, $5F, $F1, $00, $7D
	db $A1, $00

_126B:
	db $E0, $2F, $3F, $64, $F9, $BB, $00, $3F
	db $BB, $2F, $6E, $F1, $00, $2F, $B7, $2F
	db $7D, $F1, $00, $2D, $B7, $00

_1281: ; Enemy hovering -unused-
	db $E0, $08, $7F, $64, $A3, $00

_1287:
	db $E0, $2F, $3F, $64, $F9, $BB, $00, $3F
	db $BB, $2F, $3C, $F1, $00, $2F, $BE, $2F
	db $1E, $F1, $00, $2D, $BE, $00

_129D:
	db $E0, $0D, $06, $14, $A6, $00

_12A3: ; Pepper radio chat
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $2C, $18
	db $7D, $85, $24, $87, $E0, $13, $24, $6E
	db $84, $E0, $16, $26, $F9, $84, $00, $24
	db $80, $00

_12C5: ; Andross radio chat
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98

_12D2: ; Andross scream 1
	db $E0, $35, $22, $7D, $91, $16, $64, $91
	db $14, $50, $91, $12, $3C, $90, $12, $28
	db $8F, $12, $14, $8E, $00

_12E7:
	db $E0, $05, $7F, $78, $F9, $91, $00, $7C
	db $98, $00

_12F1:
	db $E0, $1C, $7F, $78, $F9, $97, $00, $7F
	db $8C, $7F, $F1, $00, $7F, $8C, $7F, $F1
	db $00, $7F, $8C, $7F, $F1, $00, $7F, $8C
	db $7F, $F1, $00, $7D, $8C, $00

_130F:
	db $E0, $0A, $06, $28, $A2, $E0, $05, $08
	db $46, $A9, $00

_131A: ; Squid hit
	db $E0, $23, $12, $46, $F9, $A8, $00, $10
	db $B0, $00

_1324: ; Stingray hit
	db $E0, $2D, $18, $5A, $F9, $A8, $00, $16
	db $BC, $00

_132E: ; Whale scream
	db $E0, $24, $0C, $0A, $F9, $BC, $00, $0C
	db $BE, $06, $F1, $00, $04, $C0, $30, $0A
	db $F9, $C0, $00, $30, $BE, $60, $F1, $00
	db $5E, $BC, $24, $00, $A4, $0C, $14, $F9
	db $BC, $00, $0C, $BE, $06, $F1, $00, $04
	db $C0, $30, $14, $F9, $C0, $00, $30, $BE
	db $60, $F1, $00, $5E, $BC, $00

_1364: ; Comet fly-by
	db $E0, $10, $1C, $00, $0A, $C7, $C5, $0F
	db $05, $14, $C2, $C0, $0A, $1E, $C1, $BF
	db $60, $28, $00, $BD, $00

_1379: ; Shooting star -unused-
	db $E0, $10, $08, $00, $A4, $1C, $0A, $C6
	db $C4, $0F, $14, $C1, $BF, $0A, $1E, $C0
	db $BE, $18, $14, $BC, $30, $0A, $BC, $00

_1391: ; Bonus ring bird
	db $E0, $01, $0C, $0A, $F9, $BE, $00, $0C
	db $B4, $06, $F1, $00, $04, $C3, $0C, $14
	db $F9, $BE, $00, $0C, $B4, $06, $F1, $00
	db $04, $C3, $18, $00, $BE, $0C, $1E, $F9
	db $BE, $00, $0A, $C5, $18, $00, $BE, $0C
	db $F9, $BE, $00, $0C, $BB, $06, $F1, $00
	db $04, $C3, $18, $00, $BE, $0C, $28, $F9
	db $BE, $00, $0A, $C5, $0C, $F9, $BE, $00
	db $0C, $BB, $06, $F1, $00, $04, $C3, $18
	db $00, $BE, $0C, $32, $F9, $BE, $00, $0A
	db $C5, $0C, $F9, $BE, $00, $0C, $BB, $06
	db $F1, $00, $04, $C3, $18, $00, $BE, $0C
	db $3C, $F9, $BE, $00, $0A, $C5, $00

_13F8: ; Come in Corneria -BGM 34-
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $37, $6F
	db $87, $E0, $15, $48, $7D, $98, $E0, $0F
	db $06, $14, $BE, $06, $00, $BE, $06, $14
	db $BE, $06, $00, $BE, $E0, $15, $18, $7D
	db $98, $E0, $38, $7F, $F9, $8A, $00, $7F
	db $8A, $1F, $F1, $00, $1D, $8A, $E0, $39
	db $7F, $8A, $E0, $3A, $7F, $F9, $8A, $00
	db $7F, $8A, $7F, $F1, $00, $7F, $8A, $2F
	db $F1, $00, $2D, $8A, $E0, $15, $18, $7D
	db $98, $E0, $0F, $06, $14, $BE, $06, $00
	db $BE, $06, $14, $BE, $06, $00, $BE, $E0
	db $15, $18, $7D, $98, $E0, $3B, $4F, $87
	db $E0, $3C, $7F, $F9, $87, $00, $7F, $87
	db $40, $F1, $00, $3E, $87, $E0, $0F, $06
	db $14, $BE, $06, $00, $BE, $06, $14, $BE
	db $06, $00, $BE, $E0, $15, $48, $7D, $98
	db $00

_1481: ; Blade barrier web attack
	db $E0, $1A, $0C, $64, $A3, $E0, $23, $60
	db $32, $F9, $8B, $00, $60, $A3, $60, $F1
	db $00, $5E, $AF, $00

_1495: ; Metal smasher close
	db $E0, $1A, $0C, $7D, $A3, $30, $7D, $A3
	db $00

_149E: ; Object impact -unused-
	db $E0, $0D, $0C, $78, $89, $24, $78, $F9
	db $89, $00, $22, $82, $00

_14AB: ; Metal smasher smashing
	db $E0, $36, $24, $50, $00, $F9, $A3, $00
	db $18, $9C, $24, $F9, $A3, $00, $18, $9C
	db $24, $64, $28, $F9, $A3, $00, $18, $9C
	db $24, $6E, $3C, $F9, $A3, $00, $18, $9C
	db $24, $7D, $50, $F9, $A3, $00, $18, $9C
	db $00

_14D4: ; Enemy -unused-
	db $E0, $36, $24, $00, $50, $F9, $A2, $00
	db $18, $9B, $24, $F9, $A2, $00, $18, $99
	db $24, $28, $64, $F9, $A2, $00, $18, $9B
	db $24, $3C, $6E, $F9, $A2, $00, $18, $9B
	db $24, $50, $7D, $F9, $A2, $00, $18, $9B
	db $00

_14FD: ; Pre-wing repaired
	db $E0, $18, $12, $0A, $F9, $AB, $00, $12
	db $BE, $0C, $F1, $00, $0A, $AD, $12, $14
	db $F9, $AB, $00, $12, $BE, $0C, $F1, $00
	db $0A, $AD, $12, $28, $F9, $AB, $00, $12
	db $BE, $0C, $F1, $00, $0A, $AD, $12, $3C
	db $F9, $AB, $00, $12, $BE, $0C, $F1, $00
	db $0A, $AD, $12, $50, $F9, $AB, $00, $12
	db $BE, $0C, $F1, $00, $0A, $A9, $12, $50
	db $F9, $AB, $00, $12, $BE, $0C, $F1, $00
	db $0A, $A9, $12, $3C, $F9, $AB, $00, $12
	db $BE, $0C, $F1, $00, $0A, $A9, $12, $28
	db $F9, $AB, $00, $12, $BE, $0C, $F1, $00
	db $0A, $A9, $12, $14, $F9, $AB, $00, $12
	db $BE, $0C, $F1, $00, $0A, $A9, $12, $0A
	db $F9, $AB, $00, $12, $BE, $0C, $F1, $00
	db $0A, $A9, $00

_1578: ; Silence -BGM 10-
	db $00

_1579: ; Text ting
	db $E0, $26, $06, $32, $BB, $00

_157F: ; Andross hit
	db $E0, $35, $12, $7D, $9A, $12, $64, $9A
	db $12, $50, $9A, $12, $3C, $9A, $12, $28
	db $9A, $12, $14, $9A, $00

_1594: ; Andross appears
	db $E0, $05, $0C, $00, $A4, $08, $28, $B9
	db $E0, $10, $48, $46, $C7, $00

_15A2: ; Pilon to ground
	db $E0, $0D, $06, $78, $89, $04, $00, $89
	db $18, $78, $89, $00

_15AE: ; Destructor weapon head attack
	db $E0, $1C, $20, $3C, $F9, $A4, $00, $14
	db $AB, $1C, $50, $F9, $A4, $00, $10, $AB
	db $1A, $64, $F9, $A4, $00, $0E, $AB, $18
	db $78, $F9, $A4, $00, $0C, $AB, $18, $F9
	db $A4, $00, $0C, $AB, $18, $64, $F9, $A4
	db $00, $0C, $AB, $18, $5A, $F9, $A4, $00
	db $0C, $AB, $18, $46, $F9, $A4, $00, $0C
	db $AB, $18, $28, $F9, $A4, $00, $0C, $AB
	db $18, $14, $F9, $A4, $00, $0C, $AB, $00

_15F6: ; Player twin blaster shot
	db $E0, $20, $0C, $3C, $94, $48, $94, $00

_15FE: ; Big arwing damage alarm
	db $E0, $26, $06, $46, $14, $F9, $B4, $00
	db $06, $B7, $0C, $F1, $B5, $00, $06, $B7
	db $06, $F9, $B4, $00, $06, $B7, $0C, $F1
	db $B5, $00, $06, $B7, $06, $F9, $B4, $00
	db $06, $B7, $0C, $F1, $B5, $00, $06, $B7
	db $06, $F9, $B4, $00, $06, $B7, $0C, $F1
	db $B5, $00, $06, $B7, $00

_1633: ; Small arwing damage alarm
	db $E0, $26, $0A, $3C, $14, $F9, $B0, $00
	db $0A, $B4, $10, $F1, $B2, $00, $0A, $B4
	db $0A, $F9, $B0, $00, $0A, $B4, $10, $F1
	db $B2, $00, $0A, $B4, $00

_1650: ; Slot machine coin
	db $E0, $01, $08, $32, $B0, $B2, $08, $28
	db $B5, $0C, $1E, $B7, $08, $32, $BC, $BE
	db $08, $28, $C1, $18, $14, $08, $C3, $30
	db $08, $00, $C3, $00

_166C: ; Bonus -unused-
	db $E0, $01, $0C, $00, $A4, $08, $32, $B0
	db $B2, $08, $28, $B5, $0C, $1E, $B7, $08
	db $32, $BC, $BE, $08, $28, $C1, $18, $14
	db $08, $C3, $30, $08, $00, $C3, $00

_168B: ; Blockade direction change
	db $E0, $05, $06, $78, $9E, $E0, $00, $08
	db $28, $F9, $A4, $00, $06, $A6, $18, $A6
	db $00

_169C: ; Bomb powerup
	db $E0, $00, $24, $00, $A4, $08, $0A, $28
	db $F9, $B7, $00, $06, $B9, $08, $00, $B9
	db $08, $0A, $28, $F9, $B7, $00, $06, $B9
	db $08, $00, $B9, $08, $0A, $28, $F9, $B7
	db $00, $06, $B9, $08, $00, $B9, $00

_16C3: ; Rock crusher uncover
	db $E0, $32, $7F, $7D, $F9, $A3, $00, $7F
	db $A3, $1F, $64, $F1, $00, $1F, $A3, $1F
	db $50, $F1, $00, $1D, $A3, $00

_16D9: ; Phantron appears
	db $E0, $31, $5F, $64, $00, $F9, $A3, $00
	db $5F, $A3, $5F, $64, $1E, $F1, $00, $5F
	db $A3, $5F, $73, $46, $F1, $00, $5F, $A3
	db $5F, $7D, $64, $F1, $00, $5F, $A3, $5F
	db $50, $69, $F1, $00, $5F, $A3, $5F, $32
	db $50, $F1, $00, $5F, $A3, $5F, $1E, $32
	db $F1, $00, $5F, $A3, $5F, $0A, $1E, $F1
	db $00, $5D, $A3, $00

_1715: ; Destructor engine
	db $E0, $30, $7F, $00, $64, $F9, $A3, $00
	db $7F, $A3, $7F, $F1, $00, $7F, $A3, $7F
	db $0A, $73, $F1, $00, $7F, $A3, $7F, $1E
	db $7D, $F1, $00, $7F, $A3, $7F, $32, $7D
	db $F1, $00, $7F, $A3, $7F, $46, $7D, $F1
	db $00, $7F, $A3, $7F, $5A, $7D, $F1, $00
	db $7F, $A3, $7F, $5A, $7D, $F1, $00, $7F
	db $A3, $7F, $50, $69, $F1, $00, $7F, $A3
	db $7F, $46, $5A, $F1, $00, $7F, $A3, $7F
	db $3C, $50, $F1, $00, $7F, $A3, $7F, $28
	db $41, $F1, $00, $7F, $A3, $7F, $1E, $32
	db $F1, $00, $7F, $A3, $7F, $14, $1E, $F1
	db $00, $7D, $A3, $00

_1779: ; Destructor engine -unused-
	db $E0, $30, $7F, $00, $64, $F9, $A1, $00
	db $7F, $A1, $7F, $0A, $64, $F1, $00, $7F
	db $A1, $7F, $1E, $73, $F1, $00, $7F, $A1
	db $7F, $32, $7D, $F1, $00, $7F, $A1, $7F
	db $46, $7D, $F1, $00, $7F, $A1, $7F, $5A
	db $7D, $F1, $00, $7F, $A1, $7F, $5A, $7D
	db $F1, $00, $7F, $A1, $7F, $7D, $5A, $F1
	db $00, $7F, $A1, $7F, $69, $50, $F1, $00
	db $7F, $A1, $7F, $5A, $46, $F1, $00, $7F
	db $A1, $7F, $50, $3C, $F1, $00, $7F, $A1
	db $7F, $41, $28, $F1, $00, $7F, $A1, $7F
	db $32, $1E, $F1, $00, $7F, $A1, $7F, $1E
	db $14, $F1, $00, $7D, $A1, $00

_17DF: ; Rock crusher appears
	db $E0, $2F, $5F, $64, $00, $F9, $A3, $00
	db $5F, $A3, $5F, $64, $1E, $F1, $00, $5F
	db $A3, $5F, $73, $46, $F1, $00, $5F, $A3
	db $5F, $7D, $64, $F1, $00, $5F, $A3, $5F
	db $69, $F1, $00, $5F, $A3, $5F, $50, $F1
	db $00, $5F, $A3, $5F, $32, $F1, $00, $5F
	db $A3, $5F, $1E, $F1, $00, $5D, $A3, $00

_1817: ; Phantron 2 hit
	db $E0, $12, $0C, $78, $F9, $98, $00, $0C
	db $97, $0C, $F1, $00, $0C, $A2, $18, $F1
	db $00, $15, $9F, $00

_182B: ; Phantron scream -unused-
	db $10, $00, $98

_182E: ; Phantron 2 scream
	db $E0, $12, $12, $78, $F9, $98, $00, $12
	db $97, $18, $F1, $00, $18, $A7, $16, $F1
	db $00, $13, $9F, $0F, $64, $F9, $95, $00
	db $0F, $93, $15, $F1, $00, $15, $A3, $14
	db $F1, $00, $11, $9C, $0C, $50, $F9, $91
	db $00, $0C, $90, $12, $F1, $00, $12, $A0
	db $12, $F1, $00, $0F, $98, $09, $3C, $F9
	db $8E, $00, $09, $8C, $0F, $F1, $00, $0F
	db $9C, $0F, $F1, $00, $0C, $95, $06, $28
	db $F9, $8B, $00, $06, $89, $0C, $F1, $00
	db $0C, $98, $1E, $F1, $00, $1B, $91, $00

_1886: ; Falco radio chat
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $16, $10
	db $7D, $80, $82, $E0, $14, $12, $7D, $84
	db $18, $7D, $85, $E0, $16, $24, $7D, $85
	db $18, $00, $85, $00

_18AA: ; Falco radio chat hit
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $16, $18
	db $7D, $85, $0C, $82, $E0, $2E, $0C, $7D
	db $84, $82, $82, $E0, $16, $08, $7D, $82
	db $0C, $00, $89, $E0, $16, $08, $7D, $82
	db $E0, $2C, $18, $7D, $87, $E0, $14, $18
	db $7D, $85, $00

_18DD: ; Falco radio chat down
	db $E0, $0F, $06, $14, $C0, $04, $00, $C0
	db $12, $14, $C0, $06, $00, $C0, $06, $14
	db $C0, $04, $00, $C0, $12, $14, $C0, $06
	db $00, $C0, $E0, $15, $18, $7D, $98, $E0
	db $2D, $24, $7D, $85, $0C, $84, $0C, $00
	db $85, $E0, $16, $0C, $7D, $89, $87, $85
	db $0C, $00, $89, $E0, $16, $08, $7D, $85
	db $E0, $14, $18, $7D, $85, $E0, $16, $18
	db $7D, $84, $00

_1920: ; Peppy radio chat
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $14, $0C
	db $7D, $9C, $95, $24, $7D, $9C, $E0, $1F
	db $0C, $7D, $93, $18, $7D, $91, $E0, $14
	db $24, $7D, $97, $00

_1944: ; Peppy radio chat hit
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $2C, $0C
	db $7D, $9C, $E0, $14, $24, $7D, $9A, $E0
	db $2E, $08, $7D, $93, $97, $18, $7D, $91
	db $E0, $16, $0C, $7D, $97, $97, $00

_196B: ; Peppy radio chat down
	db $E0, $0F, $06, $14, $C0, $04, $00, $C0
	db $12, $14, $C0, $06, $00, $C0, $06, $14
	db $C0, $04, $00, $C0, $12, $14, $C0, $06
	db $00, $C0, $E0, $15, $18, $7D, $98, $E0
	db $2D, $18, $7D, $A1, $9A, $10, $00, $A1
	db $E0, $2E, $0C, $7D, $98, $0C, $7D, $97
	db $E0, $1F, $24, $7D, $95, $00

_19A1: ; Slippy radio chat
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $16, $08
	db $7D, $89, $E0, $14, $08, $85, $E0, $16
	db $08, $89, $E0, $14, $08, $8C, $E0, $16
	db $12, $00, $87, $E0, $16, $08, $7D, $89
	db $E0, $14, $08, $85, $E0, $16, $08, $89
	db $E0, $14, $08, $8C, $E0, $16, $18, $00
	db $87, $00

_19DB: ; Slippy radio chat hit
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $14, $06
	db $7D, $8C, $90, $06, $8C, $90, $0C, $00
	db $85, $E0, $16, $06, $7D, $89, $8C, $89
	db $8C, $12, $00, $85, $06, $7D, $87, $8B
	db $87, $8B, $00

_1A06: ; Slippy radio chat down
	db $E0, $0F, $06, $14, $C0, $04, $00, $C0
	db $12, $14, $C0, $06, $00, $C0, $06, $14
	db $C0, $04, $00, $C0, $12, $14, $C0, $06
	db $00, $C0, $E0, $15, $18, $7D, $98, $E0
	db $16, $08, $7D, $85, $89, $85, $89, $12
	db $00, $85, $0A, $7D, $84, $87, $0B, $84
	db $87, $12, $00, $85, $E0, $16, $0E, $7D
	db $82, $85, $E0, $2D, $30, $7D, $89, $00

_1A46: ; Twin blaster powerup
	db $E0, $1F, $18, $00, $8C, $7F, $7D, $8C
	db $00

_1A4F: ; Shield powerup
	db $E0, $2C, $18, $00, $8C, $30, $7D, $8C
	db $00

_1A58: ; Wing repaired powerup
	db $E0, $14, $18, $00, $8C, $1C, $7D, $8B
	db $E0, $2E, $60, $7D, $8C, $00

_1A66: ; Cone hit
	db $E0, $05, $06, $5A, $B0, $E0, $0A, $24
	db $25, $84, $00

_1A71: ; Background thunder -unused-
	db $7F, $00, $90, $E0, $04, $7F, $7D, $F9
	db $8C, $00, $7F, $90, $7F, $F1, $00, $7F
	db $8E, $3F, $F1, $00, $3D, $8E, $7F, $F9
	db $8B, $00, $7F, $8D, $7F, $F1, $00, $7D
	db $8C

_1A92:
	db $E0, $04, $3F, $7D, $F9, $8B, $00, $3F
	db $8D, $7F, $F1, $00, $7D, $8C, $7F, $F9
	db $8B, $00, $7F, $8D, $7F, $F1, $00, $7D
	db $8C, $7F, $F9, $87, $00, $7F, $8A, $7F
	db $F1, $00, $7F, $89, $7F, $F1, $00, $7D
	db $89, $00

_1ABC: ; Background thunder -unused-
	db $7F, $00, $8D

_1ABF: ; Spinning core bg thunder
	db $E0, $0C, $7F, $78, $F9, $8C, $00, $7F
	db $8D, $7F, $F1, $00, $7F, $8C, $7F, $F1
	db $00, $7F, $8C, $7F, $F1, $00, $7F, $8C
	db $7F, $F1, $00, $7F, $8C, $7F, $F1, $00
	db $7F, $8C, $7F, $F1, $00, $7F, $8C, $7F
	db $F1, $00, $7F, $8C, $7F, $F1, $00, $7F
	db $8C, $7F, $F1, $00, $7F, $8C, $7F, $F1
	db $00, $7F, $8C, $7F, $F1, $00, $7F, $8C
	db $7F, $F1, $00, $7D, $8C, $00

_1B05: ; Jingle + good luck -bgm 4-
	db $E0, $01, $08, $1E, $B7, $B9, $08, $14
	db $B9, $0C, $14, $B9, $0C, $0A, $08, $B9
	db $18, $08, $00, $B9, $00

_1B1A: ; Good luck -bgm 4- -unused-
	db $E0, $01, $0C, $00, $A4, $08, $1E, $B7
	db $B9, $08, $14, $B9, $0C, $14, $B9, $0C
	db $0A, $08, $B9, $18, $08, $00, $B9, $00

_1B32: ; Bonus credit
	db $E0, $01, $08, $32, $B2, $B4, $B7, $BC
	db $08, $2E, $1C, $BE, $18, $C0, $08, $24
	db $16, $B2, $B4, $B7, $BC, $08, $1E, $12
	db $BE, $18, $C0, $08, $16, $0C, $B2, $B4
	db $B7, $BC, $08, $12, $02, $BE, $18, $C0
	db $00

_1B5B: ; Bonus -unused-
	db $E0, $01, $18, $00, $A4, $08, $28, $B2
	db $B4, $B7, $BC, $08, $0C, $1A, $BE, $0C
	db $C0, $08, $12, $1E, $B2, $B4, $B7, $BC
	db $08, $0C, $16, $BE, $18, $C0, $08, $02
	db $12, $B2, $B4, $B7, $BC, $BE, $18, $C0
	db $00

_1B84: ; Player amoeba hit
	db $E0, $2A, $0C, $64, $A1, $00

_1B8A: ; Last base entry 2.door close
	db $E0, $1C, $24, $7D, $F9, $91, $00, $24
	db $8C, $18, $F1, $00, $16, $8C, $E0, $05
	db $08, $64, $99, $00

_1B9E: ; Far last base entry 2.door close
	db $E0, $1C, $24, $46, $F9, $91, $00, $24
	db $8C, $18, $F1, $00, $16, $8C, $E0, $05
	db $08, $3C, $99, $00

_1BB2: ; Last base entry 2.door open
	db $E0, $1C, $24, $7D, $F9, $89, $00, $24
	db $90, $18, $F1, $00, $16, $93, $E0, $05
	db $08, $64, $99, $00

_1BC6: ; Far last base entry 2.door open
	db $E0, $1C, $24, $46, $F9, $89, $00, $24
	db $90, $18, $F1, $00, $16, $93, $E0, $05
	db $08, $3C, $99, $00

_1BDA: ; Atomic base core close
	db $E0, $1C, $24, $78, $F9, $9F, $00, $18
	db $98, $24, $F9, $9F, $00, $18, $98, $24
	db $64, $F9, $9F, $00, $18, $98, $24, $5A
	db $F9, $9F, $00, $18, $98, $24, $46, $F9
	db $9F, $00, $18, $98, $00

_1BFF: ; Atomic base core open
	db $E0, $1C, $24, $78, $F9, $98, $00, $18
	db $9F, $24, $F9, $98, $00, $18, $9F, $24
	db $64, $F9, $98, $00, $18, $9F, $24, $5A
	db $F9, $98, $00, $18, $9F, $24, $46, $F9
	db $98, $00, $18, $9F, $00

_1C24: ; Atomic base power supply on
	db $E0, $05, $06, $78, $9E, $E0, $1C, $18
	db $F9, $8C, $00, $18, $93, $24, $F1, $00
	db $22, $98, $00

_1C37: ; Atomic base power supply off
	db $E0, $05, $06, $78, $9E, $E0, $1C, $18
	db $F9, $98, $00, $18, $91, $24, $F1, $00
	db $22, $8C, $00

_1C4A: ; Hovering -unused-
	db $E0, $29, $7F, $64, $F9, $A3, $00, $7F
	db $A3, $7F, $F1, $00, $7F, $A3, $7F, $73
	db $F1, $00, $7F, $A3, $7F, $7D, $F1, $00
	db $7F, $A3, $7F, $F1, $00, $7F, $A3, $7F
	db $F1, $00, $7F, $A3, $7F, $69, $F1, $00
	db $7F, $A3, $7F, $50, $F1, $00, $7F, $A3
	db $7F, $32, $F1, $00, $7F, $A3, $7F, $1E
	db $F1, $00, $7D, $A3, $00

_1C87: ; Door open -unused-
	db $E0, $1C, $18, $64, $F9, $85, $00, $18
	db $8C, $18, $F1, $00, $16, $8C, $00

_1C96: ; Door close -unused-
	db $E0, $1C, $18, $64, $F9, $90, $00, $18
	db $89, $18, $F1, $00, $16, $89, $00

_1CA5: ; Hovering -unused-
	db $E0, $29, $7F, $5A, $F9, $A3, $00, $7F
	db $A3, $7F, $5F, $F1, $00, $7F, $A3, $7F
	db $64, $F1, $00, $7F, $A3, $7F, $6E, $F1
	db $00, $7F, $A3, $7F, $7D, $F1, $00, $7F
	db $A3, $7F, $78, $F1, $00, $7F, $A3, $7F
	db $6E, $F1, $00, $7F, $A3, $7F, $64, $F1
	db $00, $7F, $A3, $7F, $5F, $F1, $00, $7F
	db $A3, $7F, $46, $F1, $00, $7F, $A3, $7F
	db $3C, $F1, $00, $7F, $A3, $7F, $28, $F1
	db $00, $7F, $A3, $7F, $1E, $F1, $00, $7D
	db $A3, $00

_1CF7:
	db $E0, $29, $70, $5A, $F9, $99, $00, $70
	db $99, $7F, $5F, $F1, $00, $7F, $99, $7F
	db $64, $F1, $00, $7F, $99, $7F, $6E, $F1
	db $00, $7F, $99, $7F, $7D, $F1, $00, $7F
	db $99, $7F, $7D, $F1, $00, $7F, $99, $7F
	db $6E, $F1, $00, $7F, $99, $7F, $69, $F1
	db $00, $7F, $99, $7F, $5F, $F1, $00, $7F
	db $99, $7F, $50, $F1, $00, $7F, $99, $7F
	db $46, $F1, $00, $7F, $99, $7F, $32, $F1
	db $00, $7F, $99, $7F, $1E, $F1, $00, $7D
	db $99, $00

_1D49: ; Near enemy ring shot
	db $E0, $27, $12, $5A, $F9, $AD, $00, $12
	db $B0, $0C, $F1, $00, $0C, $AF, $0C, $F1
	db $00, $0A, $B0, $00

_1D5D: ; Mid enemy ring shot
	db $E0, $27, $12, $3C, $F9, $AD, $00, $12
	db $B0, $0C, $F1, $00, $0C, $AF, $0C, $F1
	db $00, $0A, $B0, $00

_1D71: ; Far enemy ring shot
	db $E0, $27, $12, $1E, $F9, $AD, $00, $12
	db $B0, $0C, $F1, $00, $0C, $AF, $0C, $F1
	db $00, $0A, $B0, $00

_1D85: ; Fox radio chat
	db $E0, $0F, $12, $14, $BE, $06, $00, $BE
	db $E0, $15, $18, $7D, $98, $E0, $16, $08
	db $7D, $8B, $18, $90, $0C, $8E, $18, $8C
	db $0C, $8E, $8D, $18, $00, $93, $00

_1DA4: ; Radio chat quit
	db $E0, $0F, $06, $14, $BE, $06, $00, $BE
	db $06, $14, $BE, $06, $00, $BE, $E0, $15
	db $18, $7D, $98, $00

_1DB8: ; Player camera change
	db $E0, $25, $48, $3C, $B6, $00

_1DBE: ; Good luck -BGM 4-
	db $E0, $22, $18, $00, $A4, $48, $78, $A0
	db $00

_1DC7: ; Continue let's go -BGM 30-
	db $E0, $33, $48, $78, $A2, $00

_1DCD: ; Left water splash out
	db $E0, $2B, $30, $7D, $0A, $F9, $A6, $00
	db $28, $9A, $00

_1DD8: ; Centre water splash
	db $E0, $2B, $30, $7D, $F9, $A6, $00, $28
	db $9A, $00

_1DE2: ; Right water splash
	db $E0, $2B, $30, $0A, $7D, $F9, $A6, $00
	db $28, $9A, $00

_1DED: ; Mid water splash
	db $E0, $2B, $30, $6E, $F9, $A6, $00, $28
	db $9A, $00

_1DF7: ; Far water splash
	db $E0, $2B, $30, $5A, $F9, $A6, $00, $28
	db $9A, $00

_1E01: ; Left water splash in
	db $E0, $21, $30, $7D, $0A, $A1, $00

_1E08: ; Centre water splash in
	db $E0, $21, $30, $7D, $A1, $00

_1E0E: ; Right water splash in
	db $E0, $21, $30, $0A, $7D, $A1, $00

_1E15: ; Mid water splash in
	db $E0, $21, $30, $6E, $A1, $00

_1E1B: ; Far water splash in
	db $E0, $21, $30, $5A, $A1, $00

_1E21: ; Dancing insector propelling
	db $E0, $1B, $0C, $78, $F9, $A1, $00, $0A
	db $9D, $18, $64, $F9, $A1, $00, $18, $A4
	db $24, $F1, $00, $18, $9D, $00

_1E37: ; Right object fly-by
	db $E0, $1E, $30, $00, $78, $F9, $B9, $00
	db $30, $B9, $18, $F1, $00, $16, $B0, $00

_1E47: ; Centre object fly-by
	db $E0, $1E, $30, $64, $F9, $B9, $00, $30
	db $B9, $18, $F1, $00, $16, $B0, $00

_1E56: ; Left object fly-by
	db $E0, $1E, $30, $78, $00, $F9, $B9, $00
	db $30, $B9, $18, $F1, $00, $16, $B0, $00

_1E66: ; Blade barrier player web hit
	db $E0, $19, $0C, $78, $F9, $B9, $00, $0C
	db $A4, $0C, $F1, $00, $0A, $9D, $E0, $05
	db $06, $78, $9E, $00

_1E7A: ; -unused-
	db $E0, $05, $06, $78, $9E, $0C, $A1, $00

_1E82: ; Blade barrier post-drill attack
	db $E0, $1C, $12, $78, $F9, $95, $00, $12
	db $98, $12, $F1, $00, $12, $96, $12, $F1
	db $00, $12, $94, $12, $F1, $00, $10, $92
	db $00

_1E9B: ; Cone triangle
	db $E0, $07, $12, $78, $F9, $95, $00, $12
	db $98, $12, $F1, $00, $12, $97, $12, $F1
	db $00, $12, $96, $12, $F1, $00, $12, $95
	db $12, $F1, $00, $12, $94, $12, $F1, $00
	db $12, $93, $12, $F1, $00, $10, $92, $00

_1EC3: ; 1-6+2-3 Boss shadowing
	db $E0, $07, $08, $78, $98, $08, $00, $98
	db $08, $78, $98, $18, $64, $F9, $98, $00
	db $16, $95, $18, $50, $F9, $98, $00, $16
	db $95, $18, $3C, $F9, $98, $00, $16, $95
	db $18, $32, $F9, $98, $00, $16, $95, $18
	db $28, $F9, $98, $00, $16, $95, $00

_1EF2: ; Enemy warp-in
	db $E0, $07, $08, $78, $95, $18, $64, $F9
	db $89, $00, $16, $98, $18, $50, $F9, $8B
	db $00, $16, $9A, $00

_1F06: ; Rock crusher roll
	db $E0, $05, $05, $78, $8F, $8F, $90, $90
	db $91, $91, $92, $92, $93, $93, $94, $94
	db $95, $95, $96, $96, $97, $97, $98, $98
	db $99, $99, $9A, $9A, $9B, $9B, $9C, $9C
	db $9D, $9D, $9E, $9E, $9F, $9F, $A0, $A0
	db $A1, $A1

_1F30:
	db $E0, $05, $05, $78, $A2, $A2, $A2, $A3
	db $A3, $A4, $A4, $A5, $A5, $A6, $A6, $A7
	db $A7, $A8, $A8, $A9, $A9, $06, $78, $92
	db $30, $95, $00

_1F4B:
	db $E0, $05, $0C, $78, $9C, $18, $9C, $30
	db $78, $F9, $8E, $00, $30, $8F, $30, $F1
	db $00, $30, $90, $30, $F1, $00, $30, $91
	db $30, $F1, $00, $30, $92, $48, $F1, $00
	db $48, $91, $48, $F1, $00, $45, $90, $00

_1F73: ; Small boss explosion
	db $E0, $12, $30, $78, $F9, $8C, $00, $30
	db $8D, $30, $F1, $00, $30, $8E, $30, $F1
	db $00, $30, $8F, $30, $F1, $00, $30, $90
	db $30, $F1, $00, $30, $91, $30, $F1, $00
	db $30, $92, $30, $F1, $00, $2E, $93, $00

_1F9B: ; Plasma hydra arm hit
	db $E0, $02, $0C, $78, $F9, $9C, $00, $0C
	db $9B, $0C, $F1, $00, $0C, $A5, $18, $F1
	db $00, $15, $A3, $00

_1FAF: ; Enemy rocket player hit
	db $E0, $06, $12, $78, $F9, $93, $00, $12
	db $8F, $24, $F1, $00, $24, $9B, $24, $F1
	db $00, $21, $98, $00

_1FC3: ; Dodora hit
	db $E0, $02, $12, $78, $F9, $91, $00, $12
	db $96, $0C, $F1, $00, $0C, $A2, $24, $F1
	db $00, $24, $9F, $24, $F1, $00, $21, $97
	db $00

_1FDC:
	db $E0, $02, $10, $00, $8C, $12, $3C, $50
	db $F9, $91, $00, $12, $96, $0C, $F1, $00
	db $0C, $A2, $24, $F1, $00, $24, $9F, $24
	db $F1, $00, $21, $97, $00

_1FF9: ; Dodora egg crack + bird
	db $E0, $15, $08, $78, $B4, $B9, $24, $BE

_2001: ; Bird scream -unused-
	db $E0, $07, $0C, $78, $F9, $A1, $00, $0C
	db $A5, $0C, $F1, $00, $0C, $B1, $24, $F1
	db $00, $21, $AF, $00

_2015: ; Percentage ring
	db $E0, $00, $03, $28, $B7, $B9, $B7, $B9
	db $B7, $B9, $08, $BB, $18, $BE, $00

_2024: ; Big support ring
	db $E0, $01, $0C, $1E, $BC, $BE, $BC, $BE
	db $0C, $1E, $0F, $C1, $24, $C6, $0C, $14
	db $08, $C1, $24, $C6, $0C, $0A, $00, $C1
	db $24, $C6, $00

_203F:
	db $E0, $01, $15, $00, $A4, $0C, $1E, $BC
	db $BE, $BC, $BE, $0C, $0F, $1E, $C1, $24
	db $C6, $0C, $08, $14, $C1, $24, $C6, $0C
	db $00, $0A, $C1, $24, $C6, $00

_205D: ; Controls select
	db $E0, $01, $08, $3C, $BE, $18, $C3, $08
	db $00, $0A, $BE, $18, $C3, $00

_206B:
	db $E0, $01, $18, $00, $A4, $08, $14, $00
	db $BE, $18, $C3, $08, $05, $00, $BE, $18
	db $C3, $00

_207D: ; 1up ring hit
	db $E0, $00, $08, $28, $BB, $BE, $C0, $18
	db $C3, $08, $00, $0A, $BB, $BE, $C0, $0C
	db $C3, $00

_208F:
	db $E0, $00, $18, $00, $A4, $08, $14, $00
	db $BB, $BE, $C0, $18, $C3, $08, $05, $00
	db $BB, $BE, $C0, $0C, $C3, $00

_20A5: ; Left gate moving
	db $E0, $08, $18, $78, $00, $F9, $98, $00
	db $18, $9D, $24, $F1, $00, $21, $A1, $00

_20B5: ; Centre gate moving
	db $E0, $08, $18, $64, $F9, $98, $00, $18
	db $9D, $24, $F1, $00, $21, $A1, $00

_20C4: ; Right gate moving
	db $E0, $08, $18, $00, $78, $F9, $98, $00
	db $18, $9D, $24, $F1, $00, $21, $A1, $00

_20D4: ; Mid gate moving
	db $E0, $08, $18, $3C, $F9, $98, $00, $18
	db $9D, $24, $F1, $00, $21, $A1, $00

_20E3: ; Far gate moving
	db $E0, $08, $18, $1E, $F9, $98, $00, $18
	db $9D, $24, $F1, $00, $21, $A1, $00

_20F2: ; Player boost
	db $E0, $05, $48, $78, $F9, $95, $00, $45
	db $A1, $00

_20FC:
	db $E0, $11, $60, $3C, $F9, $91, $00, $5D
	db $AB, $00

_2106: ; Player brake
	db $E0, $03, $30, $78, $F9, $A3, $00, $2D
	db $95, $00

_2110: ; Incoming enemy
	db $E0, $26, $0C, $28, $F9, $B6, $00, $0A
	db $B9, $24, $F9, $B6, $00, $10, $B9, $0C
	db $28, $F9, $B6, $00, $0A, $B9, $24, $F9
	db $B6, $00, $10, $B9, $E0, $15, $24, $7D
	db $98, $E0, $13, $70, $7D, $8C, $E0, $15
	db $0C, $7D, $98, $00

_213C: ; Wing damaged
	db $E0, $26, $30, $00, $A4, $0C, $28, $F9
	db $B6, $00, $0A, $B9, $0C, $28, $F9, $B6
	db $00, $0A, $B9, $18, $00, $A4, $E0, $15
	db $24, $7D, $98, $E0, $14, $20, $7D, $8C
	db $E0, $16, $40, $7D, $8C, $E0, $15, $0C
	db $7D, $98, $00, $E0, $0E, $30, $32, $B7
	db $00

_216D: ; Phantron 2 landing
	db $E0, $0D, $06, $78, $89, $04, $00, $89
	db $24, $78, $F9, $89, $00, $22, $82, $00

_217D: ; Phantron 2 jump
	db $E0, $1D, $0C, $78, $F9, $A9, $00, $0A
	db $9D, $24, $F9, $9D, $00, $21, $AB, $00

_218D: ; Near enemy rocket shot
	db $E0, $0B, $30, $64, $95, $00

_2193: ; Mid enemy rocket shot
	db $E0, $0B, $30, $3C, $95, $00

_2199: ; Far enemy rocket shot
	db $E0, $0B, $30, $1E, $95, $00

_219F: ; Near enemy battery
	db $E0, $19, $08, $78, $9A, $48, $98, $00

_21A7: ; Mid enemy battery
	db $E0, $19, $08, $46, $9A, $48, $98, $00

_21AF: ; Far enemy battery
	db $E0, $19, $08, $28, $9A, $48, $98, $00

_21B7: ; Left enemy laser shot
	db $E0, $09, $30, $5A, $0A, $95, $00

_21BE: ; Centre enemy laser shot
	db $E0, $09, $30, $50, $95, $00

_21C4: ; Right enemy laser shot
	db $E0, $09, $30, $0A, $5A, $95, $00

_21CB: ; Mid enemy laser shot
	db $E0, $09, $30, $28, $95, $00

_21D1: ; Far enemy laser shot
	db $E0, $09, $30, $1E, $95, $00

_21D7: ; Right wing damaged
	db $E0, $05, $30, $0A, $5A, $A9, $00

_21DE: ; Left wing damaged
	db $E0, $05, $30, $5A, $0A, $A9, $00

_21E5: ; Wingless arwing collision
	db $E0, $05, $18, $5A, $B5, $00

_21EB: ; Right wing crash
	db $E0, $05, $18, $0A, $5A, $B5, $00

_21F2: ; Left wing crash
	db $E0, $05, $18, $5A, $0A, $B5, $00

_21F9: ; Player crash
	db $E0, $0D, $06, $64, $9D, $60, $78, $9D
	db $00

_2202: ; Near enemy hit
	db $E0, $0D, $20, $46, $F9, $8E, $00, $1E
	db $85, $00

_220C: ; Mid enemy hit
	db $E0, $0D, $20, $28, $F9, $8E, $00, $1E
	db $85, $00

_2216: ; Far enemy hit
	db $E0, $0D, $20, $1E, $F9, $8E, $00, $1E
	db $85, $00

_2220: ; Near enemy explosion
	db $E0, $06, $60, $78, $98, $00

_2226:
	db $E0, $06, $18, $00, $98, $60, $50, $95
	db $00

_222F: ; Mid enemy explosion
	db $E0, $06, $70, $5A, $95, $00

_2235: ; Far enemy explosion
	db $E0, $06, $70, $46, $95, $00

_223B: ; Near big boss explosion
	db $E0, $0D, $0C, $78, $97, $12, $97, $0C
	db $64, $95, $12, $95, $0C, $50, $90, $60
	db $90, $00

_224D:
	db $E0, $0D, $24, $00, $98, $0C, $64, $00
	db $97, $12, $97, $0C, $50, $00, $95, $18
	db $95, $0C, $3C, $00, $90, $60, $90, $00

_2265:
	db $E0, $0D, $24, $00, $98, $0C, $00, $64
	db $97, $18, $97, $0C, $00, $50, $95, $18
	db $95, $0C, $00, $3C, $90, $60, $90, $00

_227D: ; Mid big boss explosion
	db $E0, $0D, $0C, $64, $97, $12, $97, $0C
	db $50, $95, $12, $95, $0C, $3C, $90, $60
	db $90, $00

_228F:
	db $E0, $0D, $24, $00, $98, $0C, $50, $00
	db $97, $12, $97, $0C, $3C, $00, $95, $18
	db $95, $0C, $28, $00, $90, $60, $90, $00

_22A7:
	db $E0, $0D, $24, $00, $98, $0C, $00, $50
	db $97, $18, $97, $0C, $00, $3C, $95, $18
	db $95, $0C, $00, $28, $90, $60, $90, $00

_22BF: ; Far big boss explosion
	db $E0, $0D, $0C, $3C, $97, $12, $97, $0C
	db $32, $95, $12, $95, $0C, $1E, $90, $60
	db $90, $00

_22D1:
	db $E0, $0D, $24, $00, $98, $0C, $32, $00
	db $97, $12, $97, $0C, $1E, $00, $95, $18
	db $95, $0C, $14, $00, $90, $60, $90, $00

_22E9:
	db $E0, $0D, $24, $00, $98, $0C, $00, $32
	db $97, $18, $97, $0C, $00, $1E, $95, $18
	db $95, $0C, $00, $14, $90, $60, $90, $00

_2301:
	db $E0, $0D, $0C, $78, $9C, $9C, $60, $9C
	db $00

_230A: ; Player down
	db $E0, $0D, $06, $00, $98, $0C, $00, $64
	db $98, $0C, $5A, $00, $98, $0C, $00, $50
	db $98, $48, $46, $00, $95, $60, $00, $28
	db $95, $00

_2324: ; Player laser shot
	db $E0, $20, $48, $32, $92, $00

_232A: ; Player bomb explosion
	db $E0, $05, $0C, $64, $9C, $30, $9C, $E0
	db $12, $60, $64, $F9, $85, $00, $60, $86
	db $60, $7D, $F1, $00, $5E, $87, $00

_2341: ; Player bomb shot
	db $E0, $0B, $06, $78, $98, $18, $78, $9D
	db $00

_234A: ; Dual beam shot
	db $E0, $19, $08, $5A, $A1, $E0, $17, $24
	db $F9, $98, $00, $22, $B0, $00

_2358: ; Near laser deflect
	db $E0, $0A, $24, $21, $B2, $00

_235E: ; Mid laser deflect
	db $E0, $0A, $24, $11, $B2, $00

_2364: ; Far laser deflect
	db $E0, $0A, $24, $0D, $B2, $00

_236A: ; Left wing scratch
	db $E0, $05, $06, $5A, $00, $BC, $BC, $00

_2372: ; Right wing scratch
	db $E0, $05, $06, $00, $5A, $BC, $BC, $00

_237A:
	db $E0, $00, $10, $1E, $B4, $30, $1E, $B7
	db $00

_2383: ; Pause
	db $E0, $00, $08, $00, $B0, $10, $1E, $B0
	db $30, $1E, $BC, $00

; make sure patterns aren't too big
assert pc() <= $238f, "Used too much space"

;incbin	238F-24FC.bin			; 61 instrument params?
patches:
incsrc patches.asm

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
	call	_648
	clr7	!eons
	mov.b	a,!eons
	mov	y,#$4d
	call	apus
	mov	a,#$01
	bne	+
_2618:
	mov	a,#$30
+
	mov.b	!_ae,a
	mov	!_af,#$00
	mov.b	a,!sf1
	and	a,#$3f
	mov	x,a
	mov	a,_26b4+x
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

_2671:
	mov.b	a,!sf1
	and	a,#$c0
	clrc
	rol	a
	rol	a
	rol	a
	mov	x,a
	mov	y,#$06
	mul	ya
	mov	x,a
	mov	y,#$74
	mov	!ttt,#$04
-
	mov	a,_269C+x
	call	apus
	inc	x
	inc	y
	dbnz	!ttt,-
	mov	a,_269C+x
	mov	!_022f,a
	inc	x
	mov	a,_269C+x
	mov	!_022e,a
ret

_269C:
	db $20, $00, $00, $E8, $04, $00, $20, $00
	db $00, $EF, $00, $60, $20, $00, $00, $E5
	db $00, $80, $20, $00, $00, $E8, $01, $C0

_26b4:
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
	mov	a,_28BF+x
	call	apus
	inc	x
	inc	y
	dbnz	!ttt,_28a6
	mov	a,_28BF+x
	mov	!_022d,a
	inc	x
	mov	a,_28BF+x
	mov	!_022c,a
	ret

_28BF:
	db $20, $0C, $E0, $70, $02, $80, $20, $0C
	db $E0, $60, $07, $00, $00, $0E, $E0, $70
	db $03, $00, $0A, $0E, $E0, $70, $01, $80
	db $01, $0E, $E0, $7F, $01, $00, $01, $0E
	db $E0, $28, $07, $00, $2D, $0E, $E0, $70
	db $01, $00, $03, $0E, $E0, $7F, $01, $40
	db $03, $0E, $E0, $70, $03, $00, $13, $0E
	db $E0, $60, $00, $60, $0A, $0E, $E0, $7F
	db $00, $60, $0B, $0E, $E0, $60, $05, $00
	db $02, $0E, $E0, $7F, $01, $80, $13, $0E
	db $E0, $70, $01, $00, $02, $0E, $E0, $40
	db $08, $00

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
	mov	a,sfx_parameters-1+x
	mov.b	!sss,a
	xcn	a
	and	a,#$0f
	asl	a
	mov	y,a
	mov	a,!_03a0+y
	beq	_2960
	mov	x,a
	mov	a,sfx_parameters-1+x
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
	mov	a,sfx_chain_table-1+x
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
	jmp	_2a4a
+
	mov	a,!_03a0+x			; get sound effect index number
	asl	a				; double it to get pointer
	mov	y,a				; check if over 255
	bcs	_2a7b
	mov	a,_109f-1+y			; sound effect data pointer high
	mov	!_0391+x,a
	mov.b	!adk+1,a
	mov	a,_109f-2+y			; sound effect data pointer low
	mov	!_0390+x,a
	mov.b	!adk,a
	jmp	_2b29
_2a7b:
	mov	a,_109f-1+256+y			; sound effect data pointer high
	mov	!_0391+x,a
	mov.b	!adk+1,a
	mov	a,_109f-2+256+y			; sound effect data pointer low
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

_2B27:					; Jumped to from PROG_CODE_02.asm
	incw	!adk				; sound effect data pointer
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
	incw	!adk				; sound effect data pointer
	mov	a,(!adk+x)			; Get next byte of sound effect data
	mov.b	!sss,a				; Store it as volume
	bmi	_2B6F

; Value is under 80
	mov	y,!_03c2			; Get DSP register address
	call	apus				; Set DSP register (which?)
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)			; Get next byte of sound effect data
	bpl	_2B62				; Branch if byte is a volume setting (under $80)

; Set default volume
	mov	x,a				; store data byte
	mov.b	a,!sss				; Get last volume used
	mov	y,!_03c2			; Get DSP register address
	inc	y
	call	apus				; Set DSP register (volume)
	mov	a,x				; restore data byte
	bra	_2B6F

_2B62:
; Set new volume level
	mov	y,!_03c2			; Get DSP register address
	inc	y
	call	apus				; Set DSP register (volume)
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
	mov	a,(!adk+x)
	mov	x,!_03c0
	mov.b	!chn,x
	mov	y,a
	call	dss
	mov	a,!_03c1
	call	_3e79

_2BD6:
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)
	mov	x,!_03c0
	mov.b	!swphc+x,a
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)
	mov	x,!_03c0
	mov.b	!swpc+x,a
	push	a
	mov	x,#$00
	incw	!adk
	mov	a,(!adk+x)
	pop	y
	mov	x,!_03c0
	mov.b	!chn,x
	call	swpadset
	jmp	_2B8B

assert pc() <= $2bff, "Used too much space"

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
	mov	!_d2,#patches
	mov	!_d2+1,#patches>>8
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

assert pc() <= $3ebb, "Used too much space"

endspcblock execute $400
