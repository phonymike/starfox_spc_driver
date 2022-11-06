org $008000
arch spc700

incsrc defines.asm
incsrc KAN.asm

base $400
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
mov	$0200+x,a
inc	x
bne	-

-
mov	$0300+x,a
inc	x
bne	-
;........................................
	inc	a
	call	esaset			; EDL & ESA set
;
	set5	!flgs			; echo off
;................................................
mov	a,#$96
mov	$03c6,a
mov	a,#$bb
mov	$03cb,a
call	$0648
;................................................
	mov	a,#$60
	mov	y,#$0c			; MVOL
	call	apus
;
	mov	y,#$1c			; MVOR
	call	apus
;
	mov	a,#$3c
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
start20:
;........................................
;	mov	a,ffk		; !! test !!
;	bne	start55		; !! test !!
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
	mov	a,#$38			; 14 count
	mul	ya
;
	clrc
	adc.b	a,!cnt
	mov.b	!cnt,a
	bcc	start50
;.......................................; 16mS
call	$257c
mov	x,#$01
call	$04ed
call	$2749
;
	mov	x,#$02			; fl2 & port2 check
	call	$04ed			; x = fl?
call	$2a1a
call	$2981
;........................................
	cmp	(!ekin),(!eclr)
	beq	start50
	inc	$03c7
;................................................
	mov	a,$03c7
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
	mov	a,$03f8
	bne	$04ce
start55:
	call	$07d5			; music
;
	mov	x,#$00			; fl0 & port0 check
	call	$04fe			; x = fl?
	jmp	$0451
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
	beq	$04e1			; kami = 0
;
	call	$0dc4			; pan move & sweep & vib check
;
start64:
	inc	x
	inc	x
	asl.b	!keyd			;
	bne	start62			; channel end ? (8ch)
;
start20x:
	call	$0614
	jmp	start20			; channel end
;************************************************
flset:
	mov.b	a,!sf0+x		; flag set flx
	mov	!port0+x,a		; flag return
;
	mov	a,!port0+x		; flag read
	cmp	a,!port0+x		; 2 kai check
	bne	$04f2			;
;
	mov	y,a
	mov	!fl0+x,y		; new data
dssr:
	ret
;************************************************
mov.b	a,$04+x
mov	$00f4+x,a
mov	a,$00f4+x
cmp	a,$00f4+x
bne	$0503
mov	y,a
mov.b	a,$08+x
mov	$08+x,y
cbne	$08+x,$0515
mov	y,#$00
mov	$00+x,y
ret

;************************************************
; 
;
;************************************************ 
;		Freq. data set
;************************************************ 
dss:
	cmp	y,#$ca			; drams check
	bcc	dss0
;************************************************ 
;		drams set    ; x=channel  a=sno 
;************************************************ 
dds:
	call	$0932			; sno data set
	mov	y,#$a4			; (takasa)
;................................................
dss0:
	cmp	y,#$c8			; tai or yyy ? 
	bcs	dssr
;................................................
	mov.b	a,!fkin			; kinshi flag check
	and.b	a,!keyd
	bne	dssr
;......
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
	adc	a,!swpd+x		; now + @
;......
	call	$0b9b			; sweep data set
;........................................ from kokaon
dss6:
	call	$0bb3			; kkk sss <-- swpd swpdw
;************************************************
;		fre. data set   kkk & sss  x=channel  bls set
;************************************************
dssx:
	mov	y,#$00			; S curve hosei
	mov.b	a,!kkk
	setc
	sbc	a,#52			; e40 = 52
	bcs	$0594			; e40 ijo add
;...
dssx02:
	mov.b	a,!kkk
	setc
	sbc	a,#19			; g10 = 19
	bcs	$0598
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
;
	asl	a
	mov	y,#00
	mov	x,#24			; decimal
	div	ya,x			; ya/x = a ... y
	mov	x,a			; x = oct.
;...
	mov	a,$0eb1+y		; high
	mov.b	!adx+1,a
	mov	a,$0eb0+y		; low
	mov.b	!adx,a
;
	mov	a,$0eb3+y		; high
	push	a
	mov	a,$0eb2+y		; low
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
	bra	$05cb
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
	mov	a,$0220+x		; 0. block su
	mov.b	y,!adx+1		; fre.(high)
	mul	ya			;
	movw	!adx+2,ya		;
;
	mov	a,$0220+x		; 0. block su
	mov.b	y,!adx			; fre.(low)
	mul	ya			;
	push	y			; --> low
;
	mov	a,$0221+x		; block su
	mov.b	y,!adx			; fre.(low)
	mul	ya			;
	addw	ya,!adx+2
	movw	!adx+2,ya		;
;
	mov	a,$0221+x		; block su
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
apusx:
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin			; kinshi flag check
	pop	a
	bne	apusr
;................................................
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
dec	$d0
mov.b	a,$d0
and	a,#$03
mov	y,#$3f
mul	ya
mov	y,a
mov	$12,#$07
inc	y
mov	x,#$04
mov1 c,$0019.6
eor1	c,$0019.5
rol	$18
rol	$19
mov.b	a,$19
and	a,$03cb
or	a,#$11
mov	$fe00+y,a
inc	y
mov.b	a,$18
or	a,#$11
mov	$fe00+y,a
inc	y
dec	x
bne	$0624
dbnz	$12,$0621
ret
mov	y,#$00
mov	x,#$1b
mov	a,$03c6
mov	$fe00+y,a
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
bne	$064f
inc	a
mov	$fe00+y,a
mov	y,#$fe
mov	a,#$00
mov	$3c80,a
mov	$3c81,y
mov	$3c82,a
mov	$3c83,y
mov.b	a,$18
or.b	a,$19
bne	$067a
inc	$18
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
call	$0ed5
mov	a,#$00
mov	$03ca,a
mov.b	$04,a
mov	$0005,a
mov	$0006,a
mov	$0007,a
mov.b	$1a,a
mov	y,#$10
mov	$039f+y,a
dbnz	y,$06a5
mov	a,#$96
mov	$03c6,a
mov	a,#$bb
mov	$03cb,a
call	$0648
;................................................
_6B7:
cmp	$04,#$11
beq	_6CF
mov	x,#$a0
mov.b	!mvoc,x
mov	$03ca,x
mov	a,#$00
mov.b	!mvom,a
setc
sbc.b	a,!mvo
call	$0bbe
movw	!mvoadw,ya
_6CF:
jmp	cha02				; finished with command
;................................................
_6D2:
mov	a,$03f1
bne	$06f5
mov.b	a,!mvo
mov	$03f1,a
mov	a,#$70
mov.b	!mvo,a
jmp	cha02				; finished with command
;................................................
_6E3:
mov	a,$03f1
beq	$06f5
mov	a,$03f1
mov.b	!mvo,a
mov	a,#$00
mov	$03f1,a
jmp	cha02				; finished with command
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
bne	$0721
_71D:
mov	x,#$fe
mov	a,#$09
_721:
mov	!tmpc,#$8f
mov	$02f0,x
mov	$02f2,x
mov	$02f4,x
mov.b	!tmpm,a
setc
sbc.b	a,!tmp
mov.b	x,!tmpc
call	$0bbe
movw	!tmpadw,ya
jmp	cha02				; finished with command
;................................................
_73C:
dec	$03ca
beq	_744
jmp	_7E8
_744:
	mov.b	a,!fkin
	eor	a,#$ff
	tset	!keyoffs,a
	mov	$04,#$00
	mov	!keyd,#$00
	mov	!mvo,#$c0		; main volume set
	mov	!tmp,#$20		; tempo data set
	ret
;................................................
adset:
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
_766:
clrc
mov	x,#$00
mov	$03ca,x
mov	$03f1,x
	mov.b	!sf0,a
	asl	a
;	beq	ks04			; 000h = end
;......
	mov	x,a			; shoki data set
	mov	a,$fdbf+x		; block add. shoki set
	mov	y,a
	bne	$077c
	mov.b	$04,a
ret
	mov	a,$fdbe+x
	movw	!ads,ya
;......
	mov	!sf0c,#$02		; count
;...................
ks04:
	mov.b	a,!fkin			; key off
	eor	a,#$ff
	tset	$0046,a			; keyoff set
	ret
;................................................ 
ks10:
	mov	x,#14			; shoki data set
	mov	!keyd,#$80

ks12:
	mov.b	a,!keyd
	and.b	a,!fkin
	and	a,#$c0
	bne	_7BC
;
	mov	a,#$ff
	mov	!pvod+x,a		; part vol
;
	mov	a,#10			; pan data set
	call	$098b			; pand & panf  set    (a=0)
;
	mov	!snos+x,a		; sound number
	mov	!tund+x,a		; tun shoki set
	mov	!ptps+x,a		; part tran. set
	mov	!swsc+x,a		; sweep count
	mov	$03e1+x,a
	mov	$03e0+x,a
	mov	$03d0+x,a
	mov.b	!vibd+x,a		; vib depth
	mov.b	!tred+x,a		; tre depth
;
_7BC:
	dec	x
	dec	x			; - 2
	lsr.b	!keyd
	bne	ks12
;......
	mov.b	!mvoc,a			; mvol count (a=0)
	mov.b	!evoc,a			; evol count 
	mov.b	!tmpc,a			; tempo count set
	mov.b	!ktps,a			; key trans. set
	mov.b	!blc,a			; block count
	mov.b	!wavs,a			; source
;
	mov	!mvo,#$c0		; main volume set
	mov	!tmp,#$20		; tempo data set
char:
	ret
;************************************************
;		music enso routin
;************************************************
	mov.b	a,$00
	beq	cha02
	jmp	decode_commands
cha02:
	mov.b	a,!sf0			; play chu ?
	beq	char			;
	mov	a,$03ca
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
ks40:
	movw	!adx+2,ya		; adx+2,+3 set
	mov	y,#15			; shoki address set (8ch)
;
ks42:
	mov	a,(!adx+2)+y		; part sento add. set
	mov	!add+y,a		; add.
	dec	y
	bpl	ks42
;........................................
	mov	x,#$00			; shoki data set
	mov	!keyd,#$01
;
ks44:
	mov.b	a,!add+1+x
	beq	ks46
;
	mov	a,!snos+x
	bne	ks46
;
	mov	a,#$00
	call	snoset			; sno data set
;
ks46:
	mov	a,#$00
	mov.b	!ptc+x,a		; pt  count = 0
;
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin
	and	a,#$c0
	pop	a
	bne	+
;
	mov.b	!panc+x,a		; pan move count
	mov.b	!pvoc+x,a		; vol move count
+
	inc	a
	mov.b	!ngc+x,a		; Nagasa count set (ngo)
;
	inc	x
	inc	x			; + 2
	asl.b	!keyd
	bne	ks44
;************************************************
txh:
	mov	x,#$00			; channel count
	mov.b	!vols,x			; vols reset
	mov	!keyd,#$01		; key data set
;................................................
tx00:
	mov.b	!chn,x
	mov.b	a,!add+1+x
	beq	tx60			; kami = 0 (no use channel)
;................................................
	dec.b	!ngc+x
	bne	tx22
;......
tx10:
	call	data_in			; data in & inc add
	bne	tx15			; block end ?
;
	mov.b	a,!ptc+x		; pattern chu ?
	beq	ks20			;
;................................................ 
	call	addset			; pattern start add set
;......
	dec.b	!ptc+x
	bne	tx10
;......
	mov	a,!adt+x		; add restore (pattern end)
	mov.b	!add+x,a
	mov	a,!adt+1+x
	mov.b	!add+1+x,a
	bra	tx10			;
;................................................
tx15:
	bmi	tx16			; d7 = 1 ?
;
	mov	!ngs+x,a		; Nagasa Store
;......
	call	data_in			; data in & inc add
	bmi	tx16			; d7 = 1 ?
;......
	push	a			; % & vol
	xcn	a			; kami
	and	a,#$07
	mov	y,a
	mov	a,$3ee8+y		; Gate off (%) set
	mov	!ngg+x,a
;
	pop	a			; shimo
	and	a,#$0f
	mov	y,a
	mov	a,$3ef0+y
	mov	!vol+x,a		; vol set
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
	call	data_in			; data in & inc add
;
tx16:
	cmp	a,#!sno			; special flag ?
	bcc	tx17
;
	call	spfx			; special flag
	bra	tx10
;
tx17:
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin
	pop	a
	bne	tx18
	call	dss			; freq. data set
;...................
tx18:
	mov	a,!ngs+x
	mov.b	!ngc+x,a
	mov	y,a
;
	mov	a,!ngg+x		; gate off (step) set
	mul	ya
	mov	a,y
	bne	tx19
	inc	a			; a = 1
tx19:
	mov.b	!ngo+x,a
	bra	tx40
;................................................
tx22:
	call	$0ce5			; keyoff & sweep & vib check
;................................................
tx40:
	call	$0b6a			; sweep check (next data)
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
	mov.b	a,!tmpc			; tmp move chu ?
	beq	evoy
;
	movw	ya,!tmpadw		; move keisan
	addw	ya,!tmpw
	dbnz	!tmpc,tmp20		; dec & bne
;					; tmpc = 0 (move end)
	movw	ya,!tmpc		; y <- tmpm , a <- 00
tmp20:
	movw	!tmpw,ya
;************************************************
;		evol move
;************************************************
evoy:
	mov.b	a,!evoc			; evo move chu ?
	beq	$08f1
;
	movw	ya,!evoladw		; move keisan
	addw	ya,!evolw
	movw	!evolw,ya
;
	movw	ya,!evoradw		; move keisan
	addw	ya,!evorw
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
	mov.b	a,!mvoc			; mvol move chu ?
	beq	$0903
;
	movw	ya,!mvoadw		; move keisan
	addw	ya,!mvow
	dbnz	!mvoc,$08fe		; dec & bne
;					; mvoc = 0 (move end)
	movw	ya,!mvoc		; y <- mvom , a <- 00
	movw	!mvow,ya
	mov	!vols,#$ff		; mvo set
;................................................
mvo40:
	mov	x,#$00			; vol set keyon & end
	mov	!keyd,#$01		; key data set (8ch)
;
mvo42:
	mov.b	a,!add+1+x
	beq	mvo46			; kami = 0
;
	call	$0c2b			; tre pan move & vol set
;
mvo46:
	inc	x
	inc	x
	asl.b	!keyd
	bne	$0908
;
mvo48:
	ret
;................................................
;
;
;************************************************
;		special flag check
;************************************************
spfx:
	asl	a
	mov	y,a
!_spft = !sno*2-256			; = $C0
	mov	a,spft+1-(!_spft)+y	; high
	push	a
	mov	a,spft-(!_spft)+y	; low
	push	a
;
	mov	a,y
	lsr	a
	mov	y,a
	mov	a,spfp-!sno+128+y	;mov	a,$0bb0+y
	beq	data_inr
;************************************************
;		data in  &  inc address
;************************************************
data_in:
	mov	a,(!add+x)		; data in
;
add_inc:
	inc.b	!add+x
	bne	data_inr
	inc.b	!add+1+x
data_inr:
	mov	y,a			; flag set
	ret
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
snoset:
	mov	!snos+x,a		; sno store
snoset0:
	mov	y,a			; d7 check
	bpl	snoset1
;......
	setc
	sbc	a,#$ca
;
	clrc
	adc.b	a,$5f			; bias add.
;......
snoset1:
	mov	y,#$06			; x=channel a=sno
	mul	ya
	movw	!adx,ya
	clrc
	adc	!adx,#$00		; #low sod
	adc	!adx+1,#$3d		; #high sod
;...
	mov.b	a,!fkin			; kinshi flag check
	and.b	a,!keyd
	bne	snosetr
;...
	push	x
	mov	a,x			; apuch
	xcn	a
	lsr	a
	or	a,#$04			; write address
	mov	x,a
;
	mov	y,#$00			; 1st data in
	mov	a,(!adx)+y		; sound data set
	bpl	$096b			; noise ?
;......
snoset2:
	and	a,#$1f			; noise clock store
	and	!flgs,#$20
	tset	!flgs,a			; noise clock store
;
	or	(!nons),(!keyd)		; noise channel store
;
	mov	a,y			; y = 0 (=dd0)
	bra	snoset8
;......
snoset4:
	mov.b	a,!keyd			; normal sno
	tclr	!nons,a			; noise channel clear
;...................
snoset6:
	mov	a,(!adx)+y		; sound data set
snoset8:
	mov	!apuadd,x		; write address
	mov	!apudt,a		; data write
;
	inc	x
	inc	y
	cmp	y,#$04
	bne	snoset6			; tensou data 4 
;
	pop	x
	mov	a,(!adx)+y		; 5 ban me (block su)
	mov	!bls+1+x,a		; block su store
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
	mov	!pand+x,a		; pan data
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
	sbc	a,!pand+x		; pan (now data)
	pop	x			; count --> x
;
	call	$0bbe			; x=count a=sa c=+,-
;......
	mov	!panadw+x,a		; + shimo
	mov	a,y			; kami
	mov	!panad+x,a		; + kami
	ret
;************************************************
;		vibrate
;************************************************
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
mv1x:
	;call	data_in			;;00
	mov	a,$03ca
	bne	$09e7
	mov	a,$03f1
	bne	$09e7
	mov	a,#$00
	movw	!mvow,ya		; main vol
	ret
;************************************************
;		main volume move
;************************************************
mvx2:
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
	call	$0bbe			; x=count a=sa c=+,-
;
	movw	!mvoadw,ya		; + @
	ret
;************************************************ 
;               tempo change   
;************************************************ 
tp1x:
	;call	data_in			;;00   
	mov	a,#$00
	movw	!tmpw,ya		; tempo
	ret
;************************************************ 
;               tempo move     
;************************************************ 
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
	call	$0bbe			; x=count a=sa c=+,-
;
	movw	!tmpadw,ya		; + @
	ret
;************************************************
;		key tras.
;************************************************
ktpx:
	;call	data_in			;; 0  
	mov.b	!ktps,a
	ret
;************************************************
;		part key tras.
;************************************************
ptpx:
	;call	data_in			;; x
	mov	$03d0+x,a
	mov	a,$03a0+x
	bne	+
	mov	a,$03d0+x
	mov	!ptps+x,a		; key trans. store
+
	ret
;************************************************
;		tremolo
;************************************************
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
	mov.b	!tred+x,a		; tre depth
	ret
;************************************************
;		sweep kurikaeshi
;************************************************
swkx:
	mov	a,#$01			;
	bra	swsx0
;................................................
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

	mov	$03e1+x,a
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
	mov	!swsc+x,a		;
	mov	$03e1+x,a
	ret
;************************************************
;		part vol set
;************************************************
pv1x:
	;call	data_in			;;0x
	mov	!pvod+x,a		; vol set
	mov	a,#$00
	mov	!pvodw+x,a
	ret
;************************************************
;		part vol move
;************************************************
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
	call	$0bbe			; x=count a=sa c=+,-
;......
	mov	!pvoadw+x,a		; + shimo
	mov	a,y			; kami
	mov	!pvoad+x,a		; + kami
	ret

mov	$03e0+x,a
mov	a,$03a0+x
bne	$0a90
mov	a,$03e0+x
mov	$0381+x,a
ret
;************************************************
;		rythm pattern
;************************************************
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
;
addset:
	mov	a,!adp+x		; pattern add. (low)
	mov.b	!add+x,a
	mov	a,!adp+1+x		; pattern add. (high)
	mov.b	!add+1+x,a
	ret

mov	$03c3,a
mov.b	$4a,a
call	data_in
mov	a,#$00
movw	$60,ya
call	data_in
mov	a,#$00
movw	$62,ya
clr5	$48
ret
mov.b	$68,a
call	data_in
mov.b	$69,a
setc
sbc.b	a,$61
mov.b	x,$68
call	$0bbe
movw	$64,ya
call	data_in
mov.b	$6a,a
setc
sbc.b	a,$63
mov.b	x,$68
call	$0bbe
movw	$66,ya
ret
movw	$60,ya
movw	$62,ya
set5	$48
ret
call	$0b14
call	data_in
mov.b	$4e,a
call	data_in
mov	y,#$08
mul	ya
mov	x,a
mov	y,#$0f
mov	a,$0e7c+x
call	apus
inc	x
mov	a,y
clrc
adc	a,#$10
mov	y,a
bpl	$0b03
mov.b	x,$44
ret
;........................................
esaset:
mov.b	$4d,a
mov	y,#$7d
mov	!apuadd,y
mov	a,!apudt
cmp.b	a,$4d
beq	$0b4d
and	a,#$0f
eor	a,#$ff
bbc7	$4c,$0b2c
clrc
adc.b	a,$4c
esaset1:
mov.b	$4c,a
mov	y,#$04
mov	a,$0e9b+y
mov	$00f2,a
mov	a,#$00
mov	$00f3,a
dbnz	y,$0b30
mov.b	a,$48
or	a,#$20
mov	y,#$6c
call	apus
mov.b	a,$4d
mov	y,#$7d
call	apus
asl	a
asl	a
asl	a
eor	a,#$ff
setc
adc	a,#$3c
mov	y,#$6d
jmp	apus
mov.b	$5f,a
ret
push	a
mov.b	a,$47
and.b	a,$1a
pop	a
beq	$0b8b
mov	$10,#$02
bra	$0b7d
mov.b	a,$a0+x
bne	$0bb2
mov	a,($30+x)
cmp	a,#$f9
bne	$0bb2
mov.b	a,$47
and.b	a,$1a
beq	$0b85
mov	$10,#$04
call	$092a
dbnz	$10,$0b7d
bra	$0bb2
call	$092a
call	data_in
mov.b	$a1+x,a
call	data_in
mov.b	$a0+x,a
call	data_in
clrc
adc.b	a,$50
adc	a,$02f0+x
and	a,#$7f
mov	$0380+x,a
setc
sbc	a,$0361+x
mov.b	y,$a0+x
push	y
pop	x
call	$0bbe
mov	$0370+x,a
mov	a,y
mov	$0371+x,a
ret
mov	a,$0361+x
mov.b	$11,a
mov	a,$0360+x
mov.b	$10,a
ret
notc
ror	$12
bpl	$0bc6
eor	a,#$ff
inc	a
mov	y,#$00
div	ya,x
push	a
mov	a,#$00
div	ya,x
pop	y
mov.b	x,$44
bbc7	$12,$0bd9
movw	$14,ya
movw	ya,$0e
subw	ya,$14
divr:
	ret
;................................................
spft:
skip 54

spfp:
	db $01, $01, $02, $03, $00, $01, $02, $01, $02
	db $01, $01, $03, $00, $01, $02, $03, $01, $03, $03, $00
	db $01, $03, $00, $03, $03, $03, $01
	db                                    $02, $00, $00, $00	; !! test !!




base $e9c
dseta: ;   EVOL EVOR EFB  EON  FLG                  NOOF PMON
	db $2c, $3c, $0d, $4d, $6c, !keyon, !keyoff, $3d, $2d, !keyoff
;base $ea6
dsetd: ;    1      2      3      4      5      6        7     8     9       10  
	db !evol, !evor, !efbs, !eons, !flgs, !keyons, !t00, !nons, !mons, !keyoffs