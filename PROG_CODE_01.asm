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
call	_648
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
call	$257c
mov	x,#$01
call	flset
call	$2749
;
	mov	x,#$02			; fl2 & port2 check
	call	flset			; x = fl?
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
dss:
	cmp	y,#!dd0			; drams check
	bcc	dss0
;************************************************ 
;		drams set    ; x=channel  a=sno 
;************************************************ 
dds:
	call	snoset			; sno data set
	mov	y,#!c30			; (takasa)
;................................................
dss0:
	cmp	y,#!xxx			; tai or yyy ? 
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
;
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
_614:
dec	$d0
mov.b	a,$d0
and	a,#$03
mov	y,#$3f
mul	ya
mov	y,a
mov	$12,#$07
inc	y
mov	x,#$04
-
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
bne	-
dbnz	$12,$0621
ret
_648:
mov	y,#$00
mov	x,#$1b
mov	a,$03c6
-
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
bne	-
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
bne	+
inc	$18
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
call	_648
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
call	divx
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
beq	+
mov	a,$03f1
mov.b	!mvo,a
mov	a,#$00
mov	$03f1,a
jmp	cha02				; finished with command
+
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
beq	$06E3
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
call	divx
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
	bne	+
	mov	$04,a
	ret
+
	mov	a,$fdbe+x
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
	call	panx			; pand & panf  set    (a=0)
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
cha:
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
	call	keych			; keyoff & sweep & vib check
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
	beq	mvo40
;
	movw	ya,!mvoadw		; move keisan
	addw	ya,!mvow
	dbnz	!mvoc,+			; dec & bne
;					; mvoc = 0 (move end)
	movw	ya,!mvoc		; y <- mvom , a <- 00
+
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
	call	voly			; tre pan move & vol set
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
	adc.b	a,!wavs			; bias add.
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
	bpl	snoset4			; noise ?
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
	call	divx			; x=count a=sa c=+,-
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
	bne	+
	mov	a,$03f1
	bne	+
	mov	a,#$00
	movw	!mvow,ya		; main vol
+
	ret
;************************************************
;		main volume move
;************************************************
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
	call	divx			; x=count a=sa c=+,-
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
sofx:
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
	call	divx			; x=count a=sa c=+,-
;......
	mov	!pvoadw+x,a		; + shimo
	mov	a,y			; kami
	mov	!pvoad+x,a		; + kami
	ret
;************************************************
;		part tune
;************************************************
tunx:
	mov	$03e0+x,a
	mov	a,$03a0+x
	bne	+
	mov	a,$03e0+x
	mov	!tund+x,a
+
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
;************************************************
;		echo on channel & volume
;************************************************
ecvx:
	;call	data_in			; data in & inc add
	mov	$03c3,a
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
eofx:
	movw	!evolw,ya		; ya = 00
	movw	!evorw,ya		; EVOL "00" set
;
	set5	!flgs			; write disable
	ret
;************************************************
;		echo delay time & feed back
;************************************************
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
	mov	a,fild+x
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
_B5D:
	push	a
	mov.b	a,!keyd
	and.b	a,!fkin			; kinshi flag check
	pop	a
	beq	swpx
	mov	$10,#$02
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
	mov	$10,#$04
_B7D:
	call	add_inc			; inc add
	dbnz	$10,_B7D
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
	mov	a,$0360+x		;
	mov.b	!sss,a
	ret
;................................................
;************** div keisan  from tp2 & mv2 & pam & swp (x=count a=sa)
;................................................
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
spft:
	dw snox,panx,pamx,vibx,vofx,mv1x,mv2x,tp1x,tp2x
	dw ktpx,ptpx,trex,tofx,pv1x,pv2x,patx,vchx,swkx,swsx,sofx
	;dw tunx,ecvx,eofx,edlx,ev2x,swpx,wavx
	dw $0A82,ecvx,eofx,edlx,ev2x,_B5D,wavx
	;dw                                    selx,cutx,fftx,plyx	; !! test !!
spfp:
	db $01, $01, $02, $03, $00, $01, $02, $01, $02
	db $01, $01, $03, $00, $01, $02, $03, $01, $03, $03, $00
	db $01, $03, $00, $03, $03, $03, $01
	;db                                    $02, $00, $00, $00	; !! test !!
;................................................
;
;
;
;************************************************
;		part vol move
;************************************************
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
	mov	$0250+x,a
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
	mov	a,($14)+y
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
	beq	$0dc0
;
	mov	a,!vibhs+x
	cbne	!vibhc+x,$0dbe		; hold chu ?
;................................................
	mov	a,!vibcc+x		;
	cmp	a,!vibcs+x
	bne	vib15			; change chu ?
;...
	mov	a,!vibdm+x		; vib change end !
	bra	$0d94
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
	beq	$0dea
;...
	mov	a,!panad+x
	mov	y,a
	mov	a,!panadw+x		; + @ keisan
;
	call	hokan			; kkk sss <-- data set
;...
pnn04:
	bbc7	!uuu,$0df0
;......
	call	$0c80			; vol data set
;................................................
;************** sweep check *********************
;................................................
sppy:
	clr7	!uuu			; sweep chu flag
;
	call	swpdset			; kkk sss <-- swpd swpdw
;...
	mov.b	a,$a0+x			; sweep chu ?
	beq	$0e07
;
	mov.b	a,$a1+x			; hold chu?
	bne	$0e07
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
	bra	$0f08			;
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
incbin	0F21-24FC.bin


_24FD:
mov	a,#$80
mov	y,#$5c
call	apus
mov	a,$03c3
and	a,#$80
beq	$2512
set7	$4a
mov	y,#$4d
call	apus
mov	$05,#$00
clr7	$1a
mov	x,#$0e
mov	a,$021f
call	snox
mov	a,#$00
mov	$03c9,a
mov.b	$d1,a
mov.b	$ae,a
mov.b	$9e,a
mov	a,$03ee
mov	$038f,a
mov	a,$03ef
mov	$028e,a
ret

mov	x,#$60
mov.b	$9e,x
mov	$03c9,x
mov	a,#$00
mov	$032e,a
mov.b	x,$9e
setc
sbc	a,$030f
call	$0bbe
mov	$031e,a
mov	a,y
mov	$031f,a
mov.b	a,$9e
beq	_24FD
cmp	a,#$01
beq	_24FD
mov	a,#$00
mov	y,#$03
mov	x,#$0e
dec	$9e
call	$0cc4
mov	a,$030f
mov	$032f,a
mov	a,#$0a
mov	$035f,a
mov.b	$11,a
mov	$10,#$00
mov	x,#$0e
call	$0c80
ret

mov	a,$03f8
beq	$2584
mov	$01,#$00
mov	y,$09
cmp	y,$01
beq	$25a1
mov.b	a,$01
mov.b	$05,a
mov.b	$09,a
beq	$2537
mov	a,y
beq	$25b5
eor.b	a,$01
and	a,#$c0
bne	$25b5
mov.b	a,$d1
bne	$25cc
bra	$2618
mov.b	a,$01
bne	$25ac
mov	x,$03c9
beq	$25b4
bra	$2553
mov.b	a,$d1
bne	$25cc
mov.b	a,$05
bne	$262f
ret

mov	$d1,#$02
mov	a,#$80
mov	y,#$5c
call	apus
set7	$1a
mov	a,#$00
mov	$028e,a
mov.b	$ae,a
mov	$038f,a
ret

dbnz	$d1,$25cb
call	$2671
mov	a,#$80
call	$3e79
mov.b	a,$05
bmi	$25ee
bbs6	$05,$25e6
mov	y,#$70
mov	x,#$96
mov	a,#$a0
bra	$25ff
mov	y,#$f7
mov	x,#$b2
mov	a,#$ff
bra	$25ff
bbs6	$05,$25f9
mov	y,#$ff
mov	x,#$b2
mov	a,#$ff
bra	$25ff
mov	y,#$bb
mov	x,#$96
mov	a,#$e0
mov	$03cb,y
mov	$03c6,x
mov	$03fc,a
call	$0648
clr7	$4a
mov.b	a,$4a
mov	y,#$4d
call	apus
mov	a,#$01
bne	$261a
mov	a,#$30
mov.b	$ae,a
mov	$af,#$00
mov.b	a,$05
and	a,#$3f
mov	x,a
mov	a,$26b4+x
mov	x,#$0e
mov.b	$44,x
call	swpadset
ret

clr7	$13
mov.b	a,$ae
beq	$265c
mov	x,#$0e
call	$3e5f
mov	a,$03fc
mov	$032f,a
mov	$030f,a
mov	a,#$0a
mov	$035f,a
mov	$033f,a
mov	x,#$0e
mov	a,$0331+x
mov	y,a
mov	a,$0330+x
movw	$10,ya
mov	a,#$0e
call	$0c80
ret

mov	a,#$70
mov.b	$ae,a
mov	$af,#$00
mov.b	a,$18
and	a,#$03
or	 a,#$a4
mov	x,#$0e
mov.b	$44,x
call	swpadset
ret

mov.b	a,$05
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
mov	$12,#$04
mov	a,_269C+x
call	apus
inc	x
inc	y
dbnz	$12,$2683
mov	a,_269C+x
mov	$022f,a
inc	x
mov	a,_269C+x
mov	$022e,a
ret

_269C:
	db $20, $00, $00, $E8, $04, $00, $20, $00
	db $00, $EF, $00, $60, $20, $00, $00, $E5
	db $00, $80, $20, $00, $00, $E8, $01, $C0
	db $A4, $A6, $A7, $A8, $A6, $A7, $A8, $A9
	db $B0, $B0, $B0, $B0, $98, $98, $98, $98

mov	a,#$00
mov.b	$06,a
mov.b	$0d,a
mov	$03f6,a
mov.b	$ac,a
mov.b	$9c,a
mov.b	$9d,a
mov	$032d,a
mov	$030d,a
mov	a,$03ec
mov	$038d,a
mov	a,$03ed
mov	$028c,a
clr6	$1a
mov	x,#$0c
mov	a,$021d
call	snox
mov	a,$03c3
and	a,#$40
beq	$26fd
set6	$4a
mov	y,#$4d
call	apus
mov	a,#$40
mov	y,#$5c
jmp	apus
mov	x,$03f6
mov.b	$9c,x
mov	a,#$00
mov	$032c,a
mov.b	x,$9c
setc
sbc	a,$030d
call	$0bbe
mov	$031c,a
mov	a,y
mov	$031d,a
mov.b	a,$9c
bne	$2723
ret
cmp	a,#$01
beq	$26c4
mov	a,#$00
mov	y,#$03
mov	x,#$0c
dec	$9c
call	$0cc4
mov	a,$030d
mov	$032d,a
mov	a,$03fb
mov	$035d,a
mov.b	$11,a
mov	$10,#$00
mov	x,#$0c
call	$0c80
ret
mov	a,$03f8
beq	$2751
mov	$02,#$00
mov	y,$0a
cmp	y,$02
beq	$277d
mov.b	a,$02
mov.b	$06,a
mov.b	$0a,a
and	a,#$c0
beq	$2704
mov	a,y
eor.b	a,$02
and	a,#$0f
bne	$2779
mov.b	a,$0d
bne	$27a4
mov	a,y
eor.b	a,$02
and	a,#$30
beq	$2776
jmp	$27ff
jmp	$2839
mov.b	a,$02
bne	_278D
mov.b	a,$02
beq	$271e
mov.b	a,$0d
bne	$27a4
mov.b	a,$06
beq	_278C
jmp	$285f
_278C:
ret
_278D:
mov	$0d,#$02
mov	a,#$40
mov	y,#$5c
call	apus
set6	$1a
mov	a,#$00
mov	$028c,a
mov.b	$ac,a
mov	$038d,a
ret
dbnz	$0d,_278C
mov.b	a,$06
and	a,#$0f
setc
sbc	a,#$01
mov	x,a
mov	a,$2921+x
mov	$03f6,a
mov	a,$2930+x
mov	$03f9,a
mov	a,x
call	$289d
mov.b	a,$06
and	a,#$30
bne	$27ca
mov	y,$03f9
bne	$27cc
mov	y,#$ad
mov	$44,#$0c
mov	x,#$0c
call	dss
mov	a,#$40
call	$3e79
clr6	$4a
mov.b	a,$4a
mov	y,#$4d
call	apus
mov.b	a,$06
and	a,#$30
xcn	a
mov	x,a
mov	a,$291d+x
mov	$032d,a
mov	$030d,a
mov.b	a,$06
and	a,#$c0
xcn	a
lsr	a
lsr	a
mov	x,a
mov	a,$2919+x
mov	$033d,a
mov	x,$03f6
mov.b	$9c,x
mov.b	a,$06
and	a,#$30
bne	$280f
mov	a,$03f9
bne	$2811
mov	a,#$ad
mov.b	$ac,x
mov	$ad,#$00
mov	x,#$0c
mov.b	$44,x
call	swpadset
mov.b	a,$06
and	a,#$30
xcn	a
mov	x,a
mov	a,$291d+x
mov	$032c,a
mov.b	x,$9c
setc
sbc	a,$030d
call	$0bbe
mov	$031c,a
mov	a,y
mov	$031d,a
mov.b	a,$06
and	a,#$c0
xcn	a
lsr	a
lsr	a
mov	x,a
mov	a,$2919+x
mov	$03fb,a
mov	$035c,a
setc
sbc	a,$033d
mov	x,$03f6
mov.b	$9d,x
call	$0bbe
movw	$10,ya
mov	$034c,a
mov	a,y
mov	$034d,a
mov.b	a,$9c
beq	$2874
mov	a,#$00
mov	y,#$03
mov	x,#$0c
dec	$9c
call	$0cc4
mov	a,$030d
mov	$032d,a
clr7	$13
mov.b	a,$ac
beq	$287f
mov	x,#$0c
call	$3e5f
mov.b	a,$9d
beq	$289c
mov	a,#$30
mov	y,#$03
mov	x,#$0c
dec	$9d
call	$0cc4
mov	a,$033d
mov	y,a
mov	a,$033c
movw	$10,ya
mov	x,#$0c
call	$0c80
ret

mov	y,#$06
mul	ya
mov	x,a
mov	y,#$64
mov	$12,#$04
mov	a,_28BF+x
call	apus
inc	x
inc	y
dbnz	$12,$28a6
mov	a,_28BF+x
mov	$022d,a
inc	x
mov	a,_28BF+x
mov	$022c,a
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
	db $08, $00, $0A, $14, $0A, $00, $FF, $90
	db $60, $30, $30, $18, $40, $40, $50, $28
	db $20, $60, $40, $40, $40, $40, $48, $20
	db $20, $AB, $A1, $AD, $AD, $AD, $A9, $AC
	db $AD, $AD, $AD, $AD, $AD, $AD, $AD, $AB


;2934 code

mov.b	x,$03
mov.b	$11,x
mov	a,$0fdf+x
mov.b	$10,a
xcn	a
and	a,#$0f
asl	a
mov	y,a
mov	a,$03a0+y
beq	$2960
mov	x,a
mov	a,$0fdf+x
setc
cmp.b	a,$10
beq	$2960
bcc	$2960
jmp	$3eba
mov.b	a,$11
mov	$03a0+y,a
mov.b	$10,y
mov	a,#$01
lsr	$10
beq	$2971
asl	a
dbnz	$10,$296d
mov	$03c1,a
mov	$03c0,y
mov	a,$03c1
or.b	 a,$1a
mov.b	$1a,a
jmp	$29c5
mov	a,$00f7
cmp	a,$00f7
bne	$2981
mov	$00f7,a
mov	y,a
mov.b	a,$0b
mov.b	$0b,y
cbne	$0b,$2996
mov	y,#$00
mov.b	$03,y
mov.b	a,$03
beq	$29a0
cmp	a,#$b0
bcc	$29c2
ret

_29A1:
mov	a,$03f8
beq	$29be
mov	a,#$00
mov	$03f8,a
call	$3e96
bra	$29be
mov.b	a,$1a
and	a,#$c0
eor	a,#$ff
mov	$03f8,a
mov	y,#$5c
call	apus
mov.b	a,$03
bra	$29d5
jmp	$293f
call	$3ea6
mov	x,$03c0
mov.b	a,$03
cmp	a,#$01
beq	_29A1
cmp	a,#$02
beq	$29b0
mov	$03a0+x,a
cmp	a,#$0b
bcc	$29eb
cmp	a,#$0e
bcc	$29e8
cmp	a,#$15
bcc	$29eb
cmp	a,#$18
bcs	$29eb
call	$3e87
mov	a,#$03
mov	$03a1+x,a
mov	a,#$00
mov	$0280+x,a
mov.b	$a0+x,a
mov	$0381+x,a
mov	$02f0+x,a
mov	a,$03c1
or	 a,$0007
mov	$0007,a
mov	a,$03c1
mov	y,#$5c
call	apus
mov	a,$03a0+x
mov	x,a
mov	a,$0f20+x
mov.b	$03,a
bne	$29c2
ret

mov	a,$0007
mov	$03ce,a
beq	$2a51
mov	x,#$0a
mov	a,#$20
mov	$03c1,a
asl	$03ce
asl	$03ce
asl	$03ce
bcc	$2a4a
mov	$03c0,x
mov	a,x
xcn	a
lsr	a
mov	$03c2,a
mov	a,$03a1+x
bne	$2a52
mov	a,$03a0+x
beq	$2a4a
jmp	$2b0c
lsr	$03c1
dec	x
dec	x
bpl	$2a2f
ret

mov	$03c0,x
mov	a,$03a1+x
dec	a
mov	$03a1+x,a
beq	$2a61
jmp	$2a4a
mov	a,$03a0+x
asl	a
mov	y,a
bcs	$2a7b
mov	a,$109e+y
mov	$0391+x,a
mov.b	$2d,a
mov	a,$109d+y
mov	$0390+x,a
mov.b	$2c,a
jmp	$2b29
mov	a,$119e+y
mov	$0391+x,a
mov.b	$2d,a
mov	a,$119d+y
mov	$0390+x,a
mov.b	$2c,a
jmp	$2b29
mov	x,$03c0
mov	a,$03a0+x
cmp	a,#$0b
bcc	$2aac
cmp	a,#$0e
bcc	$2aa4
cmp	a,#$15
bcc	$2aac
cmp	a,#$18
bcs	$2aac
mov	a,$03ca
bne	$2aac
call	$3e96
mov	a,#$00
mov	$03a0+x,a
mov.b	$a0+x,a
mov	a,$03d0+x
mov	$02f0+x,a
mov	a,$03e0+x
mov	$0381+x,a
mov	a,$03e1+x
mov	$0280+x,a
mov.b	a,$1a
setc
sbc	a,$03c1
mov.b	$1a,a
mov	a,$0007
setc
sbc	a,$03c1
mov	$0007,a
mov.b	$44,x
mov	a,$0211+x
call	snox
mov	a,$03c1
and	a,$03c3
beq	$2b02
and.b	a,$4a
bne	$2b02
mov.b	a,$4a
clrc
adc	a,$03c1
mov.b	$4a,a
mov	y,#$4d
call	apus
mov	a,$03f3
setc
sbc	a,$03c1
mov	$03f3,a
mov	x,$03c0
ret
call	$2a8e
jmp	$2a4a
call	$3ea6
mov	$03c0,x
mov	a,$0391+x
mov	y,a
mov	a,$0390+x
movw	$2c,ya
mov	a,$03b0+x
dec	a
mov	$03b0+x,a
beq	$2b27
jmp	$2b94
incw	$2c
mov	a,$03c0
xcn	a
lsr	a
mov	$03c2,a
mov	x,#$00
mov	a,($2c+x)
beq	$2b06
bmi	$2b6f
mov	y,$03c0
mov	$03b1+y,a
incw	$2c
mov	a,($2c+x)
mov.b	$10,a
bmi	$2b6f
mov	y,$03c2
call	apus
mov	x,#$00
incw	$2c
mov	a,($2c+x)
bpl	$2b62
mov	x,a
mov.b	a,$10
mov	y,$03c2
inc	y
call	apus
mov	a,x
bra	$2b6f
mov	y,$03c2
inc	y
call	apus
mov	x,#$00
incw	$2c
mov	a,($2c+x)
cmp	a,#$e0
bne	$2b76
jmp	$3e20
cmp	a,#$f9
beq	$2bc1
cmp	a,#$f1
beq	$2bd6
mov	x,$03c0
mov	y,a
call	dss
mov	a,$03c1
call	$3e79
mov	x,$03c0
mov	a,$03b1+x
mov	$03b0+x,a
clr7	$13
mov	x,$03c0
mov.b	a,$a0+x
beq	$2ba2
call	$3e5f
bra	$2bb1
mov	a,#$02
cmp	a,$03b0+x
bne	$2bb1
mov	a,$03c1
mov	y,#$5c
call	apus
mov	x,$03c0
mov.b	a,$2d
mov	$0391+x,a
mov.b	a,$2c
mov	$0390+x,a
jmp	$2a4a
mov	x,#$00
incw	$2c
mov	a,($2c+x)
mov	x,$03c0
mov.b	$44,x
mov	y,a
call	dss
mov	a,$03c1
call	$3e79
mov	x,#$00
incw	$2c
mov	a,($2c+x)
mov	x,$03c0
mov.b	$a1+x,a
mov	x,#$00
incw	$2c
mov	a,($2c+x)
mov	x,$03c0
mov.b	$a0+x,a
push	a
mov	x,#$00
incw	$2c
mov	a,($2c+x)
pop	y
mov	x,$03c0
mov.b	$44,x
call	swpadset
jmp	$2b8b

warnpc $2BFF
