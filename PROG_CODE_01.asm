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
	bne	$04ce
start55:
	call	$07d5			; music
;
	mov	x,#$00			; fl0 & port0 check
	call	_4FE			; x = fl?
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
	call	$0dc4			; pan move & sweep & vib check
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
mov.b	a,$04+x
mov	!port0+x,a
-
mov	a,!port0+x
cmp	a,!port0+x
bne	-
mov	y,a
mov.b	a,$08+x
mov	$08+x,y
cbne	$08+x,+
mov	y,#$00
+
mov	$00+x,y
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
	mov	a,$0e7c+x
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
	call	$0e4a			; volx set
	bra	pany
;................................................
tre20:
	inc.b	!trehc+x		; hold chu
;
tre22:
	mov	a,#$ff			; y = depth (tre)
	call	$0e55			; volx set
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
	mov	a,$0e68+y
	setc
	sbc	a,$0e67+y		; sa --> a
	mov.b	y,!sss			; shimo
	mul	ya			; sa x 0.???
	mov	a,y			;          --> a
;
	mov.b	y,!kkk			; kami
	clrc
	adc	a,$0e67+y		; pan data --> a
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
movw	$14,ya
movw	$16,ya
push	x
pop	y
clrc
bne	$0cd7
adc	$16,#$1f
mov	a,#$00
mov	($14)+y,a
inc	y
bra	+
adc	$16,#$10
call	_CDE
inc	y
;
_CDE:
mov	a,($14)+y
+
adc	a,($16)+y
mov	($14)+y,a
ret
;************************************************
;		keyoff check
;************************************************
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
	bra	$0cfa
;......
key18:
	inc	y			; pat
	mov	a,(!adx)+y		; data in
	push	a			; add. low
	inc	y			;
	mov	a,(!adx)+y		; data in
	mov	y,a			; add. high
	pop	a
	bra	$0cfa			;
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
	mov	a,$02c1+x
	bra	$0d94
;......
vib15:
	setp
	inc.b	$00+x
	clrp
	mov	y,a
	beq	$0d90
	mov.b	a,$b1+x
	clrc
	adc	a,$02c0+x
	mov.b	$b1+x,a
	mov	a,$02a0+x
	clrc
	adc	a,$02a1+x
	mov	$02a0+x,a
	mov.b	$12,a
	asl	a
	asl	a
	bcc	$0da8
	eor	a,#$ff
	mov	y,a
	mov.b	a,$b1+x
	cmp	a,#$f1
	bcc	$0db4
	and	a,#$0f
	mul	ya
	bra	$0db8
	mul	ya
	mov	a,y
	mov	y,#$00
	call	$0e35
	jmp	$0582
	inc.b	$b0+x
	bbs7	$13,$0dbb
	ret
	clr7	$13
	mov.b	a,$c1+x
	beq	$0dd3
	mov	a,$02e0+x
	cbne	$c0+x,$0dd3
	call	$0e3d
	mov	a,$0331+x
	mov	y,a
	mov	a,$0330+x
	movw	$10,ya
	mov.b	a,!panc+x
	beq	$0dea
	mov	a,$0341+x
	mov	y,a
	mov	a,$0340+x
	call	$0e1f
	bbc7	$13,$0df0
	call	$0c80
	clr7	$13
	call	swpdset
	mov.b	a,$a0+x
	beq	$0e07
	mov.b	a,$a1+x
	bne	$0e07
	mov	a,$0371+x
	mov	y,a
	mov	a,$0370+x
	call	$0e1f
	mov.b	a,$b1+x
	beq	$0dc0
	mov	a,$02b0+x
	cbne	$b0+x,$0dc0
	mov	y,$51
	mov	a,$02a1+x
	mul	ya
	mov	a,y
	clrc
	adc	a,$02a0+x
	jmp	$0da0
	set7	$13
	mov.b	$12,y
	call	$0bd0
	push	y
	mov	y,$51
	mul	ya
	mov.b	$14,y
	mov	$15,#$00
	mov	y,$51
	pop	a
	mul	ya
	addw	ya,$14
	call	$0bd0
	addw	ya,$10
	movw	$10,ya
	ret
	set7	$13
	mov	y,$51
	mov	a,!trecad+x
	mul	ya
	mov	a,y
	clrc
	adc	a,$02d0+x
	asl	a
	bcc	$0e4f
	eor	a,#$ff
	mov.b	y,$c1+x
	mul	ya
	mov	a,y
	eor	a,#$ff
	mov	y,$59
	mul	ya
	mov	a,$0210+x
	mul	ya
	mov	a,$0301+x
	mul	ya
	mov	a,y
	mul	ya
	mov	a,y
	mov	$0321+x,a
	ret

db $12, $34, $56, $78



base $e9c
dseta: ;   EVOL EVOR EFB  EON  FLG                  NOOF PMON
	db $2c, $3c, $0d, $4d, $6c, !keyon, !keyoff, $3d, $2d, !keyoff
;base $ea6
dsetd: ;    1      2      3      4      5      6        7     8     9       10  
	db !evol, !evor, !efbs, !eons, !flgs, !keyons, !t00, !nons, !mons, !keyoffs
;................................................
;
;
;
;
;************************************************
base $eb0
gfd:	;c00  c01  d00  d01  e00  f00  f01  g00  g01  a00  a01  b00  1.0594631
; dw	0066,0070,0075,0079,0084,0089,0094,0100,0106,0112,0119,0126  ; c00
; dw	0133,0141,0150,0159,0168,0178,0189,0200,0212,0225,0238,0252  ; c10
; dw	0267,0283,0300,0318,0337,0357,0378,0401,0425,0450,0477,0505  ; c20
; dw	0535,0567,0601,0637,0675,0715,0757,0802,0850,0901,0954,1011  ; c30
; dw	1071,1135,1202,1274,1350,1430,1515,1605,1701,1802,1909,2022  ; c40
	dw 2143,2270,2405,2548,2700,2860,3030,3211,3402,3604,3818,4045  ; c50
	dw 4286;4541,4811,5097,5400,5721,6061,6422,6804,7208,7637,8091  ; c60
