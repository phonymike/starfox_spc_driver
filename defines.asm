!cont = $0f1					; control res
;
!apuadd = $0f2					; APU address
!apudt = $0f3					;	data
;
!port0 = $0f4					;
!port1 = $0f5					;
!port2 = $0f6					;
!port3 = $0f7					;
;
!tmset = $0fa					; timer data set
!tmset2 = $0fb					;	set
!tmset3 = $0fc					;	set
;
!tmdt = $0fd					; timer data read
!tmdt2 = $0fe					;	read
!tmdt3 = $0ff					;	read
;************************************************
;cont = $01d1h					; control res.	  (0f1h)
;;
;apuadd = $01d2h				; APU address	  (0f2h)
;apudt = $01d3h					;	data	  (0f3h)
;;
;port0 = $01d4h					;		  (0f4h)
;port1 = $01d5h					;		  (0f5h)
;port2 = $01d6h					;		  (0f6h)
;port3 = $01d7h					;		  (0f7h)
;;
;tmset = $01dah					; timer data set  (0fah)
;tmset2 = $01dbh				;	set  (0fbh)
;tmset3 = $01dch				;	set  (0fch)
;;
;tmdt = $01ddh					; timer read	  (0fdh)
;tmdt2 = $01deh					;	 read 	  (0feh)
;tmdt3 = $01dfh					;	 read 	  (0ffh)
;************************************************
!pl1 = $0002					; freq. low
!ph1 = $0003					;	high
!son = $0004					; source no.
;
!keyon = $004c					;
!keyoff = $005c					;
;************************************************
!fl0 = $0000					;
!fl1 = $0001					;
!fl2 = $0002					;
!fl3 = $0003					;
; 
!sf0 = $0004					;
!sf1 = $0005					;
!sf2 = $0006					;
!sf3 = $0007					;
;
!fl0s = $0008					;
!fl1s = $0009					;
!fl2s = $000a					;
!fl3s = $000b					;
;
!sf0c = $000c					;
!sf3c = $000d					;
;
!t00 = $000e					; 
;************************************************ 
!sss = $0010				; sss & kkk pair
!kkk = $0011
!ttt = $0012
!uuu = $0013
;
!adx = $0014				; keisan (4byte)
;
!rdm = $0018				; random (2byte)
!fkin = $001a				; kinshi flag
;
!ffk = $001b				; F.F. store !! test !!
!cutk = $0400				; cut  store !! test !!
;........................................
!skc = $0020				; chs
!skdw = $0021				; 
!skd = $0022				; 
!skdadw = $0023				; 
!skdad = $0024				; 
!skca = $0025				; 
!skcb = $0026				; 
!skcs = $0027				; 
;........................................
!bkc = $0028				; chc
;........................................
!adk = $002c				; kokaon add. (2byte)
!sac = $002e				; chd
!sacs = $002f				;
;........................................
!add = $0030				; address store (16byte)
!ads = $0040				; block add. store (2byte)
;
!blc = $0042				; block count
;........................................
!cnt = $0043				; 1/16 counter (for tempo)
;
!chn = $0044				; channel No. store
!keyons = $0045				; key on set
!keyoffs = $0046			; key off set
!keyd = $0047				; key data set
;........................................
!flgs = $0048				; 06ch
!nons = $0049				; 03dh
!eons = $004a				; 04dh
!mons = $004b				; 02dh
!ekin = $004c				;
!eclr = $004d				;
!efbs = $004e				;
;........................................
!ktps = $0050				; key transpose
;
!tmpd = $0051				;
;
!tmpw = $0052				; tempo data store
!tmp = $0053				;
!tmpc = $0054				; (@)
!tmpm = $0055				; (@)
!tmpadw = $0056				; (@)
!tmpad = $0057				; (@)
;
!mvow = $0058				; main vol.
!mvo = $0059				;
!mvoc = $005a				; (@)
!mvom = $005b				; (@)
!mvoadw = $005c				; (@)
!mvoad = $005d				; (@)
!vols = $005e				; vol set flag
;
!wavs = $005f				; source
;........................................
!evolw = $0060				;
!evol = $0061				;
!evorw = $0062				;
!evor = $0063				;
;
!evoladw = $0064			;
!evolad = $0065				;
!evoradw = $0066			;
!evorad = $0067				;
;
!evoc = $0068				;
!evolm = $0069				;
!evorm = $006a				;
;........................................
!ngc = $0070				; count (@)
!ngo = $0071				; keyoff count (@)
!ngs = $0200				; nagasa store 8 channel
!ngg = $0201				; gate off (%)
;
!vol = $0210				; vol store
!snos = $0211				; sno store
!_021d = $021d				; used once
!_021f = $021f
;........................................
!bls = $0220				; block su (2byte)
!_022c = $022c				; used once
!_022d = $022d				; used once
!_022e = $022e				; used once
!_022f = $022f				; used once
;........................................
!adt = $0230				; add taihi (2byte)
!adp = $0240				; add store (2byte)
!_0250 = $0250
!_0280 = $0280				; used twice
!_028c = $028c				; used twice
;........................................
!ptc = $0080				; pattern count
;........................................
!pvoc = $0090				; (@)
!pvodw = $0300				; (addw)
!pvod = $0301				; (addw)
!_030d = $030d
!_030f = $030f
!pvoadw = $0310				; (addw)
!pvoad = $0311				; (addw)
!_031c = $031c				; used twice
!_031d = $031d				; used twice
!_031e = $031e				; used once
!_031f = $031f				; used once
!pvom = $0320				; (addw)
!volx = $0321				; tre x mvo x pvo x vol
!_032c = $032c				; used twice
!_032d = $032d
!_032f = $032f
!_032e = $032e
;
!panc = $0091				; (@)
!pandw = $0330				; (addw)
!pand = $0331				; (addw)
!_033c = $033c				; used once
!_033d = $033d
!_033f = $033f				; used once
!panadw = $0340				; (addw)
!panad = $0341				; (addw)
!_034c = $034c				; used once
!_034d = $034d				; used once
!panm = $0350				; (addw)
!panf = $0351				; pan phase
!_035c = $035c				; used once
!_035d = $035d				; used once
!_035f = $035f				; used twice
;
!swpc = $00a0				; (@)
!swphc = $00a1				; (@)
!swpdw = $0360				; (addw)
!swpd = $0361				; (addw)
!swpadw = $0370				; (addw)
!swpad = $0371				; (addw)
!swpm = $0380				; (addw)
!tund = $0381				; part tun

!_9c = $9c
!_9d = $9d
!_9e = $9e
!_ac = $ac
!_ad = $ad				; used once
!_ae = $ae
!_af = $af				; used twice
!_d0 = $d0				; used twice
!_d1 = $d1
!_d2 = $d2				; word

!_038d = $038d				; used twice
!_038f = $038f
!_0390 = $0390
!_0391 = $0391
!_039f = $039f
!_03a0 = $03a0
!_03a1 = $03a1
!_03b0 = $03b0
!_03b1 = $03b1				; used twice
!_03c0 = $03c0
!_03c1 = $03c1
!_03c2 = $03c2
!_03c3 = $03c3
!_03c6 = $03c6
!_03c7 = $03c7
!_03c9 = $03c9
!_03ca = $03ca
!_03cb = $03cb
!_03ce = $03ce
!_03d0 = $03d0
!_03e0 = $03e0
!_03e1 = $03e1
!_03ec = $03ec				; used once
!_03ed = $03ed				; used once
!_03ee = $03ee
!_03ef = $03ef
!_03f1 = $03f1
!_03f3 = $03f3				; used twice
!_03f6 = $03f6
!_03f8 = $03f8
!_03f9 = $03f9
!_03fb = $03fb				; used twice
!_03fc = $03fc				; used twice
;........................................
!swsc = $0280				;
!swshc = $0281				;
!_028e = $028e
!swsk = $0290				;
!swss = $0291				;
;
!vibc = $02a0				; count (?)
!vibhc = $00b0				; hold count (@)
!vibd = $00b1				; depth  (@)
!vibcad = $02a1				; + @ (!)
!vibhs = $02b0				; hold store (!)
;
!vibcc = $0100				; change count (1)
!vibcs = $02b1				; change store (!)
!vibad = $02c0				; (!)
!vibdm = $02c1				; (!)
;
!trec = $02d0				; count (?)
!trehc = $00c0				; hold count (@)
!tred = $00c1				; depth  (@)
!trecad = $02d1				; + @ (!)
!trehs = $02e0				; hold store (!)
;
!ptps = $02f0				; key transpose

!_3c80 = $3c80
!_3c81 = $3c81
!_3c82 = $3c82
!_3c83 = $3c83
;........................................
; Constants
!src_dir = $3c				; Sample directory base address (src_dir << 8 == address)
!patch_tab = $3d00			; Patch table base address

!voice_vol = $ff			; Voice volume
!main_vol = $c0				; Main volume
!tempo = $20				; Tempo
!max_sfx = $b0				; Max number of sound effects

!gft = $fdc0				; Song table base address
!_fe00 = $fe00