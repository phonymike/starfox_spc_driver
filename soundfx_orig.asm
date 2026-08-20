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
if !opt_f1_f9 == 0
%warnpc($121d)
endif

; ===========================
; begin sound effect patterns
; ===========================

se_androssscream4: ; Andross scream 4
	%inst(2D)
	%dur($7F) : %vol($7D,$64) : %pitch($81)

se_androssscream3: ; Andross scream 3
	%inst(2E)
	%dur($60) : %vol($7D) : %pitch($81)

se_androssscream2: ; Andross scream 2
	%inst(2D)
	%dur($5F) : %vol($64,$7D) : %pitch($82)
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
	%teammateradiochat()
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
	%teammateradiochat()

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
	%dur($1C) : %vol($00,$0A) : %pitch($C7)
	%pitch($C5)
	%dur($0F) : %vol($05,$14) : %pitch($C2)
	%pitch($C0)
	%dur($0A) : %vol($1E) : %pitch($C1)
	%pitch($BF)
	%dur($60) : %vol($28,$00) : %pitch($BD)
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
	%teammateradiochat()
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
	%dur($24) : %vol($50,$00)
	%pitchslide($A3,$18,$9C)
	%dur($24)
	%pitchslide($A3,$18,$9C)
	%dur($24) : %vol($64,$28)
	%pitchslide($A3,$18,$9C)
	%dur($24) : %vol($6E,$3C)
	%pitchslide($A3,$18,$9C)
	%dur($24) : %vol($7D,$50)
	%pitchslide($A3,$18,$9C)
	db $00

se_enemy: ; Enemy
	%inst(36)
	%dur($24) : %vol($00,$50)
	%pitchslide($A2,$18,$9B)
	%dur($24)
	%pitchslide($A2,$18,$99)
	%dur($24) : %vol($28,$64)
	%pitchslide($A2,$18,$9B)
	%dur($24) : %vol($3C,$6E)
	%pitchslide($A2,$18,$9B)
	%dur($24) : %vol($50,$7D)
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
	%dur($06) : %vol($46,$14)
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
	%dur($0A) : %vol($3C,$14)
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
	%dur($18) : %vol($14,$08) : %pitch($C3)
	%dur($30) : %vol($08,$00) : %pitch($C3)
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
	%dur($18) : %vol($14,$08) : %pitch($C3)
	%dur($30) : %vol($08,$00) : %pitch($C3)
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
	%dur($08) : %vol($0A,$28)
	%pitchslide($B7,$06,$B9)
	%dur($08) : %vol($00) : %pitch($B9)
	%dur($08) : %vol($0A,$28)
	%pitchslide($B7,$06,$B9)
	%dur($08) : %vol($00) : %pitch($B9)
	%dur($08) : %vol($0A,$28)
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
	%dur($5F) : %vol($64,$00)
	%pitchslide($A3,$5F,$A3)
	%dur($5F) : %vol($64,$1E)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($73,$46)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($7D,$64)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($50,$69)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($32,$50)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($1E,$32)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($0A,$1E)
	%pitchenv($5D,$A3)
	db $00

se_destructorengine: ; Destructor engine
	%inst(30)
	%dur($7F) : %vol($00,$64)
	%pitchslide($A3,$7F,$A3)
	%dur($7F)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($0A,$73)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($1E,$7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($32,$7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($46,$7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($5A,$7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($5A,$7D)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($50,$69)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($46,$5A)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($3C,$50)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($28,$41)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($1E,$32)
	%pitchenv($7F,$A3)
	%dur($7F) : %vol($14,$1E)
	%pitchenv($7D,$A3)
	db $00

se_destructorengine2: ; Destructor engine
	%inst(30)
	%dur($7F) : %vol($00,$64)
	%pitchslide($A1,$7F,$A1)
	%dur($7F) : %vol($0A,$64)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($1E,$73)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($32,$7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($46,$7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($5A,$7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($5A,$7D)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($7D,$5A)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($69,$50)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($5A,$46)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($50,$3C)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($41,$28)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($32,$1E)
	%pitchenv($7F,$A1)
	%dur($7F) : %vol($1E,$14)
	%pitchenv($7D,$A1)
	db $00

se_rockcrusherappears: ; Rock crusher appears
	%inst(2F)
	%dur($5F) : %vol($64,$00)
	%pitchslide($A3,$5F,$A3)
	%dur($5F) : %vol($64,$1E)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($73,$46)
	%pitchenv($5F,$A3)
	%dur($5F) : %vol($7D,$64)
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
	%teammateradiochat()
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
	%teammateradiochat()
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
	%downradiochat()
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
	%teammateradiochat()
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
	%teammateradiochat()
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
	%downradiochat()
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
	%teammateradiochat()
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
	%teammateradiochat()
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
	%downradiochat()
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
	%dur($0C) : %vol($0A,$08) : %pitch($B9)
	%dur($18) : %vol($08,$00) : %pitch($B9)
	db $00

se_goodlucksubsfx: ; Good luck
	%inst(01)
	%dur($0C) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($1E) : %pitch($B7)
	%pitch($B9)
	%dur($08) : %vol($14) : %pitch($B9)
	%dur($0C) : %vol($14) : %pitch($B9)
	%dur($0C) : %vol($0A,$08) : %pitch($B9)
	%dur($18) : %vol($08,$00) : %pitch($B9)
	db $00

se_bonuscredit: ; Bonus credit
	%inst(01)
	%dur($08) : %vol($32) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($2E,$1C) : %pitch($BE)
	%dur($18) : %pitch($C0)
	%dur($08) : %vol($24,$16) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($1E,$12) : %pitch($BE)
	%dur($18) : %pitch($C0)
	%dur($08) : %vol($16,$0C) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($12,$02) : %pitch($BE)
	%dur($18) : %pitch($C0)
	db $00

se_bonuscreditsubsfx: ; Bonus
	%inst(01)
	%dur($18) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($28) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($0C,$1A) : %pitch($BE)
	%dur($0C) : %pitch($C0)
	%dur($08) : %vol($12,$1E) : %pitch($B2)
	%pitch($B4)
	%pitch($B7)
	%pitch($BC)
	%dur($08) : %vol($0C,$16) : %pitch($BE)
	%dur($18) : %pitch($C0)
	%dur($08) : %vol($02,$12) : %pitch($B2)
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
	%teammateradiochat()
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
	%dur($30) : %vol($7D,$0A)
	%pitchslide($A6,$28,$9A)
	db $00

se_enemyupseacentre: ; Centre water splash
	%inst(2B)
	%dur($30) : %vol($7D)
	%pitchslide($A6,$28,$9A)
	db $00

se_enemyupsearight: ; Right water splash
	%inst(2B)
	%dur($30) : %vol($0A,$7D)
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
	%dur($30) : %vol($7D,$0A) : %pitch($A1)
	db $00

se_enemydownseacentre: ; Centre water splash in
	%inst(21)
	%dur($30) : %vol($7D) : %pitch($A1)
	db $00

se_enemydownsearight: ; Right water splash in
	%inst(21)
	%dur($30) : %vol($0A,$7D) : %pitch($A1)
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
	%dur($30) : %vol($00,$78)
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
	%dur($30) : %vol($78,$00)
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
	%dur($12) : %vol($3C,$50)
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
	%dur($0C) : %vol($1E,$0F) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($14,$08) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($0A,$00) : %pitch($C1)
	%dur($24) : %pitch($C6)
	db $00

se_gateofringsubsfx: ; Big support ring sub-sfx
	%inst(01)
	%dur($15) : %vol($00) : %pitch($A4)
	%dur($0C) : %vol($1E) : %pitch($BC)
	%pitch($BE)
	%pitch($BC)
	%pitch($BE)
	%dur($0C) : %vol($0F,$1E) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($08,$14) : %pitch($C1)
	%dur($24) : %pitch($C6)
	%dur($0C) : %vol($00,$0A) : %pitch($C1)
	%dur($24) : %pitch($C6)
	db $00

se_cursor: ; Controls select
	%inst(01)
	%dur($08) : %vol($3C) : %pitch($BE)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($00,$0A) : %pitch($BE)
	%dur($18) : %pitch($C3)
	db $00

se_controlsselectsubsfx: ; Controls select sub-sfx
	%inst(01)
	%dur($18) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($14,$00) : %pitch($BE)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($05,$00) : %pitch($BE)
	%dur($18) : %pitch($C3)
	db $00

se_itemcatch: ; 1up ring hit
	%inst(00)
	%dur($08) : %vol($28) : %pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($00,$0A) : %pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($0C) : %pitch($C3)
	db $00

se_itemcatchsubsfx: ; 1up ring hit sub-sfx
	%inst(00)
	%dur($18) : %vol($00) : %pitch($A4)
	%dur($08) : %vol($14,$00) : %pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($18) : %pitch($C3)
	%dur($08) : %vol($05,$00) : %pitch($BB)
	%pitch($BE)
	%pitch($C0)
	%dur($0C) : %pitch($C3)
	db $00

se_movingwallleft: ; Left gate moving
	%inst(08)
	%dur($18) : %vol($78,$00)
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
	%dur($18) : %vol($00,$78)
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
	%dur($30) : %vol($5A,$0A) : %pitch($95)
	db $00

se_lasercentre: ; Centre enemy laser shot
	%inst(09)
	%dur($30) : %vol($50) : %pitch($95)
	db $00

se_laserright: ; Right enemy laser shot
	%inst(09)
	%dur($30) : %vol($0A,$5A) : %pitch($95)
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
	%dur($30) : %vol($0A,$5A) : %pitch($A9)
	db $00

se_wingdestructleft: ; Left wing damaged
	%inst(05)
	%dur($30) : %vol($5A,$0A) : %pitch($A9)
	db $00

se_winglessarwingcollision: ; Wingless arwing collision
	%inst(05)
	%dur($18) : %vol($5A) : %pitch($B5)
	db $00

se_wingdamageright: ; Right wing crash
	%inst(05)
	%dur($18) : %vol($0A,$5A) : %pitch($B5)
	db $00

se_wingdamageleft: ; Left wing crash
	%inst(05)
	%dur($18) : %vol($5A,$0A) : %pitch($B5)
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
	%dur($0C) : %vol($64,$00) : %pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($50,$00) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($3C,$00) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossnearsubsubsfx: ; Near big boss explosion sub-sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($00,$64) : %pitch($97)
	%dur($18) : %pitch($97)
	%dur($0C) : %vol($00,$50) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($00,$3C) : %pitch($90)
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
	%dur($0C) : %vol($50,$00) : %pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($3C,$00) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($28,$00) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossmidsubsfx: ; Mid big boss explosion sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($00,$50) : %pitch($97)
	%dur($18) : %pitch($97)
	%dur($0C) : %vol($00,$3C) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($00,$28) : %pitch($90)
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
	%dur($0C) : %vol($32,$00) : %pitch($97)
	%dur($12) : %pitch($97)
	%dur($0C) : %vol($1E,$00) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($14,$00) : %pitch($90)
	%dur($60) : %pitch($90)
	db $00

se_destructbossfarsubsfx: ; Far big boss explosion sub-sfx
	%inst(0D)
	%dur($24) : %vol($00) : %pitch($98)
	%dur($0C) : %vol($00,$32) : %pitch($97)
	%dur($18) : %pitch($97)
	%dur($0C) : %vol($00,$1E) : %pitch($95)
	%dur($18) : %pitch($95)
	%dur($0C) : %vol($00,$14) : %pitch($90)
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
	%dur($0C) : %vol($00,$64) : %pitch($98)
	%dur($0C) : %vol($5A,$00) : %pitch($98)
	%dur($0C) : %vol($00,$50) : %pitch($98)
	%dur($48) : %vol($46,$00) : %pitch($95)
	%dur($60) : %vol($00,$28) : %pitch($95)
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
	%dur($06) : %vol($5A,$00) : %pitch($BC)
	%pitch($BC)
	db $00

se_wingtouchright: ; Right wing scratch
	%inst(05)
	%dur($06) : %vol($00,$5A) : %pitch($BC)
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