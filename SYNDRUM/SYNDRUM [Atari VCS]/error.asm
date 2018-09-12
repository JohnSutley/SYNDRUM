game
.L00 ;  t  =  0

	LDA #0
	STA t
.L01 ;  u  =  0

	LDA #0
	STA u
.L02 ;  v  =  0

	LDA #0
	STA v
.L03 ;  w  =  0

	LDA #0
	STA w
.L04 ;  COLUBK  =  0

	LDA #0
	STA COLUBK
.L05 ;  COLUPF  =  $80

	LDA #$80
	STA COLUPF
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.title
 ; title

.
 ; 

.L06 ;  t  =  t  +  1

	INC t
.L07 ;  if t  =  31 then t  =  0

	LDA t
	CMP #31
     BNE .skipL07
.condpart0
	LDA #0
	STA t
.skipL07
.L08 ;  rem AUDV0 = 2

.L09 ;  rem AUDC0 = 4

.L010 ;  rem AUDF0 = 30 - t

.
 ; 

.
 ; 

.L011 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00111100, %11001111
	if (pfwidth>2)
	.byte %11001111, %00111100
 endif
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.
 ; 

.L012 ;  player0:

	LDX #<playerL012_0
	STX player0pointerlo
	LDA #>playerL012_0
	STA player0pointerhi
	LDA #7
	STA player0height
.
 ; 

.L013 ;  player1:

	LDX #<playerL013_1
	STX player1pointerlo
	LDA #>playerL013_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L014 ;  player0x = 130 : player0y = 10 : player1x = 138 : player1y = 10

	LDA #130
	STA player0x
	LDA #10
	STA player0y
	LDA #138
	STA player1x
	LDA #10
	STA player1y
.
 ; 

.L015 ;  COLUBK  =  0

	LDA #0
	STA COLUBK
.L016 ;  COLUPF  =  14

	LDA #14
	STA COLUPF
.
 ; 

.L017 ;  a  =  5

	LDA #5
	STA a
.L018 ;  b  =  8

	LDA #8
	STA b
.L019 ;  c  =  10

	LDA #10
	STA c
.L020 ;  d  =  5

	LDA #5
	STA d
.
 ; 

.p ; p  =  14

