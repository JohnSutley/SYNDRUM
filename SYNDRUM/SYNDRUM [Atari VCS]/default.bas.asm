 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
  if bankswitch == 64
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif

 ifconst bankswitch_hotspot
 if bankswitch_hotspot = $083F ; 0840 bankswitching hotspot
   .byte 0 ; stop unexpected bankswitches
 endif
 endif
start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifnconst multisprite
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 endif
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
     ; This is a 2-line kernel!
     ifnconst vertical_reflect
kernel
     endif
     sta WSYNC
     lda #255
     sta TIM64T

     lda #1
     sta VDELBL
     sta VDELP0
     ldx ballheight
     inx
     inx
     stx temp4
     lda player1y
     sta temp3

     ifconst shakescreen
         jsr doshakescreen
     else
         ldx missile0height
         inx
     endif

     inx
     stx stack1

     lda bally
     sta stack2

     lda player0y
     ldx #0
     sta WSYNC
     stx GRP0
     stx GRP1
     stx PF1L
     stx PF2
     stx CXCLR
     ifconst readpaddle
         stx paddle
     else
         sleep 3
     endif

     sta temp2,x

     ;store these so they can be retrieved later
     ifnconst pfres
         ldx #128-44+(4-pfwidth)*12
     else
         ldx #132-pfres*pfwidth
     endif

     dec player0y

     lda missile0y
     sta temp5
     lda missile1y
     sta temp6

     lda playfieldpos
     sta temp1
     
     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif
     clc
     sbc playfieldpos
     sta playfieldpos
     jmp .startkernel

.skipDrawP0
     lda #0
     tay
     jmp .continueP0

.skipDrawP1
     lda #0
     tay
     jmp .continueP1

.kerloop     ; enter at cycle 59??

continuekernel
     sleep 2
continuekernel2
     lda ballheight
     
     ifconst pfres
         ldy playfield+pfres*pfwidth-132,x
         sty PF1L ;3
         ldy playfield+pfres*pfwidth-131-pfadjust,x
         sty PF2L ;3
         ldy playfield+pfres*pfwidth-129,x
         sty PF1R ; 3 too early?
         ldy playfield+pfres*pfwidth-130-pfadjust,x
         sty PF2R ;3
     else
         ldy playfield-48+pfwidth*12+44-128,x
         sty PF1L ;3
         ldy playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sty PF2L ;3
         ldy playfield-48+pfwidth*12+47-128,x ;4
         sty PF1R ; 3 too early?
         ldy playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sty PF2R ;3
     endif

     ; should be playfield+$38 for width=2

     dcp bally
     rol
     rol
     ; rol
     ; rol
goback
     sta ENABL 
.startkernel
     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawP1 ;2
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continueP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         lda (player1color),y
         sta COLUP1
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif

     ifconst pfres
         lda playfield+pfres*pfwidth-132,x 
         sta PF1L ;3
         lda playfield+pfres*pfwidth-131-pfadjust,x 
         sta PF2L ;3
         lda playfield+pfres*pfwidth-129,x 
         sta PF1R ; 3 too early?
         lda playfield+pfres*pfwidth-130-pfadjust,x 
         sta PF2R ;3
     else
         lda playfield-48+pfwidth*12+44-128,x ;4
         sta PF1L ;3
         lda playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sta PF2L ;3
         lda playfield-48+pfwidth*12+47-128,x ;4
         sta PF1R ; 3 too early?
         lda playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sta PF2R ;3
     endif 
     ; sleep 3

     lda player0height
     dcp player0y
     bcc .skipDrawP0
     ldy player0y
     lda (player0pointer),y
.continueP0
     sta GRP0

     ifnconst no_blank_lines
         ifnconst playercolors
             lda missile0height ;3
             dcp missile0y ;5
             sbc stack1
             sta ENAM0 ;3
         else
             lda (player0color),y
             sta player0colorstore
             sleep 6
         endif
         dec temp1
         bne continuekernel
     else
         dec temp1
         beq altkernel2
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle
             inc paddle
             jmp continuekernel2
noreadpaddle
             sleep 2
             jmp continuekernel
         else
             ifnconst playercolors 
                 ifconst PFcolors
                     txa
                     tay
                     lda (pfcolortable),y
                     ifnconst backgroundchange
                         sta COLUPF
                     else
                         sta COLUBK
                     endif
                     jmp continuekernel
                 else
                     ifconst kernelmacrodef
                         kernelmacro
                     else
                         sleep 12
                     endif
                 endif
             else
                 lda (player0color),y
                 sta player0colorstore
                 sleep 4
             endif
             jmp continuekernel
         endif
altkernel2
         txa
         ifnconst vertical_reflect
             sbx #256-pfwidth
         else
             sbx #256-pfwidth/2
         endif
         bmi lastkernelline
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
         jmp continuekernel
     endif

altkernel

     ifconst PFmaskvalue
         lda #PFmaskvalue
     else
         lda #0
     endif
     sta PF1L
     sta PF2


     ;sleep 3

     ;28 cycles to fix things
     ;minus 11=17

     ; lax temp4
     ; clc
     txa
     ifnconst vertical_reflect
         sbx #256-pfwidth
     else
         sbx #256-pfwidth/2
     endif

     bmi lastkernelline

     ifconst PFcolorandheight
         ifconst pfres
             ldy playfieldcolorandheight-131+pfres*pfwidth,x
         else
             ldy playfieldcolorandheight-87,x
         endif
         ifnconst backgroundchange
             sty COLUPF
         else
             sty COLUBK
         endif
         ifconst pfres
             lda playfieldcolorandheight-132+pfres*pfwidth,x
         else
             lda playfieldcolorandheight-88,x
         endif
         sta.w temp1
     endif
     ifconst PFheights
         lsr
         lsr
         tay
         lda (pfheighttable),y
         sta.w temp1
     endif
     ifconst PFcolors
         tay
         lda (pfcolortable),y
         ifnconst backgroundchange
             sta COLUPF
         else
             sta COLUBK
         endif
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
     endif
     ifnconst PFcolorandheight
         ifnconst PFcolors
             ifnconst PFheights
                 ifnconst no_blank_lines
                     ; read paddle 0
                     ; lo-res paddle read
                     ; bit INPT0
                     ; bmi paddleskipread
                     ; inc paddle0
                     ;donepaddleskip
                     sleep 10
                     ifconst pfrowheight
                         lda #pfrowheight
                     else
                         ifnconst pfres
                             lda #8
                         else
                             lda #(96/pfres) ; try to come close to the real size
                         endif
                     endif
                     sta temp1
                 endif
             endif
         endif
     endif
     

     lda ballheight
     dcp bally
     sbc temp4


     jmp goback


     ifnconst no_blank_lines
lastkernelline
         ifnconst PFcolors
             sleep 10
         else
             ldy #124
             lda (pfcolortable),y
             sta COLUPF
         endif

         ifconst PFheights
             ldx #1
             ;sleep 4
             sleep 3 ; REVENG - this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 3
             sleep 2 ; REVENG - this was over 1 cycle
         endif

         jmp enterlastkernel

     else
lastkernelline
         
         ifconst PFheights
             ldx #1
             ;sleep 5
             sleep 4 ; REVENG - this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 4
             sleep 3 ; REVENG - this was over 1 cycle
         endif

         cpx #0
         bne .enterfromNBL
         jmp no_blank_lines_bailout
     endif

     if ((<*)>$d5)
         align 256
     endif
     ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
     lda #0
     tay ; REVENG - added so we don't cross a page
     jmp .continuelastP1

.endkerloop     ; enter at cycle 59??
     
     nop

.enterfromNBL
     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

enterlastkernel
     lda ballheight

     ; tya
     dcp bally
     ; sleep 4

     ; sbc stack3
     rol
     rol
     sta ENABL 

     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawlastP1
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continuelastP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
     else
         lda (player1color),y
         sta COLUP1
     endif

     dex
     ;dec temp4 ; might try putting this above PF writes
     beq endkernel


     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

     ifnconst player1colors
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif
     
     lda.w player0height
     dcp player0y
     bcc .skipDrawlastP0
     ldy player0y
     lda (player0pointer),y
.continuelastP0
     sta GRP0



     ifnconst no_blank_lines
         lda missile0height ;3
         dcp missile0y ;5
         sbc stack1
         sta ENAM0 ;3
         jmp .endkerloop
     else
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle2
             inc paddle
             jmp .endkerloop
noreadpaddle2
             sleep 4
             jmp .endkerloop
         else ; no_blank_lines and no paddle reading
             pla
             pha ; 14 cycles in 4 bytes
             pla
             pha
             ; sleep 14
             jmp .endkerloop
         endif
     endif


     ; ifconst donepaddleskip
         ;paddleskipread
         ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
         ; plus we get a lo-res paddle read
         ; bmi donepaddleskip
     ; endif

.skipDrawlastP0
     lda #0
     tay
     jmp .continuelastP0

     ifconst no_blank_lines
no_blank_lines_bailout
         ldx #0
     endif

endkernel
     ; 6 digit score routine
     stx PF1
     stx PF2
     stx PF0
     clc

     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif

     sbc playfieldpos
     sta playfieldpos
     txa

     ifconst shakescreen
         bit shakescreen
         bmi noshakescreen2
         ldx #$3D
noshakescreen2
     endif

     sta WSYNC,x

     ; STA WSYNC ;first one, need one more
     sta REFP0
     sta REFP1
     STA GRP0
     STA GRP1
     ; STA PF1
     ; STA PF2
     sta HMCLR
     sta ENAM0
     sta ENAM1
     sta ENABL

     lda temp2 ;restore variables that were obliterated by kernel
     sta player0y
     lda temp3
     sta player1y
     ifnconst player1colors
         lda temp6
         sta missile1y
     endif
     ifnconst playercolors
         ifnconst readpaddle
             lda temp5
             sta missile0y
         endif
     endif
     lda stack2
     sta bally

     ; REVENG - strangely, this isn't required any more. might have
     ; resulted from the no_blank_lines score bounce fix
     ;ifconst no_blank_lines
         ;sta WSYNC
     ;endif

     lda INTIM
     clc
     ifnconst vblank_time
         adc #43+12+87
     else
         adc #vblank_time+12+87

     endif
     ; sta WSYNC
     sta TIM64T

     ifconst minikernel
         jsr minikernel
     endif

     ; now reassign temp vars for score pointers

     ; score pointers contain:
     ; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
     ; swap lo2->temp1
     ; swap lo4->temp3
     ; swap lo6->temp5
     ifnconst noscore
         lda scorepointers+1
         ; ldy temp1
         sta temp1
         ; sty scorepointers+1

         lda scorepointers+3
         ; ldy temp3
         sta temp3
         ; sty scorepointers+3


         sta HMCLR
         tsx
         stx stack1 
         ldx #$E0
         stx HMP0

         LDA scorecolor 
         STA COLUP0
         STA COLUP1
         ifconst scorefade
             STA stack2
         endif
         ifconst pfscore
             lda pfscorecolor
             sta COLUPF
         endif
         sta WSYNC
         ldx #0
         STx GRP0
         STx GRP1 ; seems to be needed because of vdel

         lda scorepointers+5
         ; ldy temp5
         sta temp5,x
         ; sty scorepointers+5
         lda #>scoretable
         sta scorepointers+1
         sta scorepointers+3
         sta scorepointers+5
         sta temp2
         sta temp4
         sta temp6
         LDY #7
         STY VDELP0
         STA RESP0
         STA RESP1


         LDA #$03
         STA NUSIZ0
         STA NUSIZ1
         STA VDELP1
         LDA #$F0
         STA HMP1
         lda (scorepointers),y
         sta GRP0
         STA HMOVE ; cycle 73 ?
         jmp beginscore


         if ((<*)>$d4)
             align 256 ; kludge that potentially wastes space! should be fixed!
         endif

loop2
         lda (scorepointers),y ;+5 68 204
         sta GRP0 ;+3 71 213 D1 -- -- --
         ifconst pfscore
             lda.w pfscore1
             sta PF1
         else
             ifconst scorefade
                 sleep 2
                 dec stack2 ; decrement the temporary scorecolor
             else
                 sleep 7
             endif
         endif
         ; cycle 0
beginscore
         lda (scorepointers+$8),y ;+5 5 15
         sta GRP1 ;+3 8 24 D1 D1 D2 --
         lda (scorepointers+$6),y ;+5 13 39
         sta GRP0 ;+3 16 48 D3 D1 D2 D2
         lax (scorepointers+$2),y ;+5 29 87
         txs
         lax (scorepointers+$4),y ;+5 36 108
         ifconst scorefade
             lda stack2
         else
             sleep 3
         endif

         ifconst pfscore
             lda pfscore2
             sta PF1
         else
             ifconst scorefade
                 sta COLUP0
                 sta COLUP1
             else
                 sleep 6
             endif
         endif

         lda (scorepointers+$A),y ;+5 21 63
         stx GRP1 ;+3 44 132 D3 D3 D4 D2!
         tsx
         stx GRP0 ;+3 47 141 D5 D3! D4 D4
         sta GRP1 ;+3 50 150 D5 D5 D6 D4!
         sty GRP0 ;+3 53 159 D4* D5! D6 D6
         dey
         bpl loop2 ;+2 60 180

         ldx stack1 
         txs
         ; lda scorepointers+1
         ldy temp1
         ; sta temp1
         sty scorepointers+1

         LDA #0 
         sta PF1
         STA GRP0
         STA GRP1
         STA VDELP0
         STA VDELP1;do we need these
         STA NUSIZ0
         STA NUSIZ1

         ; lda scorepointers+3
         ldy temp3
         ; sta temp3
         sty scorepointers+3

         ; lda scorepointers+5
         ldy temp5
         ; sta temp5
         sty scorepointers+5
     endif ;noscore
     LDA #%11000010
     sta WSYNC
     STA VBLANK
     RETURN

     ifconst shakescreen
doshakescreen
         bit shakescreen
         bmi noshakescreen
         sta WSYNC
noshakescreen
         ldx missile0height
         inx
         rts
     endif

; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*pfwidth-1
 else
 ldx #47-(4-pfwidth)*12 ; will this work?
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 if pfwidth=4
  asl ; multiply y pos by 4
 endif ; else multiply by 2
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 if pfwidth=4
   asl ; multiply by 4
 endif ; else multiply by 2
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 if pfwidth=4
   iny
   iny
 endif
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 ifnconst pfcenter
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 endif
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
drawscreen
     ifconst debugscore
         ldx #14
         lda INTIM ; display # cycles left in the score

         ifconst mincycles
             lda mincycles 
             cmp INTIM
             lda mincycles
             bcc nochange
             lda INTIM
             sta mincycles
nochange
         endif

         ; cmp #$2B
         ; bcs no_cycles_left
         bmi cycles_left
         ldx #64
         eor #$ff ;make negative
cycles_left
         stx scorecolor
         and #$7f ; clear sign bit
         tax
         lda scorebcd,x
         sta score+2
         lda scorebcd1,x
         sta score+1
         jmp done_debugscore 
scorebcd
         .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
         .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
         .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
         .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
         .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
         .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
         .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
         .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
done_debugscore
     endif

     ifconst debugcycles
         lda INTIM ; if we go over, it mucks up the background color
         ; cmp #$2B
         ; BCC overscan
         bmi overscan
         sta COLUBK
         bcs doneoverscan
     endif

overscan
     ifconst interlaced
         PHP
         PLA 
         EOR #4 ; flip interrupt bit
         PHA
         PLP
         AND #4 ; isolate the interrupt bit
         TAX ; save it for later
     endif

overscanloop
     lda INTIM ;wait for sync
     bmi overscanloop
doneoverscan

     ;do VSYNC

     ifconst interlaced
         CPX #4
         BNE oddframevsync
     endif

     lda #2
     sta WSYNC
     sta VSYNC
     STA WSYNC
     STA WSYNC
     lsr
     STA WSYNC
     STA VSYNC
     sta VBLANK
     ifnconst overscan_time
         lda #37+128
     else
         lda #overscan_time+128
     endif
     sta TIM64T

     ifconst interlaced
         jmp postsync 

oddframevsync
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #2
         sta VSYNC
         sta WSYNC
         sta WSYNC
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #0
         sta VSYNC
         sta VBLANK
         ifnconst overscan_time
             lda #37+128
         else
             lda #overscan_time+128
         endif
         sta TIM64T

postsync
     endif

     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop
             lda player0x,x
             sec
             sbc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop
         endif
     endif
     if ((<*)>$e9)&&((<*)<$fa)
         repeat ($fa-(<*))
         nop
         repend
     endif
     sta WSYNC
     ldx #4
     SLEEP 3
HorPosLoop     ; 5
     lda player0x,X ;+4 9
     sec ;+2 11
DivideLoop
     sbc #15
     bcs DivideLoop;+4 15
     sta temp1,X ;+4 19
     sta RESP0,X ;+4 23
     sta WSYNC
     dex
     bpl HorPosLoop;+5 5
     ; 4

     ldx #4
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 18

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 32

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 46

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 60

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 74

     sta WSYNC
     
     sta HMOVE ;+3 3


     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop2
             lda player0x,x
             clc
             adc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop2
         endif
     endif




     ;set score pointers
     lax score+2
     jsr scorepointerset
     sty scorepointers+5
     stx scorepointers+2
     lax score+1
     jsr scorepointerset
     sty scorepointers+4
     stx scorepointers+1
     lax score
     jsr scorepointerset
     sty scorepointers+3
     stx scorepointers

vblk
     ; run possible vblank bB code
     ifconst vblank_bB_code
         jsr vblank_bB_code
     endif
vblk2
     LDA INTIM
     bmi vblk2
     jmp kernel
     

     .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
     .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
     and #$0F
     asl
     asl
     asl
     adc #<scoretable
     tay 
     txa
     ; and #$F0
     ; lsr
     asr #$F0
     adc #<scoretable
     tax
     rts
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

.L021 ;  p  =  14

	LDA #14
	STA p
.L022 ;  q  =  14

	LDA #14
	STA q
.
 ; 

.jumphere
 ; jumphere

.
 ; 

.
 ; 

.L023 ;  COLUP0  =  216

	LDA #216
	STA COLUP0
.L024 ;  COLUP1  =  216

	LDA #216
	STA COLUP1
.
 ; 

.L025 ;  AUDV0  =  0

	LDA #0
	STA AUDV0
.L026 ;  AUDV1  =  0

	LDA #0
	STA AUDV1
.
 ; 

.L027 ;  if joy0up then goto __up0on

 lda #$10
 bit SWCHA
	BNE .skipL027
.condpart1
 jmp .__up0on

.skipL027
.L028 ;  goto __up0off

 jmp .__up0off

.after1
 ; after1

.
 ; 

.L029 ;  if joy0left then goto __left0on

 bit SWCHA
	BVS .skipL029
.condpart2
 jmp .__left0on

.skipL029
.L030 ;  goto __left0off

 jmp .__left0off

.after2
 ; after2

.
 ; 

.L031 ;  if joy0right then goto __right0on

 bit SWCHA
	BMI .skipL031
.condpart3
 jmp .__right0on

.skipL031
.L032 ;  goto __right0off

 jmp .__right0off

.after3
 ; after3

.
 ; 

.L033 ;  if joy0down then goto __down0on

 lda #$20
 bit SWCHA
	BNE .skipL033
.condpart4
 jmp .__down0on

.skipL033
.L034 ;  goto __down0off

 jmp .__down0off

.after4
 ; after4

.
 ; 

.L035 ;  drawscreen

 jsr drawscreen
.L036 ;  goto jumphere

 jmp .jumphere

.
 ; 

.
 ; 

.
 ; 

.L037 ;  rem kick

.
 ; 

.__up0on
 ; __up0on

.
 ; 

.
 ; 

.L038 ;  COLUPF  =  30

	LDA #30
	STA COLUPF
.
 ; 

.L039 ;  AUDV1  =  a

	LDA a
	STA AUDV1
.L040 ;  AUDC1  =  2

	LDA #2
	STA AUDC1
.L041 ;  AUDF1  =  10  -  a

	LDA #10
	SEC
	SBC a
	STA AUDF1
.
 ; 

.L042 ;  if a  >  0 then a  =  a  -  1

	LDA #0
	CMP a
     BCS .skipL042
.condpart5
	DEC a
.skipL042
.
 ; 

.
 ; 

.L043 ;  pfhline 2 9 5 on

	LDX #0
	LDA #5
	STA temp3
	LDY #9
	LDA #2
 jsr pfhline
.L044 ;  pfhline 2 8 5 on

	LDX #0
	LDA #5
	STA temp3
	LDY #8
	LDA #2
 jsr pfhline
.L045 ;  pfhline 2 7 5 on

	LDX #0
	LDA #5
	STA temp3
	LDY #7
	LDA #2
 jsr pfhline
.L046 ;  pfhline 2 6 5 on

	LDX #0
	LDA #5
	STA temp3
	LDY #6
	LDA #2
 jsr pfhline
.L047 ;  pfhline 2 5 5 on

	LDX #0
	LDA #5
	STA temp3
	LDY #5
	LDA #2
 jsr pfhline
.L048 ;  pfhline 2 4 5 on

	LDX #0
	LDA #5
	STA temp3
	LDY #4
	LDA #2
 jsr pfhline
.L049 ;  pfhline 2 3 5 on

	LDX #0
	LDA #5
	STA temp3
	LDY #3
	LDA #2
 jsr pfhline
.
 ; 

.L050 ;  t  =  0

	LDA #0
	STA t
.
 ; 

.L051 ;  goto after1

 jmp .after1

.
 ; 

.__up0off
 ; __up0off

.L052 ;  a  =  9

	LDA #9
	STA a
.L053 ;  if t  <  7 then t  =  t  +  1

	LDA t
	CMP #7
     BCS .skipL053
.condpart6
	INC t
.skipL053
.L054 ;  if t  =  7 then pfhline 2 9 5 off

	LDA t
	CMP #7
     BNE .skipL054
.condpart7
	LDX #1
	LDA #5
	STA temp3
	LDY #9
	LDA #2
 jsr pfhline
.skipL054
.L055 ;  if t  =  6 then pfhline 2 8 5 off

	LDA t
	CMP #6
     BNE .skipL055
.condpart8
	LDX #1
	LDA #5
	STA temp3
	LDY #8
	LDA #2
 jsr pfhline
.skipL055
.L056 ;  if t  =  5 then pfhline 2 7 5 off

	LDA t
	CMP #5
     BNE .skipL056
.condpart9
	LDX #1
	LDA #5
	STA temp3
	LDY #7
	LDA #2
 jsr pfhline
.skipL056
.L057 ;  if t  =  4 then pfhline 2 6 5 off

	LDA t
	CMP #4
     BNE .skipL057
.condpart10
	LDX #1
	LDA #5
	STA temp3
	LDY #6
	LDA #2
 jsr pfhline
.skipL057
.L058 ;  if t  =  3 then pfhline 2 5 5 off

	LDA t
	CMP #3
     BNE .skipL058
.condpart11
	LDX #1
	LDA #5
	STA temp3
	LDY #5
	LDA #2
 jsr pfhline
.skipL058
.L059 ;  if t  =  2 then pfhline 2 4 5 off

	LDA t
	CMP #2
     BNE .skipL059
.condpart12
	LDX #1
	LDA #5
	STA temp3
	LDY #4
	LDA #2
 jsr pfhline
.skipL059
.L060 ;  if t  =  1 then pfhline 2 3 5 off

	LDA t
	CMP #1
     BNE .skipL060
.condpart13
	LDX #1
	LDA #5
	STA temp3
	LDY #3
	LDA #2
 jsr pfhline
.skipL060
.L061 ;  goto after1

 jmp .after1

.
 ; 

.L062 ;  rem snare

.
 ; 

.__left0on
 ; __left0on

.
 ; 

.L063 ;  COLUPF  =  78

	LDA #78
	STA COLUPF
.
 ; 

.L064 ;  AUDV0  =  b

	LDA b
	STA AUDV0
.L065 ;  AUDC0  =  8

	LDA #8
	STA AUDC0
.L066 ;  AUDF0  =  2

	LDA #2
	STA AUDF0
.
 ; 

.L067 ;  if b  >  0 then b  =  b  -  1

	LDA #0
	CMP b
     BCS .skipL067
.condpart14
	DEC b
.skipL067
.
 ; 

.
 ; 

.L068 ;  pfhline 8 9 11 on

	LDX #0
	LDA #11
	STA temp3
	LDY #9
	LDA #8
 jsr pfhline
.L069 ;  pfhline 8 8 11 on

	LDX #0
	LDA #11
	STA temp3
	LDY #8
	LDA #8
 jsr pfhline
.L070 ;  pfhline 8 7 11 on

	LDX #0
	LDA #11
	STA temp3
	LDY #7
	LDA #8
 jsr pfhline
.L071 ;  pfhline 8 6 11 on

	LDX #0
	LDA #11
	STA temp3
	LDY #6
	LDA #8
 jsr pfhline
.L072 ;  pfhline 8 5 11 on

	LDX #0
	LDA #11
	STA temp3
	LDY #5
	LDA #8
 jsr pfhline
.L073 ;  pfhline 8 4 11 on

	LDX #0
	LDA #11
	STA temp3
	LDY #4
	LDA #8
 jsr pfhline
.L074 ;  pfhline 8 3 11 on

	LDX #0
	LDA #11
	STA temp3
	LDY #3
	LDA #8
 jsr pfhline
.
 ; 

.L075 ;  u  =  0

	LDA #0
	STA u
.
 ; 

.L076 ;  goto after2

 jmp .after2

.
 ; 

.__left0off
 ; __left0off

.L077 ;  b  =  5

	LDA #5
	STA b
.L078 ;  if u  <  7 then u  =  u  +  1

	LDA u
	CMP #7
     BCS .skipL078
.condpart15
	INC u
.skipL078
.L079 ;  if u  =  7 then pfhline 8 9 11 off

	LDA u
	CMP #7
     BNE .skipL079
.condpart16
	LDX #1
	LDA #11
	STA temp3
	LDY #9
	LDA #8
 jsr pfhline
.skipL079
.L080 ;  if u  =  6 then pfhline 8 8 11 off

	LDA u
	CMP #6
     BNE .skipL080
.condpart17
	LDX #1
	LDA #11
	STA temp3
	LDY #8
	LDA #8
 jsr pfhline
.skipL080
.L081 ;  if u  =  5 then pfhline 8 7 11 off

	LDA u
	CMP #5
     BNE .skipL081
.condpart18
	LDX #1
	LDA #11
	STA temp3
	LDY #7
	LDA #8
 jsr pfhline
.skipL081
.L082 ;  if u  =  4 then pfhline 8 6 11 off

	LDA u
	CMP #4
     BNE .skipL082
.condpart19
	LDX #1
	LDA #11
	STA temp3
	LDY #6
	LDA #8
 jsr pfhline
.skipL082
.L083 ;  if u  =  3 then pfhline 8 5 11 off

	LDA u
	CMP #3
     BNE .skipL083
.condpart20
	LDX #1
	LDA #11
	STA temp3
	LDY #5
	LDA #8
 jsr pfhline
.skipL083
.L084 ;  if u  =  2 then pfhline 8 4 11 off

	LDA u
	CMP #2
     BNE .skipL084
.condpart21
	LDX #1
	LDA #11
	STA temp3
	LDY #4
	LDA #8
 jsr pfhline
.skipL084
.L085 ;  if u  =  1 then pfhline 8 3 11 off

	LDA u
	CMP #1
     BNE .skipL085
.condpart22
	LDX #1
	LDA #11
	STA temp3
	LDY #3
	LDA #8
 jsr pfhline
.skipL085
.L086 ;  goto after2

 jmp .after2

.
 ; 

.__right0on
 ; __right0on

.
 ; 

.L087 ;  COLUPF  =  134

	LDA #134
	STA COLUPF
.
 ; 

.L088 ;  AUDV1  =  c

	LDA c
	STA AUDV1
.L089 ;  AUDC1  =  8

	LDA #8
	STA AUDC1
.L090 ;  AUDF1  =  2

	LDA #2
	STA AUDF1
.
 ; 

.L091 ;  if c  >  0 then c  =  c  -  1

	LDA #0
	CMP c
     BCS .skipL091
.condpart23
	DEC c
.skipL091
.
 ; 

.
 ; 

.L092 ;  pfhline 14 9 17 on

	LDX #0
	LDA #17
	STA temp3
	LDY #9
	LDA #14
 jsr pfhline
.L093 ;  pfhline 14 8 17 on

	LDX #0
	LDA #17
	STA temp3
	LDY #8
	LDA #14
 jsr pfhline
.L094 ;  pfhline 14 7 17 on

	LDX #0
	LDA #17
	STA temp3
	LDY #7
	LDA #14
 jsr pfhline
.L095 ;  pfhline 14 6 17 on

	LDX #0
	LDA #17
	STA temp3
	LDY #6
	LDA #14
 jsr pfhline
.L096 ;  pfhline 14 5 17 on

	LDX #0
	LDA #17
	STA temp3
	LDY #5
	LDA #14
 jsr pfhline
.L097 ;  pfhline 14 4 17 on

	LDX #0
	LDA #17
	STA temp3
	LDY #4
	LDA #14
 jsr pfhline
.L098 ;  pfhline 14 3 17 on

	LDX #0
	LDA #17
	STA temp3
	LDY #3
	LDA #14
 jsr pfhline
.
 ; 

.L099 ;  v  =  0

	LDA #0
	STA v
.
 ; 

.L0100 ;  goto after3

 jmp .after3

.
 ; 

.__right0off
 ; __right0off

.L0101 ;  c  =  10

	LDA #10
	STA c
.L0102 ;  if v  <  7 then v  =  v  +  1

	LDA v
	CMP #7
     BCS .skipL0102
.condpart24
	INC v
.skipL0102
.L0103 ;  if v  =  7 then pfhline 14 9 17 off

	LDA v
	CMP #7
     BNE .skipL0103
.condpart25
	LDX #1
	LDA #17
	STA temp3
	LDY #9
	LDA #14
 jsr pfhline
.skipL0103
.L0104 ;  if v  =  6 then pfhline 14 8 17 off

	LDA v
	CMP #6
     BNE .skipL0104
.condpart26
	LDX #1
	LDA #17
	STA temp3
	LDY #8
	LDA #14
 jsr pfhline
.skipL0104
.L0105 ;  if v  =  5 then pfhline 14 7 17 off

	LDA v
	CMP #5
     BNE .skipL0105
.condpart27
	LDX #1
	LDA #17
	STA temp3
	LDY #7
	LDA #14
 jsr pfhline
.skipL0105
.L0106 ;  if v  =  4 then pfhline 14 6 17 off

	LDA v
	CMP #4
     BNE .skipL0106
.condpart28
	LDX #1
	LDA #17
	STA temp3
	LDY #6
	LDA #14
 jsr pfhline
.skipL0106
.L0107 ;  if v  =  3 then pfhline 14 5 17 off

	LDA v
	CMP #3
     BNE .skipL0107
.condpart29
	LDX #1
	LDA #17
	STA temp3
	LDY #5
	LDA #14
 jsr pfhline
.skipL0107
.L0108 ;  if v  =  2 then pfhline 14 4 17 off

	LDA v
	CMP #2
     BNE .skipL0108
.condpart30
	LDX #1
	LDA #17
	STA temp3
	LDY #4
	LDA #14
 jsr pfhline
.skipL0108
.L0109 ;  if v  =  1 then pfhline 14 3 17 off

	LDA v
	CMP #1
     BNE .skipL0109
.condpart31
	LDX #1
	LDA #17
	STA temp3
	LDY #3
	LDA #14
 jsr pfhline
.skipL0109
.L0110 ;  goto after3

 jmp .after3

.
 ; 

.
 ; 

.__down0on
 ; __down0on

.L0111 ;  COLUPF  =  218

	LDA #218
	STA COLUPF
.L0112 ;  AUDV1  =  d

	LDA d
	STA AUDV1
.L0113 ;  AUDC1  =  8

	LDA #8
	STA AUDC1
.L0114 ;  AUDF1  =  15  -  d

	LDA #15
	SEC
	SBC d
	STA AUDF1
.
 ; 

.L0115 ;  if d  >  0 then d  =  d  -  1

	LDA #0
	CMP d
     BCS .skipL0115
.condpart32
	DEC d
.skipL0115
.
 ; 

.
 ; 

.L0116 ;  pfhline 20 9 23 on

	LDX #0
	LDA #23
	STA temp3
	LDY #9
	LDA #20
 jsr pfhline
.L0117 ;  pfhline 20 8 23 on

	LDX #0
	LDA #23
	STA temp3
	LDY #8
	LDA #20
 jsr pfhline
.L0118 ;  pfhline 20 7 23 on

	LDX #0
	LDA #23
	STA temp3
	LDY #7
	LDA #20
 jsr pfhline
.L0119 ;  pfhline 20 6 23 on

	LDX #0
	LDA #23
	STA temp3
	LDY #6
	LDA #20
 jsr pfhline
.L0120 ;  pfhline 20 5 23 on

	LDX #0
	LDA #23
	STA temp3
	LDY #5
	LDA #20
 jsr pfhline
.L0121 ;  pfhline 20 4 23 on

	LDX #0
	LDA #23
	STA temp3
	LDY #4
	LDA #20
 jsr pfhline
.L0122 ;  pfhline 20 3 23 on

	LDX #0
	LDA #23
	STA temp3
	LDY #3
	LDA #20
 jsr pfhline
.
 ; 

.L0123 ;  w  =  0

	LDA #0
	STA w
.
 ; 

.L0124 ;  goto after4

 jmp .after4

.
 ; 

.__down0off
 ; __down0off

.L0125 ;  d  =  5

	LDA #5
	STA d
.L0126 ;  if w  <  7 then w  =  w  +  1

	LDA w
	CMP #7
     BCS .skipL0126
.condpart33
	INC w
.skipL0126
.L0127 ;  if w  =  7 then pfhline 20 9 23 off

	LDA w
	CMP #7
     BNE .skipL0127
.condpart34
	LDX #1
	LDA #23
	STA temp3
	LDY #9
	LDA #20
 jsr pfhline
.skipL0127
.L0128 ;  if w  =  6 then pfhline 20 8 23 off

	LDA w
	CMP #6
     BNE .skipL0128
.condpart35
	LDX #1
	LDA #23
	STA temp3
	LDY #8
	LDA #20
 jsr pfhline
.skipL0128
.L0129 ;  if w  =  5 then pfhline 20 7 23 off

	LDA w
	CMP #5
     BNE .skipL0129
.condpart36
	LDX #1
	LDA #23
	STA temp3
	LDY #7
	LDA #20
 jsr pfhline
.skipL0129
.L0130 ;  if w  =  4 then pfhline 20 6 23 off

	LDA w
	CMP #4
     BNE .skipL0130
.condpart37
	LDX #1
	LDA #23
	STA temp3
	LDY #6
	LDA #20
 jsr pfhline
.skipL0130
.L0131 ;  if w  =  3 then pfhline 20 5 23 off

	LDA w
	CMP #3
     BNE .skipL0131
.condpart38
	LDX #1
	LDA #23
	STA temp3
	LDY #5
	LDA #20
 jsr pfhline
.skipL0131
.L0132 ;  if w  =  2 then pfhline 20 4 23 off

	LDA w
	CMP #2
     BNE .skipL0132
.condpart39
	LDX #1
	LDA #23
	STA temp3
	LDY #4
	LDA #20
 jsr pfhline
.skipL0132
.L0133 ;  if w  =  1 then pfhline 20 3 23 off

	LDA w
	CMP #1
     BNE .skipL0133
.condpart40
	LDX #1
	LDA #23
	STA temp3
	LDY #3
	LDA #20
 jsr pfhline
.skipL0133
.L0134 ;  goto after4
 jmp .after4
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL012_0
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %11001110
	.byte  %01001000
	.byte  %01001000
	.byte  %01001000
	.byte  %11101110
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL013_1
	.byte  %00001001
	.byte  %00001001
	.byte  %00001001
	.byte  %11101011
	.byte  %00100000
	.byte  %11100000
	.byte  %10000000
	.byte  %11100000
 if ECHOFIRST
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 endif 
ECHOFIRST = 1
 
 
 
; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC-8
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 64
       ORG  $10F80-bscode_length
       RORG $1FF80-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif

; font equates
.21stcentury = 1
alarmclock = 2     
handwritten = 3    
interrupted = 4    
retroputer = 5    
whimsey = 6
tiny = 7

scoretable

 ifconst font
  if font == .21stcentury
    include "score_graphics.asm.21stcentury"
  endif
  if font == alarmclock
    include "score_graphics.asm.alarmclock"
  endif
  if font == handwritten
    include "score_graphics.asm.handwritten"
  endif
  if font == interrupted
    include "score_graphics.asm.interrupted"
  endif
  if font == retroputer
    include "score_graphics.asm.retroputer"
  endif
  if font == whimsey
    include "score_graphics.asm.whimsey"
  endif
  if font == tiny
    include "score_graphics.asm.tiny"
  endif
 else ; default font

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 

       ifnconst DPC_kernel_options
 
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000 

       endif

 endif

 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 64
       ORG  $10FE0-bscode_length
       RORG $1FFE0-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
   if bankswitch == 64
     ORG  $10FF0
     RORG $1FFF0
     lda $ffe0 ; we use wasted space to assist stella with EF format auto-detection
     ORG  $10FF8
     RORG $1FFF8
     ifconst superchip 
       .byte "E","F","S","C"
     else
       .byte "E","F","E","F"
     endif
     ORG  $10FFC
     RORG $1FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word start
 .word start
