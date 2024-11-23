.var music = LoadSid("../music/tubular.sid")

.const maxCrunch = 5
.const flipAfter = 20
#define postponeUpdate
//#define debug
#define showBorder

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

.label screen2     = $0c00  // this is a nice spot, since we copy everything to here
.label bitmap2     = $2000
.label bitmap1     = $4000
.label empty       = $6000
.label screen1     = $6800
.label sprites     = $6c00

.label code        = $8000

.label bitmap3     = $c000
.label screen3     = $e000
.label d800Colors  = $e800 // has to be $400 aligned

// these are the demo-spanning 0 page adresses!
.label nextpart = $02
.label timelow  = $03
.label timehigh = $04

.label loader0 = $05
.label loader1 = $06
.label loader2 = $07
.label loader3 = $08
.label loader4 = $09

// these are the 0 page adresses for the part

.label linecrunch = $a0
.label atemp      = $a1
.label xtemp      = $a2
.label ytemp      = $a3
.label vicbank    = $a4
.label sinePhase  = $a5
.label low        = $a6
.label high       = $a7
.label low2       = $a8
.label high2      = $a9
.label low3       = $aa
.label high3      = $ab


#if AS_SPINDLE_PART
  .var PLAY=$1003
  .label spindleLoadAddress = $4000
  *=spindleLoadAddress-18-11-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word prepare       // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word bottomIRQ.MusicPlayCall // location of playroutine call

    .byte 'S'                            // this part is IO safe and can load below $d000
    .byte 'X'                            // not enough raster left for loading..
    // screen2 + bitmap 2 is used at runtime
    .byte 'P', >screen2, >(screen2+$3ff)    // we use the screen memory at runtime    
    .byte 'P', >bitmap2, >(bitmap2+$1f40)   // we use the screen memory at runtime   

    .byte 'Z', $a0, $ab


    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart


#else    
    :BasicUpstart2(start)
#endif


* = code "[CODE] main"
start:
{
  sei

  lda $01
  sta restore01

  lda #$35
  sta $01

  // -----------------------------------------------------------------
  // copied from spindle earlysetup to get the same cia timers running
  // -----------------------------------------------------------------

	bit	$d011
	bmi	*-3

	bit	$d011
	bpl	*-3

	lda	#RTI
	sta	$ffff
	ldx	#$ff
	stx	$fffa
	stx	$fffb
	inx
	stx	$dd0e
	stx	$dd04
	stx	$dd05
	lda	#$81
	sta	$dd0d
	lda	#$19
	sta	$dd0e

	ldx	$d012
	inx
resync:
	cpx	$d012
	bne	*-3
	// at cycle 4 or later
	ldy	#0		 // 4
	sty	$dc07	 // 6
	lda	#62		 // 10
	sta	$dc06	 // 12
	iny			   // 16
	sty	$d01a	 // 18
	dey			   // 22
	dey			   // 24
	sty	$dc02	 // 26
	nop 			 // 30
	nop 			 // 32
	nop 			 // 34
	nop 			 // 36
	nop 			 // 38
	nop 			 // 40
	nop 			 // 42
	nop 			 // 44
	nop	  		 // 46
	lda	#$11	 // 48
	sta	$dc0f  // 50
	txa			   // 54
	inx			   // 56
	inx			   // 58
	cmp	$d012	 // 60	still on the same line?
	bne	resync

  // ----------------------------------
  // cia timers should be running now..
  // ----------------------------------

  #if !AS_SPINDLE_PART
    jsr prepare
  #endif

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d

  lda #<bottomIRQ
  sta $fffe
  lda #>bottomIRQ
  sta $ffff
  lda #$fa
  sta $d012

  lda #$3f
  sta $d011

  lda #0
  //sta scrollMove.direction  
  sta sinePhase
  
  lda #$0
  sta linecrunch

  lda #1
  sta vicbank       // show bitmap1

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
    lda #0
    jsr music.init
  #endif

  #if showBorder
    lda #$0f
  #else
    lda #$09
  #endif

  sta $d020
  lda #$09
  sta $d021
  lda #$d8
  sta $d016
  lda #$01
  sta $dd02
  lda #$90
  sta $d018
  
  lda #$00
  sta $7fff

  jsr setSprites

  lda $dc0d
  lda $dd0d
  asl $d019

  lda restore01: #0
  cmp #$37
  beq !+
  sta $01
!:
  cli

  #if !AS_SPINDLE_PART
mainloop:
    inc $d020
    jmp mainloop
  #else
    rts
  #endif
}

dummy:
  .byte 0
d011tab:
  .byte $1c,$1b,$1a,$19,$18,$1f
d011tab2:
  .byte $3d,$3c,$3b,$3a,$39,$38
d012tab:
  .byte $34,$33,$32,$31,$30,$2f

waste37:
  lda waste36: #$a9
waste35:
  lda waste34: #$a9
waste33:
  lda waste32: #$a9
waste31:
  lda waste30: #$a9
waste29:
  lda waste28: #$a9
waste27:
  lda waste26: #$a9
waste25:
  lda waste24: #$a9
waste23:
  lda waste22: #$a9
waste21:
  lda waste20: #$a9
waste19:
  lda waste18: #$a9
waste17:
  lda waste16: #$a9
waste15:
  bit waste14: $ea
waste12:
  rts

prepare:
{
  ldx #0
loop:
  lda d800Colors,x
  sta $d800,x
  lda d800Colors+$100,x
  sta $d800+$100,x
  lda d800Colors+$200,x
  sta $d800+$200,x
  lda d800Colors+$300,x
  sta $d800+$300,x
  inx
  bne loop

  //jsr modifyToUp
  jsr hideBugs
  jsr prepBitmap2and3
  jsr prepBitmaps    // prepare bitmaps 2+3
  jsr prepScreens
  rts
}

.align $100

linecrunchirq:
{
  // Jitter correction. Put earliest cycle in parenthesis.
  // (10 with no sprites, 19 with all sprites, ...)
  // Length of clockslide can be increased if more jitter
  // is expected, e.g. due to NMIs.

  dec 0            // 10..18
  sta savea        // 15..23
  lda #39-(10)     // 19..27 <- (earliest cycle)
  sec              // 21..29
  sbc $dc06        // 23..31, A becomes 0..8
  sta *+4          // 27..35
  bpl *+2          // 31..39
  lda #$a9         // 34
  lda #$a9         // 36
  lda #$a9         // 38
  lda $eaa5        // 40
                   // at cycle 34+(10) = 44
  // we would have 15 cycles until needed sta $d011 and have to update :
  // x     (3 cycles)
  // $ffff (6 cycles)

  stx savex
  sty savey

  ldx.abs linecrunch
  lda d011tab,x

  // we are at cycle 59/$3b

  sta $d011
  lda #0
  sta $ffff

  lda spritesOn: #0
  sta $d015

  ldy sinePhase  // 3
  lda sineX,y    // 4
  sta $d016      // 4

  nop
  inc dummy
  inc dummy

  dex
  bmi stoploop

loop:
  lda d011tab,x
  jsr waste12
  sta $d011
  jsr waste36
  dex
  nop
  bpl loop
  nop

stoploop:
  lda nextD012: #$5e
  sta $d012
  dec $d011
  nop

  // with and without sprites we are at cycle 53 of raster $35

  lda #$28          
  sec
  sbc xoffset: #0   // 0 = 15 cycles
  sta dmaDelay+1
dmaDelay:
  bcs *             // 12
  lda #$a9  //0
  lda #$a9  //2
  lda #$a9  //4
  lda #$a9  //6
  lda #$a9  //8
  lda #$a9  //10
  lda #$a9  //12
  lda #$a9  //14
  lda #$a9  //16
  lda #$a9  //18
  lda #$a9  //20
  lda #$a9  //22
  lda #$a9  //24
  lda #$a9  //26
  lda #$a9  //28
  lda #$a9  //30
  lda #$a9  //32
  lda #$a9  //34
  lda #$a9  //36
  lda #$24  //38
  nop       //40   40 = 2 cycles

  lda d011Write: #$1e   // go to char or bitmapmode
  sta $d011
  inc $d011

  ldx vicbank
dd02Write:
  ldx $dd02
  lda d018tab,x
d018Write:
  lda $d018

  lda nextIRQl: #<toBitmapIrq
  sta $fffe
  lda nextIRQh: #>toBitmapIrq
  sta $ffff
  asl $d019

  inc 0
  lda $01
  sta save01
  cli

  jsr copyLine

  lda save01: #0
  sta $01

  ldy savey: #0
  ldx savex: #0
  lda savea: #0
  rti  
}

d018tab:
  .byte $38,$a0,$00,$80

toBitmapIrq:
{
  sta savea
  stx savex
  sty savey

  lda $01
  sta save01

  lda #$35
  sta $01
 
  // cleanly go to bitmap mode again
  lda $d011
  ora #$20
  tay
  ldx vicbank
  lda d018tab,x
  stx $dd02
  sta $d018
  sty $d011

  lda #<bottomIRQ
  sta $fffe
  lda #>bottomIRQ
  sta $ffff
  lda #$fa
  sta $d012
  asl $d019

  lda save01: #0
  sta $01
  lda savea: #0
  ldx savex: #0
  ldy savey: #0
  rti
}

jumpTable:
  .word copyLineBitmap20, copyLineBitmap10, $0000, copyLineBitmap30
  .word copyLineBitmap21, copyLineBitmap11, $0000, copyLineBitmap31
  .word copyLineBitmap22, copyLineBitmap12, $0000, copyLineBitmap32
  .word copyLineBitmap23, copyLineBitmap13, $0000, copyLineBitmap33
  .word copyLineBitmap24, copyLineBitmap14, $0000, copyLineBitmap34
  .word copyLineBitmap25, copyLineBitmap15, $0000, copyLineBitmap35

bottomIRQ:
{
  pha
  lda $01
  pha

  lda #$35
  sta $01

  lda #$37    // open border. this is needed to show the lines at the bottom
  sta $d011

  txa
  pha
  tya
  pha

  lda $3fff
  pha

  lda #0      // hide bugs. 
  sta $3fff   // todo : this might give a visible bug in the bitmap and we should time this exactly!! this is a few rasters too high!!
  sta $ffff
  sta $d015
  
  jsr scriptIt

  ldx #$7f
  lda bottomIRQ.charOrBitmap
  beq !+
  ldx #$00
!:
  stx linecrunchirq.spritesOn

  // go to char (0) or bitmap (1) mode
  ldx charOrBitmap: #1
  lda modCharOrBitmap,x
  sta linecrunchirq.d011Write
  lda modCharOrBitmap+2,x
  sta linecrunchirq.dd02Write
  lda modCharOrBitmap+4,x
  sta linecrunchirq.d018Write
  lda modCharOrBitmap+6,x
  sta linecrunchirq.nextD012
  lda modCharOrBitmap+8,x
  sta linecrunchirq.nextIRQl
  lda modCharOrBitmap+10,x
  sta linecrunchirq.nextIRQh

  // switching between up and down :
  // -> if we are at the max offset for a certain direction, just before we switch to a new bitmap 
  //    scrollup   : linecrunch == maxCrunch
  //    scrolldown : linecrunch == 0
  // then we can switch direction and start scrolling the other way

  MusicPlayCall:
  #if !AS_SPINDLE_PART
    jsr music.play
  #else         
    bit.abs $0000
  #endif

  jsr scrollMove

  // x movement
  lda sinePhase
  clc
  adc #1
  and #$7f
  sta sinePhase
  tax
  lda sineX+128,x
  sta linecrunchirq.xoffset

  // y movement
  ldx linecrunch
  lda d012tab,x
  sta $d012
  lda d011tab2,x
  sta $d011

  lda #$88     // hide bugs..
  sta $d018
  lda #1
  sta $dd02

  lda #<linecrunchirq
  sta $fffe
  lda #>linecrunchirq
  sta $ffff
  asl $d019

  // restore $3fff
  pla
  sta $3fff
  
  cli
  jsr scrollCopy

  pla
  tay
  pla
  tax

  pla
  sta $01

  pla
  rti
}

modCharOrBitmap:
  .byte $1e,$3e,LDX_ABS,STX_ABS,LDA_ABS,STA_ABS,$66,$fa,<toBitmapIrq,<bottomIRQ,>toBitmapIrq,>bottomIRQ

scriptIt:
{
  dec wait
  bne endScript

  ldx scriptPointer: #0
  lda script,x
  beq endPart

  sta wait

  lda bottomIRQ.charOrBitmap
  eor #1
  sta bottomIRQ.charOrBitmap

  inc scriptPointer
endScript:
  rts

endPart:
  inc nextpart
  rts

wait:
  .byte 1
}

.var Wait = 1
.var End  = $ff

script:
  .byte 128, 2,8, 8,8, 6,6, 5,5, 4,4, 3,3, 2,2, 1,1, 1,1, 1,0
  .byte Wait,255
  .byte End

setSprites:
{
  lda #$7f
  sta $d015

  ldx #((sprites&$3fff)/64)
  stx empty+$3f8
  inx
  stx empty+$3f9
  inx
  stx empty+$3fa
  inx
  stx empty+$3fb
  inx
  stx empty+$3fc
  inx
  stx empty+$3fd
  inx
  stx empty+$3fe
  lda #$1f
  sta $d000
  lda #$4f
  sta $d002
  lda #$7f
  sta $d004
  lda #$af
  sta $d006
  lda #$df
  sta $d008
  lda #$0f
  sta $d00a
  lda #$3f
  sta $d00c

  lda #$60
  sta $d010

  lda #$36
  sta $d001
  sta $d003
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d

  lda #$00
  sta $d01c
  lda #$ff
  sta $d017
  sta $d01d

  lda #$f
  sta $d027
  sta $d028
  sta $d029
  sta $d02a
  sta $d02b
  sta $d02c
  sta $d02d

  rts
}

scrollMove:
{
  //lda direction: #0
  //bne scrollDown

scrollUp:
  lda linecrunch   // 0, 1, 2, 3, 4, 5 -> 0
  clc
  adc #1
  cmp #maxCrunch+1
  bcc !++
  // flip?
  //dec flipIt
  //bne noFlip
  //lda #1
  //sta direction
  //rts

noFlip:
  // keep track of where $d800 colors come from

  lda topLine
  clc
  adc #6
  cmp #25
  bcc !+
  sec
  sbc #25
!:
  sta topLine

  //lda topD800
  //clc
  //adc #6*40
  //sta topD800
  //lda topD800+1
  //adc #0
  //and #3
  //sta topD800+1

  // switch to the other bitmap
  ldx vicbank
  lda nextBank,x
  sta vicbank
  inc scrollCopy.copyUp
  lda #0
!:
  sta linecrunch
  rts

/*
scrollDown:
  dec linecrunch  // 5, 4, 3, 2, 1, 0 -> 5
  bpl !+
  // flip?
  //dec flipIt
  //bne noFlip2
  //lda #0
  //sta direction
  //sta linecrunch
  //rts

noFlip2:
  // keep track of where $d800 colors come from
  lda topLine
  sec
  sbc #6
  bpl !+
  clc
  adc #25
!:
  sta topLine

  // switch to the other bitmap
  // bitmap 1 = $4000, vicbank =1
  // bitmap 2 = $c000, vicbank =3
  // bitmap 3 = $2000, vicbank =0
  ldx vicbank
  lda nextBank,x
  sta vicbank
  inc scrollCopy.copyDown
  lda #maxCrunch
  sta linecrunch
!:
  rts
*/
}

nextBank:
  .byte 3,0,$ff,1  // bank1->bank0, bank0->bank3, bank3->bank1

//flipIt:
//  .byte flipAfter

// keep track of top $d800 position (where do the colors come from for the first $d800 row?)
//topD800:
//  .byte >$0000,<$0000  // f0 = first move up
topLine:
  .byte 0

scrollCopy:
{
  //lda scrollMove.direction
  //bne scrollDown

  //lda flipIt
  //bne !+
  //jsr modifyToUp
  //lda #flipAfter
  //sta flipIt
  //bne onlyBitmapUp

  // if we flip, we do not have to copy $d800, but we do have to modify the screen and bitmap!!

!:
  lda copyUp: #0
  beq endCopy
  lda #0
  sta copyUp
  jsr copyD800Up
onlyBitmapUp:
  #if debug
    lda #7
    sta $d020
  #endif
  jmp copyBitmap
endCopy:
  rts
/*
scrollDown:
  lda flipIt
  bne !+
  jsr modifyToDown
  lda #flipAfter
  sta flipIt
  bne onlyBitmapDown
!:
  lda copyDown: #0
  beq endCopy
  lda #0
  sta copyDown
  jsr copyD800Down
onlyBitmapDown:
  #if debug
    lda #7
    sta $d020
  #endif
  jmp copyBitmap
*/
}

// turn copyBitmap into copyBitmapUp
/*
modifyToUp:
{
  lda #>(screen2 + ((-6*40)&$3ff))
  sta modify.writehigh3
  and #$fc
  sta modify.screenHigh1
  sta modify.screenHigh2
  lda #<(screen2 + ((-6*40)&$3ff))
  sta modify.writelow3

  lda #>(bitmap2 + ((-6*320)&$1fff))
  ldx #>(bitmap1 + ((-6*320)&$1fff))
  bne modify
}
*/

// turn copyBitmap into copyBitmapDown
/*
modifyToDown:
{
  lda #>(screen2 + ((6*40)&$3ff))
  sta modify.writehigh3
  and #$fc
  sta modify.screenHigh1
  sta modify.screenHigh2
  lda #<(screen2 + ((6*40)&$3ff))
  sta modify.writelow3

  lda #>(bitmap2 + ((6*320)&$1fff))
  ldx #>(bitmap1 + ((6*320)&$1fff))
}
modify:  
{
  sta writehigh
  stx writehigh2

  lda #<(copyBitmap1a.loop+4)
  sta low
  lda #>(copyBitmap1a.loop+4)
  sta high
  lda #<(copyBitmap2a.loop+4)
  sta low2
  lda #>(copyBitmap2a.loop+4)
  sta high2

  ldx #(128/4)-1
  clc
loop:

  // actually the low byte does not have to be updated...
  //ldy #0
  //lda #$80
  //sta (low),y
  //ldy #6
  //lda #$c0
  //sta (low),y  
  //ldy #12
  //lda #$00
  //sta (low),y  
  //ldy #18
  //lda #$40
  //sta (low),y  
  
  ldy #1
  lda writehigh: #>(bitmap2 + ((6*320)&$1fff))
  sta (low),y
  ldy #7
  sta (low),y
  ldy #13
  adc #1
  and #$1f
  ora #>bitmap2
  sta (low),y
  ldy #19
  sta (low),y
  sta writehigh

  ldy #1
  lda writehigh2: #>(bitmap1 + ((6*320)&$1fff))
  sta (low2),y
  ldy #7
  sta (low2),y
  ldy #13
  adc #1
  and #$1f
  ora #>bitmap1
  sta (low2),y
  ldy #19
  sta (low2),y
  sta writehigh2

  // next 4

  lda low
  adc #6*4
  sta low
  bcc !+
  inc high
  clc
!:
  lda low2
  adc #6*4
  sta low2
  bcc !+
  inc high2
  clc
!:
  dex
  bpl loop
  
  // modify copyScreen

  lda #(128/2)
  sta flipsLeft

  lda #<(copyScreen1.loop+4)
  sta low
  lda #>(copyScreen1.loop+4)
  sta high

  lda #<(copyScreen2.loop+4)
  sta low2
  lda #>(copyScreen2.loop+4)
  sta high2
  ldx writehigh3: #0

flipScreenLoop:
  ldy #0
  lda writelow3:  #0
  sta (low),y
  sta (low2),y
  adc #8
  sta writelow4

  iny
  txa
  sta (low),y
  eor #>(screen1^screen2)
  sta (low2),y
  bcc !+
  inx
  txa
  and #3
  ora screenHigh1: #>screen2
  tax
  clc
!:
  ldy #6
  lda writelow4: #0
  sta (low),y
  sta (low2),y
  adc #8
  sta writelow3

  iny
  txa
  sta (low),y
  eor #>(screen1^screen2)
  sta (low2),y
  bcc !+
  inx
  txa
  and #3
  ora screenHigh2: #>screen2
  tax
  clc
!:
  lda low
  adc #12
  sta low
  bcc !+
  inc high
  clc
!:
  lda low2
  adc #12
  sta low2
  bcc !+
  inc high2
  clc
!:

  dec flipsLeft
  bne flipScreenLoop

  rts
flipsLeft:
  .byte 0
}
*/

// if we keep the bitmap fixed at 25 lines, we can copy a line into a different part of the bitmap
// we have to copy the line after it has been displayed at the top! say in the middle of the screen
// frame 0 : copy line 0 ($0000-$013f) -> $1f40-$1fff + $0000-$0080  + 6 higher in bitmap 2 + 6 higher in bitmap 3
// frmae 1 : copy line 1 ($0140-$027f) -> $0080+$01c0
// etc..

copyLine:
{
  // calculate first copy from
  lda linecrunch
  asl
  asl
  ora vicbank
  asl
  adc #<jumpTable
  sta indJump
  lda #>jumpTable
  adc #0
  sta indJump+1

  lda #$35
  sta $01
  ldx #7

  jmp indJump: (jumpTable)
}

.macro acopyALine(bitmap,i)
{
  .var bitmapa = bitmap1
  .var bitmapb = bitmap2
  .var bitmapc = bitmap3
  .var screena = screen1
  .var screenb = screen2
  .var screenc = screen3
  
  .if (bitmap==2)
  {
    .eval bitmapa = bitmap2
    .eval bitmapb = bitmap3
    .eval bitmapc = bitmap1
    .eval screena = screen2
    .eval screenb = screen3
    .eval screenc = screen1
  }

  .if (bitmap==3)
  {
    .eval bitmapa = bitmap3
    .eval bitmapb = bitmap1
    .eval bitmapc = bitmap2
    .eval screena = screen3
    .eval screenb = screen1
    .eval screenc = screen2
  }

loop2:
  lda screena+$0000+i*$28,x
  sta screena+(($3e8- 0*40+i*$28)&$3ff),x
  .if (i>0)  // we update the top line by a copy in the background
  {
    sta screenb+(($3e8- 6*40+i*$28)&$3ff),x
    sta screenc+(($3e8-12*40+i*$28)&$3ff),x
  }
  lda screena+$0008+i*$28,x
  sta screena+(($3f0- 0*40+i*$28)&$3ff),x
  .if (i>0)  // we update the top line by a copy in the background
  {
    sta screenb+(($3f0- 6*40+i*$28)&$3ff),x
    sta screenc+(($3f0-12*40+i*$28)&$3ff),x
  }
  lda screena+$0010+i*$28,x
  sta screena+(($3f8- 0*40+i*$28)&$3ff),x
  .if (i>0)  // we update the top line by a copy in the background
  {
    sta screenb+(($3f8- 6*40+i*$28)&$3ff),x
    sta screenc+(($3f8-12*40+i*$28)&$3ff),x
  }
  lda screena+$0018+i*$28,x
  sta screena+(($000- 0*40+i*$28)&$3ff),x
  .if (i>0)  // we update the top line by a copy in the background
  {
    sta screenb+(($000- 6*40+i*$28)&$3ff),x
    sta screenc+(($000-12*40+i*$28)&$3ff),x
  }
  lda screena+$0020+i*$28,x
  sta screena+(($008- 0*40+i*$28)&$3ff),x
  .if (i>0)  // we update the top line by a copy in the background
  {
    sta screenb+(($008- 6*40+i*$28)&$3ff),x
    sta screenc+(($008-12*40+i*$28)&$3ff),x
  }

  lda $d800+$0000 +i*$28,x
  sta $d800+(($3e8+i*$28)&$3ff),x
  lda $d800+$0008 +i*$28,x
  sta $d800+(($3f0+i*$28)&$3ff),x
  lda $d800+$0010 +i*$28,x
  sta $d800+(($3f8+i*$28)&$3ff),x
  lda $d800+$0018 +i*$28,x
  sta $d800+(($000+i*$28)&$3ff),x
  lda $d800+$0020 +i*$28,x
  sta $d800+(($008+i*$28)&$3ff),x

  dex
  bpl loop2

  lda #$34
  sta $01

  ldx #$1f

loop:
  lda bitmapa+$0000+i*$140,x
  sta bitmapa+(($1f40- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($1f40- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1f40-12*320+i*$140)&$1fff),x
  }
  lda bitmapa+$0020+i*$140,x
  sta bitmapa+(($1f60- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($1f60- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1f60-12*320+i*$140)&$1fff),x
  }

  lda bitmapa+$0040+i*$140,x
  sta bitmapa+(($1f80- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($1f80- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1f80-12*320+i*$140)&$1fff),x
  }
  lda bitmapa+$0060+i*$140,x
  sta bitmapa+(($1fa0- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($1fa0- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1fa0-12*320+i*$140)&$1fff),x
  }

  lda bitmapa+$0080+i*$140,x
  sta bitmapa+(($1fc0- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($1fc0- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1fc0-12*320+i*$140)&$1fff),x
  }
  lda bitmapa+$00a0+i*$140,x
  sta bitmapa+(($1fe0- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($1fe0- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1fe0-12*320+i*$140)&$1fff),x
  }

  lda bitmapa+$00c0+i*$140,x
  sta bitmapa+(($0000- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($0000- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($0000-12*320+i*$140)&$1fff),x
  }
  lda bitmapa+$00e0+i*$140,x
  sta bitmapa+(($0020- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($0020- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($0020-12*320+i*$140)&$1fff),x
  }

  lda bitmapa+$0100+i*$140,x
  sta bitmapa+(($0040- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($0040- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($0040-12*320+i*$140)&$1fff),x
  }
  lda bitmapa+$0120+i*$140,x
  sta bitmapa+(($0060- 0*320+i*$140)&$1fff),x
    .if (i>0)  // we update the top line by a copy in the background
  {
    sta bitmapb+(($0060- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($0060-12*320+i*$140)&$1fff),x
  }

  dex
  bpl loop

  rts
}

* = * "[CODE] CopyLine"
copyLineBitmap10:
  acopyALine(1,0)
copyLineBitmap11:
  acopyALine(1,1)
copyLineBitmap12:
  acopyALine(1,2)
copyLineBitmap13:
  acopyALine(1,3)
copyLineBitmap14:
  acopyALine(1,4)
copyLineBitmap15:
  acopyALine(1,5)
copyLineBitmap20:
  acopyALine(2,0)
copyLineBitmap21:
  acopyALine(2,1)
copyLineBitmap22:
  acopyALine(2,2)
copyLineBitmap23:
  acopyALine(2,3)
copyLineBitmap24:
  acopyALine(2,4)
copyLineBitmap25:
  acopyALine(2,5)
copyLineBitmap30:
  acopyALine(3,0)
copyLineBitmap31:
  acopyALine(3,1)
copyLineBitmap32:
  acopyALine(3,2)
copyLineBitmap33:
  acopyALine(3,3)
copyLineBitmap34:
  acopyALine(3,4)
copyLineBitmap35:
  acopyALine(3,5)

bitmaptab:
  .byte >bitmap2,>bitmap1,$00,>bitmap3
bitmaptab2:
  .byte >bitmap3,>bitmap2,$00,>bitmap1
bitmaptab3:
  .byte >bitmap1,>bitmap3,$00,>bitmap2

copyBitmap:
  lda vicbank
  cmp #1
  bne !+
  jsr copyBitmap1a
  jmp copyScreen1
!:
  cmp #0
  bne !+
  jsr copyBitmap2a
  jmp copyScreen2
!:
  jsr copyBitmap3a
  jmp copyScreen3

xStart:
  .byte $3f  // $3f->$1f->$3f->$1f

prepBitmaps:
{
  lda #$34
  sta $01

  ldx #$3f
  lda #$00
  sta copyBitmap1a.xEnd
  beq copyBitmap1a.loop
}
* = * "[CODE] copyBitmap1a"
copyBitmap1a:
{
  lda #$34
  sta $01

  #if postponeUpdate
    // because of the optimization of not coyping to $1f40 during the frame update, we do have to copy stripe 125,126,127,0 (=128) and 1 (= 129)
    ldx #$3f
    loop3:
      .for (var stripe=125; stripe<130; stripe++)
      {
        lda bitmap1 + ((  0*320+stripe*64)&$1fff),x  // 125*64 = $1f40
        sta bitmap2 + (( -6*320+stripe*64)&$1fff),x
        sta bitmap3 + ((-12*320+stripe*64)&$1fff),x
      }
    dex
    bpl loop3
  #endif

  lax xStart
  sec
  sbc #$1f
  sta xEnd
loop:
  // stripes 0-26 en 125-127 hoeven we niet te kopiëren
  // 1f40-1fff is not needed
  // 1f40+6*320 = 26c0 = 6c0 = stripe $1b,$1c,$1d = 27,28,29

  // line 6 = 6*320 = 1920
  // start stripe = 1920/64 = 30
  // end stripe = line 25 - 1 = 25*350/64 - 1 = 124

  .for (var stripe=30; stripe<125; stripe++)  
  {
    lda bitmap1 + ((  0*320+stripe*64)&$1fff),x
    .if (stripe< 95) sta bitmap2 + (( -6*320+stripe*64)&$1fff),x
    .if (stripe>=60) sta bitmap3 + ((-12*320+stripe*64)&$1fff),x
  }

  cpx xEnd: #0
  beq endloop
  dex
  jmp loop
endloop:
  lda xStart
  eor #$20
  sta xStart

  lda #$35
  sta $01
  rts
}

* = * "[CODE] copyBitmap2a"
copyBitmap2a:
{
  lda #$34
  sta $01

  #if postponeUpdate
    // because of the optization of not coyping to $1f40 during the frame update, we do have to copy stripe 125,126,127,0 (=128) and 1 (= 129)
    ldx #$3f
    loop3:
      .for (var stripe=125; stripe<130; stripe++)
      {
        lda bitmap2 + ((  0*320+stripe*64)&$1fff),x  // 125*64 = $1f40
        sta bitmap3 + (( -6*320+stripe*64)&$1fff),x
        sta bitmap1 + ((-12*320+stripe*64)&$1fff),x
      }
    dex
    bpl loop3
  #endif

  lax xStart
  sec
  sbc #$1f
  sta xEnd
loop:
  // stripes 0-26 en 125-127 hoeven we niet te kopiëren
  .for (var stripe=30; stripe<125; stripe++)
  {
    lda bitmap2 + ((  0*320+stripe*64)&$1fff),x
    .if (stripe< 95) sta bitmap3 + (( -6*320+stripe*64)&$1fff),x
    .if (stripe>=60) sta bitmap1 + ((-12*320+stripe*64)&$1fff),x
  }

  cpx xEnd: #0
  beq endloop
  dex
  jmp loop
endloop:
  lda xStart
  eor #$20
  sta xStart

  lda #$35
  sta $01
  rts
}

* = * "[CODE] copyBitmap3a"
copyBitmap3a:
{
  lda #$34
  sta $01

  #if postponeUpdate
    // because of the optization of not coyping to $1f40 during the frame update, we do have to copy stripe 125,126,127,0 (=128) and 1 (= 129)
    ldx #$3f
    loop3:
      .for (var stripe=125; stripe<130; stripe++)
      {
        lda bitmap3 + ((  0*320+stripe*64)&$1fff),x  // 125*64 = $1f40
        sta bitmap1 + (( -6*320+stripe*64)&$1fff),x
        sta bitmap2 + ((-12*320+stripe*64)&$1fff),x
      }
    dex
    bpl loop3
  #endif

  lax xStart
  sec
  sbc #$1f
  sta xEnd
loop:
  // stripes 0-26 en 125-127 hoeven we niet te kopiëren
  .for (var stripe=30; stripe<125; stripe++)
  {
    lda bitmap3 + ((  0*320+stripe*64)&$1fff),x
    .if (stripe< 95) sta bitmap1 + (( -6*320+stripe*64)&$1fff),x
    .if (stripe>=60) sta bitmap2 + ((-12*320+stripe*64)&$1fff),x
  }

  cpx xEnd: #0
  beq endloop
  dex
  jmp loop
endloop:
  lda xStart
  eor #$20
  sta xStart

  lda #$35
  sta $01
  rts
}

xStart2:
  .byte $7

prepScreens:
  ldx #7
  lda #0
  sta copyScreen1.xEnd
  beq copyScreen1.loop

* = * "[CODE] copyScreen1Up"
copyScreen1:
{
  #if postponeUpdate
    // because of the optization of not coyping to $1f40 during the frame update, we do have to copy stripe 125,126,127,0 (=128) and 1 (= 129)
    ldx #$07
    loop3:
      .for (var stripe=125; stripe<130; stripe++)
      {
        lda screen1 + ((  0*40+stripe*8)&$3ff),x  // 125*8 = $03e8
        sta screen2 + (( -6*40+stripe*8)&$3ff),x
        sta screen3 + ((-12*40+stripe*8)&$3ff),x
      }
    dex
    bpl loop3
  #endif

  lax xStart2
  sec
  sbc #3
  sta xEnd

loop:
  // stripes 0-26 en 125-127 hoeven we niet te kopiëren
  .for (var stripe=30; stripe<125; stripe++)
  {
    lda screen1 + ((  0*40+stripe*8)&$3ff),x
    .if (stripe< 95) sta screen2 + (( -6*40+stripe*8)&$3ff),x
    .if (stripe>=60) sta screen3 + ((-12*40+stripe*8)&$3ff),x
  }

  cpx xEnd: #0
  beq endloop
  dex
  jmp loop
endloop:
  lda xStart2
  eor #4
  sta xStart2

  #if debug
    lda #0
    sta $d020
  #endif
  rts
}

* = * "[CODE] copyScreen2"
copyScreen2:
{
  #if postponeUpdate
    // because of the optization of not coyping to $1f40 during the frame update, we do have to copy stripe 125,126,127,0 (=128) and 1 (= 129)
    ldx #$07
    loop3:
      .for (var stripe=125; stripe<130; stripe++)
      {
        lda screen2 + ((  0*40+stripe*8)&$3ff),x  // 125*8 = $03e8
        sta screen3 + (( -6*40+stripe*8)&$3ff),x
        sta screen1 + ((-12*40+stripe*8)&$3ff),x
      }
    dex
    bpl loop3
  #endif

  lax xStart2
  sec
  sbc #3
  sta xEnd

loop:
  // stripes 0-26 en 125-127 hoeven we niet te kopiëren
  .for (var stripe=30; stripe<125; stripe++)
  {
    lda screen2 + ((  0*40+stripe*8)&$3ff),x
    .if (stripe< 95) sta screen3 + (( -6*40+stripe*8)&$3ff),x
    .if (stripe>=60) sta screen1 + ((-12*40+stripe*8)&$3ff),x
  }

  cpx xEnd: #0
  beq endloop
  dex
  jmp loop
endloop:
  lda xStart2
  eor #4
  sta xStart2
  
  #if debug
    lda #0
    sta $d020
  #endif
  rts
}

* = * "[CODE] copyScreen3"
copyScreen3:
{
  #if postponeUpdate
    // because of the optization of not coyping to $1f40 during the frame update, we do have to copy stripe 125,126,127,0 (=128) and 1 (= 129)
    ldx #$07
    loop3:
      .for (var stripe=125; stripe<130; stripe++)
      {
        lda screen3 + ((  0*40+stripe*8)&$3ff),x  // 125*8 = $03e8
        sta screen1 + (( -6*40+stripe*8)&$3ff),x
        sta screen2 + ((-12*40+stripe*8)&$3ff),x
      }
    dex
    bpl loop3
  #endif

  lax xStart2
  sec
  sbc #3
  sta xEnd

loop:
  // stripes 0-26 en 125-127 hoeven we niet te kopiëren
  .for (var stripe=30; stripe<125; stripe++)
  {
    lda screen3 + ((  0*40+stripe*8)&$3ff),x
    .if (stripe< 95) sta screen1 + (( -6*40+stripe*8)&$3ff),x
    .if (stripe>=60) sta screen2 + ((-12*40+stripe*8)&$3ff),x
  }

  cpx xEnd: #0
  beq endloop
  dex
  jmp loop
endloop:
  lda xStart2
  eor #4
  sta xStart2
  
  #if debug
    lda #0
    sta $d020
  #endif
  rts
}


* = * "[CODE] copy d800 up"
copyD800Up:
{ 
  #if debug
    dec $d020
  #endif

// first part copies first 12 rows. the bottom of this area is at $d012 = $30+12*8=$90 do not start copying when $d012 < $90

  ldx #39
!:
  .for (var row=0;row<12;row++)
  {
    lda $d800+6*40+row*40,x
    sta $d800+0*40+row*40,x
  }
  dex
  bpl !-

  // we are here before the upper border ends, so there are no timing issues in the code before

  #if debug
    dec $d020
  #endif
  
  ldx #15
!:
  .for (var i=0; i<18; i++)
  {
    lda $d800+18*40+i*16,x  // 18*40 = $2d0 + 17*16 = $110 = $3e0
    sta $d800+12*40+i*16,x  // i=0 : $d9e0. i=18 : $db00 + x=15 -> $db0f. 19 rows = $daf8
  }
  dex
  bpl !-

  #if debug
    dec $d020
  #endif

  // first copyoffset = $0000
  // 2nd copyoffset = ($0000+240)&$3f8

  ldx topLine
  lda d800UpTabLow,x
  sta fromD800a
  clc
  adc #120
  sta fromD800b
  lda d800UpTabHigh,x
  sta fromD800a+1
  adc #0
  sta fromD800b+1

  /*
  lda topD800
  clc
  adc #<(1024-6*40)
  sta fromD800a
  lda topD800+1
  adc #>(1024-6*40)
  and #3
  ora #>d800Colors
  sta fromD800a+1

  lda fromD800a
  clc
  adc #120
  sta fromD800b
  lda fromD800a+1
  adc #0
  and #3
  ora #>d800Colors
  sta fromD800b+1
  */

  ldx #119
loop3:
  lda fromD800a: d800Colors,x
  sta $daf8,x
  lda fromD800b: d800Colors+120,x
  sta $daf8+120,x
  dex
  bpl loop3

  #if debug
    lda #0
    sta $d020
  #endif
  rts
}

hideBugs:
{
  lax #0
loop:
  sta empty+$007,x
  sta empty+$107,x
  sta empty+$207,x
  sta empty+$307,x
  sta empty+$407,x
  sta empty+$507,x
  sta empty+$607,x
  sta empty+$707,x

  txa
  clc
  adc #8
  tax
  lda #0
  bcc loop

  rts
}

d800UpTabLow:
  .fill 25, <(d800Colors+(mod(i+19,25)*40))
d800UpTabHigh:
  .fill 25, >(d800Colors+(mod(i+19,25)*40))

//d800TabLow:
//  .fill 25, <(d800Colors+i*40)
//d800TabHigh:
//  .fill 25, >(d800Colors+i*40)

/*
* = * "[CODE] copy d800 down"
copyD800Down:
{
  #if debug
    dec $d020
  #endif

  // update the odd copies
*/
/*
  lda topD800
  sta fromD800a
  sta fromD800c
  sta fromD800e
  sta fromD800g

  lda topD800+1
  ora #>d800Colors
  sta fromD800a+1
  clc
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800c+1  
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800e+1   
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800g+1  

  // update the even copies
  lda fromD800a
  //clc
  adc #128
  sta fromD800b
  sta fromD800d
  sta fromD800f
  sta fromD800h
  lda fromD800a+1
  adc #0
  and #3
  ora #>d800Colors
  sta fromD800b+1
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800d+1
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800f+1
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800h+1
*/
/*
  // while copying, we don't want to cross $dbff -> $dc00
  // we are copying 6 rows down

  ldx #$7f
!:
  lda fromD800a: d800Colors+(($000-6*40)&$3ff),x  // todo: we should try to avoid crossing pages here..
                                                  // shifts by $f0 everytime, so to avoid crossing the page we can copy max $10 bytes
  sta $d800,x
  lda fromD800b: d800Colors+(($080-6*40)&$3ff),x
  sta $d880,x
  lda fromD800c: d800Colors+(($100-6*40)&$3ff),x
  sta $d900,x
  lda fromD800d: d800Colors+(($180-6*40)&$3ff),x
  sta $d980,x
  dex
  bpl !-

  // we are here before the upper border ends.. so before this there are no timing problems

  #if debug
    dec $d020
  #endif

  ldx #$7f
!:
  lda fromD800e: d800Colors+(($200-6*40)&$3ff),x  // todo: we should try to avoid crossing pages here..
  sta $da00,x
  lda fromD800f: d800Colors+(($280-6*40)&$3ff),x
  sta $da80,x
  dex
  bpl !-

  ldx #$7f
!:
  lda fromD800g: d800Colors+(($300-6*40)&$3ff),x
  sta $db00,x
  lda fromD800h: d800Colors+(($380-6*40)&$3ff),x
  sta $db80,x
  dex
  bpl !-

  #if debug
    lda #0
    sta $d020
  #endif

  rts
}
*/

prepBitmap2and3:
{
  // bitmap 1: .import c64 "includes/weed_and_witches.kla",0,$1f40
  // bitmap 2: .import c64 "includes/weed_and_witches.kla",6*320,($1f40-6*320)
  // bitmap 2: .import c64 "includes/weed_and_witches.kla",0,6*320
  // bitmap 3: .import c64 "includes/weed_and_witches.kla",12*320,($1f40-12*320)
  // bitmap 3: .import c64 "includes/weed_and_witches.kla",0,12*320

  lda #$34
  sta $01

  ldy #0
loop2:
  ldx #$3f
loop:
  lda from: bitmap1,x
  sta to:   bitmap2+19*320,x  // 19 = -5
  sta to2:  bitmap3+13*320,x  // 13 = -12
  dex
  bpl loop

  ldx #0
  jsr update
  ldx #3
  jsr update
  ldx #6
  jsr update

  // stop at $1f40
  iny
  cpy #($1f40/40)
  bne loop2

  lda #$35
  sta $01

  rts

update:
  lda from,x
  clc
  adc #$40
  sta from,x
  bcc !+
  inc from+1,x
!:
  // wrap to $0000 if at $1f40
  lda from,x
  cmp #$40
  bne !+
  lda from+1,x
  and #$1f
  cmp #$1f
  bne !+

  // wrap
  lda #0
  sta from,x
  lda from+1,x
  and #$e0
  sta from+1,x
!:
  rts
}

.align $100
sineX:
*=* "[DATA] sine for x bounce"
{
    .var sinSize = 128
    .var sinMin  = 00
    .var sinMax  = 40*8
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .fill sinSize, (((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize)))&$7)|$d0
    .fill sinSize, (((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize)))&$1f8)>>3
}

* = bitmap1 "[DATA] bitmap 1"
  .import c64 "includes/weed_and_witches_spot.kla",0,$1f40
  .fill $c0,0  // extra bitmap data should go here
* = screen1 "[DATA] screen 1 colors"
  .import c64 "includes/weed_and_witches_spot.kla",$1f40,$3e8
  .fill 24, $00



#if !AS_SPINDLE_PART
  * = screen2 "[RUNTIME] screen 2 colors"
    .import c64 "includes/weed_and_witches_spot.kla",$1f40+6*40,$3e8-6*40
    .import c64 "includes/weed_and_witches_spot.kla",$1f40,6*40
    .fill 24, 0
#endif

// todo : data needs to go here or the screen will get b0rked.

#if AS_SPINDLE_PART
  * = screen2 "[RUNTIME] screen 2 colors" virtual
    .import c64 "includes/weed_and_witches_spot.kla",$1f40+6*40,$3e8-6*40
    .import c64 "includes/weed_and_witches_spot.kla",$1f40,6*40
    .fill 24, 0
#endif


* = screen3 "[RUNTIME] screen 3 colors"
  .import c64 "includes/weed_and_witches_spot.kla",$1f40+12*40,$3e8-12*40
  .import c64 "includes/weed_and_witches_spot.kla",$1f40,12*40
  .fill 24, 0


* = d800Colors "[DATA] d800 colors"
  .import c64 "includes/weed_and_witches_spot.kla",$1f40+$3e8,$3e8
  .import c64 "includes/weed_and_witches_spot.kla",$1f40+$3e8,$118

* = $7fff "[DATA] ghost byte" virtual
  .byte 0
* = empty "[RUNTIME] hide bugs" virtual
  .fill 8*256,$54  // 6 empty lines -> only every 8th byte has to be 0
* = bitmap2 "[RUNTIME] bitmap 2" virtual
  .fill 32*256,0
* = bitmap3 "[RUNTIME] bitmap 3" virtual
  .fill 32*256,0 

* = sprites "[DATA] sprites"
  .var spriteData = LoadBinary("includes/we_are_coming.bin")
  #if showBorder
    .fill spriteData.getSize(),spriteData.get(i)^$00
  #else
    .fill spriteData.getSize(),spriteData.get(i)^$ff
  #endif

