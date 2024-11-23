.var music = LoadSid("../music/tubular.sid")
.var nrlines = 40
.var backgroundColor = $0
.var sideborderColor = $f
//#define showBorder
.var sinSizes = 160

#define YExpand

.var lst_bitmapData = List()
.var lst_screenData = List()
.var lst_d800Data   = List()

{
  // read 2 bitmaps and transform into 32 lines of data

  .var bitmap1 = LoadBinary("includes/agsp1.kla", BF_C64FILE)
  .var bitmap2 = LoadBinary("includes/agsp2.kla", BF_C64FILE)

  .for (var i=0; i<8000; i++) .eval lst_bitmapData.add(bitmap1.get(i))
  .for (var i=0; i<8000; i++) .eval lst_bitmapData.add(bitmap2.get(i))

  .for (var i=0; i<1000; i++) .eval lst_screenData.add(bitmap1.get(i+8000))
  .for (var i=0; i<1000; i++) .eval lst_screenData.add(bitmap2.get(i+8000))

  .for (var i=0; i<1000; i++) .eval lst_d800Data.add(bitmap1.get(i+8000+1000))
  .for (var i=0; i<1000; i++) .eval lst_d800Data.add(bitmap2.get(i+8000+1000))
}

.var bitmapRowPostions = List().add($6e00, $6f40, $7080, $71c0, $7300, $7440, $7580, $76c0, $7800, $7940, $7a80, $7bc0, $7d00, $7e40,          // 14 lines
                                    $ad40, $ae80, $afc0, $b100, $b240, $b380, $b4c0, $b600, $b740, $b880, $b9c0, $bb00, $bc40, $bd80, $bec0,   // 15 lines
                                    $f080, $f1c0, $f300, $f440, $f580, $f6c0, $f800, $f940, $fa80, $fbc0, $fe00)                                                                  // 6 lines

// fill memory with line data
.for (var line=0; line<bitmapRowPostions.size(); line++)
{
  * = bitmapRowPostions.get(line) "bitmapline data"
  .fill 320, lst_bitmapData.get(320*line + i)
}

.const maxCrunch = 5
.const flipAfter = 20
#define postponeUpdate

#if postponeUpdate
  .var notpostpone=0
#else
  .var notpostpone=1
#endif

//#define debug

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .label sprites2    = $0800
#else
  .label sprites2    = $0a00
#endif

.label screen2     = $0400  // this is a nice spot, since we copy everything to here
.label bitmap2     = $2000
.label bitmap1     = $4000
.label empty       = $6000
.label screen1     = $6800
.label sprites     = $6c00

.label code        = $8000

.label bitmap3      = $c000
.label screen3      = $e000
.label d800Colors   = $e400
.label screenColors = d800Colors + nrlines*40

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
.label vicbank    = $a1
.label topLine    = $a2
.label topLine2   = $a3
.label dd02       = $a4

#if AS_SPINDLE_PART
  .var PLAY=$1003
  .label spindleLoadAddress = sprites
  *=spindleLoadAddress-18-10-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word prepare       // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word bottomIRQ.MusicPlayCall // location of playroutine call

    .byte 'X'                            // not enough raster left for loading..
    // screen2 + bitmap 2 is used at runtime
    .byte 'P', >screen2,  >(screen2+$3ff)    // we use the screen memory at runtime    
    .byte 'P', >bitmap2,  >(screen1+$3ff)     // we use this memory at runtime

    .byte 'Z', $a0, $a4


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

  #if !AS_SPINDLE_PART
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

    jsr prepare

    lda #0
    sta timelow
    sta timehigh
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

  #if AS_SPINDLE_PART
    lda $d011
    and #$7f
    ora #$28
    sta $d011
  #else
    lda #$3f
    sta $d011
  #endif
  
  lda #$0
  sta linecrunch
  sta topLine

  lda #1
  sta vicbank       // show bitmap1
  lda #$3d
  sta dd02

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
    lda #0
    jsr music.init
  #endif

  #if showBorder
    lda #sideborderColor
  #else
    lda #backgroundColor
  #endif

  sta $d020
  lda #backgroundColor
  sta $d021
  lda #$d8
  sta $d016
  lda #$3d
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

  lda updateSprites: #0
  beq *-2

  ldx #$3e
loop:
  lda sprites2,x
  sta sprites,x
  lda sprites2+$40,x
  sta sprites+$40,x
  lda sprites2+$80,x
  sta sprites+$80,x
  lda sprites2+$c0,x
  sta sprites+$c0,x
  lda sprites2+$100,x
  sta sprites+$100,x
  lda sprites2+$140,x
  sta sprites+$140,x
  lda sprites2+$180,x
  sta sprites+$180,x
  dex
  bpl loop

  #if !AS_SPINDLE_PART
mainloop:
    //inc $d020
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
  // clear bitmaps :
  // bitmap 1
  // bitmap 2
  // screen 2  -> if the bitmap is clear, the colors do not matter so we don't have to clear it

  ldy #32-1
  lda #0
  tax
clearLoop:
  sta clear1: bitmap1,x
  sta clear2: bitmap2,x
  inx
  bne clearLoop
  inc clear1+1
  inc clear2+1
  dey
  bpl clearLoop
  
  // normally we can't prepare d800 here, but there is a dummy part before.
  ldy #3
  ldx #0
  txa
loopD800:
  sta empty+$000,x
  sta empty+$400,x
  sta screen1+$000,x
  sta $d800,x
  inx
  bne loopD800
  inc loopD800+2
  inc loopD800+5
  inc loopD800+8
  inc loopD800+11
  dey
  bpl loopD800

  rts
}

jumpTable:
  .word copyLineBitmap20, copyLineBitmap10, $0000, copyLineBitmap30
  .word copyLineBitmap21, copyLineBitmap11, $0000, copyLineBitmap31
  .word copyLineBitmap22, copyLineBitmap12, $0000, copyLineBitmap32
  .word copyLineBitmap23, copyLineBitmap13, $0000, copyLineBitmap33
  .word copyLineBitmap24, copyLineBitmap14, $0000, copyLineBitmap34
  .word copyLineBitmap25, copyLineBitmap15, $0000, copyLineBitmap35

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
  nop

  ldx.abs linecrunch
  lda d011tab,x
  nop

  // we are at cycle 59/$3b

  sta $d011
  lda #0
  sta $ffff

  lda spritesOn: #0
  sta $d015

  lda d016: #0   // 2
  sta $d016      // 4
  bit $ea        // 3
  nop
  nop
  lda nextIRQl: #<toBitmapIrq
  sta $fffe
  lda nextIRQh: #>toBitmapIrq
  sta $ffff

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
  #if YExpand
    lda nextD012: #$66
  #else
    lda nextD012: #$56
  #endif

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

  ldx dd02
dd02Write:
  ldx $dd02
  lda (d018tab-$3c),x
d018Write:
  lda $d018

  asl $d019

  inc 0

  ldx savex: #0
  lda savea: #0
  rti  
}

d018tab:
  .byte ((screen2&$3fff)/$0400)*$10+((bitmap2&$3fff)/$2000)*8
  .byte ((screen1&$3fff)/$0400)*$10+((bitmap1&$3fff)/$2000)*8
  .byte 0
  .byte ((screen3&$3fff)/$0400)*$10+((bitmap3&$3fff)/$2000)*8

toBitmapIrq:
{
  sta savea
  stx savex
  sty savey

  lda $01
  sta save01

  lda #$35
  sta $01

  lda d016: #0
  sta $d016

  // cleanly go to bitmap mode again
  lda $d011
  ora #$20
  tay
  ldx dd02
  lda (d018tab-$3c),x
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

  lda scrollMove.scrollAdd
  beq !+
    jsr copyLine
  !:

  lda save01: #0
  sta $01
  lda savea: #0
  ldx savex: #0
  ldy savey: #0
  rti
}

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

  lda blankSprites: #0
  bne spritesOff
  ldx #$7f
  lda bottomIRQ.charOrBitmap
  beq !+
  spritesOff:
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

  inc timelow
  bne !+
    inc timehigh
  !:

  MusicPlayCall:
  #if !AS_SPINDLE_PART
    jsr music.play
  #else         
    bit.abs $0000
  #endif

  jsr scrollMove

  // x movement
  lda sinePhase: #$0
  clc
  adc sineInc: #0
  cmp #sinSizes
  bne !+
    lda #0
  //and #$7f
  !:
  sta sinePhase

  tax
  cpx #40  // switch at 40 or 120
  beq switch
  cpx #120
  beq switch
  bne !+

switch:
  // do we have to change movements?
  lda switchSine: #0
  beq !+
  lda #0
  sta switchSine

  lda from+1
  cmp #>sineX
  beq toBounce

  lda #<sineX
  sta from
  lda #>sineX
  sta from+1
  lda #<(sineX+sinSizes)
  sta from2
  lda #>(sineX+sinSizes)
  sta from2+1
  bne !+
toBounce:
  lda #<sineBounceX
  sta from
  lda #>sineBounceX
  sta from+1
  lda #<(sineBounceX+sinSizes)
  sta from2
  lda #>(sineBounceX+sinSizes)
  sta from2+1
  bne !+

!:
  lda from2: sineX+sinSizes,x
  sta linecrunchirq.xoffset
  lda from:  sineX,x
  sta toBitmapIrq.d016
  bit linecrunchirq.spritesOn
  beq !+
  ora #$08
  !:
  sta linecrunchirq.d016

  // y movement
  ldx linecrunch
  lda d012tab,x
  sta $d012
  lda d011tab2,x
  sta $d011

  lda #$88     // hide bugs..
  sta $d018
  lda #$3d
  sta $dd02

  lda #<linecrunchirq
  sta $fffe
  lda #>linecrunchirq
  sta $ffff
  asl $d019

  // restore $3fff
  pla
  sta $3fff

  // copy bitmap, old scrollcopy functin
  lda copyUp: #0
  beq endCopy
  cli
  lda #0
  sta copyUp
  jsr copyD800Up
onlyBitmapUp:
  #if debug
    lda #7
    sta $d020
  #endif
  jsr copyBitmap
endCopy:

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
  .byte $1e,$3e,LDX_ABS,STX_ABS,LDA_ABS,STA_ABS

.var Wait         = $80
.var ScrollOn     = $81
.var SineOn       = $82
.var Switch       = $83
.var modBlack     = $84
.var fadeOut      = $85
.var End          = $86
.var WaitUntil    = $87
.var SpriteChange = $88

*=* "[CODE] scriptit"
scriptIt:
{
  lda waitUntil: #0
  beq dontWaitUntil
  lda timehigh
  cmp waitHi: #0
  bcc notyet
  lda timelow
  cmp waitLo: #0
  bcs now
notyet:
  rts

now:
  lda #0
  sta waitUntil
dontWaitUntil:
  lda wait
  beq advanceScript
  dec wait
  rts
advanceScript:
  ldx scriptPointer: #0

  lda script,x
  bmi checkCommands
  {
    cmp #$40
    bcc onoff
    and #$3f
    sta wait

    // blank and unblank the sprites
    inc scriptPointer
    lda bottomIRQ.blankSprites
    eor #1
    sta bottomIRQ.blankSprites
    rts

  onoff:
    sta wait

    lda bottomIRQ.charOrBitmap
    eor #1
    sta bottomIRQ.charOrBitmap

    inc scriptPointer
    rts
  }
checkCommands:
  cmp #Wait
  bne testScroll
  {
    lda script+1,x
    sta wait
    inc scriptPointer
    inc scriptPointer
    rts
  }
testScroll:
  cmp #ScrollOn
  bne testSine
  {
    lda #1
    sta scrollMove.scrollAdd
    inc scriptPointer
    rts
  }
testSine:
  cmp #SineOn
  bne testSwitch
  {
    lda #1
    sta bottomIRQ.sineInc
    inc scriptPointer
    rts
  }
testSwitch:
  cmp #Switch
  bne testMod
  {
    lda #1
    sta bottomIRQ.switchSine
    inc scriptPointer
    rts
  }
testMod:
{
  cmp #modBlack
  bne testFadeOut
  {
    lda script+1,x
    sta scrollMove.black
    inc scriptPointer
    inc scriptPointer
    rts
  }
}
testFadeOut:
  cmp #fadeOut
  bne testEnd
  {
    // check if shift-lock is pressed
    lda #$fd
    sta $dc00
    lda $dc01
    bpl !+         // branch if shift is pressed
    {
      lda #1
      sta scrollMove.black
      inc scriptPointer
    }
    !:
    rts
  }
testEnd:
  cmp #End
  bne testWaitUntil
  {
    inc nextpart   // only skip to the next part if shift is not pressed
    lda #$ff
    sta wait
  }
testWaitUntil:
  cmp #WaitUntil
  bne testSpriteChange
  {
    inc waitUntil
    lda script+1,x
    sta waitHi
    lda script+2,x
    sta waitLo
    lda scriptPointer
    clc
    adc #3
    sta scriptPointer
  }
testSpriteChange:
  cmp #SpriteChange
  bne endScript
  {
    inc start.updateSprites
    inc scriptPointer
  }
endScript:
  rts

wait:
  .byte 1
}

script:
  // at the start the sprites are off
  .byte Wait, 32

  // flash on the sprites
  .byte 1,1, 1, 1, 2,2, 3,3, 4,4, 5,5, 6,6, 8,8, 8
  .byte Wait, 25

  // agsp on
  .byte ScrollOn
  .byte modBlack,1
  .byte Wait,7
  .byte modBlack,0
  .byte modBlack,1
  .byte Wait,3
  .byte modBlack,0
  .byte modBlack,1
  .byte Wait,1
  .byte modBlack,0
  .byte modBlack,1
  .byte modBlack,0
  .byte modBlack,1
  .byte modBlack,0
  .byte Wait,0
  .byte modBlack,1
  .byte modBlack,0
  .byte Wait,2
  .byte modBlack,1
  .byte modBlack,0

  // blank the sprites 
  .byte $48, $48,$48, $46,$46, $45,$45, $44,$44, $43,$43, $42,$42, $41, $41, $41,$40

  // update sprites
  .byte SpriteChange
  .byte Wait,23

  // unblank the sprites
  .byte $41,$41, $41, $41, $42,$42, $43,$43, $44,$44, $45,$45, $46,$46, $48,$48, $48

  #if AS_SPINDLE_PART
    .byte WaitUntil, >$089b, <$089b
  #else
    .byte Wait,50
  #endif

  // unveil the magic
  .byte 2,8, 8,8, 6,6-6
  .byte SineOn, Switch  // $8c7 use sine for soft start and the switch to bounce
  .byte Wait,3
  .byte 5,5, 4,4, 3,3, 2,2, 1,1, 1,1, 1  // rest of unveil

  .byte Wait,255
  .byte Wait, 80
  .byte Wait,255,fadeOut
  .byte Wait, 25,End // time correctly, so after eyeball distort can start without too much delay

setSprites:
{
  lda #$0
  sta $d015
  
  lda #$1f-7
  sta $d000
  lda #$4f-7
  sta $d002
  lda #$7f-7
  sta $d004
  lda #$af-7
  sta $d006
  lda #$df-7
  sta $d008
  lda #$0f-7
  sta $d00a
  lda #$3f-7
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
  #if YExpand
    lda #$ff
    sta $d017
  #else
    sta $d017
    lda #$ff
  #endif

  sta $d01d

  lda #sideborderColor
  sta $d027
  sta $d028
  sta $d029
  sta $d02a
  sta $d02b
  sta $d02c
  sta $d02d

  //rts
}

setImages:
{
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

  rts
}

scrollMove:
{
  lda topLine    // keep track of where $d800 colors come from  : todo - this will be easier with 32 lines
  clc
  adc scrollAdd: #0
  .if (nrlines==32)
  {
    and #$1f
  } else {
    cmp #nrlines
    bcc !+
    sec
    sbc #nrlines
  !:
  }
  sta topLine
  ldx black: #0
  beq !+
    lda #nrlines
  !:
  sta topLine2

  lda linecrunch   // 0, 1, 2, 3, 4, 5 -> 0
  clc
  adc scrollAdd
  cmp #maxCrunch+1
  bcc end

  // switch to the other bitmap
  ldx vicbank
  lda nextBank,x
  sta vicbank
  ora #$3c
  sta dd02
  inc bottomIRQ.copyUp
  lda #0
end:
  sta linecrunch
  rts
}

nextBank:
  .byte 3,0,$ff,1  // bank1->bank0, bank0->bank3, bank3->bank1

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
  clc
  ldx topLine
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

  .if ((i==0))
  {
    lda screenLineTabLow,x
    sta screenFrom1
    ldy screenLineTabHigh,x
    sty screenFrom1+1
    //clc   // already cleared by copyLine

    adc #$8
    sta screenFrom2
    bne !+
    clc
    iny
    !:
    sty screenFrom2+1
    
    adc #$8
    sta screenFrom3
    bne !+
    clc
    iny
    !:
    sty screenFrom3+1

    adc #$8
    sta screenFrom4
    bne !+
    clc
    iny
    !:
    sty screenFrom4+1

    adc #$8
    sta screenFrom5
    bne !+
    iny
    !:
    sty screenFrom5+1
    
    lda d800LineTabLow,x
    sta d800From1
    ldy d800LineTabHigh,x
    sty d800From1+1
    clc

    adc #$8
    sta d800From2
    bne !+
    clc
    iny
    !:
    sty d800From2+1
    
    adc #$8
    sta d800From3
    bne !+
    clc
    iny
    !:
    sty d800From3+1

    adc #$8
    sta d800From4
    bne !+
    clc
    iny
    !:
    sty d800From4+1

    adc #$8
    sta d800From5
    bne !+
    iny
    !:
    sty d800From5+1

    ldy #7

    loop2:
      lda screenFrom1: screena+$0000+i*$28,y
      sta screena+(($3e8- 0*40+i*$28)&$3ff),y
      .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
      {
        sta screenb+(($3e8- 6*40+i*$28)&$3ff),y
        sta screenc+(($3e8-12*40+i*$28)&$3ff),y
      }
      lda screenFrom2: screena+$0008+i*$28,y
      sta screena+(($3f0- 0*40+i*$28)&$3ff),y
      .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
      {
        sta screenb+(($3f0- 6*40+i*$28)&$3ff),y
        sta screenc+(($3f0-12*40+i*$28)&$3ff),y
      }
      lda screenFrom3: screena+$0010+i*$28,y
      sta screena+(($3f8- 0*40+i*$28)&$3ff),y
      .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
      {
        sta screenb+(($3f8- 6*40+i*$28)&$3ff),y
        sta screenc+(($3f8-12*40+i*$28)&$3ff),y
      }
      lda screenFrom4: screena+$0018+i*$28,y
      sta screena+(($000- 0*40+i*$28)&$3ff),y
      .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
      {
        sta screenb+(($000- 6*40+i*$28)&$3ff),y
        sta screenc+(($000-12*40+i*$28)&$3ff),y
      }
      lda screenFrom5: screena+$0020+i*$28,y
      sta screena+(($008- 0*40+i*$28)&$3ff),y
      .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
      {
        sta screenb+(($008- 6*40+i*$28)&$3ff),y
        sta screenc+(($008-12*40+i*$28)&$3ff),y
      }

      lda d800From1: $d800+$0000 +i*$28,y  // never overflows ($dbff->$d800)
      sta $d800+(($3e8+i*$28)&$3ff),y      // can overflow for i=0
      lda d800From2: $d800+$0008 +i*$28,y  // never overflows ($dbff->$d800)
      sta $d800+(($3f0+i*$28)&$3ff),y      // can overflow for i=0
      lda d800From3: $d800+$0010 +i*$28,y  // never overflows ($dbff->$d800)
      sta $d800+(($3f8+i*$28)&$3ff),y      // can overflow for i=0
      lda d800From4: $d800+$0018 +i*$28,y  // never overflows ($dbff->$d800)
      sta $d800+(($000+i*$28)&$3ff),y      // never overflows ($dbff->$d800)
      lda d800From5: $d800+$0020 +i*$28,y  // never overflows ($dbff->$d800)
      sta $d800+(($008+i*$28)&$3ff),y      // never overflows ($dbff->$d800)

      dey
    bpl loop2
  } else
  {
    // update the fetch pointers

    lda d800LineTabLow,x
    sta d800ColorsFrom
    //clc   // already cleared by copyLine
    adc #<(screenColors-d800Colors)
    sta screenColorsFrom
    lda d800LineTabHigh,x
    sta d800ColorsFrom+1
    adc #>(screenColors-d800Colors)
    sta screenColorsFrom+1

    //lda screenLineTabLow,x
    //sta screenColorsFrom
    //lda screenLineTabHigh,x
    //sta screenColorsFrom+1

    ldy #39
    loop2:
      lda screenColorsFrom: screena+$0000+i*$28,y
      sta screena+(($3e8- 0*40+i*$28)&$3ff),y
      .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
      {
        sta screenb+(($3e8- 6*40+i*$28)&$3ff),y
        sta screenc+(($3e8-12*40+i*$28)&$3ff),y
      }

      lda d800ColorsFrom: $d800+$0000 +i*$28,y
      sta $d800+(($3e8+i*$28)&$3ff),y

      dey
    bpl loop2
  }

  lda #$34
  sta $01

  ldx topLine2
  lda bitmapLineTabLow,x
  sta bitmapFrom1
  sta bitmapFrom5   // this is +$100, so the low byte is always equal
  ldy bitmapLineTabHigh,x
  sty bitmapFrom1+1
  iny
  sty bitmapFrom5+1 // this is +$100, so the high byte is always +1
  sec

  sbc #$40
  sta bitmapFrom4
  bcs !+
  sec
  dey
  !:
  sty bitmapFrom4+1

  sbc #$40
  sta bitmapFrom3
  bcs !+
  sec
  dey
  !:
  sty bitmapFrom3+1

  sbc #$40
  sta bitmapFrom2
  bcs !+
  dey
  !:
  sty bitmapFrom2+1

  ldx #$3f
loop:
set1:
  lda bitmapFrom1: bitmapa+$0000+i*$140,x
  sta bitmapa+(($1f40- 0*320+i*$140)&$1fff),x
    .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!. this does cost extra 320*4 (lda) cycles though + looping (64*5) = 27 rasterlines, but saves 2560/58=44 rasters in this frame
  {
    sta bitmapb+(($1f40- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1f40-12*320+i*$140)&$1fff),x
  }
set2:
  lda bitmapFrom2: bitmapa+$0040+i*$140,x
  sta bitmapa+(($1f80- 0*320+i*$140)&$1fff),x
    .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
  {
    sta bitmapb+(($1f80- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1f80-12*320+i*$140)&$1fff),x
  }
set3:
  lda bitmapFrom3: bitmapa+$0080+i*$140,x
  sta bitmapa+(($1fc0- 0*320+i*$140)&$1fff),x
    .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
  {
    sta bitmapb+(($1fc0- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($1fc0-12*320+i*$140)&$1fff),x
  }
set4:
  lda bitmapFrom4: bitmapa+$00c0+i*$140,x
  sta bitmapa+(($2000- 0*320+i*$140)&$1fff),x

    .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
  {
    sta bitmapb+(($2000- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($2000-12*320+i*$140)&$1fff),x
  }
set5:
  lda bitmapFrom5: bitmapa+$0100+i*$140,x
  sta bitmapa+(($2040- 0*320+i*$140)&$1fff),x

    .if ((i>0) || (notpostpone==1))  // we update the top line by a copy in the background, we do not want to update too much in this frame, since we also have to update $d800!
  {
    sta bitmapb+(($2040- 6*320+i*$140)&$1fff),x
    sta bitmapc+(($2040-12*320+i*$140)&$1fff),x
  }

  dex
  bpl loop

  rts
}

* = * "[CODE] CopyLine"
copyLineBitmap10: acopyALine(1,0)
copyLineBitmap11: acopyALine(1,1)
copyLineBitmap12: acopyALine(1,2)
copyLineBitmap13: acopyALine(1,3)
copyLineBitmap14: acopyALine(1,4)
copyLineBitmap15: acopyALine(1,5)
copyLineBitmap20: acopyALine(2,0)
copyLineBitmap21: acopyALine(2,1)
copyLineBitmap22: acopyALine(2,2)
copyLineBitmap23: acopyALine(2,3)
copyLineBitmap24: acopyALine(2,4)
copyLineBitmap25: acopyALine(2,5)
copyLineBitmap30: acopyALine(3,0)
copyLineBitmap31: acopyALine(3,1)
copyLineBitmap32: acopyALine(3,2)
copyLineBitmap33: acopyALine(3,3)
copyLineBitmap34: acopyALine(3,4)
copyLineBitmap35: acopyALine(3,5)

copyBitmap:
  lda vicbank
  bne !+
  jsr copyBitmap2a  // value is 0
  jmp copyScreen2
!:
  cmp #1
  bne !+
  jsr copyBitmap1a  // value is 1
  jmp copyScreen1
!:
  jsr copyBitmap3a  // value is 2
  jmp copyScreen3

xStart:
  .byte $3f  // $3f->$1f->$3f->$1f

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
  ldx #0
  jsr copyD800Line1
  //ldx #40-40
  //jsr copyD800Line1b // jsr not needed : we pass to the next function by omitting an rts
  ldx #80-40
  jsr copyD800Line1b
  ldx #120-40
  jsr copyD800Line1b
  ldx #160-40
  jsr copyD800Line1b
  ldx #200-40
  jsr copyD800Line1b
  ldx #240-40
  jsr copyD800Line1b

  //ldx #0
  //jsr copyD800Line2
  //ldx #40
  //jsr copyD800Line2
  //ldx #80
  //jsr copyD800Line2
  //ldx #120
  //jsr copyD800Line2
  //ldx #160
  //jsr copyD800Line2
  //ldx #200
  //jsr copyD800Line2
  ldx #240
  jsr copyD800Line2

  ldx #0
  jsr copyD800Line3
  ldx #40
  jsr copyD800Line3
  ldx #80
  jsr copyD800Line3
  ldx #120
  jsr copyD800Line3
  ldx #160
  jsr copyD800Line3

  // first copyoffset = $0000
  // 2nd copyoffset = ($0000+240)&$3f8

  ldy topLine
  lda d800UpTabLow,y
  sta fromD800a
  ldx d800UpTabHigh,y
  stx fromD800a+1

  lda d800UpTabLow+1,y
  sta fromD800b
  ldx d800UpTabHigh+1,y
  stx fromD800b+1

  lda d800UpTabLow+2,y
  sta fromD800c
  ldx d800UpTabHigh+2,y
  stx fromD800c+1

  lda d800UpTabLow+3,y
  sta fromD800d
  ldx d800UpTabHigh+3,y
  stx fromD800d+1

  lda d800UpTabLow+4,y
  sta fromD800e
  ldx d800UpTabHigh+4,y
  stx fromD800e+1

  lda d800UpTabLow+5,y
  sta fromD800f
  ldx d800UpTabHigh+5,y
  stx fromD800f+1

  ldx #39
loop3:
  lda fromD800a: d800Colors,x
  sta $daf8,x
  lda fromD800b: d800Colors+40,x
  sta $daf8+40,x
  lda fromD800c: d800Colors+80,x
  sta $daf8+80,x
  lda fromD800d: d800Colors+120,x
  sta $daf8+120,x
  lda fromD800e: d800Colors+160,x
  sta $daf8+160,x
  lda fromD800f: d800Colors+200,x
  sta $daf8+200,x
  dex
  bpl loop3

  rts
}

// this pagebreaks for most values of x
// x=0 -> no break
copyD800Line1:
  .for (var i=0; i<40; i++)
  {
    lda $d800+6*40+i,x
    sta $d800+0*40+i,x
  }
  //rts  // directly go the the next function

// this is the above, but shifted 1 line. this does not pagebreak for 
// x = 0 (was 40)
copyD800Line1b:
  .for (var i=0; i<40; i++)
  {
    lda $d800+7*40+i,x
    sta $d800+1*40+i,x
  }
  //rts  // directly go the the next function

// 6*40 + 280 = 520 = $208
// this does not pagebreak for all values of x
copyD800Line2:
  .for (var i=0; i<40; i++)
  {
    lda $d800+6*40+280+i,x
    sta $d800+0*40+280+i,x
  }
  rts

// 6*40+560 = 320
// this does not pagebreak for the used values of x
copyD800Line3:
  .for (var i=0; i<40; i++)
  {
    lda $d800+6*40+560+i,x
    sta $d800+0*40+560+i,x
  }
  rts

d800UpTabLow:
  .fill nrlines, <(d800Colors+(mod(i+nrlines-6,nrlines)*40))
  .fill 5,       <(d800Colors+(mod(i+nrlines-6,nrlines)*40))
d800UpTabHigh:
  .fill nrlines, >(d800Colors+(mod(i+nrlines-6,nrlines)*40))
  .fill 5,       >(d800Colors+(mod(i+nrlines-6,nrlines)*40))

bitmapLineTabLow:
  .fill nrlines, <bitmapRowPostions.get(mod(i,nrlines))
  .byte <empty  // we use the empty space for fadeout
bitmapLineTabHigh:
  .fill nrlines, >bitmapRowPostions.get(mod(i,nrlines))
  .byte >empty  // we use the empty space for fadeout

.print("sine bounce")
sineX:
*=* "[DATA] sine for x movement"
{
    .var sinSize = sinSizes
    .var sinMin  = 00
    .var sinMax  = 40*8
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .var ValueList = List()
    .var phase = 3*sinSize/4

    .for (var i=0; i<sinSize; i++) 
    {
      .var Value = ((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians((mod(i+phase,sinSize))*360/sinSize)))
      //.eval ValueList.add(Value)
      //.print (Value)
    }

    .var speed = 0
    .var accel = 0
    .var position = 0

    .for (var i=0; i<sinSize; i++)
    {
      .if (i==1)  .eval accel =  1.0
      .if (i==2)  .eval accel =  0.9
      .if (i==3)  .eval accel =  0.7
      .if (i==4)  .eval accel =  0.6
      .if (i==5)  .eval accel =  0.5
      .if (i==6)  .eval accel =  0.4
      .if (i==7)  .eval accel =  0.3
      .if (i==9)  .eval accel =  0.2
      .if (i==11) .eval accel =  0.1
      .if (i==13) .eval accel =  0.0
      .if (i==15) .eval accel = -0.1


      // update speed + position
      .eval speed = speed + accel
      .eval position = position + speed
      .eval ValueList.add(position)

       .print ("i, speed, pos:" + i + "," + speed + "," + position)

    }

    .fill sinSize, ((ValueList.get(i))&$7)|$d0   // $d016 value
    .fill sinSize, ((ValueList.get(i))&$1f8)>>3  // vsp move value (0..40)

    //.print (ValueList)
}
.print("sine x bounce")
sineBounceX:
*=* "[DATA] sine for x bounce"
{
    .var sinSize = sinSizes
    .var offset = 0          // switching from sine to bounce means we have to offset to get a smooth crossover
    .var sinMin  = -20*8
    .var sinMax  = 20*8
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .var ValueList = List()
    .var ValueList2 = List()

    .for (var i=0; i<sinSize; i++)
    {
      .var sinValue = ((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize)))
      .var quarter = floor(4*i/sinSize)

      .var Value = sinValue
      .var Value2 = 0

      .if (quarter==0) 
      {
        .eval Value  = Value
        .eval Value2 = 320 * (2*i/sinSize)
      }
      .if (quarter==1) 
      {
        .eval Value = 320-Value
        .eval Value2 = 320 * (2*i/sinSize)
      }
      .if (quarter==2) 
      {
        .eval Value = 320+Value
        .eval Value2 = 640 - 320 * (2*i/sinSize)
      }
      .if (quarter==3) 
      {
        .eval Value = -Value
        .eval Value2 = 640 - 320 * (2*i/sinSize)
      }

      .eval Value = (Value + 2*Value2)/3

      .eval ValueList.add(Value)
      .eval ValueList2.add(Value2)

      .print (Value)
    }
    
    //.print (ValueList)

    .fill sinSize, ((ValueList.get(mod(i+sinSize-offset,sinSize)))&$7)|$d0   // $d016 value
    .fill sinSize, ((ValueList.get(mod(i+sinSize-offset,sinSize)))&$1f8)>>3  // vsp move value (0..40)
}

*=sprites+$1c0 "[DATA] tables to fill up memory"
d800LineTabLow:
  .fill nrlines, <(d800Colors+(i*40))

*=$7f80 "[DATA] tables to fill up memory"
d800LineTabHigh:
  .fill nrlines, >(d800Colors+(i*40))
screenLineTabLow:
  .fill nrlines, <(screenColors+(i*40))
screenLineTabHigh:
  .fill nrlines, >(screenColors+(i*40))

* = bitmap1 "[DATA] bitmap 1" virtual
  .fill 8192,0
* = bitmap2 "[RUNTIME] bitmap 2" virtual
  .fill 8192,0
* = bitmap3 "[RUNTIME] bitmap 3"         // this is inside the load, so it doesn't have to be virtual
  .fill 32*256,0 

* = screen1 "[RUNTIME] screen 1 colors" virtual
  .fill 1024,0
* = screen2 "[RUNTIME] screen 2 colors" virtual
  .fill 1024,0
* = screen3 "[RUNTIME] screen 3 colors"  // this is inside the load, so it doesn't have to be virtual
  .fill 1024,0

* = $7fff "[DATA] ghost byte"            // this is inside the load, so it doesn't have to be virtual
  .byte 0
* = empty "[RUNTIME] hide bugs" virtual
  .fill 8*256,$54  // 6 empty lines -> only every 8th byte has to be 0


* = d800Colors "[DATA] d800 colors"
  .fill nrlines*40,lst_d800Data.get(i)

* = screenColors "[DATA] screen colors"
  .fill nrlines*40,lst_screenData.get(i)






* = sprites "[DATA] sprites"
  .var spriteData = LoadBinary("includes/linecrunch1.bin")
  #if showBorder
    .fill spriteData.getSize(),spriteData.get(i)^$00
  #else
    .fill spriteData.getSize(),spriteData.get(i)^$ff
  #endif

 
#if !AS_SPINDLE_PART
* = sprites2 "[GFX] sprites2"
  .var spriteData2 = LoadBinary("includes/linecrunch2.bin")
  #if showBorder
    .fill spriteData2.getSize(),spriteData2.get(i)^$ff
  #else
    .fill spriteData2.getSize(),spriteData2.get(i)^$00
  #endif 
#endif

#if AS_SPINDLE_PART
* = sprites2 "[DATA] sprites2" virtual
  .fill 7*64,0