.var music = LoadSid("../music/tubular.sid")

// these are the demo-spanning 0 page adresses!
.label nextpart = $02
.label timelow  = $03
.label timehigh = $04

.label loader0 = $05
.label loader1 = $06
.label loader2 = $07
.label loader3 = $08
.label loader4 = $09

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .var PLAY=music.play
  .label spindleLoadAddress = start
  *=spindleLoadAddress-18-7-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"                  // fileformat magic
    .word prepare                 // prepare routine
    .word start                   // setup routine
    .word 0                       // irq handler
    .word 0                       // main routine
    .word 0                       // fadeout routine
    .word 0                       // cleanup routine
    .word rasterirq.MusicPlayCall // location of playroutine call

    .byte 'S'                     // this part is IO safe and can load below $d000
    .byte 'Z', $c0, $cf
    .byte 'P', >(square2_lo), >(square2_lo+$3ff)

    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart

#else    
    :BasicUpstart2(basicStart)
#endif

.const barHeight = 132
.const minRaster = $03  // 007 for VICE full border
.const maxRaster = $12d // 12c for VICE full border
.const toColor   = $0
.const fromColor = $0

.label posLow      = $c0
.label posHigh     = $c1
.label height      = $c2
.label sinphase    = $c3
.label sinphase2   = $c4
.label fade        = $c5
.label ypos        = $c6
.label yposhi      = $c7
.label maxLines    = $c8
.label cliplow     = $c9
.label factor1     = $ca
.label factor2     = $cb
.label resultlo    = $cc
.label resulthi    = $cd
.label result2lo   = $ce
.label result2hi   = $cf

#if !AS_SPINDLE_PART
  * = $4000
  basicStart:
    sei
    lda #$35
    sta $01
    jmp start
#endif

* = $e000 "[CODE] main code"
start:
{
  sei
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
  #endif

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d
  lda #<topirq
  sta $fffe
  lda #>topirq
  sta $ffff
  lda #$2e
  sta $d012
  lda #$8b
  sta $d011

  lda #0
  sta sinphase
  sta fade
  lda #30
  sta sinphase2

  #if !AS_SPINDLE_PART
    lda #0
    jsr music.init
  #endif

  lda $dc0d
  lda $dd0d
  asl $d019

  cli

  #if !AS_SPINDLE_PART
    jmp *
  #else
    rts
  #endif
}

prepare:
{
  rts
  ldx #$00
  ldy #$ff
loop:
  lda square1_hi+1,x
  sta square2_hi+$100,x
  lda square1_hi,x
  sta square2_hi,y
  lda square1_lo+1,x
  sta square2_lo+$100,x
  lda square1_lo,x
  sta square2_lo,y
  dey
  inx
  bne loop

  rts
}

topirq:
{
  sta restorea
  stx restorex
  sty restorey

  lda $01
  sta zp01
  lda #$35
  sta $01

  jsr script
  jsr setHeight

  lda ypos
  cmp #minRaster
  bne !+
  lda yposhi
  beq skipD020
!:
  lda setToColor: #toColor
  sta $d020
skipD020:
  lda ypos
  sec
  sbc #1
  sta $d012
  ldy #$0b
  lda yposhi
  sbc #0
  beq !+
  ldy #$8b
!:
  sty $d011

  lda #<rasterirq
  sta $fffe
  lda #>rasterirq
  sta $ffff
  asl $d019

  lda zp01: #0
  sta $01

  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}

.var Wait   = $80
.var Accel  = $81
.var hAccel = $82
.var Cycle  = $83
.var Break  = $84
.var Vpos   = $85
.var Size   = $86
.var Speed  = $87
.var GoGrey = $88
.var End    = $ff

script:
{
  lda wait: #0
  beq advanceScript
  dec wait
  rts

advanceScript:
  ldx scriptpointer: #0
  lda scriptdata,x
  cmp #Wait
  bne !+
  // set wait
  lda scriptdata+1,x
  sta wait
  lda #2
  jmp endScript2
!:
  cmp #1
  bne !+
  {
    ldy scriptdata+1,x
    sty fade
    cpy #17
    bcc !+
    lda colorramp+11,y
    sta topirq.setToColor
    sta rasterirq.setFromColor    
  !:
    lda #2
    jmp endScript2  
  }
!:
  cmp #Accel
  bne !+
  lda scriptdata+1,x
  sta setHeight.accelLoLo
  lda scriptdata+2,x
  sta setHeight.accelLo
  sta setHeight.accelHi
  lda #3
  jmp endScript2
!:
  cmp #hAccel
  bne !+
  lda scriptdata+1,x
  sta setHeight.hAccelLo
  lda scriptdata+2,x
  sta setHeight.hAccel
  sta setHeight.hAccelHi
  lda #3
  jmp endScript2 
!:
  cmp #Cycle
  bne !+
  {
    lda scriptdata+1,x
    sta cycle.cycleAdd
    lda #2
    bne endScript2
  }
!:
  cmp #Vpos
  bne !+
  {
    // update ypos
    lda scriptdata+1,x
    sta setHeight.fixedYHi
    lda scriptdata+2,x
    sta setHeight.fixedYLo
    sta ypos
    lda #3
    bne endScript2
  }
!:
  cmp #Size
  bne !+
  {
    // update size
    lda #0
    sta setHeight.HeightLo
    lda scriptdata+1,x
    sta setHeight.Height
    lda scriptdata+2,x
    sta setHeight.HeightHi
    lda #3
    bne endScript2
  }
!:
  cmp #Speed
  bne !+
  {
    // update ypos
    lda scriptdata+1,x
    sta setHeight.hSpeedLo
    lda scriptdata+2,x
    sta setHeight.hSpeed
    sta setHeight.hSpeedHi
    lda #3
    bne endScript2
  }

!:
  cmp #End
  bne !+
  {
    // reset script
    #if !AS_SPINDLE_PART
      //lda #0
      //sta scriptpointer
    #endif

    inc nextpart
    rts
  }
!:
  cmp #GoGrey
  bne endScript
  {
    lda #20    
    sta setHeight.Height

    lda #0
    sta setHeight.HeightHi
    sta setHeight.hAccelLo
    sta setHeight.hAccel
    sta setHeight.hAccelHi  
    sta setHeight.hSpeedLo
    sta setHeight.hSpeed
    sta setHeight.hSpeedHi  
    lda #$0f
    sta topirq.setToColor
    sta rasterirq.setFromColor
  }
endScript:
  lda #1
endScript2:
  clc
  adc scriptpointer
  sta scriptpointer
  rts
}

scriptdata:
  .byte 1,0 // black
  .byte Size, 100,0

  .byte Accel, 0, 0
  .byte hAccel, 0, 0
  .byte Vpos, >150, <150
  .byte Cycle,1
  .byte Wait,16
  .byte hAccel, $8,$00
  .fill 16,[1,i+1,Wait,1]      // fadein
  .byte hAccel, $f8,$ff
  .byte Wait,100
  .byte hAccel, $12,$00

  .byte Wait,60

  .byte hAccel, $f9, $ff
  .byte Wait,14

  .fill 39,[1,i+1+16]   // fade to lgrey
  
  .byte GoGrey

  .byte End

setHeight:
{
  // -------------
  // wijzig hoogte
  // -------------

  lda hSpeedLo:    #0
  clc
  adc hAccelLo:    #0
  sta hSpeedLo
  lda hSpeed:      #0
  adc hAccel:      #0
  sta hSpeed
  lda hSpeedHi:    #0
  adc hAccelHi:    #0
  sta hSpeedHi

  lda HeightLo:    #0
  clc
  adc hSpeedLo
  sta HeightLo
  lda Height:      #100
  adc hSpeed
  sta Height
  sta height
  tax
  lda HeightHi:    #0
  adc hSpeedHi
  sta HeightHi
  lsr             // shift into carry

  txa
  ror
  sta halfHeight   // we need half the height to calculate the y position of the top

  bmi step100

  lda stepSizeHigh,x
  sta rasterirq.stepHigh
  lsr
  sta posHigh
  lda stepSizeLow,x
  sta rasterirq.stepLow
  ror
  sta posLow
  jmp cont
step100:

  lda stepSizeHigh2,x
  sta rasterirq.stepHigh
  lsr
  sta posHigh
  lda stepSizeLow2,x
  sta rasterirq.stepLow
  ror
  sta posLow
cont:
  // ------------------------------------------------------
  // wijzig y positie - dit is het midden van de rasterbar.
  // ------------------------------------------------------

  lda fixedYHi
  ora fixedYLo
  //beq getSineYpos

  // accelerate
  lda speedLoLo: #0
  clc
  adc accelLoLo: #0
  sta speedLoLo
  lda speedLo:   #0
  adc accelLo:   #0
  sta speedLo
  lda speedHi:   #0
  adc accelHi:   #0
  sta speedHi

  // add speed
  lda fixedYLoLo: #0
  clc
  adc speedLoLo
  sta fixedYLoLo

  lda fixedYLo: #150
  adc speedLo
  sta fixedYLo
  sta ypos

  lda fixedYHi: #0
  adc speedHi
  sta fixedYHi
  sta yposhi

  // correct y position for height of rasterbar
  lda ypos
  sec
  sbc halfHeight: #0
  sta ypos
  lda yposhi
  sbc #0
  sta yposhi

  // check if bar is above the screen
  lda ypos
  clc
  adc height
  sta toplo
  lda yposhi
  adc setHeight.HeightHi

  bmi AboveOrBelow
  bne notAbove
  // tophi is 0
  lda toplo: #0
  cmp #minRaster
  bcc AboveOrBelow

notAbove:
  // check if bar is below the screen
  lda yposhi
  bmi notBelow
  cmp #>maxRaster
  bcc notBelow
  bne AboveOrBelow
  lda ypos
  cmp #<maxRaster
  bcc notBelow
AboveOrBelow:
  jmp makeBarInvisible
notBelow:
  // -----------------------------------
  // clip rasterbar correctly at the top
  // -----------------------------------
  
  // 1.calculate how many lines to clip

  lda #minRaster
  sec
  sbc ypos
  sta cliplow       // number of lines to clip
  lda #0            // highbyte of minRaster is 0
  sbc yposhi
  bmi dontClipTop

  // set start to minRaster position
  lda #minRaster
  sta ypos
  lda #0
  sta yposhi

  // calculate start of rasterbar correctly
  // start = pos + clip*stepsize

  lda cliplow
  sta factor1
  lda rasterirq.stepLow
  sta factor2
  ldy #0
  jsr multiply

  lda rasterirq.stepHigh
  sta factor2
  ldy #2
  jsr multiply.multiply2

  lda result2lo
  clc
  adc resulthi
  sta resulthi

  lda resultlo
  clc
  adc posLow
  sta posLow
  lda resulthi
  adc posHigh
  sta posHigh
dontClipTop:

  // clip rasterbar correctly at the bottom
  lda #<maxRaster
  sec
  sbc ypos
  sta maxLines
  rts

makeBarInvisible:
  // we put the rasterbar somewhere, but this will show the top color for a while and we do not want that
  lda yposhi
  bpl moveUp

  lda #minRaster
  sta ypos
  lda #0
  sta height
  sta yposhi
  sta posLow
  sta rasterirq.stepLow
  sta rasterirq.stepHigh
  lda #barHeight
  sta posHigh
  rts

moveUp:
  lda #maxRaster-1
  sta ypos
  lda #1
  sta yposhi
  lda #0
  sta height
  sta posLow
  sta rasterirq.stepLow
  sta rasterirq.stepHigh
  lda #barHeight
  sta posHigh
  rts
}

multiply:   
{                                   
  lda factor1                                              
  sta sm1                                           
  sta sm3                                             
  eor #$ff                                              
  sta sm2                                             
  sta sm4                                             
multiply2:
  ldx factor2
  sec   
  lda sm1: square1_lo,x
  sbc sm2: square2_lo,x
  sta resultlo,y
  lda sm3: square1_hi,x
  sbc sm4: square2_hi,x
  sta resulthi,y   
  rts
}

.align $100
rasterirq:
{
  // Jitter correction. Put earliest cycle in parenthesis.
  // (10 with no sprites, 19 with all sprites, ...)
  // Length of clockslide can be increased if more jitter
  // is expected, e.g. due to NMIs.

  dec 0            // 10..18
  sta atempLocal   // 15..23
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

  stx xtempLocal
  sty ytempLocal

  lda fade
  clc
  adc #<colorrampstart
  sta rasterirq.lowhigh
  lda #>colorrampstart
  adc #0
  sta rasterirq.lowhigh+1

  inc $dbff
  inc $dbff
  inc $dbff
  inc $dbff
  nop
  nop
  bit $ea

  ldx posHigh

  clc
loop:
  ldy rasterbar2,x         // laad de kleur
  ldx colorcycler2,y
  ldy posincolorramp,x    // bepaal de positie in de ramp
  lda lowhigh: colorrampstart+16,y    // load the color
  sta $d020               // 4
  nop
  nop

  lda posLow              // 3
  //clc
  adc stepLow: #0         // 2
  sta posLow              // 3
  lda posHigh             // 3
  adc stepHigh: #0        // 2
  sta posHigh             // 3
  tax

  dec.zp maxLines   // waste 1 extra cycle to make 63
  bne continueloop

  // are we in the region where d011 is negative
  bit $d011
  bpl continueloop2
  nop
  nop
  //nop
  bmi continue  // if we jump here, we end the bar because we are running out of the screen -> do not set fromColor to $d020
continueloop:
  nop
  nop
  nop
continueloop2:
  nop
  cpx #barHeight
  bcc loop
  // if we get here, the bar ended because all the colors have been shown
endloop:
  bit $ea
  inc $dbff
  inc $dbff
  lda setFromColor: #fromColor
  sta $d020
continue:
  lda #<topirq
  sta $fffe
  lda #>topirq
  sta $ffff

  lda #$8b
  sta $d011
  lda #$2f
  sta $d012
  asl $d019 
  inc 0
  lda $01
  sta restore01
  lda #$35
  sta $01

  cli
  MusicPlayCall:
  #if !AS_SPINDLE_PART
    jsr music.play
  #else         
    bit.abs $0000
  #endif
  jsr cycle

  lda restore01: #0
  sta $01
  ldx xtempLocal: #0
  ldy ytempLocal: #0
  lda atempLocal: #0
  rti
}

.align $100
*=* "[DATA] colorramps"
posincolorramp:
  .byte 00,15,03,11,05,09,01,13,06,02,10,04,08,14,07,12
colorrampstart:  // this is leading black in front of the colorramp.
  .byte 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
colorramp:
  .byte 00,06,09,02,11,04,08,14,12,05,10,03,15,07,13,01
colorrampend:  // this is the forward fade to lightgrey
  .byte 01,13,07,15,12,14,08,04,11,02,09,06,06,09,02,11
  .byte 04,08,14,12,05,10,03,15,15,15,15,15,15,15,15,15
  .byte 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
  .byte 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15

colorcycler2:
  .byte 0,0,6,6,11,11,4,4,14,14,3,3,13,13,7,1,1,7,13,13,3,3,14,14,4,4,11,11,6,6,0,0  // this part does color cycling to the left
  .byte 0,0,6,6,11,11,4,4,14,14,3,3,13,13,7,1,1,7,13,13,3,3,14,14,4,4,11,11,6,6,0,0  // this part does color cycling to the left
  .byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15                                        // these are fixed colors
originalColorCycle:
  .byte  9, 9,11, 9,11,11, 8, 8, 5, 5,15,15,13,13, 7, 1, 1, 7,13,13,15,15, 5, 5, 8, 8,11,11, 9,11, 9, 9
  .byte  9, 9,11, 9,11,11, 8, 8, 5, 5,15,15,13,13, 7, 1, 1, 7,13,13,15,15, 5, 5, 8, 8,11,11, 9,11, 9, 9
  //.byte  6, 6,11, 6,11,11, 4, 4,14,14, 3, 3,13,13, 7, 1, 1, 7,13,13, 3, 3,14,14, 4, 4,11,11, 6,11, 6, 6
  //.byte 6,6,11,6,11,11,4,4,14,14,3,3,13,13,7,1,1,7,13,13,3,3,14,14,4,4,11,11,6,11,6,6
originalColorCycle2:
  .byte  9, 9,11, 9,11,11, 8, 8, 5, 5,15,15,13,13, 7, 1, 1, 7,13,13,15,15, 5, 5, 8, 8,11,11, 9,11, 9, 9
  .byte  9, 9,11, 9,11,11, 8, 8, 5, 5,15,15,13,13, 7, 1, 1, 7,13,13,15,15, 5, 5, 8, 8,11,11, 9,11, 9, 9
  //.byte  6, 6,11, 6,11,11, 4, 4,10,10,13,13, 7, 7, 1, 1, 1, 1, 7, 7,13,13,10,10, 4, 4,11,11, 6,11, 6, 6
  //.byte  6, 6,11, 6,11,11, 4, 4,10,10,13,13, 7, 7, 1, 1, 1, 1, 7, 7,13,13,10,10, 4, 4,11,11, 6,11, 6, 6
  //.byte  9, 9, 2, 9, 2, 2, 8, 8,10,10,15,15, 7, 7, 1, 1, 1, 1, 7, 7,15,15,10,10, 8, 8, 2, 2, 9, 2, 9, 9
  //.byte 9,9,2,9,2,2,8,8,10,10,15,15,7,7,1,1,1,1,7,7,15,15,10,10,8,8,2,2,9,2,9,9

*=* "[code] rest"
cycle:
{
  lda phase: #140
  clc
  adc cycleAdd: #0
  cmp #200
  bcc !+
  lda #0
!:
  sta phase
  tax
  ldy sineColorCycle,x

  .for (var i=0; i<32; i++)
  {
    lda originalColorCycle+i,y
    sta colorcycler2+i
    lda originalColorCycle2+i,y
    sta colorcycler2+32+i
  }
  //.for (var i=0; i<32; i++)
  //{
  //  lda originalColorCycle+i,y
  //  sta colorcycler2+i
  //  lda originalColorCycle2+i,y
  //  sta colorcycler2+32+i
  //}
  rts
}

waste24:
  jsr waste12
waste12:
  rts

.align $100
rasterbar:
// lengte 64, this is the 'censor' style bumped rasterbar
  .byte 0
  .byte 6,0
  .byte 11,6,0
  .byte 10,4,11,0
  .byte 3,10,10,4,0
  .byte 7,3,3,3,10,0
  .byte 1,7,7,7,7,3,0
  .byte 7,1,1,1,1,1,1,0
  .byte 1,7,7,7,7,3,0
  .byte 7,3,3,3,10,0
  .byte 3,10,10,4,0
  .byte 10,4,11,0
  .byte 11,6,0
  .byte 6,0
  .byte 0
  .byte 0

rasterbar2:
// lengte 65*2
.var fix = $40  // use this for non-cycling colors

  .byte 0|fix,0|fix
  .byte 1,2,3,4,0|fix,0|fix
  .byte 1,2,3,4,5,6,0|fix,0|fix
  .byte 1,2,3,4,5,6,7,8,0|fix,0|fix
  .byte 1,2,3,4,5,6,7,8,9,10,0|fix,0|fix
  .byte 1,2,3,4,5,6,7,8,9,10,11,12,0|fix,0|fix
  .byte 1,2,3,4,5,6,7,8,9,10,11
  //.byte 3|fix,13|fix,1|fix,1|fix,13|fix,3|fix,
  .byte 12,13,14,14|32,13|32,12|32
  .byte 11|32,10|32,9|32,8|32,7|32,6|32,5|32,4|32,3|32,2|32,1|32,0|fix,0|fix
  .byte 12|32, 11|32, 10|32, 9|32, 8|32, 7|32, 6|32, 5|32, 4|32, 3|32, 2|32, 1|32, 0|fix, 0|fix
  .byte 10|32,  9|32,  8|32, 7|32, 6|32, 5|32, 4|32, 3|32, 2|32, 1|32, 0|fix,0|fix
  .byte 8|32,7|32,6|32,5|32,4|32,3|32,2|32,1|32,0|fix,0|fix
  .byte 6|32,5|32,4|32,3|32,2|32,1|32,0|fix,0|fix
  .byte 4|32,3|32,2|32,1|32,0|fix,0|fix
  .byte 0|fix  // to make bar invisible

stepSizeLow:
* = * "[DATA] stepsizelow tabel"
  .byte barHeight
  .for (var i=1; i<256; i++){
    .var step = 256*barHeight/i  
    .byte <step
  }
stepSizeHigh:
* = * "[DATA] stepsizehigh tabel"
  .byte 0
  .for (var i=1; i<256; i++){
    .var step = 256*barHeight/i  
    .byte >step
  }
stepSizeLow2:
* = * "[DATA] stepsizelow tabel"
  .for (var i=256; i<512; i++){
    .var step = 256*barHeight/i  
    .byte <step
  }
stepSizeHigh2:
* = * "[DATA] stepsizehigh tabel"
  .for (var i=256; i<512; i++){
    .var step = 256*barHeight/i  
    .byte >step
  }

sineColorCycle:
*=* "[DATA] sine for colorcycler"
{
  .var sinSize = 200
  .var sinMin  = 00
  .var sinMax  = 32
  .var sinAmp  = 0.5 * (sinMax-sinMin)
  .fill sinSize, ((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize))) & $1f
}

.align $100
*=* "[DATA] multiplication tables"
square1_lo:
  .fill 512,<((i*i)/4)
square1_hi:
  .fill 512,>((i*i)/4)
*=* "[DATA] multiplication tables 2" // virtual
square2_lo:
  .fill 512,<(((i-255)*(i-255))/4)  
square2_hi:
  .fill 512,>(((i-255)*(i-255))/4)  
