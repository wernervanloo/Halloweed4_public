.var music = LoadSid("../music/intro.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .var PLAY=$1003
  .label spindleLoadAddress = $6400
  *=spindleLoadAddress-18-16-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"               // fileformat magic
    .word prepare              // prepare routine
    .word start                // setup routine
    .word 0                    // irq handler
    .word 0                    // main routine
    .word 0                    // fadeout routine
    .word 0                    // cleanup routine
    .word topirq.MusicPlayCall // location of playroutine call

    //.byte 'S'           // this part is IO safe and can load below $d000      
    .byte 'A'
    .byte 'Z', $a0, $af
    .byte 'I', >screen1, >(screen1+$3f8)  // inherit screen1 from 1d_freemem
    .byte 'I', >bitmap1, >(bitmap1+$1f40) // inherit bitmap1 from 1d_freemem
    .byte 'P', >screen2, >(screen2+$3f8)  // screen 2 is used at runtime
    .byte 'P', >bitmap2, >(bitmap2+$1f40) // bitmap 2 is used at runtime

    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart
#else    
  :BasicUpstart2(start)
#endif

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

.label atemp     = $a0
.label xtemp     = $a1
.label ytemp     = $a2
.label firstrow  = $a3
.label firstrow2 = $a4
.label screenNr  = $a5

.label buffer    = $a6
.label state     = $a7
.label low       = $a8
.label high      = $a9
.label low2      = $aa
.label high2     = $ab

.label loadBitmap3b = $ac
.label loadBitmap4b = $ae

.label screen1     = $0400  // ..$07e7
.label bitmap1     = $2000  // ..$3f3f
.label bitmap2     = $4000  // ..$5f3f
.label screen2     = $6000  // ..$63e7
.label code        = $6400  //
//.label bitmapdata2 = $6d00  //
.label allScreenColors   = $6d80

* = screen1 "[screen] screen1" virtual
  .fill 1000,0
* = bitmap1 "[bitmap] bitmap1" virtual
  .fill 8000,0
* = bitmap2 "[bitmap] bitmap2" virtual
  .fill 8000,0
* = screen2 "[screen] screen2" virtual
  .fill 1000,0

* = allScreenColors "[DATA] screen colors"
  .import c64 "includes/2spot.kla",$1f40,1000 //+$3e8+$3e8
  .import c64 "includes/3spot.kla",$1f40,1000 //+$3e8+$3e8
  .import c64 "includes/4spot.kla",$1f40,1000 //+$3e8+$3e8
  .import c64 "includes/5spot.kla",$1f40,1000 //+$3e8+$3e8
  * = * "[DATA] D800 colors"
  allD800Colors:
  .import c64 "includes/2spot.kla",$1f40+$3e8,1000 //+$3e8+$3e8
  .import c64 "includes/3spot.kla",$1f40+$3e8,1000 //+$3e8+$3e8
  .import c64 "includes/4spot.kla",$1f40+$3e8,1000 //+$3e8+$3e8
  .import c64 "includes/5spot.kla",$1f40+$3e8,1000 //+$3e8+$3e8
* = * "buffer for drivers" virtual
  .fill 64,0
.align $100

#if !AS_SPINDLE_PART
  * = * "[DATA] bitmap data 2"
  bitmapdata2:
  .import c64 "includes/3spot.kla",0,$1f40 //+$3e8+$3e8
  .align $100
  bitmapdata1:
  * = * "[DATA] bitmap data 1"
  .import c64 "includes/2spot.kla",0,$1f40 //+$3e8+$3e8
#endif

#if AS_SPINDLE_PART
  * = * "[DATA] bitmap data 2" virtual
  bitmapdata2:
  .fill $1f40,0 //+$3e8+$3e8,0
  .align $100
  bitmapdata1:
  * = * "[DATA] bitmap data 1" virtual
  .fill $1f40,0 //+$3e8+$3e8,0
#endif

* = code "[CODE] main code"

start:
{
  sei

  #if !AS_SPINDLE_PART
    lda #$35
    sta $01
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
  lda #$10
  sta $d012
  lda #$b7
  sta $d011

  lda #0
  sta buffer     // buffer == 0, display bitmap1 and update bitmap2. 
                 // buffer == 1, display bitmap2 and update bitmap1. 
  sta firstrow
  sta firstrow2
  sta state
  sta screenNr

  lda $dc0d
  lda $dd0d
  asl $d019

  #if !AS_SPINDLE_PART
    lda #0
    sta $d020
    lda #$0a
    sta $d021

    lda #$d8
    sta $d016
    lda #$18
    sta $d018
    lda #$94
    sta $dd00
    lda #$3c
    sta $dd02
    lda #0
    jsr music.init
  #endif

  cli

  #if !AS_SPINDLE_PART
  mainLoop:
    jmp mainLoop
  #else
    rts
  #endif
}

dummy:
.byte 0

.label bloodbitmap = $e000
.label bloodscreen = $c000

prepare:
{
  rts
}

switchD021IRQ:
{
  sta restoreA
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #<topirq
  sta $fffe
  lda #>topirq
  sta $ffff
  lda #10
  sta $d012
  lda $d011
  ora #$80
  sta $d011
  
  lda.imm switchColor: #0
  sta $d021

  asl $d019

  lda restore01: #0
  sta $01
  lda.imm restoreA: #0
  rti
}

switchColors1:
  .byte $00  // color of screen 1 (XENON)
switchColors:
  .byte $00  // color of screen 2 (top of house)
  .byte $09  // color of screen 3 (part 2 of house)
  .byte $00  // color of screen 4 (part 3 of house)
  .byte $00  // color of screen 5 (HALLOWEED IV)
  .byte $00  // color of screen 4
  .byte $00  // color of screen 5

topirq:
{
  sta restoreA
  stx restoreX
  sty restoreY

  lda $01
  sta restore01
  
  lda #$35
  sta $01

  ldx #0               // this is the color for the log
  lda screenNr
  beq !+
  ldx #$08             // this is the start color if the logo is done
!:

  // now select the correct start color depending on the position
  ldx screenNr
  lda switchColors1,x
  sta $d021

  // set switch background irq

  lda #<switchD021IRQ
  sta $fffe
  lda #>switchD021IRQ
  sta $ffff
  
  ldx screenNr
  lda switchColors,x
  sta switchD021IRQ.switchColor
  
  lda #24
  sec
  sbc firstrow
add25:
  // negative? add 25
  cmp #$80
  bcc !+
  clc
  adc #25
  jmp add25
!:

  asl
  asl
  asl
  ldx state
  clc
  adc stateadd,x
  sta add8

  lda state
  clc
  adc #1
  and #7
  eor #$07
  clc
  adc.imm add8: #0
  adc #$2f
  sta $d012

MusicPlayCall:
#if !AS_SPINDLE_PART
        jsr music.play
#else         
        bit.abs $0000
#endif

  lda buffer
  bne !+
  lda #$18
  sta $d018
  lda #$3c
  sta $dd02
  jmp nextState
!:
  lda #$80
  sta $d018
  lda #$3d
  sta $dd02

nextState:

  lda state
  clc
  adc #1
  and #$07
  eor #$37
  sta $d011

  asl $d019

  cli
  jsr copy

  // ---------------
  // stop scrolling?
  // ---------------

  lda screenNr    // wait until we are displaying the last bitmap
  cmp #4
  bne continue

  lda state       // wait until we arrive at $d011 = $3b
  cmp #3
  bcc continue
  
  // switch to fader

  lda #4         // this part starts at nextpart == 3, so we increase to 4
  sta nextpart 

  jmp endIRQ

continue:
  // softstart
  ldx softStart: #0
  lda softTable,x
  bmi !+
  sta addState
  inc softStart
!:
  lda state
  clc
  adc addState: #0
  and #$07
  sta state

  lda state
  bne endIRQ

  // go to next row of bitmap
  inc firstrow2
  lda firstrow
  clc
  adc #1
  cmp #25
  bne !+
  inc screenNr
!:

  cmp #50
  bne !+
  inc screenNr
  lda #0
!:
  // increase screen if multiple of 25
  sta firstrow

  #if !AS_SPINDLE_PART
    // reset screenNrs
    lda screenNr
    cmp #6
    bne !+
    lda #4
    sta screenNr
  !:
  #endif

  // -------------------------
  // set flags to load bitmaps
  // -------------------------

  // bitmap 3 ready to load?
  //lda screenNr
  //beq !+
  //lda firstrow
  cmp #25
  bne !+
  lda #1
  sta loadBitmap3b  // this switches the loading
!:

  // bitmap 4 ready to load?
  lda screenNr
  cmp #2
  bcc !+
  lda firstrow
  cmp #1
  bne !+

  lda #1
  sta loadBitmap4b  // this switches the loading
!:

endIRQ:
  lda restore01: #0
  sta $01
  
  ldy restoreY: #0
  ldx restoreX: #0
  lda restoreA: #0

  rti
}

stateadd:
  .byte 8,8,8,8,8,8,8,0
softTable:
  .byte 1,0,0,0,1,0,0,1,0,1,1,0,1,$ff
  
.macro setFromBitmap2(i)
{
  lda bitmapToLo+i+1,x
  sta copy.from1+i*6
  adc #160
  sta copy.from1a+i*6
  lda bitmapToHi+i+1,x
  sta copy.from1+i*6+1
  adc #0
  sta copy.from1a+i*6+1

  lda bitmapToLo+25+i,x
  sta copy.to1+i*6
  adc #160
  sta copy.to1a+i*6
  lda bitmapToHi+25+i,x
  sta copy.to1+i*6+1
  adc #0
  sta copy.to1a+i*6+1
}

copy:
{
  lda state

  // state == 0,1,2,3,4,5 -> copy 4 bitmap lines
  // state == 6           -> copy screencolors
  // state == 7           -> copy d800
!:
  cmp #6
  bne !+
  jmp copyScreen
!:
  cmp #7
  bne !+
  jmp copyColors
!:
  // *4
  asl
  asl

  ldx buffer  // buffer == 0 : display bitmap 1 and update bitmap 2
  beq !+
  clc
  adc #25     // add 25 if buffer == 0 in order to update bitmap 2
!:
  tax
  clc
  setFromBitmap2(0)
  setFromBitmap2(1)
  setFromBitmap2(2)
  setFromBitmap2(3)

  // copy first part of lines
  ldy #159
!:
  lda from1:  $0000,y
  sta to1:    $0000,y
  lda from2:  $0000,y
  sta to2:    $0000,y
  lda from3:  $0000,y
  sta to3:    $0000,y
  lda from4:  $0000,y
  sta to4:    $0000,y
  lda from1a: $0000,y  // this is +160 bytes from from1
  sta to1a:   $0000,y
  lda from2a: $0000,y
  sta to2a:   $0000,y
  lda from3a: $0000,y
  sta to3a:   $0000,y
  lda from4a: $0000,y
  sta to4a:   $0000,y

lastTime:
  dey
  bne !-
  lda #RTS
  sta lastTime
  jsr !-
  lda #DEY
  sta lastTime

endCopy:
  rts
}

* = * "[CODE] copyscreen"
copyScreen:
{
  // first copy the last bitmap line
  ldx firstrow

  lda bitmapFromLo,x
  sta from1
  clc
  adc #160
  sta from1a
  lda bitmapFromHi,x
  sta from1+1
  adc #0
  sta from1a+1

  ldx #49
  lda buffer
  beq !+
  ldx #24
!:

  lda bitmapToLo,x
  sta to1
  clc
  adc #160
  sta to1a
  lda bitmapToHi,x
  sta to1+1
  adc #0
  sta to1a+1

  ldy #159
!:
  lda from1:  bitmapdata1,y
  sta to1:    bitmap1+24*320,y
  lda from1a: bitmapdata1+160,y
  sta to1a:   bitmap1+24*320+160,y
lastTime:
  dey
  bne !-
  lda #RTS
  sta lastTime
  jsr !-
  lda #DEY
  sta lastTime

  ldx firstrow2
  lda screenFromLo,x
  sta from1b
  sta from1c
  lda screenFromHi,x
  sta from1b+1
  sta from1c+1

  ldx #39

  lda buffer
  beq loop
  jmp copyBuffer2to1
loop:
  // buffer == 0 : display bitmap 1 and update bitmap 2
  // copy bitmap 1 to bitmap 2
  .for (var line=0; line<24; line++)
  {
    lda screen1+40+line*40,x
    sta screen2+line*40,x
  }
  lda.abs from1b: $0000,x
  sta.abs screen2+24*40,x
  dex
  bmi !+
  jmp loop
!:

  lda buffer
  eor #$01
  sta buffer

  jmp copy.endCopy

copyBuffer2to1:
  // buffer == 1 : display bitmap 2 and update bitmap 1
  // copy bitmap 2 to bitmap 1

  .for (var line=0; line<24; line++)
  {
    lda screen2+40+line*40,x
    sta screen1+line*40,x
  }
  lda.abs from1c: $0000,x
  sta.abs screen1+24*40,x
  dex
  bmi !+
  jmp copyBuffer2to1
!:

  lda buffer
  eor #$01
  sta buffer

  jmp copy.endCopy
}

copyColors:
{
  ldx firstrow2
  lda d800FromLo,x
  sta from1e
  lda d800FromHi,x
  sta from1e+1
  
  ldx #19
!:
  .for (var line=0; line<8; line++)
  {
    lda $d800+40+line*40,x
    sta $d800+line*40,x

    lda $d800+20+40+line*40,x
    sta $d800+20+line*40,x
  }
  dex
  bpl !-

  ldx #19
!:
  .for (var line=8; line<16; line++)
  {
    lda $d800+40+line*40,x
    sta $d800+line*40,x

    lda $d800+20+40+line*40,x
    sta $d800+20+line*40,x
  }
  dex
  bpl !-

  ldx #19
!:
  .for (var line=16; line<24; line++)
  {
    lda $d800+40+line*40,x
    sta $d800+line*40,x

    lda $d800+20+40+line*40,x
    sta $d800+20+line*40,x
  }
  dex
  bpl !-

  ldx #39
loop2:
  lda.abs from1e: $1000,x
  sta.abs $d800+24*40,x
  dex
  bpl loop2

  jmp copy.endCopy
}

bitmapFromLo:
  .fill 25,<(i*320 + bitmapdata1)
  .fill 25,<(i*320 + bitmapdata2)
bitmapFromHi:
  .fill 25,>(i*320 + bitmapdata1)
  .fill 25,>(i*320 + bitmapdata2)

*=* "[DATA] bitmapToLo"
bitmapToLo:  // I checked, this needs 75 bytes
  .fill 25,<(i*320 + bitmap1)
  .fill 25,<(i*320 + bitmap2)
  .fill 25,<(i*320 + bitmap1)

bitmapToHi:
  .fill 25,>(i*320 + bitmap1)
  .fill 25,>(i*320 + bitmap2)
  .fill 25,>(i*320 + bitmap1)

screenFromLo:
  .fill 100,<(allScreenColors + i*40)
screenFromHi:
  .fill 100,>(allScreenColors + i*40)
d800FromLo:
  .fill 100,<(allD800Colors + i*40)
d800FromHi:
  .fill 100,>(allD800Colors + i*40)