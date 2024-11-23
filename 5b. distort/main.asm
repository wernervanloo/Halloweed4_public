.var debug   = 0
.var background = BLACK
#define CHANGECOLORS

.var music = LoadSid("../music/tubular.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

// yellow web picture

.var col1 = $09
.var col2 = $0a
.var col3 = $07
.var picture = LoadPicture("./includes/web.png", List().add($5C4700, $BB776D, $D0DC71, $000000)) // set black as $d800
                                                         // these are the colors for the bitpairs

.var breedte = picture.width/8
.var center  = ((breedte*4)-160)/2+163

.var charsets = List().add(charset01, charset02, charset03, charset04, charset05, 
                           charset06, charset07, charset08, charset09, charset10, charset11, charset12,
                           charset13, charset14, charset15, charset16, charset17, 
                           charset18, charset19, charset20, charset21, charset22, charset23, charset24, 
                           charset01)

.var screens  = List().add(screen1, screen1, screen1, screen1, screen1,
                           screen2, screen2, screen2, screen2, screen2, screen2, screen2,
                           screen3, screen3, screen3, screen3, screen3,
                           screen4, screen4, screen4, screen4, screen4, screen4, screen4, 
                           screen1)

#if AS_SPINDLE_PART
  .var PLAY=$1003
  .label spindleLoadAddress = gfxdata1
  *=spindleLoadAddress-18-(1+29*3)-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word prepare       // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word irq.MusicPlayCall // location of playroutine call

    .byte 'A'           // avoid loading
    //.byte 'S'                                 // !!! this part is NOT IO safe and can NOT load below $d000
    .byte 'P', >screen1,   >(screen1+1000)      // screen 1
    .byte 'P', >screen2,   >(screen2+1000) 
    .byte 'P', >screen3,   >(screen3+1000) 
    //.byte 'P', >screen4,   >(screen4+1000)    // we ARE using screen4, but only when the part starts, not during prepare.. if we declare this memory prepare will not start during the eyeball-on-a-rope loader thingy and spindle will insert a blank.. grrr.
                                                // we should be really careful with this!!
                                                
    .byte 'P', >charset01, >(charset01+$5ff) 
    .byte 'P', >charset02, >(charset02+$5ff) 
    .byte 'P', >charset03, >(charset03+$5ff) 
    .byte 'P', >charset04, >(charset04+$5ff)     
    .byte 'P', >charset05, >(charset05+$5ff) 
    .byte 'P', >charset06, >(charset06+$5ff) 
    .byte 'P', >charset07, >(charset07+$5ff) 
    .byte 'P', >charset08, >(charset08+$5ff) 
    .byte 'P', >charset09, >(charset09+$5ff) 
    .byte 'P', >charset10, >(charset10+$5ff) 
    .byte 'P', >charset11, >(charset11+$5ff) 
    .byte 'P', >charset12, >(charset12+$5ff) 
    .byte 'P', >charset13, >(charset13+$5ff) 
    .byte 'P', >charset14, >(charset14+$5ff) 
    .byte 'P', >charset15, >(charset15+$5ff) 
    .byte 'P', >charset16, >(charset16+$5ff) 
    .byte 'P', >charset17, >(charset17+$5ff) 
    .byte 'P', >charset18, >(charset18+$5ff) 
    .byte 'P', >charset19, >(charset19+$5ff) 
    .byte 'P', >charset20, >(charset20+$5ff) 
    .byte 'P', >charset21, >(charset21+$5ff) 
    .byte 'P', >charset22, >(charset22+$5ff) 
    .byte 'P', >charset23, >(charset23+$5ff) 
    .byte 'P', >charset24, >(charset24+$5ff)

    .byte 'P', >scrolledChars, >(scrolledChars+$1ff)
    .byte 'P', $b6, $b6                                // protect the area where the driver is!
    //.byte 'P', >sprites1,  >(sprites1+$ff) 
    //.byte 'P', >sprites2,  >(sprites2+$ff) 

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

.label sid0 = $f8
.label sid1 = $f9
.label sid2 = $fa
.label sid3 = $fb
.label sid4 = $fc
.label sid5 = $fd
.label sid6 = $fe
.label sid7 = $ff

.label screen1   = $0400
.label buffer    = $0400
.label charset01 = $0800
.label charset02 = $2000
.label charset03 = $2800
.label charset04 = $3000
.label charset05 = $3800
.label sprites1  = $3e00

.label screen2   = $4000
.label charset06 = $4800
.label charset07 = $5000
.label charset08 = $5800
.label charset09 = $6000
.label charset10 = $6800
.label charset11 = $7000
.label charset12 = $7800
.label sprites2  = $7e00

.label screen3   = $8000
.label scrolledChars = $8600  // table used to scroll left/right
.label charset13 = $8800

.label charset14 = $a000
.label charset15 = $a800
.label charset16 = $b000
.label charset17 = $b800

.label charset18 = $c000
.label charset19 = $c800
.label charset20 = $d000
.label charset21 = $d800
.label charset22 = $e000
.label charset23 = $e800
.label charset24 = $f000
.label screen4   = $f800

.label gfxdata1  = $7700  // data that will end up in $8000-$8fff, total : 13 rows * 48 chars * 8 = $1380
.label code      = $8e00
.label gfxdata2  = $a000  // data that will end up in $a000-$ffff, total : 11 rows * 48 chars * 8 = $1080

// decode charsets

.var DataPositions = List()

*=gfxdata1 "[DATA] gfx before code"
.for (var row=0; row<charsets.size()-1; row++)
{
  .var charset = charsets.get(row)
  .if (charsets.get(row)<code)
  {
    .eval DataPositions.add(*)
    .for (var charx=0; charx<(breedte+1); charx++)  // one char extra for the wrap around
    {
      .for (var byte=0; byte<8; byte++)
      {
        .byte picture.getMulticolorByte(mod(charx,breedte), byte+(row*8))
      }
    }
  }
}

*=gfxdata2 "[DATA] gfx after code"
.for (var row=0; row<charsets.size()-1; row++)
{
  .var charset = charsets.get(row)
  .if (charset>code)
  {
    .eval DataPositions.add(*)
    .for (var charx=0; charx<(breedte+1); charx++)  // one char extra for the wrap around
    {
      .for (var byte=0; byte<8; byte++)
      {
        .byte picture.getMulticolorByte(mod(charx,breedte), byte+(row*8))
      }
    }
  }
}

// fill until $b600 with 0 to get the driver to move in a safe spot
.var fillBytes = $b600-*
.fill fillBytes,0

*=code "[CODE] main code"
start:
{
  sei
  lda $01
  sta restore01

  lda #$35
  sta $01

  #if !AS_SPINDLE_PART
    jsr prepare
    lda #$40
    sta timelow
    lda #$0d
    sta timehigh

  #endif

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d
  lda #<irq
  sta $fffe
  lda #>irq
  sta $ffff
  lda #$fa
  sta $d012
  lda #$00
  sta $d011

  lda #$00
  sta $d020
  lda #col1
  sta $d021
  lda #col2
  sta $d022
  lda #col3
  sta $d023

  lda #$00
  sta $d015

  lda #$d8
  sta $d016

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
    lda #$3c
    sta $dd02

    lda #0
    jsr music.init
  #endif

  lda $dc0d
  lda $dd0d
  asl $d019

  lda #17
  sta theOneIrq.yScroll

  lda restore01: #0
  cmp #$37
  beq !+
  sta $01
!:

  cli

  #if !AS_SPINDLE_PART
    mainloop:
    jmp mainloop
  #else
    rts
  #endif
}

gfxDataLow:
  .fill DataPositions.size(),<DataPositions.get(i)
gfxDataHigh:
  .fill DataPositions.size(),>DataPositions.get(i)
depackOrder:
  .byte  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12
  .byte 23,22,21,20,19,18,17,16,15,14,13  // depack charsets above code in reverse order

prepare:
{
  jsr writeRows.write
  
  ldx #0
  lda #(background | $08)
loop:
  sta $d800,x
  sta $d900,x
  sta $da00,x
  sta $db00,x
  inx
  bne loop

  // grahamize charsets

  // step 1 : copy into buffer
  // step 2 : copy back to charset, separated by 4
  // step 3 : shift 2 pixels
  // step 4 : = step 2
  // repeat

  // step 1, copy to buffer

  lda #$30
  sta $01

charsetLoop:
  ldy charsetnr: #0
  ldx depackOrder,y

  lda gfxDataLow,x
  sta from1
  clc
  adc #(((breedte+1)*8)/2)  
  sta from2
  lda gfxDataHigh,x
  sta from1+1
  adc #0
  sta from2+1  

  ldy #0
copyToBufferLoop:
  lda from1: charset03,y
  sta           buffer,y
  lda from2: charset03+(((breedte+1)*8)/2),y
  sta           buffer+(((breedte+1)*8)/2),y
  iny
  cpy #(((breedte+1)*8)/2)  // +1 : one extra char for wrapping
  bne copyToBufferLoop

  // step 2
  ldx #0
  jsr copyBack

  jsr shiftBuffer
  jsr shiftBuffer
  ldx #8
  jsr copyBack

  jsr shiftBuffer
  jsr shiftBuffer
  ldx #16
  jsr copyBack

  jsr shiftBuffer
  jsr shiftBuffer
  ldx #24
  jsr copyBack
  
  inc charsetnr
  lda charsetnr
  cmp #charsets.size()-1
  bne charsetLoop

  lda #$35
  sta $01

  // make data for scrolling the picture
  ldy #1 // nr of pages to generate -1
  ldx #0
  txa
  clc
loop2:
  sta scrolledChars,x
  adc #1
  cmp #(breedte*4)
  bne !+
  lda #0
  clc
!:

  inx
  bne loop2
  inc loop2+2
  dey
  bpl loop2

  rts
}

copyBack:
{
  stx to1    // offset 0,8,16,24
  ldy prepare.charsetnr
  ldx depackOrder,y
  ldy charsetTabHi,x
  sty to1+1  // set charset
  lda #>buffer
  sta from1+1

  ldx #breedte-1

  ldy #0
copyBackLoop:
  lda from1:  buffer,y
  sta to1: charset01,y
  iny

  tya
  and #7
  bne copyBackLoop

  cpy #0
  bne !+
  inc from1+1
  inc to1+1
!:
  lda to1
  clc
  adc #$18
  sta to1
  bcc !+
  inc to1+1
!:
  dex
  bpl copyBackLoop
  rts
}

shiftBuffer:
{
  ldx #7
loop:
  .for (var i=0; i<(breedte+1); i++) rol buffer+((breedte-i)*8),x
  dex
  bmi endShift
  jmp loop

endShift:
  rts
}

theOneIrq:
{
  sta atemp
  stx xtemp
  sty ytemp

  lda $d012
  clc
  adc #8
  sta $d012

  ldx index: #0
  ldy yScroll: scrolledChars,x

  asl $d019
  inc index

  lda d018Tab,y
  sta $d018
  lda dd02Tab,y
  sta $dd02

  ldy ytemp: #0

  lda d021Tab,x
  sta $d021
  lda d022Tab,x
  sta $d022
  lda d023Tab,x
  sta $d023

  lda atemp: #0

  cpx #25
  beq enterIrq
end:
  ldx xtemp: #0
  rti
}

enterIrq:
  ldx theOneIrq.xtemp
irq:
{
  sta atemp
  sty ytemp
  stx xtemp

  jsr script
  //jsr writeRows
  jsr scrollY

  // move X
  lda useSine: #0
  and #1
  beq endSineScroll

  inc sineOffset
  ldy sineOffset: #$40
  lda sineX,y
  sta xScroll
endSineScroll:

  jsr flash.flash   // first move, than flash so we can use the yscroll if need be

  lda scrollLow
  ora #$10
  sta $d011

  lda #<theOneIrq
  sta $fffe
  lda #>theOneIrq
  sta $ffff

  lda d021Tab
  sta $d021
  lda d022Tab
  sta $d022
  lda d023Tab
  sta $d023

  lda #1
  sta theOneIrq.index
  //sta theOneIrq.flashOffset

  lda #$37
  clc
  adc scrollLow
  sta $d012

  asl $d019

  ldy theOneIrq.yScroll
  lda d018Tab,y
  sta $d018
  lda dd02Tab,y
  sta $dd02

  cli
  jsr plotter
  jsr writeRows

  // keep demotime (always before calling music player)
  inc timelow
  bne !+
  inc timehigh
!:
  .if (debug==1) inc $d020
    MusicPlayCall:
    #if !AS_SPINDLE_PART
      jsr music.play
    #else         
      bit.abs $0000
    #endif
  .if (debug==1) dec $d020

  lda atemp: #0
  ldx xtemp: #0
  ldy ytemp: #0
  rti
}

.var Wait      = $00
.var Sine      = $01
.var Flash     = $04
.var Morph     = $05
.var WaitUntil = $06
.var Reflect   = $07
.var End       = $ff

script:
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
  lda waitFrames:    #0
  beq advanceScript
  dec waitFrames
  rts

advanceScript:
  ldx scriptPointer: #0
  lda scriptData,x

testWait:
  cmp #Wait
  bne testWaitUntil
  {
    lda scriptData+1,x
    sta waitFrames
    inx
    inx
    stx scriptPointer
    rts
  }
testWaitUntil:
  cmp #WaitUntil
  bne testSine
  {
    lda scriptData+1,x
    sta script.waitHi
    lda scriptData+2,x
    sta script.waitLo
    lda #3
    sta script.waitUntil
    clc
    adc scriptPointer
    sta scriptPointer
    rts
  }
testSine:
  cmp #Sine
  bne testAccel
  {
    inc irq.useSine
    inc scriptPointer
    jmp script
  }
testAccel:
  cmp #2
  bne testSpeed
  {
    lda scriptData+1,x
    sta scrollY.accelLow
    lda scriptData+2,x
    sta scrollY.accelHigh

    inx
    inx
    inx
    stx scriptPointer
    jmp script
  }
testSpeed:
  cmp #3
  bne testFlash
  {
    lda scriptData+1,x
    sta ySpeed
    lda scriptData+2,x
    sta ySpeedLow

    inx
    inx
    inx
    stx scriptPointer
    jmp script
  }
testFlash:
  cmp #4
  bne testMorph
  {
    lda scriptData+1,x
    sta flash.flashType

    inx
    inx
    stx scriptPointer
    jmp script
  }
testMorph:
  cmp #5
  bne testRelect
  {
    lda writeRows.morph
    eor #1
    sta writeRows.morph
    inc scriptPointer
    jmp script
  }
testRelect:
  cmp #Reflect
  bne testEnd
  {
    lda plotter.reflect
    eor #1
    sta plotter.reflect
    inc scriptPointer
    jmp script
  }

testEnd:
  // end of the script?
  cmp #$ff
  bne endScript

  lda #(restartScript-scriptData)
  sta scriptPointer

  #if AS_SPINDLE_PART
    lda #$fd
    sta $dc00
    lda $dc01
    bpl reset      // branch if shift is NOT pressed
    inc nextpart   // only skip to the next part if shift is not pressed
    rts
  #endif
reset:
  // reset
  lda #$ff
  sta scrollLow
  lda #0
  sta scrollLowLow
  sta ySpeedLow
  lda #0
  sta ySpeed
  lda #center
  sta xScroll
  lda #20
  sta theOneIrq.yScroll

  // make straight
  lda #12
  sta writeRows.morphPhase
  jmp writeRows.write

endScript:
  rts
}

scriptData:
restartScript:
  .byte WaitUntil, >$0da0, <$0da0

  .byte 3,4,0       // Y speed 4
  .byte Wait,4
  .byte Flash,2     // fade in

  .byte Wait,75     // wait for fade in to finish
  .byte 2,$e8,$ff   // decelerate
  .byte Wait,41     // $400/$28=$19 (wait until full stop)

  .byte 3,0,0       // set speed to 0
  .byte 2,0,0       // set accel to 0
  .byte Morph       // morph

  .byte Wait,50     // wait until morph is finished..

  .byte 2,$28,$00   // accelerate again
  .byte Wait,25     // wait until speed 4
  .byte 3,4,0       // set speed to 4
  .byte 2,0,0       // set accel to 0 

  .byte Wait,69     // wait correct # of frames so the picture is vertically centered again after the next deceleration

  .byte 2,$d8,$ff   // decelerate again
  .byte Wait,25     // wait until speed 0
  .byte 3,0,0       // set speed to 4
  .byte 2,0,0       // set accel to 0  

  .byte Wait,25
  .byte Sine        // turn sine on

  .byte Wait,64      
  .byte 2,$10,$00  // accelerate up
  
  .byte Wait,10
  .byte Flash,1    // flash
  .byte Wait,118

  .byte 2,$f0,$ff  // accelerate down

  .byte Wait,128
  .byte Morph
  .byte Wait,52    // wait until straight
  .byte Morph      // morph again (other direction)
  .byte Wait,127-52
  .byte Wait,48

  .byte 2,$18,$00  // accelerate up  
  .byte Wait,128

  .byte 2,$00,$00  // speed 0
  .byte 3,$00,$00

  .byte Wait,7

  .byte Sine        // turn sine off

  .byte Wait,50     // wait until morph is finished..
  .byte Reflect
    .byte Flash,1
  .byte Sine
  .byte 2,$10,$00
  .byte Wait,95
  .byte 2,$f0,$ff
  .byte Wait,95
  .byte 2,0,0
  .byte Wait,63
  .byte Sine

  // wait until music fadeout
  .byte WaitUntil, >$12a8, <$12a8

  .byte 2,$20,$00   // accelerate again
  .byte Wait,16     // wait until speed 2.5 (was 4.. fadeout a bit slower to much music)
  .byte 2,0,0       // set accel to 0 
  .byte 3,2,$00     // set speed to 3

  .byte Flash,3     // fade out
  .byte Wait,102

  .byte End   // end

scrollLow:
  .byte $ff
scrollLowLow:
  .byte 0

xScroll:
  .byte center // start centered
ySpeed:
  .byte 0
ySpeedLow:
  .byte 0

flash:
{
flashUp:
  lda previousY: #0
  cmp theOneIrq.yScroll
  beq dontMove

  lda theOneIrq.yScroll
  sta previousY

  dec index1
dontMove:
  ldx index1: #(pred022Tab-pred021Tab)
  bpl !+
  inc colormode
  jmp endFlash
!:
  // 7,a,8
  // 7,f,a,8,2,9,0

  lda colormode: #0
  bne greenToPink

  lda #$09
  sta pred021Tab+0,x
  lda #$08
  sta pred022Tab+0,x
  lda #$0a
  sta pred023Tab+0,x

  lda #$00
  sta pred021Tab+1,x
   sta pred021Tab+2,x
  lda #$02
  sta pred022Tab+1,x
   sta pred023Tab+2,x
  lda #$08
  sta pred023Tab+1,x

  //lda #$00
  //sta pred021Tab+2,x
  lda #$09
  sta pred022Tab+2,x
  //lda #$02
  //sta pred023Tab+2,x

  lda #$00
  sta pred021Tab+3,x
  //lda #$00
  sta pred022Tab+3,x
  //lda #$00
  sta pred023Tab+3,x

  //lda #$00
  sta pred021Tab+4,x
  lda #$06
  sta pred022Tab+4,x
  lda #$0b
  sta pred023Tab+4,x

  lda #$00
  sta pred021Tab+5,x
  lda #$0b
  sta pred022Tab+5,x
  lda #$05
  sta pred023Tab+5,x

  lda #$0b
  sta pred021Tab+6,x
  lda #$05
  sta pred022Tab+6,x
  lda #$03
  sta pred023Tab+6,x

  lda #$b //col1
  sta pred021Tab+7,x  
  lda #$5 //col2
  sta pred022Tab+7,x
  lda #$d //col3
  sta pred023Tab+7,x

  // some frames we move 2 rows up, so we have to change back an extra row
  lda #$b //col1
  sta pred021Tab+8,x  
  lda #$5 //col2
  sta pred022Tab+8,x
  lda #$d //#col3
  sta pred023Tab+8,x

  rts
greenToPink:
  // green -> pink

  lda #$0b
  sta pred021Tab+0,x
  lda #$05
  sta pred022Tab+0,x
  lda #$03
  sta pred023Tab+0,x

  lda #$00
  sta pred021Tab+1,x
  lda #$0b
  sta pred022Tab+1,x
  lda #$05
  sta pred023Tab+1,x

  lda #$00
  sta pred021Tab+2,x
  lda #$06
  sta pred022Tab+2,x
  lda #$0b
  sta pred023Tab+2,x

  lda #$00
  sta pred021Tab+3,x
  lda #$00
  sta pred022Tab+3,x
  lda #$00
  sta pred023Tab+3,x

  lda #$00
  sta pred021Tab+4,x
  lda #$06
  sta pred022Tab+4,x
  lda #$02
  sta pred023Tab+4,x

  lda #$00
  sta pred021Tab+5,x
  lda #$06
  sta pred022Tab+5,x
  lda #$04
  sta pred023Tab+5,x

  lda #$06
  sta pred021Tab+6,x
  lda #$04
  sta pred022Tab+6,x
  lda #$08
  sta pred023Tab+6,x

  lda #$6 //col1
  sta pred021Tab+7,x  
  lda #$4 //col2
  sta pred022Tab+7,x
  lda #$a //col3
  sta pred023Tab+7,x

  // some frames we move 2 rows up, so we have to change back an extra row
  lda #$6 //col1
  sta pred021Tab+8,x  
  lda #$4 //col2
  sta pred022Tab+8,x
  lda #$a //#col3
  sta pred023Tab+8,x

  rts

endFlash:
  lda #0
  sta flashType

  lda #(pred022Tab-pred021Tab)
  sta index1
  sta index2
  sta index3
  rts

flash:
  lda flashType: #0
  cmp #1
  bne !+
  jmp flashUp
!:
  cmp #3
  beq fadeOut
  cmp #2
  bne endFadeIn

fadeIn:
  lda previousIn: #0
  cmp theOneIrq.yScroll
  beq endFadeIn

  lda theOneIrq.yScroll
  sta previousIn

  dec index2

  ldx index2: #(pred022Tab-pred021Tab)
  bmi endFlash

  lda #col1
  sta pred023Tab+0,x
  sta pred022Tab+1,x
  sta pred021Tab+2,x

  lda #col2
  sta pred023Tab+1,x
  sta pred022Tab+2,x

  lda #col3
  sta pred023Tab+2,x
endFadeIn:
  rts

fadeOut:
  lda previous: #0
  cmp theOneIrq.yScroll
  beq endFadeOut

  lda theOneIrq.yScroll
  sta previous

  dec index3

  ldx index3: #(pred022Tab-pred021Tab)
  bmi endFlash

#if !CHANGECOLORS
  lda #background
  sta pred021Tab+0,x
  sta pred022Tab+1,x
  sta pred023Tab+2,x

  lda #col1
  sta pred022Tab+0,x
  sta pred023Tab+1,x

  lda #col2
  sta pred023Tab+0,x
#endif
#if CHANGECOLORS
  lda #background
  sta pred021Tab+0,x
  sta pred022Tab+1,x
  sta pred023Tab+2,x

  lda #$9 // b //col1
  sta pred022Tab+0,x
  sta pred023Tab+1,x

  lda #$4 // 5 //col2
  sta pred023Tab+0,x
#endif

endFadeOut:
  rts
}

scrollY:
{
  lda ySpeedLow
  clc
  adc accelLow:  #$0
  sta ySpeedLow
  
  lda ySpeed
  adc accelHigh: #0
  sta ySpeed

  lda scrollLowLow
  sec
  sbc ySpeedLow
  sta scrollLowLow

  lda scrollLow
  sbc ySpeed
  tax
  and #$07
  sta scrollLow

  cpx #$80
  bcs scrollUp
  cpx #$08
  bcs scrollDown
  rts

scrollUp:
  txa
  eor #$ff
  lsr
  lsr
  lsr
  sta add

  lda theOneIrq.yScroll
  sec
  adc add: #1
  cmp #24
  bne !+
  lda #0
!:
  sta theOneIrq.yScroll
  rts

scrollDown:
  txa
  lsr
  lsr
  lsr
  sta subtract
  
  lda theOneIrq.yScroll
  sec
  sbc subtract: #1
  bpl !+
  //clc
  adc #24
!:
  sta theOneIrq.yScroll
  rts
}

*=* "[CODE] scroll X code"
plotter:
{
  .if (debug==1) inc $d020
  ldx xScroll
  ldy xScroll

  lda reflect: #0
  beq dontReflect

  lda #(breedte*4)-1
  sec
  sbc xScroll
  sec
  sbc #25   // i should update this value and calculate it dynamically, but i can't be bothered.
  bcs !+
  clc
  adc #(breedte*4)
!:
  tay
dontReflect:

  // write xScroll
  .for (var row=0; row<4; row++) 
  {
    .var row1 = row-0
    stx loop1[row1].scrollRead     // low byte of scrolledChars
    sty loop1[row1].scrollRead20
  } 
  .for (var row=4; row<8; row++) 
  {
    .var row1 = row-4
    stx loop2[row1].scrollRead     // low byte of scrolledChars
    sty loop2[row1].scrollRead20
  }
  .for (var row=8; row<13; row++) 
  {
    .var row1 = row-8
    stx loop3[row1].scrollRead     // low byte of scrolledChars
    sty loop3[row1].scrollRead20
  }
  .for (var row=21; row<25; row++) 
  {
    .var row1 = row-21
    stx loop4[row1].scrollRead     // low byte of scrolledChars
    sty loop4[row1].scrollRead20
  }

  // --------------------------------------------
  // now write the correct screens to the plotter
  // --------------------------------------------

  ldy theOneIrq.yScroll

  // the first batch is only the top 4 rows to avoid update bugs (racing the beam)
  // rows 0,1,2,3
  .for (var row=0; row<4; row++) 
  {
    .var row1 = row-0

    lda screenTab+row,y           // $00(00),$00(28),$00(50),$00(78)
    sta loop1[row1].screen+1       // high byte of screen
    sta loop1[row1].screen20+1
  } 

  // next are rows 4,5,6,7 and 17,18,19,20
  .for (var row=4; row<8; row++) 
  {
    .var row1 = row-4
    .var row2 = 24-row

    lda screenTab+row,y            // $00a0,$00c8,$00f0,$0118
    .if ((>(row*40))==1) ora #1
    sta loop2[row1].screen+1       // high byte of screen
    .if ((>(row*40+20))>(>(row*40))) ora #1
    sta loop2[row1].screen20+1

    lda screenTab+row2,y           // 24-4*40 = $320, $2f8, $2d0, $2a8
    .if ((>(row2*40))==1) ora #1
    .if ((>(row2*40))==2) ora #2
    .if ((>(row2*40))==3) ora #3
    sta loop2[row1].screen_ref+1
    .if ((>(row2*40+20))>(>(row2*40))) ora #$01  // #$02 -> #$03
    sta loop2[row1].screen20_ref+1
  } 

  // next are rows 8,9,10,11,12, 13,14,15,16
  .for (var row=8; row<13; row++) 
  {
    .var row1 = row-8
    .var row2 = 24-row

    lda screenTab+row,y            // 8:$0140, 9:$168, 10:$190, 11:$1b8, 12:$1e0
    .if ((>(row*40))==1) ora #1
    sta loop3[row1].screen+1       // high byte of screen
    sta loop3[row1].screen20+1

    .if (row!=12)
    {
      lda screenTab+row2,y           // (24-8)*40 = $280, $258, $230, $208, $1e0
      .if ((>(row2*40))==1) ora #1
      .if ((>(row2*40))==2) ora #2
      .if ((>(row2*40))==3) ora #3
      sta loop3[row1].screen_ref+1
      sta loop3[row1].screen20_ref+1
    }
  } 

  // last are rows 21,22,23,24
  .for (var row=21; row<25; row++) 
  {
    .var row1 = row-21

    lda screenTab+row,y           // $348, $370, $398, $3c0
    .if ((>(row*40))==1) ora #1
    .if ((>(row*40))==2) ora #2
    .if ((>(row*40))==3) ora #3
    sta loop4[row1].screen+1       // high byte of screen
    sta loop4[row1].screen20+1
  } 

  // write top 4 rows
  ldy #19

loop1: .for (var row=0; row<4; row++)
  {

    ldx proto:        screenPrototype+12*64,y
    lda scrollRead:   scrolledChars,x
    sta screen:       screen1+row*40,y
    ldx proto20:      screenPrototype+12*64+20,y
    lda scrollRead20: scrolledChars,x
    sta screen20:     screen1+row*40+20,y
  }
  dey
  bpl loop1
endLoop1:

  // write middle
  ldy #19
loop2: .for (var row=4; row<8; row++)
  {
    ldx proto:        screenPrototype+12*64,y
    lda scrollRead:   scrolledChars,x
    sta screen:       screen1+row*40,y
    sta screen_ref:   screen1+((24-row)*40),y  // write the y reflected screen

    ldx proto20:      screenPrototype+12*64+20,y
    lda scrollRead20: scrolledChars,x
    sta screen20:     screen1+row*40+20,y
    sta screen20_ref: screen1+((24-row)*40)+20,y
  }
  dey
  bpl loop2
endLoop2:

  // write middle
  ldy #19
loop3: .for (var row=8; row<13; row++)
  {
    ldx proto:        screenPrototype+12*64,y
    lda scrollRead:   scrolledChars,x
    sta screen:       screen1+row*40,y
    .label screen_ref = *+1
    .if (row!=12)
    {
      sta screen1+((24-row)*40),y
    }

    ldx proto20:      screenPrototype+12*64+20,y
    lda scrollRead20: scrolledChars,x
    sta screen20:     screen1+row*40+20,y
    .label screen20_ref = *+1
    .if (row!=12)
    {
      sta screen1+((24-row)*40+20),y
    }
  }
  dey
  bpl loop3
endLoop3:

  // write bottom 4 rows
  ldy #19
loop4: .for (var row=21; row<25; row++)
  {
    ldx proto:        screenPrototype+(24-12)*64,y
    lda scrollRead:   scrolledChars,x
    sta screen:       screen1+row*40,y

    ldx proto20:      screenPrototype+(24-12)*64+20,y
    lda scrollRead20: scrolledChars,x
    sta screen20:     screen1+row*40+20,y
  }
  dey
  bpl loop4
endLoop4:

  .if (debug==1) dec $d020
  rts
}

.var Adresses   = List().add(plotter.loop1[0].proto,   plotter.loop1[1].proto,   plotter.loop1[2].proto,   plotter.loop1[3].proto,
                             plotter.loop2[0].proto,   plotter.loop2[1].proto,   plotter.loop2[2].proto,   plotter.loop2[3].proto,
                             plotter.loop3[0].proto,   plotter.loop3[1].proto,   plotter.loop3[2].proto,   plotter.loop3[3].proto,   plotter.loop3[4].proto,
                             plotter.loop4[0].proto,   plotter.loop4[1].proto,   plotter.loop4[2].proto,   plotter.loop4[3].proto)    
.var Adresses20 = List().add(plotter.loop1[0].proto20, plotter.loop1[1].proto20, plotter.loop1[2].proto20, plotter.loop1[3].proto20,
                             plotter.loop2[0].proto20, plotter.loop2[1].proto20, plotter.loop2[2].proto20, plotter.loop2[3].proto20,
                             plotter.loop3[0].proto20, plotter.loop3[1].proto20, plotter.loop3[2].proto20, plotter.loop3[3].proto20, plotter.loop3[4].proto20,
                             plotter.loop4[0].proto20, plotter.loop4[1].proto20, plotter.loop4[2].proto20, plotter.loop4[3].proto20)  

.var Adresses_old = List().add(plotter.loop1+1,        plotter.loop1+1+18,      plotter.loop1+1+2*18,     plotter.loop1+1+3*18,
                               plotter.loop2+1+(0*12), plotter.loop2+1+(1*12), plotter.loop2+1+(2*12), plotter.loop2+1+(3*12),
                               plotter.loop3+1+(0*12), plotter.loop3+1+(1*12), plotter.loop3+1+(2*12), plotter.loop3+1+(3*12), plotter.loop3+1+(4*12),
                               plotter.loop4+1,        plotter.loop4+1+9,      plotter.loop4+1+18,     plotter.loop4+1+27)

writeRows:
{
  lda morph: #0
  bne !+
  rts
!:
  lda wait: #0
  beq !+
  dec wait
  rts
!:
  lda #3
  sta wait

  lda direction: #0
  beq toMorph

  lda morphPhase
  cmp #12
  bne !+
  // we are back to straight.. switch the morph direction
  ldx #11
loop:
  lda prototypesLow2,x
  sta prototypesLow1,x
  lda prototypesHigh2,x
  sta prototypesHigh1,x
  dex
  bpl loop

  // arrived at the end of the morphing
endMorph:
  lda #0
  sta morph
swapDirection:
  lda direction
  eor #1
  sta direction
  jmp write
!:
  inc morphPhase
  jmp write

toMorph:
  lda morphPhase
  beq endMorph
  dec morphPhase

write:
  .if (debug==1) inc $d020

  ldy morphPhase: #12     // 0 = totally morphed, 12 = straight
  .for (var i=0; i<13; i++)
  {
    lda prototypesLow1+i,y
    sta Adresses.get(i)
    clc
    adc #20
    sta Adresses20.get(i)
    lda prototypesHigh1+i,y
    sta Adresses.get(i)+1
    adc #0
    sta Adresses20.get(i)+1
  }

  // fix the last couple of rows which are not mirrored in the plot routine. nasty shit.
  lda Adresses.get(0)
  sta Adresses.get(16)
  lda 1+Adresses.get(0)
  sta 1+Adresses.get(16)

  lda Adresses.get(1)
  sta Adresses.get(15)
  lda 1+Adresses.get(1)
  sta 1+Adresses.get(15)

  lda Adresses.get(2)
  sta Adresses.get(14)
  lda 1+Adresses.get(2)
  sta 1+Adresses.get(14)

  lda Adresses.get(3)
  sta Adresses.get(13)
  lda 1+Adresses.get(3)
  sta 1+Adresses.get(13)

  // and the +20 offset

  lda Adresses20.get(0)
  sta Adresses20.get(16)
  lda 1+Adresses20.get(0)
  sta 1+Adresses20.get(16)

  lda Adresses20.get(1)
  sta Adresses20.get(15)
  lda 1+Adresses20.get(1)
  sta 1+Adresses20.get(15)

  lda Adresses20.get(2)
  sta Adresses20.get(14)
  lda 1+Adresses20.get(2)
  sta 1+Adresses20.get(14)

  lda Adresses20.get(3)
  sta Adresses20.get(13)
  lda 1+Adresses20.get(3)
  sta 1+Adresses20.get(13)
  rts
}

d018Tab:
.for (var i=0; i<(charsets.size()-1); i++) .byte (16*((screens.get(i)&$3fff)/$0400))|(2*((charsets.get(i)&$3800)/$800))
.for (var i=0; i<(charsets.size()-1); i++) .byte (16*((screens.get(i)&$3fff)/$0400))|(2*((charsets.get(i)&$3800)/$800))

dd02Tab:
.for (var i=0; i<(charsets.size()-1); i++) .byte $3c|((charsets.get(i))&$c000)/$4000
.for (var i=0; i<(charsets.size()-1); i++) .byte $3c|((charsets.get(i))&$c000)/$4000

screenTab:
.for (var i=0; i<(charsets.size()-1); i++) .byte >(screens.get(i))
.for (var i=0; i<(charsets.size()-1); i++) .byte >(screens.get(i))

pred021Tab:
  .byte 0,0,0,0,0,0,0,0,0
d021Tab:
  .fill 25,0
pred022Tab:
  .byte 0,0,0,0,0,0,0,0,0
d022Tab:
  .fill 25,0
pred023Tab:
  .byte 0,0,0,0,0,0,0,0,0
d023Tab:
  .fill 25,0
  .byte 0,0,0,0,0,0,0

charsetTabHi:
  .for (var charset=0; charset<charsets.size(); charset++) .byte >(charsets.get(charset)) 

*=* "[DATA] xsine"
sineX:
{
    .var sinSize = 256
    .var sinMin  = center
    .var sinMax  = center + (breedte*4)*2
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .fill sinSize, mod(((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize))),breedte*4)
}

.var rowStarts = List()
*=* "[DATA] prototype"
screenPrototype:
.for (var row=0; row<25; row++)
{
  // skip page if this row will cross a page
  .if ((>(*+0))<(>(*+39))) .align $100

  .eval rowStarts.add(*)

  .var fromVCenter = (12-row)
  .var shrink = 2*(cos(fromVCenter/40*PI)-1)
  .if (row>12) .eval shrink = 0-(shrink*1.5)

  .for (var charx=0; charx<40; charx++)
  {
    .var sign = 1
    .var fromHCenter = (20-charx)
    .var absFromHCenter = abs(20-charx)
    .if (fromHCenter!=0)   .eval sign = fromHCenter/absFromHCenter

    .var value = (charx*4) - sign*round(40*(12*shrink*sin((absFromHCenter*pow(absFromHCenter,0.5)/20/360)*PI)))
    
    .if (value<0) .eval value = value+(breedte*4)
    .byte mod(value, breedte*4)
  }

  //.fill 24,0  // make every data row 64 bytes to avoid page crossing
}
.print (rowStarts)

prototypesLow1:
  .fill 12,<(rowStarts.get(i))  // screenPrototype+i*64)       // 0 = morphed, 12 = straight
  .fill 13,<(rowStarts.get(12)) // screenPrototype+12*64)      // 12 = straight
prototypesLow2:
  .fill 12,<(rowStarts.get(12-i+12)) // screenPrototype+((12-i)+12)*64)  // morph other direction

prototypesHigh1:
  .fill 12,>(rowStarts.get(i))       // screenPrototype+i*64)
  .fill 13,>(rowStarts.get(12))      // screenPrototype+12*64)
prototypesHigh2:
  .fill 12,>(rowStarts.get(12-i+12)) // screenPrototype+((12-i)+12)*64)

// virtual stuff goes here

*=scrolledChars "[DATA] scrolled chars" virtual
  .fill 512,0

* = buffer "[BUFFER]" virtual
  .fill (breedte+1)*8,0

* = charset01 "[GFX] charset01" virtual
  .fill 6*$100,0
* = charset02 "[GFX] charset02" virtual
  .fill 6*$100,0
* = charset03 "[GFX] charset03" virtual
  .fill 6*$100,0
* = charset04 "[GFX] charset04" virtual
  .fill 6*$100,0
* = charset05 "[GFX] charset05" virtual
  .fill 6*$100,0
* = charset06 "[GFX] charset06" virtual
  .fill 6*$100,0
* = charset07 "[GFX] charset07" virtual
  .fill 6*$100,0
* = charset08 "[GFX] charset08" virtual
  .fill 6*$100,0
* = charset09 "[GFX] charset09" virtual
  .fill 6*$100,0
* = charset10 "[GFX] charset10" virtual
  .fill 6*$100,0
* = charset11 "[GFX] charset11" virtual
  .fill 6*$100,0
* = charset12 "[GFX] charset12" virtual
  .fill 6*$100,0
* = charset13 "[GFX] charset13" virtual
  .fill 6*$100,0
* = charset14 "[GFX] charset14" virtual
  .fill 6*$100,0
* = charset15 "[GFX] charset15" virtual
  .fill 6*$100,0
* = charset16 "[GFX] charset16" virtual
  .fill 6*$100,0
* = charset17 "[GFX] charset17" virtual
  .fill 6*$100,0
* = charset18 "[GFX] charset18" virtual
  .fill 6*$100,0
* = charset19 "[GFX] charset19" virtual
  .fill 6*$100,0
* = charset20 "[GFX] charset20" virtual
  .fill 6*$100,0
* = charset21 "[GFX] charset21" virtual
  .fill 6*$100,0
* = charset22 "[GFX] charset22" virtual
  .fill 6*$100,0
* = charset23 "[GFX] charset23" virtual
  .fill 6*$100,0
* = charset24 "[GFX] charset24" virtual
  .fill 6*$100,0

* = screen1 "[GFX] screen1" virtual
  .fill 1000,0
* = screen2 "[GFX] screen2" virtual
  .fill 1000,0
* = screen3 "[GFX] screen3" virtual
  .fill 1000,0
* = screen4 "[GFX] screen4" virtual
  .fill 1000,0

* = $b600 "[SPINDLE] driver" virtual
  .fill 64,0

.assert "loop1",(>plotter.endLoop1)-(>plotter.loop1),0
.assert "loop2",(>plotter.endLoop2)-(>plotter.loop2),0
.assert "loop3",(>plotter.endLoop3)-(>plotter.loop3),0
.assert "loop4",(>plotter.endLoop4)-(>plotter.loop4),0
