.var music = LoadSid("../music/intro.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .var PLAY=$1003
  .label spindleLoadAddress = $c400
  *=spindleLoadAddress-18-9-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word prepare       // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word topirq.MusicPlayCall // location of playroutine call

    //.byte 'S'                          // this part is IO safe and can load below $d000
    .byte 'P', >screen, >(screen+$3ff) // we use the screen memory at runtime     
    .byte 'Z', $40, $45
    .byte 'Z', $50, $6f

    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart

#else    
    :BasicUpstart2(startBasic)
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

// these are the 0 page adresses for the part

.label low     = $40
.label high    = $41
.label low2    = $42
.label high2   = $43
.label numcols = $44
.label fadePos = $45

.label col0    = $50
.label col1    = $51
.label col2    = $52
.label col3    = $53
.label col4    = $54
.label col5    = $55
.label col6    = $56
.label col7    = $57
.label col8    = $58
.label col9    = $59
.label cola    = $5a
.label colb    = $5b
.label colc    = $5c
.label cold    = $5d
.label cole    = $5e
.label colf    = $5f

.label col0b   = $60
.label col1b   = $61
.label col2b   = $62
.label col3b   = $63
.label col4b   = $64
.label col5b   = $65
.label col6b   = $66
.label col7b   = $67
.label col8b   = $68
.label col9b   = $69
.label colab   = $6a
.label colbb   = $6b
.label colcb   = $6c
.label coldb   = $6d
.label coleb   = $6e
.label colfb   = $6f

.label irqHook  = $70
.label irqHook2 = $71
.label irqHook3 = $72

.label screen  = $c000
.label code    = $c400
.label bitmap  = $e000

.var koalaTemplate = "loadAddress=0,Bitmap=2,Screen=$1f42,D800=$232a"
.var koala = LoadBinary("/includes/logo.kla", koalaTemplate)
.var background = BLACK

#import "optimize_bitmap.inc"

*=screen "[RUNTIME] actual screencolors" virtual
  .fill $3f8,0
*=bitmap "[DATA] bitmap"
  .fill bitmapData.size(), bitmapData.get(i)

#if !AS_SPINDLE_PART
*=$9000 "[CODE] start from basic (not Spindle)"
startBasic:
  sei
  lda #$35
  sta $01
  jmp start
#endif

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
  #endif

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d
  lda #<irqHook
  sta $fffe
  lda #>irqHook
  sta $ffff
  lda #$00
  sta $d012

  lda #1
  sta $d020
  sta $d021

  #if !AS_SPINDLE_PART
    jsr music.init
  #endif

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
    jmp *
  #else
    rts
  #endif
}

prepare:
{
  #if !AS_SPINDLE_PART
    lda #0
    sta nextpart
  #endif

  lda #16
  sta fadePos

  // set up IRQ hook
  lda #$4c
  sta irqHook
  lda #<topirq
  sta irqHook2
  lda #>topirq
  sta irqHook3
  rts
}

topirq:
{
  pha
  txa
  pha
  tya
  pha
  lda $01
  pha

  lda #$35
  sta $01

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
  #endif

  lda #(((screen&$3fff)/$0400)*$10)+(((bitmap&$3fff)/$2000)*$8)
  sta $d018
  lda #((bitmap&$c000)/$4000)|$3c
  sta $dd02

  // already set by shakeIt
  lda #$d8
  sta $d016
  lda #$37
  sta $d011

  jsr fade
  jsr shakeIt
  jsr speedcode

  // keep demotime (always before calling music player)
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

  // only 1 irq..
  lda #<irqHook
  sta $fffe
  lda #>irqHook
  sta $ffff
  lda #$00
  sta $d012
  asl $d019

  pla
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti
}

fade:
{
  lda fadePos
  bne fadeIn
  lda fadeBlood
  bne endBlood

  // switch to blood fade
  inc fadeBlood
  lda #32
  sta fadePos
  lda #30        // wait time before flickering in the blood
  sta waitFrame
  rts
endBlood:  
  lda #2         // signal bitmap scroller that all color are set
  sta nextpart
  rts

fadeIn: 
  dec waitFrame
  bpl endFade

  lda #1
  sta waitFrame
  dec fadePos

  lda fadeBlood: #0
  bne fade2

  // set background color
  lda posincolorramp+background
  clc
  adc fadePos
  tay
  lda colorramp,y
  sta $d020
  sta $d021

  // set bitmap color
  ldx #$f
loop:
  lda posincolorramp,x  // where in the colorramp is this color?
  clc
  adc fadePos           // add current flash index
  tay
  lda colorramp,y       // the color shows as this current color
  sta col0,x
  asl
  asl
  asl
  asl
  ora #$0f
  sta col0b,x
  dex
  bpl loop

  // fade pink as lgrey
  lda col0+15
  sta col0+10
  lda col0b+15
  sta col0b+10

  // fade purple as mgrey
  lda col0+12
  sta col0+4
  lda col0b+12
  sta col0b+4

  // fade brown as dgrey
  lda col0+11
  sta col0+9
  lda col0b+11
  sta col0b+9

  // replace red with background
  lda col0+background
  sta col2
  lda col0b+background
  sta col2b

endFade:
  rts

fade2:
  // set background color
  ldy fadePos
  lda colorramp2,y
  ora #$f0
  sta col0+2
  sta col0+4
  sta col0+9
  sta col0+10

  asl
  asl
  asl
  asl
  ora #$0f
  sta col0b+2
  sta col0b+4
  sta col0b+9
  sta col0b+10

  lda colorramp2,y
  bne endFade

  lda #$fc
  sta col0+4
  lda #$ff
  sta col0+10
  lda #$fb
  sta col0+9

  lda #$cf
  sta col0b+4
  lda #$ff
  sta col0b+10
  lda #$bf
  sta col0b+9

  rts

waitFrame:
  .byte 0
}

// shake the screen when fading in
shakeIt:
{
  lda fade.fadeBlood
  bne endShake

  lda fadePos  
  bmi endShake

  // multiply by 3
  asl
  adc fadePos

  // add the frame
  clc
  adc fade.waitFrame

  tax
  lda xOff,x
  ora #$d8
  sta $d016
  lda yOff,x
  ora #$38
  sta $d011
endShake:
  rts
}

xOff:
  .byte 0,0,1,0,0,1,0,0,1,2,1,0,0,0,2,2
  .byte 0,0,3,3,4,1,5,0,1,6,5,1,0,0,6,7
  .byte 0,0,4,3,0,0,6,6,1,1,7,7,0,0,8,8
yOff:
  .byte 7,7,6,7,5,4,7,4,3,6,7,3,2,3,4,5
  .byte 2,3,5,4,2,2,3,3,6,7,2,1,4,6,6,4
  .byte 3,1,1,5,7,5,2,3,0,7,6,3,0,1,6,7

posincolorramp:
  .byte 00,15,03,11,05,09,01,13,06,02,10,04,08,14,07,12
colorramp:
  .byte $f0,$f6,$f9,$f2,$fb,$f4,$f8,$fe,$fc,$f5,$fa,$f3,$ff,$f7,$fd,$f1
  .fill 17,$f1
  .byte $f1,$fd,$f7,$ff,$f3,$fa,$f5,$fc,$fe,$f8,$f4,$fb,$f2,$f9,$f6,$f0

colorramp2:
  .byte 2,2,0,2,0,0,0,2
  .byte 2,0,0,2,0,0,0,0
  .byte 2,0,0,0,0,0,0,0
  .byte 2,0,0,0,2,0,2,0,0

*=* "[CODE] unrolled speedcode"
speedcode:

  .var oldXColor = -1

  .for (var color=0; color<16; color++)
  {
    .var colorUsed = 0

    // check if this color is used
    .for (var char=0; char<1000; char++)
    {
      .if (((((d800Data.get(char) >> 4) & 1) == 1) && ((  d800Data.get(char) & $f) == color)) ||
           (( (d800Data.get(char) >> 4)      >= 2) && ((screenData.get(char) & $f) == color)))
      {
          .eval colorUsed=1
          .eval char = 1000
      }
    }

    // if the color is used, then generate speedcode for it..
    .if (colorUsed > 0)
    {
      lda col0+color

      // generate all writes to $d800
      .for (var char=0; char<1000; char++)
      {
        .var nrColors = ((d800Data.get(char))>>4)
        
        .if (((nrColors == 1) || (nrColors == 3))  && (((d800Data.get(char)) & $f) == color)) sta $d800+char
      }  

      // generate all chars with 2 or 3 colors
      .for (var color2=0; color2<16; color2++)
      {
        .for (var char=0; char<1000; char++)
        {
          .var nrColors = ((d800Data.get(char))>>4)
          
          .if ((nrColors >= 2) && (((screenData.get(char)) & $f) == color) && ((screenData.get(char)) >> 4) == color2)
          {
            .var msbColor = ((screenData.get(char)) >> 4)
            .if ((oldXColor != msbColor))
            {
              ldx col0b+msbColor
              .eval oldXColor = msbColor
            }
            sax screen+char
          }
        }
      }  
    }
  }
  rts
