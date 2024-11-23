.var debug   = 0
.var middle  = $50
.var col1 = $9
.var col2 = $5
.var col3 = $f

.var music = LoadSid("../music/tubular.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .var PLAY=$1003
  .label spindleLoadAddress = sprites
  *=spindleLoadAddress-18-2-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word 0             // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word irq.MusicPlayCall // location of playroutine call

    .byte 'A'                                 // load only what is needed in the next part (avoid loading in between the little gaps)
    .byte 'S'                                 // this part is IO safe and can load below $d000

    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart

#else    
    :BasicUpstart2(startStandalone)
#endif

.var xPos  = List()
.var yPos  = List()
.var slopeHi = List()
.var slopeLo = List()
.var r = 120 // 4*42  // the rope is 4 y-expanded sprites high
.var xMin = 0

.for (var i=0; i<128; i++)
{
  .var sinSize = 128
  .var sinMin  = -20
  .var sinMax  = 20
  .var sinAmp  = 0.5 * (sinMax-sinMin)

  .var angle = ((sinMin+sinAmp) + sinAmp*sin(toRadians(i*360/sinSize)))

  .var x = sin(toRadians(angle)) * r
  .var y = cos(toRadians(angle)) * r
  .eval xPos.add(round(x))
  .eval yPos.add(round(y))

  .if (round(x)<xMin) .eval xMin = round(x)

  // calculate the slope..
  .var slope = 0
  .if (x!=0)
  {
    .eval slope = 2*y/x  // 2* -> we move in multicolor pixels..
  }
  .if (slope>128) .eval slope=0
  .if (slope<-127) .eval slope=0

  .if (slope < 0) .eval slope = 0-slope

  .var slo1 = floor(slope)
  .var slo2 = ((slope*256)&$ff)

  .eval slopeHi.add(slo1)
  .eval slopeLo.add(slo2)
}

// these are the demo-spanning 0 page adresses!
.label nextpart = $02
.label timelow  = $03
.label timehigh = $04

.label loader0 = $05
.label loader1 = $06
.label loader2 = $07
.label loader3 = $08
.label loader4 = $09

.label startStandalone = $4000
.label sprites = $f840
.label screen  = $f800
.label start   = $fc00

#if !AS_SPINDLE_PART
  * = startStandalone "[code] start from basic"
  sei
  lda #$35
  sta $01
  jmp start
#endif

*=sprites "[GFX] sprites"
.import binary "./includes/oogbal.bin"
.fill 21,[0,0,%00000011] // touwtje sprite

sinX:
  .fill xPos.size(), xPos.get(i)
sinY:
  .fill yPos.size(), yPos.get(i)
skipY:
  .fill slopeHi.size(), slopeHi.get(i)
skipYLo:
  .fill slopeLo.size(), slopeLo.get(i)
// be careful of the sprite pointers here!! check with an assertion
.assert "spritepointer check", *<screen+$3f8, true

*=start "[CODE] main code"
{
  sei
  lda $01
  sta restore01

  lda #$35
  sta $01

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d
  lda #<irq
  sta $fffe
  lda #>irq
  sta $ffff
  lda #$ff
  sta $d012
  lda #$0b // turn off screen
  sta $d011

  lda #0
  sta $d020
  sta $d021
  sta $d022
  sta $d023

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
    lda #$3f
    sta $dd02

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
    mainloop:
    jmp mainloop
  #else
    rts
  #endif
}

setSprites:
{
  lda #$ff
  sta $d015
  sta $d01c // multicolor

  lda #0
  sta $d01d // x stretch
  sta $d01b // priority
  lda #$f0
  sta $d017 // y stretch

  lda #col1
  sta $d025
  lda #col2
  sta $d026
  lda #col3
  sta $d027
  sta $d028
  sta $d029
  sta $d02a

  lda #col3
  sta $d02b
  sta $d02c
  sta $d02d
  sta $d02e

  ldx #(sprites&$3fff)/64
  stx screen+$3f8
  inx
  stx screen+$3f9
  inx
  stx screen+$3fa
  inx
  stx screen+$3fb

  inx
  stx screen+$3fc
  stx screen+$3fd
  stx screen+$3fe
  stx screen+$3ff

  // linear movement (speed 0.75)
  inc wait
  lda wait: #0
  and #3
  beq !+

  inc xlin
  bne !+
  inc xlinh
!:
  // copy linear movement to position for the current frame
  lda xlin
  sta xposl
  lda xlinh
  sta xposh

  // swinging motion
  inc phase
  lda phase
  cmp #(xPos.size()-1)
  bne !+
  lda #0
  sta phase
!:

  lda phase: #0
  clc
  adc #$60
  cmp #(xPos.size())
  bcc !+
  sec
  sbc #(xPos.size())
!:
  tax

  lda sinX,x
  sec
  sbc #xMin  // make it positive
  lsr
  lsr
  clc
  adc xposl
  sta xposl
  lda xposh
  adc #0
  sta xposh 

  lda xposl
  clc
  adc #(0-xMin)
  sta updateX.xposl_rope
  lda xposh
  adc #0
  sta updateX.xposh_rope

  ldx phase
  ldy #2
  lda sinX,x
  bpl !+
  ldy #$fe
!:
  sty updateX.addX
  
  sec
  sbc #xMin  // make it positive

  // calc total x position of eyeball
  clc
  adc xposl
  sta xposl_eyeball

  lda xposh
  adc #0
  sta xposh_eyeball

  lda xposl_eyeball
  clc
  adc #$18
  sta xposl_eyeball+1
  lda xposh_eyeball
  adc #0
  sta xposh_eyeball+1

  lda xposl_eyeball
  sta $d000
  sta $d004
  lda xposl_eyeball+1
  sta $d002
  sta $d006

  // calculate $d010 

  lda #0
  ldy xposh_eyeball
  beq !+
    // if xposh > 1, set $d010 to $1|$4
    ora #$05
  !:
  ldy xposh_eyeball+1
  beq !+
    ora #$0a
  !:
  sta $d010

  // make eyeball invisible if it should be
  lda xposh_eyeball
  cmp #2
  bcc !+
    lda #$ff
    sta $d000
    sta $d004
  !:
  lda xposh_eyeball+1
  cmp #2
  bcc !+
    lda #$ff
    sta $d002
    sta $d006
  !:

  lda sinY,x
  clc
  adc #$32
  sta $d001
  sta $d003
  clc
  adc #21
  sta $d005
  sta $d007

  lda skipYLo,x
  sta slopeLow
  lda skipY,x
  sta slopeHigh
 
  // x position of rope
  lda updateX.xposl_rope
  sta $d008
  sta $d00a
  sta $d00c
  sta $d00e

  // set start Y of rope
  lda $d001
  sec
  sbc #42
  sta $d009
  sec
  sbc #42
  sta $d00b
  sec
  sbc #42
  sta $d00d
  sec
  sbc #42
  sta $d00f

  // set d010 of rope
  lda $d010
  ldy updateX.xposh_rope
  beq !+
    ora #$f0
  !:
  sta $d010

  // hide rope if needed
  lda updateX.xposh_rope
  cmp #2
  bcc !+
    lda #$ff
    sta $d008
    sta $d00a
    sta $d00c
    sta $d00e
  !:

  // set addXh
  ldy #0
  lda updateX.addX
  bpl !+
  dey
  !:
  sty updateX.addXh

  rts
}

xlin: 
  .byte $c0
xlinh:
  .byte $ff
xposl:
  .byte $c0
xposh:
  .byte $ff
xposl_eyeball:
  .byte 0,0
xposh_eyeball:
  .byte 0,0

irq:
{
  sta restorea
  stx restorex
  sty restorey

  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #$1b
  sta $d011
  lda #$d8
  sta $d016

  jsr setSprites

  // set first y skip
  lda slopeHigh
  lsr
  sta yPosHigh
  lda slopeLow
  ror
  sta yPosLow

  lda yPosHigh
  clc
  adc #$32
  sta yPosHigh

  lda #((screen&$3fff)/$400)*$10
  sta $d018
  lda #$3f
  sta $dd02

  lda #$30
  sta $d012

  lda #<slopeIrq
  sta $fffe
  lda #>slopeIrq
  sta $ffff

  asl $d019

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

  lda WaitLo
  bne !+
  dec WaitHi
  bpl !+

  inc nextpart
!:
  dec WaitLo

  lda restore01: #0
  sta $01

  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}
WaitLo:
  .byte 0
WaitHi:
  .byte 2

slopeLow:
  .byte 0
slopeHigh:
  .byte 0
yPosLow:
  .byte 0
yPosHigh:
  .byte 0

slopeIrq:
{
  sta restorea
  stx restorex
  sty restorey
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda slopeLow
  ora slopeHigh
  bne setSlope

  // vertical line, skip the slope code
  lda #<irq
  sta $fffe
  lda #>irq
  sta $ffff
  lda #$ff
  sta $d012
  jmp endIrq
setSlope:
  jsr updateX

  lda yPosHigh
  sta $d012
  lda #<xIrq
  sta $fffe
  lda #>xIrq
  sta $ffff

endIrq:
  asl $d019
  lda restore01: #0
  sta $01
  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}

xIrq:
{
  sta restorea

  lda $01
  sta restore01
  lda #$35
  sta $01
  lda $d012

  !: cmp $d012
  beq !- 

  // move sprites
  lda d010: #0
  sta $d010

  lda pos_rope: #0
  sta $d008
  sta $d00a
  sta $d00c
  sta $d00e

  jsr updateX

  // calculate next y position
  lda yPosLow
  clc
  adc slopeLow
  sta yPosLow
  lda yPosHigh
  adc slopeHigh
  sta yPosHigh
  bcs toIrq
  cmp #($32+r)
  bcs toIrq

  // we kan keep fffe/ffff
  sta $d012 // return to this interrupt, but a bit later

endIrq:
  asl $d019
  lda restore01: #0
  sta $01
  lda restorea: #0
  rti

toIrq:
  lda #<irq
  sta $fffe
  lda #>irq
  sta $ffff
  lda #$ff
  sta $d012
  bne endIrq
}

updateX:
{
 // calc new x position
  lda xposl_rope: #0
  clc
  adc addX: #2
  sta xposl_rope
  sta xIrq.pos_rope

  lda xposh_rope: #0
  adc addXh: #0
  sta xposh_rope

  // calculate d010 value
  bne !+
  {
    lda $d010
    and #$0f
    sta xIrq.d010
    bpl continue
  }
  !:
  lda $d010
  ora #$f0
  sta xIrq.d010
continue:

  // hide sprites
  lda xposh_rope
  cmp #2
  bcc !+
  {
    lda #$ff
    sta xIrq.pos_rope
  }
  !:
  rts
}