.var music = LoadSid("../music/tubular.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#else 
  *=music.location "[MUSIC]" virtual
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .label spindleLoadAddress = sprites
  *=spindleLoadAddress-18-4-3 "Spindle header"
  .label spindleHeaderStart = *

  .text "EFO2"              // fileformat magic
  .word 0                   // prepare routine
  .word start               // setup routine
  .word irq                 // irq handler
  .word 0                   // main routine
  .word 0                   // fadeout routine
  .word 0                   // cleanup routine
  .word irq.MusicPlayCall   // location of playroutine call

  .byte 'S'                 // declare safe IO
  .byte 'P', >(screen+$3ff), >(screen+$3ff)   // sprite pointers

  .byte 0
  .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart
#else    
    :BasicUpstart2(standAloneStart)
#endif

// these are the demo-spanning 0 page adresses!
.label nextpart = $02
.label timelow  = $03
.label timehigh = $04

.label loader0  = $05
.label loader1  = $06
.label loader2  = $07
.label loader3  = $08
.label loader4  = $09

.label standAloneStart = $4000
.label screen      = $f800
.label sprites     = $f840
.label spritesHigh = $fd00

.var xLocations = List().add( 54, 84, 142, 176, 238, 260)
.var yLocations = List().add( 72, 78, 102, 102,  72,  86)

#if !AS_SPINDLE_PART
* = standAloneStart
  sei
  lda #$35
  sta $01
  jmp start
#endif

* = sprites "[GFX] sprites"
  .import binary "./includes/eyes.bin",(6+5)*$40,(6*$40)

* = * "[CODE] Main"
start:
{
  sei
  lda $01
  sta zp01
  lda #$35
  sta $01

  lda #$1b
  sta $d011

  #if !AS_SPINDLE_PART
    lda #0
    sta $d020
    sta $d021
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

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00

    lda #0
    jsr music.init
  #endif

  lda zp01: #0
  #if !AS_SPINDLE_PART
    cmp #$37
    bne !+
    lda #$35 
  !:
  #endif

  sta $01
  cli
      
  #if !AS_SPINDLE_PART
    jmp *
  #else
    rts
  #endif
}

setSprites:
{
  ldx #((sprites&$3fff)/64)
  stx screen+$3f8
  inx
  stx screen+$3fa
  inx
  stx screen+$3f9
  inx
  stx screen+$3fb
  inx
  stx screen+$3fc
  inx
  stx screen+$3fd

  lda d015: #0
  sta $d015
  sta $d01c
  ldx #0
  stx $d017
  stx $d01b
  stx $d01d

  lda #$01
  sta $d025
  lda #$0a
  sta $d026

  ldx #(6*2)-1
!:
  lda spriteLocations,x
  sta $d000,x
  dex
  bpl !-

  lda #$30
  sta $d010
  rts    
}

spriteLocations:
  .byte xLocations.get(0)+$18,yLocations.get(0)+$32
  .byte xLocations.get(1)+$18,yLocations.get(1)+$32
  .byte xLocations.get(2)+$18,yLocations.get(2)+$32
  .byte xLocations.get(3)+$18,yLocations.get(3)+$32
  .byte xLocations.get(4)+$18,yLocations.get(4)+$32
  .byte xLocations.get(5)+$18,yLocations.get(5)+$32

irq:
{
  pha
  txa
  pha
  tya
  pha
  lda $01
  sta zp01
  lda #$35
  sta $01

  lda #$03   // open the border, but turn the screen off
  sta $d011
  
  jsr script

  inc timelow // keep track of demotime
  bne !+
  inc timehigh
!:

  MusicPlayCall:
  #if !AS_SPINDLE_PART
    jsr music.play
  #else         
    bit.abs $0000
  #endif

  lda #16*((screen&$3fff)/$400)
  sta $d018
  lda #((screen&$c000)/$4000)|$3c
  sta $dd02

  lda high: #0
  beq skip
  lda #<highIrq
  sta $fffe
  lda #>highIrq
  sta $ffff
  lda #$b8
  sta $d012

skip:
  lda #$0b
  sta $d011

  asl $d019

  jsr setSprites

  lda zp01: #0
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti
}

highIrq:
{
  pha
  txa
  pha
  tya
  pha

  lda #$d0
  sta $d001
  sta $d003
  sta $d005
  lda #$c0
  sta $d007
  sta $d009

  lda #$01
  sta $d027
  sta $d028
  sta $d029
  lda #$0a
  sta $d02a
  sta $d02b

  lda #$a4
  sta $d000
  lda #$a4+24
  sta $d002
  lda #$a4+48
  sta $d004

  lda #$a0
  sta $d006
  lda #$d0
  sta $d008

  lda #%00011111
  sta $d015
  lda #0
  sta $d010
  sta $d01b
  sta $d01c
  lda #%00011000
  sta $d01d
  sta $d017
  
  ldx #((spritesHigh&$3fff)/64)
  stx screen+$3fb
  inx
  inx
  stx screen+$3fc
  inx
  inx
  stx screen+$3f8
  inx
  inx
  stx screen+$3f9
  inx
  inx
  stx screen+$3fa

waitd012:
  lda $d012
  cmp #$d1
  bcc waitd012

  lda $d001
  clc
  adc #21
  sta $d001
  sta $d003
  sta $d005

  lda $d007
  adc #42
  sta $d007
  sta $d009

waitd012b:
  lda $d012
  cmp #$d0+21
  bcc waitd012b

  inc screen+$3f8
  inc screen+$3f9
  inc screen+$3fa

waitd012a:
  lda $d012
  cmp #$c0+42
  bcc waitd012a

  inc screen+$3fb
  inc screen+$3fc

  lda #<irq
  sta $fffe
  lda #>irq
  sta $ffff
  lda #$fa
  sta $d012
  asl $d019

  pla
  tay
  pla
  tax
  pla
  rti
}

.var Wait  = $80
.var High  = $81
.var End   = $ff
.var eyes1 = $01|$08
.var eyes2 = $02|$10
.var eyes3 = $04|$20

script:
{
  lda wait: #0
  beq advanceScript
  dec wait
  rts

advanceScript:
  ldx scriptPointer: #0
  lda scriptData,x
  cmp #End
  bne testHigh
  {
    // end script + go to next part
    inc nextpart
    lda #0
    sta scriptPointer
    rts
  }

testHigh:
  cmp #High
  bne testWait
  {
    lda irq.high
    eor #1
    sta irq.high
    inc scriptPointer
    rts
  }

testWait:
  cmp #$80
  bcc onOff

  and #$7f
  sta wait
  bpl endScript

onOff:
  // turn sprites on/off
  eor setSprites.d015
  sta setSprites.d015
endScript:
  inc scriptPointer
  jmp script
}

scriptData:
  .byte 50|Wait
  .byte eyes1|eyes2|eyes3  // eyes on for 100
  .byte 100|Wait
  .byte eyes1|eyes2|eyes3  // eyes off for 8
  .byte 8|Wait
  .byte eyes1|eyes2|eyes3  // eyes on for 88
  .byte High
  .byte 8|Wait
  .byte eyes1|eyes2|eyes3  // eyes off
  .byte High
  .byte 50|Wait
  .byte End

* = spritesHigh "[GFX] sprite high"
  .import binary "./includes/high_with_fairlight.bin"

* = screen+$3f8 "[DATA] spritepointers" virtual
  .fill 6,0
