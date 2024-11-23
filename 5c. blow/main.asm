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
  *=spindleLoadAddress-18-10-3 "Spindle header"
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
  .byte 'I', $48, $c0       // don't load too much, blow&redeye will load the rest
  .byte 'P', $f8, $fb       // declare that we use this memory so it does not get loaded over -> in reality this memory is used by distort
  .byte 'P', >(screen+$3f8),>(screen+$3ff) // we use the spritepointers

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

.label loader0  = $05
.label loader1  = $06
.label loader2  = $07
.label loader3  = $08
.label loader4  = $09

.label sprites = $4440
.label screen  = $4400

.var xLocations = List().add(54,84,142,176,237,259)
.var yLocations = List().add(72,78,102,102, 72, 86)

* = sprites "[GFX] sprites"
  .import binary "./includes/eyes.bin",0,$180

.text "-ALL YOUR EYES ARE BELONG TO US-"

* = * "[CODE] Main"
start:
{
  sei
  lda $01
  sta zp01
  lda #$35
  sta $01

  lda $d011
  and #$7f
  ora #$08
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

  lda #0
  sta $d015

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00

    lda #0
    jsr music.init

    lda #0
    sta timelow
    sta timehigh
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

.var eyes1 = $03
.var eyes2 = $0c
.var eyes3 = $30

setSprites:
{
  clc
  ldx #0
  lda #((sprites&$3fff)/64)
!:
  sta screen+$3f8,x
  adc #1
  inx
  cpx #6
  bne !-

  lda d015: #0 // #(eyes1|eyes2|eyes3)
  sta $d015
  ldx #0
  stx $d017
  stx $d01b
  stx $d01c
  stx $d01d

  lda color: #0
!:
  sta $d027,x
  inx
  cpx #6
  bne !-

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
  jsr setSprites

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

  asl $d019

  lda #$0b
  sta $d011

  lda #16*((screen&$3fff)/$400)
  sta $d018
  lda #((screen&$c000)/$4000)|$3c
  sta $dd02

  lda zp01: #0
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti
}

.var Wait      = $80
.var Color     = $01
.var WaitUntil = $02
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
  lda wait: #0
  beq advanceScript
  dec wait
  rts

advanceScript:
  ldx scriptPointer: #0
  lda scriptData,x
  cmp #End
  bne testColor
  {
    // end script + go to next part
    inc nextpart
    lda #0
    sta scriptPointer
    rts
  }
testColor:
  cmp #Color
  bne testWaitUntil
  {
    lda scriptData+1,x
    sta setSprites.color
    inc scriptPointer
    inc scriptPointer
    rts
  }
testWaitUntil:
  cmp #WaitUntil
  bne testWait
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
testWait:
  cmp #Wait
  bcc onOff
  {
    and #$7f
    sta wait
    bpl endScript
  }
onOff:
  // turn sprites on/off
  eor setSprites.d015
  sta setSprites.d015
endScript:
  inc scriptPointer
  rts
}

scriptData:
  #if AS_SPINDLE_PART
    .byte WaitUntil, >$1360, <$1360
  #else
    .byte 100|Wait
  #endif

  // sprites on
  .byte eyes1|eyes2|eyes3
  .byte Color,$06
  .byte 3|Wait
  .byte Color,$0b
  .byte 3|Wait
  .byte Color,$0c
  .byte 3|Wait  
  .byte Color,$0e
  .byte 3|Wait  
  .byte Color,$0f
  .byte 3|Wait  
  .byte Color,$01
  .byte 3|Wait
  .byte 49|Wait

  // blink left
  .byte eyes1
  .byte 2|Wait
  .byte eyes1
  .byte 5|Wait
  .byte eyes1
  .byte 2|Wait
  .byte eyes1
  .byte 49|Wait

  // blink middle
  .byte eyes2
  .byte 3|Wait
  .byte eyes2
  .byte 6|Wait
  .byte eyes2
  .byte 3|Wait
  .byte eyes2
  .byte 19|Wait

  // blink right
  .byte eyes3
  .byte 4|Wait
  .byte eyes3
  .byte 6|Wait
  .byte eyes3
  .byte 4|Wait
  .byte eyes3
  .byte 49|Wait

  // blink middle
  .byte eyes2
  .byte 2|Wait
  .byte eyes2
  .byte 6|Wait
  .byte eyes2
  .byte 2|Wait
  .byte eyes2
  .byte 29|Wait

  // blink all
  .byte eyes1|eyes2|eyes3
  .byte 3|Wait
  .byte eyes1|eyes2|eyes3
  .byte 5|Wait
  .byte eyes1|eyes2|eyes3
  .byte 3|Wait

  .byte End
