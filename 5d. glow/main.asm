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
  *=spindleLoadAddress-18-7-3 "Spindle header"
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
  .byte 'I', $88, $c0       // don't load too much, blow&redeye will load the rest
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
.label sprites = $f040
.label screen  = $f000

.var xLocations = List().add( 87, 191, 208,  77, 177, 211)
.var yLocations = List().add(124, 150,  98, 114, 146,  97)

#if !AS_SPINDLE_PART
* = standAloneStart
  sei
  lda #$35
  sta $01
  jmp start
#endif

* = sprites "[GFX] sprites"
  .import binary "./includes/eyes.bin",$180,$140

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
  stx screen+$3fa
  inx
  stx screen+$3f9
  inx
  stx screen+$3fb
  inx
  stx screen+$3fc
  inx
  stx screen+$3fd

  lda #$3f
  sta $d015
  ldx #0
  stx $d017
  stx $d01b
  stx $d01c
  stx $d01d

  ldx #(6*2)-1
!:
  lda spriteLocations,x
  sta $d000,x
  dex
  bpl !-

  lda #$00
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
  jsr glow

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

.var Wait  = $80
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
  bne testWait

  // end script + go to next part
  inc nextpart
  lda #0
  sta scriptPointer
  rts

testWait:
  cmp #$80
  bcc glowIt

  and #$7f
  sta wait
  bpl endScript

glowIt:
  tax
  and #1
  beq !+
  lda #0
  sta glow.joint1
!:
  txa
  and #2
  beq !+
  lda #0
  sta glow.joint2
!:
  txa
  and #4
  beq !+
  lda #0
  sta glow.joint3
!:

endScript:
  inc scriptPointer
  rts
}

scriptData:
  .byte 10|Wait
  .byte eyes1

  .byte 50|Wait
  .byte eyes2

  .byte 30|Wait
  .byte eyes3

  .byte 80|Wait  // wait at least 80 for glow of the last joint to finish

  .byte End

glow:
{
  inc wait
  lda wait: #0
  lsr
  bcs !+
  rts
!:
  ldx joint1: #(glowDataEnd-glowData)-1
  lda glowData,x
  sta $d027
  lsr
  lsr
  lsr
  lsr
  sta $d02a

  ldx joint2: #(glowDataEnd-glowData)-1
  lda glowData,x
  sta $d028
  lsr
  lsr
  lsr
  lsr
  sta $d02b

  ldx joint3: #(glowDataEnd-glowData)-1
  lda glowData,x
  sta $d029
  lsr
  lsr
  lsr
  lsr
  sta $d02c

  lda joint1
  cmp #(glowDataEnd-glowData)-1
  beq !+
  inc joint1
!:

  lda joint2
  cmp #(glowDataEnd-glowData)-1
  beq !+
  inc joint2
!:

  lda joint3
  cmp #(glowDataEnd-glowData)-1
  beq !+
  inc joint3
!:
  rts
}

glowData:
  .byte $00,$9b,$9b,$2b,$2b,$4c,$4c,$8c,$8c,$af,$a1,$71,$71,$71,$71,$a1,$a1,$71,$71,$71,$af,$af,$af,$71,$71,$a1,$8c,$8c,$4c,$4c,$2b,$2b,$2b,$9b,$9b,$00,$9b,$00
glowDataEnd:
