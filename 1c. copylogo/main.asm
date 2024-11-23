.var music = LoadSid("../music/intro.sid")

#if AS_SPINDLE_PART
  .label spindleLoadAddress = $bd40
  *=spindleLoadAddress-18-13-3 "Spindle header"
  .label spindleHeaderStart = *

  .text "EFO2"              // fileformat magic
  .word prepare             // prepare routine
  .word start               // setup routine
  .word 0                   // irq handler
  .word 0                   // main routine
  .word 0                   // fadeout routine
  .word 0                   // cleanup routine
  .word irq.MusicPlayCall   // location of playroutine call

  .byte 'X'                 // declare safe IO
  .byte 'I',>(screen),>(screen+$3ff)
  .byte 'I',>(bitmap),>(bitmap+$1f40)
  .byte 'P',>(screen1),>(screen1+$3e8)
  .byte 'P',>(bitmap1),>(bitmap1+$1f40)

  .byte 0
  .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart
#else    
    :BasicUpstart2(start)
#endif

.label screen = $c000
.label bitmap = $e000

.label screen1 = $0400  // ..$07e7
.label bitmap1 = $2000  // ..$3f3f

// these are the demo-spanning 0 page adresses!
.label nextpart = $02
.label timelow  = $03
.label timehigh = $04

.label loader0 = $05
.label loader1 = $06
.label loader2 = $07
.label loader3 = $08
.label loader4 = $09

* = $bd40 "[CODE] Main"
start:
{
  sei
  lda $01
  sta zp01
  lda #$35
  sta $01

  lda $d011
  and #$7f
  sta $d011

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
    lda #0
    jsr music.init
  #endif
  lda zp01: #0
  sta $01
  cli
      
  #if !AS_SPINDLE_PART
    jmp *
  #else
    rts
  #endif
}

prepare:
{
  // wait for fade to finish
#if AS_SPINDLE_PART
  // wait until the colors are set in logo fader
waitForColorsSet:
  lda nextpart
  cmp #2
  bne waitForColorsSet
#endif

  // copy xenon blood logo to bitmap1
  ldx #31
  jsr copy

  lda #<screen
  sta copyFrom
  lda #>screen
  sta copyFrom+1
  lda #<screen1
  sta copyTo
  lda #>screen1
  sta copyTo+1

  ldx #3

copy:
  ldy #0
loop:
  lda copyFrom: bitmap,y
  sta copyTo:   bitmap1,y
  iny
  bne loop
  inc copyFrom+1
  inc copyTo+1
  dex
  bpl loop

  rts
}

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

  inc timelow
  bne !+
  inc timehigh
!:

  inc $d020
  
  MusicPlayCall:
  #if !AS_SPINDLE_PART
    jsr music.play
  #else         
    bit.abs $0000
  #endif
    
  lda #$ff   //this is the orthodox and safe way of clearing the interrupt condition of the VICII.
  sta $d019 

  lda zp01: #0
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti
}

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#else 
  *=music.location "[MUSIC]" virtual
    .fill music.size, music.getData(i)
#endif