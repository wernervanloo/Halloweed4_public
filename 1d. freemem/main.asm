.var music = LoadSid("../music/intro.sid")

#if AS_SPINDLE_PART
  .label spindleLoadAddress = sprites
  *=spindleLoadAddress-18-7-3 "Spindle header"
  .label spindleHeaderStart = *

  .text "EFO2"              // fileformat magic
  .word 0                   // prepare routine
  .word start               // setup routine
  .word 0                   // irq handler
  .word 0                   // main routine
  .word 0                   // fadeout routine
  .word 0                   // cleanup routine
  .word irq.MusicPlayCall   // location of playroutine call

  .byte 'S'                 // declare safe IO
  .byte 'I',>(screen1),>(screen1+$3e8)
  .byte 'I',>(bitmap1),>(bitmap1+$1f40)

  .byte 0
  .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart
#else    
    :BasicUpstart2(start)
#endif

.label screen1     = $0400  // ..$07e7
.label bitmap1     = $2000  // ..$3f3f

// these are the demo-spanning 0 page adresses!
.label nextpart = $02
.label timelow  = $03
.label timehigh = $04

.label loader0 = $05
.label loader1 = $06
.label loader2 = $07
.label loader3 = $08
.label loader4 = $09
.label sprites = $0b00

* = sprites "[GFX] blooddrop"
 .import binary "./includes/blooddrop.bin"

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
  
  lda #(((screen1&$3fff)/$0400)*16)+(((bitmap1&$2000)/$2000)*8)
  sta $d018
  lda #((screen1&$c000)/$4000)|$3c
  sta $dd02

  lda #((sprites&$3fff)/64)
  sta screen1+$3f8
  sta screen1+$3f9
  sta screen1+$3fa

  lda #0
  sta $d015

  // move blood
  lda dropBlood: #0
  beq skipBlood1

  ldx bloodPhase: #0
  lda bloodY,x
  beq skipBlood1
  sta ypos
  lda bloodX,x
  sta xpos
  inc bloodPhase
  lda #$01
  sta $d015
skipBlood1:

  // move blood
  lda dropBlood2: #0
  beq skipBlood2

  ldx bloodPhase2: #0
  lda bloodY2,x
  beq skipBlood2
  sta ypos2
  inc bloodPhase2
  lda $d015
  ora #$02
  sta $d015
skipBlood2:

  lda #$55

  // move blood
  lda dropBlood3: #0
  beq skipBlood3

  ldx bloodPhase3: #0
  lda wait3: #0
  beq advance3

  // keep current value
  dec wait3
  lda currentYBlood3: #0
  beq skipBlood3
  sta ypos3
  bne skipBlood3a

advance3: 
  lda oldWait3: #0
  sta wait3

  lda bloodY3,x
  cmp #$ff
  bne noWaitChange
  lda bloodY3+1,x
  sta wait3
  sta oldWait3
  inx
  inx
noWaitChange:
  lda bloodY3,x
  sta currentYBlood3
  beq skipBlood3
  sta ypos3
  inx
  stx bloodPhase3
skipBlood3a:
  lda $d015
  ora #$04
  sta $d015
  lda #$04
  sta $d01b
skipBlood3:

  lda #RED
  sta $d027
  sta $d028
  sta $d029

  lda xpos: #$9a
  sta $d000
  lda ypos: #$c0
  sta $d001

  lda xpos2: #$f2
  sta $d002
  lda ypos2: #$c9
  sta $d003

  lda xpos3: #$33
  sta $d004
  lda ypos3: #$c3
  sta $d005

  lda #$00
  sta $d017
  sta $d01b
  sta $d01d
  sta $d01c

  inc timelow
  bne !+
  inc timehigh
!:

  // when to drop blood
  lda timehigh
  cmp #3
  bne !+
    lda timelow
    cmp #0
    bne !+
      lda #1
      sta dropBlood
!:

  // when to drop blood
  lda timehigh
  cmp #2
  bne !+
    lda timelow
    cmp #$c0
    bne !+
      lda #1
      sta dropBlood2
!:

  // when to drop blood
  lda timehigh
  cmp #2
  bne !+
    lda timelow
    cmp #$b0
    bne !+
      lda #1
      sta dropBlood3
!:

  lda timehigh
  cmp #4
    bne !+
    lda timelow
    cmp #120-7
    bne !+
      inc nextpart
!:

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

bloodY:
  .fill 44,[$c1+i,$c1+i,$c1+i,$c1+i,$c1+i]  // stop $ec = $2b steps
  .byte 0
bloodX:
  .fill 23*5,$9a
  .fill 21*5,$9b

bloodY2:
  .fill 15,[$c9+i,$c9+i,$c9+i,$c9+i,$c9+i,$c9+i,$c9+i,$c9+i]
  .fill 30,$d8
  {
    .var yCo = $d8
    .var speed = 0
    .while(yCo < $ff)
    {
      .eval speed = speed + 0.15
      .eval yCo = yCo + speed
      .byte round(yCo)
    }
  }
  .byte 0

bloodY3:
  .byte $ff,$8  // wait 8 frames
  .fill $1d,$c7+i
  .byte $ff,30  // wait 30 frames
  .byte $e4
  .byte $ff,0   // wait 0 frames
  {
    .var yCo = $e4
    .var speed = 0
    .while(yCo < $fb)
    {
      .eval speed = speed + 0.15
      .eval yCo = yCo + speed
      .byte round(yCo)
    }
  }
  .byte 0

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#else 
  *=music.location "[MUSIC]" virtual
    .fill music.size, music.getData(i)
#endif