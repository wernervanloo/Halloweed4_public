.var music = LoadSid("../music/tubular.sid")

#if AS_SPINDLE_PART
    .label spindleLoadAddress = start
    *=spindleLoadAddress-18-8-3 "Spindle header"
    .label spindleHeaderStart = *

      .text "EFO2"                // fileformat magic
      .word 0                     // prepare routine
      .word start                 // setup routine
      .word 0                     // irq handler
      .word 0                     // main routine
      .word 0                     // fadeout routine
      .word 0                     // cleanup routine
      .word irq.MusicPlayCall     // location of playroutine call

      .byte 'A'                   // no loading, go directly to the next part
      .byte 'S'                   // declare safe IO
      .byte 'I',$70,$bf           // avoid loading distort -> oogbal has to do this
      .byte 'I',$44,$47           // avoid loading 'blow' -> oogbal has to do this

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

* = $0b40 "[CODE] Main"
start:
{
  sei
  #if AS_SPINDLE_PART
    lda $01
    sta zp01
  #endif

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

  #if AS_SPINDLE_PART
    lda $d011
    and #$6f
    ora #$08
    sta $d011
  #else
    lda $d011
    and #$7f
    ora #$18
    sta $d011
  #endif

  lda #$fa
  sta $d012

  #if !AS_SPINDLE_PART
    lda #0
    jsr music.init
  #endif

  lda $dc0d
  lda $dd0d
  asl $d019

  #if AS_SPINDLE_PART
    lda zp01: #0  
    sta $01
  #endif

  cli
      
  ldx #0
  txa
loop:
  sta $d800,x
  sta $d900,x
  sta $da00,x
  sta $db00,x
  inx
  bne loop

  #if !AS_SPINDLE_PART
    jmp *
  #else
    rts
  #endif
}

irq:
{
  sta restorea
  stx restorex
  sty restorey

  lda $01
  sta zp01
  lda #$35
  sta $01

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

  // wait 8 frames
  //lda wait: #50
  //bne !+
  //  inc nextpart
  //!:
  //dec wait

  lda #$fa
  sta $d012
  asl $d019

  lda zp01: #0
  sta $01
  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti 
}

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#else 
  *=music.location "[MUSIC]" virtual
    .fill music.size, music.getData(i)
#endif