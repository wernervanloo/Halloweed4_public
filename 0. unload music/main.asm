#if AS_SPINDLE_PART
  .label spindleLoadAddress = $ff40
  *=spindleLoadAddress-18-5-3 "Spindle header"
  .label spindleHeaderStart = *

  .text "EFO2"              // fileformat magic
  .word 0                   // prepare routine
  .word start               // setup routine
  .word irq                 // irq handler
  .word 0                   // main routine
  .word 0                   // fadeout routine
  .word 0                   // cleanup routine
  .word 0                   // location of playroutine call

  //.byte 'A'               // avoid loading (load only what is needed)
  .byte 'S'                 // declare safe IO
  .byte 'M',0,0             // unload music
  .byte 'X'                 // no loading to speed up demo
  //.byte 'I',$60,$ac         // do not load too much during unload : so we 'inherit' some memory
  .byte 0
  .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart
#else    
  :BasicUpstart2(basicstart)
#endif

#if !AS_SPINDLE_PART
basicstart:
  sei
  lda #$35
  sta $01
  jmp start
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

* = $ff40 "[CODE] Main"
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
  sta timelow
  sta timehigh

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

  lda $01
  sta zp01
  lda #$35
  sta $01

  lda $d011
  and #$77    // turn screen off ($10) and open border ($8)
  sta $d011

  lda #$ff   //this is the orthodox and safe way of clearing the interrupt condition of the VICII.
  sta $d019 

  ldx #100
loop:
  nop
  dex
  bpl loop

  lda $d011
  and #$6f  // turn screen off
  ora #$08  // open border
  sta $d011

  lda zp01: #0
  sta $01

  pla
  tax
  pla
  rti
}
