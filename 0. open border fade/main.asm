.var music = LoadSid("../music/tubular.sid")
#define skipmusic

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
    .label spindleLoadAddress = code
    *=spindleLoadAddress-18-10-3 "Spindle header"
    .label spindleHeaderStart = *

      .text "EFO2"              // fileformat magic
      .word prepare             // prepare routine
      .word start               // setup routine
      .word 0                   // irq handler
      .word 0                   // main routine
      .word 0                   // fadeout routine
      .word 0                   // cleanup routine
      .word 0 // this is a new sid.. no need to enter this openirq_1.MusicPlayCall   // location of playroutine call

      .byte 'M', <music.play, >music.play  // this is a new sid.. declare where the player is
      .byte 'Z', $e0, $e5       // declare used 0 page adresses
      .byte 'P', $bf, $bf       // declare used 0 page adresses
      .byte 'S'                 // declare safe IO (load under $d000 possible)

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

.label yposh     = $e0
.label yposl     = $e1
.label endD020   = $e2
.label endD021   = $e3
.label startD020 = $e4
.label startD021 = $e5

.var Position = $000300
.var Speed    = $000300  // $000080
.var accel    = $000100  // $000030
.var Positions = List()
.eval Positions.add(Position)

.var Bounces = 0
.var maxBounces = 2

.label code = $6000

.var BouncePositions = List().add($012000,$012900,$012900)

.while (Bounces < maxBounces)
{
  // accelerate
  .eval Speed = Speed + accel
  .eval Position = (Position + Speed) & $7fffff

  // bounce?

  .if (Position > BouncePositions.get(Bounces))
  {
    .var bouncePosition = BouncePositions.get(Bounces)
    .eval Position = Position - (2* (Position - bouncePosition))

    // reverse speed + drop a bit of momentum
    .eval Speed = (Speed * -0.50)

    // count the bounces
    .eval Bounces = Bounces + 1
  }

  // store
  .var add = Position
  .if (Bounces == maxBounces) .eval add = BouncePositions.get(Bounces)
  .eval Positions.add(add)
}
.print (Positions)

.eval Positions.lock()
.var PositionsSize = Positions.size()

* = $6000 "[CODE] Main"
start:
{
  // wait one frame, so we are sure that we can open the border and screen is 'on'
  lda $d011
  ora #$10
  sta $d011

  bit $d011
  bmi *-3
  bit $d011
  bpl *-3
  
  sei

  lda #$35
  sta $01

  #if !AS_SPINDLE_PART
    // -----------------------------------------------------------------
    // copied from spindle earlysetup to get the same cia timers running
    // -----------------------------------------------------------------

    bit	$d011
    bmi	*-3

    bit	$d011
    bpl	*-3

    lda	#RTI
    sta	$ffff
    ldx	#$ff
    stx	$fffa
    stx	$fffb
    inx
    stx	$dd0e
    stx	$dd04
    stx	$dd05
    lda	#$81
    sta	$dd0d
    lda	#$19
    sta	$dd0e

    ldx	$d012
    inx
  resync:
    cpx	$d012
    bne	*-3
    // at cycle 4 or later
    ldy	#0		 // 4
    sty	$dc07	 // 6
    lda	#62		 // 10
    sta	$dc06	 // 12
    iny			   // 16
    sty	$d01a	 // 18
    dey			   // 22
    dey			   // 24
    sty	$dc02	 // 26
    nop 			 // 30
    nop 			 // 32
    nop 			 // 34
    nop 			 // 36
    nop 			 // 38
    nop 			 // 40
    nop 			 // 42
    nop 			 // 44
    nop	  		 // 46
    lda	#$11	 // 48
    sta	$dc0f  // 50
    txa			   // 54
    inx			   // 56
    inx			   // 58
    cmp	$d012	 // 60	still on the same line?
    bne	resync

    // ----------------------------------
    // cia timers should be running now..
    // ----------------------------------

    lda startD020  // colors are already there when linked to the demo
    sta $d020
    lda startD021
    sta $d021

    lda #$94
    sta $dd00
    lda #$3f
    sta $dd02

    jsr prepare
  #endif

  lda #0        // reset demo time, this is a new sid
  sta timelow
  sta timehigh

  jsr music.init

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d

  lda #<openirq_1
  sta $fffe
  lda #>openirq_1
  sta $ffff
  lda #$fa
  sta $d012

  #if !AS_SPINDLE_PART
    lda #$1b
    sta $d011
  #else
    lda $d011
    and #$7f
    sta $d011
  #endif

  lda #$c8
  sta $d016

  lda $dc0d
  lda $dd0d
  asl $d019

  lda #$00
  sta yposh
  sta startD020
  sta endD020
  sta endD021
  lda #$03
  sta yposl

  lda #0
  sta startD021

  cli

  #if !AS_SPINDLE_PART
mainloop:
    //lda nextpart
    //sta $d020
    bit $ea
    inc $dbff
    jmp mainloop
  #else
    rts
  #endif
}

prepare:
{
}
waste12:
  rts

// this is the irq for changing d020/d021 way above the position for opening the border
d021irq_1:
{
  sta restorea
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #39-(17)     // 19..27 <- (earliest cycle)
  sec              // 21..29
  sbc $dc06        // 23..31, A becomes 0..8
  sta *+4          // 27..35
  bpl *+2          // 31..39
  lda #$a9         // 34
  lda #$a9         // 36
  lda #$a9         // 38
  lda $eaa5        // 40
                   // at cycle 34+(10) = 44

  nop
  nop
  nop

  lda endD020
  sta $d020
  lda endD021
  sta $d021

  lda #<openirq_1
  sta $fffe
  lda #>openirq_1
  sta $ffff
  lda #$fa
  sta $d012
  lda $d011
  and #$7f
  sta $d011
  asl $d019

  lda restore01: #0
  sta $01
  lda restorea: #0
  rti
}

// this is the IRQ for opening the border (without changing d020/d021)
openirq_1:
{
  sta restorea
  stx restorex
  sty restorey

  lda $01
  sta zp01
  lda #$35
  sta $01

  lda $d011
  and #$f7
  sta $d011

  lda #0
  sta $bfff
  lda #$3e
  sta $dd02

  lda #$37
  sta $d012
  lda #<bottomirq
  sta $fffe
  lda #>bottomirq
  sta $ffff

  lda #$80
  sta $d011
  asl $d019

  #if skipmusic
    //jmp skip
  #endif

  MusicPlayCall:
    jsr music.play  // this is a new SID, so we have to call it directly, even in Spin
  skip:

  lda zp01: #0
  sta $01
  
  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}

// this is the IRQ for opening the border (without changing d020/d021)
openirq_2:
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

  lda $d011
  and #$f7
  sta $d011

  lda yposl
  sta $d012
  lda #<d021irq_2
  sta $fffe
  lda #>d021irq_2
  sta $ffff

  lda #$80
  sta $d011

  #if skipmusic
    //jmp skip
  #endif

  MusicPlayCall:
    jsr music.play  // this is a new SID, so we have to call it directly, even in Spin
skip:

endIrq:
  asl $d019

  lda zp01: #0
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti
}

// this is the irq for changing d020/d021 way below the position for opening the border
d021irq_2:
{
  // Jitter correction. Put earliest cycle in parenthesis.
  // (10 with no sprites, 19 with all sprites, ...)
  // Length of clockslide can be increased if more jitter
  // is expected, e.g. due to NMIs.

  sta restorea
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #39-(17)     // 19..27 <- (earliest cycle)
  sec              // 21..29
  sbc $dc06        // 23..31, A becomes 0..8
  sta *+4          // 27..35
  bpl *+2          // 31..39
  lda #$a9         // 34
  lda #$a9         // 36
  lda #$a9         // 38
  lda $eaa5        // 40
                   // at cycle 34+(10) = 44

  nop
  nop
  nop

  lda endD020
  sta $d020
  lda endD021
  sta $d021

  lda #<bottomirq
  sta $fffe
  lda #>bottomirq
  sta $ffff
  lda #$37
  sta $d012
  lda $d011
  ora #$80
  sta $d011
  asl $d019

  stx restorex
  sty restorey

  lda restore01: #0
  sta $01
  lda restorea:  #0
  ldx restorex:  #0
  ldy restorey:  #0
  rti
}

// this is the irq for both changing d020/d021 AND opening the border.
bothirq:
{
  // Jitter correction. Put earliest cycle in parenthesis.
  // (10 with no sprites, 19 with all sprites, ...)
  // Length of clockslide can be increased if more jitter
  // is expected, e.g. due to NMIs.

  sta restorea
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #39-(17)     // 19..27 <- (earliest cycle)
  sec              // 21..29
  sbc $dc06        // 23..31, A becomes 0..8
  sta *+4          // 27..35
  bpl *+2          // 31..39
  lda #$a9         // 34
  lda #$a9         // 36
  lda #$a9         // 38
  lda $eaa5        // 40
                   // at cycle 34+(10) = 44

  stx restorex

  ldx yposl
  dex
  stx compare
loop:
  ldx $d012
  cpx compare: #0
  beq changed020d021

  cpx #$f8  // carry set if $f8 or higher
  lda #$00
  rol
  rol
  rol
  rol
  eor #$08
  sta $d011
  
  // make 63 cycle loop
  nop
  inc $dbff
  inc $dbff
  inc $dbff
  inc $dbff
  inc $dbff

  cpx #$20
  bne loop
continue:
  lda #<bottomirq
  sta $fffe
  lda #>bottomirq
  sta $ffff
  lda #$80
  sta $d011
  lda #$37
  sta $d012
  asl $d019
endIrq:
  sty restorey

  #if skipmusic
    //jmp skip
  #endif

  MusicPlayCall:
    jsr music.play  // this is a new SID, so we have to call it directly, even in Spin
skip:

  lda restore01: #0
  sta $01
  ldy restorey:  #0 
  ldx restorex:  #0
  lda restorea:  #0
  rti 

changed020d021:
  jsr waste12
  jsr waste12
  jsr waste12
  inc $dbff
  nop
  bit $ea

  lda endD020
  sta $d020
  lda endD021
  sta $d021

loop2:
  ldx $d012
  cpx #$f8  // carry set if $f8 or higher
  lda #$00
  rol
  rol
  rol
  rol
  eor #$08
  sta $d011
  
  cpx #$f0
  bcs loop2
  cpx #$10
  bcc loop2
  jmp continue
}

.align $20
bottomirq:
{
  // Jitter correction. Put earliest cycle in parenthesis.
  // (10 with no sprites, 19 with all sprites, ...)
  // Length of clockslide can be increased if more jitter
  // is expected, e.g. due to NMIs.
  sta restorea
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #39-(17)     // 19..27 <- (earliest cycle)
  sec              // 21..29
  sbc $dc06        // 23..31, A becomes 0..8
  sta *+4          // 27..35
  bpl *+2          // 31..39
  lda #$a9         // 34
  lda #$a9         // 36
  lda #$a9         // 38
  lda $eaa5        // 40
                   // at cycle 34+(10) = 44

  lda startD021
  sta $d021
  lda startD020
  sta $d020

  stx restorex

  jsr script

  ldx phase: #0

  lda move: #0   
  beq continue   // if not moving yet, don't move
  cpx #(Positions.size()-1)
  beq cleanSplit  // if at maximum, don't move

  // move the split
  inc phase
  inx
  bne continue

cleanSplit:
  lda startD021
  sta endD021
  lda endD020
  sta startD020

continue:
  lda bounce,x
  sta yposl
  lda bounce+(PositionsSize),x
  sta yposh

  lda invert: #0
  beq continue2

  // invert movement
  // $003 -> $120
  // $120 -> $003
  lda #<$12c
  sec
  sbc yposl
  sta yposl
  lda #>$12c
  sbc yposh
  sta yposh

continue2:
  // there are 3 cases :
  // 1. color change above open border -> go to d021irq_1

  lda yposh
  bne goToOpenBorderIrq
  lda yposl
  cmp #$f0
  bcs goToBothIrq

  // color change is far above opening the border..
  lda #<d021irq_1
  sta $fffe
  lda #>d021irq_1
  sta $ffff
  lda yposl
  sta $d012
  lda #$08
  sta $d011
endIrq:
  asl $d019

  lda restore01: #0
  sta $01
  lda restorea: #0
  ldx restorex: #0
  rti

goToOpenBorderIrq:
  lda yposl
  cmp #$20
  bcc goToBothIrq

  // color change is far below opening the border..
  lda #<openirq_2
  sta $fffe
  lda #>openirq_2
  sta $ffff
  lda #$fa
  sta $d012
  lda #$08
  sta $d011
  jmp endIrq

goToBothIrq:
  // color change is near opening the border.. do both in the same irq
  lda #<bothirq
  sta $fffe
  lda #>bothirq
  sta $ffff
  lda #$ec
  sta $d012
  lda #$08
  sta $d011
  jmp endIrq
}

.var Wait      = $80
.var StartDown = $81
.var StartUp   = $82
.var End       = $83

script:
  lda wait: #0
  beq advanceScript
  dec wait
  rts
advanceScript:
  ldx scriptPointer: #0
  lda scriptData,x
  cmp #Wait
  bne testStartDown
  {
    lda scriptData+1,x
    sta wait
    inc scriptPointer
    inc scriptPointer
    rts
  }
testStartDown:
  cmp #StartDown
  bne testStartUp
  {
    lda #6
    sta startD021
    lda #1
    sta bottomirq.move
    lda #0
    sta bottomirq.phase
    sta bottomirq.invert
    inc scriptPointer
    rts
  }
testStartUp:
  cmp #StartUp
  bne testEnd
  {
    // fix the $d021 color
    lda startD021
    sta endD021
    lda #12
    sta endD020
    lda #1
    sta bottomirq.invert

    lda #0
    sta bottomirq.phase
    inc scriptPointer
    rts
  }
testEnd:
  inc nextpart
  dec wait
  lda #0
  sta scriptPointer
  // stop movement
  lda endD020
  sta startD020
  rts

scriptData:
  .byte Wait,255
  .byte Wait,50
  .byte StartDown
  .byte Wait,255
  .byte Wait,60
  .byte StartUp
  .byte Wait,50
  .byte End

*=* "[DATA] bouncedata"
bounce:
.lohifill Positions.size(), Positions.get(i)/256
.byte $ff