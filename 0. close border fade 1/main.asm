.var music = LoadSid("../music/tubular.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
    .label spindleLoadAddress = sprite
    *=spindleLoadAddress-18-4-3 "Spindle header"
    .label spindleHeaderStart = *

      .text "EFO2"              // fileformat magic
      .word 0                   // prepare routine
      .word start               // setup routine
      .word 0                   // irq handler
      .word 0                   // main routine
      .word 0                   // fadeout routine
      .word 0                   // cleanup routine
      .word openborder.MusicPlayCall   // location of playroutine call

      .byte 'A'                                                    // avoid loading
      .byte 'Z', $f0, $f3                                          // declare used 0 page adresses
      //.byte 'P', >((screen&$c000)|$3fff),>((screen&$c000)|$3fff) // we mess up the ghostbyte

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

.label tempa  = $f0
.label tempx  = $f1
.label tempy  = $f2
.label temp01 = $f3

#if !AS_SPINDLE_PART
  .label screen    = $7000
  .label sprite    = $7000
  .label codePart2 = $7400
#else
  .label screen    = $0400
  .label sprite    = $0400
  .label codePart2 = $0800
#endif

.var ghostbyte = ((screen&$c000)|$3fff)

.var Position = $ff5800
.var Speed    = $000080
.var maxPos   = $fff800
.var Positions = List()
.var Bounces = 0
.var maxBounces = 5

.for (var i=0; i<256; i++)
{
  // accelerate
  .eval Speed = Speed + $000030
  .eval Position = (Position + Speed)

  // bounce?

  .if ((Position >= maxPos))
  {
    .eval Position = (Position - Speed)

    // reverse speed + drop a bit of momentum
    .eval Speed = (Speed * -0.65)

    // count the bounces
    .eval Bounces = Bounces + 1

    .if (Bounces == maxBounces)
    {
      .eval Positions.add(maxPos)
    }
  }

  // stop if we reach max bounces
  .if (Bounces < maxBounces)
  {
    // store
    .eval Positions.add(Position)
  }
}
.eval Positions.lock()
.var PositionsSize = Positions.size()

* = sprite "[GFX] filled sprite"
.fill 63,$ff

* = * "[CODE] Main"
start:
{
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

    lda #$3c
    sta $dd02
    lda #$94
    sta $dd00

    jsr music.init
    jsr prepare
  #endif

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d

  lda #<openborder
  sta $fffe
  lda #>openborder
  sta $ffff
  lda $d011
  and #$07
  ora #$08
  sta $d011

  lda #$fa
  sta $d012

  lda $dc0d
  lda $dd0d
  asl $d019

  jsr setX

  // hide
  lda #$80
  sta $d000
  sta $d002
  sta $d004
  sta $d006
  sta $d008
  sta $d00a
  sta $d00c
  sta $d00e
  lda #$ff
  sta $d010

  jsr setSprites

  ldx nextpart
  inx
  stx openborder.writeNext

  cli

  ldx #0
  lda #8  // color for the next part
loop:
  sta $d800,x
  sta $d900,x
  sta $da00,x
  sta $db00,x
  inx
  bne loop
  
  #if !AS_SPINDLE_PART
    mainloop:
    {
      bit $ea
      inc $dbff
    }
    jmp mainloop
  #else
    rts
  #endif
}

prepare:
{
  rts
}

setSprites:
{
  lda #$ff
  sta $d015
  sta $d01d
  sta $d017
  lda #0
  sta $d01c

  jsr resetY

  lda $d020
  sta $d027
  sta $d028
  sta $d029
  sta $d02a
  sta $d02b
  sta $d02c
  sta $d02d
  sta $d02e

  lda #((sprite&$3fff)/64)
  sta screen+$3f8
  sta screen+$3f9
  sta screen+$3fa
  sta screen+$3fb
  sta screen+$3fc
  sta screen+$3fd
  sta screen+$3fe
  sta screen+$3ff
  rts
}

resetY:
{
  lda #$00
  beq add42.writeY
}

add42:
{
  lda $d001
  clc
  adc #42
writeY:
  sta $d001
  sta $d003
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d
  sta $d00f

  rts
}

.macro setXleft(spritenr)
{
  .var add = spritenr*$30

  lda xlo
  clc
  adc #<add
  tax
  lda xhi
  adc #>add
  tay

  // if positive, the sprite is visible
  bpl visible
  // if the position is negative, we have to substract 8 pixels extra, to avoid the $fff8-$ffff sprite gap
  txa
  sec
  sbc #8
  cmp #$d0       // check lowbyte for visibility
  bcc invisible
  tax

visible:
  // modify d010
  cpy #$ff
  jmp end
invisible:
  sec
  ldx #$c0
end:
  stx topirq.writepos+spritenr*5
  ror topirq.d010
}

.macro setXright(spritenr)
{
  .var add = spritenr*$30

  lda xlo2
  clc
  adc #<add
  tax
  lda xhi2
  adc #>add
  tay

  // is the sprite invisible? xco < $ffd0?
  beq visible

  // highbyte is $01, so check lowbyte
  cpx #$80
  bcs invisible
visible:
  // modify d010
  cpy #1
  jmp end
invisible:
  //sec
  ldx #$c0
end:
  stx topirq.writepos+((spritenr+4)*5)
  ror topirq.d010
}

setX:
{
  // sprites need to cover 160 pixels.
  // 4 sprites = 4*48 = 184 pixels.
  // max xco = 0

  setXleft(0)
  setXleft(1)
  setXleft(2)
  setXleft(3)

  setXright(0)
  setXright(1)
  setXright(2)
  setXright(3)
}
waste12:
  rts

xhi:
  .byte $ff
xlo:
  .byte $50
xhi2:
  .byte $01
xlo2:
  .byte $60

d010:
  .byte 0
xcos:
  .fill 8,0

topirq:
{
  sta restorea
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #$20
  sta $d012
  lda $d011
  and #$7f
  sta $d011

  lda #<multiplex1
  sta $fffe
  lda #>multiplex1
  sta $ffff
  asl $d019

  dec $d017
  //lda #$ff
  //sta $d017

  jsr waste12
  inc $dbff
  nop

  lda writepos: #0
  sta $d000
  lda #0
  sta $d002
  lda #0
  sta $d004
  lda #0
  sta $d006
  lda #0
  sta $d008
  lda #0
  sta $d00a
  lda #0
  sta $d00c
  lda #0
  sta $d00e
  lda d010: #0
  sta $d010

  lda restore01: #0
  sta $01
  lda restorea: #0
  rti
}

startirq:
  sta tempa
  stx tempx
  sty tempy
  lda $01
  sta temp01
  lda #$35
  sta $01
  jmp add42

endirq:
  sta $d012
  stx $fffe
  sty $ffff
  asl $d019
  lda temp01
  sta $01
  lda tempa
  ldx tempx
  ldy tempy
  rti

multiplex1:
{
  jsr startirq
  lda #$20+42
  ldx #<multiplex2
  ldy #>multiplex2
  jmp endirq
}

multiplex2:
{
  jsr startirq
  lda #$20+2*42
  ldx #<multiplex3
  ldy #>multiplex3
  jmp endirq
}

multiplex3:
{
  jsr startirq
  lda #$20+3*42
  ldx #<multiplex4
  ldy #>multiplex4
  jmp endirq
}

multiplex4:
{
  jsr startirq
  lda #$20+4*42
  ldx #<multiplex5
  ldy #>multiplex5
  jmp endirq
}

multiplex5:
{
  jsr startirq
  lda #$20+5*42  // this is the last one before openborder
  ldx #<multiplex6
  ldy #>multiplex6
  jmp endirq
}

multiplex6:
{
  jsr startirq
  lda #$fa
  ldx #<openborder
  ldy #>openborder
  jmp endirq
}

multiplex7:
{
  jsr startirq

  lda #0
  sta $d017

  lda #$21
  jsr add42.writeY

  lda #$28
  ldx #<multiplex8
  ldy #>multiplex8
  jmp endirq
}

multiplex8:
{
  jsr startirq
  jsr resetY
  
  //lda $d011
  //ora #$80
  //sta $d011

  lda #$35
  ldx #<topirq
  ldy #>topirq
  jmp endirq
}

* = codePart2 "[CODE] 2nd part"

// this is the IRQ for opening the border
openborder:
{
  sta restorea

  lda $01
  sta zp01
  lda #$35
  sta $01

  lda screenOn: #$00
  beq openBorder
  lda writeNext: #0
  sta nextpart
  lda #0
  sta $d015
  jmp closedBorder

openBorder:
  lda #$13
  sta $d011
closedBorder:
  lda #0
  sta ghostbyte

  lda #$80
  sta $d011  
  lda #((screen&$c000)/$4000)|$3c
  sta $dd02
  lda #((screen&$3fff)/$400)*16
  sta $d018

  lda #$1c-2
  sta $d012
  lda #<multiplex7
  sta $fffe
  lda #>multiplex7
  sta $ffff

  asl $d019

  stx restorex
  sty restorey
  cli
  
  MusicPlayCall:
  #if !AS_SPINDLE_PART
    jsr music.play
  #else        
    bit.abs $0000
  #endif

  ldx phase: #0
  lda bounce,x
  sta xlo
  lda #$ff
  sta xhi
  lda phase
  cmp #PositionsSize
  bne !+

  lda #$01
  sta screenOn
  lda #$f8
  sta xlo
  lda #$ff
  sta xhi
  bne continue
!:
  inc phase
continue:
  lda #<($0010+160)
  sec
  sbc xlo
  sta xlo2
  lda #>($0010+160)
  sbc xhi
  sta xhi2
  jsr setX

  inc timelow
  bne !+
    inc timehigh
  !:

  lda #$88
  sta $d011  

  lda zp01: #0
  sta $01
  
  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}


* = * "[DATA] bounce"
bounce:
.fill Positions.size(), (Positions.get(i)/256)&$ff

* = screen+$3f8 "[GFX] spritepointers"
  .fill 8,0 // spritepointers

