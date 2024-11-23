//#define DEBUG

.var music = LoadSid("../music/tubular.sid")

.var nrBars     = 8
.var background = BLUE
.var blank      = BLACK

.var sinSize1 = 128
.var sinSize2 = 128
.var sinSize3 = 224

.var size1 = 48
.var size2 = 64
.var size3 = 32

.var backgroundPattern = $00

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
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

// these are the 0 page adresses for the part

.label NOPvalue = $ea
.label sinPhase = $eb

.label screen   = $0c00
.label charseta = $0800
.label charsetb = $2000
.label charsetc = $2800
.label charsetd = $3000
.label charsete = $3800

// we try to do some sort of mipmapping here..
.var PatList16 = List().add(8,6,6,8,8,6,6,6,8,8,8,6,6,8,8,6)
.var PatList32 = List().add(8,10,10,8,8,8,10,10,10,10,8,8,8,8,8,8,10,10,10,10,10,10,8,8,8,8,10,10,10,8,8,10)
.var PatList48 = List().add(8,8,10,10,10,10,8,8,8,8,8,8,10,10,10,10,10,10,10,10,8,8,8,8,8,8,8,8,8,8,8,8,10,10,10,10,10,10,10,10,10,10,10,10,8,8,8,8,8,8,8,8,10,10,10,10,10,10,8,8,8,8,10,10)
.var PatList64 = List().add(12,12,14,14,14,14,12,12,12,12,12,12,14,14,14,14,14,14,14,14,12,12,12,12,12,12,12,12,12,12,12,12,14,14,14,14,14,14,14,14,14,14,14,14,12,12,12,12,12,12,12,12,14,14,14,14,14,14,12,12,12,12,14,14)

.macro calcPattern(i)
{
  // this gives a table of i bytes long with the correct pattern, starting with background color
  .var j=i-1

  .byte (((charseta&$3fff)/$0800)*$2)|((screen&$3fff)/$400*$10)
  .if (j<=16)             .for (var k=0; k<j; k++) .byte (PatList16.get((k*16/j)+0.5) | (screen&$3fff)/$400*$10)
  .if ((j>16) && (j<=32)) .for (var k=0; k<j; k++) .byte (PatList32.get((k*32/j)+0.5) | (screen&$3fff)/$400*$10)
  .if ((j>32) && (j<=48)) .for (var k=0; k<j; k++) .byte (PatList48.get((k*64/j)+0.5) | (screen&$3fff)/$400*$10)
  .if (j>48)              .for (var k=0; k<j; k++) .byte (PatList64.get((k*64/j)+0.5) | (screen&$3fff)/$400*$10)
}

.macro calcDD02(i)
{
  .fill i,$3c
}

.var BarList15   = List().add($9,$2,$4,$8,$a,$f,$7,$1,$7,$f,$a,$8,$4,$2,$9)
.var BarList30A  = List().add($9,$9,$2,$2,$2,$4,$4,$4,$8,$8,$8,$a,$8,$a,$a,$a,$a,$8,$a,$8,$8,$8,$4,$4,$4,$2,$2,$2,$9,$9)
.var BarList60A  = List().add($9,$9,$2,$9,$2,$2,$4,$2,$4,$4,$8,$4,$8,$8,$a,$8,$a,$a,$f,$a,$f,$f,$7,$f,$7,$7,$1,$7,$1,$1,$1,$1,$1,$1,$7,$1,$7,$7,$f,$7,$f,$f,$a,$f,$a,$a,$8,$a,$8,$8,$4,$8,$4,$4,$2,$4,$2,$2,$9,$2,$9,$9)

.var BarList30B = List().add($9,$9,$b,$b,$b,$8,$8,$8,$5,$5,$5,$f,$5,$f,$f,$f,$f,$5,$f,$5,$5,$5,$8,$8,$8,$b,$b,$b,$9,$9)
.var BarList60B = List().add($9,$9,$b,$9,$b,$b,$8,$b,$8,$8,$c,$8,$c,$c,$5,$c,$5,$5,$f,$5,$f,$f,$d,$f,$d,$d,$1,$d,$1,$1,$1,$1,$1,$1,$d,$1,$d,$d,$f,$d,$f,$f,$5,$f,$5,$5,$c,$5,$c,$c,$8,$c,$8,$8,$b,$8,$b,$b,$9,$b,$9,$9)

.var BarList30C = List().add($6,$6,$b,$b,$b,$e,$e,$e,$ec,$c,$c,$3,$e,$3,$3,$3,$3,$e,$3,$c,$c,$c,$e,$e,$e,$b,$b,$b,$6,$6)
.var BarList60C = List().add($6,$6,$b,$6,$b,$b,$e,$b,$e,$e,$c,$e,$c,$c,$3,$c,$3,$3,$f,$3,$f,$f,$d,$f,$d,$d,$1,$d,$1,$1,$1,$1,$1,$1,$d,$1,$d,$d,$f,$d,$f,$f,$3,$f,$3,$3,$c,$3,$c,$c,$e,$c,$e,$e,$b,$e,$b,$b,$6,$b,$6,$6)

.var BarList30D  = List().add($9,$9,$2,$2,$2,$8,$8,$8,$a,$a,$a,$7,$a,$7,$7,$7,$7,$a,$7,$a,$a,$a,$8,$8,$8,$2,$2,$2,$9,$9)
.var BarList60D  = List().add($9,$9,$2,$9,$b,$b,$c,$b,$c,$c,$8,$4,$8,$8,$a,$8,$a,$a,$f,$a,$f,$f,$7,$f,$7,$7,$1,$7,$1,$1,$1,$1,$1,$1,$7,$1,$7,$7,$f,$7,$f,$f,$a,$f,$a,$a,$8,$a,$8,$8,$4,$8,$4,$4,$2,$4,$2,$2,$9,$2,$9,$9)


.macro calcColors(i, color)
{
  // this gives a table of i bytes long with the correct colors, starting with background color
  .var j=i-1
  .var List15 = List()
  .var List30 = List()
  .var List60 = List()

  .eval List15.addAll(BarList15)

  .if (color==0)
  {
    .eval List30.addAll(BarList30A)
    .eval List60.addAll(BarList60A)
  }

  .if (color==1)
  {
    .eval List30.addAll(BarList30B)
    .eval List60.addAll(BarList60B)
  }

  .if (color==2)
  {
    .eval List30.addAll(BarList30C)
    .eval List60.addAll(BarList60C)
  }

  .if (color==3)
  {
    .eval List30.addAll(BarList30D)
    .eval List60.addAll(BarList60D)
  }

  .byte background
  .if (j<=15)             .for (var k=0; k<j; k++) .byte List15.get((k*15/j)+0.5) 
  .if ((j>15) && (j<=48)) .for (var k=0; k<j; k++) .byte List30.get((k*30/j)+0.5) 
  .if (j>48)              .for (var k=0; k<j; k++) .byte List60.get((k*60/j)+0.5) 
}

#if AS_SPINDLE_PART
  .var PLAY=music.play
  .label spindleLoadAddress = charsetb
  *=spindleLoadAddress-18-9-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word prepare       // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word bottomIRQ.MusicPlayCall // location of playroutine call

    //.byte 'S'                                                // this part is IO safe and can load below $d000
    .byte 'P', >screen, >(screen+40+(40*8))                    // the used screen + the used chars in the charset  
    .byte 'P', >((screen&$c000)|$3fff),>((screen&$c000)|$3fff) // mark ghostbyte as used!
    .byte 'Z', $ea, $eb

    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart

#else    
    :BasicUpstart2(start)
#endif


*=screen "[GFX] screen data" virtual
  .fill 40,(40/8)+i
  
*=charseta+40+$400 "[GFX] empty line" virtual
  .fill 40*8,backgroundPattern

*=charsetb "[DATA] d021Tab ice"
  d021_33c: calcColors(size1+1, 2)
.align $100
  d021_64c: calcColors(size2+1, 2)
.align $100
*=* "[DATA] d021Tab fire"
  d021_33d: calcColors(size1+1, 3)
.align $100
  d021_64d: calcColors(size2+1, 3)

*=charsetb+40+$400 "[GFX] smallest line"
charset1:
  //.byte 0,0,0,0,0,0,0,$0f
  //.fill 19,[$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f]
  //.fill 8,$f0
  .fill 8,[$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc,0,0,0,0,0,0,0,0]

.align $100
*=* "[DATA] d018Tab"
d018Tab:
  d018_33: calcPattern(size1+1)

*=* "[DATA] sine"
sine:
{
    .var sinSize = sinSize1
    .var sinMin  = 00
    .var sinMax  = 200-size1-9
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .fill sinSize, ((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize)))
}

.align $100
*=* "[DATA] d018Tab"
  d018_64: calcPattern(size2+1)

*=* "[DATA] sine 2"
sine2:
{
    .var sinSize = sinSize2
    .var sinMin  = 00
    .var sinMax  = 200-size2-9
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .fill sinSize, ((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize)))
}

.align $100
*=* "[DATA] dd02Tab"
dd02Tab:
  dd02_33: calcDD02(size1+1)
.align $100
  dd02_64: calcDD02(size2+1)

.align $100
*=* "[DATA] d021Tab reddish"
d021Tab:
  d021_33: calcColors(size1+1, 0)
.align $100
  d021_64: calcColors(size2+1, 0)



*=charsetc+40+$400 "[GFX] inv. smallest line"
charset2:
  //.fill 8,$f0
  //.fill 19,[$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0]
  //.fill 8,$0f

  .fill 8,[$0,$0,$0,$0,$0,$0,$0,$0,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$03,$03,$03,$03,$03,$03,$03,$03,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff]


* = * "[CODE] main"
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

    jsr prepare
    lda #0
    jsr music.init

  #endif

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d

  lda #<bottomIRQ
  sta $fffe
  lda #>bottomIRQ
  sta $ffff
  lda #$fa
  sta $d012

  lda #$08
  sta $d011

  lda #$0c
  sta $d020
  lda #background
  sta $d021
  lda #$c8
  sta $d016

  lda #backgroundPattern
  sta (screen&$c000)+$3fff  // ghostbyte

  lda #$3c
  sta $dd02

  lda #(((charseta&$3fff)/$0800)*$2)|((screen&$3fff)/$400*$10)
  sta $d018

  lda $dc0d
  lda $dd0d
  asl $d019

  lda #$00
  sta NOPvalue
  lda #sinSize1-51
  sta sinPhase

  ldx #39
  lda #0
loop:  
  sta $d800,x
  dex
  bpl loop

  cli

  #if !AS_SPINDLE_PART
mainloop:
    jmp mainloop
  #else
    rts
  #endif
}

dummy:
  .byte 0

prepare:
{
  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
  #endif

  // set chars and colors
  ldx #0
  lda #128+(40/8)
loop:
  sta screen,x
  clc
  adc #1
  inx
  cpx #40
  bne loop

  // clear "empty" charset
  ldx #0
  txa
loop2:
  sta charseta+40+$400,x
  sta charseta+40+$400+((40*8)/2),x
  inx
  cpx #((40*8)/2)
  bne loop2

  rts
}

bottomIRQ:
{
  dec 0
  pha
  txa
  pha
  tya
  pha

  lda #$10     // scroll up as much as possible to start at line 7
  sta $d011

  // keep demotime (always before calling music player)
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

  jsr script  
  lda screenoff: #0
  beq !+
  {
    lda #$08
    sta $d011
    bne continue
  }
  !:
  #if DEBUG
    inc $d020
  #endif
  jsr move
  jsr plotBars
  #if DEBUG
    inc $d020
  #endif
  jsr scrollIt
  jsr scrollIt2
  jsr scrollIt2
  #if DEBUG
    dec $d020
    dec $d020
  #endif

  lda #>d018Tab
  sta topIRQ.d018Write+1
  lda #>dd02Tab
  sta topIRQ.dd02Write+1
  lda #>d021Tab
  sta topIRQ.d021Write+1
continue:
  lda #$36
  sta $d012

  lda #<topIRQ
  sta $fffe
  lda #>topIRQ
  sta $ffff
  asl $d019

  pla
  tay
  pla
  tax
  pla
  inc 0
  rti
}

.align $100
topIRQ:
{
  // Jitter correction. Put earliest cycle in parenthesis.
  // (10 with no sprites, 19 with all sprites, ...)
  // Length of clockslide can be increased if more jitter
  // is expected, e.g. due to NMIs.

  dec 0            // 10..18
  sta savea        // 15..23
  lda #39-(10)     // 19..27 <- (earliest cycle)
  sec              // 21..29
  sbc $dc06        // 23..31, A becomes 0..8
  sta *+4          // 27..35
  bpl *+2          // 31..39
  lda #$a9         // 34
  lda #$a9         // 36
  lda #$a9         // 38
  lda $eaa5        // 40
                   // at cycle 34+(10) = 44

  stx savex
  sty savey

  tsx
  stx saves

  jsr waste12
  jsr waste12
  inc dummy
  nop
  nop
  bit $ea

  ldy #0
  ldx #0
  clc
loop:
  lda barTable,x     // 4 start of new bar? (0= no) in the table we have the size of the bar
                     //   load into a, we don't want to destroy Y
                     // -> we could do lax ,y and save the tay?
  beq writeVic       // 2

  tay
  inc dummy
  lda d011Tab,x         // 4 depends on rasterline
  // we should be at cycle 50 here..
  sta $d011             // 4

  lda #blank            // 2
  sta $d021             // 4 black on black = nothing to see..

  lda d018TabHigh,y     // 4
  sta d018Write+1       // 3 point to the correct table for this size
  adc #>(dd02Tab-d018Tab)
  sta dd02Write+1       // 4
  lda colorTable,x
  //adc #>(d021Tab-dd02Tab)
  sta d021Write+1       // 3
  //nop
  nop
  ldy startAt,x         // position where to start reading

  // we can save a lot of cycles in this special case if we make the first line fixed black!
                     
  // waste 63-57=96 cycles here
  inx                   // 2
  cpx #192              // 2
  bne loop              // 3
  beq endLoop

writeVic:             // 7 from before

  lda d011Tab,x
  txs
  ldx d021Write: d021Tab,y
  sta $d011           // 4 linecrunch
  stx $d021           // 4
  lda d018Write: d018Tab,y     // 4
  sta $d018           // 4 select charset = 00/02/04/06/08
  lda dd02Write: dd02Tab,y     // 4       
  sta $dd02           // 4 3c/3d/3e/3f   -> todo : we have to write this earlier.. idea is to use X and load with LAX.
  tsx

  inc dummy

  dey                   // 2
  bpl nextLine          // 2/3
  // end of the bar...  
  ldy nextLine: $ea     // 3/2  (ldy $ea -> $ea holds 0 = select last raster = black)
                        // 7
nextLine2:

  inx                   // 2
  cpx #192              // 2
  bne loop              // 3
endLoop:
  lda #$fa
  sta $d012
  lda #<bottomIRQ
  sta $fffe
  lda #>bottomIRQ
  sta $ffff
  // waste cycles before to get the write in the sideborder and hide the grey dot bug
  lda #background
  sta $d021
  lda #((screen&$3fff)/$0400)*$10+((charseta&$3fff)/$800)*$2
  sta $d018

  asl $d019

  ldx saves: #0
  txs

  ldy savey: #0
  ldx savex: #0
  lda savea: #0
  inc 0

  rti  
}

.var Wait     = $00
.var FadeIn   = $01
.var FadeOut  = $02
.var FadeOut2 = $03
.var Movement = $04
.var End      = $ff

script:
{
  lda wait: #0
  beq advanceScript
  dec wait
  rts

advanceScript:
  ldx scriptPointer: #0
  lda scriptData,x
  
  cmp #Wait
  bne testFadeIn
  {
    lda scriptData+1,x
    sta wait
    inx
    inx
    stx scriptPointer
    rts
  }
testFadeIn:
  cmp #FadeIn
  bne testFadeOut
  {
    lda #1
    sta fadeIn
    lda #0
    sta fadeInPhase
    sta fadeOut
    lda #(fadeSineEnd-fadeSine-1)
    sta fadeOutPhase
    inc scriptPointer
    rts
  }
testFadeOut:
  cmp #FadeOut
  bne testFadeOut2
  {
    lda #1
    sta fadeOut
    lda #(fadeSineEnd-fadeSine-1)
    sta fadeOutPhase
    lda #0
    sta fadeIn
    sta fadeInPhase
    inc scriptPointer
    rts
  }
testFadeOut2:
  cmp #FadeOut2
  bne testMovement
  {
    lda #1
    sta move1.fadeOut
    inc scriptPointer
    rts
  }
testMovement:
{
  cmp #Movement
  bne testEnd
  lda scriptData+1,x
  sta move.movement
  inc scriptPointer
  inc scriptPointer
  rts 
}
testEnd:
  cmp #End
  bne endScript
  {
    #if !AS_SPINDLE_PART
      lda #0
      sta scriptPointer
    #endif

    #if AS_SPINDLE_PART
      lda #1
      sta bottomIRQ.screenoff
      inc nextpart
    #endif
  }
endScript:
  rts
}

scriptData:
  .byte Movement,0
  .byte FadeIn
  .byte Wait,255
  .byte Wait,120+126
  .byte FadeOut
  .byte Wait,128
  .byte Movement,1
  .byte Wait,255
  .byte Wait,(2*28)-1
  .byte FadeOut2
  .byte Wait,250
  .byte End

.align $100
*=* "[REALTIME] colortab"
colorTable:
  .fill 256,0

*=charsetd+40+$400 "[GFX] biggest line"
charset3:
  .fill 8,$00
  .fill 9,[$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  .fill 16,$ff
  .fill 8,$0


* = * "[CODE] 2nd part"
scrollIt:
  lda charset1+39*8+7
  lsr
  .for (var i=0; i<40; i++) ror charset1+i*8+7

  lda charset2+39*8+7
  lsr
  .for (var i=0; i<40; i++) ror charset2+i*8+7
  rts

scrollIt2:
  lda charset4+39*8+7
  lsr
  .for (var i=0; i<40; i++) ror charset4+i*8+7

  lda charset3+39*8+7
  lsr
  .for (var i=0; i<40; i++) ror charset3+i*8+7

  rts

// this table has a value whenever a new bar starts
// we should only have to update this table!!
.align $100
* = * "[REALTIME] bartable"
barTable:
  .fill 256,0
startAt:
  .fill 256,0
d011Tab:
  .fill 256,((i+7)&7)|$18

.align $100
* = * "[DATA] d018tabhigh"
d018TabHigh:
  .for (var i=0;i<128;i++)
  {
    .if (i<(size1+1)) 
    {
      .byte >d018_33
    }
    else 
    {
      .byte >d018_64
    }
  }

*=charsete+40+$400 "[GFX] inv. biggest line"
charset4:
  .fill 8,$ff
  .fill 9,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff]
  .fill 16,$00
  .fill 8,$ff

*=* "[DATA] fade sine"
fadeSine:
{
    .var sinSize = 256
    .var sinMin  = 00
    .var sinMax  = 200
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .var phase   = 0.75 * sinSize
    .fill sinSize/2, ((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians((mod(i-phase,sinSize))*360/sinSize)))
}
fadeSineEnd:

.align $100
*=* "[DATA] d021Tab greenbrown"
  d021_33b: calcColors(size1+1, 1)
.align $100
  d021_64b: calcColors(size2+1, 1)

* = $3fff "[GFX] ghostbyte"
  .byte 0

* = $4000 "[CODE] move"
move:
{
  lda movement: #0
  beq move0
  jmp move1
}

* = * "[CODE] move0"
move0:
{
  ldx sinPhase

  // clear old bars
  lda #0
  .for (var i=0; i<nrBars; i++)
  {
    //ldy sortedSizes+i
    ldy sortedOffset+i // test
    sty writeAtEnd0    // do an add for free
    ldy sortedYValues+i
    sta barTable,y
    sta writeAtEnd0: barTable,y
    lda #0
    sta sortedInvisible+i
  }

  // when plotting : add the biggest bars at the start of the table!!

  // new positions
  lda sinPhase
  sec
  sbc #1
  and #$7f
  sta sinPhase

  ldx sinPhase

  // position and size of new bars
  lda #size1   // all the same size for now..
  .for (var i=1; i<nrBars; i++)
  {
    sta sizes+i
    sta offset+i
  }

  .for (var i=0; i<nrBars; i++)
  {
    txa
    clc
    
    .if (nrBars>4)
    {
      adc #(mod(i*12,sinSize1))
      and #$7f
      tay
      lda sine,y
    }
    else
    {
      adc #(mod(i*16,sinSize1))
      and #$7f
      tay
      lda sine,y 
    }

    sta yValues+i
  }

  // grotere bars
  lda #size2
  sta sizes
  sta offset
  lda sine2,x
  sta yValues

  lda #>d021_64
  sta colors
  .for (var i=1; i<nrBars; i++)
  {
    .if (mod(i,4)==0)
    {
      lda #>d021_33
      sta colors+i
    } 
    .if (mod(i,4)==1)
    {
      lda #>d021_33b
      sta colors+i
    } 
    .if (mod(i,4)==2)
    {
      lda #>d021_33d
      sta colors+i
    } 
    .if (mod(i,4)==3)
    {
      lda #>d021_33c
      sta colors+i
    } 
  }
  rts
}

* = * "[CODE] move1"
move1:
{
  // clear old bars
  lda #0
  sta moveUpPixels
  sta moveDownPixels

  .for (var i=0; i<nrBars; i++)
  {
    //ldy sortedSizes+i
    ldy sortedOffset+i // test
    sty writeAtEnd0    // do an add for free
    ldy sortedYValues+i
    sta barTable,y
    sta writeAtEnd0: barTable,y
    lda #0
    sta sortedInvisible+i
  }

  // when plotting : add the biggest bars should have the lowest order (be in front)

  // new positions
  lda sinPhase2: #0
  clc
  adc #1

  .if (sinSize3!=256)
  {
    cmp #sinSize3
    bcc !+
      lda #0
    !:
  }
  sta sinPhase2

  // position and size of new bars
  lda #size2   // all the same size for now..
  .for (var i=0; i<nrBars; i++)
  {
    sta sizes+i
    sta offset+i
  }

  ldx sinPhase2
  txa
  eor #$ff
  sta order

  cpx #0
  bne !+
  {
    // this is the place to fade out
    lda fadeOut
    beq cont
      lda #0
      sta visible
    cont:
  }
  !:
  lda visible
  bne !+
    lda #$ff
    sta yValues
    bne done
  !:
  lda sine3+sinSize3,x // high byte
  bpl positive
    lda sine3,x
    clc
    adc #size2
    bcs !+
      lda #$ff
      sta yValues
      bne done
    !:
    sta offset
    lda #0
    sta yValues
    beq done
  positive:
  lda sine3,x
  sta yValues
  done:

  .for (var i=1; i<nrBars; i++)
  {
    txa
    sec
    sbc #(sinSize3/8)
    bcs !+
    {
      clc
      adc #sinSize3
    }
    !:
    tax
    eor #$ff
    sta order+i
    cpx #0
    bne !+
    // this is the moment to make the bar visible or invisible, when the phase passes through zero
    {
      lda fadeOut
      bne !+
        lda #1
        sta visible+i
        bne cont
      !:
        lda #0
        sta visible+i
      cont:
    }
    !:

    lda visible+i
    bne plot
    {
      lda #$ff
      sta yValues+i
      bne done
    }
    plot:

    // flip every other bar (it just doesn't look nice..)
    //.if (mod(i,2)==1)
    //{
    //  stx temp
    //  lda #sinSize3-1
    //  sec
    //  sbc temp: #0
    //  tay
    //} else
    //{
      txa
      tay
    //}

    lda sine3+sinSize3,y // high byte
    bpl positive
      lda sine3,y
      clc
      adc #size2
      bcs !+
        lda #$ff
        sta yValues+i
        bne done
      !:
      sta offset+i
      lda #0
      sta yValues+i
      beq done
    positive:
    lda sine3,y
    sta yValues+i
    done:
  }

  lda #>d021_64
  sta colors
  .for (var i=1; i<nrBars; i++)
  {
    .if (mod(i,4)==0)
    {
      lda #>d021_64
      sta colors+i
    } 
    .if (mod(i,4)==1)
    {
      lda #>d021_64b
      sta colors+i
    } 
    .if (mod(i,4)==2)
    {
      lda #>d021_64d
      sta colors+i
    } 
    .if (mod(i,4)==3)
    {
      lda #>d021_64c
      sta colors+i
    } 
  }
  rts

  fadeOut:
    .byte 0
  visible:
    .byte 1
    .fill nrBars-1,0
}

* = * "[CODE] plotbars"
plotBars:
  lda fadeIn: #0
  beq skipFadeIn
  lda #0
  sta moveDownPixels
  ldx fadeInPhase: #10
  lda fadeSine,x
  sta moveUpPixels
  
  inc fadeInPhase
  cpx #(fadeSineEnd-fadeSine-1)
  bne skipFadeIn
  lda #0
  sta fadeIn
skipFadeIn:

  lda fadeOut: #0
  beq skipFadeOut
  lda #0
  sta moveUpPixels
  ldx fadeOutPhase: #0
  lda fadeSine,x
  sta moveDownPixels
  
  dec fadeOutPhase
  cpx #0
  bne skipFadeOut
  lda #0
  sta fadeOut
skipFadeOut:

  // sort the bars
  jsr sort

  // after sorting, move them up together
  lda moveUpPixels
  beq skipMoveUp
  ldx #nrBars-1
  moveUpLoop:
    lda sortedYValues,x
    sec
    sbc moveUpPixels: #200

    bcs completelyVisible // the whole bar is visible.. 
    // is the bar completely invisible?

    // if we add the size of the bar and get positive again, then part of the bar is visible
    clc
    adc sortedSizes,x
    bcs partlyVisible
    // the bar is invisible.. let's put the y-value at an invisible position if this is the last bar

    lda #255              
    sta sortedInvisible,x
    lda #0 
    sta sortedOffset,x
    bne completelyVisible

    partlyVisible:
    sta sortedOffset,x  // we get the # of pixels visible for free from the previous adc
    lda #0        // set y value to 0
  completelyVisible:
    // do nothing
    sta sortedYValues,x
  
  dex
  bpl moveUpLoop
skipMoveUp:

  // after sorting, move them down together
  lda moveDownPixels
  beq skipMoveDown

  ldx #nrBars-1
  moveDownLoop:
    lda sortedYValues,x
    clc
    adc moveDownPixels: #$00

    bcc Visible // the whole bar is visible.. 
    // max value to #$ff
    lda #$ff
  Visible:
    sta sortedYValues,x
  dex
  bpl moveDownLoop
skipMoveDown:

  // plot the bars

  // we can plot the topmost bar directly..
  lda sortedInvisible
  bne skipWrite
  ldy sortedYValues
  lda sortedSizes
  sta barTable,y
  lda sortedOffset
  sta startAt,y
  lda sortedColors
  sta colorTable,y
skipWrite:
  .for (var i=0; i<nrBars-1; i++)
  {
    // the previous bar is up to this position..
    lda sortedYValues+i
    clc
    //adc sortedSizes+i  // test
    adc sortedOffset+i
    tay

    // is the next bar below the previous?
    cmp sortedYValues+i+1
    bcc !+    // if the bottom of the first is higher than the start of the 2nd, there is no problem
    
    // the bars overlap... which one should be on top?
    lda sortedOrder+i+1
    cmp sortedOrder+i
    bcc !+  // the bars overlap, but this bar goes in the front. 
    
    // does the next bar start before this one will, now that we have to draw it later?
    .if (i<(nrBars-2)) // only if there is a next bar
    {
      // is the next bar in front of this one?
      lda sortedOrder+i+2
      cmp sortedOrder+i+1
      bcs checkNext  // no, there is no problem, but also check the next bar

      // does it start before the new start position? (y)
      cpy sortedYValues+i+2
      bcc ok
      // the current bar starts lower than the next bar which has priority, so we can skip it
      // copy the data from the previous bar to this one, since the current one doesn't exist
      ldx #i
      jsr copyToNext
      jmp next

    checkNext:
      // how about the next one after this one?
      .if (i<(nrBars-3)) // only if there is a next,next bar
      {
        // is the next bar in front of this one?
        lda sortedOrder+i+3
        cmp sortedOrder+i+1
        bcs checkNext2  // no, there is no problem, but check the next one

        // does it start before the new start position? (y)
        cpy sortedYValues+i+3
        bcc ok
        // the current bar starts lower than the next bar which has priority, so we can skip it
        // copy the data from the previous bar to this one, since the current one doesn't exist
        ldx #i
        jsr copyToNext
        jmp next

      checkNext2:
        // how about the next one after this one?
        .if (i<(nrBars-4)) // only if there is a next,next,next bar
        {
          // is the next bar in front of this one?
          lda sortedOrder+i+4
          cmp sortedOrder+i+1
          bcs ok  // no, there is no problem, but check the next one

          // does it start before the new start position? (y)
          cpy sortedYValues+i+4
          bcc !+  
          // the current bar starts lower than the next bar which has priority, so we can skip it
          // copy the data from the previous bar to this one, since the current one doesn't exist
          ldx #i
          jsr copyToNext
          jmp next
        }
      }
    !:
    ok:
    }

    lda sortedYValues+i+1
    clc
    adc sortedOffset+i+1
    sec
    sbc sortedYValues+i
    sbc sortedOffset+i

    bcs noWorries
      // this bar is completely covered, so we have to skip it

      // copy the data from the previous bar to this one, since the current one doesn't exist
      ldx #i
      jsr copyToNext
      jmp next
    noWorries:
    bit sortedInvisible+i+1
    bmi next     // skip write if invisible
    bpl writeBar
  !:
    // the bar can be placed without problem
    bit sortedInvisible+i+1
    bmi next

    ldy sortedYValues+i+1
    lda sortedOffset+i+1
  writeBar:
    sta startAt,y
    lda sortedSizes+i+1
    sta barTable,y
    lda sortedColors+i+1
    sta colorTable,y
  next:
  }
  rts

copyToNext:
{
  lda sortedYValues,x
  sta sortedYValues+1,x
  lda sortedSizes,x
  sta sortedSizes+1,x
  lda sortedOrder,x
  sta sortedOrder+1,x   
  lda sortedOffset,x       // test
  sta sortedOffset+1,x     // test
  lda sortedInvisible,x    // test
  sta sortedInvisible+1,x  // test
  lda sortedColors,x
  sta sortedColors+1,x
  rts
}
*=* "yvalues"
yValues:
  .fill nrBars,0
sizes:
  .fill nrBars,0
offset:
  .fill nrBars,0
order:
  .fill nrBars,i
colors:
  .for (var i=0; i<nrBars; i++)
  {
    .if (mod(i,4)==0) .byte >d021_33
    .if (mod(i,4)==1) .byte >d021_33b
    .if (mod(i,4)==2) .byte >d021_33d
    .if (mod(i,4)==3) .byte >d021_33c
  }

* = *  "[CODE] sort"
sort:
{
  // find the smallest and put it at the top
  // this works kinda like a bubble sort
  .for (var j=0; j<nrBars; j++)
  {
    ldy #$fe
    lda #$fe
    .for (var i=0; i<nrBars; i++)
    {
      bit skip+i     // value already in list
      bmi !+         // yes, skip it
      cmp yValues+i
      bcc !+         // skip if the current value is smaller
      beq !+

      ldx #i
      lda yValues+i
    !:
    }
    sta sortedYValues+j
    lda #$ff
    sta skip,x
    lda sizes,x
    sta sortedSizes+j
    lda order,x
    sta sortedOrder+j
    lda offset,x
    sta sortedOffset+j
    lda colors,x
    sta sortedColors+j
  }
  lda #0
  .for (var i=0; i<nrBars; i++)
  {
    sta skip+i
  }


  // now sort the y-values with 0, we have to sort by the offsets
  // we can do this neatly, but i'm not into it right now..

resort:
  ldy #0  // did we resort?

  .for (var j=0; j<(nrBars-1); j++)
  {
    lda sortedYValues+j+1
    bne sorted // extra sorting not needed

    lda sortedOffset+j+1    // this is the 2nd offset
    cmp sortedOffset+j      // this is the first offset
    bcs sorted
    ldx #j
    jsr swap
    sorted:
  }
  cpy #0
  beq !+
    jmp resort
  !:
}
waste12:
  rts

.macro swapone(array)
{
  lda array,x
  ldy array+1,x
  sta array+1,x
  tya
  sta array,x
}

swap:
  swapone(sortedSizes)
  swapone(sortedOrder)
  swapone(sortedOffset)
  swapone(sortedColors)
 
  iny
  rts

*=* "[DATA] sine3"
sine3:
{
    .var valueList = List()
    .var sinSize = sinSize3
    .var sinMin  = 00
    .var sinMax  = 200-size1-9
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .for (var i=0; i<sinSize; i++)
    {
      .var value = i + (1.5*256)/(2*3.1415)*sin(i/sinSize*2*3.1415) - (128)/(2*3.1415)*sin(((2*i)/sinSize)*2*3.1415)

      .eval value = value/sinSize*300     // scale
      .eval value = 300-value-64-18   // invert + centre
      .eval value = round(value)      // round
      .eval valueList.add(value)
    }
    .lohifill valueList.size(), valueList.get(i)
}

*=* "sorted values"
// sorted Y values
sortedYValues:
  .fill nrBars,0
sortedSizes:
  .fill nrBars,0
sortedOffset:
  .fill nrBars,0
sortedOrder:
  .fill nrBars,0
sortedInvisible:
  .fill nrBars,0
sortedColors:
  .fill nrBars,0
skip:             // already sorted?
  .fill nrBars,0
