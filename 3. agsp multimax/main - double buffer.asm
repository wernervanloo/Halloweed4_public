.const maxCrunch = 5
.const flipAfter = 20
//#define debug

// name idea : clarence : agsp max 
// this part : agsp multimax

// idea : triple buffer to save cycles
// lda bitmap1,x
// sta bitmap2+..,x
// sta bitmap3+..,x
// saves : 0.5 lda per bitmap
// save : 8192*0.5*4 = 16384 cycles
// maar : 3 bitmaps nodig

// bitmap1    : 4000-5fff + 6800-6bff (6000-67ff empty to avoid bugs)
// code       : 8000-a000
// d800colors : a000-a4ff
// bitmap2    : c000-dfff + e800-e8ff (e000-e7ff empty to avoid bugs -> still needed?!?!)

// problem : only 2 bitmap in low area. low area is nice to avoid $3fff 
// not possible with 3 bitmaps : move to high area
//  sid                 : 1000-1fff
// bitmap1              : 2000-3fff
// screen1              : 4000-47ff
//  bitmap copy-in data : 4800-5fff
// bitmap2              : 6000-7fff
// screen2              : 8000-87ff
//   code               : 8800-9fff
// bitmap3              : a000-bfff
// screen3              : c000-c7ff
// save code memory by modifying copyBitmap routines!

// frames       show    finish       50%
//    0-5  | bitmap1 | bitmap2 | bitmap3
//   6-11  | bitmap2 | bitmap3 | bitmap1
//  12-17  | bitmap3 | bitmap1 | bitmap2
//  18-23  | bitmap1 | bitmap2 | bitmap3

//d011 write at line/cycle, value

//$33,#60,$7b (was 7c)
//7f naar d011 at lijn 35/6

//32,60,7a (was 7b)
//33,58,7b
//7f naar d011 at lijn 35/6

//31,60,79  (was 7a)
//32,58,7a
//33,58,7b
//->7f 35/6

//30,60,78  (was : 79)
//31,58,79
//32,58,7a
//33,58,7b
//->7f 35/6

//2f,60,7f (was : 78)
//30,58,78
//31,58,79
//32,58,7a
//33,58,7b
  // 34,58,7c
  //->7f 53/6


// maxcrunch = 6, linecrunch = 0
// 35,59,3D check
//
// 34,59,3C
// 35,57,3D


.var music = LoadSid("../music/Dumpbass.sid")
//.var music = LoadSid("../music/freakandel.sid")

#if !AS_SPINDLE_PART
      *=music.location "[MUSIC]"
               .fill music.size, music.getData(i)

.label bitmap1     = $4000
// 6000-67ff is empty to avoid bugs
.label screen1     = $6800
.label code        = $8000
.label d800Colors  = $a000
.label bitmap2     = $c000
.label screen2     = $e800

.label linecrunch = $10

.label atemp      = $20
.label xtemp      = $21
.label ytemp      = $22
.label vicbank    = $23
.label sinePhase  = $24
.label low        = $25
.label high       = $26
.label low2       = $27
.label high2      = $28

:BasicUpstart2(start)

* = code "[CODE] main"
start:
{
  sei

  lda #$35
  sta $01

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

  #if !AS_SPINDLE_PART
    jsr prepare
  #endif

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d

  lda #<bottomirq
  sta $fffe
  lda #>bottomirq
  sta $ffff
  lda #$fa
  sta $d012

  lda #$3f
  sta $d011

  jsr modifyToUp
  jsr copyBitmap1    // prepare bitmap 2  copyBitmap1Up

  lda #0
  sta scrollMove.direction  
  sta linecrunch
  sta sinePhase
  lda #1
  sta vicbank      // show bitmap1

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
    lda #0
    jsr music.init
  #endif

  lda #$09
  sta $d021
  lda #$d8
  sta $d016
  lda #$01
  sta $dd02
  lda #$90
  sta $d018

  lda #$00
  sta $7fff

  lda $dc0d
  lda $dd0d
  asl $d019

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
d011tab:
  .byte $3c,$3b,$3a,$39,$38,$3f
d011tab2:
  .byte $3d,$3c,$3b,$3a,$39,$38
d012tab:
  .byte $34,$33,$32,$31,$30,$2f

waste37:
  lda waste36: #$a9
waste35:
  lda waste34: #$a9
waste33:
  lda waste32: #$a9
waste31:
  lda waste30: #$a9
waste29:
  lda waste28: #$a9
waste27:
  lda waste26: #$a9
waste25:
  lda waste24: #$a9
waste23:
  lda waste22: #$a9
waste21:
  lda waste20: #$a9
waste19:
  lda waste18: #$a9
waste17:
  lda waste16: #$a9
waste15:
  bit waste14: $ea
waste12:
  rts

prepare:
{
  ldx #0
loop:
  lda d800Colors,x
  sta $d800,x
  lda d800Colors+$100,x
  sta $d800+$100,x
  lda d800Colors+$200,x
  sta $d800+$200,x
  lda d800Colors+$300,x
  sta $d800+$300,x
  inx
  bne loop
  rts
}

.align $100

linecrunchirq:
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
  // we would have 15 cycles until needed sta $d011 and have to update :
  // x     (3 cycles)
  // $ffff (6 cycles)

  stx savex

  ldx.abs linecrunch
  nop
  nop
  lda d011tab,x

  // we are at cycle 59/$3b
  sta $d011
  lda #0
  sta $ffff

  lda #<bottomirq
  sta $fffe

  jsr waste25

  dex
  bmi stoploop

loop:
  lda d011tab,x
  jsr waste12
  sta $d011
  jsr waste36
  dex
  nop
  bpl loop
  nop

stoploop:
  lda #$fa
  sta $d012
  dec $d011
  ldx savex: #0

  lda #$28          
  sec
  sbc xoffset: #0   // 0 = 15 cycles
  sta dmaDelay+1
dmaDelay:
  bcs *             // 12
  lda #$a9  //0
  lda #$a9  //2
  lda #$a9  //4
  lda #$a9  //6
  lda #$a9  //8
  lda #$a9  //10
  lda #$a9  //12
  lda #$a9  //14
  lda #$a9  //16
  lda #$a9  //18
  lda #$a9  //20
  lda #$a9  //22
  lda #$a9  //24
  lda #$a9  //26
  lda #$a9  //28
  lda #$a9  //30
  lda #$a9  //32
  lda #$a9  //34
  lda #$a9  //36
  lda #$24  //38
  nop       //40   40 = 2 cycles
  
  lda #$3e
  sta $d011
  inc $d011

  lda vicbank
  sta $dd02
  lda #$a0
  sta $d018

  lda #>bottomirq
  sta $ffff
  asl $d019

  inc 0
  lda savea: #0
  rti  
}

bottomirq:
{
  pha
  txa
  pha
  tya
  pha
  lda $01
  pha

  lda #$35
  sta $01

  lda #0      // hide bugs
  sta $ffff

  lda #$37    // open border
  sta $d011

  // switching between up and down :
  // -> if we are at the max offset for a certain direction, just before we switch to a new bitmap 
  //    scrollup   : linecrunch == maxCrunch
  //    scrolldown : linecrunch == 0
  // then we can switch direction and start scrolling the other way

  jsr scrollMove
  jsr music.play

  // x movement
  lda sinePhase
  clc
  adc #1
  and #$7f
  sta sinePhase
  tax
  lda sineX,x
  sta $d016
  lda sineX+128,x
  sta linecrunchirq.xoffset

  // y movement
  ldx linecrunch
  lda d012tab,x
  sta $d012
  lda d011tab2,x
  sta $d011

  lda #$88     // hide bugs..
  sta $d018
  lda #1
  sta $dd02

  lda #<linecrunchirq
  sta $fffe
  lda #>linecrunchirq
  sta $ffff
  asl $d019

  cli
  jsr scrollCopy

  pla
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti
}

scrollMove:
{
  lda direction: #0
  bne scrollDown

scrollUp:
  lda linecrunch   // 0, 1, 2, 3, 4, 5 -> 0
  clc
  adc #1
  cmp #maxCrunch+1
  bcc !+
  // flip?
  dec flipIt
  bne noFlip
  lda #1
  sta direction
  rts

noFlip:
  // keep track of where $d800 colors come from
  lda topD800
  clc
  adc #6*40
  sta topD800
  lda topD800+1
  adc #0
  and #3
  sta topD800+1

  // switch to the other bitmap
  lda vicbank
  eor #2
  sta vicbank
  inc scrollCopy.copyUp
  lda #0
!:
  sta linecrunch
  rts

scrollDown:
  dec linecrunch  // 5, 4, 3, 2, 1, 0 -> 5
  bpl !+
  // flip?
  dec flipIt
  bne noFlip2
  lda #0
  sta direction
  sta linecrunch
  rts

noFlip2:
  // keep track of where $d800 colors come from
  lda topD800
  sec
  sbc #6*40
  sta topD800
  lda topD800+1
  sbc #0
  and #3
  sta topD800+1

  // switch to the other bitmap
  lda vicbank
  eor #2
  sta vicbank
  inc scrollCopy.copyDown
  lda #maxCrunch
  sta linecrunch
!:
  rts
}

flipIt:
  .byte flipAfter

// keep track of top $d800 position (where do the colors come from for the first $d800 row?)
topD800:
  .byte >$0000,<$0000  // f0 = first move up

scrollCopy:
{
  lda scrollMove.direction
  bne scrollDown

  lda flipIt
  bne !+
  jsr modifyToUp
  lda #flipAfter
  sta flipIt
  bne onlyBitmapUp

  // if we flip, we do not have to copy $d800, but we do have to modify the screen and bitmap!!

!:
  lda copyUp: #0
  beq endCopy
  lda #0
  sta copyUp
  jsr copyD800Up
onlyBitmapUp:
  #if debug
    lda #7
    sta $d020
  #endif
  jmp copyBitmap
endCopy:
  rts

scrollDown:
  lda flipIt
  bne !+
  jsr modifyToDown
  lda #flipAfter
  sta flipIt
  bne onlyBitmapDown
!:
  lda copyDown: #0
  beq endCopy
  lda #0
  sta copyDown
  jsr copyD800Down
onlyBitmapDown:
  #if debug
    lda #7
    sta $d020
  #endif
  jmp copyBitmap
}

// turn copyBitmap into copyBitmapUp
modifyToUp:
{
  lda #>(screen2 + ((-6*40)&$3ff))
  sta modify.writehigh3
  and #$fc
  sta modify.screenHigh1
  sta modify.screenHigh2
  lda #<(screen2 + ((-6*40)&$3ff))
  sta modify.writelow3

  lda #>(bitmap2 + ((-6*320)&$1fff))
  ldx #>(bitmap1 + ((-6*320)&$1fff))
  bne modify
}

// turn copyBitmap into copyBitmapDown
modifyToDown:
{
  lda #>(screen2 + ((6*40)&$3ff))
  sta modify.writehigh3
  and #$fc
  sta modify.screenHigh1
  sta modify.screenHigh2
  lda #<(screen2 + ((6*40)&$3ff))
  sta modify.writelow3

  lda #>(bitmap2 + ((6*320)&$1fff))
  ldx #>(bitmap1 + ((6*320)&$1fff))
}
modify:  
{
  sta writehigh
  stx writehigh2

  lda #<(copyBitmap1.loop+4)
  sta low
  lda #>(copyBitmap1.loop+4)
  sta high
  lda #<(copyBitmap2.loop+4)
  sta low2
  lda #>(copyBitmap2.loop+4)
  sta high2

  ldx #(128/4)-1
  clc
loop:
  /* actually the low byte does not have to be updated...
  ldy #0
  lda #$80
  sta (low),y
  ldy #6
  lda #$c0
  sta (low),y  
  ldy #12
  lda #$00
  sta (low),y  
  ldy #18
  lda #$40
  sta (low),y  
  */

  ldy #1
  lda writehigh: #>(bitmap2 + ((6*320)&$1fff))
  sta (low),y
  ldy #7
  sta (low),y
  ldy #13
  adc #1
  and #$1f
  ora #>bitmap2
  sta (low),y
  ldy #19
  sta (low),y
  sta writehigh

  ldy #1
  lda writehigh2: #>(bitmap1 + ((6*320)&$1fff))
  sta (low2),y
  ldy #7
  sta (low2),y
  ldy #13
  adc #1
  and #$1f
  ora #>bitmap1
  sta (low2),y
  ldy #19
  sta (low2),y
  sta writehigh2

  // next 4

  lda low
  adc #6*4
  sta low
  bcc !+
  inc high
  clc
!:
  lda low2
  adc #6*4
  sta low2
  bcc !+
  inc high2
  clc
!:
  dex
  bpl loop
  
  // modify copyScreen

  lda #(128/2)
  sta flipsLeft

  lda #<(copyScreen1.loop+4)
  sta low
  lda #>(copyScreen1.loop+4)
  sta high

  lda #<(copyScreen2.loop+4)
  sta low2
  lda #>(copyScreen2.loop+4)
  sta high2
  ldx writehigh3: #0

flipScreenLoop:
  ldy #0
  lda writelow3:  #0
  sta (low),y
  sta (low2),y
  adc #8
  sta writelow4

  iny
  txa
  sta (low),y
  eor #>(screen1^screen2)
  sta (low2),y
  bcc !+
  inx
  txa
  and #3
  ora screenHigh1: #>screen2
  tax
  clc
!:
  ldy #6
  lda writelow4: #0
  sta (low),y
  sta (low2),y
  adc #8
  sta writelow3

  iny
  txa
  sta (low),y
  eor #>(screen1^screen2)
  sta (low2),y
  bcc !+
  inx
  txa
  and #3
  ora screenHigh2: #>screen2
  tax
  clc
!:
  lda low
  adc #12
  sta low
  bcc !+
  inc high
  clc
!:
  lda low2
  adc #12
  sta low2
  bcc !+
  inc high2
  clc
!:

  dec flipsLeft
  bne flipScreenLoop

  rts
flipsLeft:
  .byte 0
}

copyBitmap:
  lda vicbank
  cmp #1
  beq copyBitmap1
  jmp copyBitmap2

* = * "[CODE] copyBitmap1"
copyBitmap1:
{
  lda #$34
  sta $01

  ldx #$3f
loop:
  .for (var stripe=0; stripe<128; stripe++)
  {
    lda bitmap1 + ((0*320+stripe*64)&$1fff),x  // 6*320 = $780. so there are no penalties crossing a page
    sta bitmap2 + ((-6*320+stripe*64)&$1fff),x
  }

  dex
  bmi endloop
  jmp loop
endloop:
  lda #$35
  sta $01
}

* = * "[CODE] copyScreen1Up"
copyScreen1:
{
  ldx #$07
loop:
  .for (var stripe=0; stripe<128; stripe++)
  {
    lda screen1 + ((0*40+stripe*8)&$3ff),x
    sta screen2 + ((-6*40+stripe*8)&$3ff),x
  }

  dex
  bmi endloop
  jmp loop
endloop:

  #if debug
    lda #0
    sta $d020
  #endif
  rts
}

* = * "[CODE] copyBitmap2"
copyBitmap2:
{
  lda #$34
  sta $01

  ldx #$3f
loop:
  .for (var stripe=0; stripe<128; stripe++)
  {
    lda bitmap2 + ((0*320+stripe*64)&$1fff),x  // 6*320 = $780. so there are no penalties crossing a page
    sta bitmap1 + ((-6*320+stripe*64)&$1fff),x
  }

  dex
  bmi endloop
  jmp loop
endloop:
  lda #$35
  sta $01
}

* = * "[CODE] copyScreen2Up"
copyScreen2:
{
  ldx #$07
loop:
  .for (var stripe=0; stripe<128; stripe++)
  {
    lda screen2 + ((0*40+stripe*8)&$3ff),x
    sta screen1 + ((-6*40+stripe*8)&$3ff),x
  }

  dex
  bmi endloop
  jmp loop
endloop:

  #if debug
    lda #0
    sta $d020
  #endif
  rts
}

* = * "[CODE] copy d800 up"
copyD800Up:
{
  #if debug
    dec $d020
  #endif

  ldx #39
!:
  .for (var row=0;row<12;row++)
  {
    lda $d800+6*40+row*40,x
    sta $d800+0*40+row*40,x
  }
  dex
  bpl !-

  // we are here before the upper border ends, so there are no timing issues in the code before

  #if debug
    dec $d020
  #endif
  
  ldx #15
!:
  .for (var i=0; i<19; i++)
  {
    lda $d800+18*40+i*16,x
    sta $d800+12*40+i*16,x
  }
  dex
  bpl !-

  #if debug
    dec $d020
  #endif

  // first copyoffset = $0000
  // 2nd copyoffset = ($0000+240)&$3f8

  lda topD800
  clc
  adc #<(1024-6*40)
  sta fromD800a
  lda topD800+1
  adc #>(1024-6*40)
  and #3
  ora #>d800Colors
  sta fromD800a+1

  lda fromD800a
  clc
  adc #120
  sta fromD800b
  lda fromD800a+1
  adc #0
  and #3
  ora #>d800Colors
  sta fromD800b+1

  ldx #119
loop3:
  lda fromD800a: d800Colors,x
  sta $db10,x
  lda fromD800b: d800Colors+120,x
  sta $db10+120,x
  dex
  bpl loop3

  #if debug
    lda #0
    sta $d020
  #endif
  rts
}

* = * "[CODE] copy d800 down"
copyD800Down:
{
  #if debug
    dec $d020
  #endif

  // update the odd copies

  lda topD800
  sta fromD800a
  sta fromD800c
  sta fromD800e
  sta fromD800g

  lda topD800+1
  ora #>d800Colors
  sta fromD800a+1
  clc
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800c+1  
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800e+1   
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800g+1  

  // update the even copies
  lda fromD800a
  //clc
  adc #128
  sta fromD800b
  sta fromD800d
  sta fromD800f
  sta fromD800h
  lda fromD800a+1
  adc #0
  and #3
  ora #>d800Colors
  sta fromD800b+1
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800d+1
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800f+1
  adc #1
  and #3
  ora #>d800Colors
  sta fromD800h+1

  // while copying, we don't want to cross $dbff -> $dc00
  // we are copying 6 rows down

  ldx #$7f
!:
  lda fromD800a: d800Colors+(($000-6*40)&$3ff),x  // todo: we should try to avoid crossing pages here..
                                                  // shifts by $f0 everytime, so to avoid crossing the page we can copy max $10 bytes
  sta $d800,x
  lda fromD800b: d800Colors+(($080-6*40)&$3ff),x
  sta $d880,x
  lda fromD800c: d800Colors+(($100-6*40)&$3ff),x
  sta $d900,x
  lda fromD800d: d800Colors+(($180-6*40)&$3ff),x
  sta $d980,x
  dex
  bpl !-

  // we are here before the upper border ends.. so before this there are no timing problems

  #if debug
    dec $d020
  #endif

  ldx #$7f
!:
  lda fromD800e: d800Colors+(($200-6*40)&$3ff),x  // todo: we should try to avoid crossing pages here..
  sta $da00,x
  lda fromD800f: d800Colors+(($280-6*40)&$3ff),x
  sta $da80,x
  dex
  bpl !-

  ldx #$7f
!:
  lda fromD800g: d800Colors+(($300-6*40)&$3ff),x
  sta $db00,x
  lda fromD800h: d800Colors+(($380-6*40)&$3ff),x
  sta $db80,x
  dex
  bpl !-

  #if debug
    lda #0
    sta $d020
  #endif

  rts
}

.align $100
sineX:
*=* "[DATA] sine for x bounce"
{
    .var sinSize = 128
    .var sinMin  = 00
    .var sinMax  = 40*8
    .var sinAmp  = 0.5 * (sinMax-sinMin)
    .fill sinSize, (((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize)))&$7)|$d0
    .fill sinSize, (((sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize)))&$1f8)>>3
}

* = bitmap1 "[DATA] bitmap data"
.import c64 "includes/weed_and_witches_spot.kla",0,$1f40
* = screen1 "[DATA] screen coolors"
.import c64 "includes/weed_and_witches_spot.kla",$1f40,$3e8
* = $7fff "[DATA] ghost byte" virtual
  .byte 0

* = d800Colors "[DATA] d800 colors"
.import c64 "includes/weed_and_witches_spot.kla",$1f40+$3e8,$3e8
.fill 24,0
.import c64 "includes/weed_and_witches_spot.kla",$1f40+$3e8,$100
