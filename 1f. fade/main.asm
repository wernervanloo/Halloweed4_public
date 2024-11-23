.var music = LoadSid("../music/intro.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .var PLAY=$1003
  .label spindleLoadAddress = $ec00
  *=spindleLoadAddress-18-10-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word prepare       // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word irq.MusicPlayCall // location of playroutine call

    .byte 'S'                   // this part is IO safe and can load below $d000
    .byte 'Z', $e0, $e1
    .byte 'I', $6d, $ac                         // inherit the data for the last bitmap from bitmapscroll (with colors)
                                                // color start at 6d80, bitmap starts at 8d00-acff
    .byte 'P', >(bitmap),>(d800_original+$3e8)  // bitmap gets copied to here and faded

    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart

#else    
    :BasicUpstart2(basicstart)
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

.label fadePointer  = $e0
.label fadePointer2 = $e1

.label firstrow2 = $a4 // used by previous part..

.label bitmapdata      = $8d00 // the bitmapdata from the previous part is at this location
.label AllColors       = $6d80 // all the color data from the previous part are at this location
.label bitmap          = $c000
.label screen          = $e000
.label screen_original = $e400
.label d800_original   = $e800
.label code            = $ec00
.label anim            = $f800

.var screencolour = screen

#if !AS_SPINDLE_PART
  *=bitmapdata "[STANDALONE] test gfx data"
  .import c64 "../1e. bitmapscroll/includes/5spot.kla",0,8000

  *=AllColors+75*40 "[GFX] screencolors"
  .import c64 "../1e. bitmapscroll/includes/5spot.kla",$1f40,1000  

  *=AllColors+4000+75*40 "[GFX] d800 colors"
  .import c64 "../1e. bitmapscroll/includes/5spot.kla",$1f40+$3e8,1000  
#endif

*= bitmap "[RUNTIME] bitmap" virtual
  .fill 25*320,0

*= screen "[RUNTIME] screen ram" virtual
  .fill 1000,0

#if !AS_SPINDLE_PART
  *=bitmap "[STANDALONE] jump for basic start"

  basicstart:
    sei
    lda #$35
    sta $01
    jmp code
#endif

*=anim "[DATA] Animation"

.var animation = LoadBinary("./includes/logo.fade")
.var maxAnimation = 0

.for (var i=0; i<1000; i++) {
  .var Value = animation.get(i)

  .if (Value == 1) {
    .eval Value = Value
  } else {
    .eval Value =  (Value + 6) * 1.3
  }

  .eval Value = round(Value)

  .byte Value
  .if (Value>maxAnimation) .eval maxAnimation = Value
}

*=code "[CODE] main code"
start:
{
  sei
  lda $01
  sta restore01

  lda #$35
  sta $01

  #if !AS_SPINDLE_PART
    jsr prepare
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
    lda #$33
    sta $d011

    lda #$0
    sta $d020
    sta $d021

    jsr music.init
    lda #$94
    sta $dd00
  #endif

  ldx nextpart
  inx
  stx irq.nextPartValue

  lda $dc0d
  lda $dd0d
  asl $d019

  lda restore01: #0
  cmp #$37
  beq !+
  sta $01
!:

  cli

  #if !AS_SPINDLE_PART
    jmp *
  #else
    rts
  #endif
}

prepare:
{
	jsr Fader.GenerateFadeTableColor  // generate color table for fader

   #if AS_SPINDLE_PART
    // wait until data is present
  wait:
    lda firstrow2
    cmp #77
    bcc wait
  #endif

  // copy colors to $e000/$e400/$e800 ($d800 for standalone)

  ldy #4-1 // nr of pages to copy
  ldx #0
  {
  loop:
    lda from:  AllColors+75*40,x        //bitmapdata+$1f40,x
    sta toa:   screen,x
    sta tob:   screen_original,x

    lda from3: AllColors+4000+75*40,x //bitmapdata+$1f40+$3e8,x
    #if !AS_SPINDLE_PART
      sta to3a:  $d800,x
    #endif
  
    sta to3b:  d800_original,x

    inx
    bne loop

    inc from+1
    inc toa+1
    inc tob+1

    inc from3+1
  
    #if !AS_SPINDLE_PART
      inc to3a+1
    #endif
  
    inc to3b+1

    dey
    bpl loop
  }
  // copy bitmap to $c000
  
  lda $01
  sta restore01

  lda #$30
  sta $01

  ldy #32-1  // nr of pages to copy
  ldx #0
  {
    loop:
    lda from: bitmapdata,x
    sta to:   bitmap,x
    inx
    bne loop

    inc from+1
    inc to+1
    dey
    bpl loop
  }

  lda restore01: #0
  sta $01

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
  pha

  lda #$35
  sta $01

  lda #$3f
  sta $dd02
  lda #$80
  sta $d018
  lda #$d8
  sta $d016

  // keep demotime (always before calling music player)
  inc timelow
  bne !+
  inc timehigh
!:

  // mark sid as finished
  lda timehigh
  cmp #$05
  bne !+
  lda timelow
  cmp #$20
  bne !+

  inc sidDone
!:

  MusicPlayCall:
  #if !AS_SPINDLE_PART
    jsr music.play
  #else         
    bit.abs $0000
  #endif  

  jsr script

  lda sidDone: #0
  beq !+
  lda fadeDone: #0
  beq !+
  lda nextPartValue: #0
  sta nextpart
!:

  // only 1 irq..
  lda #<irq
  sta $fffe
  lda #>irq
  sta $ffff
  lda #$fa
  sta $d012
  asl $d019

  lda Fader.IsRunning
  beq noFader
  lda Fader.Busy
  bne noFader

  cli
  jsr Fader.Run

noFader:
  pla
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti
}

.var Wait = $00
.var Fade = $01
.var End  = $ff

script:
  lda wait: #0
  beq advanceScript
  dec wait
  rts

advanceScript:
  ldx scriptPointer: #0
  lda scriptData,x
  cmp #Wait
  bne testFade

  lda scriptData+1,x
  sta wait
  
  inc scriptPointer
  inc scriptPointer
  rts


testFade:
  cmp #Fade
  bne testEnd

  lda #1
  sta Fader.IsRunning
  inc scriptPointer
  rts

testEnd:
  cmp #End
  bne endScript

  lda #1
  sta irq.fadeDone
  
  #if !AS_SPINDLE_PART
    lda #0
    sta scriptPointer
  #endif

endScript:
  rts

scriptData:
  .byte Wait,32
  .byte Fade
  .byte Wait,60
  .byte End

*=* "[CODE] Fader"
Fader:
{
	IsRunning: .byte 0
  Busy:      .byte 0
  PageNr:    .byte 3

	Run:
	{
    lda #0
    sta fadePointer   // set low byte of pointer

    inc Busy          // we are still processing this frame..

		lda #3            // fade $400 of colors
		sta PageNr

    // reset all the high bytes for the reads and writes

    lda #>anim            // restore read from anim every frame
    sta readAnim+1
		lda #>screen_original // reset where we read the colors from
		sta screenRead+1
		lda #>screen          // reset where the colors are going to
		sta screenWrite+1
		lda #>screen_original // reset where we read the $d800 colors
		sta d800Read+1       
		lda #>$d800           // reset where we write the $d800 colors
		sta d800Write+1

    ldx #0
		loop:	
			lda readAnim: anim,x  // read the animation for this char
      //beq loop2             // don't fade this char

      cmp rangeh:     #9      // compare with the current step +8 in the fade
      bcs loop2               // this char is done fading (or the other way around?)
			cmp rangel:     #1      // compare with the current step in the fade
      bcc loop2               // do not fade this char yet

      // this char is in range : fade it

        //sec                        // sec already done by cmp rangel
        sbc rangel                   // subtract the lower bound from the animation
        tay		                       // this gives the fade step (0-7)

        // clear animation to speed things up
        // future..

        lda fadetableaddress: tabp,y  // what table belongs to this fade step? read from the table
        sta fadePointer+1             // set high byte of pointer to fader table

        ldy screenRead:  screen_original,x  // read the target color from the screen
        lda             (fadePointer),y     // load the current color
        sta screenWrite: screencolour,x     // write the current color to the display screen
        ldy d800Read:    d800_original,x  // read the target color for $d800
        lda             (fadePointer),y     // load the current color
        sta d800Write:   $d800,x            // write to $d800
    
		loop2:	
      inx
		bne loop
		
    // increase all pages by one
		inc screenRead+1
		inc screenWrite+1
		inc d800Read+1
		inc d800Write+1
		inc readAnim+1

    dec PageNr     // all 4 pages done?
    bpl loop
		
    // move the range up by one
		inc rangel
		inc rangeh

		lda rangeh
		cmp checkend: #(round(maxAnimation+0.5)+8)            // is ook de laatste char in gefade?
		bne notCompletelyFinished	

		// the fade has ended completely

		// notify that the complete fade has ended
		lda #0
		sta IsRunning
		
  	// reset range in the animation that gets faded to the start
		lda #1
		sta rangel
		lda #9
		sta rangeh

  notCompletelyFinished:    
    // notify that this frame is finished
    dec Busy	
		rts
  }

  // in screenram there are 2 colors per byte
  // so there are 256 combinations. Each combination has 8 fade steps.
  // f.e. color $11 -> 00,bb,88,cc,55,33,77,11. there are at offsets $11, $111, $211, $311, etc in the table

	GenerateFadeTableColor:
	{ 
    // loop over all color combinations

		ldy #0                   // combination $00
    loop:
      lda msb: #0            // this is the MSB color
      and #$0f
      tax
      lda readMSB: tab,x     // this is the current color of the MSB color for this fade step
      asl
      asl
      asl
      asl
      sta current_msb

			ldx #0                  // this is the LSB color
			loopx:	
				lda readLSB: tab,x    // read the current color for the LSB color
				ora current_msb:#0    // OR with the current color for the MSB color
				sta to: fadetable,y
				inx
				iny
				cpx #$10
			bne loopx

			inc msb	          // next MSB color

			cpy #0
			bne loop          // loop over all color combinations

      // next stage
			lda readLSB
			clc
			adc #16
			sta readLSB
			sta readMSB
			lda readLSB+1
			adc #$00
			sta readLSB+1
			sta readMSB+1

			inc to+1
			lda to+1
			cmp #>(fadetable+$800)     // einde van de tabel?
		bne loop
		rts
	}

	.var fh = >fadetable
	tabp2:    .byte fh+7, fh+6, fh+5, fh+4, fh+3, fh+2, fh+1, fh+0, fh+0, fh+0               // loop through the stages from bright (fadetable+$700) to dark
	tabp:	 .byte fh+0, fh+1, fh+2, fh+3, fh+4, fh+5, fh+6, fh+7, fh+7, fh+7, fh+7         // loop through the stages from dark (offset $000) to bright (offset $700)

  .align $100
	*=* "[DATA] Color table"
	tab: 
		.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0  // read this table column wise :
		.byte $0,$b,$0,$9,$0,$9,$0,$b,$0,$0,$9,$0,$9,$b,$9,$9  // f.e. the second column 1 : you get color $1 via $0,$b,$8,$c,...$7,$1
		.byte $0,$8,$0,$b,$9,$b,$0,$8,$9,$0,$b,$0,$9,$8,$9,$b
		.byte $0,$c,$9,$8,$9,$b,$0,$c,$9,$0,$b,$9,$b,$c,$b,$8
		.byte $0,$5,$9,$8,$b,$8,$0,$c,$b,$0,$8,$9,$b,$c,$b,$8
		.byte $0,$3,$9,$c,$b,$c,$0,$5,$b,$0,$c,$9,$8,$5,$8,$c
		.byte $0,$7,$9,$5,$b,$c,$0,$3,$b,$0,$c,$9,$8,$3,$8,$5
		.byte $0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f		

  .align $100
	*=* "[DATA] Fade table"
  fadetable:
	.fill $0800,0
}

