//#define BIGBAR

.const debug=0
.var music = LoadSid("../music/thriller.sid")
.var backColor = $c

// Setup
#if !AS_SPINDLE_PART
  .file [name="main.prg", segments="Base,Patch", allowOverlap]
#else
  .segmentout [segments="Base,Patch", allowOverlap] 
#endif

.segmentdef Base  []  
.segmentdef Patch []

// Base code
.segment Base

#import "./memory/bitmap_koppel.asm"
#import "./memory/screen_koppel.asm"
#import "./memory/sprite_koppel.asm"
#import "./memory/pointer_koppel.asm"

.var afli   = LoadPicture("./ufli picture/afli.png") 
.var bits   = LoadPicture("./ufli picture/prio.png",    List().add($000000, $ffffff))
.var sprite = LoadPicture("./ufli picture/sprites.png", List().add($000000, $ffffff))

.var bitmap  = List()
.var screen  = List()
.var sprites = List()

// generate hashtable to decode the bitmap colors
.var ht = Hashtable()

.const RGBValues = List().add($000000,$ffffff,$68372b,$70a4b2,$6f3d86,$588d43,$352879,$b8c76f,
                              $6f4f25,$433900,$9a6759,$444444,$6c6c6c,$9ad284,$6c5eb5,$959595)
.for (var i=0; i<16; i++)
{
  .eval ht.put(RGBValues.get(i),i)
}
.print(RGBValues)

// ----------------------
// read the sprite data -
// ----------------------

.for (var Y=0; Y<200; Y++) {
  .for (var X=0; X<3*6; X++) {
    .var byte1 = sprite.getSinglecolorByte(2 + 2*X,   Y)   // X is as byte number, not pixel number
    .var byte2 = sprite.getSinglecolorByte(2 + 2*X+1, Y)   // X is as byte number, not pixel number
    // compress 2 bytes into 1 byte
    .var value=((byte1&$80)<<0) + ((byte1&$20)<<1) + ((byte1&$08)<<2) + ((byte1&$02)<<3) + ((byte2&$80)>>4) + ((byte2&$20)>>3) + ((byte2&$08)>>2) + ((byte2&$02)>>1)
    .eval sprites.add(value)
  }
}

// -------------------------------------------------
// read AFLI.png and convert into a real AFLI bitmap
// -------------------------------------------------

// loop over complete raw sprite data
.for (var Y=0; Y<128; Y++) {   // 200  17
  .for (var X=0; X<40; X++) {  // 40  15
    
    // count the # of colors in this byte
    .var ColorsInByte = List()
    .var Colors       = List()

    .for (var pixel=0; pixel<8; pixel++)
    {
      // read C64 color
      .var Color = ht.get(afli.getPixel(X*8+pixel, Y))

      // add color to list
      .eval Colors.add(Color)

      // add color to the list of unique colors if this is the first pixel
      .if (pixel==0) .eval ColorsInByte.add(Color)

      // is this color already used in this byte or is this a new color?
      .var InList = 0
      .for (var i=0; i<ColorsInByte.size(); i++)
      {
        .if (ColorsInByte.get(i) == Color) .eval InList = 1
      }
      // no? add it to the list of unique colors
      .if (InList==0) .eval ColorsInByte.add(Color)
    }

    .if (ColorsInByte.size()>2)
    {

      // cannot decode..
      .eval screen.add(0)
      .eval bitmap.add(0)
    }
    else
    // there are 1 or 2 colors in this byte
    {
      // read how to set the bits
      .var prioByte = bits.getSinglecolorByte(X, Y)   // X is as byte number, not pixel number

      // if there is only one color, the same color could be used in the fore- and background, so we have to add it as a second color
      .if (ColorsInByte.size() == 0) .eval ColorsInByte.add(0)
      .if (ColorsInByte.size() == 1) .eval ColorsInByte.add(ColorsInByte.get(0))

      // read the two colors seperately
      .var ColorOf0 = ColorsInByte.get(0) // choose the first color to correspond to 0
      .var ColorOf1 = ColorsInByte.get(1)

      // now we decode to AFLI
      // there are 2 cases : if there are 2 distinct colors, then we decode by looking at the colors
      // we already got the priority of the first pixel right, so we only have to look at the color to decide

      .var Byte = 0
      // case 1 : the 2 colors are the same -> we have to look at the prioByte to see if we should write a 0 or 1
      .if (ColorOf0 == ColorOf1)
      {
        // decode into a byte
        .var Bit  = $80
        .for (var pixel=0; pixel<8; pixel++)
        {
          // shift left
          .eval Byte = Byte * 2

          // get current bit
          .var prio = (prioByte & Bit)

          // write a 0 or 1
          .if (prio != 0)  // bit set -> AFLI should be in front
          {
            .eval Byte = Byte | 1  // set a 1
          }

          // shift the bit
          .eval Bit = Bit / 2
        }
      }

      // case 2 : there are 2 distinct colors. We first get the priority of the first color right, and then we decode by looking at the colors
      .if (ColorOf0 != ColorOf1)
      {
        // check the first bit : should this color be 0 or 1?
        .var firstBit = prioByte & $80

        .if (firstBit == 0)  
        { 
          // the first color (Colors(0)) should correspond to a 0, does it?
          .if (ColorOf0 != Colors.get(0))
          {
            // if it does not, swap the colors
            .eval ColorOf0 = ColorsInByte.get(1)
            .eval ColorOf1 = ColorsInByte.get(0)          
          }
        }

        // check the other case
        .if (firstBit != 0)  
        { 
          // the first color (Colors(0)) should correspond to a 1, does it?
          .if (ColorOf1 != Colors.get(0))
          {
            // if it does not, swap the colors
            .eval ColorOf0 = ColorsInByte.get(1)
            .eval ColorOf1 = ColorsInByte.get(0)          
          }
        }
        
        // decode into a byte
        .for (var pixel=0; pixel<8; pixel++)
        {
          // shift left
          .eval Byte = Byte * 2

          .if (Colors.get(pixel) == ColorOf1)
          {
            .eval Byte = Byte | 1  // set a 1
          }
        }
      } // ColorOf0 != ColorOf1

      // save decode converted AFLI
      .eval bitmap.add(Byte)
      // bit 0 = screenmem high nybble
      // bit 1 = screenmem low nybble

      .eval screen.add(ColorOf0 + (ColorOf1 << 4))
    } // else
  } // for X
} // for Y

// loop over complete raw AFLI data
.for (var Y=128; Y<200; Y++) {   // 200  17
  .for (var X=0; X<40; X++) {  // 40  15
  .eval screen.add(0)
  .eval bitmap.add(0)
  }
}

// --------------------------------------
// set screen data in normal AFLI order -
// --------------------------------------

.var bitmapData = List()
.var screenData = List()

.for (var screenNr = 0; screenNr < 8; screenNr++)
{
  .for (var i=0; i<1000; i++)
  {
    .var rowNr = floor(i/40)
  
    .var Y = rowNr * 8 + screenNr
    .var X = mod(i,40)

    .eval screenData.add(screen.get(X + (Y*40)))
  }

  // filler
  .for (var i=0; i<24; i++) .eval screenData.add(0)
}

// --------------------------------------
// set bitmap data in normal AFLI order -
// --------------------------------------

.for (var i=0; i<8000; i++)
{
  .var rowNr = floor(i/320)
  .var YinRow = i & 7
  
  .var Y = rowNr * 8 + YinRow
  .var X = floor(mod (i,320) / 8)

  .eval bitmapData.add(bitmap.get(X + (Y*40)))
}

// ---------------------------------------------------
// put AFLI and screen data in the order for display -
// ---------------------------------------------------

// first fill bank 1

.var bank1 = List()
.for (var byte=0; byte<$4000; byte++)  // pass all bytes in the bank
{
  .var address = byte+$4000
  // does a bitmap byte go here?
  .if (ht_bitmapdata.containsKey(address))
  {
    .eval bank1.add(bitmapData.get(ht_bitmapdata.get(address)))
  } else {
    // does a screen (color) byte go here?
    .if (ht_screen.containsKey(address))
    {
      .eval bank1.add(screenData.get(ht_screen.get(address)))

    } else {  
      // does a sprite byte go here?
      .if (ht_sprites.containsKey(address))
      {
        .eval bank1.add(sprites.get(ht_sprites.get(address)))
      } else {
        // does a spritepointer go here?
        .if (ht_pointerdata.containsKey(address))
        {
          .eval bank1.add(ht_pointerdata.get(address))
        } else {
          // no data goes here, add a 0
          .eval bank1.add($00)
        }
      }
    }
  }
}

// fill bank 2

.var bank2 = List()
.for (var byte=0; byte<$4000; byte++)  // pass all bytes in the bank
{
  .var address = byte+$8000
  // does a bitmap byte go here?
  .if (ht_bitmapdata.containsKey(address))
  {
    .eval bank2.add(bitmapData.get(ht_bitmapdata.get(address)))
  } else {
    // does a screen (color) byte go here?
    .if (ht_screen.containsKey(address))
    {
      .eval bank2.add(screenData.get(ht_screen.get(address)))

    } else {  
      // does a sprite byte go here?
      .if (ht_sprites.containsKey(address))
      {
        .eval bank2.add(sprites.get(ht_sprites.get(address)))
      } else {
        // does a spritepointer go here?
        .if (ht_pointerdata.containsKey(address))
        {
          .eval bank2.add(ht_pointerdata.get(address))
        } else {
          // no data goes here, add a 0
          .eval bank2.add($00)
        }
      }
    }
  }
}





*=$4000 "[bank1]"
.fill bank1.size(), bank1.get(i)
*=$8000 "[bank2]"
.fill bank2.size(), bank2.get(i)










// Patch Code 
.segment Patch

#if AS_SPINDLE_PART
  .var PLAY=music.play
  .label spindleLoadAddress = $2600
  *=spindleLoadAddress-18-0-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"               // fileformat magic
    .word 0                    // prepare routine
    .word start                // setup routine
    .word 0                    // irq handler
    .word 0                    // main routine
    .word 0                    // fadeout routine
    .word 0                    // cleanup routine
    .word 0                    // location of playroutine call

    .byte 0
    .word spindleLoadAddress    // Load address

  .label spindleHeaderEnd = *
  .var efoHeaderSize = spindleHeaderEnd-spindleHeaderStart

#else    
    BasicUpstart2(start)
#endif

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

.var spriteY = $3

// load raw font data
.var fontRaw = LoadPicture("./font/chars.png",List().add($000000, $ffffff))
.var width   = fontRaw.width/8   // width in chars
.var height  = fontRaw.height/8  // height in chars

// read all chars from the raw font data
.var nrChars = 2                 // current number of chars
.var Chars = List()              // array with all current chars
.var CharMap = List()            // charmap

// fill with empty and filled char (char 0 = empty char, char 1 = filled char)
.for (var byte=0; byte<8; byte++) .eval Chars.add($0)
.for (var byte=0; byte<8; byte++) .eval Chars.add($ff)
//.for (var byte=0; byte<8; byte++) .eval Chars.add($00)

// loop over complete raw font data
.for (var y=0; y<height; y++) {      // loop over y
  .for (var x=0; x<width; x++) {     // loop over x
    
    .var Char = List()              // read character

    .for (var byte=0; byte<8; byte++) {  // read all bytes of character
      //.eval Char.add(fontRaw.getMulticolorByte(x, y*8+byte))
      .eval Char.add(fontRaw.getSinglecolorByte(x, y*8+byte))
    }

    // check if character is already known
    // -----------------------------------
    
    .var notFound = true    // assume this is a new character
    // loop over all known characters
    .for (var i=0; i<nrChars; i++) {
      .var isSame = true
      // test if it is character is the same
      .for (var byte=0; byte<8; byte++) {
        .if (Chars.get(i*8 + byte) != Char.get(byte)) {
          .eval isSame = false
        }
      }

      // is the character the same?
      .if (isSame == true) {
        .eval CharMap.add(i)             // write char into map
        .eval i = nrChars                // break loop
        .eval notFound = false           // char found
      }
    } // for i

    // character not found? insert it
    .if (notFound == true) {
      // copy char into charset
      .for (var byte=0; byte<8; byte++) {
        .eval Chars.add(Char.get(byte))       // copy byte
      }
      .eval CharMap.add(nrChars)         // write char into map
      .eval nrChars = nrChars+1          // one char added
    }
  } // for y
} // for x

.print "nr of chars : "
.print nrChars

// load raw sprite font data
.var spriteRaw = LoadPicture("./font/sprites.png",List().add($000000, $ffffff))
.var spriteWidth   = spriteRaw.width/8   // width in chars
.var spriteHeight  = spriteRaw.height    // height in pixels
.var spriteColumns = List()

// we want the data in columns for easy access
.for (var x=0; x<spriteWidth; x++)
{
  .for (var y=0; y<spriteHeight; y++)
  {
    .var byte = spriteRaw.getSinglecolorByte(x, y)
    .eval spriteColumns.add(byte)
  }
}

*=$9000 "[CODE] main code"
start:
  sei

  lda #$35
  sta $01

  lda #$08
  sta $d011

  lda #$00
  sta $dd00 // kill spindle and stop drive spinning

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

  #endif  

  lda #0
  jsr music.init

  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d
  lda #$08
  sta $d011
  lda #$c8 
  sta $d016
  lda #$0f
  sta $d020
  sta $d021

  lda #<waitIrq
  sta $fffe
  lda #>waitIrq
  sta $ffff
  lda #$fa
  sta $d012

  lda #0 //%11111100
  sta $d015

  lda #$2d
  ldx #$0f
!:
  sta $d000,x
  dex
  dex
  bpl !-

  ldx #0
  lda #$0f
{
loop:
  sta $d800,x
  sta $d900,x
  sta $da00,x
  sta $db00,x
  inx
  bne loop
}
  ldx #5
loopa:
  lda #$0a
  sta $da82,x
  sta $da82+40,x
  lda #$08
  sta $da82+6,x
  sta $da82+46,x
  lda #$09
  sta $da82+12,x
  sta $da82+52,x
  lda #$06
  sta $da82+18,x
  sta $da82+58,x
  lda #$04
  sta $da82+24,x
  sta $da82+64,x
  lda #$0e
  sta $da82+30,x
  sta $da82+70,x
  dex
  bpl loopa

  ldx #39
{
  lda #$0
loop:
  sta $d800+22*40,x
  sta $d800+23*40,x
  sta $d800+24*40,x
  dex
  bpl loop
}

  jsr setSpriteCol

  lda #0
  sta $d017  // y stretch
  sta $d01c  // multicolor
  lda #$ff
  sta $d01d  // x stretch
  sta $d01b  // priority

  inc waitIrq.ready

  lda $dc0d
  lda $dd0d
  asl $d019

  cli
mainloop:    // even in spindle, we have to stay in the main loop : this is the last part
  nop
  bit $ea

  jmp mainloop

waitIrq:
{
  sta restorea
  lda ready: #0
  beq notReady

  lda #<controlIrq
  sta $fffe
  lda #>controlIrq
  sta $ffff
  lda #$fa
  sta $d012

  lda restorea: #0
  rti
notReady:
  lda #<waitIrq
  sta $fffe
  lda #>waitIrq
  sta $ffff
  lda #$fa
  sta $d012

  lda restorea
  rti
}

setSpriteX:
{
  lda #$28
  sta $d004
  lda #$58
  sta $d006
  lda #$88
  sta $d008
  lda #$b8
  sta $d00a
  lda #$e8
  sta $d00c
  lda #$18
  sta $d00e
  lda #$80
  sta $d010
  rts
}

setSpriteCol:
{
  lda spriteColors
  sta $d029
  lda spriteColors+1
  sta $d02a
  lda spriteColors+2
  sta $d02b
  lda spriteColors+3
  sta $d02c
  lda spriteColors+4
  sta $d02d
  lda spriteColors+5
  sta $d02e
  rts
}

setSpriteY:
{
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d
  sta $d00f
  rts
}

setSpritePointer:
{
  //lda #(filledSprite&$3fff)/64
  sta fillScreen+$3fa
  sta fillScreen+$3fb
  sta fillScreen+$3fc
  sta fillScreen+$3fd
  sta fillScreen+$3fe
  sta fillScreen+$3ff
  rts
}

spriteOnIrq:
{
  sta restorea

  lda #$fc
  sta $d015

  lda #(filledSprite&$3fff)/64
  jsr setSpritePointer

  lda $d018
  and #$0f
  ora #((fillScreen&$3fff)/$400)*$10
  sta $d018

  lda #$10
  sta $d012
  lda #$18
  sta $d011

  lda #<multiplexIrq
  sta $fffe
  lda #>multiplexIrq
  sta $ffff
  asl $d019

  lda restorea: #0
  rti
}

multiplexIrq:
{
  sta restorea
  lda #(spriteY+$15)
  jsr setSpriteY
  lda #<multiplex2Irq
  sta $fffe
  lda #>multiplex2Irq
  sta $ffff
  lda #$20
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

multiplex2Irq:
{
  sta restorea
  lda #$2d
  jsr setSpriteY
  lda #<stabilizeIrq
  sta $fffe
  lda #>stabilizeIrq
  sta $ffff
  lda #$2b
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

stabilizeIrq:
{
  sta restorea

  lda #<fliIrq
  sta $fffe
  lda #>fliIrq
  sta $ffff
  inc $d012
  asl $d019
  cli
  .fill 20,NOP

  lda restorea: #0
  rti
}

fliIrq:
{
  lda #39-(8)     // 18..19   // 19..27 <- (earliest cycle)
  sec              // 20..21   // 21..29
  sbc $dc06        // 22..23   // 23..31, A becomes 0..8
  sta *+4          // 26..27   // 27..35
  bpl *+2             // 31..39
  lda #$a9            // 34
  lda #$a9            // 36
  lda #$a9            // 38
  lda $eaa5           // 40
                      // at cycle 34+(10) = 44

  nop
  nop
  nop

  inc $dbff
  inc $dbff
  nop

  // raster 45, cycle 3
  dec $d017

  stx restorex
  sty restorey

  jsr waste12
  jsr waste12

  nop
  nop
  bit $ea

  // here we are at raster 45, cycle 57
  inc $d017
  dec $d017

  lda #$ab    // ab for multiplex... 2d for single (2d wordt gezet in irq1 in een loop)
  sta $d001
  sta $d003
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d
  sta $d00f

  nop

  inc $d017
  dec $d017

  bit $ea

  jsr speedcode

  lda #0
  sta $d017
  inc $dbff

  lda #$3e   // bank at $4000  -> 8000
  sta $dd02
  lda #$de   // screen at 13*$400 = $7400 -> b400
  sta $d018
  
  inc $dbff
  nop
  nop

  // d018 = $3f -> screen $8c00, bitmap $a000
  // dd02 = $12 -> bank $8000
  //lda #0
  //sta $d017
  //inc $dbff

  lda #$18   // go charmode
  sta $d011

  lda #<rasterbarIrq
  sta $fffe
  lda #>rasterbarIrq
  sta $ffff
  lda #rasterd012
  sta $d012
  asl $d019

  ldx restorex: #0
  ldy restorey: #0
  rti
}

speedcode:
*=* "[IMPORT] FLI generated speed code"

.macro FLICODE(line) {
  // fli plaat en sprites zitten nu van 4000-7fff en 8000-bfff

  lda #$20     // a = 20, x = 2a (22), y = 3e                            in oneven rijen is d018 hier 2d en niet 0f
  nop          // ontdekking : carry maakt niet uit...
  .if (line==0)
  {
    nop        // extra cycle over bij de eerste rij.. kunnen we er wat mee?!
  }
  .if (line>0)
  {
    sax $d011  // 2d and 3a = 0010.1101  2d 
               //             0011.1010  3a
               //             0010.1000  28
  }
  //sta $d011    // d011 = 20  (0)
  sre $ddf2    // dd02 = 12 /2 = 09. a = 20 EOR 9 = 29              in oneven rijen is d018 hier 2d en niet 0f
  sta $d011    // d011 - 29  (1)
  sty $d3d8    // d018 = 3e                                         hier is d018 weer gelijk in even en oneven rijen
  stx $d011    // d011 = 22  (2)
  slo $ddf2    // dd02 = 09*2 = 12. a = 29 OR 12 = 3B
  sta $d011    // d011 = 3b  (3)
  sre $d3d8    // d018 = 3e/2 = 1f. a = 3B EOR 1F = 24
  sta $d011    // d011 = 24  (4)
  sre $ddf2    // dd02 = 12/2 = 09, a = 24 EOR 09 = 2D
  sta $d011    // d011 = 2d  (5)
  .if (mod(line,2)==0)
  {
    sta $d3d8  // d018 = 2a  was : sta $d018 (kan ook : sax $d018)
    sty $d011  // d011 = 3E (6)
    slo $ddf2  // dd02 = 09*2 = 12, a = 2D OR 12 = 3F
    sta $d011  // d011 = 3F (7)

    // ideaal : aan het einde van een even lijn wisselen van dd02
  }
  .if (mod(line,2)==1)
  {
    lsr $d3d8  // d018 = 0f
    sty $d011  // d011 = 3E (6)
    slo $ddf2  // dd02 = 09*2 = 12, a = 2D OR 12 = 3F
    sta $d011    // d011 = 3F (7)
  }

               // wat zijn de opties om naar bank 00 of 03 te gaan?
               // SLO : 09->12  A=A OR  12 = 2D or 12 = 3F
               // RLA : 09->12  A=A AND 12 = 2D and 12 = 00
               // SRE : 09->04  A=A EOR 04 = 2D or 04 = 29
               // RRA : 09->04  A=A ADC 04 = 2D + 04 = 31
               // ISC : 09->0A  A=A - 0A = 2D - 0a = 23

  // d018 = 2d, dd02 = 12  <- d018 is anders geworden.
}

.macro FLICODE_end(i) {
  lda #$20
  nop
  sta $d311
  sre $ddf2
  sta $d2d1
  sty $d3d8
  stx $d351
  slo $ddf2
  sta $d091
}

  // register setup

  lda #$0f
  sta $d218
  lda #$00
  sta $ddc0
  lda #$12
  sta $ddb2
  ldx #$2a // 22 of 2a of 32, 3a, a2, aa, b2, ba...
  ldy #$3e

  .for (var line=0; line<15; line++)
  {
    FLICODE(line)
  }
  // d018 = 2d, dd02 = 12

  FLICODE_end(15)
  // d018 = 3d, dd02 = 12
  // screen is at $8c00
  rts

fadeInIrq:
{
  sta restorea

  lda #$fc
  sta $d015

  lda #<fadeMultiplex1
  sta $fffe
  lda #>fadeMultiplex1
  sta $ffff
  lda #30
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

fadeMultiplex1:
{
  sta restorea

  lda $d00f
  clc
  adc #42
  jsr setSpriteY

  lda #<fadeMultiplex2
  sta $fffe
  lda #>fadeMultiplex2
  sta $ffff
  lda #30+42
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

fadeMultiplex2:
{
  sta restorea

  lda $d00f
  clc
  adc #42
  jsr setSpriteY
  lda #<fadeMultiplex3
  sta $fffe
  lda #>fadeMultiplex3
  sta $ffff
  lda #30+42+42
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

fadeMultiplex3:
{
  sta restorea

  lda $d00f
  clc
  adc #42
  jsr setSpriteY
  lda #<fadeMultiplex4
  sta $fffe
  lda #>fadeMultiplex4
  sta $ffff
  lda #30+42+42+42
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

fadeMultiplex4:
{
  sta restorea

  lda $d00f
  clc
  adc #42
  jsr setSpriteY

  lda #<fadeMultiplex5
  sta $fffe
  lda #>fadeMultiplex5
  sta $ffff
  lda #30+42+42+42+30
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

fadeMultiplex5:
{
  sta restorea

  lda #(partlyFilledSprite&$3fff)/64
  jsr setSpritePointer

  lda #<controlIrq
  sta $fffe
  lda #>controlIrq
  sta $ffff
  lda #$fa
  sta $d012
  asl $d019

  lda restorea: #0
  rti
}

.label rasterd012 = $b9+8+6

.align $100
rasterbarIrq:
{
  bit $ea
  nop
  sta restorea        // 15..23
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

  stx restorex
  sty restorey

  nop

 	ldx #$00
loop:
	lda rasterColors,x
	sta $d020
	sta $d021
  inx
  ldy delay+6,x
  bne continue

  lda rasterColors,x
  sta $d020
  sta $d021
  inx
  ldy delay+6,x
  bit $ea  // this is never a badline

continue:
  lda #$0f  // background
  sta $d027
  sta $d028
  sta $d029
  sta $d02a
  sta $d02b
  sta $d02c
  sta $d02d
  sta $d02e

  bit $ea

	cpx #18
	bcc loop

  lda scroller.scrollLow
  ora #$c0
  sta $d016

  lda #$3d
  sta $dd02
  lda #(((scrollScreen&$3fff)/$400)*$10)+(((charset&$3fff)/$800)*2)
  sta $d018

  lda #$0
  sta $d017
  lda #$ff
  sta $d01b

  // multiplex sprites for scroller

  lda #(22*8+$31)
  sta $d001
  sta $d003
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d
  sta $d00f
  lda #$ff
  sta $d015

  lda #0
  sta d010

  // 20, 1e, 1c, 1a
  // 18, 16, 14, 12
  // 10, 0e, 0c, 0a
  // 08, 06, 04, 02
  // 00, f6, f4, f2
  // f0, ee, ec, ea

  lda firstPos: #$18 // f0,00,08,10,18,20
  sec
  sbc scrollSpeedOld: #0
  bcs !+
  // position $00-1 = $f7! -> $00-2 = $f6
  // sub 8 extra
  sec
  sbc #8
!:
  // if the value is below e8, we have to add $30+$8 again
  bpl !+
  cmp #$e8
  bcs !+

  clc
  adc #$38
  tax

  lda firstScrollSprite
  clc
  adc #1
  and #7
  ora #((scrollSprites&$3fff)/64)
  sta firstScrollSprite

  txa
!:
  sta firstPos

  lda firstPos
  cmp #$80
  ror d010
  sta $d000
  clc
  adc #$30
  bcc !+
  clc
  adc #$08
!:
  ror d010
  sta $d002   // 20,28,30,38,40,48  -> d010 always clear

  clc
  adc #$30
  ror d010
  sta $d004   // 50,58,60,68,70,78  -> d010 always clear

  clc
  adc #$30
  ror d010
  sta $d006   // 80,88,90,98,a0,a8  -> d010 always clear

  clc
  adc #$30
  ror d010
  sta $d008   // b0,b8,c0,c8,d0,d8  -> d010 always clear

  clc
  adc #$30
  ror d010
  sta $d00a   // e0,e8,f0,f8,00,08  -> d010 clear or set

  clc
  adc #$30
  ror d010
  sta $d00c   // 10,18,20,28,30,38  -> d010 always set

  clc
  adc #$30    // 40,48,50,58,60,68  -> d010 always set
  ror d010
  sta $d00e

  lda d010: #0
  ora #$c0
  sta $d010

  lda #((scrollSprites&$3fff)/64)+7
  ldx firstScrollSprite: #(scrollSprites&$3fff)/64
  sax scrollScreen+$3f8
  inx
  sax scrollScreen+$3f9
  inx
  sax scrollScreen+$3fa
  inx
  sax scrollScreen+$3fb
  inx
  sax scrollScreen+$3fc
  inx
  sax scrollScreen+$3fd
  inx
  sax scrollScreen+$3fe
  inx
  sax scrollScreen+$3ff

  lda scrollSpeed: #0
  sta scrollSpeedOld

  lda #<scrollRasterIrq
  sta $fffe
  lda #>scrollRasterIrq
  sta $ffff
  lda #$e1   // startbar df
  sta $d012
  asl $d019

  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}

rasterColors:
  .byte $f,$f
  .byte $f,$f,$f
  .byte $f,$f,$f,$f,$f,$f,$f
  .byte $f,$f,$f
  .byte $f,$f
  .byte $f
  .byte $f

//  .byte $6,$0
//  .byte $6,$e,$0
//  .byte $6,$e,$f,$f,$e,$6,$0
//  .byte $e,$6,$0
//  .byte $6,$0
//  .byte $f
//  .byte $f

delay:
  .byte 7,7,7,7,7,7,7,0
  .byte 7,7,7,7,7,7,7,0
  .byte 7,7,7,7,7,7,7,0
  .byte 7,7,7,7,7,7,7,0

.align $100
startBar:
{
  sta restorea

  lda #<scrollRasterIrq
  sta $fffe
  lda #>scrollRasterIrq
  sta $ffff
  lda #$e1
  sta $d012
  asl $d019

  jsr waste12

  inc $dbff
  lda #$0f
  sta $d021

  lda restorea: #0
  rti
}

waste24:
  jsr waste12
waste12:
  rts

controlIrq:
{
  sta restorea
  stx restorex
  sty restorey
  
  lda #$10
  sta $d011

  lda #$00
  sta $d015
  sta $d01c
  sta $d01b
  lda #$ff
  sta $d017
  sta $d01d

  lda #$c8
  sta $d016

  lda goMusic: #0
  beq noMusic
  jsr music.play
noMusic:

  jsr script
  jsr fadeIn1
  jsr fadeIn2
  jsr fadeIn3
  jsr fadeIn4
  jsr fadeIn5
  jsr fadeIn6
  jsr setBar

  lda #$18
  sta $d011

  lda #0     // hide bugs
  sta $ffff

  lda speedcode+1
  sta $d018
  lda speedcode+6
  sta $dd00
  lda speedcode+11
  sta $dd02

  lda goLogo: #0
  beq goBars

  // show fli logo

  lda #0
  sta $d017
  lda #$ff
  sta $d01b

  lda #$01
  sta $d012
  lda #<spriteOnIrq
  sta $fffe
  lda #>spriteOnIrq
  sta $ffff

  // reset Y sprite positions

  lda #spriteY
  jsr setSpriteY
  jsr setSpriteX
  jsr setSpriteCol

  asl $d019

  lda restorea
  ldx restorex
  ldy restorey
  rti

goBars:
  // show bars
  lda #$fc
  sta $d017

  lda #$1
  sta $d012
  lda #<fadeInIrq
  sta $fffe
  lda #>fadeInIrq
  sta $ffff
  asl $d019

  lda #$03
  jsr setSpriteY
  jsr setSpriteX
  jsr setSpriteCol

  lda #(((fillScreen&$3c00)/$0400)*$10)+(((charset&$3fff)/$800)*2)
  sta $d018

  lda #(filledSprite&$3fff)/64
  jsr setSpritePointer

  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}

.var Wait       = $80
.var switchBars = $81
.var End        = $82
.var Fade1      = $83
.var Fade2      = $84
.var Fade3      = $85
.var Fade4      = $86
.var Fade5      = $87
.var Fade6      = $88
.var PlayMusic  = $89
.var fadeBar    = $8a
.var scrollOn   = $8b

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
  bne testShowLogo
  {
    lda scriptData+1,x
    sta wait
    inc scriptPointer
    inc scriptPointer
    rts
  }
testShowLogo:
  cmp #switchBars
  bne testFade1
  {
    lda controlIrq.goLogo
    eor #1
    sta controlIrq.goLogo
    inc scriptPointer
    rts
  }
testFade1:
  cmp #Fade1
  bne testFade2
  {
    lda #1
    sta fadeIn1.fade
    inc scriptPointer
    rts
  }
testFade2:
  cmp #Fade2
  bne testFade3
  {
    lda #1
    sta fadeIn2.fade
    inc scriptPointer
    rts
  }
testFade3:
  cmp #Fade3
  bne testFade4
  {
    lda #1
    sta fadeIn3.fade
    inc scriptPointer
    rts
  }
testFade4:
  cmp #Fade4
  bne testFade5
  {
    lda #1
    sta fadeIn4.fade
    inc scriptPointer
    rts
  }
testFade5:
  cmp #Fade5
  bne testFade6
  {
    lda #1
    sta fadeIn5.fade
    inc scriptPointer
    rts
  }
testFade6:
  cmp #Fade6
  bne testMusic
  {
    lda #1
    sta fadeIn6.fade
    inc scriptPointer
    rts
  }
testMusic:
  cmp #PlayMusic
  bne testFadeBar
  {
  lda #1
  sta controlIrq.goMusic
  inc scriptPointer
  rts
  }
testFadeBar:
{
  cmp #fadeBar
  bne testScroll
  {
    lda #1
    sta setBar.fadeInBar
    inc scriptPointer
    rts
  }
}
testScroll:
{
  cmp #scrollOn
  bne testEnd
  {
    lda #2
    sta scroller.scrollSpeed
    sta rasterbarIrq.scrollSpeed

    inc scriptPointer
    rts
  }
}
testEnd:
  rts
}

scriptData:
  .byte Wait,100,PlayMusic,Fade1
  .byte Wait,102,Fade4
  .byte Wait,102,Fade6
  .byte Wait,102,Fade2
  .byte Wait,102,Fade5
  .byte Wait,102,Fade3
  .byte Wait,250-102-102
  .byte Wait,30
  .byte switchBars,Wait, 8-2,switchBars,Wait,5-2
  .byte switchBars,Wait, 5-2,switchBars,Wait,2-2
  .byte switchBars,Wait,11-2,switchBars,Wait,2-2
  .byte switchBars,Wait, 4-2,switchBars,Wait,9-2
  .byte switchBars
  .byte Wait,50,fadeBar,scrollOn
  .byte End

spriteColors:
  .fill 6,$f

setBar:
{
  lda fadeInBar: #0
  bne continue
  rts
continue:
  lda wait: #0
  beq advance
  dec wait
  rts
advance:
  ldx phase: #0
  lda coltab1,x  //6
  sta rasterColors+0
  sta rasterColors+2
  sta rasterColors+5
  sta rasterColors+10
  sta rasterColors+13
  sta rasterColors+15
  lda coltab2,x  //e
  sta rasterColors+3
  sta rasterColors+6
  sta rasterColors+9
  sta rasterColors+12
  lda coltab3,x //f
  sta rasterColors+7
  sta rasterColors+8
  lda coltab4,x // 0
  sta rasterColors+1
  sta rasterColors+4
  sta rasterColors+11
  sta rasterColors+14
  sta rasterColors+16

  cpx #10
  beq endFadeBar
  inc phase
  lda #4
  sta wait
  rts
endFadeBar:
  lda #0
  sta fadeInBar
  rts
}

coltab1:
  .byte $f,$f,$f,$7,$1,$7,$3,$a,$4,$b,$6  //6
coltab2:
  .byte $f,$f,$7,$1,$1,$1,$7,$3,$a,$4,$e  //e
coltab3:
  .byte $f,$7,$1,$1,$1,$1,$1,$7,$f,$f,$f  //f
coltab4:
  .byte $f,$f,$f,$f,$7,$3,$a,$4,$b,$6,$0  //0

fadeIn1:
{
  lda fade: #0
  bne doFade
  rts
doFade:
  lda wait: #0
  beq advance
  dec wait
  rts

advance:
  ldx phase: #0
  lda #0
  ldy fadeColors,x
  beq !+
  lda #$0a
  !:
  sta spriteColors
  
  cpx #(fadeColorsEnd-fadeColors-1)//-6)
  bne *+3
  rts
  inc phase
  lda #1
  sta wait
  rts
}

fadeIn2:
{
  lda fade: #0
  bne doFade
  rts
doFade:
  lda wait: #0
  beq advance
  dec wait
  rts

advance:
  ldx phase: #0
  lda #0
  ldy fadeColors,x
  beq !+
  lda #$08
  !:
  sta spriteColors+1
  
  cpx #(fadeColorsEnd-fadeColors-1)//-6)
  bne *+3
  rts
  inc phase
  lda #1
  sta wait
  rts
}

fadeIn3:
{
  lda fade: #0
  bne doFade
  rts
doFade:
  lda wait: #0
  beq advance
  dec wait
  rts

advance:
  ldx phase: #0
  lda #0
  ldy fadeColors,x
  beq !+
  lda #$09
  !:
  sta spriteColors+2
  
  cpx #(fadeColorsEnd-fadeColors-1)//-6)
  bne *+3
  rts
  inc phase
  lda #1
  sta wait
  rts
}

fadeIn4:
{
  lda fade: #0
  bne doFade
  rts
doFade:
  lda wait: #0
  beq advance
  dec wait
  rts

advance:
  ldx phase: #0
  lda #0
  ldy fadeColors,x
  beq !+
  lda #$06
  !:
  sta spriteColors+3
  
  cpx #(fadeColorsEnd-fadeColors-1)//-6)
  bne *+3
  rts
  inc phase
  lda #1
  sta wait
  rts
}

fadeIn5:
{
  lda fade: #0
  bne doFade
  rts
doFade:
  lda wait: #0
  beq advance
  dec wait
  rts

advance:
  ldx phase: #0
  lda #0
  ldy fadeColors,x
  beq !+
  lda #$04
  !:
  sta spriteColors+4
  
  cpx #(fadeColorsEnd-fadeColors-1)//-6)
  bne *+3
  rts
  inc phase
  lda #1
  sta wait
  rts
}

fadeIn6:
{
  lda fade: #0
  bne doFade
  rts
doFade:
  lda wait: #0
  beq advance
  dec wait
  rts

advance:
  ldx phase: #0
  lda #0
  ldy fadeColors,x
  beq !+
  lda #$0e
  !:
  sta spriteColors+5
  
  cpx #(fadeColorsEnd-fadeColors-1)//-6)
  bne *+3
  rts
  inc phase
  lda #1
  sta wait
  rts
}

scroller:
{
  lda scrollLow: #0
  sec
  sbc scrollSpeed: #0
  and #$07
  sta scrollLow
  bcs endScroll

  jmp scrollLeft

endScroll:
  rts
}

.var scrollerPosition = scrollScreen+(22*40)

scrollLeft:
{
  ldx #0
loop:
  lda scrollerPosition+0*40+1,x
  sta scrollerPosition+0*40,x
  lda scrollerPosition+1*40+1,x
  sta scrollerPosition+1*40,x
  lda scrollerPosition+2*40+1,x
  sta scrollerPosition+2*40,x
  inx
  cpx #39
  bne loop
  //rts
}

putChar:
{
  lda charsRemaining: #0
  beq nextChar // read next char

  dec charsRemaining

  ldx charFrom: #0
  lda charMap,x
  sta scrollerPosition+39
  lda charMap+width,x
  sta scrollerPosition+39+40
  lda charMap+(2*width),x
  sta scrollerPosition+39+80

  jsr readSpriteColumn

  inc charFrom 
  rts

nextChar:
  inc scrollFrom
  bne !+
  inc scrollFrom+1
!:
  // read char
  ldx scrollFrom: scrolltext-1
  bpl normalChar

  cpx #$ff
  beq resetScroller
  cpx #$fc
  beq turnOnLightGrey
  cpx #$fd
  beq turnOnRaster
  cpx #$fe
  beq turnOnSine

  // this is a special code..
  txa
  and #$0f
  sta scroller.scrollSpeed
  sta rasterbarIrq.scrollSpeed
  bpl nextChar

resetScroller:
  // reset scroller
  lda #<(scrolltext-1)
  sta scrollFrom
  lda #>(scrolltext-1)
  sta scrollFrom+1
  bne nextChar  // read the next real char

turnOnRaster:
  lda #1
  sta scrollRasterIrq.animRaster
  lda #0
  sta scrollRasterIrq.rasterAnimFirst
  beq nextChar

turnOnLightGrey:
  lda #0
  sta scrollRasterIrq.animRaster
  lda #(rasterLightGrey-rasterAnim)
  sta scrollRasterIrq.rasterAnimFirst
  bne nextChar

turnOnSine:
  lda #0
  sta scrollRasterIrq.animRaster
  lda #(rasterSine-rasterAnim)
  sta scrollRasterIrq.rasterAnimFirst
  bne nextChar

normalChar:
  txa
  and #$3f
  tax

  // read width
  lda xpositions+1,x
  sec
  sbc xpositions,x
  sta charsRemaining

  // read position
  lda xpositions,x
  sta charFrom
  jmp putChar
}

readSpriteColumn:
{
  ldy #0

  lda putChar.charFrom
  lsr                    // the sprite columns are only 4 pixels wide
  bcc !+
  iny                    // even->y=0, odd->y=1
!:
  tax
  lda columnHi,x
  sta from+1
  lda columnLo,x
  sta from

  ldx #19
loop:
  lda from: spriteData,x
  // if charFrom is even, we have to shift 4 bits to the right
  cpy #0
  bne skipShift
  lsr
  lsr
  lsr
  lsr
skipShift:
  // always and #$0f at the end
  and #$0f

  sta buffer,x
  dex
  bpl loop
  //rts
}
  
writeSpriteColumn:
{
  lda writeColumn: #40
  cmp #(8*6)
  bcc !+
  lda #0
!:
  sta writeColumn
  lsr
  tax
  bcs writeRight

writeLeft:
  lda spriteWriteLo,x
  sta spriteToLeft
  lda spriteWriteHi,x
  sta spriteToLeft+1

  ldx #0
  ldy #0
loopLeft:
  lda buffer,y
  asl
  asl
  asl
  asl
  sta spriteToLeft: scrollSprites,x
  inx
  inx
  inx
  iny
  cpy #20
  bne loopLeft
  inc writeColumn
  rts


writeRight:
  lda spriteWriteLo,x
  sta spriteToRight1
  sta spriteToRight2
  lda spriteWriteHi,x
  sta spriteToRight1+1
  sta spriteToRight2+1

  ldx #0
  ldy #0
loopRight:
  lda buffer,y
  ora spriteToRight1: scrollSprites,x
  sta spriteToRight2: scrollSprites,x
  inx
  inx
  inx
  iny
  cpy #20
  bne loopRight
  inc writeColumn
  rts
}

buffer:
  .fill 21,0

spriteWriteLo:
  .for (var x=0; x<3*8; x++)
  {
    .var sprite = floor(x/3)
    .var inSprite = mod(x,3)

    .byte <(scrollSprites+(sprite*64)+inSprite)
  }
spriteWriteHi:
  .for (var x=0; x<3*8; x++)
  {
    .var sprite = floor(x/3)
    .var inSprite = mod(x,3)

    .byte >(scrollSprites+(sprite*64)+inSprite)
  }

columnLo:
  .fill ceil(width/2), <(spriteData+((floor(i))*21))
columnHi:
  .fill ceil(width/2), >(spriteData+((floor(i))*21))


fadeColors:
.byte 0,0,0,0,0,0,0
.byte 1,0,1,0,1,0,1,1
fadeColorsEnd:

// fill top of logo

* = $8280 "[GFX] sprite fill"
  .fill 8,255
* = $82c0 "[GFX] sprite fill"
  .fill 8,255
* = $8300 "[GFX] sprite fill"
  .fill 8,255
* = $8340 "[GFX] sprite fill"
  .fill 8,255
* = $8380 "[GFX] sprite fill"
  .fill 8,255
* = $8000+$1a*$40 "[GFX] sprite fill"
  .fill 8,255

// fixcolors
* = $8c00+15*40 "[GFX] fix colors bottom"
//  .fill 1*40,$ff
  .fill 2,$55
  .fill 6,$aa
  .fill 6,$88
  .fill 6,$99
  .fill 6,$66
  .fill 6,$44
  .fill 6,$ee
  .fill 2,$ff

* = $7800 "[GFX] charset"
charset:
  .fill Chars.size(), Chars.get(i)

* = $7400 "[GFX] scroll sprites and screen"
scrollScreen:
scrollSprites:
  .fill 8*64,$0
* = scrollScreen+$280 "[GFX] fill below ufli"
  .fill 80,1

// chars below bitmap start at $280


* = $b400 "[GFX] fillscreen"
fillScreen:

* = fillScreen+$100  "[GFX] fillscreen"
filledSprite:
  .fill 63,255
  .byte 0
partlyFilledSprite:
  .fill 33,255
  .fill 30,0

* = fillScreen+(18*40)  "[GFX] fillscreen"
  .fill (7*40),$ff
  .fill 16,$ff

* = $2600 "[CODE] rasterirq"

scrollRasterIrq:
{
  sta restorea     // 10..11
  lda #39-(6)      // 14..15 <- (earliest cycle)
  sec              // 16..17
  sbc $dc06        // 18..20, A becomes 0..8
  sta *+4          // 22..24
  bpl *+2          // 26..28
  lda #$a9         // 34
  lda #$a9         // 36
  lda #$a9         // 38
  lda $eaa5        // 40
                   // at cycle 34+(10) = 44

  stx restorex
  sty restorey
  
  nop
  ldx rasterAnimFirst: #6*21
  lda rasterAnim,x
  sta $d021

  txa
  clc
  adc #5
  sta x1
  adc #8
  sta x2
  adc #4
  sta x3
  adc #2
  sta x4

  lda rasterAnim+1,x
  nop
  nop
preloop:
  nop
  ldy #$0c
  sta $d021
  nop
  nop
  jsr waste24
  inx
  lda rasterAnim+1,x

  #if BIGBAR
    ldy #$0f
  #else 
    ldy #backColor
  #endif

  sty $d027
  sta $d021

  sty $d028
  sty $d029
  sty $d02a
  sty $d02b
  sty $d02c
  sty $d02d
  sty $d02e
  
  inx
  nop
  nop
  nop
loop:
  lda rasterAnim+1,x   //0 tm 4
  sta $d021
  lda rasterAnim+2,x   //5
  ldy rasterAnim+3,x   //6
  jsr waste12
  inc $dbff
  bit $ea

  inx
  cpx x1: #5
  bne loop

  bit $ea

  sta $d021              //5
  inx
  sty $d021              //6
  inx
  nop
  jsr waste24
  inc $dbff
  bit $ea

loop2:
  lda rasterAnim+1,x   //7 tm 12'
  sta $d021
  lda rasterAnim+2,x   //13
  ldy rasterAnim+3,x   //14
  jsr waste12
  inc $dbff
  bit $ea

  inx
  cpx x2: #13
  bne loop2

  sta $d021              //13
  inx

  sty $d021              //14

  nop
  jsr waste24
  inx
  nop
  nop
  nop
  #if BIGBAR
    ldy #backColor
  #else
    ldy #$0f  // background
  #endif

loop3:
  lda rasterAnim+1,x   //15 tm 20
  sta $d021
  lda rasterAnim+2,x   //13
  nop
  nop
  jsr waste12
  inc $dbff
  bit $ea

  inx
  cpx x3: #5+8+7
  bne loop3

  sty $d027
  sta $d021
  sty $d028
  sty $d029
  sty $d02a
  sty $d02b
  sty $d02c
  sty $d02d
  sty $d02e
  nop
  nop
  inx
loop4:
  lda rasterAnim+1,x   //15 tm 20
  sta $d021
  lda rasterAnim+2,x   //13
  ldy rasterAnim+3,x   //14
  jsr waste12
  inc $dbff
  bit $ea

  inx
  cpx x4: #5+8+7
  bne loop4

  bit $ea
  lda $d028
  sta $d021

  jsr waste24
  jsr waste24
  jsr waste24
  jsr waste24
  inc $dbff

  lda #$0f
  sta $d021

  // move bar

  lda rasterAnimFirst
  cmp #(rasterSine-rasterAnim)
  bcc !+
  // we are using the moving rasterbar
  ldx phase: #0
  lda sineRaster,x
  clc
  adc #(rasterSine-rasterAnim)
  sta rasterAnimFirst

  lda phase
  clc
  adc #1
  and #$3f
  sta phase
!:

  // animate bar..

  lda animRaster: #0
  beq skipAnim

  lda wait: #0
  bne waitAnim

  lda #4
  sta wait

  lda rasterAnimFirst
  clc
  adc #21
  cmp #6*21
  bcc !+
  sec
  sbc #6*21
!:
  sta rasterAnimFirst

waitAnim:
  dec wait
skipAnim:

  lda #<controlIrq
  sta $fffe
  lda #>controlIrq
  sta $ffff
  lda #$fa
  sta $d012
  asl $d019
  
  cli
  jsr scroller

  lda restorea: #0
  ldx restorex: #0
  ldy restorey: #0
  rti
}

* = * "[DATA] scrolltext"
scrolltext:
.var text0  = @"                    S"
.var text1  = @"Byou have been watching halloweed 4.. after a year we are back at transmission64 to blow your mind, this time with guest stars youth playing the keys like a perfectionist and deekay holding his paintbrush to paint behind the pixels. youth very kindly provided an excellent groovy soundtrack and deekay the pixels you are looking at. after last transmission deekay complained that we claimed ufli-max was finally possible, but no ufli-max picture was shown. so last x party he decided to step up and draw this D colourful B ufli-max logo in the top part of the screen together with this nice underlay effect scroller.   "
.var text2  = @"                    R"
.var text3  = @" C D E F greetings to abyss connection  agony  artline designs  arsenic  artstate  atlantis  arise  bam bam  blazon  bonzai  booze design  camelot  censor design  chorus  cosine  creators  crest  dekadence  delysid  desire  elysium  excess  extend  f4cg  fairlight  finnish gold  fossil  focus  g*p  gorbat soft  graffity  hitmen  hoaxers hokuto force  house designs  laxity  lepsi de  lethargy  lft  mayday  megastyle  msl  nah kolor  nostalgia  offence  onslaught  oxyron  padua  panoramic  performers  phonics  plush  powers of pain  pretzel logic  proxima  reflex  resource  reyn ouwehand  samar  silicon ltd  shape  singular crew  success  the dreams  the ruling company  the solution  tempest  triad  trsi  tubesockor  wiseguy industries  wrath design and yavin "
.var text4  = @"                    S"

.var text5  = @" E D C B some words from wvl about this demo.. the '3d' rasterbars part was an experiment to do rasterbars not by plotting colors in a table, but using a pointer-like approach. this allows plotting way more bars (or at least really big bars) at quite a complexity expense. i'm not sure it's worth it. the agsp part in this demo is the main point of the demo. i was always inspired by clarence's agsp-without-linecrunch in rocketry (watch it!) but felt it needed to move a multicolor bitmap. the issue with multicolor is ofcourse the extra d800 colors you need to move around in the memory. i finally managed to eek out one more line of hidden linecrunch, which allowed to move a multicolor bitmap instead of a hires one. "
.var text6  = @" then it struck me : i wanted the bitmap to be higher aswell. to do that, you have to copy one extra bitmaprow each frame. in total there is about one quarter more memory to move around. it seemed impossible, but in the end it works out. i call this version agsp-multimax, because there will never be a better bitmap mover on c-64 :-) "
.var text7  = @" next up, the distorter is ofcourse based on trident's one from 13:37. i wondered how it would look if i could make it move horizontally aswell. if you ask me it looks quite *amazing* and fluid! i also allowed the part to modify the distortion in realtime and added rorschach-mode and some colors, pretty nice. moving horizontally is a lot harder than vertically because different vic-banks are use for different rows. the picture itself is 48 chars wide, but could have been 64 chars wide. i really liked the 2:1 ratio it has now. "
.var text8  = @" finally, i think enough has been said about ufli. the version in this demo is my original 124-line version that i made in 2006, now reused to display the most colourful logo ever. that's it for now, you'll be seeing me around.         "

.var text9  = @" hey sceners, it's deekay on the keys, your gfx person for this part!.. so i'm the reason this part exists, because i found it irritating that something called \"ufli max\" is actually neither underlay nor a graphics mode. so i thought it needed a proper picture to qualify for a name like that! one that maxes out the color density and really shows off the full fli! since there is no editor (shame on you, wvl!), i made photoshop my bitch, pixelling tediously in layer masks and constantly working with 8*1 selections for the colors, always trying to manually stay within the mode's restrictions.."
.var text10 = @" merely switching priorities (* key in our editors!) is insanely complicated like that and takes filling 2 8*1 selections and manually inverting two 8*1 layer masks, so any antialiasing that you can spot in the gfx took for fucking ever!.. anyhow, i had an idea how to speed things up when working in this mode, but that's for another demo! :-) hey, what do you think of this nice underlay 3*3 scroller? i had this idea decades ago, wondering why nobody's ever done that and when wvl asked what i wanted for a scroller i suggested it. i think it looks super nice and fits the underlay theme just perfectly!.."
.var text11 = @" plus we could even use the nice rotating blue bar raster anim i made some 25 years ago that jammer just made thcm throw out of the faithless digi part in next level (which also features scrolling underlay gfx, just with some super weird ecm limitations) without even talking to me (i love you kamil, but that did annoy me!).. anyhow, gotta get back to some more censor work, cause there's that other offline party called mysdata happening in sweden on the same weekend... see you guys in another scrollie and remember to always keep your thumbs up! deekay/crest+censor+performers+oxy is out..         "

.var text12 = @" hi all… nyke here… i hope you all like this new demo from xenon with some nice effects and music. i had the honor to pixel some graphics voor this nice production. it has actually been a while since i wrote some real lines for a scroll text.  i think it was back in time when i was a member of powers of pain in the nineties. i now realize that it was a lot easier to write some nonsense back then, maybe it has to do with a long period of absence in the scene. i was not active from about 1995 to 2017. so for quite a while. then i met the guys from xenon and returned to the scene. xenon felt like a warm bath to say and i regained the joy of being a part of a demo group once again."
.var text13 = @" i am actually quite proud of what we have realized the last few years and hope we can keep on creating enjoyable stuff for you all in the future. when i returned to the scene i was really flabbergasted by the stuff still being created for this humble machine. so many people still active or active again and creating this art for this device we all seem to have embraced. simply great and wonderful. i guess i feel the urge to thank you all for that. thank you and keep up the good work. "
.var text   = text0+text1+text2+text3+text4+text5+text6+text7+text8+text9+text10+text11+text12+text13

.for (var i=0;i<text.size();i++){
  .if (text.charAt(i)==' ')   {.byte 0 }
  .if (text.charAt(i)=='.')   {.byte 27 }
  .if (text.charAt(i)==',')   {.byte 28 }
  .if (text.charAt(i)=='!')   {.byte 29 }
  .if (text.charAt(i)=='?')   {.byte 30 }
  .if (text.charAt(i)=='-')   {.byte 31 }
  .if (text.charAt(i)=='+')   {.byte 32 }
  .if (text.charAt(i)=="'")   {.byte 33 }
  .if (text.charAt(i)==@"\"") {.byte 34 }
  .if (text.charAt(i)==':')   {.byte 35 }
  .if (text.charAt(i)=='/')   {.byte 36 }
  .if (text.charAt(i)=='*')   {.byte 37 }
  .if (text.charAt(i)=="(")   {.byte 38 }
  .if (text.charAt(i)==")")   {.byte 39 }
  .if (text.charAt(i)=='0')   {.byte 40 }
  .if (text.charAt(i)=='1')   {.byte 41 }
  .if (text.charAt(i)=='2')   {.byte 42 }
  .if (text.charAt(i)=='3')   {.byte 43 }
  .if (text.charAt(i)=='4')   {.byte 44 }
  .if (text.charAt(i)=='5')   {.byte 45 }
  .if (text.charAt(i)=='6')   {.byte 46 }
  .if (text.charAt(i)=='7')   {.byte 47 }
  .if (text.charAt(i)=='8')   {.byte 48 }
  .if (text.charAt(i)=='9')   {.byte 49 }
  .if (text.charAt(i)=='A')   {.byte $81 } // scrollspeed 1
  .if (text.charAt(i)=='B')   {.byte $82 } // scrollspeed 2
  .if (text.charAt(i)=='C')   {.byte $83 } // scrollspeed 3
  .if (text.charAt(i)=='D')   {.byte $84 } // scrollspeed 4
  .if (text.charAt(i)=="E")   {.byte $85 } // scrollspeed 5
  .if (text.charAt(i)=='F')   {.byte $86 } // scrollspeed 6
  .if (text.charAt(i)=='G')   {.byte $87 } // scrollspeed 7
  .if (text.charAt(i)=='H')   {.byte $88 } // scrollspeed 8

  .if (text.charAt(i)=='L')   {.byte $fc } // turn on light grey raster
  .if (text.charAt(i)=='R')   {.byte $fd } // turn on raster
  .if (text.charAt(i)=='S')   {.byte $fe } // turn on sinus raster

  .if ((text.charAt(i)>='a') && (text.charAt(i)<='z')) {.byte text.charAt(i) }
}

.byte $ff  // end of text

* = $c000 "[DATA] rasterbar animation"  // this has to be page aligned!
rasterAnim:
// animated raster bar
.byte $0,$0,$8,$c,$c,$b,$4,$e,$1,$1,$1,$e,$e,$e,$f,$f,$c,$6,$6,$6,$b
.byte $0,$b,$8,$c,$6,$6,$b,$7,$1,$1,$e,$e,$e,$7,$f,$f,$6,$6,$6,$b,$b
.byte $b,$b,$8,$6,$6,$6,$f,$7,$1,$e,$e,$e,$1,$7,$f,$e,$b,$6,$8,$b,$b
.byte $b,$b,$6,$6,$6,$f,$f,$7,$e,$e,$e,$1,$1,$7,$e,$e,$4,$c,$8,$b,$0
.byte $b,$0,$6,$6,$c,$f,$f,$e,$e,$e,$1,$1,$1,$e,$e,$e,$c,$c,$8,$6,$0
.byte $0,$0,$6,$c,$c,$f,$4,$e,$e,$1,$1,$1,$e,$e,$e,$f,$c,$c,$6,$6,$0
rasterLightGrey:
// lgrey background
.byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f
rasterSine:
// sine rasterbar
.byte 4,4,14,4,14,14,15,14,15,15,7,15,7,7,1,7,1,1,1,1,13,1,13,13,3,13,3,3,5,3,5,5,8,5,8,8
endRaster:

* = * "[DATA] charmap"
charMap:
.fill CharMap.size(), CharMap.get(i)

* = * "[DATA] sprite font columns"
spriteData:
.fill spriteColumns.size(), spriteColumns.get(i)

// x start position of letters
* = * "[DATA] xpositions in charmap"
xpositions:
.byte  0  // spatie  3 breed
.byte  2  // a
.byte  5  // b
.byte  8  // c
.byte 11  // d
.byte 14  // e
.byte 17  // f
.byte 20  // g
.byte 23  // h
.byte 26  // i   2 breed
.byte 28  // j  
.byte 31  // k
.byte 34  // l  
.byte 37  // m   4 breed
.byte 41  // n
.byte 44  // o
.byte 47  // p
.byte 50  // q
.byte 53  // r  
.byte 56  // s
.byte 59  // t
.byte 62  // u
.byte 65  // v
.byte 68  // w   4 breed 
.byte 72  // x
.byte 75  // y
.byte 78  // z
.byte 81  // .  2 breed
.byte 83  // ,  2 breed
.byte 85  // !  2 breed
.byte 87  // ?  3 breed
.byte 90  // -  2 breed
.byte 92  // +  3 breed
.byte 95  // '  2 breed
.byte 97  // "  3 breed
.byte 100 // :  2 breed
.byte 102 // /  2 breed
.byte 104 // *  3 breed
.byte 107 // (  2 breed
.byte 109 // )  2 breed
.byte 111 // 0  3 breed
.byte 114 // 1  2 breed
.byte 116 // 2  3 breed
.byte 119 // 3
.byte 122 // 4
.byte 125 // 5
.byte 128 // 6
.byte 131 // 7
.byte 134 // 8
.byte 137 // 9
.byte 140 // end

*=* "[DATA] sine raster movement"
sineRaster:
{
    .var sinSize = 64
    .var sinMin  = 00
    .var sinMax  = (endRaster-rasterSine-20-1)
    .var sinAmp  = 0.5 * (sinMax-sinMin)

    .fill sinSize, (sinMin+sinAmp+0.5) + sinAmp*sin(toRadians(i*360/sinSize))
}
