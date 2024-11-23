#import "./bin/bitmap_koppel.txt"
#import "./bin/screen_koppel.txt"
#import "./bin/sprite_koppel.txt"


.var afli = LoadPicture("./deekay/afli.png") 
.var bits = LoadPicture("./deekay/prio.png", List().add($000000, $ffffff))
.var sprite = LoadPicture("./deekay/sprites.png", List().add($000000, $ffffff))

.var bitmap = List()
.var screen = List()

// generate hashtable to decode the bitmap colors
.var ht = Hashtable()

.const RGBValues = List().add($000000,$ffffff,$68372b,$70a4b2,$6f3d86,$588d43,$352879,$b8c76f,
                              $6f4f25,$433900,$9a6759,$444444,$6c6c6c,$9ad284,$6c5eb5,$959595)
.for (var i=0; i<16; i++)
{
  .eval ht.put(RGBValues.get(i),i)
}

// -------------------------------------------------
// read AFLI.png and convert into a real AFLI bitmap
// -------------------------------------------------

// loop over complete raw sprite data
.for (var Y=0; Y<200; Y++) {
  .for (var X=0; X<40; X++) {
    
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
      // no data goes here, add a 0
      .eval bank1.add(0)
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
      // no data goes here, add a 0
      .eval bank2.add(0)
    }
  }
}

* = $4000 "[DATA] color data"
.fill screenData.size(), screenData.get(i)
* = $6000 "[DATA] bitmap data"
.fill bitmapData.size(), bitmapData.get(i)











*=$8000

	jmp start
irq0:	
  pha
  lda $d019
	sta $d019
	inc $d012
	lda #<irq1
	sta $fffe      // set up 2nd IRQ to get a stable IRQ
	cli

  // Following here: A bunch of NOPs which allow the 2nd IRQ
  // to be triggered with either 0 or 1 clock cycle delay
  // resulting in an "almost" stable IRQ.

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
irq1:
ntsc1:  lda #$ea       // modified to NOP NOP on NTSC
	lda #$80
	sta $d018      // setup first color RAM address early
	lda #$38
	sta $d011      // setup first DMA access early
	pla
	pla
	pla
  
  lda $d019
	sta $d019
	lda #$2d
	sta $d012
	lda #<irq0
	sta $fffe      // switch IRQ back to first stabilizer IRQ
	lda $d012
	cmp $d012      // stabilize last jittering cycle
	beq delay      // if equal, 2 cycles delay. else 3 cycles delay
delay:
        stx savex+1

	ldx #$0d
wait:	dex
	bne wait

ntsc2:  lda #$ea       // modified to NOP NOP on NTSC
ntsc3:  lda #$ea       // modified to NOP NOP on NTSC

        // Following here is the main FLI loop which forces the VIC-II to read
        // new color data each rasterline. The loop is exactly 23 clock cycles
        // long so together with 40 cycles of color DMA this will result in
        // 63 clock cycles which is exactly the length of a PAL C64 rasterline. 

        nop
        nop
l0:
	lda tab18+1,x
	sta $d018      // set new color RAM address
	lda tab11+1,x
	sta $d011      // force new color DMA
	inx            // FLI bug $D800 color = 8 (orange)
	cpx #199       // last rasterline?
ntsc4:	bne l0         // branches to l0-1 on NTSC for 2 extra cycles per rasterline

        lda #$70
        sta $d011      // open upper/lower border

savex:  ldx #$00
        pla
nmi:    rti

start:
	sei
	lda #$35
	sta $01        // disable all ROMs
	lda #$7f
	sta $dc0d      // no timer IRQs
	lda $dc0d      // clear timer IRQ flags

	lda #$2b
	sta $d011
	lda #$2d
	sta $d012

	lda #<nmi
	sta $fffa
	lda #>nmi
	sta $fffb      // dummy NMI to avoid crashing due to RESTORE
	lda #<irq0
	sta $fffe
	lda #>irq0
	sta $ffff
	lda #$01
	sta $d01a      // enable raster IRQs

        jsr initgfx
        jsr inittables
        jsr ntscfix

        lda $d019
        dec $d019      // clear raster IRQ flag
        cli
        jmp *          // that's it, no more action needed

initgfx:
	lda #$00
	sta $d015      // disable sprites
	sta $d020      // border color black

  lda #$00       // background color
	sta $d021

  lda #$ff
  sta $7fff      // upper/lower border black

	lda #$c8
	sta $d016
	lda #$08
	sta $d018
	lda #$96       // VIC bank $4000-$7FFF
	sta $dd00

	rts

inittables:
	ldx #$00
l2:	txa
	asl
	asl
	asl
	asl
	and #$70       // color RAMs at $4000
	ora #$08       // bitmap data at $6000
	sta tab18,x    // calculate $D018 table
	txa
	and #$07
	ora #$38       // bitmap
	sta tab11,x    // calculate $D011 table
	inx
	bne l2
	rts

ntscfix:
	bit $d011
	bmi *-3
	bit $d011      // wait for rasterline 256
	bpl *-3
	lda #$00
test:	cmp $d012
	bcs nt
	lda $d012      // get rasterline low byte
nt:	bit $d011
	bmi test
	cmp #$20       // PAL: $37, NTSC: $05 or $06
	bcs pal

	lda #$ea
	sta ntsc1
	sta ntsc2
	sta ntsc3
	dec ntsc4+1
pal:	rts

.align $100
tab18:
* = * + $100
tab11:
