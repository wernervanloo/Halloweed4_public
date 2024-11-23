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