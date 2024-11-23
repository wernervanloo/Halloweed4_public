
.var bitmapData = List()
.var screenData = List()
.var d800Data   = List()

.for (var i=0; i<koala.getBitmapSize(); i++) .eval bitmapData.add(koala.getBitmap(i))
.for (var i=0; i<koala.getScreenSize(); i++) .eval screenData.add(koala.getScreen(i))
.for (var i=0; i<koala.getD800Size();   i++) .eval d800Data.add(koala.getD800(i))

// optimize bitmap
.for (var charY=0; charY<25; charY++)
{
  .for (var charX=0; charX<40; charX++)
  {
    // determine the bitpairs used.

    .var used00 = 0  // d021 color
    .var used01 = 0  // msb screen
    .var used10 = 0  // lsb screen
    .var used11 = 0  // d800

    .for (var i=0; i<8; i++)
    {
      .var byte = bitmapData.get(i + (charX*8) + (charY*320))
      .var temp = byte
      .for (var pair=0; pair<4; pair++)
      {
        .var bitpair = temp & 3
        .if (bitpair == 0) .eval used00=1
        .if (bitpair == 1) .eval used01=1
        .if (bitpair == 2) .eval used10=1
        .if (bitpair == 3) .eval used11=1
        .eval temp = temp >> 2
      }
    }
    
    // read colors.. 
    .var pos = charX+(40*charY)
    .var screenColor = screenData.get(pos)
    .var msbColor    = ((screenColor & $f0) >> 4)
    .var lsbColor    =  (screenColor & $f)
    .var d800Color   = ((d800Data.get(pos)) & $f)
      
    // ---------------------------------------------------
    // 1. does any used bitpair have the background color?
    // ---------------------------------------------------

    .if ((used01 > 0) && (msbColor == background))
    {
      //.print ("msbColor is background!")

      // remove this bitpair
      .for (var i=0; i<8; i++)
      {
        .var bytePos = i + (charX*8) + (charY*320)
        .var byte = bitmapData.get(bytePos)
        .if ((byte & %11000000) == %01000000) .eval byte = byte & %00111111
        .if ((byte & %00110000) == %00010000) .eval byte = byte & %11001111
        .if ((byte & %00001100) == %00000100) .eval byte = byte & %11110011
        .if ((byte & %00000011) == %00000001) .eval byte = byte & %11111100
        .eval bitmapData.set(bytePos, byte)
      }

      .eval msbColor = background
      .eval used01 = 0      
    }

    .if ((used10 > 0) && (lsbColor == background))
    {
      //.print ("lsbColor is background!")

      // remove this bitpair
      .for (var i=0; i<8; i++)
      {
        .var bytePos = i + (charX*8) + (charY*320)
        .var byte = bitmapData.get(bytePos)
        .if ((byte & %11000000) == %10000000) .eval byte = byte & %00111111
        .if ((byte & %00110000) == %00100000) .eval byte = byte & %11001111
        .if ((byte & %00001100) == %00001000) .eval byte = byte & %11110011
        .if ((byte & %00000011) == %00000010) .eval byte = byte & %11111100
        .eval bitmapData.set(bytePos, byte)
      }

      .eval lsbColor = background
      .eval used10 = 0      
    }

    .if ((used11 > 0) && (d800Color == background))
    {
      //.print ("d800Color is background!")

      // remove this bitpair
      .for (var i=0; i<8; i++)
      {
        .var bytePos = i + (charX*8) + (charY*320)
        .var byte = bitmapData.get(bytePos)
        .if ((byte & %11000000) == %11000000) .eval byte = byte & %00111111
        .if ((byte & %00110000) == %00110000) .eval byte = byte & %11001111
        .if ((byte & %00001100) == %00001100) .eval byte = byte & %11110011
        .if ((byte & %00000011) == %00000011) .eval byte = byte & %11111100
        .eval bitmapData.set(bytePos, byte)
      }

      .eval d800Color = background
      .eval used11 = 0      
    }

    // count the number of colors used
    .var colorsUsed = 0
    .if (used01 > 0) .eval colorsUsed = colorsUsed+1
    .if (used10 > 0) .eval colorsUsed = colorsUsed+1
    .if (used11 > 0) .eval colorsUsed = colorsUsed+1

    // ------------------------------------------
    // 2. if only 1 color used, it should be D800
    // ------------------------------------------

    .if ((colorsUsed == 1) && (used11 == 0))
    {
      // the color used is not the D800 color!!
      //.print ("error color at "+charX+","+charY)

      .if (used01 == 1)
      {
        // replace this bitpair
        .for (var i=0; i<8; i++)
        {
          .var bytePos = i + (charX*8) + (charY*320)
          .var byte = bitmapData.get(bytePos)
          .if ((byte & %11000000) == %01000000) .eval byte = byte | %11000000
          .if ((byte & %00110000) == %00010000) .eval byte = byte | %00110000
          .if ((byte & %00001100) == %00000100) .eval byte = byte | %00001100
          .if ((byte & %00000011) == %00000001) .eval byte = byte | %00000011
          .eval bitmapData.set(bytePos, byte)
        }

        .eval used01 = 0
        .eval d800Color = msbColor
        .eval msbColor = background
      }
   
      .if (used10 == 1)
      {
        // replace this bitpair
        .for (var i=0; i<8; i++)
        {
          .var bytePos = i + (charX*8) + (charY*320)
          .var byte = bitmapData.get(bytePos)
          .if ((byte & %11000000) == %10000000) .eval byte = byte | %11000000
          .if ((byte & %00110000) == %00100000) .eval byte = byte | %00110000
          .if ((byte & %00001100) == %00001000) .eval byte = byte | %00001100
          .if ((byte & %00000011) == %00000010) .eval byte = byte | %00000011
          .eval bitmapData.set(bytePos, byte)
        }

        .eval used10 = 0
        .eval d800Color = lsbColor
        .eval lsbColor = background
      }

      // update colors
      .eval screenData.set(pos, (msbColor << 4) | lsbColor)
      .eval d800Data.set(  pos, d800Color)
      .eval used11 = 1
    }

    // -----------------------------------------------------------------------------
    // if 2 colors are used, then this should be the LSB color and MSB screen colors 
    // -----------------------------------------------------------------------------

    .if ((colorsUsed == 2) && (used11 == 1))  // 2 colors and d800 color used?
    {
      .if (used01 == 0)  // swap with msb?
      {
        // replace this bitpair
        .for (var i=0; i<8; i++)
        {
          .var bytePos = i + (charX*8) + (charY*320)
          .var byte = bitmapData.get(bytePos)
          .if ((byte & %11000000) == %11000000) .eval byte = (byte & %00111111) | %01000000
          .if ((byte & %00110000) == %00110000) .eval byte = (byte & %11001111) | %00010000
          .if ((byte & %00001100) == %00001100) .eval byte = (byte & %11110011) | %00000100
          .if ((byte & %00000011) == %00000011) .eval byte = (byte & %11111100) | %00000001
          .eval bitmapData.set(bytePos, byte)
        }

        .eval msbColor = d800Color
      }
      .if (used10 == 0)  // swap with lsb?
      {
        // replace this bitpair
        .for (var i=0; i<8; i++)
        {
          .var bytePos = i + (charX*8) + (charY*320)
          .var byte = bitmapData.get(bytePos)
          .if ((byte & %11000000) == %11000000) .eval byte = (byte & %00111111) ^ %10000000
          .if ((byte & %00110000) == %00110000) .eval byte = (byte & %11001111) ^ %00100000
          .if ((byte & %00001100) == %00001100) .eval byte = (byte & %11110011) ^ %00001000
          .if ((byte & %00000011) == %00000011) .eval byte = (byte & %11111100) ^ %00000010
          .eval bitmapData.set(bytePos, byte)
        }
        
        .eval lsbColor = d800Color
      }
      
      .eval used01 = 1
      .eval used10 = 1
      .eval used11 = 0
      .eval d800Color = background
    }

    .eval screenData.set(pos, (msbColor << 4 | lsbColor))
    .eval d800Data.set(pos, d800Color | (colorsUsed << 4))
  }
}
