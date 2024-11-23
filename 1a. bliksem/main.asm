.var picture_a1 = LoadPicture("./includes/c64/a1.gif", List().add($000000, $ffffff)) 
.var picture_a2 = LoadPicture("./includes/c64/a2.gif", List().add($000000, $ffffff)) 
.var picture_a3 = LoadPicture("./includes/c64/a3.gif", List().add($000000, $ffffff)) 

.var picture_b1 = LoadPicture("./includes/c64/b1.gif", List().add($000000, $ffffff)) 
.var picture_b2 = LoadPicture("./includes/c64/b2.gif", List().add($000000, $ffffff)) 
.var picture_b3 = LoadPicture("./includes/c64/b3.gif", List().add($000000, $ffffff)) 
.var picture_b4 = LoadPicture("./includes/c64/b4.gif", List().add($000000, $ffffff)) 
.var picture_b5 = LoadPicture("./includes/c64/b5.gif", List().add($000000, $ffffff)) 
.var picture_b6 = LoadPicture("./includes/c64/b6.gif", List().add($000000, $ffffff)) 
.var picture_b7 = LoadPicture("./includes/c64/b7.gif", List().add($000000, $ffffff)) 

.var picture_c1 = LoadPicture("./includes/c64/c1.gif", List().add($000000, $ffffff)) 
.var picture_c2 = LoadPicture("./includes/c64/c2.gif", List().add($000000, $ffffff)) 
.var picture_c3 = LoadPicture("./includes/c64/c3.gif", List().add($000000, $ffffff)) 

.var picture_d1 = LoadPicture("./includes/c64/d1.gif", List().add($000000, $ffffff)) 
.var picture_d2 = LoadPicture("./includes/c64/d2.gif", List().add($000000, $ffffff)) 
.var picture_d3 = LoadPicture("./includes/c64/d3.gif", List().add($000000, $ffffff)) 
.var picture_d4 = LoadPicture("./includes/c64/d4.gif", List().add($000000, $ffffff)) 
.var picture_d5 = LoadPicture("./includes/c64/d5.gif", List().add($000000, $ffffff)) 

.var picture_e1 = LoadPicture("./includes/c64/e1.gif", List().add($000000, $ffffff)) 
.var picture_e2 = LoadPicture("./includes/c64/e2.gif", List().add($000000, $ffffff)) 
.var picture_e3 = LoadPicture("./includes/c64/e3.gif", List().add($000000, $ffffff)) 

.var music = LoadSid("../music/intro.sid")

#if !AS_SPINDLE_PART
  *=music.location "[MUSIC]"
    .fill music.size, music.getData(i)
#endif

#if AS_SPINDLE_PART
  .label spindleLoadAddress = $2000
  *=spindleLoadAddress-18-13-3 "Spindle header"
  .label spindleHeaderStart = *

    .text "EFO2"        // fileformat magic
    .word prepare       // prepare routine
    .word start         // setup routine
    .word 0             // irq handler
    .word 0             // main routine
    .word 0             // fadeout routine
    .word 0             // cleanup routine
    .word 0             // location of playroutine call

    .byte 'S'           // this part is IO safe and can load below $d000
    .byte 'I', $04, $07        
    .byte 'M', <music.play, >music.play
    .byte 'Z', $02, $04
    .byte 'Z', $10, $17

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

// these are the 0 page adresses for the part

.label atemp        = $10
.label xtemp        = $11
.label ytemp        = $12
.label zp01         = $13   // store $01 value for loading under d000
.label whichpicture = $14
.label d020Color    = $15
.label d020ColorOri = $16
.label spriteColor  = $17

.label screen      = $0400  // basic screen
.label sprites     = $2000

* = screen "[VIC] copy of basic screen" virtual
.fill 1000,0

// ---------------------------------------------------
// read pictures and convert to sprites and spritemaps
// ---------------------------------------------------

// read all sprites from the raw sprite data
.var nrSprites = 1                // current number of sprites (first sprite is empty)
.var Sprites   = List()           // array with all current sprites
.var SpriteMap = List()           // spritemap

// fill with empty sprite (sprite 0 = empty sprite)
.for (var byte=0; byte<64; byte++) .eval Sprites.add($00)

// these are the pictures to evaluate
.var Pictures = List().add(picture_a1, picture_a2, picture_a3, 
                           picture_b1, picture_b2, picture_b3, picture_b4, picture_b5, picture_b6, picture_b7, 
                           picture_c1, picture_c2, picture_c3, 
                           picture_d1, picture_d2, picture_d3, picture_d4, picture_d5,
                           picture_e1, picture_e2, picture_e3)

.var nrPictures = Pictures.size()

// loop over all pictures
.for (var pic=0; pic<Pictures.size(); pic++)
{
  .var Pic = Pictures.get(pic)
  .var fillUp = 0

  // loop over complete raw sprite data
  .for (var spriteY=0; spriteY<5; spriteY++) {
    .for (var spriteX=0; spriteX<7; spriteX++) {
      
      .var completelyFilled = 1

      .var Sprite = List()        // read sprite at this position

      .for (var y=0; y<21; y++) {
        .for (var x=0; x<3; x++) {  // read all bytes of sprite

          .var xpos = spriteX*3+x
          .var ypos = spriteY*21+y

          .if ((xpos>=20) || (ypos>=100))
          {
            .eval Sprite.add(fillUp)  // fill with fillUp byte

          } else {
            .var value = Pic.getSinglecolorByte(xpos, ypos)
            .eval Sprite.add(value)
            .if (value!=255)
            {
              .eval completelyFilled = 0
            }
          }
        }
      }
      // is this a completely white frame? then fill up the edges with #$ff
      .if (completelyFilled == 1)
      {
        .eval fillUp = 255
      }

      .eval Sprite.add(0) // add closing byte

      // check if sprite is already known
      // -----------------------------------
      
      .var notFound = true    // assume this is a new character
      // loop over all known sprites
      .for (var i=0; i<nrSprites; i++) {
        .var isSame = true
        // test if sprite is the same
        .for (var byte=0; byte<63; byte++) {
          .if (Sprites.get(i*64+byte) != Sprite.get(byte)) {
            .eval isSame = false
            .eval byte   = 64     // stop loop early
          }
        }

        // is the sprite the same?
        .if (isSame == true) {
          .eval SpriteMap.add(i)           // write sprite into map
          .eval i = nrSprites              // break loop
          .eval notFound = false           // sprite found
        }
      } // for i

      // sprite not found? insert it
      .if (notFound == true) {
        // copy sprite into spriteset
        .for (var byte=0; byte<64; byte++) {
          .eval Sprites.add(Sprite.get(byte))  // copy byte
        }
        .eval SpriteMap.add(nrSprites)         // write sprite into map
        .eval nrSprites = nrSprites+1          // one sprite added
      }
    } // for y
  } // for x
} // pic

* = sprites "[DATA] lightning sprites"
  .fill Sprites.size(), Sprites.get(i)
  
* = * "[DATA] spritemaps"
spritemaps:
  .for (var y=0; y<5; y++) {
    .for (var x=0; x<7; x++) {
      .for (var pic=0; pic<nrPictures; pic++)
      {
        .var i = pic*5*7+y*7+x
        .byte SpriteMap.get(i)+((sprites&$3fff)/64)
      }
    }
  }

*=* "[CODE] main code"
start:
{
  sei

  lda $01
  sta restore01

  lda #$35
  sta $01

  jsr prepare

  lda $d020
  sta d020Color
  sta d020ColorOri
  lda #$f
  sta spriteColor
  
  lda #$01
  sta $d019
  sta $d01a
  sta $dc0d
  lda #<topirq
  sta $fffe
  lda #>topirq
  sta $ffff
  lda #$00
  sta $d012

  lda $d011
  and #$7f
  sta $d011

  lda #0
  sta nextpart
  sta timelow
  sta timehigh
  sta whichpicture

  jsr music.init

  jsr setsprites
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
  rts
}

topirq:
  pha
  txa
  pha
  tya
  pha
  lda $01
  pha

  lda #$35
  sta $01

  #if !AS_SPINDLE_PART
    lda #$94
    sta $dd00
  #endif

  lda #((screen&$c000)/$4000)|$3c
  sta $dd02
  lda #((screen&$3fff)/$0400)*$10|$5
  sta $d018

  // turn sprites on/off
  jsr script

  lda spriteson
  sta $d015
  lda d020Color
  sta $d020

  jsr setsprites

  // keep demotime (always before calling music player)
  inc timelow
  bne !+
  inc timehigh
!:
  jsr music.play

  lda #<multiplex0
  sta $fffe
  lda #>multiplex0
  sta $ffff
  lda #$33
  sta $d012
  asl $d019

endtopirq:
  pla
  sta $01
  pla
  tay
  pla
  tax
  pla
  rti

multiplex0:
  pha
  txa
  pha
  tya
  pha
  lda $01
  pha

  lda #$35
  sta $01

  lda $d001
  clc
  adc #42
  sta $d001
  sta $d003
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d

  lda #<premultiplex1
  sta $fffe
  lda #>premultiplex1
  sta $ffff
  lda #($32+42-1-1-2)
  sta $d012
  asl $d019

  jmp endtopirq


script:
  // still waiting?
  lda wait
  beq advanceScript
  dec wait
  rts

advanceScript:
  ldx scriptpointer: #0
  lda scriptdata,x
  cmp #$00
  bne !+
  // wait
  lda scriptdata+1,x
  sta wait
  lda #2
  bne endScript2

!:
  cmp #$01
  bne checkChangePicture

  // flip sprites
  lda spriteson
  eor #$7f
  sta spriteson

  lda spriteson
  beq !+
  lda #$f
  sta d020Color
  //lda #$00
  //sta d020ColorOri
  jmp endScript
!:
  lda d020ColorOri
  sta d020Color
  jmp endScript

checkChangePicture:
  cmp #$02
  bne !+
  // change picture
  lda scriptdata+1,x
  sta whichpicture
  lda #2
  bne endScript2
!:
  cmp #$03
  bne !+
  // change sprite color
  lda scriptdata+1,x
  sta spriteColor
  lda #2
  bne endScript2
!:
  cmp #$04
  bne !+
  // screen white
  lda #1
  sta d020Color
  sta $d021
  lda #$0b
  sta $d011
  bne endScript
!:
  cmp #$ff
  bne !+
  #if !AS_SPINDLE_PART
    lda #0
    sta scriptpointer
  #endif
  lda #1
  sta nextpart
  rts
!:
endScript:
  lda #1
endScript2:
  clc
  adc scriptpointer
  sta scriptpointer
  rts

scriptdata:
// 0 = a1, 2 = a3
// 3 = b1, 9 = b7
// 10 = c1, 12 = c3

  .byte 3,$f  // color light grey

  .byte 0,87  // wait
  .byte 2,13  // d1 picture
  .byte 1     // flip sprites on
  .byte 2,14  // d2 picture
  .byte 2,15  // d3 picture
  .byte 2,16  // d4 picture
  .byte 2,17  // d5 picture
  .byte 1     // flip sprites off

  .byte 0,90-8+3  // wait
  .byte 2,18   // e1 picture
  .byte 1     // flip sprites on
  .byte 1     // flip sprites off
  .byte 1     // flip sprites on
  .byte 2,19  // e2 picture
  .byte 2,20  // e3 picture
  .byte 2,19  // e2 picture
  .byte 1     // flip sprites off

  .byte 0,24  // wait
  .byte 2,0   // a1 picture
  .byte 1     // flip sprites on
  .byte 2,1   // a2 picture
  .byte 2,2   // a3 picture
  .byte 1     // flip sprites off
  
  .byte 3,$f  // color dark grey
  .byte 0,66  // wait
  .byte 2,10  // c1 picture
  .byte 1     // flip sprites on
  .byte 2,11  // c2 picture
  .byte 2,12  // c3 picture
  .byte 1     // flip sprites off

  .byte 0,36  // wait
  .byte 2,3   // b1 picture
  .byte 1     // flip sprites on
  .byte 2,4   // b2 picture
  .byte 2,5   // b3 picture
  .byte 2,6   // b4 picture
  .byte 2,7   // b5 picture
  .byte 2,8   // b6 picture
  .byte 2,9   // b7 picture
  .byte 1     // flip sprites off
  .byte 4     // screen white

  .byte $ff   // restart / end

wait:
  .byte 40
spriteson:
  .byte 0

setsprites:
{
  lda #$18
  sta $d000
  lda #$48
  sta $d002
  lda #$78
  sta $d004
  lda #$a8
  sta $d006
  lda #$d8
  sta $d008
  lda #$08
  sta $d00a
  lda #$38
  sta $d00c
  lda #%01100000
  sta $d010
  lda #$32
  sta $d001
  sta $d003
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d
  lda #$ff
  sta $d017
  sta $d01d
  lda #$ff
  sta $d01b
  lda #$00
  sta $d01c
  
  lda spriteColor
  sta $d027
  sta $d028
  sta $d029
  sta $d02a
  sta $d02b
  sta $d02c
  sta $d02d
  
  ldx whichpicture  // which picture?
  lda spritemaps+nrPictures*0,x
  sta screen+$3f8
  lda spritemaps+nrPictures*1,x
  sta screen+$3f9
  lda spritemaps+nrPictures*2,x
  sta screen+$3fa
  lda spritemaps+nrPictures*3,x
  sta screen+$3fb
  lda spritemaps+nrPictures*4,x
  sta screen+$3fc
  lda spritemaps+nrPictures*5,x
  sta screen+$3fd
  lda spritemaps+nrPictures*6,x
  sta screen+$3fe

  lda spritemaps+nrPictures*7,x
  sta multiplex1.store1
  lda spritemaps+nrPictures*8,x
  sta multiplex1.store2
  lda spritemaps+nrPictures*9,x
  sta multiplex1.store3
  lda spritemaps+nrPictures*10,x
  sta multiplex1.store4
  lda spritemaps+nrPictures*11,x
  sta multiplex1.store5
  lda spritemaps+nrPictures*12,x
  sta multiplex1.store6
  lda spritemaps+nrPictures*13,x
  sta multiplex1.store7

  lda spritemaps+nrPictures*14,x
  sta multiplex2.store1
  lda spritemaps+nrPictures*15,x
  sta multiplex2.store2
  lda spritemaps+nrPictures*16,x
  sta multiplex2.store3
  lda spritemaps+nrPictures*17,x
  sta multiplex2.store4
  lda spritemaps+nrPictures*18,x
  sta multiplex2.store5
  lda spritemaps+nrPictures*19,x
  sta multiplex2.store6
  lda spritemaps+nrPictures*20,x
  sta multiplex2.store7

  lda spritemaps+nrPictures*21,x
  sta multiplex3.store1
  lda spritemaps+nrPictures*22,x
  sta multiplex3.store2
  lda spritemaps+nrPictures*23,x
  sta multiplex3.store3
  lda spritemaps+nrPictures*24,x
  sta multiplex3.store4
  lda spritemaps+nrPictures*25,x
  sta multiplex3.store5
  lda spritemaps+nrPictures*26,x
  sta multiplex3.store6
  lda spritemaps+nrPictures*27,x
  sta multiplex3.store7

  lda spritemaps+nrPictures*28,x
  sta multiplex4.store1
  lda spritemaps+nrPictures*29,x
  sta multiplex4.store2
  lda spritemaps+nrPictures*30,x
  sta multiplex4.store3
  lda spritemaps+nrPictures*31,x
  sta multiplex4.store4
  lda spritemaps+nrPictures*32,x
  sta multiplex4.store5
  lda spritemaps+nrPictures*33,x
  sta multiplex4.store6
  lda spritemaps+nrPictures*34,x
  sta multiplex4.store7
}
waste12:
  rts

premultiplex1:
  pha
  lda $01
  pha
  lda #$35
  sta $01
  lda #($32+42-1-1)
  sta $d012
  lda #<multiplex1
  sta $fffe
  lda #>multiplex1
  sta $ffff
  asl $d019

  cli
  jmp endirq

multiplex1:
{
  sta atemp42
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #<premultiplex2
  sta $fffe
  lda #>premultiplex2
  sta $ffff
  lda #($32+42+42-1-3)
  sta $d012
  asl $d019

  nop

  lda store1: #0
  sta screen+$3f8
  lda store2: #0
  sta screen+$3f9
  lda store3: #0
  sta screen+$3fa
  lda store4: #0
  sta screen+$3fb
  lda store5: #0
  sta screen+$3fc
  lda store6: #0
  sta screen+$3fd
  lda store7: #0
  sta screen+$3fe
}
add42:
  lda $d001
  clc
  adc #42
  sta $d001
  sta $d003
  sta $d005
  sta $d007
  sta $d009
  sta $d00b
  sta $d00d

  lda restore01: #0
  sta $01
  lda atemp42: #0
  rti

premultiplex2:
  pha
  lda $01
  pha
  lda #$35
  sta $01
  lda #($32+42+42-1)
  sta $d012
  lda #<multiplex2
  sta $fffe
  lda #>multiplex2
  sta $ffff
  asl $d019
  cli
  .fill 10,NOP
  jmp endirq

multiplex2:
{
  sta atemp42
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #<premultiplex3
  sta $fffe
  lda #>premultiplex3
  sta $ffff
  lda #($32+42+42+42-1-2)
  sta $d012

  nop

  lda store1: #0
  sta screen+$3f8
  lda store2: #0
  sta screen+$3f9
  lda store3: #0
  sta screen+$3fa
  lda store4: #0
  sta screen+$3fb
  lda store5: #0
  sta screen+$3fc
  lda store6: #0
  sta screen+$3fd
  lda store7: #0
  sta screen+$3fe

  asl $d019

  jmp add42
}

premultiplex3:
  pha
  lda $01
  pha
  lda #$35
  sta $01
  lda #($32+42+42+42-1)
  sta $d012
  lda #<multiplex3
  sta $fffe
  lda #>multiplex3
  sta $ffff
  asl $d019
  cli
endirq:
  .fill 40,NOP
  pla
  sta $01
  pla
  rti

multiplex3:
{
  sta atemp42
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #<premultiplex4
  sta $fffe
  lda #>premultiplex4
  sta $ffff
  lda #($32+42+42+42+42-1-2)
  sta $d012

  nop

  lda store1: #0
  sta screen+$3f8
  lda store2: #0
  sta screen+$3f9
  lda store3: #0
  sta screen+$3fa
  lda store4: #0
  sta screen+$3fb
  lda store5: #0
  sta screen+$3fc
  lda store6: #0
  sta screen+$3fd
  lda store7: #0
  sta screen+$3fe

  asl $d019

  jmp add42
}

premultiplex4:
  pha
  lda $01
  pha
  lda #$35
  sta $01
  lda #($32+42+42+42+42-1)
  sta $d012
  lda #<multiplex4
  sta $fffe
  lda #>multiplex4
  sta $ffff
  asl $d019
  cli
  jmp endirq

multiplex4:
{
  sta atemp42
  lda $01
  sta restore01
  lda #$35
  sta $01

  lda #$00
  sta $d012

  lda #<topirq
  sta $fffe
  lda #>topirq
  sta $ffff

  nop

  lda store1: #0
  sta screen+$3f8
  lda store2: #0
  sta screen+$3f9
  lda store3: #0
  sta screen+$3fa
  lda store4: #0
  sta screen+$3fb
  lda store5: #0
  sta screen+$3fc
  lda store6: #0
  sta screen+$3fd
  lda store7: #0
  sta screen+$3fe

  asl $d019
  
  jmp add42
}
