unrollD800:
{
  lda #<speedcode
  sta low
  lda #>speedcode
  sta high

  lda #<d800Colors
  sta d800From
  lda #<screenColors
  sta screenFrom

  lda #0             // set color '0' first
  sta currentColor

colorLoop:
  lda #>d800Colors
  sta d800From+1
  lda #>screenColors
  sta screenFrom+1

  lda #LDA_ZP
  ldy #0
  sta (low),y
  iny
  lda currentColor
  clc
  adc #<col0
  sta (low),y
  iny

  // loop over all d800 colors
loop2:
  ldx #0
loop1:
  lda.abs d800From: d800Colors,x
  sta numcols
  // are there used colors in this char?
  cmp #$10     // # colors used are in the 4 msb bits.. if the value is <$10, then no colors are used
  bcc nextChar // branch if this char does not need fading

  and #$0f
  cmp currentColor: #0
  bne next   // branch if $d800 color does not need fading
  
  // add sta to unrolled code
  lda #STA_ABS
  sta (low),y
  iny

  txa
  sta (low),y
  iny

  lda d800From+1
  clc
  adc #((>$d800)-(>d800Colors))
  sta (low),y
  iny
  
next:
  // check if the screencolor is correct
  lda.abs screenFrom: $0000,x
  sta screenColor
  and #$0f
  cmp currentColor
  bne nextChar

  // 2 or 3 colors to write? if not : skip
  lda numcols
  and #$30
  cmp #$10
  beq nextChar  // only 1 color in this char : skip to next char
  
  // are there 3 colors? then we should load X
  cmp #$30
  bne skipLDX   // skip loading X if there are less than 3 colors in the char

  lda #LDX_ZP
  sta (low),y
  iny
  lda screenColor: #0
  lsr
  lsr
  lsr
  lsr
  clc
  adc #<col0b
  sta (low),y
  iny

skipLDX:
  // add sax to unrolled code
  lda #SAX_ABS
  sta (low),y
  iny

  txa
  sta (low),y
  iny

  lda screenFrom+1
  clc
  adc #((>screen)-(>screenColors))
  sta (low),y
  iny

nextChar:
  cpy #0
  beq noWrite
  tya
  clc
  adc low
  sta low
  bcc *+4
  inc high
  ldy #0
noWrite:
  inx
  bne loop1

  // process next page
  inc screenFrom+1
  inc d800From+1
  lda d800From+1
  cmp #>(d800Colors+$400)
  bne loop2

  inc currentColor
  lda currentColor
  cmp #$10
  beq endSpeedcode
  jmp colorLoop
endSpeedcode:
  lda #RTS
  sta (low),y

  rts
}
