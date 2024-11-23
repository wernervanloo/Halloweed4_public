EFFECT_NAME = 1e_bitmapscroll
INCLUDES = ./includes/

include ../include.mk

DATA = \
   $(INCLUDES)2spot_bitmap.bin \
   $(INCLUDES)3spot_bitmap.bin \
   $(INCLUDES)4spot_bitmap.bin \
   $(INCLUDES)5spot_bitmap.bin 

$(EFFECT_NAME).pef: $(EFFECT_NAME).efo $(DATA)
		$(MKPEF) -o $@ $< \
		--stream $(INCLUDES)2spot_bitmap.bin,ad00 \
		         $(INCLUDES)3spot_bitmap.bin,8d00 \
				 $(INCLUDES)4spot_bitmap.bin,ad00 \
				 $(INCLUDES)5spot_bitmap.bin,8d00

%_bitmap.bin: %.kla
	dd bs=1 skip=2 count=8000 if=$< of=$@

%_bitmap1.bin: %.kla
	dd bs=1 skip=2 count=4096 if=$< of=$@

%_bitmap2.bin: %.kla
	dd bs=1 skip=4098 count=3840 if=$< of=$@

%_colors.bin: %.kla
	dd bs=1 skip=7938 count=2064 if=$< of=$@

%.bin: %.kla
	dd bs=1 skip=2 if=$< of=$@

clean::
	$(DEL) $(INCLUDES)*.bin
