EFFECT_NAME = agsp
INCLUDES = ./includes/

include  ../include.mk

DATA = $(INCLUDES)linecrunch2.bin

$(EFFECT_NAME).pef: $(EFFECT_NAME).efo
		$(MKPEF) -o $@ $^

$(EFFECT_NAME).pef: $(EFFECT_NAME).efo $(DATA)
		$(MKPEF) -o $@ $< \
		--stream $(INCLUDES)linecrunch2.bin,0800
