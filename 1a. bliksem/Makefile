EFFECT_NAME = 1a_bliksem

MUSIC = intro

include ../include.mk

$(EFFECT_NAME).pef: $(EFFECT_NAME).efo
		$(MKPEF) -o $@ $^ --music ../music/$(MUSIC).sid,,7c
