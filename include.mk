# Set various globally-used variables.
include ../@.Spindle/environment.mk

.PHONY: all
all: $(EFFECT_NAME).pef $(EFFECT_NAME).prg

# Run the standalone version
.PHONY: test
test: main.test.prg
	$(X64) $<

# Run standalone version in debugger
.PHONY: debug
debug: main.test.prg
	$(DBG) $<

# Make as standalone version
main.test.prg: main.asm
	$(KA) -debugdump -o $@ $<

# Make as spindle part
$(EFFECT_NAME).efo:	main.asm
	$(KA) -define AS_SPINDLE_PART -binfile -o $@ $<

%.prg: %.pef
	$(PEF2PRG) -o $@ -m $(EFFECT_NAME).vs $<

.PHONY: clean
clean:: clean_artifacts
	$(DEL) *.sym
	$(DEL) .source.txt
	$(DEL) *.dbg
	$(DEL) *.vs
	$(DEL) splitefocmd_$(EFFECT_NAME).txt

.PHONY: clean_artifacts
clean_artifacts:
	$(DEL) *.efo
	$(DEL) *.pef
	$(DEL) *.prg
	$(DEL) $(EFFECT_NAME)_part*.bin

