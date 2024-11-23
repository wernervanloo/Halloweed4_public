# Set various globally needed variables.

# Java, KickAssembler, Vice
JAVA := java
KA_JAR := ~/Commodore64/Dev/KickAssembler/KickAss.jar
X64 := x64sc
KA := $(JAVA) -jar $(KA_JAR)
DBG := "/Applications/Retro Debugger.app/Contents/MacOS/Retro Debugger"

# Spindle
SPINDLE_BIN_DIR := ~/Commodore64/Dev/spindle-3.1/src/
MKPEF := $(SPINDLE_BIN_DIR)mkpef -v
PEF2PRG := $(SPINDLE_BIN_DIR)pef2prg
PEFCHAIN := $(SPINDLE_BIN_DIR)pefchain
PEFFLAGS := -v -w

# Miscellaneous
DEL := rm -f
