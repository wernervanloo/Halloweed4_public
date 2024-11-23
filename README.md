## introduction

This is the sourcecode for Halloweed 4, a C64 demo that competed in the [Transmission64](https://transmission64.com/) 2024 demo competition that was held on 23-11-2024. Halloweed 4 can be downloaded from [CSDb](https://csdb.dk/release/?id=247767)

Halloweed 4 was made by:
- Werner van Loo (code)
- Michel de Bree (music, makefile magic)
- Nikaj Eijk (graphics)
- John de Bruin (graphics, fadepattern and help)
- Daniel Kottmair (graphics last part and banners in the AGSP part)

using LFT's excellent Spindle trackmo system.

### prerequisites

To compile the demo and generate a .d64 you will need the following installed on your pc:

- [Kickassembler](https://theweb.dk/KickAssembler/) v5.25 (or higher) by Mads Nielsen,
- Java in order to run Kickassembler. The current version of Kickassembler needs at least version 8.0,
- the demo is linked with the [Spindle v3.1 trackmo system by LFT](https://www.linusakesson.net/software/spindle/v3.php),
- everything that is needed to run make on your system

### environment

In order to compile the demo you need to provide the locations of these dependencies in the `environment.mk` file, which is in the `@.Spindle` directory.

You will need to set at least the locations for the following variables: 
- JAVA (the java executable)
- KA_JAR (the kickassembler jar file)
- SPINDLE_BIN_DIR (the directory that holds the spindle binaries, in my case for Windows)

When compiling the demo, it will start the demo in VICE at the end. You will have to set the X64 variable correctly for this.

### compiling

The demo-spanning files are in the `@.Spindle` directory. You can compile the demo with `make vice` using the makefile in the `@.Spindle` directory. The separate sourcefiles will be compiled and linked with Spindle, according to the `script.txt` file.

Spindle will also generate a .d64 with dirart from the d64 that is located in the dirart directory.

### running parts separately

Most parts (except some which are only used for linking) can be compiled and run as a separate part. I do not use the facilities that Spindle supplies for this, but compile them directly from Visual Code with the [8-bit retro studio extension](https://marketplace.visualstudio.com/items?itemName=paulhocker.kick-assembler-vscode-ext) by Paul Hocker. 
If you compile parts separately with the makefiles in the directories, you will not get a runnable prg file since the parts will be compiled with the AS_SPINDLE_PART flag and components needed to run standalone will be missing.

You should be able to compile the parts separately using kickassembler from the commandline and generate a runnable .prg file like so :

`java -jar KickAss.jar main.asm`

Some compiled parts will need an emulated cartridge in X64.exe in order to be able to load correctly, since they can occupy memory ranges above $d000.

### UFLI-max part

The last part uses a couple of .asm files in the /memory subdirectory that instructs where bytes go in the memory in order to correctly display the UFLI picture. These files are generated by the UFLI tool, which is written in C++. The UFLI tool directory contains a Visual Studio project for compiling the tool. You do not need the UFLI tool to compile the demo, but if you run it it can take a while until the files are generated. The tool has to search for a 'solution' to the 6-sprites-over-fli sudoko since the result is not hardcoded in the tool.
