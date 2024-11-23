#include <iostream>
#include <fstream>
#include <iomanip>
#include <time.h>

using namespace std;

#include <stdio.h>
#include <stdlib.h>
#define findlines 124      //number of lines to find a solution for
#define onlywritemax false //if true, only a solution with the max number of lines (findlines) is written to disk
#define sprites 6          //number of sprites to take care of extra nops..

int d800colors = 0x9c00;
int speedcode = 0x1000;

//testfile :
//   2 loadbytes
//1024 d800 colors           
//8192 bitmap
//8192 screencolors

int d800offset = 2;  // not used in AFLI
int screenoffset = 2;
int bitmapoffset = 2 + 8192;

//nice startvalues :
//y=#$30  (reset $d011 always)
//dd00 = 0
//dd02 = ?1
//a=#$30,#$31,#$38,#$39,#$b0,#$b1,#$b8,#$b9

// d011 ok values :
// ---------------------------
// 7,6,5,4,3,2,1,0
// bit 2,1,0 == line&7
// bit 3 = dc ,(24/25 columns)
// bit 4 = dc ,screen on/off 
// bit 5 = 1  ,bitmap on
// bit 6 = 0  ,ecm mode
// bit 7 = dc ,bit 8 of d012
// care =& 0110 0111

//============
//global stuff
//============

int maxlines = 0;
int code[(3 + 4 + 6)*findlines + 32];
int d011code[4] = { 0x8d,0x8e,0x8c,0x8f };
int d011len[4] = { 3,   3,   3,   3 };
int d018code[11][19] = { {0x18,0x38,0xa9, 0x8d,0x8e,0x8c,0x8f, 0xee,0xce, 0x0e,0x2e,0x4e,0x6e, 0x0f,0x2f,0x4f,0x6f, 0xcf,0xef},
					  {0xea,0xea,0x00, 0x00,0x00,0x00,0x00, 0x00,0x00, 0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00, 0x00,0x00},
					  {0xea,0xea,0x18, 0x00,0x00,0x00,0x00, 0x00,0x00, 0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00, 0x00,0x00},
					  {0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea},
					  {0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea},
					  {0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea},
					  {0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea},
					  {0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea},
					  {0x24,0x24,0xea, 0xea,0xea,0xea,0xea, 0x24,0x24, 0x24,0x24,0x24,0x24, 0x24,0x24,0x24,0x24, 0x24,0x24},
					  {0xea,0xea,0x24, 0x24,0x24,0x24,0x24, 0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea,0xea,0xea, 0xea,0xea},
					  {0x00,0x00,0xea, 0xea,0xea,0xea,0xea, 0x00,0x00, 0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00, 0x00,0x00} };
int d018len[19] = { 10,  10,  11,   11,  11,  11,  11,   10,  10,   10,  10,  10,  10,   10,  10,  10,  10,   10,  10 };
int d018lenmin[19] = { 1,   1,   3,    3,   3,   3,   3,    3,   3,    3,   3,   3,   3,    3,   3,   3,   3,    3,   3 }; //minimum length of d018 code  
int d018write[19] = { 10,  10,   1,    1,   1,   1,   1,    1,   1,    1,   1,   1,   1,    1,   1,   1,   1,    1,   1 }; //where to write the register in the d018 code
int spritevslen[7] = { 0, 3,4,5,6,7,8 }; //how many bytes to remove depending on the # of sprites
int vicreg[3] = { 0xd3d8,0xddf2,0xddf0 };
int memory[65536];
int memory2[65536];
int memorycopy[65536];
int memory2copy[65536];
int testpic[65536];
char* flidata;
char* spritefile;
int spritedata[65536];

int d018tab[findlines]; //storage for solution
int dd00tab[findlines];
int dd02tab[findlines];
int d011tab[findlines];
int atab[findlines]; //storage for solution
int xtab[findlines];
int ytab[findlines];
int carrytab[findlines];
int ctrop1tab[findlines];
int ctrop2tab[findlines];
int victab[findlines];

int spritestructure[4 * 42 + 40][3] = { { 5, 6, 7},  //00
								 { 5, 6, 7},
								 { 8, 9,10},  //02
								 { 8, 9,10},
								 {11,12,13},  //04
								 {11,12,13},
								 {14,15,16},  //06
								 {14,15,16},
								 {17,18,19},  //08
								 {17,18,19},
								 {20,21,22},  //0a
								 {20,21,22},
								 {23,24,25},  //0c
								 {23,24,25},
								 {26,27,28},  //0e
								 {26,27,28},
								 {29,30,31},  //10
								 {29,30,31},
								 {32,33,34},  //12
								 {32,33,34},
								 {35,36,37},  //14
								 {35,36,37},
								 {38,39,40},  //16
								 {38,39,40},
								 {41,42,43},  //18
								 {41,42,43},
								 {44,45,46},  //1a
								 {44,45,46},
								 {47,48,49},  //1c
								 {47,48,49},
								 {50,51,52},  //1e
								 {50,51,52},
								 {53,54,55},  //20
								 {53,54,55},
								 {56,57,58},  //22
								 {56,57,58},
								 {59,60,61},  //24
								 {59,60,61},
								 {62,63, 0},  //26
								 {62,63, 0},

								 { 1, 2, 3},  //28
								 { 1, 2, 3},
								 { 4, 5, 6},  //2a
								 { 4, 5, 6},
								 { 7, 8, 9},  //2c
								 { 7, 8, 9},
								 {10,11,12},  //2e
								 {10,11,12},
								 {13,14,15},  //30
								 {13,14,15},
								 {16,17,18},  //32
								 {16,17,18},
								 {19,20,21},  //34
								 {19,20,21},
								 {22,23,24},  //36
								 {22,23,24},
								 {25,26,27},  //38
								 {25,26,27},
								 {28,29,30},  //3a
								 {28,29,30},
								 {31,32,33},  //3c
								 {31,32,33},
								 {34,35,36},  //3e
								 {34,35,36},
								 {37,38,39},  //40
								 {37,38,39},
								 {40,41,42},  //42
								 {40,41,42},
								 {43,44,45},  //44
								 {43,44,45},
								 {46,47,48},  //46
								 {46,47,48},
								 {49,50,51},  //48
								 {49,50,51},
								 {52,53,54},  //4a
								 {52,53,54},
								 {55,56,57},  //4c
								 {55,56,57},
								 {58,59,60},  //4e
								 {58,59,60},
								 {61,62,63},  //50
								 {61,62,63},

								 { 0, 1, 2},  //52
								 { 0, 1, 2},
								 { 3, 4, 5},  //54
								 { 3, 4, 5},
								 { 6, 7, 8},  //56
								 { 6, 7, 8},
								 { 9,10,11},  //58
								 { 9,10,11},
								 {12,13,14},  //5a
								 {12,13,14},
								 {15,16,17},  //5c
								 {15,16,17},
								 {18,19,20},  //5e
								 {18,19,20},
								 {21,22,23},  //60
								 {21,22,23},
								 {24,25,26},  //62
								 {24,25,26},
								 {27,28,29},  //64
								 {27,28,29},
								 {30,31,32},  //66
								 {30,31,32},
								 {33,34,35},  //68
								 {33,34,35},
								 {36,37,38},  //6a
								 {36,37,38},
								 {39,40,41},  //6c
								 {39,40,41},
								 {42,43,44},  //6e
								 {42,43,44},
								 {45,46,47},  //70
								 {45,46,47},
								 {48,49,50},  //72
								 {48,49,50},
								 {51,52,53},  //74
								 {51,52,53},
								 {54,55,56},  //76
								 {54,55,56},
								 {57,58,59},  //78
								 {57,58,59},
								 {60,61,62},  //7a
								 {60,61,62},

								 { 0, 1, 2},  //7c
								 { 0, 1, 2},
								 { 3, 4, 5},  //7e
								 { 3, 4, 5},
								 { 6, 7, 8},  //80
								 { 6, 7, 8},
								 { 9,10,11},
								 { 9,10,11},
								 {12,13,14},
								 {12,13,14},
								 {15,16,17},
								 {15,16,17},
								 {18,19,20},
								 {18,19,20},
								 {21,22,23},
								 {21,22,23},
								 {24,25,26},
								 {24,25,26},
								 {27,28,29},
								 {27,28,29},
								 {30,31,32},
								 {30,31,32},
								 {33,34,35},
								 {33,34,35},
								 {36,37,38},
								 {36,37,38},
								 {39,40,41},
								 {39,40,41},
								 {42,43,44},
								 {42,43,44},
								 {45,46,47},
								 {45,46,47},
								 {48,49,50},
								 {48,49,50},
								 {51,52,53},
								 {51,52,53},
								 {54,55,56},
								 {54,55,56},
								 {57,58,59},
								 {57,58,59},
								 {60,61,62},
								 {60,61,62},

								 { 0, 1, 2},
								 { 0, 1, 2},
								 { 3, 4, 5},
								 { 3, 4, 5},
								 { 6, 7, 8},
								 { 6, 7, 8},
								 { 9,10,11},
								 { 9,10,11},
								 {12,13,14},
								 {12,13,14},
								 {15,16,17},
								 {15,16,17},
								 {18,19,20},
								 {18,19,20},
								 {21,22,23},
								 {21,22,23},
								 {24,25,26},
								 {24,25,26},
								 {27,28,29},
								 {27,28,29},
								 {30,31,32},
								 {30,31,32},
								 {33,34,35},
								 {33,34,35},
								 {36,37,38},
								 {36,37,38},
								 {39,40,41},
								 {39,40,41},
								 {42,43,44},
								 {42,43,44},
								 {45,46,47},
								 {45,46,47},
								 {48,49,50},
								 {48,49,50},
								 {51,52,53},
								 {51,52,53},
								 {54,55,56},
								 {54,55,56},
								 {57,58,59},
								 {57,58,59},
								 {60,61,62},
								 {60,61,62} };

int spritecheck[64][64]; //check to see if spritedata is still original..
int spriteplot[findlines][3][sprites];

int bankbyte[4] = { 0x0f,0x55,0xaa,0xf0 };
int astart;
int xstart;
int ystart;
int dd02start;
int carrystart;

//add
//?) allow use of TAS (9b) -  S:=A&X , {adr}:=S&H (aby) => adr=a&x&h not much different from sax
//?) allow use of SHY (9c) - {adr}:=Y&H (abx) -> maybe a good way to write $cf to d001?
//?) allow use of SHX (9e) - {adr}:=X&H (aby) (vice seems to be doing &(H+1)) so : and #$d1... hoxs also.. at least for writes to into h=$d0
//note: sometimes the &H drops off. Also page boundary crossing will not work as expected (the bank where the value is stored may be equal to the value stored).
//x=22 & d1 = $00!!
//y=3e & d1 = $10!!
//x=22 & d2 = $02!!
//y=3e & d2 = $12!!
// x=22 & d3 = $02
// y=3e & d3 = $12
// x=22 & d4 = $00
//y=3e & d4 = $14!!
//
// x=01, shy $cf00,x -> goes to cf00! &d0
//
// x=01, shy $d000,x -> goes to d000! &d1
// x=01, shy $d100,x -> goes to d000! &d2 
// x=01, shy $d200,x -> goes to d000! &d3
// x=01, shy $d300,x -> goes to d000! &d4
//
//=> do not use pagecrossing..
// x=10, shy $cef0,x -> goes to d000! &d0 -> bank = value written?!!!
// x=10, shy $cff0,x -> goes to d000! &d1
// x=10, shy $d0f0,x -> goes to d000! &d2
// x=10, shy $d1f0,x -> goes to d000! &d3
// x=10, shy $d2f0,x -> does NOT goes to d000!
//
//we can write to $d000,$d040,$d080,$d0c0,$d100, up to $d3c0 -> d018 last register in page = d0d8 -> y/x is max d8
//we can write to $dd00,$dd10,$dd20, up to $ddf0 -> dd02 last register in page is $ddf2 ($ddf0)   -> y/x is max (f2/f0)
//
//writing to $dd00/$dd02 -> &de
//x=22 & de =  2 (leuk!)
//y=3e & de = 1e = same bank as 2, maybe nice for dd02





//?) allow for update x,y if line&7==7 =>> lots of possibilities -> :( or :) ?
//?) allow lsr/asl/etc for dd00..
//?) possibility to multiplex? in line&7==7?
//?) add decimal mode? we can set/unset it together with lda #. (brrr!)
//?) we could use 2 spritestructures -> starting with 1,2,3 or 5,6,7.. -> extra line for sprite & maybe different solution..

void prepmemory()
{
	int ctr;
	int ctr2;

	//clear c64 memory
	for (ctr = 0; ctr < 65536; ctr++) {
		memory[ctr] = 0;
		memory2[ctr] = 0;
	}
	//occupy 0page & stack,200 and 300
	for (ctr = 0; ctr < 256; ctr++) {
		memory[ctr] = '0';
		memory[0x100 + ctr] = 's';
		memory[0x200 + ctr] = '2';
		memory[0x300 + ctr] = '3';
	}
	//occupy $1000-$1fff and $9000-$9fff
	for (ctr = 0; ctr < 0x1000; ctr++) {
		memory[0x1000 + ctr] = 'x';
		memory[0x9000 + ctr] = 'x';
	}
	//put d800 colors into memory2
	//for (ctr=0;ctr<1000;ctr++){
	//  memory2[d800colors+ctr]=testpic[d800offset+ctr];
	//}//for

	//clear spritecheck
	for (ctr = 0; ctr < 64; ctr++) {
		for (ctr2 = 0; ctr2 < 64; ctr2++) {
			spritecheck[ctr][ctr2] = 0;
		}//for ctr2
	}//for ctr                 
}

void WriteFiles(int line, int codeptr2)
{
	ofstream out2;
	out2.open("../bin/code.prg", ios::out | ios::binary);
	out2 << char(0);
	out2 << char(16);
	for (int filectr = 0; filectr < codeptr2; filectr++) { out2 << char(code[filectr]); }
	//out2 << char(0x60);
	out2.close();

	out2.open("../bin/memory.txt", ios::out | ios::binary);
	out2 << "line : " << line << char(0x0D);
	for (int filectr = 0; filectr < 65536; filectr++)
	{
		if ((filectr & 0xff) == 0x00)
		{
			out2 << std::setfill('0') << std::setw(4) << hex << filectr << ':';
		}
		out2 << char(memorycopy[filectr]);
		if ((filectr & 0xff) == 0xff) out2 << char(0x0D);
	}
	out2.close();

	// determine last byte
	int lastbyte = 0xffff;
	for (lastbyte = 0xffff; lastbyte >= 0; lastbyte--)
	{
		if (memorycopy[lastbyte] > 0) break;
	}
	out2.open("../bin/test.prg", ios::out | ios::binary);
	out2 << char(0);
	out2 << char(4);
	for (int filectr = 0x0400; filectr < (65536 - 2); filectr++) { out2 << char(memory2copy[filectr]); }
	out2.close();

	out2.open("../bin/screen.bin", ios::out | ios::binary);
	for (int filectr = 0; filectr < line; filectr++) { out2 << char(((dd02tab[filectr] & 0x03) << 6) + ((d018tab[filectr] & 0xf0) >> 2)); }
	out2.close();

	out2.open("../bin/spriteoffset.bin", ios::out | ios::binary);
	for (int filectr = 0; filectr < line; filectr++) { out2 << char(spritestructure[filectr][0]); }
	out2.close();
}

void nextline(int line, int a, int x, int y, int carry, int d018, int dd00, int dd02, int codeptr, int codeptr2)
{
	int ctrop1;
	int ctrop2;
	int ctrop2start;
	int ctrop2end;
	int vic;
	int change;
	int anew, xnew, ynew, carrynew, codeptrnew;

	int filectr;
	int bank;
	int colors;
	int bitmap;

	int copybytes;

	int screen;
	int sprite;
	int occupied;
	int spritebyte;
	int spriteimage;
	int spriteline;
	int spritepointer;
	bool imageused;
	bool problem = false;

	bool memused = false;
	bool spritepointersset = false;

	d018tab[line] = d018;
	dd02tab[line] = dd02;
	dd00tab[line] = dd00;
	atab[line] = a;
	xtab[line] = x;
	ytab[line] = y;
	carrytab[line] = carry;

	//==========================================================
	//check if the memory is available for the new unique data..
	//==========================================================

	bank = 0x4000 * (((dd00 ^ 3) & (dd02)) & 3);
	colors = (d018 & 0xf0) * 0x040 + bank + (line / 8) * 40;
	bitmap = (d018 & 0x08) * 0x400 + bank + (line / 8) * 320 + (line & 7);
	screen = (colors & 0xfc00) / 0x400;
	if (spritecheck[spritestructure[line][0]][screen] != 0) { memused = true; problem = true; }
	if (spritecheck[spritestructure[line][1]][screen] != 0) { memused = true; problem = true; }
	if (spritecheck[spritestructure[line][2]][screen] != 0) { memused = true; problem = true; }

	//if (line>125 && (bank==0x4000 || bank==0x8000)){memused=true;}  //quick hack :P

	for (filectr = 0; filectr < 40; filectr++) {
		if (memory[colors + filectr] != 0) { memused = true; filectr = 40; }
		if (memory[bitmap + filectr * 8] != 0) { memused = true; filectr = 40; }
	}

	//===============================================================
	//check if the space for the sprite pointers is not occupied yet.
	//===============================================================

	for (filectr = 2; filectr < 8; filectr++) {
		if (!((memory[((colors & 0xfc00) | 0x3f8) + filectr] == 0) || (memory[((colors & 0xfc00) | 0x3f8) + filectr] == 'p'))) {
			memused = true;
			filectr = 8;
		}//if
	}//for  

	if (line > maxlines && !problem) {
		maxlines = line;
		printf("max lines found : %d @ a=%i,x=%i,y=%i,dd02=%i\n", maxlines, astart, xstart, ystart, dd02start);

		//check if there's space for sprite images..
		problem = false;

		if ((onlywritemax == false || line == findlines)) {
			//copy memory
			for (filectr = 0; filectr < 0x10000; filectr++) {
				memorycopy[filectr] = memory[filectr];
				memory2copy[filectr] = memory2[filectr];
			}//for

			//check all screens to see if we're using sprites there..                   
			for (screen = 0; screen < 64; screen++) {
				if (memorycopy[screen * 0x400 + 0x3fa] == 'p') {
					//sprite pointers here... so we need to allocate #sprites number of images.
					occupied = 0;
					for (sprite = 0; sprite < sprites; sprite++) {
						for (spriteimage = 0; (spriteimage < 256 && occupied <= sprite); spriteimage++) {
							imageused = false;
							for (spritebyte = 0; spritebyte < 64; spritebyte++) {
								if (memorycopy[spritebyte + spriteimage * 64 + ((screen / 16) * 0x4000)] != 0) {
									imageused = true;
									spritebyte = 64;
								}//if 
							}//for spritebyte
							if (!imageused) {
								memory2copy[screen * 0x400 + 0x3fa + sprite] = spriteimage;
								occupied++;
								for (spritebyte = 0; spritebyte < 64; spritebyte++) {
									memorycopy[spritebyte + spriteimage * 64 + ((screen / 16) * 0x4000)] = 'S';  //occupy space
								}//for               
							}//if !imageused                                                 
						}//for spriteimage                                                                                          
					}//for sprite
					if (occupied != sprites && !problem) {
						printf("not enough space for the sprite images in bank %i\n", (screen / 16));
						problem = true;
					}//if
				}//if  
			}//for                     

			//===========================
			//put sprite data into memory
			//===========================

			for (spriteline = 0; spriteline < line; spriteline++) {
				for (sprite = 0; sprite < sprites; sprite++) {
					//in what screen can we find the spritepointers?
					spritepointer = memory2copy[(d018tab[spriteline] & 0xf0) * 0x040 + 0x4000 * (((dd00tab[spriteline] ^ 3) & (dd02tab[spriteline])) & 3) + 0x3fa + sprite];
					int membyte = 0x4000 * (((dd00tab[spriteline] ^ 3) & (dd02tab[spriteline])) & 3) + spritepointer * 0x40;

					spritebyte = spritestructure[spriteline][0];
					memory2copy[membyte + spritebyte] = spritedata[spriteline * 6 * 3 + sprite * 3 + 0];
					memorycopy[membyte + spritebyte] = 'S';
					spriteplot[spriteline][0][sprite] = 0x4000 * (((dd00tab[spriteline] ^ 3) & (dd02tab[spriteline])) & 3) + spritepointer * 0x40 + spritebyte;

					spritebyte = spritestructure[spriteline][1];
					memory2copy[membyte + spritebyte] = spritedata[spriteline * 6 * 3 + sprite * 3 + 1];
					memorycopy[membyte + spritebyte] = 'S';
					spriteplot[spriteline][1][sprite] = 0x4000 * (((dd00tab[spriteline] ^ 3) & (dd02tab[spriteline])) & 3) + spritepointer * 0x40 + spritebyte;

					spritebyte = spritestructure[spriteline][2];
					memory2copy[membyte + spritebyte] = spritedata[spriteline * 6 * 3 + sprite * 3 + 2];
					memorycopy[membyte + spritebyte] = 'S';
					spriteplot[spriteline][2][sprite] = 0x4000 * (((dd00tab[spriteline] ^ 3) & (dd02tab[spriteline])) & 3) + spritepointer * 0x40 + spritebyte;
				}//for sprite    
			}//for
		}//if findlines                    

		//===============
		//output codefile
		//===============

		if ((onlywritemax == false || line == findlines) && !problem) {
			WriteFiles(line, codeptr2);
		}//if onlywritemax

		if (problem) { maxlines = 0; } //this solution doesnt count..

	}//if

	if (memused || line == findlines || maxlines == findlines) { ; }
	else {

		//==============
		// occupy memory
		//==============

		screen = (colors & 0xfc00) / 0x400;
		spritecheck[(spritestructure[line][0])][screen] = 1;
		spritecheck[(spritestructure[line][1])][screen] = 1;
		spritecheck[(spritestructure[line][2])][screen] = 1;

		for (filectr = 0; filectr < 40; filectr++) {
			memory[colors + filectr] = 'c';
			memory[bitmap + filectr * 8] = 'b';

			memory2[colors + filectr] = flidata[screenoffset + (line & 7) * 0x400 + ((line / 8) * 40) + filectr];
			memory2[bitmap + filectr * 8] = flidata[bitmapoffset + (line & 7) + ((line / 8) * 320) + filectr * 8];
		}//for

		//==========================================================
		//check if we need to occupy space for the sprite pointers..
		//==========================================================

		if (memory[((colors & 0xfc00) | 0x3fa)] == 0) {
			spritepointersset = true;
			for (filectr = 2; filectr < 8; filectr++) { memory[((colors & 0xfc00) | 0x3f8) + filectr] = 'p'; }//for                                        
		}//if

		//============================================
		//step 1 : select an opcode for the d011 write
		//============================================

		for (ctrop1 = 0; ctrop1 < 4; ctrop1++) {
			if ((ctrop1 == 0) && (a & 0x67) == (0x20 + (line & 7))) { d011tab[line] = a; }    //sta $d011
			else { if (ctrop1 == 0) { ctrop1++; } }
			if ((ctrop1 == 1) && (x & 0x67) == (0x20 + (line & 7))) { d011tab[line] = x; }    //stx $d011
			else { if (ctrop1 == 1) { ctrop1++; } }
			if ((ctrop1 == 2) && (y & 0x67) == (0x20 + (line & 7))) { d011tab[line] = y; }    //sty $d011
			else { if (ctrop1 == 2) { ctrop1++; } }
			if ((ctrop1 == 3) && ((a&x) & 0x67) == (0x20 + (line & 7))) { d011tab[line] = a & x; }  //sax $d011
			else { if (ctrop1 == 3) { ctrop1++; } }

			//============================================
			//step 2 : select an opcode for the d018 write
			//============================================

			if (ctrop1 < 4) {

				//===========================
				//write d011 opcode to memory
				//===========================

				code[codeptr] = d011code[ctrop1];
				code[codeptr + 1] = 0x11;   //0x11
				code[codeptr + 2] = 0xd0;   //0xd0

				for (vic = 0; vic < 3; vic++) {

					//if line&7 == 7 -> allow to skip write to d018/dd02
					ctrop2start = 3;
					ctrop2end = 19;

					if ((line & 0x07) == 7 && vic == 0) { ctrop2start = 2; }

					//for dd00 : only acceptable opcodes are sta,stx,sty,sax,inc,dec
					if (vic == 2) { ctrop2start = 3; ctrop2end = 9; }

					for (ctrop2 = ctrop2start; ctrop2 < ctrop2end; ctrop2++) {
						ctrop1tab[line] = ctrop1;
						ctrop2tab[line + 1] = ctrop2;
						victab[line + 1] = vic;

						if (vic == 0) { change = (d018 | 1); }
						if (vic == 1) { change = dd02; }
						if (vic == 2) { change = dd00; }
						anew = a;
						xnew = x;
						ynew = y;
						carrynew = carry;

						switch (ctrop2)
						{
						case 0:
							carrynew = 0;
							break;
						case 1:
							carrynew = 1;
							break;
						case 2:
							//lda #
							break;
						case 3:
							change = a;   //sta
							break;
						case 4:
							change = x;   //stx   
							break;
						case 5:
							change = y;   //sty  
							break;
						case 6:
							change = a & x; //sax   
							break;
						case 7:
							change = (change + 1) & 0xff; //inc
							break;
						case 8:
							change = (change - 1) & 0xff; //dec   
							break;
						case 9:
							carrynew = change / 128;    //asl
							change = (change * 2) & 255;
							break;
						case 10:
							carrynew = change / 128;    //rol
							change = (change * 2 + carry) & 255;
							break;
						case 11:
							carrynew = change & 1;      //lsr
							change = change / 2;
							break;
						case 12:
							carrynew = change & 1;      //ror
							change = (change / 2) + (carry * 0x80);
							break;
						case 13:
							carrynew = change / 128;    //slo
							change = (change * 2) & 255;
							anew = a | change;
							break;
						case 14:
							carrynew = change / 128;    //rla
							change = (change * 2 + carry) & 255;
							anew = a & change;
							break;
						case 15:
							carrynew = change & 1;      //sre
							change = (change / 2);
							anew = a ^ change;
							break;
						case 16:
							carrynew = change & 1;      //rra
							change = (change / 2 + carry * 128);
							anew = a + change + carrynew;
							carrynew = anew / 256;
							anew = anew & 255;
							break;
						case 17:
							change = (change - 1) & 0xff;  //dcp
							carrynew = 0;
							if (a >= change) { carrynew = 1; }
							break;
						case 18:
							change = (change + 1) & 255;   //isc
							anew = a - change - 1 + carry;
							carrynew = 1;
							if (anew < 0) { carrynew = 0; anew = anew + 256; }
							break;
						}//switch    

						anew = anew & 0xff;
						change = change & 0xff;

						copybytes = d018len[ctrop2] - spritevslen[sprites];
						if (copybytes < d018lenmin[ctrop2]) { copybytes = d018lenmin[ctrop2]; }

						for (filectr = 0; filectr < copybytes; filectr++) {
							code[codeptr + 3 + filectr] = d018code[filectr][ctrop2];
						}//for
						codeptrnew = codeptr + d011len[ctrop1] + copybytes;

						if (ctrop2 == 2) {
							for (anew = 0; anew < 512; anew++) {
								//write argument for lda #
								carrynew = (a / 256) & 1;
								code[codeptr + 4] = anew & 0xff;
								code[codeptr + 5] = 0x18 + carrynew * 0x20;
								nextline(line + 1, anew & 0xff, xnew, ynew, carrynew, d018, dd00, dd02, codeptrnew, codeptr + d011len[ctrop1]);
							}//for
						}//if   
						else {
							code[codeptr + 3 + d018write[ctrop2]] = vicreg[vic] & 255;
							code[codeptr + 4 + d018write[ctrop2]] = vicreg[vic] / 256;

							if (vic == 0) { nextline(line + 1, anew, xnew, ynew, carrynew, change, dd00, dd02, codeptrnew, codeptr + d011len[ctrop1]); }
							if (vic == 1) { nextline(line + 1, anew, xnew, ynew, carrynew, d018, dd00, change, codeptrnew, codeptr + d011len[ctrop1]); }
							if (vic == 2) { nextline(line + 1, anew, xnew, ynew, carrynew, d018, change, dd02, codeptrnew, codeptr + d011len[ctrop1]); }
						}//else      
					}//ctrop2
				}//vic
			}//if
		}//for ctrop1

		//we're going back in the recursion, so we clear the memory again..

		screen = (colors & 0xfc00) / 0x400;
		spritecheck[(spritestructure[line][0])][screen] = 0;
		spritecheck[(spritestructure[line][1])][screen] = 0;
		spritecheck[(spritestructure[line][2])][screen] = 0;

		for (filectr = 0; filectr < 40; filectr++) {
			memory[colors + filectr] = 0;
			memory2[colors + filectr] = 0;
			memory[bitmap + filectr * 8] = 0;
			memory2[bitmap + filectr * 8] = 0;
		}//for

		//check if we need to free the space for the sprite pointers..

		if (spritepointersset) {
			for (filectr = 2; filectr < 8; filectr++) { memory[((colors & 0xfc00) | 0x3f8) + filectr] = 0; }//for                                        
		}//if spritepointersset

	}//else
}// void nextline

static char * ReadAllBytes(const char * filename, int * read)
{
	ifstream ifs(filename, ios::binary | ios::ate);
	ifstream::pos_type pos = ifs.tellg();

	// What happens if the OS supports really big files.
	// It may be larger than 32 bits?
	// This will silently truncate the value/
	int length = pos;

	// Manuall memory management.
	// Not a good idea use a container/.
	char *pChars = new char[length];
	ifs.seekg(0, ios::beg);
	ifs.read(pChars, length);

	// No need to manually close.
	// When the stream goes out of scope it will close the file
	// automatically. Unless you are checking the close for errors
	// let the destructor do it.
	ifs.close();
	*read = length;
	return pChars;
}

void convertToSprites()
{
	for (int x = 0; x < (6 * 3); x++)
	{
		for (int y = 0; y < 166; y++)
		{
			int charline = y >> 3;
			int yoffset = y & 7;
			int msb = spritefile[2 + charline * 320 + yoffset + x * 8 * 2 + 0];
			int lsb = spritefile[2 + charline * 320 + yoffset + x * 8 * 2 + 8];

			// %aabbccdd -> %abcd
			msb = (msb & 0x80) + ((msb & 0x20) << 1) + ((msb & 0x08) << 2) + ((msb & 0x02) << 3);
			lsb = (lsb & 0x01) + ((lsb & 0x04) >> 1) + ((lsb & 0x10) >> 2) + ((lsb & 0x40) >> 3);
			spritedata[y * 6 * 3 + x] = (msb + lsb); // ^ 255;
		}
	}
	return;
}

int main(int argc, char *argv[])
{
	//define c64 registers
	int a;
	int x;
	int y;
	int carry;
	int d018;
	int dd00;
	int dd02;
	int d011;

	int ctr;        //counter for d011-startvalue
	int codeptr;

	// read test picture
	int length;
	flidata = ReadAllBytes("../bin/convert_AFLI.prg", &length);
	printf("%d fli bytes read\n", length);

	spritefile = ReadAllBytes("../deekay/sprites.art", &length);
	printf("%d sprite bytes read\n", length);

	convertToSprites();

	// clear code

	for (ctr = 0; ctr < (3 + 4 + 6)*findlines; ctr++) { code[ctr] = 0; }

	printf("starting search for solution\n");

	prepmemory();
	d018 = 0x0f;
	dd00 = 0;
	d011 = 0;

	code[0] = 0xa9; //lda #
	code[2] = 0x8d; //sta $
	code[3] = 0x18; //d018
	code[4] = 0xd0;

	code[5] = 0xa9; // lda #
	code[7] = 0x8d; // sta $
	code[8] = 0x00; // dd00
	code[9] = 0xdd;

	code[10] = 0xa9; // lda #
	code[12] = 0x8d; // sta $
	code[13] = 0x02; // dd02
	code[14] = 0xdd;

	code[15] = 0xa9; // lda #
	code[17] = 0xa2; // ldx #
	code[19] = 0xa0; // ldy #

	codeptr = 22;

	// nice startvalues -> a=20,x=22,y=3e,dd02=12

//no solution for x=cf (rest free)
//no solution for a=cf (rest free)        
//solution for y=cf?

//loop for the startvalues
	for (a = 0x20; a < 512 && maxlines < findlines; a++) {
		carry = (a / 256) & 1;
		for (x = 0x22; x < 0x100 && maxlines < findlines; x++) {
			for (y = 0x3e; y < 0x100 && maxlines < findlines; y++) {
				for (dd02 = 0x12; dd02 < 256 && maxlines < findlines; dd02++) {

					astart = a & 0xff;
					xstart = x;
					ystart = y;
					dd02start = dd02;

					code[1] = d018;
					code[6] = dd00;
					code[11] = dd02;
					code[16] = a;
					code[18] = x;
					code[20] = y;
					code[21] = 0x18 + carry * 0x20;

					//look for solutions

					nextline(0, a & 255, x, y, carry, d018, dd00, dd02, codeptr, codeptr);

				}//dd02
			}//y
			printf("x=%d finished\n", x);
		}//x
		printf("a=%d finished\n", a);
	}//a

	return 0;
}
