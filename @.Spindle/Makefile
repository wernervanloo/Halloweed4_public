# Set various globally-used variables.
include environment.mk

all: halloweed4.d64

.PHONY: *.dir

1a_bliksem.pef:	
	make -C "../1a. bliksem" 1a_bliksem.pef
	mv "../1a. bliksem/1a_bliksem.pef" .

1b_logo.pef:	
	make -C "../1b. logo" 1b_logo.pef
	mv "../1b. logo/1b_logo.pef" .

1c_copylogo.pef:
	make -C "../1c. copylogo" 1c_copylogo.pef
	mv "../1c. copylogo/1c_copylogo.pef" .

1d_freemem.pef:
	make -C "../1d. freemem" 1d_freemem.pef
	mv "../1d. freemem/1d_freemem.pef" .

1e_bitmapscroll.pef:	
	make -C "../1e. bitmapscroll" 1e_bitmapscroll.pef
	mv "../1e. bitmapscroll/1e_bitmapscroll.pef" .

1f_fade.pef:
	make -C "../1f. fade" 1f_fade.pef
	mv "../1f. fade/1f_fade.pef" .

3dbars.pef:
	make -C "../2. 3d bars" 3dbars.pef
	mv "../2. 3d bars/3dbars.pef" .

agsp.pef:
	make -C "../3. agsp multimax" agsp.pef
	mv "../3. agsp multimax/agsp.pef" .

rasterfade1.pef:
	make -C "../4. rasterfade 1" rasterfade1.pef
	mv "../4. rasterfade 1/rasterfade1.pef" .

oogbal.pef:
	make -C "../5a. oogbal" oogbal.pef
	mv "../5a. oogbal/oogbal.pef" .

distort.pef:
	make -C "../5b. distort" distort.pef
	mv "../5b. distort/distort.pef" .

blow.pef:
	make -C "../5c. blow" blow.pef
	mv "../5c. blow/blow.pef" .

glow.pef:
	make -C "../5d. glow" glow.pef
	mv "../5d. glow/glow.pef" .

redeye.pef:
	make -C "../5e. redeye" redeye.pef
	mv "../5e. redeye/redeye.pef" .

rasterfade2.pef:
	make -C "../6. rasterfade 2" rasterfade2.pef
	mv "../6. rasterfade 2/rasterfade2.pef" .

ufli.pef:
	make -C "../7. ufli logo" ufli.pef
	mv "../7. ufli logo/ufli.pef" .

unload.pef:
	make -C "../0. unload music" unload.pef
	mv "../0. unload music/unload.pef" .

unload2.pef:
	make -C "../0. unload music 2" unload2.pef
	mv "../0. unload music 2/unload2.pef" .

dummy.pef:
	make -C "../0. dummy" dummy.pef
	mv "../0. dummy/dummy.pef" .

borderfade.pef:
	make -C "../0. open border fade" borderfade.pef
	mv "../0. open border fade/borderfade.pef" .

closeborder1.pef:
	make -C "../0. close border fade 1" closeborder1.pef
	mv "../0. close border fade 1/closeborder1.pef" .

PARTS = 1a_bliksem.pef 1b_logo.pef 1c_copylogo.pef 1d_freemem.pef 1e_bitmapscroll.pef 1f_fade.pef unload.pef borderfade.pef 3dbars.pef agsp.pef closeborder1.pef dummy.pef oogbal.pef distort.pef blow.pef glow.pef redeye.pef rasterfade1.pef rasterfade2.pef unload2.pef ufli.pef  

# For testing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
halloweed4.d64: script.txt dirart.txt $(PARTS)
	$(PEFCHAIN) $(PEFFLAGS) -r 02 -z 5 -o $@ -t HALLOWEED4/XENON -a ../dirart/dirart.d64 $<

vice: clean halloweed4.d64
	$(X64) -autostart halloweed4.d64

clean:
	$(DEL) *.prg
	$(DEL) *.exe.prg
	$(DEL) *.sym
	$(DEL) *.vs
	$(DEL) *.dbg
	$(DEL) *.d64
	$(DEL) *.efo
	$(DEL) *.pef
	$(DEL) .source.txt
	$(DEL) splitefocmd_*.txt
	$(DEL) *_part*.bin
	make -C "../0. close border fade 1" clean
	make -C "../0. dummy" clean
	make -C "../0. open border fade" clean
	make -C "../0. unload music" clean
	make -C "../0. unload music 2" clean
	make -C "../1a. bliksem" clean
	make -C "../1b. logo" clean
	make -C "../1c. copylogo" clean
	make -C "../1d. freemem" clean
	make -C "../1e. bitmapscroll" clean
	make -C "../1f. fade" clean
	make -C "../2. 3d bars" clean
	make -C "../3. agsp multimax" clean
	make -C "../4. rasterfade 1" clean
	make -C "../5a. oogbal" clean
	make -C "../5b. distort" clean
	make -C "../5c. blow" clean
	make -C "../5d. glow" clean
	make -C "../5e. redeye" clean
	make -C "../6. rasterfade 2" clean
	make -C "../7. ufli logo" clean