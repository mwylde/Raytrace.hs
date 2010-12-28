BIN = raytrace
DEFS = 
PACKAGES = -package GLUT
GHC = ghc

all: 
	$(GHC) --make -O $(DEFS) $(PACKAGES) GUI.hs -o $(BIN)

#raytrace:
#	$(GHC) --make -prof -auto-all $(DEFS) $(PACKAGES) Main.hs -o $(EXE).prof

#debug:
#	$(GHC) --make -O $(DEFS) $(PACKAGES) -DDEBUG Main.hs -o $(EXE).debug

clean:
	rm -fr *.o *.hi *.p_hi $(BIN)
