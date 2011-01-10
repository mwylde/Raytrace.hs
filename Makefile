BIN = raytrace
DEFS = 
PACKAGES = -package GLUT
GHC = ghc

all: 
	make table

table: 
	$(GHC) --make -O $(DEPS) $(PACKAGES) scenes/table.hs -o table

clean:
	rm -fr *.o *.hi *.p_hi $(BIN)
