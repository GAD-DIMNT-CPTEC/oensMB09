#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
BIN=$(SOPERMOD)/eof/bin/$(RESOL)$(NIVEL)

OBJ = eofhum.$(EXT) \
abcx.$(EXT) epsmac.$(EXT) rswz.$(EXT) tql2.$(EXT) \
tred2.$(EXT) varave.$(EXT)

PROG=$(BIN)/eofhum.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eofhum$(EXT).a $(BIN)/*.o
	-rm $(BIN)/*.o
	ar x $(BIN)/eofhum$(EXT).a $(BIN)/eofhum.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eofhum.o $(BIN)/eofhum$(EXT).a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(BIN)/eofhum$(EXT).a
	-rm $(BIN)/*.o *.$(EXT)
	-rm $(PROG)
#
eofhum.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc 
