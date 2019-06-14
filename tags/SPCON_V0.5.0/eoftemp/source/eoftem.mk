#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
BIN=$(SOPERMOD)/eof/bin/$(RESOL)$(NIVEL)

OBJ = eoftem.$(EXT) \
abcx.$(EXT) epsmac.$(EXT) rswz.$(EXT) tql2.$(EXT) \
tred2.$(EXT) varave.$(EXT)

PROG=$(BIN)/eoftem.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eoftem$(EXT).a $(BIN)/*.o
	-rm $(BIN)/*.o
	ar x $(BIN)/eoftem$(EXT).a $(BIN)/eoftem.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eoftem.o $(BIN)/eoftem$(EXT).a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(BIN)/eoftem$(EXT).a
	-rm $(BIN)/*.o *.$(EXT)
	-rm $(PROG)
#
eoftem.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc
