#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
BIN=$(SOPERMOD)/eof/bin/$(RESOL)$(NIVEL)

OBJ = eofwin.$(EXT) \
abcx.$(EXT)   chwz.$(EXT) epsmac.$(EXT) htribk.$(EXT) \
htridi.$(EXT) tql2.$(EXT) varave.$(EXT)

PROG=$(BIN)/eofwin.$(EXT)

all:     $(PROG)

chwz.$(EXT): chwz.f 
	$(F77) $(CPP) $(CHWFLAG) -c -o $(BIN)/chwz.o chwz.f
	touch chwz.$(EXT)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eofwin$(EXT).a $(BIN)/*.o
	-rm $(BIN)/*.o
	ar x $(BIN)/eofwin$(EXT).a $(BIN)/eofwin.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eofwin.o $(BIN)/eofwin$(EXT).a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(BIN)/eofwin$(EXT).a
	-rm $(BIN)/*.o *.$(EXT)
	-rm $(PROG)
#
eofwin.$(EXT): $(INC)/reseofes.inc 
chwz.$(EXT): $(INC)/nvector.inc
