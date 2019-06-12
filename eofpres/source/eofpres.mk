#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
BIN=$(SOPERMOD)/eof/bin/$(TRUNC)$(LEV)

OBJ = eofpres.$(EXT) \
abcx.$(EXT) epsmac.$(EXT) rswz.$(EXT) tql2.$(EXT) \
tred2.$(EXT) varave.$(EXT) 

PROG=$(BIN)/eofpres.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eofpres$(EXT).a $(BIN)/*.o
	ar x $(BIN)/eofpres$(EXT).a $(BIN)/eofpres.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eofpres.o $(BIN)/eofpres$(EXT).a
	-rm $(BIN)/*.o


.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(BIN)/eofpres$(EXT).a
	-rm $(BIN)/*.o *.$(EXT)
	-rm $(PROG)
#
eofpres.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc
