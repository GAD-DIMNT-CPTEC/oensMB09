#!/bin/sh

TRUNC=TQ0126
LEV=L028
F77=ftn -byteswapio -i4 -r4 
INC=../../include/TQ0126L028/
SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
BIN=../
FTNFLAG="-I../../include/TQ0126L028/"

OBJ = \
abcx.$(EXT) epsmac.$(EXT) rswz.$(EXT) tql2.$(EXT) \
tred2.$(EXT) varave.$(EXT) eofhum.$(EXT) 

PROG=$(BIN)/eofhum.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eofhum$(EXT).a $(BIN)/*.o
	ar x $(BIN)/eofhum$(EXT).a $(BIN)/eofhum.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eofhum.o $(BIN)/eofhum$(EXT).a
	-rm $(BIN)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(FTNFLAG) -c -o $(BIN)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(BIN)/eofhum$(EXT).a
	-rm $(BIN)/*.o  $(OBJ)
	-rm $(PROG)
#
eofhum.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc 
