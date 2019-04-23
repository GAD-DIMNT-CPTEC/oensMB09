#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
DIR=$(SOPERMOD)/deceof/bin/$(EXT)
LIB=$(SOPERMOD)/fftpln/lib/$(EXT)

OBJ = deceof.$(EXT) \
dectrg.$(EXT) fl22s.$(EXT)  msu22.$(EXT)  psu22.$(EXT) \
qread.$(EXT)  qwrite.$(EXT) symasy.$(EXT) uvtodz.$(EXT)

PROG=$(DIR)/deceof.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	ar rcv $(DIR)/deceof.a $(DIR)/*.o
	ar x $(DIR)/deceof.a $(DIR)/deceof.o
	$(F77) $(FTNFLAG) -o $(PROG) $(DIR)/deceof.o $(DIR)/deceof.a $(LIB)/fftpln.a
	-rm $(DIR)/*.o


.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(DIR)/deceof.a
	-rm $(DIR)/*.o *.$(EXT) i.*.f
	-rm $(PROG)
#
deceof.$(EXT): $(INC)/deceof.h $(INC)/deceof2.h
dectrg.$(EXT): $(INC)/deceof.h
fl22s.$(EXT):  $(INC)/deceof.h
msu22.$(EXT):  $(INC)/deceof.h
psu22.$(EXT):  $(INC)/deceof.h
qread.$(EXT):  $(INC)/deceof.h
qwrite.$(EXT): $(INC)/deceof.h
symasy.$(EXT): $(INC)/deceof.h
uvtodz.$(EXT): $(INC)/deceof.h
