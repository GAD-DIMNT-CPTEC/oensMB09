#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
DIR=$(SOPERMOD)/recfct/bin/$(EXT)
LIB=$(SOPERMOD)/fftpln/lib/$(EXT)

OBJ = recfct.$(EXT) \
dztouv.$(EXT) fwrite.$(EXT) qread.$(EXT) \
rectrg.$(EXT) sumpls.$(EXT) sumplv.$(EXT)

PROG=$(DIR)/recfct.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/recfct.a $(DIR)/*.o
	 ar x $(DIR)/recfct.a $(DIR)/recfct.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/recfct.o $(DIR)/recfct.a $(LIB)/fftpln.a
	 -rm $(DIR)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(DIR)/recfct.a
	-rm $(DIR)/*.o *.$(EXT) i.*.f
	-rm $(PROG)
#
recanl.$(EXT): $(INC)/recfct.h
dztouv.$(EXT): $(INC)/recfct.h
fwrite.$(EXT): $(INC)/recfct.h
qread.$(EXT):  $(INC)/recfct.h
rectrg.$(EXT): $(INC)/recfct.h
sumpls.$(EXT): $(INC)/recfct.h
sumplv.$(EXT): $(INC)/recfct.h
