#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
DIR=$(SOPERMOD)/decanl/bin/$(EXT)
LIB=$(SOPERMOD)/fftpln/lib/$(EXT)

OBJ = decanl.$(EXT) \
dectrg.$(EXT) fl22s.$(EXT)  msu22.$(EXT)  psu22.$(EXT) \
qread.$(EXT)  qwrite.$(EXT) symasy.$(EXT) uvtodz.$(EXT)

PROG=$(DIR)/decanl.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	ar rcv $(DIR)/decanl.a $(DIR)/*.o
	ar x $(DIR)/decanl.a $(DIR)/decanl.o
	$(F77) $(FTNFLAG) -o $(PROG) $(DIR)/decanl.o $(DIR)/decanl.a $(LIB)/fftpln.a
	-rm $(DIR)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(DIR)/decanl.a
	-rm $(DIR)/*.o *.$(EXT) i.*.f
	-rm $(PROG)
#
decanl.$(EXT): $(INC)/decanl.h 
dectrg.$(EXT): $(INC)/decanl.h
fl22s.$(EXT):  $(INC)/decanl.h
msu22.$(EXT):  $(INC)/decanl.h
psu22.$(EXT):  $(INC)/decanl.h
qread.$(EXT):  $(INC)/decanl.h
qwrite.$(EXT): $(INC)/decanl.h
symasy.$(EXT): $(INC)/decanl.h
uvtodz.$(EXT): $(INC)/decanl.h
