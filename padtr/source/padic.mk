#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
DIR=$(SOPERMOD)/padtr/bin/$(EXT)

OBJ = padic.$(EXT) \
linear.$(EXT) padtrg.$(EXT) reset.$(EXT) setsig.$(EXT) smtcft.$(EXT)

PROG=$(DIR)/padic.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 -rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/padic.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/padic.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
padic.$(EXT): $(INC)/horpadr.h $(INC)/verpadr.h $(INC)/delsigp.h \
              $(INC)/horpadw.h $(INC)/verpadw.h
