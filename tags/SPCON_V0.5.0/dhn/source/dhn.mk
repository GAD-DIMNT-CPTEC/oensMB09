#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
DIR=$(SOPERMOD)/dhn/bin/$(EXT)

OBJ = dhn.$(EXT)

PROG=$(DIR)/dhn.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 -rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/dhn.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/dhn.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
dhn.$(EXT): $(INC)/resdhn.h
