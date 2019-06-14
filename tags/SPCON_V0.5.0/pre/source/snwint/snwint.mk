#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)
DIR=$(SOPERMOD)/pre/bin/snwint/$(EXT)

OBJ = snwint.$(EXT)

PROG=$(DIR)/snwint.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 -rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/snwint.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/snwint.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
snwint.$(EXT): $(INC)/reshsno.h
