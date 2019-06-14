#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)
DIR=$(SOPERMOD)/pre/bin/sibmsk/$(EXT)

OBJ = \
msksib.$(EXT) fixsl.$(EXT) gl.$(EXT) nterp.$(EXT) ply.$(EXT) \
polavg.$(EXT) revrow.$(EXT) vterp.$(EXT) wterp.$(EXT)

PROG=$(DIR)/fixmsk.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/msksib.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/msksib.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
msksib.$(EXT): $(INC)/reshsib.h
