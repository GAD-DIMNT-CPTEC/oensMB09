#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)
DIR=$(SOPERMOD)/pre/bin/albint/$(EXT)

OBJ = \
albint.$(EXT) gl.$(EXT) nterp.$(EXT) ply.$(EXT) polavg.$(EXT) \
revrow.$(EXT) wterp.$(EXT)

PROG=$(DIR)/albint.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/albint.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/albint.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)
clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)

#
albint.$(EXT): $(INC)/reshalb.h
