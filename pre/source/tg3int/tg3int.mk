#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)
DIR=$(SOPERMOD)/pre/bin/tg3int/$(EXT)

OBJ = \
tg3int.$(EXT) gl.$(EXT) nterp.$(EXT) ply.$(EXT) polavg.$(EXT) \
revrow.$(EXT) wterp.$(EXT)

PROG=$(DIR)/tg3int.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/tg3int.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/tg3int.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
tg3int.$(EXT): $(INC)/reshtgz.h
