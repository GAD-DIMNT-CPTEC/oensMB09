#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)
DIR=$(SOPERMOD)/pre/bin/zrlint/$(EXT)

OBJ = \
zrlint.$(EXT) gl.$(EXT) nterp.$(EXT) ply.$(EXT) polavg.$(EXT) \
revrow.$(EXT) wterp.$(EXT)

PROG=$(DIR)/zrlint.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/zrlint.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/zrlint.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
zrlint.$(EXT): $(INC)/reshtgz.h
