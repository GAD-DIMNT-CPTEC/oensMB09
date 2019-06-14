#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)
DIR=$(SOPERMOD)/pre/bin/tg3zrl/$(EXT)

OBJ = \
tg3zrl.$(EXT)

PROG=$(DIR)/tg3zrl.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/tg3zrl.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/tg3zrl.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
tg3zrl.$(EXT): $(INC)/reshtgz.h
