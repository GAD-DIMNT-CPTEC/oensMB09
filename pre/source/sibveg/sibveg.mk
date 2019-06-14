#!/bin/sh -x

SHELL=/bin/sh
DIR=$(SOPERMOD)/pre/bin/sibveg

OBJ = sibveg.s

PROG=$(DIR)/sibveg

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 -rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/sibveg.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/sibveg.o $(DIR)/objects.a

.SUFFIXES : .f .s
.f.s:
	 $(F77) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.s

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.s
	 -rm $(PROG)
