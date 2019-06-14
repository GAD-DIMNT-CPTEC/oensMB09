#!/bin/sh -x

SHELL=/bin/sh
DIR=$(SOPERMOD)/pre/bin/sibalb

OBJ = sibalb.s

PROG=$(DIR)/sibalb

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 -rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/sibalb.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/sibalb.o $(DIR)/objects.a

.SUFFIXES : .f .s
.f.s:
	 $(F77) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.s

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.s
	 -rm $(PROG)
