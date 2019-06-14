#!/bin/sh

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
DIR=$(SOPERMOD)/rdpert/bin/$(EXT)

OBJ = rdpert.$(EXT) \
fread.$(EXT)  fwrite.$(EXT) gasdev.$(EXT) glatd.$(EXT)  gsctij.$(EXT) \
pertur.$(EXT) pln0.$(EXT)   ran1.$(EXT)   varave.$(EXT)

PROG=$(DIR)/rdpert.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/rdpert.a $(DIR)/*.o
	 -rm $(DIR)/*.o
	 ar x $(DIR)/rdpert.a $(DIR)/rdpert.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/rdpert.o $(DIR)/rdpert.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(DIR)/rdpert.a
	-rm $(DIR)/*.o *.$(EXT) i.*.f
	-rm $(PROG)
#
rdpert.$(EXT): $(INC)/rdpert.h 
fread.$(EXT):  $(INC)/rdpert.h
fwrite.$(EXT): $(INC)/rdpert.h
