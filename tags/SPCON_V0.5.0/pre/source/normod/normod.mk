#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)$(LEV)
DIR=$(SOPERMOD)/pre/bin/normod/$(EXT)

OBJ = \
getmod.$(EXT) amhmtm.$(EXT) asyg0.$(EXT)  asyrg.$(EXT) \
balanc.$(EXT) balbak.$(EXT) bmcm.$(EXT)   filter.$(EXT)\
hormod.$(EXT) hqr2.$(EXT)   hqr3.$(EXT)   ident.$(EXT) \
iminv.$(EXT)  order.$(EXT)  orthes.$(EXT) ortran.$(EXT)\
record.$(EXT) reset.$(EXT)  rg.$(EXT)     setsig.$(EXT)\
symg0.$(EXT)  symrg.$(EXT)  tql2.$(EXT)   tred2.$(EXT) \
vereig.$(EXT) vermod.$(EXT) w3fa03.$(EXT) znorma.$(EXT)

PROG=$(DIR)/normod.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/getmod.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/getmod.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
getmod.$(EXT): $(INC)/resvnmd.h
hormod.$(EXT): $(INC)/reshnmd.h
vereig.$(EXT): $(INC)/resvnmd.h
vermod.$(EXT): $(INC)/resvnmd.h $(INC)/delsnmd.h
