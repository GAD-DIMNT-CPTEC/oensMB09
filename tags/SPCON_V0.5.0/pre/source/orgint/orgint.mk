#!/bin/sh -x

SHELL=/bin/sh
EXT=$(TRUNC)
DIR=$(SOPERMOD)/pre/bin/orgint/$(EXT)

OBJ = \
epslon.$(EXT) fax.$(EXT) fft991.$(EXT) fft99a.$(EXT) fft99b.$(EXT) \
fftfxt.$(EXT) fftrig.$(EXT) fl22.$(EXT) getrow.$(EXT) grd2sp.$(EXT) \
grdtos.$(EXT) gslats.$(EXT) la0la1.$(EXT) orgphy.$(EXT) pln2.$(EXT) \
poly.$(EXT) reset.$(EXT) revrow.$(EXT) smtcft.$(EXT) sp2grd.$(EXT) \
stogrd.$(EXT) sumpls.$(EXT) symasy.$(EXT) transp.$(EXT) vpassm.$(EXT)

PROG=$(DIR)/orgint.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/objects.a $(DIR)/*.o
	 rm $(DIR)/*.o
	 ar x $(DIR)/objects.a $(DIR)/orgphy.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/orgphy.o $(DIR)/objects.a

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	 $(F77) -I$(INC) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	 touch $*.$(EXT)

clean:
	 -rm $(DIR)/objects.a
	 -rm $(DIR)/*.o *.$(EXT)
	 -rm $(PROG)
#
grd2sp.$(EXT): $(INC)/reshoro.h
orgphy.$(EXT): $(INC)/reshoro.h
sp2grd.$(EXT): $(INC)/reshoro.h
