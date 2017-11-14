include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = $(SOPERMOD)/eofhumi/include

EXT = $(TRUNC)$(LEV)
BIN = $(SOPERMOD)/eof/bin/$(TRUNC)$(LEV)

FTNFLAG = -byteswapio -r8 -i8 -mcmodel=medium 
CHWFLAG = -byteswapio -i8 -r8
CPP = -I\${INC}

OBJ = eofhum.$(EXT) \
abcx.$(EXT) epsmac.$(EXT) rswz.$(EXT) tql2.$(EXT) \
tred2.$(EXT) varave.$(EXT)

PROG = $(BIN)/eofhum.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eofhum$(EXT).a $(BIN)/*.o
	ar x $(BIN)/eofhum$(EXT).a $(BIN)/eofhum.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eofhum.o $(BIN)/eofhum$(EXT).a
	-rm $(BIN)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(BIN)/eofhum$(EXT).a
	-rm $(BIN)/*.o  $(OBJ)
	-rm $(PROG)
	-rm *.o
#
eofhum.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc 
