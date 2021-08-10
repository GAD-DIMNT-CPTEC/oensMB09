include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = ../include

EXT = $(TRUNC)$(LEV)
BIN = ../../eof/bin/$(TRUNC)$(LEV)

FTNFLAG = -g -fconvert=big-endian 
CHWFLAG = -fconvert=big-endian 
CPP = -I\$(INC)

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

clean:
	-rm $(BIN)/eofhum$(EXT).a
	-rm $(BIN)/*.o  $(OBJ)
	-rm $(PROG)
	-rm *.o
#
eofhum.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc 
