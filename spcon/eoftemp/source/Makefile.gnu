include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = $(SOPERMOD)/eoftemp/include

EXT = $(TRUNC)$(LEV)
BIN = $(SOPERMOD)/eof/bin/$(EXT)

FTNFLAG = -g -fconvert=big-endian -fdefault-real-8 
CHWFLAG = -fconvert=big-endian -fdefault-real-8

CPP = -I\$(INC)
F_UFMTIEEE = 10,11,20,62,64,68,69,72,74

OBJ = eoftem.$(EXT) \
abcx.$(EXT) epsmac.$(EXT) rswz.$(EXT) tql2.$(EXT) \
tred2.$(EXT) varave.$(EXT)

PROG = $(BIN)/eoftem.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eoftem$(EXT).a $(BIN)/*.o
	ar x $(BIN)/eoftem$(EXT).a $(BIN)/eoftem.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eoftem.o $(BIN)/eoftem$(EXT).a
	-rm $(BIN)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<

clean:
	-rm $(BIN)/eoftem$(EXT).a
	-rm $(BIN)/*.o $(OBJ)
	-rm $(PROG)
	-rm *.o

eoftem.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc
