include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = ../include

EXT = $(TRUNC)$(LEV)
BIN = ../../eof/bin/$(EXT)

FTNFLAG = -g -fconvert=big-endian #-fdefault-real-8 
CHWFLAG = -fconvert=big-endian #-fdefault-real-8
CPP = -I\$(INC)
F_UFMTIEEE = 10,11,20,62,64,68,69,72,74

OBJ = eofpres.$(EXT) \
abcx.$(EXT) epsmac.$(EXT) rswz.$(EXT) tql2.$(EXT) \
tred2.$(EXT) varave.$(EXT) 

PROG = $(BIN)/eofpres.$(EXT)

all:     $(PROG)

$(PROG): $(OBJ)
	ar rcv $(BIN)/eofpres$(EXT).a $(BIN)/*.o
	ar x $(BIN)/eofpres$(EXT).a $(BIN)/eofpres.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eofpres.o $(BIN)/eofpres$(EXT).a
	-rm $(BIN)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<

clean:
	-rm $(BIN)/eofpres$(EXT).a
	-rm $(BIN)/*.o *.$(EXT)
	-rm $(PROG)
	-rm *.o
#
eofpres.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc
