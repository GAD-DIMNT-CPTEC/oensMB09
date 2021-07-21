include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = ../include

EXT = $(TRUNC)$(LEV)
BIN = ../../eof/bin/$(EXT)

#FTNFLAG = -g -hbyteswapio -hpic -hdynamic -Ktrap=fp -s default64 -h display_opt,noomp 
FTNFLAG = -g -fconvert=big-endian #-fdefault-real-8
CHWFLAG = -fconvert=big-endian #-fdefault-real-8 

#CPP = -I\${INC}
CPP = -I\$(INC)
F_UFMTIEEE = 10,11,20,62,64,68,69,72,74

OBJ = eofwin.$(EXT) \
abcx.$(EXT)   chwz.$(EXT) epsmac.$(EXT) htribk.$(EXT) \
htridi.$(EXT) tql2.$(EXT) varave.$(EXT)

PROG=$(BIN)/eofwin.$(EXT)

all:     $(PROG)

chwz.$(EXT): chwz.f 
	$(F77) $(CPP) $(CHWFLAG) -c -o $(BIN)/chwz.o chwz.f

$(PROG): $(OBJ)
	ar rcv $(BIN)/eofwin$(EXT).a $(BIN)/*.o
	ar x $(BIN)/eofwin$(EXT).a $(BIN)/eofwin.o
	$(F77) $(FTNFLAG) -o $(PROG) $(BIN)/eofwin.o $(BIN)/eofwin$(EXT).a
	-rm $(BIN)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(BIN)/$*.o $<

clean:
	-rm $(BIN)/eofwin$(EXT).a
	-rm $(BIN)/*.o $(OBJ)
	-rm $(PROG)
	-rm *.o
#
eofwin.$(EXT): $(INC)/reseofes.inc 
chwz.$(EXT): $(INC)/nvector.inc
