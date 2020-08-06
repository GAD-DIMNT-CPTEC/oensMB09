include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = $(SOPERMOD)/eofhumi/include

EXT = $(TRUNC)$(LEV)
BIN = $(SOPERMOD)/eof/bin/$(TRUNC)$(LEV)

#FTNFLAG = -g -hbyteswapio -hpic -hdynamic -Ktrap=fp -s default64 -h display_opt,noomp 
#FTNFLAG = -g -hbyteswapio -hpic -Ktrap=fp -s default64 -h display_opt,noomp 
#CHWFLAG = -hbyteswapio -s default64
FTNFLAG = -g -fconvert=big-endian -fdefault-real-8
CHWFLAG = -fconvert=big-endian -fdefault-real-8 
#CPP = -I\${INC}
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
