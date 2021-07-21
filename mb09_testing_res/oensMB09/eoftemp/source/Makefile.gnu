include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = ../include

EXT = $(TRUNC)$(LEV)
BIN = ../../eof/bin/$(EXT)

#FTNFLAG = -g -hbyteswapio -hpic -hdynamic -Ktrap=fp -s default64 -h display_opt,noomp 
#FTNFLAG = -g -hbyteswapio -Ktrap=fp -s default64 -h display_opt,noomp,page_align_allocate
#FTNFLAG = -g -hbyteswapio -Ktrap=fp -s default64 -h display_opt,noomp
#FTNFLAG=-g -hbyteswapio -O nomodinline,cache0,vector0,ipa0,fusion0,unroll0,nopattern -h cache0 -e0 -Ktrap=fp -s default64  -h display_opt
FTNFLAG = -g -fconvert=big-endian #-fdefault-real-8 
CHWFLAG = -fconvert=big-endian #-fdefault-real-8

#CPP = -I\${INC}
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
#
eoftem.$(EXT): $(INC)/reseofes.inc 
rswz.$(EXT): $(INC)/nvector.inc
