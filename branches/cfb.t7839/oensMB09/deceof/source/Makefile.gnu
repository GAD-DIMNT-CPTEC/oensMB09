include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = ../include

EXT = $(TRUNC)$(LEV)
DIR = ../bin/$(EXT)
LIB = ../../fftpln/lib/$(EXT)

FTNFLAG = -g -fconvert=big-endian -fdefault-real-8 
CPP = -I\$(INC)

F_UFMTIEEE = 10,11,20,62,64,72,74
F_UFMTADJUST10 = TYPE2
F_UFMTADJUST11 = TYPE2
F_UFMTADJUST20 = TYPE2
F_UFMTADJUST62 = TYPE2
F_UFMTADJUST64 = TYPE2
F_UFMTADJUST72 = TYPE2
F_UFMTADJUST74 = TYPE2

OBJ = deceof.$(EXT) \
dectrg.$(EXT) fl22s.$(EXT)  msu22.$(EXT)  psu22.$(EXT) \
qread.$(EXT)  qwrite.$(EXT) symasy.$(EXT) uvtodz.$(EXT)

PROG = $(DIR)/deceof.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	ar rcv $(DIR)/deceof.a $(DIR)/*.o
	ar x $(DIR)/deceof.a $(DIR)/deceof.o
	$(F77) $(FTNFLAG) -o $(PROG) $(DIR)/deceof.o $(DIR)/deceof.a $(LIB)/fftpln.a
	-rm $(DIR)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<

clean:
	-rm $(DIR)/deceof.a
	-rm $(DIR)/*.o *.$(EXT) i.*.f
	-rm $(PROG)
	-rm *.o
#
deceof.$(EXT): $(INC)/deceof.h $(INC)/deceof2.h
dectrg.$(EXT): $(INC)/deceof.h
fl22s.$(EXT):  $(INC)/deceof.h
msu22.$(EXT):  $(INC)/deceof.h
psu22.$(EXT):  $(INC)/deceof.h
qread.$(EXT):  $(INC)/deceof.h
qwrite.$(EXT): $(INC)/deceof.h
symasy.$(EXT): $(INC)/deceof.h
uvtodz.$(EXT): $(INC)/deceof.h
