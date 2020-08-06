include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = $(SOPERMOD)/recfct/include

EXT = $(TRUNC)$(LEV)
DIR = $(SOPERMOD)/recfct/bin/$(EXT)
LIB = $(SOPERMOD)/fftpln/lib/$(EXT)

#FTNFLAG = -g -hbyteswapio -hpic -hdynamic -Ktrap=fp -s default64 -h display_opt,noomp 
#FTNFLAG = -g -hbyteswapio -hpic -Ktrap=fp -s default64 -h display_opt,noomp 
FTNFLAG = -g -fconvert=big-endian -fdefault-real-8 
#CPP = -I\${INC}
CPP = -I\$(INC)
F_UFMTIEEE = 10,20
F_UFMTADJUST10 = TYPE2

OBJ = recfct.$(EXT) \
dztouv.$(EXT) fwrite.$(EXT) qread.$(EXT) \
rectrg.$(EXT) sumpls.$(EXT) sumplv.$(EXT)

PROG=$(DIR)/recfct.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	ar rcv $(DIR)/recfct.a $(DIR)/*.o
	ar x $(DIR)/recfct.a $(DIR)/recfct.o
	$(F77) $(FTNFLAG) -o $(PROG) $(DIR)/recfct.o $(DIR)/recfct.a $(LIB)/fftpln.a
	-rm $(DIR)/*.o
	-rm *.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<

clean:
	-rm $(DIR)/recfct.a
	-rm $(DIR)/*.o *.$(EXT) i.*.f
	-rm $(PROG)
#
recanl.$(EXT): $(INC)/recfct.h
dztouv.$(EXT): $(INC)/recfct.h
fwrite.$(EXT): $(INC)/recfct.h
qread.$(EXT):  $(INC)/recfct.h
rectrg.$(EXT): $(INC)/recfct.h
sumpls.$(EXT): $(INC)/recfct.h
sumplv.$(EXT): $(INC)/recfct.h
