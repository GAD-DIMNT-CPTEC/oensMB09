include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = $(SOPERMOD)/fftpln/include

EXT = $(TRUNC)$(LEV)
DIR = $(SOPERMOD)/fftpln/lib/$(EXT)

FTNFLAG = -byteswapio -r8 -i8 
CPP = -I\${INC}

OBJ = \
epslon.$(EXT) fax.$(EXT)    fft991.$(EXT) fft99a.$(EXT) \
fft99b.$(EXT) fftpln.$(EXT) fftrig.$(EXT) glats.$(EXT)  \
pln2.$(EXT)   plnder.$(EXT) poly.$(EXT)   reset.$(EXT)  \
transs.$(EXT) transv.$(EXT) vpassm.$(EXT)

LIB = $(DIR)/fftpln.a

all:	 $(LIB)

$(LIB): $(OBJ)
	ar rcv $(LIB) $(DIR)/*.o
	-rm $(DIR)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	touch $*.$(EXT)

clean:
	-rm $(LIB)
	-rm *.$(EXT) i.*.f

epslon.$(EXT): $(INC)/fftpln.h
fftpln.$(EXT): $(INC)/fftpln.h
glats.$(EXT):  $(INC)/fftpln.h
pln2.$(EXT):   $(INC)/fftpln.h
plnder.$(EXT): $(INC)/fftpln.h
transs.$(EXT): $(INC)/fftpln.h
transv.$(EXT): $(INC)/fftpln.h
