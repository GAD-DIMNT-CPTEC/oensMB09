include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = ../include

EXT = $(TRUNC)$(LEV)
DIR = ../lib/$(EXT)

#FTNFLAG = -g -fconvert=big-endian -fcheck=all -fbacktrace -Wall -Wextra 
#FTNFLAG = -g -fconvert=big-endian -fbacktrace -Wall -Wextra 
#FTNFLAG = -g -fconvert=big-endian -fdefault-real-8 
#FTNFLAG = -g -fconvert=big-endian -fdefault-real-8 -fpic -O -Wall -fcheck=all -fbacktrace
FTNFLAG = -fconvert=big-endian -fdefault-real-8
# DEBUG:
#FTNFLAG = -g -fconvert=big-endian -fdefault-real-8 -fpic -Wall -Wextra -fcheck=all -fbacktrace -O0
CPP = -I\$(INC)

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
