include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = ../include

EXT = $(TRUNC)$(LEV)
DIR = ../bin/$(EXT)
LIB = ../..//fftpln/lib/$(EXT)

#FTNFLAG = -g -hbyteswapio -hpic -hdynamic -Ktrap=fp -s default64 -h display_opt,noomp 
#FTNFLAG = -g -fconvert=big-endian -fcheck=all -fbacktrace -Wall -Wextra
#FTNFLAG = -g -fconvert=big-endian -fbacktrace -Wall -Wextra
FTNFLAG = -g -fconvert=big-endian -fdefault-real-8 
#CPP = -I\${INC} 
CPP = -I\$(INC)

F_UFMTIEEE = 10,20
F_UFMTADJUST10 = TYPE2

OBJ = recanl.$(EXT) \
dztouv.$(EXT) fwrite.$(EXT) qread.$(EXT) \
rectrg.$(EXT) sumpls.$(EXT) sumplv.$(EXT)

PROG = $(DIR)/recanl.$(EXT)

all:	 $(PROG)

$(PROG): $(OBJ)
	 ar rcv $(DIR)/recanl.a $(DIR)/*.o
	 ar x $(DIR)/recanl.a $(DIR)/recanl.o
	 $(F77) $(FTNFLAG) -o $(PROG) $(DIR)/recanl.o $(DIR)/recanl.a $(LIB)/fftpln.a
	 -rm $(DIR)/*.o

.SUFFIXES : .f .$(EXT)
.f.$(EXT):
	$(F77) $(CPP) $(FTNFLAG) -c -o $(DIR)/$*.o $<

clean:
	-rm $(DIR)/recanl.a
	-rm $(DIR)/*.o *.$(EXT) i.*.f
	-rm $(PROG)
	-rm *.o
#
recanl.$(EXT): $(INC)/recanl.h
dztouv.$(EXT): $(INC)/recanl.h
fwrite.$(EXT): $(INC)/recanl.h
qread.$(EXT):  $(INC)/recanl.h
rectrg.$(EXT): $(INC)/recanl.h
sumpls.$(EXT): $(INC)/recanl.h
sumplv.$(EXT): $(INC)/recanl.h
