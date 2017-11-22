include ../../config/Makefile.conf.$(comp)

SOPERMOD = $(WORKDIR)
INC = $(SOPERMOD)/recanl/include

EXT = $(TRUNC)$(LEV)
DIR = $(SOPERMOD)/recanl/bin/$(EXT)
LIB = $(SOPERMOD)/fftpln/lib/$(EXT)

FTNFLAG = -byteswapio -i8 -r8 -mcmodel=medium 
CPP = -I\${INC} 

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
	touch $*.$(EXT)

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
