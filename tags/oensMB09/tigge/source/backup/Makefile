SHELL   =/bin/sh
INCMOD  =
SRC     =Tigge.f90
INC     =
SRC     =Constants.f90 
OBJ     =Constants.o Tigge.o
MODS    =Constants.mod
FCP     =ifort
FFLAGS  =
LDFLAGS =
LIBS    =../../pos/source/w3lib-1.4/libw3.a
CMD     =../bin/tigge.x

pos:	$(OBJ)
	$(FCP) -o $(CMD) $(FFLAGS) $(OBJ) $(LIBS)

Constants.o:	Constants.f90
	$(FCP) $(FFLAGS) -c Constants.f90

Tigge.o: Tigge.f90 Constants.o
	$(FCP) $(FFLAGS) -c Tigge.f90

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(F90) -c $(FFLAGS) $<

clean:
	-rm -f $(CMD) *.o *.mod
