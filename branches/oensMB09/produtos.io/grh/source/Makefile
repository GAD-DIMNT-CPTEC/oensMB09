#
#  $Author: bonatti $
#  $Date: 2005/05/18 10:00:00 $
#  $Revision: 1.0 $
#
FTN=ifort -O3
FTNFLAG32= 
LOADFLAG=
EXEC=/mpp/io_dop/tempo/global/oens/grh/exec/PostGridHistory

OBJ = \
	InputParameters.o \
	Units.o \
	PostGridHistory.o

pre:	$(OBJ)
	$(FTN) -o $(EXEC) $(FTNFLAG) $(LOADFLAG) $(OBJ)

InputParameters.o: InputParameters.f90
	$(FTN) -c $(FTNFLAG32) InputParameters.f90

Units.o: Units.f90 InputParameters.o
	$(FTN) -c $(FTNFLAG) Units.f90

PostGridHistory.o: PostGridHistory.f90 Units.o InputParameters.o
	$(FTN) -c $(FTNFLAG) PostGridHistory.f90

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FTN) -c $(FTNFLAG) $<

clean:
	-rm $(OBJ)
	-rm $(EXEC)
	-rm *.mod
#
