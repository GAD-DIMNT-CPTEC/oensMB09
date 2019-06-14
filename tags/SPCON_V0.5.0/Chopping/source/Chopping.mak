#!/bin/sh -x

SHELL=/bin/sh
DIR=$(DATAMAINDIR)/$(SRCSUBDIR)/bin

OBJ= \
m_Parameters.chk \
m_Arrays.chk \
m_Recomposition.chk \
m_Fourier.chk \
m_Legendre.chk \
m_Decomposition.chk \
m_Utils.chk \
m_Vertical_Interpolation.chk \
Chopping.chk

PROG=$(DIR)/$(SRCNAME).bin

all:	 $(PROG)

m_Parameters.chk: m_Parameters.f90
	-rm $(DIR)/m_Parameters.o $(DIR)/m_parameters.mod
	$(FTN) $(FTNFLAG) -c -o $(DIR)/m_Parameters.o m_Parameters.f90
	mv m_parameters.mod $(DIR)
	touch m_Parameters.chk

m_Arrays.chk: m_Arrays.f90
	-rm $(DIR)/m_Arrays.o $(DIR)/m_arrays.mod
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/m_Arrays.o m_Arrays.f90
	mv m_arrays.mod $(DIR)
	touch m_Arrays.chk

m_Recomposition.chk: m_Recomposition.f90
	-rm $(DIR)/m_Recomposition.o $(DIR)/m_recomposition.mod
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/m_Recomposition.o m_Recomposition.f90
	mv m_recomposition.mod $(DIR)
	touch m_Recomposition.chk

m_Fourier.chk: m_Fourier.f90
	-rm $(DIR)/m_Fourier.o $(DIR)/m_fourier.mod
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/m_Fourier.o m_Fourier.f90
	mv m_fourier.mod $(DIR)
	touch m_Fourier.chk

m_Legendre.chk: m_Legendre.f90
	-rm $(DIR)/m_Legendre.o $(DIR)/m_legendre.mod
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/m_Legendre.o m_Legendre.f90
	mv m_legendre.mod $(DIR)
	touch m_Legendre.chk

m_Decomposition.chk: m_Decomposition.f90
	-rm $(DIR)/m_Decomposition.o $(DIR)/m_decomposition.mod
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/m_Decomposition.o m_Decomposition.f90
	mv m_decomposition.mod $(DIR)
	touch m_Decomposition.chk

m_Utils.chk: m_Utils.f90
	-rm $(DIR)/m_Utils.o $(DIR)/m_utils.mod
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/m_Utils.o m_Utils.f90
	mv m_utils.mod $(DIR)
	touch m_Utils.chk

m_Vertical_Interpolation.chk: m_Vertical_Interpolation.f90
	-rm $(DIR)/m_Vertical_Interpolation.o $(DIR)/m_vertical_interpolation.mod
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/m_Vertical_Interpolation.o m_Vertical_Interpolation.f90
	mv m_vertical_interpolation.mod $(DIR)
	touch m_Vertical_Interpolation.chk

$(PROG): $(OBJ)
	-rm $(PROG) $(DIR)/objects.a
	ar rcv $(DIR)/objects.a $(DIR)/*.o
	-rm $(DIR)/*.o
	ar x $(DIR)/objects.a $(DIR)/Chopping.o
	$(FTN) $(FTNFLAG) -o $(PROG) $(DIR)/Chopping.o $(DIR)/objects.a

.SUFFIXES: .f90 .chk
.f90.chk:
	-rm $(DIR)/$*.o
	$(FTN) -I$(DIR) $(FTNFLAG) -c -o $(DIR)/$*.o $<
	touch $*.chk

clean:
	-rm *.chk
	-rm $(DIR)/objects.a
	-rm $(DIR)/*.o
	-rm $(DIR)/*.mod
	-rm $(PROG)
#
m_Parameters.chk: m_Parameters.h
m_Arrays.chk: $(DIR)/m_parameters.mod
m_Recomposition.chk: $(DIR)/m_parameters.mod $(DIR)/m_arrays.mod
m_Fourier.chk: $(DIR)/m_parameters.mod
m_Legendre.chk: $(DIR)/m_parameters.mod
m_Decomposition.chk: $(DIR)/m_parameters.mod $(DIR)/m_fourier.mod $(DIR)/m_legendre.mod
m_Utils.chk: $(DIR)/m_parameters.mod $(DIR)/m_arrays.mod
m_Vertical_Interpolation.chk: $(DIR)/m_parameters.mod
Chopping.chk: $(DIR)/m_parameters.mod $(DIR)/m_arrays.mod $(DIR)/m_recomposition.mod \
              $(DIR)/m_fourier.mod $(DIR)/m_legendre.mod $(DIR)/m_decomposition.mod \
              $(DIR)/m_utils.mod $(DIR)/m_vertical_interpolation.mod
