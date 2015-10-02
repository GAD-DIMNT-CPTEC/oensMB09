F90 = pgf90
F90 = gfortran
CMD = ../exec/CRPS.exe
OBJM = SubDomain.f90 WGT2D.f SORTMM.f crps.f90 ClimateInterp.f90 prob.f90 Main.f90
OBJS = SubDomain.o WGT2D.o SORTMM.o crps.o ClimateInterp.o prob.o Main.o

all:	$(CMD)

$(CMD):	$(OBJS)
	$(F90) -o $(@) $(OBJS)

.SUFFIXES:	.f90 .f .o

%.o: %.f
	$(F90) -ffixed-form -c $<
%.o: %.f90
	$(F90) -ffree-form -c $<
clean: 
	-rm -f *.[omjl]
