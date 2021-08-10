#
# This makefile creates executables for the ensemble spread
#

#
# Set object files
#
OBJM = GaussRep.f90 spread.f90
OBJS = GaussRep.o  spread.o

#
#  Set compiler options
#
F90 = ftn
AR = ar
F90_32FLAGS = -fconvert=big-endian -O0
LOADFLAG = -O0 -fconvert=big-endian 
LIBS = ../../libs/w3lib-1.4/libw3.a
CMD = ../bin/spread.x
LD_LIBRARY_PATH = /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2

#
# Define executable
#
all: $(CMD)

$(CMD): $(OBJS)
	$(F90) -o $(@) $(F90_32BITS) $(LOADFLAG) $(OBJS) $(LIBS)

.SUFFIXES:	.f90 .f .o
.f90.o:
	$(F90) $(LOADFLAG) -c $<

clean: 
	-rm -f *.o
	-rm -f *.mod
