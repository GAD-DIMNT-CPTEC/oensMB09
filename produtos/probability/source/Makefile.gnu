#
# This makefile creates executables for the ensemble probability
#
include ../../../config/Makefile.conf.$(comp)

#
# Set object files
#
OBJM = GaussRep.f90 ReadFields.f90 probability.f90
OBJS = GaussRep.o ReadFields.o probability.o

#
#  Set compiler options
#
#F90 = ftn
AR = ar
LDFLAGS = -fconvert=big-endian 
F90_32FLAGS = -O0 -fconvert=big-endian
LOADFLAG = -O0 -fconvert=big-endian 
LIBS = ../../libs/w3lib-1.4/libw3.a
F90FLAGS = -O0 -fconvert=big-endian
CMD = ../bin/probability.x
LD_LIBRARY_PATH = /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2

#
# Define executable
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(F90) -o $(@) $(F90_32BITS) $(LOADFLAG) $(OBJS) $(LIBS)


.SUFFIXES:	.f90 .f .o
.f90.o:
	$(F90) $(LOADFLAG) -c $<

clean: 
	-rm -f *.[omjl]
