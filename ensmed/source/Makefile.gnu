#
# This makefile creates optimized executables of the ensemble mean
#
include ../../config/Makefile.conf.$(comp)

#
# Set object files
#
OBJM = ensmed.f90 
OBJS = ensmed.o

#
# Set compiler options
#
#F90 = ftn
LDFLAGS = -fconvert=big-endian -O0    
LOADFLAG =     
#LIBS = ../../produtos/libs/w3lib-1.4/libw3.a
LIB1 = ../../produtos/libs/w3emc/NCEPLIBS-w3emc-2.9.3/rls/lib64/libw3emc_4.a
LIB2 = ../../produtos/libs/bacio/NCEPLIBS-bacio-2.5.0/rls/lib/libbacio.a
CMD = ../bin/ensmed.x
LD_LIBRARY_PATH = /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2

#
# Define executable
#
all: $(CMD)

$(CMD): $(OBJS)
	$(F90) -o $(@) $(F90_32BITS) $(LOADFLAG) $(OBJS) $(LIB1) $(LIB2)

.SUFFIXES:	.f90 .f .o
.f90.o:
	$(F90) $(LOADFLAG) -c $<

clean: 
	-rm -f *.[omjl]
