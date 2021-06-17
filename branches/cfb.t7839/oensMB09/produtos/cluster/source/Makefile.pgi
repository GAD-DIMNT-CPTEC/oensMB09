#
#   File     :  makecluster
#
#   This makefile creates optimized executables for the clustering
#
#   Set object files.
#
#

OBJM=	 GaussRep.f90 ReadFields.f90 cluster.f90
             
OBJS=	 GaussRep.o ReadFields.o cluster.o

#
#   Set compiler options.
#
F90 = ftn
AR =               ar
LDFLAGS = -byteswapio
F90_32FLAGS = -byteswapio #-Wf"-pvctl noassume vwork=stack"
LOADFLAG = -byteswapio #-Wl"-Z 4G" 
LIBS = ../../libs/w3lib-1.4/libw3.a
F90FLAGS = -byteswapio #-float0 -ew
CMD = ../bin/cluster.x
LD_LIBRARY_PATH = /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2
#
#
#   Define three executable makes.
#      all :  "optimized" executable 
#
all:		$(CMD)
#

$(CMD):		$(OBJS)
	$(F90) -o $(@) $(F90_32BITS) $(LOADFLAG) $(OBJS) $(LIBS)

.SUFFIXES:	.f90 .f .o
.f90.o:
	$(F90) $(LOADFLAG) -c $<

clean: 
	-rm -f *.o
	-rm -f *.mod
