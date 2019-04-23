#
#   File     :  makespread
#
#   This makefile creates optimized executables for the clustering
#
#   Set object files.
#
#

OBJM=	 GaussRep.f90 spread.f90
        
             
OBJS=	 GaussRep.o  spread.o

#
#   Set compiler options.
#
F90 =		  ftn
AR=               ar
F90_32FLAGS =    -hbyteswapio #-Wf"-pvctl noassume vwork=stack"
LOADFLAG=        -hbyteswapio#-Wl"-Z 4G" 
LIBS=             ../../pos/source/w3lib-1.4/libw3.a
CMD =             ../bin/spread.exe
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
