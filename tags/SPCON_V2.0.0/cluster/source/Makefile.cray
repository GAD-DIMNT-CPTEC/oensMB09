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
F90 =		  ftn
AR=               ar
LDFLAGS =        -hbyteswapio # -s real64 -s integer64
F90_32FLAGS =    -hbyteswapio # -s real64 -s integer64#-Wf"-pvctl noassume vwork=stack"
LOADFLAG=        -hbyteswapio # -s real64 -s integer64#-Wl"-Z 4G" 
LIBS=            ../../pos/source/w3lib-1.4/libw3.a
F90FLAGS =       -hbyteswapio # -s real64 -s integer64#-float0 -ew
CMD =            ../bin/cluster.exe
LD_LIBRARY_PATH =  /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2
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
