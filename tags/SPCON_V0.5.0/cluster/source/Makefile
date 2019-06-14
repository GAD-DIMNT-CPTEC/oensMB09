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
F90 =		  sxf90 -V
AR=               sxar
LDFLAGS =        -float0 -ew
F90_32FLAGS =    -Wf"-pvctl noassume vwork=stack"
LOADFLAG=        -Wl"-Z 4G" 
LIBS=             ../../w3lib-1.1/libw3.a
F90FLAGS =        -float0 -ew
CMD =              ../bin/cluster.exe
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
	-rm -f *.[omjl]
