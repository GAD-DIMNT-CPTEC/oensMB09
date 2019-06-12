#
#   File     :  makeaccumulated
#
#   This makefile creates optimized executables of the accumulated
#
#   Set object files.
#
#

OBJM=	 GaussRep.f90 ReadFields_accum.f90 HorizontalInterp.f90 accumulated.f90
             
OBJS=	 GaussRep.o ReadFields_accum.o HorizontalInterp.o accumulated.o

#
#   Set compiler options.
#
#SX6 Options
#F90 =		  sxf90
#AR=               sxar
#LDFLAGS =        -float0 -ew
#F90_32FLAGS =    -Wf"-pvctl noassume vwork=stack"
#LOADFLAG=        -Wl"-Z 4G" 
#LIBS=             /gfs/home1/prestrel/w3lib-1.1/libw3.a
#F90FLAGS =        -float0 -ew

#TX7 INTEL Options
F90 =             ifort -O 
AR=               ar
F90_32FLAGS =    
LOADFLAG=        -static
LIBS=             ../w3lib-1.4/libw3.a

CMD =             ../bin/accumulated.exe
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
	-rm -f *.mod		
	-rm -f *.nml
	-rm -f *.out		
	-rm -f *.exe
