#
#   File     :  makeprobability
#
#   This makefile creates optimized executables of the probability
#
#   Set object files.
#
#

OBJM=	 GaussRep.f90 ReadFields.f90 probability.f90
        
             
OBJS=	 GaussRep.o ReadFields.o probability.o

#
#   Set compiler options.
#
F90 =		   ftn
AR=                ar
LDFLAGS =          -hbyteswapio
F90_32FLAGS =      -hbyteswapio #-Wf"-pvctl noassume vwork=stack"
LOADFLAG=          #-Wl"-Z 4G" 
LIBS=              ../../pos/source/w3lib-1.4/libw3.a
F90FLAGS =         -hbyteswapio # -float0 -ew
CMD =              ../bin/probability.exe
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
