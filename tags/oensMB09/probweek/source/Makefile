#
#   File     :  makeprobweek
#
#   This makefile creates optimized executables of the week precipitation probability
#
#   Set object files.
#
#
OBJM=	GaussRep.f90 ReadFields.f90 probweek.f90 
        
             
OBJS=	GaussRep.o ReadFields.o probweek.o


#
#   Set compiler options.
#
F90 =		  ifort
AR=               ar
LDFLAGS =        -convert big_endian 
F90_32FLAGS =    -convert big_endian #-Wf"-pvctl noassume vwork=stack"
LOADFLAG=        -convert big_endian #-Wl"-Z 4G" 
LIBS=              ../../pos/source/w3lib-1.4/libw3.a
F90FLAGS =        -convert big_endian 
CMD =              ../bin/probweek.exe
LD_LIBRARY_PATH =  /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2
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
