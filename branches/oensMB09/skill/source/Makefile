#
#   File     :  makecluster
#
#   This makefile creates optimized executables for the clustering
#
#   Set object files.
#
#

OBJM=	DayintyGau.f90

OBJS=	DayintyGau.o

#
#   Set compiler options.
#
F90 =		  ifort
AR=               ar
LDFLAGS =        -convert big_endian -Vaxlib -static
F90_32FLAGS =    -convert big_endian -Vaxlib -static#-Wf"-pvctl noassume vwork=stack"
LOADFLAG=        -convert big_endian -Vaxlib -static#-Wl"-Z 4G" 
LIBS=            
F90FLAGS =       -convert big_endian #-float0 -ew
CMD =            ../bin/skill.exe
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
