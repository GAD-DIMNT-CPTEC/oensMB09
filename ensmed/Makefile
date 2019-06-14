#
#   File     :  makeensmed
#
#   This makefile creates optimized executables of the ensemble mean
#
#   Set object files.
#
#
OBJM=	ensmed.f90 
        
             
OBJS=	ensmed.o


#
#   Set compiler options.
#
F90 =		  sxmpif90 
LDFLAGS =     	 
F90_32FLAGS =    -Wf"-pvctl noassume vwork=stack"
LOADFLAG=        -Wl"-Z 4G" 
LIBS=             ../w3lib-1.1/libw3.a
CMD =             ./ensmed.exe
LD_LIBRARY_PATH = /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2
#
#   Define three executable makes.
#      all :  "optimized" executable 
#
all:		$(CMD)
#

$(CMD):		$(OBJS)
	$(F90) -o $(@) $(LDFLAGS) $(F90_32BITS) $(LOADFLAG) $(OBJS) $(LIBS)


.SUFFIXES:	.f90 .f .o
.f90.o:
	$(F90) -c $<

clean: 
	-rm -f *.[omjl]
