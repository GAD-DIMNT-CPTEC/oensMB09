#
#   File     :  makeplumes
#
#   This makefile creates optimized executables of the plumes
#
#   Set object files.
#
#

OBJM=	 plumes.f90
        
             
OBJS=	 plumes.o

#
#   Set compiler options.
#
F90 =		 ftn
AR=               ar
LDFLAGS =         -g -byteswapio
F90_32FLAGS =     -g -byteswapio# -Wf"-pvctl noassume vwork=stack"
LOADFLAG=         -g -byteswapio # -Wl"-Z 4G" 
LIBS=              ../../libs/w3lib-1.4/libw3.a
F90FLAGS =        
CMD =              ../bin/plumes.x
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
