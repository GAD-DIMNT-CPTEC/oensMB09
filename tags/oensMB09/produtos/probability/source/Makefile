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
LDFLAGS =          -byteswapio 
F90_32FLAGS =      -O2 -byteswapio #-Wf"-pvctl noassume vwork=stack"
LOADFLAG=          -O2 -byteswapio#-Wl"-Z 4G" 
LIBS=              ../../../pos/source/w3lib-1.4/libw3.a
F90FLAGS =         -O2 -byteswapio  # -float0 -ew
CMD =              /scratch1/home/oper/oens_MCGA/produtos/probability/bin/probability.x
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
