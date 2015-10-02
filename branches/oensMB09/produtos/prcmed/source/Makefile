#
#   File     :  makeprcmed
#
#   This makefile creates optimized executables of the week mean precipitation
#
#   Set object files.
#
#
OBJM=	GaussRep.f90 ReadFields.f90 prcmed.f90 
        
             
OBJS=	GaussRep.o ReadFields.o prcmed.o


#
#   Set compiler options.
#
F90 =		  ftn 
AR=               ar
LDFLAGS =        
F90_32FLAGS =    -byteswapio #-Wf"-pvctl noassume vwork=stack"
LOADFLAG=        -byteswapio #-Wl"-Z 4G" 
LIBS=             ../../../pos/source/w3lib-1.4/libw3.a
F90FLAGS =       #-float0 -ew
CMD =              /scratch1/oper/tempo/oens_MCGA/produtos/prcmed/bin/prcmed.x

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
