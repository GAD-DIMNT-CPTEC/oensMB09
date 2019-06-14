#
#   File     :  makeprobagr
#
#   This makefile creates optimized executables of the week mean precipitation probability
#
#   Set object files.
#
#
OBJM=	GaussRep.f90 probagr.f90 
        
             
OBJS=	GaussRep.o probagr.o


#
#   Set compiler options.
#
F90 =	     sxf90 -V
LDFLAGS =    -float0 -ew     
LIBS =          
INCLUDES=    
MODULES=    
CMD =        /gfs/home3/modoper/tempo/global/oenspro/produtos/probagr/SX6/bin/probagr.exe
F90FLAGS =   -float0 -ew
FPFLAGS =	
#LD_LIBRARY_PATH =	/rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2
#
#   Define three executable makes.
#      all :  "optimized" executable 
#
all:		$(CMD)
#

$(CMD):		$(OBJS)
	$(F90) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS) 


.SUFFIXES:	.f90 .f .o
.f90.o:
	$(F90) $(LDFLAGS) -c $<

clean: 
	-rm -f *.[omjl]
