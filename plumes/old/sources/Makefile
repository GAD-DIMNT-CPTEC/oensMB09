#
#   File     :  makepost
#
#   This makefile creates optimized executables of the probability plumes
#
#   Set object files.
#
#
OBJM=	geral.f90 globvars.f90 config.f90 wout_ctl.f90 plumas.f90 \
	chekplumas.f90 cont_ctl.f90 rwgrhens_luf.f90
        
             
OBJS=	geral.o globvars.o config.o wout_ctl.o plumas.o \
	chekplumas.o cont_ctl.o  rwgrhens_luf.o      


#
#   Set compiler options.
#
F90 =		f90 
LDFLAGS =     	-P openmp  
LIBS =          
INCLUDES=       ${OPERMOD}/plumes/sources
MODULES=        ${OPERMOD}/plumes/sources
CMD =           ${OPERMOD}/plumes/exec/gerapluma.x
F90FLAGS =	-P openmp -R5 
FPFLAGS =	
LD_LIBRARY_PATH =	/rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2
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
	$(F90) $(F90FLAGS) -c $<

clean: 
	-rm -f *.[omjl]
