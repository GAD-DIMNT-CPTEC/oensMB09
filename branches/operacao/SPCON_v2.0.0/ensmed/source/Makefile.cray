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
F90 =	     ftn -hbyteswapio -Rb -O nomodinline -emacdE -rd 
LDFLAGS =     
F90_32FLAGS = -Wf"-pvctl noassume vwork=stack"
LOADFLAG=     
LIBS=         ../../pos/source/w3lib-1.4/libw3.a
CMD =             ../bin/ensmed.exe
LD_LIBRARY_PATH = /rlib/lib2:/rlib/usr/lib2/lib2:/usr/lib2

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
