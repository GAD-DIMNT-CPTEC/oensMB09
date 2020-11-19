OBJS = m_gauss2lats.o spread2table.o
FC = gfortran -fconvert='big-endian' -fbacktrace -g -O0 -Wall 
RM = rm

main: $(OBJS)
	$(FC) -o spread2table.x $(OBJS) 

%.o: %.F90
	$(FC) -c $<

%.o: %.f90
	$(FC) -c $<

clean:
	$(RM) *.o *.mod spread2table.x
