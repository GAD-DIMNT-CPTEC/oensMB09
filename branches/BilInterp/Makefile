#OBJS = map_utils.o bilinear_interpolation.o interpola_2d.o 
OBJS = coord_compute.o BilinInterp.o interpola_2d-tloop.o 


FC = ftn

RM = rm

main: $(OBJS)

	$(FC) -o interpola_2d-tloop.x $(OBJS)

%.o: %.F90

	$(FC) -c $<

%.o: %.f90

	$(FC) -c $<

clean:

	$(RM) *.o *.mod interpola_2d-tloop.x
