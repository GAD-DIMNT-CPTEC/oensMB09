#
#  $Author: alexalm $
#  $Date: 2005/10/17 14:25:38 $
#  $Revision: 1.1.1.1 $
#
F90=sxf90

FTRACE=
F90COMMONFLAGS= $(FTRACE) -P openmp -ew 
F90NOASSUME= $(FTRACE) -P openmp -ew -Wf"-pvctl noassume vwork=stack"
F90_32BITS = $(FTRACE) -P openmp 
F90RAD=-pi exp=r8btgt,r8btge,r8bteq,r8scnt,r8vctr,r8vgat,r8vsct,b8not,b8eq,i8vgat,i8vint,rvabs,rvacos,rvexp,rvlog,rvsqrt,vimax,vimin,vamax,vamin expin=ToRemove.f90
LOADFLAG= -Wl"-ZL 2G" 
EXEC=${FDBS}/model/exec/ParModel


OBJ= \
	Constants.o \
	Convection.o \
	Diagnostics.o \
	FieldsDynamics.o \
	FieldsPhysics.o \
	GridDynamics.o \
	GridHistory.o \
	InputOutput.o  \
	Options.o \
	PhysicsDriver.o \
	Init.o \
	Model.o \
	NonLinearNMI.o \
	PlanBoundLayer.o \
	Radiation.o \
	IOLowLevel.o \
	SemiLagrangian.o \
	Sizes.o \
	SpecDynamics.o \
	Surface.o \
	TimeStep.o \
	ToRemove.o \
	Transform.o \
	Utils.o
  
model:	$(OBJ)
	$(F90) -o $(EXEC) $(F90COMMONFLAGS) $(LOADFLAG) $(OBJ) 

Constants.o	:	Constants.f90 
	$(F90) -c $(F90_32BITS) Constants.f90


Convection.o	:	Convection.f90 Constants.o Diagnostics.o GridHistory.o \
	 InputOutput.o Options.o Init.o
	$(F90) -pi exp=es -c $(F90NOASSUME) Convection.f90


Diagnostics.o	:	Diagnostics.f90 InputOutput.o Constants.o \
	 IOLowLevel.o  Options.o Utils.o
	$(F90) -c $(F90COMMONFLAGS) Diagnostics.f90


FieldsPhysics.o	:	FieldsPhysics.f90 InputOutput.o  Options.o IOLowLevel.o \
	 Surface.o Utils.o GridHistory.o Sizes.o Constants.o
	$(F90) -c $(F90COMMONFLAGS) FieldsPhysics.f90


GridDynamics.o	:	GridDynamics.f90 Sizes.o FieldsDynamics.o \
			Constants.o SpecDynamics.o SemiLagrangian.o \
			PhysicsDriver.o GridHistory.o Diagnostics.o \
			FieldsPhysics.o Options.o
	$(F90) -c $(F90COMMONFLAGS) GridDynamics.f90


GridHistory.o	:	GridHistory.f90 InputOutput.o Options.o Utils.o
	$(F90) -c $(F90COMMONFLAGS) GridHistory.f90


Init.o	:	Init.f90   Utils.o Constants.o  \
	FieldsDynamics.o  SpecDynamics.o  Sizes.o  Transform.o 
	$(F90) -c $(F90COMMONFLAGS) Init.f90


InputOutput.o	:	InputOutput.f90 Utils.o IOLowLevel.o
	$(F90) -c $(F90COMMONFLAGS) InputOutput.f90


PhysicsDriver.o	:	PhysicsDriver.f90  Sizes.o \
	 FieldsPhysics.o Convection.o Constants.o Radiation.o Surface.o \
	Diagnostics.o PlanBoundLayer.o GridHistory.o \
	InputOutput.o Options.o Init.o
	$(F90) -c $(F90COMMONFLAGS) PhysicsDriver.f90

Model.o	:	Model.f90   Utils.o  FieldsDynamics.o  \
	Init.o  NonLinearNMI.o FieldsPhysics.o Constants.o\
	SemiLagrangian.o InputOutput.o Options.o GridHistory.o Diagnostics.o \
	SpecDynamics.o  TimeStep.o  IOLowLevel.o  Convection.o \
	Sizes.o  Radiation.o PlanBoundLayer.o Surface.o
	$(F90) -c $(F90COMMONFLAGS) Model.f90

NonLinearNMI.o	:	NonLinearNMI.f90 Sizes.o FieldsDynamics.o FieldsPhysics.o \
			IOLowLevel.o Constants.o SpecDynamics.o TimeStep.o \
			Options.o InputOutput.o Diagnostics.o Utils.o
	$(F90) -c $(F90COMMONFLAGS) NonLinearNMI.f90


PlanBoundLayer.o	:	PlanBoundLayer.f90  Constants.o 
	$(F90) -c $(F90NOASSUME) PlanBoundLayer.f90


Radiation.o	:	Radiation.f90 Constants.o Diagnostics.o Utils.o \
			Options.o Init.o
	$(F90) -c $(F90RAD) $(F90NOASSUME) Radiation.f90


SemiLagrangian.o	:	SemiLagrangian.f90   Constants.o FieldsDynamics.o  \
				Sizes.o  Utils.o
	$(F90) -c $(F90COMMONFLAGS) SemiLagrangian.f90


Sizes.o	:	Sizes.f90 Constants.o
	$(F90) -c $(F90_32BITS) Sizes.f90


SpecDynamics.o	:	SpecDynamics.f90   Constants.o FieldsDynamics.o  Sizes.o  \
			Options.o Utils.o 
	$(F90) -c $(F90NOASSUME) SpecDynamics.f90


Surface.o	:	Surface.f90 Constants.o InputOutput.o 
	$(F90) -c $(F90COMMONFLAGS) Surface.f90


IOLowLevel.o	:	IOLowLevel.f90
	$(F90) -c $(F90_32BITS) IOLowLevel.f90


TimeStep.o	:	TimeStep.f90   Constants.o FieldsDynamics.o  \
	Utils.o  GridDynamics.o  Transform.o SpecDynamics.o  Options.o\
	SemiLagrangian.o  Sizes.o  Radiation.o \
	FieldsPhysics.o PhysicsDriver.o IOLowLevel.o 
	$(F90) -c $(F90COMMONFLAGS) TimeStep.f90


TransDiagCol.o	:	TransDiagCol.f90   Sizes.o
	$(F90) -c $(F90COMMONFLAGS) TransDiagCol.f90


Transform.o	:	Transform.f90 Constants.o Utils.o Sizes.o
	$(F90) -c $(F90COMMONFLAGS) Transform.f90


Utils.o	:	Utils.f90 Sizes.o Constants.o
	$(F90) -c $(F90COMMONFLAGS) Utils.f90


.SUFFIXES:
.SUFFIXES: .f90 .o


.f90.o :
	$(F90) -c $(F90COMMONFLAGS) $<


clean:
	-rm -f $(OBJ) 
	-rm -f $(EXEC) 
	-rm -f *.mod
