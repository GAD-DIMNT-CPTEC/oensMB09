#
#  $Author: panetta $
#  $Date: 2007/08/12 13:52:18 $
#  $Revision: 1.17 $
#
#  PC Cluster, Intel compiler, mpi scali
#
#FTRACE= 
#OPENMP=
#F90=mpif90 -ccl ifort $(FTRACE) $(OPENMP) 
#NOASSUME=
#INLINE_CU_GRELL=
#INLINE_CU_KUO=
#INLINE_CU_RAS=
#INLINE_SOUZA=
#LOADFLAG=
#
#  SX6
#
FTRACE= 
OPENMP=
F90=sxmpif90 $(FTRACE) $(OPENMP)
NOASSUME=-Wf"-pvctl noassume vwork=stack"
INLINE_CU_GRELL=-pi exp=es5
INLINE_CU_KUO=-pi exp=es
INLINE_CU_RAS=-pi exp=es3
INLINE_SOUZA=-pi exp=es2
LOADFLAG= -Wl"-Z 4G" 
#
#  TX7
#
#FTRACE=
#OPENMP=
#F90=/opt/NECmpi/bin/mpif90 $(FTRACE) $(OPENMP)
#NOASSUME=
#INLINE_CU_GRELL=
#INLINE_CU_KUO=
#INLINE_CU_RAS=
#INLINE_SOUZA=
#LOADFLAG=-static
#
#  Executable
#
#EXEC=/gfs/dk12/pkubota/MPIGlobal_CVS/model/exec/ParModel_MPI
EXEC=../exec/ParModel_MPI


OBJ= \
	Constants.o \
	Cu_Grellens.o\
	Cu_Kuolcl.o \
	Cu_RAS.o\
	Clirad.o \
	Convection.o \
	Communications.o \
	Diagnostics.o \
	FieldsDynamics.o \
	FieldsPhysics.o \
	GridDynamics.o \
	GridHistory.o \
	Init.o \
	InputOutput.o  \
	IOLowLevel.o \
	Model.o \
	NonLinearNMI.o \
	Options.o \
	Parallelism.o \
	PhysicsDriver.o \
	PlanBoundLayer.o \
	Prec_LrgScl.o \
	Radiation.o \
	SemiLagrangian.o \
	Shall_Souza.o\
	Shall_Tied.o \
	Sizes.o \
	SpecDynamics.o \
	Surface.o \
	Sfc_SSiB.o \
	TimeStep.o \
	Transform.o \
	Utils.o \
	Watches.o

model:	$(OBJ)
	$(F90) -o $(EXEC) $(LOADFLAG) $(OBJ) 

Constants.o	:	Constants.f90 
	$(F90) -c Constants.f90


Convection.o	:	Convection.f90 Constants.o Diagnostics.o \
	 		GridHistory.o Options.o Init.o Cu_Kuolcl.o \
			Cu_Grellens.o Shall_Tied.o Shall_Souza.o Cu_RAS.o \
			Prec_LrgScl.o
	$(F90) -c Convection.f90

Communications.o	:	Communications.f90  Sizes.o Options.o \
				Constants.o Parallelism.o
	$(F90) -c Communications.f90

Cu_Grellens.o	:	Cu_Grellens.f90 Constants.o Options.o
	$(F90) -c ${INLINE_CU_GRELL}  $(NOASSUME) Cu_Grellens.f90


Cu_Kuolcl.o	:	Cu_Kuolcl.f90 Constants.o Options.o
	$(F90) -c ${INLINE_CU_KUO}  $(NOASSUME) Cu_Kuolcl.f90


Cu_RAS.o	:	Cu_RAS.f90 Constants.o  Options.o
	$(F90) -c ${INLINE_CU_RAS}  $(NOASSUME) Cu_RAS.f90

Diagnostics.o	:	Diagnostics.f90 Constants.o Parallelism.o \
	 IOLowLevel.o  Options.o Utils.o Communications.o Sizes.o  InputOutput.o
	$(F90) -c Diagnostics.f90


FieldsDynamics.o	:	FieldsDynamics.f90 Constants.o 
	$(F90) -c FieldsDynamics.f90

FieldsPhysics.o	:	FieldsPhysics.f90 Options.o IOLowLevel.o \
	 Sfc_SSiB.o Utils.o Sizes.o Constants.o InputOutput.o
	$(F90) -c FieldsPhysics.f90


GridDynamics.o	:	GridDynamics.f90 Sizes.o FieldsDynamics.o \
			Constants.o SpecDynamics.o PhysicsDriver.o \
			GridHistory.o Diagnostics.o Utils.o Parallelism.o\
			FieldsPhysics.o Options.o
	$(F90) -c GridDynamics.f90


GridHistory.o	:	GridHistory.f90 Constants.o Parallelism.o \
			Options.o Utils.o IOLowLevel.o Communications.o Sizes.o InputOutput.o
	$(F90) -c GridHistory.f90


Init.o	:	Init.f90 Utils.o Constants.o FieldsDynamics.o \
		IOLowLevel.o SpecDynamics.o Sizes.o Transform.o Options.o Parallelism.o
	$(F90) -c Init.f90


InputOutput.o	:	InputOutput.f90 Constants.o Options.o Utils.o IOLowLevel.o \
		Communications.o Sizes.o Parallelism.o
	$(F90) -c InputOutput.f90


PhysicsDriver.o	:	PhysicsDriver.f90 Constants.o Sizes.o FieldsPhysics.o\
			Convection.o Init.o Radiation.o Sfc_SSiB.o \
			Diagnostics.o PlanBoundLayer.o GridHistory.o Options.o \
			Surface.o
	$(F90) -c PhysicsDriver.f90

Model.o		:	Model.f90 Utils.o FieldsDynamics.o Init.o \
			NonLinearNMI.o FieldsPhysics.o SemiLagrangian.o \
			InputOutput.o Options.o GridHistory.o Diagnostics.o \
			SpecDynamics.o TimeStep.o IOLowLevel.o Convection.o \
			Sizes.o Radiation.o PlanBoundLayer.o Surface.o \
			Parallelism.o Constants.o PhysicsDriver.o Watches.o
	$(F90) -c $(NOASSUME) Model.f90

NonLinearNMI.o	:	NonLinearNMI.f90 Sizes.o FieldsDynamics.o FieldsPhysics.o \
			IOLowLevel.o Constants.o SpecDynamics.o TimeStep.o \
			Options.o Utils.o \
			Parallelism.o InputOutput.o
	$(F90) -c NonLinearNMI.f90


Options.o	:	Options.f90 Constants.o Parallelism.o
	$(F90) -c Options.f90

PlanBoundLayer.o	:	PlanBoundLayer.f90  Constants.o Options.o
	$(F90) -c PlanBoundLayer.f90


Prec_LrgScl.o	:	Prec_LrgScl.f90 Constants.o Options.o
	$(F90) -c Prec_LrgScl.f90


Radiation.o	:	Radiation.f90 Clirad.o Constants.o \
			Options.o Utils.o
	$(F90) -c Radiation.f90


Clirad.o	:	Clirad.f90  Constants.o  Options.o \
	coa.data90 cah.data90 mcai.data90
	$(F90) -c Clirad.f90

SemiLagrangian.o	:	SemiLagrangian.f90 Constants.o FieldsDynamics.o \
				Sizes.o Utils.o Communications.o Options.o Parallelism.o
	$(F90) -c SemiLagrangian.f90


Shall_Souza.o	:	Shall_Souza.f90 Constants.o Options.o
	$(F90) -c ${INLINE_SOUZA}  $(NOASSUME) Shall_Souza.f90


Shall_Tied.o	:	Shall_Tied.f90 Constants.o Options.o
	$(F90) -c $(NOASSUME) Shall_Tied.f90



Sizes.o	:		Sizes.f90 Constants.o Options.o
	$(F90) -c Sizes.f90


SpecDynamics.o	:	SpecDynamics.f90 Constants.o FieldsDynamics.o \
			Sizes.o Options.o Utils.o Diagnostics.o Parallelism.o
	$(F90) -c SpecDynamics.f90


Surface.o	:	Surface.f90 Constants.o  Sfc_SSiB.o \
			GridHistory.o Diagnostics.o Options.o
	$(F90) -c Surface.f90

Sfc_SSiB.o	:	Sfc_SSiB.f90 Constants.o Options.o
	$(F90) -c Sfc_SSiB.f90


IOLowLevel.o	:	IOLowLevel.f90 Constants.o Options.o
	$(F90) -c IOLowLevel.f90


TimeStep.o	:	TimeStep.f90 Constants.o FieldsDynamics.o Options.o \
			Utils.o GridDynamics.o Transform.o SpecDynamics.o \
			SemiLagrangian.o  Sizes.o  Radiation.o IOLowLevel.o \
			FieldsPhysics.o PhysicsDriver.o Communications.o \
			Parallelism.o
	$(F90) -c TimeStep.f90


Transform.o	:	Transform.f90 Constants.o Utils.o Sizes.o \
			Options.o Parallelism.o
	$(F90) -c $(NOASSUME) Transform.f90


Utils.o		:	Utils.f90 Sizes.o Constants.o IOLowLevel.o Options.o
	$(F90) -c Utils.f90


.SUFFIXES:
.SUFFIXES: .f90 .o


.f90.o :
	$(F90) -c  $<


clean:
	-rm -f $(OBJ) 
	-rm -f $(EXEC) 
	-rm -f *.mod
