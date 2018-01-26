#!/bin/bash
# 
# script to run CPTEC Global Model on PC Clusters under MPI Scali
# and Sun Grid Engine without OpenMP
#
# assumptions: assume present at the same directory:
#              ParModel_MPI (Global Model Executable file)
#              MODELIN (Global Model input Namelist file)
#
# usage: run_multi_UNA cpu_mpi cpu_node name hold
# where:
# cpu_mpi: integer, the desired number of mpi processes
# cpu_node: integer, the desired number of mpi processes per shared memory node
# name: character, the job name (for SGE)
# hold: any, present or not;
#            if absent, script finishes after queueing job;
#            if present, script holds till job completion
if [ "$#" == 4 ]
then hold=""
else hold=
fi
export FEXE=`pwd`
export cpu_mpi=$1
export cpu_node=$2
export RES=$3
num=$(($cpu_mpi+$cpu_node-1))
fra=$(($num/$cpu_node))
cpu_tot=$(($fra*$cpu_node))
echo fila=mpi-npn${cpu_node} total cpus=${cpu_tot}

############################################################
#export dirhome=/stornext/home/paulo.kubota/agcm/pre
#export dirdata=/scratch1/home/paulo.kubota/agcm
#export dirgrads=/usr/local/grads
#
## Machine options: SX6; Linux
#export MAQUI=Linux
#
## Set  Res for Chopping
#export RESIN=382
#export KMIN=64
#export RESOUT=42
#export KMOUT=28
#export SetLinear=FALSE
#export RESO=42
#export IM=128
#export JM=64
#export prefix=${JM}
##set run date
#export DATA=2004032600
###############################################################

varname=SSTWeekly
ieeefiles='10,30,50,60'
#
direxe=${dirdata}/pre/exec
dirsrc=${dirhome}/sources/${varname}
dirout=${dirhome}/scripts/output
dirrun=${dirhome}/scripts
echo " "
host=`hostname`
echo " ${host}"
RUNTM=`date +'%d_%H:%M'`
#
cat <<EOT0 > ${direxe}/${varname}.nml
 &InputDim
  Mend=${RESO},             ! Spectral Resolution Horizontal Truncation
  Kmax=${KMOUT},              ! Number of Layers of the Initial Condition for the Global Model
  !Idim=360,             ! Number of Longitudes For Climatological SST Data
  !Jdim=180,             ! Number of Latitudes For Climatological SST Data
  Idim=4320,         ! Number of Longitudes at the NCEP SST Weekly Grid
  Jdim=2160,         ! Number of Latitudes at the NCEP SST Weekly Grid
  SSTSeaIce=-1.749,     ! SST Value in Celsius Degree Over Sea Ice (-1.749 NCEP, -1.799 CAC)
  LatClimSouth=-50.0,   ! Southern Latitude For Climatological SST Data
  LatClimNorth=60.0,    ! Northern Latitude For Climatological SST Data
  ClimWindow=.FALSE.,   ! Flag to Climatological SST Data Window
  Linear=.TRUE.,        ! Flag for Linear (T) or Area Weighted (F) Interpolation
  GrADS=.TRUE.,         ! Flag for GrADS Outputs
  DateICn='${DATA}', ! Date of th Initial Condition for the Global Model
  Preffix='GANLNMC',    ! Preffix of the Initial Condition for the Global Model
  Suffix='S.unf.',      ! Suffix of the Initial Condition for the Global Model
  DirMain='${dirdata}/ '             ! Main Datain/Dataout Directory
 /
EOT0
cat <<EOT1 > ${direxe}/set${varname}.bash
#!/bin/bash
#PBS -o ${direxe}/${varname}_${DATA}.out
#PBS -e ${direxe}/${varname}_${DATA}.err
#PBS -l walltime=0:01:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES
#PBS -q ${QUEUE1}

#
if [[ (${MAQUI} = "Linux") || (${MAQUI} = "linux") ]]; then
  export F_UFMTENDIAN=${ieeefiles}
  export GFORTRAN_CONVERT_UNIT=big_endian:${ieeefiles}
  echo "F_UFMTENDIAN= " ${F_UFMTENDIAN}
  echo "GFORTRAN_CONVERT_UNIT= " ${GFORTRAN_CONVERT_UNIT}
fi
export KMP_STACKSIZE=128m
ulimit -s unlimited
#
cd ${direxe}
date
${direxe}/${varname} -i ${direxe}/${varname}.nml
date
EOT1
#
#   Change mode to be executable
#

echo ${direxe}/set${varname}.bash
echo ${direxe}/${varname}_${DATA}.out

export PBS_SERVER=aux20-eth4
chmod +x ${direxe}/set${varname}.bash
jobnumber=$(qsub ${direxe}/set${varname}.bash  | cut -d. -f1)
it=2
while [ ${it} -gt 0 ];do
   it=`qstat | grep $jobnumber | wc -l`
done