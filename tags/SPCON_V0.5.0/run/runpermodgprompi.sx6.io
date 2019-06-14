#!/bin/ksh
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runmodg.ksh                                       #
#                                                                       #
#     Function:       This script submits the global                    #
#                     model script to the NQS queue.                    #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           October 08th, 2002.                               #
#     Last change:    October 08th, 2002.                               #
#                                                                       #
#     Valid Arguments for runmodg.sx6:                                  #
#                                                                       #
#                   WARNING:                                            #
#     The parameters are just to name of files.                         #
#     They are not enough to change resolution model run.               #
#     It is necessary to change the namelist TEMPLATE_MODELIN.          #
#                                                                       #
#     First:        TRC: three-digit triangular truncation              #
#    Second:         LV: two-digit number of vertical sigma-layers      #
#                                                                       #
#***********************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
else
  TRC=${1}  
fi
if [ -z "${2}" ]
then
  echo "LV is not set" 
  exit 2
else
  LV=${2}  
fi
if [ -z "${3}" ]
then
  echo "LABELI is not set" 
  exit 3
else
  export LABELI=${3}  
fi
if [ -z "${4}" ]
then
  echo "LABELF is not set" 
  exit 3
else
  export LABELF=${4}  
fi
if [ -z "${5}" ]
then
  echo "DHFCT is not set" 
  exit 3
else
  export DHFCT=${5}  
fi

if [ -z "${6}" ]
then
  echo "PERT ID is not set" 
  exit 3
else
  export PERT=${6}  
fi

#
# SETTING THE APPROPRIATED ENVIRONMENT
#
#. ${HOME}/configenvT213.ksh

#
#   Set nproc, resol, host, machine, NQS Queue and Run time
#
NPROC=8
RESOL=T${TRC}L${LV}
HSTMAQ=`hostname`
RUNTM=`date +'%Y%m%d%T'`
typeset -Z4 yi yf
typeset -Z2 mi di hi mf df hf

let yi=`awk 'BEGIN {print substr("'${LABELI}'",1,4)}'` ; export yi
let mi=`awk 'BEGIN {print substr("'${LABELI}'",5,2)}'` ; export mi
let di=`awk 'BEGIN {print substr("'${LABELI}'",7,2)}'` ; export di
let hi=`awk 'BEGIN {print substr("'${LABELI}'",9,2)}'` ; export hi
let yf=`awk 'BEGIN {print substr("'${LABELF}'",1,4)}'` ; export yf
let mf=`awk 'BEGIN {print substr("'${LABELF}'",5,2)}'` ; export mf
let df=`awk 'BEGIN {print substr("'${LABELF}'",7,2)}'` ; export df
let hf=`awk 'BEGIN {print substr("'${LABELF}'",9,2)}'` ; export hf

#mkdir -p ${T213HSM}/GFCT/${yi}${mi}${di}
#mkdir -p ${T213HSM}/GFGH/${yi}${mi}

#
# Defining the label date to remove old files
# labelr: to remove the run made 24 hours ago
#
export labelr=`~/bin/caldate.3.0.1 ${LABELI} - 24h 'yyyymmddhh'`
print "Old run to remove: " ${labelr}

#print rm -f ${MODATAOUT}/${NAMEF}${labelr}*
#rm -f ${MODATAOUT}/${NAMEF}${labelr}*
#rm -f ${MODATAOUT}/GFGHAVN${labelr}*
#find ${MODATAOUT} -name "GFGHAVN${labelr}*" -mtime +1 -exec rm -f {} \;

#########################################################
#
#      SCRIPT FOR GLOBAL MODEL PRODUCTION RUNS 
#
#########################################################

#
# Step 1: Set Directories and files:
#
#   DIRBASE is the root directory path; 
#           all files belong to subdirectories of root;
#   EXECFILEPATH is the executable filename (with path)
#   SCRIPTFILEPATH is the script file that submits executable (with path)
#   NAMELISTFILEPATH contains the namelist file read by the executable (with path)
#   OUTPUTFILEPATH is the executable output file (with path)
#
. ../include/config.sx6
DIRBASE=$OPERM
EXECFILEPATH=${DIRBASE}/model/exec/ParModel_MPI
SCRIPTFILEPATH=${DIRBASE}/run/setctrmodg.${PREFX}.${RESOL}.${MAQUI}
NAMELISTFILEPATH=${DIRBASE}/model/datain/MODELIN${PERT}
OUTPUTFILEPATH=${DIRBASE}/run/setout/modg${RESOL}${PREFX}.${MAQUI}.${RUNTM}.out
FSCR=${OPERM}/run
FEXE=${DIRBASE}/model/exec

if [ "$PERT" = "AVN" ]; then
echo "+++ENTREI RUNMODGPROMPI.SX6 +++"
      node=1
      cpu0=$NPROC
      cpu1=0
      cpu2=0
      cpu3=0
      CpuPerNode=$NPROC
      TaskPerProc=1
      MemPerNode=5gb
else
      node=1
      cpu0=2
      cpu1=0
      cpu2=0
      cpu3=0
      CpuPerNode=2
      TaskPerProc=1
      MemPerNode=4gb
fi

BUFFER=20480
cputime=`echo "1*1*3600" | bc -l`
nnode=`expr ${node} - 1`
cpu=`expr ${cpu0} + ${cpu1} + ${cpu2} + ${cpu3}`
data=`date +'%Y%m%d%H%M%s'`

# Initial label
HHi=`echo $LABELI | cut -c 9-10`
DDi=`echo $LABELI | cut -c 7-8`
MMi=`echo $LABELI | cut -c 5-6`
YYYYi=`echo $LABELI | cut -c 1-4`
# Final label
HHo=`echo $LABELF | cut -c 9-10`
DDo=`echo $LABELF | cut -c 7-8`
MMo=`echo $LABELF | cut -c 5-6`
YYYYo=`echo $LABELF | cut -c 1-4`

echo "NAMELIST - "${NAMELISTFILEPATH}

if [ `echo $PERT | grep "R" | wc -l` -ge 1 ]; then
      DHFCT=3
else
      DHFCT=6
fi


cat <<EOT1 > ${NAMELISTFILEPATH}
!namelist
!############################### Change Log ##################################
! 1.0.0.0
!
!  \$Author: alexalm $
!  \$Date: 2005/10/17 14:24:56 $
!  \$Revision: 1.1.1.1 $
!
!
!#############################################################################
!
&MODEL_RES
 TRUNC    = ${TRC},
 VERT     = ${LV},
 DT       = 600.0,
 IDATEI   = ${HHi},${DDi},${MMi},${YYYYi},
 IDATEW   = ${HHo},${DDo},${MMo},${YYYYo},
 IDATEF   = ${HHo},${DDo},${MMo},${YYYYo},
                           !	    if cold, then LABELF=LABELC
 NMSST    = 'sstwkl',
 DHFCT    = ${DHFCT},
		           !	    equal zero to use default list	   
 DHRES    = 0,         !DHRES : interval in hours to output restart,	 
		           !	    equal zero to use default list	
 DHDHN    = 0,
 NHDHN    = 0,
		           !	    equal zero to not execute DHN diagnostics	 
 DHEXT    = 0,
 NHEXT    = 0,
		           !	    equal zero to not execute extra diagnostics  
 DOGRH	  = .FALSE.,       !DOGRH : logical (T or F) to do grid history for	 
		           !	    selected points
 DOPRC	  = .FALSE.,       !DOPRC : logical (T or F) to do time step output
		           !	    of global precipitation	  
 PREFX    = '${PERT}',
 PREFY    = '${PERT}',
 TABLE    = 'n',           !TABLE : indicator of the desire table:	  
		           !	   	  n => for desirtable (default) 	       
		           !	   	  p => for desirtable.pnt		       
		           !	   	  c => for desirtable.clm		       
		           !	   	   LABELx: hhddmmyyyy				       
		           !	   		   yyyy = four digit year		       
		           !	   		     mm = two digit month		       
		           !	   		     dd = two digit day 		       
		           !	   		     hh = two digit hour		       
 NMSST    = 'sstwkl',
		           !sstaoi : for optimum interpolated SST  	       
		           !	     climatology (original 1x1 degree)  	  
		           !sstanp : for optimum interpolated SST		  
		           !	     climatology plus persisted SST anomaly	  
		           !sstwkl : for weekly run mean of SST for		  
		           !	     the week finished at initial day		  
		           !	     minus one (original 1x1 degree)		  
		           !sstwkd : for weekly run mean of SST 		  
		           !	     direct access file (original 1x1 degree)	  
		           !sstmtd : for monthly run mean of SST		  
		           !	     direct access file (original 1x1 degree)  
path_in       = '$IOPERM',
dirfNameOutput= '$ROPERM/model/dataout/T${TRC}L${LV}',
/
 &MODEL_IN
 slagr         =.FALSE.,  ! Semi-Lagrangian option (.FALSE. for Eulerian Model)
 nlnminit      =.TRUE.,   ! do normal mode non linear initialization
 diabatic      =.TRUE.,   ! diabatic or not initialization (.FALSE. if nlnminit=.FALSE.)
 eigeninit     =.FALSE.,  ! eigenInit  --> .FALSE. 
 rsettov       =.TRUE.,   ! rsettov    --> .TRUE.
 intcosz       =.TRUE.,   ! ntcosz     --> .TRUE.
 Model1D       =.FALSE.,  ! .TRUE. when using 1D Model.
 reducedGrid   =.TRUE.,   !  reduced    --> .TRUE. 
 linearGrid    =.FALSE.,  !  lineargrid --> .FALSE.
 GenRestFiles  =.FALSE.,  ! .TRUE. to generate restart files.
 rmRestFiles   =.TRUE.,   ! .TRUE. to remove restart files after read them.
 MasCon        =.FALSE.   ! .TRUE. to do Mass Conservation: ln(ps)
 tamBlock      = 512      ! quantidade de fft enviadas por bloco   
/

&PHYSPROC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     ixxxx=yes  the physical process included
!     ixxxx=no   the physical process excluded
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ISWRAD  = 'LCH'   ! iswrad = NON: don't do sw radiation, LCH: lacis & hansen
                   !          CRD: clirad                UKM: ukmet office
 ILWRAD  = 'HRS'   ! ilwrad = NON: don't do lwradiation, HRS: Harshvardhan
                   !          CRD: clirad                UKM: ukmet office
 ICCON   = 'KUO',  ! iccon=KUO:cumulus convection(kuo)
                   ! iccon=ARA:cumulus convection(ARAKAWA)
                   ! iccon=GRE:cumulus convection(grell)
 ISCON   = 'TIED', ! iscon=TIED:shallow convection this process follows cumulus 
 
 
                   !            convection ()
		       ! iscon=SOUZ:cumulus heating and moistening tendencies
                   ! Enio Pereira de Souza 12/Jul/2001 
 ILCON   = 'YES',  ! ilcon=yes:large scale condensation
 IQDIF   = 'YES', ! iqdif=yes:horizontal diffusion of moisture
 IGWD    = 'YES', ! igwd =yes:gravity wave 
 ISIMP   = 'NO',  ! isimp=yes:simplified physics version. 
 ENHDIF  = 'YES', ! enhdif=yes: enhance diffusion on higher levels )
 masci   = 0,      ! 1 Mass conservation (ps) in hours(for MasCon=false)         
 ! specific for clirad
 ASOLC   = 0.22, ! continental: total column aerosol in the first 2km
 ASOLM   = 0.14, ! maritime:    total column aerosol in the first 2km
 CRDCLD  = 1,       ! cloud scheme =1 (old) =4 (ccm3)
 ! specific for grell
 grepar1 = 1,      ! integer: 0 ensemble 1 GRE   4 OMG   7 KUO  10 Chappel 13 ARA   24 ensemble2
 grepar2 = 3,      ! integer: number eff-ensemble(1,2,3)
 grepar3 = 85.,    ! cpmax
 grepar4 = 30.,    ! cpmax-diff
 /
&PHYSCS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   the following are vertical resolution dependent cloud parameters
!   used in cldgen.  correct settings for these parameters need to be
!   determined experimentally.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
 mxrdcc  =.true.,   ! use maximum random converage for radiative conv. clouds
 lcnvl   = 2,	    ! the lowest layer index where non-convective clouds can
 		    ! occur (ben says this should be 2 or more)
 lthncl  = 80,	    ! minimum depth in mb of non-zero low level cloud
 rccmbl  = 3.0,	    ! radiative convective cloud minimum base layer index
 swint   = 1.000000,! short wave radiation call interval in hours swint=1.0e0
 trint   = 3.000000,! ir subr. call interval in hours  trint=3.0e0
                    ! long  wave radiation call interval in hours
                    !   physical constants for simple physics options
 icld  =1,
 inalb =2,
 mxiter=200,
 co2val =345.0,     ! co2val is wgne standard value in ppm
 STHICK=0.65e0,     ! sthick; upper limit for originating air for lcl.
                    ! replaces kthick.
 SACUM=0.46e0,      ! sacum; top level for integrated moisture 
                    ! convergence test. replaces
                    ! kacum
 ACUM0=-2.0e-8,     ! acum0; threshold moisture convergence such that 
                    ! integrated moisture
                    ! convergence > - acum0 for convection to occur.
 TBASE=273.15e00,
 MLRG=0,            ! mlrg=1 ;output of pre-adjusted & post adjusted 
                    ! temp. & s.h. in lrgscl
 IS=1,              ! is  ;start i-point
 KI=1               ! ki  ; lowest level from which parcels can be 
                    ! lifted to find lcl
 cflric=0.10        ! parameter used by relaxed arakawa-schubert
 /

&COMCON
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     files
!     ifxxx=0    xxx is not processed
!     ifxxx=1    xxx is set to month=idatec(2) in the first call,
!                but not processed from the subsequent calls.
!                ifxxx is set to zero after interpolation
!     ifxxx=2    xxx is interpolated to current day and time every fint
!                hours synchronized to 00z regardless of initial time.
!                interpolation is continuous (every time step) if fint<0.
!     ifxxx=3    xxx is interpolated to current day and time when ifday=0
!                and tod=0.0 but not processed otherwise
!                ( appropriate only when xxx is predicted )
!
!                the following are for sst only (fint applies as in
!                ifxxx=2):
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 initlz  =2,	 ! nitlz =2 diabatic normal mode initialization
        	 !	 =1 diabatic with no normal mode initialization
        	 !	 =0 adiabatic with no normal mode initialization
        	 !	 <0 same as >0 with sib variables read in instead of 
        	 !	    initialized
 nstep   =1,	 ! number of steps in getting diabatic heating rate
        	 ! in diaten if nstep=1,nstep is set equal to 7 in Model.    
 fint	 =6,	 ! surface boundary calling interval in hours
 intsst  =-1,	 ! sst data set interval in days (if > 0)
        	 ! sst data set is in calendar months if < 0.
 ndord   =4,	 ! order (even) of horizontal diffusion del operator
 filta  =0.92e0, ! time filter constant
 percut =27502., ! percut   cut off period in sec  in nlnmi
        	 ! modes are to be read.
 varcut =1.6e5,  ! cut off height variance in m**2 for gravity wave drag
 ifsst  =-1,	 ! ifsst=4  sst is linearly interpolated from continuous 
                 !              direct access data set to current day and 
		 !              time.data set is assumed to be spaced every 
		 !              intsst days or every calendar month is 
		 !              intsst < 0.
 		 ! ifsst=5  sst is expanded from piecewise cubic 
		 !              coefficients in	direct access data set to 
		 !              current day and time. data set
 		 !		is assumed to be spaced every intsst days.
 ifsnw  =3,
 ifalb  =0,
 ifslm  =3,  
 ifozone=0,
    !   =0    old ozone
    !   =1    read field from single month file (first call only)
    !   =2    interpolated to current day and time from 12 month clim            (update every fint hours)
    !   =3    interpolated to current day and time from 12 month predicted field (first call only)
    !   =4    interpolated from continuous direct access data set to current day and time
 allghf=.false.,  ! it is possible to select all available grid history
                  ! fields:  
                  !
                  ! allghf=.TRUE.-  all available fields are required
 dpercu=27502.
 vcrit  =85.0,	  ! critical velocity (m/s) at which damping kicks in
 		  ! (for troposphere)
 alpha  =2.50,
 dodyn  =.false., ! logical flag to output
 		  ! first level of divergence, vorticity,
 		  ! virtual temperature, specific humidity
 		  ! and log of surface pressure
 		  ! at every time step
/
 49                       ! number of output forecast
   6.0 12.0   18.0  24.0   
  30.0 36.0   42.0  48.0   
  54.0 60.0   66.0  72.0   
  84.0 96.0  120.0 144.0   
 168.0 174.0 180.0 186.0 
 192.0 198.0 204.0 210.0 
 216.0 222.0 228.0 234.0 
 240.0 246.0 252.0 258.0 
 264.0 270.0 276.0 282.0 
 288.0 294.0 300.0 306.0 
 312.0 318.0 324.0 330.0 
 336.0 342.0 348.0 354.0 
 360.0 
EOT1
#
# Step 2: Build script to submit
#
cat <<EOF1 > ${FSCR}/mpisep${PERT}.sh
#!/usr/bin/sh -x
exec ${FEXE}/ParModel_MPI < ${NAMELISTFILEPATH}
EOF1

chmod 750 ${FSCR}/mpisep${PERT}.sh
cat <<EOT1> ${FSCR}/model${PERT}.sh
#!/usr/bin/ksh
#PBS -q multi                    # queue: dq for <=8 CPUs
#PBS -T mpisx                    # Job type: mpisx for MPI
#PBS -l cpunum_prc=${CpuPerNode} # cpus per Node
#PBS -l tasknum_prc=${TaskPerProc}
#PBS -b ${node}                  # number of nodes
#PBS -l cputim_job=${cputime}    # max accumulated cputime
#PBS -l memsz_job=${MemPerNode}  # memory per node
#PBS -o turi:${FSCR}/setout/run${cpu}${QUEUE}${data}.out
#PBS -e turi:${FSCR}/setout/run${cpu}${QUEUE}${data}.out
#PBS -j o                        # join stdout/stderr
#PBS -N OENS${PERT}              # job name

echo "RODANDO MPI"

cd ${FEXE}
export OMP_NUM_THREADS=1
export F_RSVTASK=${TaskPerProc}
export MPIPROGINF=DETAIL
export F_FILEINF=DETAIL
export F_PROGINF=DETAIL
export F_ERRCNT=1
export F_SETBUF=20480
export MPIMULTITASKMIX=YES
#MPIEXPORT="OMP_NUM_THREADS F_FILEINF"
#export MPIEXPORT
if [[ ${node} -eq  4 ]] ; then
mpirun  -v -host 0 -np ${cpu0} -host 1 -np ${cpu1} -host 2 -np ${cpu2} -host 3 -np ${cpu3} ${FSCR}/mpisep${PERT}.sh
fi
if [[ ${node} -eq  3 ]] ; then
mpirun  -v -host 0 -np ${cpu0} -host 1 -np ${cpu1} -host 2 -np ${cpu2}  ${FSCR}/mpisep${PERT}.sh
fi
if [[ ${node} -eq  2 ]] ; then
mpirun  -v -host 0 -np ${cpu0} -host 1 -np ${cpu1}  ${FSCR}/mpisep${PERT}.sh
fi
if [[ ${node} -eq  1 ]] ; then
mpirun  -v -host 0 -np ${cpu0}  ${FSCR}/mpisep${PERT}.sh
fi
EOT1

chmod 750 ${FSCR}/model${PERT}.sh
submit ${FSCR}/model${PERT}.sh ${QUEUE} Info

exit 0
