#!/bin/bash
#help#
#*******************************************************************#
#                                                                   #
#      Name:           runsurfacerec.scr                            #
#                                                                   #
#      Function:       This script compiles/runs the program        #
#                      to transform data binary to grib.            #
#                                                                   #
#      Date:           April 03th, 2000.                            #
#      Last change:    Abril 03th, 2000.                            #
#                                                                   #
#      Valid Arguments for runsurfacerec.scr                        #
#                                                                   #
#      First : COMPILE: help, comp or run                           #
#      Second: LABELI : initial forecasting label                   #
#      Third : LABELF : final forecasting label                     #
#      Fourth: TRC    : three-digit triangular truncation           #
#      Fifth : LV     : two-digit number of vertical sigma-layers   #
#      sixth : NMEMBR : number of members of the ensemble           #
#                                                                   #
#              LABLEx : yyyymmddhh                                  #
#                       yyyy = two digit year                       #
#                       mm   = two digit month                      #
#                       dd   = two digit day                        #
#                       hh   = two digit hour                       #
#                                                                   #
#*******************************************************************#
#help#
#
#       Help:
#
#
#       Test of Valid Arguments
#
LABELI=$1
LABELS=${LABELI:0:8}
HH=${LABELI:8:2}

TRC=$2
LV=$3
PREFIC=$4

CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

export FILEENV=`find ${HOME_suite} -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} ${PREFIC}

LABELF=`date -d "$LABELS ${NFDAYS} days" +"%Y%m%d${HH}"`
NMEMBR=`echo "$NPERT*2+1" | bc -l`
echo "PARAMETROS - $LABELI $LABELF $TRC $LV $NMEMBR"
#
# Set TERM
#
TERM=vt100;export TERM
#
# Set labels
#
COMPILE=run
echo "COMPILE = $COMPILE"
#
# set umask
#
umask 033
#
# Check for compilation
#
if [ "$COMPILE" = "comp" ]
then
#
# Generate include files
#
#
#   Compile FORTRAN source
#
echo "rm -f fixgrb"
rm -f fixgrb
#
echo "f77 -cpp -I./ -u -o fixgrb fixgrb.f"
f77 -cpp -I./ -u -o fixgrb fixgrb.f
#
exit 0
fi
#
#   Set Horizontal Truncation (TRUNC) and Vertical Layers (LEV)
#
GPOS=GPOS
GPOS2=GESF
RESOL=TQ0${TRC}
NIVEL=L0${LV}
MODELID=cptec002
export RESOL NIVEL CASE

####################################################################
#
#   Set directories and remote machines
#
#   DIRPRD: is the directory of the machine RMTCPR where there
#           are the programs to generate special products.
#   DIRSKL: is the directory of the machine RMTCPR where there
#           are the programs to generate skill of forecast.
#   RMTUSR: is the remote user at the telecom machine RMTCPR.
#   RMTCPR: is the remote machine to run the special products.
#   RMTGRU: is the remote user at the guara machine RMTGRM.
#   RMTGRM: is the remote machine to run the runfazlink1.scr.
#   RMTOPM: is the remote machine to run the recmod.scr.
#   DIRGRA: is the directory of the machine RMTGRM.
#   DIROPE: is the directory of the machine RMTOPM where there
#           is the program to do the desired link.  
#   DIRSCR: is the directory of the scripts which grib model data
#   RMMWAM: is the remote machine to run the runwamT062.ksh
#   RMTWAM: is the remote user at the machine RMMWAM
#   DIRWAM: is the directory of the machine RMMWAM  
#
####################################################################

# -----------------  PRODUTOS  --------------------

DIRSCR=${HOME_suite}/produtos/recortes/scripts

####################################################################
#
#   Generate script file to run special products
#
####################################################################

cd ${DIRSCR}

echo "rm -f setsurfacerec.${date}.$CASE"
rm -f setsurfacerec.${date}.$CASE

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

cat <<EOT5 > ${SCRIPTFILENAME}
#!/bin/ksh 
#PBS -W umask=026
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -lselect=1:ncpus=1
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -V
#PBS -N RECOENS

####################################################################
#
#   function calday
#
####################################################################

calday ()
{
yi=\`awk 'BEGIN {print substr("'\${LABELI}'",1,4)}'\`
mi=\`awk 'BEGIN {print substr("'\${LABELI}'",5,2)}'\`
di=\`awk 'BEGIN {print substr("'\${LABELI}'",7,2)}'\`
hi=\`awk 'BEGIN {print substr("'\${LABELI}'",9,2)}'\`
#
let ybi=\${yi}%4
if [ \${ybi} = 0 ]
then
set -A md 31 29 31 30 31 30 31 31 30 31 30 31
else
set -A md 31 28 31 30 31 30 31 31 30 31 30 31
fi
let dr=\${di}-\${nrdays}
let mr=\${mi}
let yr=\${yi}
let hr=\${hi}
if [ dr -le 0 ]
then
let nr=\${mi}-2
if [ nr -lt 0 ]
then let nr=11
fi
let dr=dr+md[\${nr}]
let mr=\${mi}-1
fi
if [ mr -le 0 ]
then
let mr=12
let yr=yr-1
fi
if [ dr -lt 10 ]
then DR=0\${dr}
else DR=\${dr}
fi
if [ mr -lt 10 ]
then MR=0\${mr}
else MR=\${mr}
fi
YR=\${yr}
if [ hr -lt 10 ]
then HR=0\${hr}
else HR=\${hr}
fi
let str=\${MR}-1
LSTD=\${md[\${str}]}
}
#
# Set initial current hour
#
HOUR=\`date +'%H:%M'\`
export HOUR
echo "  HOUR = \$HOUR"
#
# Set labels
#
LABELI=$LABELI
LABELF=$LABELF
echo "LABELI = \$LABELI"
echo "LABELF = \$LABELF"
#
#
# Set directories
#
yydir=\`awk 'BEGIN {print substr("'\$LABELI'",1,4)}'\`
mmdir=\`awk 'BEGIN {print substr("'\$LABELI'",5,2)}'\`
dddir=\`awk 'BEGIN {print substr("'\$LABELI'",7,2)}'\`
export yydir mmdir dddir
resol=\`awk 'BEGIN {print tolower("'$RESOL'")}'\`
nivel=\`awk 'BEGIN {print tolower("'$NIVEL'")}'\`
echo "resol="\$resol
echo "nivel="\$nivel
#

export DIRCTL1 DIRCTL2 dirgrb DIRBANG DIRLATS DIRSCR
CASE=$CASE
echo "CASE   = \$CASE"
echo "DIRCTL1 = \$DIRCTL1"
echo "DIRCTL2 = \$DIRCTL2"

#
#-------------------------------------------------------------
#
# Remove old files
#
#-------------------------------------------------------------

NMEMBRP=${NMEMBR}
let NP=NMEMBRP-1
let NPR=NP/2

cd ${DIRSCR}

################################################################
#
# Grads:  (tupa)
#
################################################################

#export GRADSB=/stornext/home/oper/bin/grads-2.0.a9.oga

################################################################
#
# Run task
#
# Set characteristics of the product
################################################################

tit='CPTEC GLOBAL ENSEMBLE SURFACE PRODUCTS '\$CASE'  COLD'
lon1=-101.250
lon2=-11.250
lat1=-60.62040
lat2=13.98945
fields='topo psnm uves vves tems tsfc umrs prec'

################################################################
#
# Does rec to members (1 to 15)
#
################################################################

dirgrb=${SC2_suite}/produtos/recortes/GESFAM/\$CASE/\${LABELI}; mkdir -p \$dirgrb

for TIPO in P.icn P.fct; do
     for arq in \$(ls ${SC2_suite}/pos/dataout/\$CASE/\${LABELI}/${GPOS}???\${LABELI}??????????\${TIPO}.\$CASE.ctl); do

        arq1=\$(basename \$arq)
        strmb=\$(awk 'BEGIN { print substr("'\$arq1'",5,3) }')
        strg1=\${arq1:7:35}
        arqout=${GPOS2}\${strmb}\${strg1}

        op="-i \$arq -o \$dirgrb/\$arqout -center $MODELID -grid gaussian -format grads_grib -ftype ctl -lat \${lat1} \${lat2} -lon \${lon1} \${lon2}  -table cptec.table -v -vars \${fields} -title \${tit}"
        echo "${GRADSB}/lats4d.sh \$op"
        rep=0

        until [ -s \$dirgrb/\$arqout.ctl ]
        do
            let rep=rep+1
            echo "=> Grib Repet: \$rep"
            if [ \$rep -ge 5 ]; then
                rm -f WARNNING.\${LABELI}
                echo "#####################################">WARNNING.\${LABELI}
                echo "  rec product execution problem encontered">>WARNNING.\${LABELI}
                echo "  Repeted more then \$rep times">>WARNNING.\${LABELI}
                echo "  Please execute the command below:">>WARNNING.\${LABELI}
                echo "  ./setsurfacerec.scr">>WARNNING.\${LABELI}
                echo "#####################################">>WARNNING.\${LABELI}
                exit 1
            fi 
            rm -f \$dirgrb/\$arqout.ctl
            rm -f \$dirgrb/\$arqout.gmp
            rm -f \$dirgrb/\$arqout.grb

            ${GRADSB}/lats4d.sh \$op
        done
    done
done

#-------------------------------------------------------------
#
# Does rec to ensemble mean 
#
#-------------------------------------------------------------

for TIPO in P.icn P.fct; do
   for arq in \$(ls ${SC2_suite}/ensmed/dataout/\$CASE/\${LABELI}/${GPOS}ENM\${LABELI}2*.ctl); do
      arq1=\$(basename \$arq)
      strmb=\$(awk 'BEGIN { print substr("'\$arq1'",5,3) }')
      strg1=\${arq1:7:35}
      arqout=${GPOS2}\${strmb}\${strg1}
#
      op="-i \$arq -o \$dirgrb/\$arqout -center $MODELID -grid gaussian -format grads_grib -ftype ctl -lat \${lat1} \${lat2} -lon \${lon1} \${lon2}  -table cptec.table -v -vars \${fields} -title \${tit}"
      echo "${GRADSB}/lats4d.sh \$op"
#
      rep=0
      until [ -s \$dirgrb/\$arqout.ctl ]; do
         let rep=rep+1
         echo "=> Grib Repet: \$rep"
         if [ \$rep -ge 5 ]
            then rm -f WARNNING.\${LABELI}
            echo "#####################################">WARNNING.\${LABELI}
            echo "  rec product execution problem encontered">>WARNNING.\${LABELI}
            echo "  Repeted more then \$rep times">>WARNNING.\${LABELI}
            echo "  Please execute the command below:">>WARNNING.\${LABELI}
            echo "  ./setsurfacerec.scr">>WARNNING.\${LABELI}
            echo "#####################################">>WARNNING.\${LABELI}
            exit 1
         fi
         rm -f \$dirgrb/\$arqout.ctl
         rm -f \$dirgrb/\$arqout.gmp
         rm -f \$dirgrb/\$arqout.grb

         ${GRADSB}/lats4d.sh \$op
      done
   done
done

exit 0

EOT5

export PBS_SERVER=aux20-eth4
echo "Submetendo: ${SCRIPTFILENAME}"
JID=$(qsub ${SCRIPTFILENAME} | awk -F "." '{print $1}')
it=2
while [ ${it} -gt 0 ];do
	it=`qstat | grep $JID | wc -l`
	sleep 3
done

#

