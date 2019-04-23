#!/bin/bash
#help#
#************************************************************************************#
#                                                                                    #
# script to run CPTEC Post-processing on PC Clusters under MPI Scali                 #
# and Sun Grid Engine without OpenMP                                                 #
#                                                                                    #
# assumptions: assume present but NOT at the same directory:                         #
#              $FEXE/PostGrib (Post-processing Executable file)                      #
#              $FSCR/POSTIN-GRIB (Post-processing input Namelist file)               #
#                                                                                    #
# usage: run_post_UNA.sh cpu_mpi  cpu_node name TRC LV LABELI LABELF name ps NMC hold #
# where:                                                                             #
# cpu_mpi: integer, the desired number of mpi processes                              #
# cpu_node: integer, the desired number of mpi processes per shared memory node      #
#************************************************************************************#
#TESTE runGrh.ok 1 1 GRHTQ0213L042 213 42 2011011400 2011011500 GFGNNMC reduzida NMC hold
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
fi
if [ -z "${4}" ]
then
  echo "TRC is not set" 
  exit 2
else
  TRC=`echo ${4} | awk '{print $1/1}'`   
fi
if [ -z "${5}" ]
then
  echo "LV is not set" 
  exit 2
else
  LV=`echo ${5} | awk '{print $1/1}'`    
fi

if [ -z "${6}" ]
then
  echo "LABELI is not set" 
  exit 3
else
  export LABELI=${6}  
fi
if [ -z "${7}" ]
then
  echo "LABELF is not set" 
  exit 3
else
  export LABELF=${7}  
fi

if [ -z "${8}" ]
then
  echo "NAME is not set" 
  exit 3
else
  export NAME=${8}  
fi
if [ -z "${9}" ]
then
  echo "PS is not set" 
  exit 3
else
  export PS=${9}  
fi
if [ -z "${10}" ]
then
  echo "PREFIXC is not set" 
  exit 3
else
  export PREFIXC=${10}  
fi

if [ "$#" == 11 ]
then hold=""
else hold=
fi

if [ ${TRC} = 21 ]; then
 export timestep=3600
fi 
if [ ${TRC} = 31 ]; then
 export timestep=1800
fi 
if [ ${TRC} = 42 ]; then
 export timestep=1800
fi 
if [ ${TRC} = 62 ]; then
 export timestep=1200
fi
if [ ${TRC} = 106 ]; then
 export timestep=900
fi
if [ ${TRC} = 126 ]; then
 export timestep=600
fi
if [ ${TRC} = 133 ]; then
 export timestep=600
fi
if [ ${TRC} = 159 ]; then
 export timestep=600
fi
if [ ${TRC} = 170 ]; then
 export timestep=450
fi
if [ ${TRC} = 213 ]; then
 export timestep=450
fi
if [ ${TRC} = 213 ]; then
 export timestep=360
fi
if [ ${TRC} = 254 ]; then
 export timestep=300
fi
if [ ${TRC} = 299 ]; then
 export timestep=300
fi
if [ ${TRC} = 319 ]; then
 export timestep=225
fi
if [ ${TRC} = 341 ]; then
 export timestep=200
fi
if [ ${TRC} = 382 ]; then
 export timestep=180
fi
if [ ${TRC} = 511 ]; then
 export timestep=150
fi
if [ ${TRC} = 533 ]; then
 export timestep=150
fi
if [ ${TRC} = 666 ]; then
 export timestep=150
fi
if [ ${TRC} = 863 ]; then
 export timestep=150
fi
if [ ${TRC} = 1279 ]; then
 export timestep=20
fi

echo "meteogr.ksh: Start execution at `date`"
#
# SETTING THE APPROPRIATED ENVIRONMENT
#
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`
set -x
export PREFIXC=NMC

. ${FILEENV} ${CASE} ${PREFIXC}
cd ${HOME_suite}/run
HSTMAQ=`hostname`

RUNTM=$(date +"%s")
# script arguments and directory
DIRRESOL=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
EXECFILEPATH=${SC1_suite}/grh/exec
SCRIPTFILEPATH=${HOME_suite}/run/setgrhg${DIRRESOL}.${MAQUI}
NAMELISTFILEPATH=${HOME_suite}/run ; mkdir -p $NAMELISTFILEPATH
OUTPUTFILEPATH=${SC2_suite}/out_err/${DIRRESOL}/$LABELI/grhg${DIRRESOL}.${MAQUI}.${RUNTM}
GRHDATAOUT=${SC2_suite}/produtos/grh/${DIRRESOL}/${LABELI}; mkdir $GRHDATAOUT

# total cpus and nodes
cpu_mpi=${1};  if [[ -z "${1}"  ]]; then cpu_mpi=1 ; fi
cpu_node=${2}; if [[ -z "${2}"  ]]; then cpu_node=1; fi
export cpu_mpi cpu_node
export RES=$3
num=$(($cpu_mpi+$cpu_node-1))
fra=$(($num/$cpu_node))
cpu_tot=$(($fra*$cpu_node))
echo fila=mpi-npn${cpu_node} total cpus=${cpu_tot}

#
# Grads
#
export gradsbin="grads"
#
export trunc=${TRC}
export lev=${LV}
export aspa="'"
if [ -z "${PREFXO}" ] ;then
export Preffix="NMC" 
else
export Preffix="${PREFXO}" 
fi 
export DirInPut="${SC2_suite}/model/dataout/$DIRRESOL/${LABELI}/" ; mkdir -p  ${DirInPut}  #, ! Main Data Directory
export DirOutPut="${SC2_suite}/pos/dataout/$DIRRESOL/${LABELI}/"  ; mkdir -p  ${DirOutPut}  #, ! Main Data Directory
export DirMain="${SC2_suite}"        #,    ! Main Data Directory
echo $DirInPut
export aspa="'"
export TMean=${timestep}
labeli=${LABELI}
labelf=${LABELF}
name=${NAME}
ps=${PS}
MAQUI=`uname -s`
if [ -z "${ps}" ]; then
  ps=psuperf
fi
ext=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
DATE=`echo $LABELI | cut -c1-8`
HH=`echo $LABELI | cut -c9-10`
DATEF=`echo $LABELF | cut -c1-8`
HHF=`echo $LABELF | cut -c9-10`
labelr=`date -d "${DATE} ${HH}:00 12 hour ago" +"%Y%m%d%H"`
julday1=`date -d "${DATE} ${HH}:00" +"%j"`
julday2=`date -d "${DATEF} ${HHF}:00" +"%j"`

ndays=`echo ${julday2} ${julday1} |awk '{{nday=$1-$2}if(nday < 0){nday = $1 + (365-$2)} if(nday >7){nday=7} {print nday}}'`

# script invoked by mpirun
dateoutjob=`date +"%Y%m%d%H%S"`

tmstp=$(date +"%s")

echo "${labeli} ${labelf} ${name} ${ext} ${ps} ${labelr}"

cat << EOF1 > ${HOME_suite}/produtos/grh/scripts/meteogr$DIRRESOL.gs
'reinit'

pull argumentos

_labeli=subwrd(argumentos,1);_labelf=subwrd(argumentos,2);_nomea=subwrd(argumentos,3);_trlv=subwrd(argumentos,4)
_ps=subwrd(argumentos,5);_labelr=subwrd(argumentos,6)
_time1=subwrd(argumentos,7);_time2=subwrd(argumentos,8);_xfile=subwrd(argumentos,9)

* nome do arquivo com prefixos dos pontos
_nomeb='${SC2_suite}/pos/dataout/${DIRRESOL}/${labeli}/Preffix${PREFIXC}'%_labeli%_labelf%'.'%_trlv

* nomes dos arquivos com identificacao e local dos pontos
_nomec='${SC2_suite}/pos/dataout/${DIRRESOL}/${labeli}/Identif${PREFIXC}'%_labeli%_labelf%'.'%_trlv
_nomed='${SC2_suite}/pos/dataout/${DIRRESOL}/${labeli}/Localiz${PREFIXC}'%_labeli%_labelf%'.'%_trlv
nomectl ='${SC2_suite}/pos/dataout/${DIRRESOL}/${labeli}/'%_nomea%_labeli%_labelf%'M.grh.'%_trlv%'.ctl'
*say nomectl

_lonlat2ur="05038W2104S 04823W2137S 04823W2030S 04856W2211S 04715W2245S 04715W2030S 05004W2211S 04749W2245S 05111W2211S 04749W2104S 04930W2104S 04715W2318S"
_nlonlat2ur=12

_ndias=${ndays}
_ntimes=_ndias*24
say "abrindo o arquivo "nomectl
'open 'nomectl
'q file'
say result

say _time1' '_time2
'set time '_time1' '_time2

rec=read(_nomeb);nloc=sublin(rec,2)
rec=read(_nomec);nloc1=sublin(rec,2)
rec=read(_nomed);nloc2=sublin(rec,2)

_loc=0
while (_loc < nloc)
   _loc=_loc+1
say 'nloc ' nloc  _loc

   'clear';'set x '_loc
   'set time '_time1' '_time2
   routine=dimet()
   if (_faz=1)
      lixo=write('${GRHDATAOUT}/umrs_min'_labeli'.txt',_linha)
      lixo=write('${GRHDATAOUT}/umrs_min'_labeli'.txt','')
   endif  
endwhile
lixo=close('${GRHDATAOUT}/umrs_min'_labeli'.txt')

***********************************************
********* Grid History Maps Finalized *********
***********************************************
function dimet()
'set display color white';'clear'
'set vpage 0 8.5 10.2 11';'set grads off'
routine=title()
'set vpage 0 8.5 8.75 10.70';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=prec()
'set vpage 0 8.5 7.00 8.95';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=temp()
'set vpage 0 8.5 5.25 7.20';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=umrl()
if (_faz=1); routine=umrl_min(); endif 
'set vpage 0 8.5 3.50 5.45';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=wvel()
'set vpage 0 8.5 1.75 3.70';'set parea 0.7 8.0 0.3 1.6';'set grads off'
if (_ps=reduzida)
routine=psnm()
else
routine=pslc()
endif
'set vpage 0 8.5 0.0 1.95';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=cbnv()
label=_labeli%_labelf
rec=read(_nomeb);lab=sublin(rec,2)
taga=png;tagb=png;tagc=png
*say 'printim ${GRHDATAOUT}/'lab'.png png'
*'printim ${GRHDATAOUT}/'lab'.png png' 
say 'printim ${GRHDATAOUT}/'_loc'.png png'
'printim ${GRHDATAOUT}/'_loc'.png png' 
'!rm -f meteogram'
if (_loc=1)
'!rm -f puttag.'_labeli'.out'
'!rm -f deltag.'_labeli'.out'
endif
*AAF '!echo put '_state'/'lab''label'.'taga' >> puttag.'_labeli'.out'
return
************************************************
function title()
rec=read(_nomec);local=sublin(rec,2)
_state=estado(local)
rec=read(_nomed)
lonlat=sublin(rec,2);loi=substr(lonlat,1,3);lof=substr(lonlat,4,2)
lo=substr(lonlat,6,1);lai=substr(lonlat,7,2);laf=substr(lonlat,9,2);la=substr(lonlat,11,1)
lalo=loi%':'%lof%lo%'-'%lai%':'%laf%la
say ' '
say '   Plotando localizacao numero = '_loc'  Local: 'local
'set string 8 l 6';'set strsiz .13 .14'
'draw string 0.4 0.7 CPTEC:'
'draw string 1.4 0.7 'lalo
'draw string 3.4 0.7 'local
_faz=0

n=1
while (n<=_nlonlat2ur)
   latlonur=subwrd(_lonlat2ur,n)
   if (lonlat=latlonur)
      lixo=write('${GRHDATAOUT}/umrs_min'_labeli'.txt',lonlat' - 'local)
      _faz=1
      _linha=""
      n=9999
   endif
   n=n+1
endwhile

'set t 5';'q dims'
tm = sublin(result,5);tim = subwrd(tm,6);tmm = substr(tim,7,9)
utc=substr(_labeli,9,2)

if (_ps!=reduzida)
'd topo'
_tpg=subwrd(result,4)
endif
'set strsiz .125 .13'
'draw string 0.4 0.5 'tmm' 'utc'Z (GMT)                           Vertical Grid Line: 'utc'Z'
'set time '_time1' '_time2
return

************************************************
function umrl()
'set gxout line';'set grads off';'set axlim 0 100';'set cmark 0';'set ylint 20';'set ccolor 4'
'd umrs'
'set string 6 l 5';'set strsiz .12 .13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Relative Humidity (%)'
return
************************************************
function umrl_min()
t=1;urmin=200; datamin=xx
while (t<=_ntimes)
'set t 't''
'q time'
data=subwrd(result,3)
'd umrs'
umid=subwrd(result,4)
if (umid<urmin)
urmin=umid
datamin=data
endif  
  
fimdia=math_fmod(t,24)
if (fimdia=0)
urmin=math_format('%5.1f',urmin)
_linha=_linha' 'datamin' 'urmin
urmin=200; datamin=xx
endif
t=t+1
endwhile
'set time '_time1' '_time2
return
************************************************
function cbnv()
'set gxout bar';'set bargap 0';'set barbase 0';'set vrange 0 100';'set ylint 20';'set grads off';'set ccolor 15'
'd cbnv'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw recf 0 0 8.5 0.1'
'draw string 1 1.75 Cloud Cover (%)'
return
************************************************
function snof()
'set gxout bar';'set bargap 0';'set barbase 0';'set vrange 0 10';'set ylint 2';'set grads off';'set ccolor 4'
'd neve'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Snow Fall (mm/h)'
return
************************************************
function prec()
'set gxout bar';'set bargap 0';'set barbase 0';'set vrange 0 5';'set ylint 1';'set grads off';'set ccolor 4'
'd prec'
'set gxout stat';'d neve'
lnv=sublin(result,8)
nv=subwrd(lnv,5)
'set gxout bar';'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
if (nv > 0.0001)
'set ccolor 3'
'd neve'
'draw string 1 1.75 Precipitation (blue) and Snow Fall (green) (mm/h)'
else
'draw string 1 1.75 Precipitation (mm/h)'
endif
'set string 8 l 6';'set strsiz .13 .14'
'draw string 7.1 1.75 '_trlv
return
************************************************
function psnm()
rotina=maxmin(psnm)
'set gxout line';'set vrange '_vmin' '_vmax'';'set cmark 0';'set ylint '_itvl'';'set ccolor 4';'set grads off'
'd psnm'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Mean Sea Level Pressure (hPa)'
return
************************************************
function pslc()
rotina=maxmin(pslc)
'set gxout line';'set vrange '_vmin' '_vmax'';'set cmark 0';'set ylint '_itvl'';'set ccolor 4';'set grads off'
'd pslc'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Pressure (hPa)        Model Altitude: '_tpg' m'
return
************************************************
function wvel()
'set lev 1000 ';'set gxout vector';'set ylab off';'set grads off';'set arrowhead 0.075';'set z 0.5 1.5'
'd skip(uves,1,12);vves'
'set gxout line';'set grads off';'set z 1';'set ylab on';'set cmark 0';'set ylint 2';'set ccolor 4'
'd mag(uves,vves)'
'set string 6 l 5';'set strsiz .12 .13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Wind (m/s)'
return
************************************************
function temp()
rotina=maxmin(tems)
'set gxout line';'set vrange '_vmin' '_vmax'';'set grads off';'set ylint '_itvl'';'set ccolor 4';'set cmark 0'
'd tems'
'set string 6 l 5';'set strsiz .12 .13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Temperature (\`aO\`nC)'
return
************************************************
function maxmin(var)
'set t 1'

'd max('var',t=1,t='_ntimes',1)'

linha=sublin(result,2);imax=subwrd(linha,4)
'd min('var',t=1,t='_ntimes',1)'

linha=sublin(result,2);imin=subwrd(linha,4)
say linha
say imin

if(imin>0);imin=imin-1;else;imin=imin+1;endif
if(imax>0);imax=imax+1;else;imax=imax-1;endif
_vmax=math_nint(imax);_vmin=math_nint(imin)
_itvl=math_nint((imax-imin)/5)
'set time '_time1' '_time2
return
************************************************
function estado(local)
frase='AC AL AM AP BA CE DF ES GO MA MG MS MT PA PB PE PI PR RJ RN RO RR RS SC SE SP TO'
ne=1
while(ne<=27)
est.ne=subwrd(frase,ne)
ne=ne+1
endwhile

i=1
c=substr(local,i,1)
while (c != '(')
i=i+1
c=substr(local,i,1)
if (i > 40)
break
endif
endwhile
j=1
c=substr(local,j,1)
while (c != ')')
j=j+1
c=substr(local,j,1)
if (j > 40)
break
endif
endwhile
if (i > 40 | j > 40)
state='ZZ'
else
i=i+1
j=j-i
state=substr(local,i,j)
k=0
l=0
while (k < 27)
k=k+1
if (state = est.k)
l=1
endif
endwhile
endif
if (l = 0)
state='WW'
endif
return state
***********************************************
EOF1

DATE=`echo ${LABELI} | cut -c 1-8`
HH=`echo ${LABELI} | cut -c 9-10`
DATEF=`echo ${LABELF} | cut -c 1-8`
HHF=`echo ${LABELF} | cut -c 9-10`

time1=`date -d "$DATE $HH:00" +"%HZ%d%b%Y"`
time2=`date -d "$DATEF $HHF:00" +"%HZ%d%b%Y"`

echo ${labeli} ${labelf} ${name} ${ext} ${ps} ${labelr}
echo ${time1} ${time2}
GSSTEP=1

if [ X$GSSTEP = X1 ]; then

echo ${OUTPUTFILEPATH}.grads

tmstp=$(date +"%s")
cat << _EOF > ${SCRIPTFILEPATH}.grads
#!/bin/bash
#PBS -W umask=026
#PBS -o ${OUTPUTFILEPATH}.grads.out
#PBS -e ${OUTPUTFILEPATH}.grads.err
#PBS -lselect=1:ncpus=1
#PBS -q $QUEUE1
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES

ti=\$(date +"%s %Y/%m/%d %H:%M:%S") # TEMPO INICIAL

mkdir -p ${GRHDATAOUT}
echo ${GRHDATAOUT}
${GRADSB}/grads -bp  << EOT
run ${HOME_suite}/produtos/grh/scripts/meteogr$DIRRESOL.gs
${labeli} ${labelf} ${name} ${ext} ${ps} ${labelr} ${time1} ${time2} ${HH}
quit
EOT

mkdir -p ${CTRL_HOME}/${CASE}/${LABELI:0:6}
date +"${HOME_suite}/run/rungrhfig.bash @ aux 1 @ ini \$ti @ fim %s %Y/%m/%d %H:%M:%S" >> ${CTRL_HOME}/${CASE}/${LABELI:0:6}/${LABELI:6:4}.txt

exit 0
_EOF
#
#   Change mode to be executable
#
chmod +x ${SCRIPTFILEPATH}.grads

export PBS_SERVER=aux20-eth4  # PARA RODAR NOS NOS DE PROCESSAMENTO AUXILIAR

SBJ=$(qsub $hold ${SCRIPTFILEPATH}.grads | cut -d. -f1)
#
it=2
while [ ${it} -gt 0 ] ; do
	it=0$(qstat | grep $SBJ | wc -l)
	sleep 30
done

fi

echo "meteogr.ksh: Finish execution at `date`"
echo " Saida - Ok"
exit 0

