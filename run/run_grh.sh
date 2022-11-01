#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para submeter o grid history dos membros pós-processados do
# modelo BAM do Sistema Previsão por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_grh.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5> <opcao6>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> num_proc  -> número de processadores
#            
#            <opcao2> resolucao -> resolução espectral do grh
#                                
#            <opcao3> datai     -> data da análise corrente 
#
#            <opcao4> dataf     -> data da previsao final 
#
#            <opcao5> prefixo   -> prefixo que identifica o tipo de análise 
#            
#            <opcao6> n_mem b   -> tamanho do conjunto de perturbações
#            
#  Uso/Exemplos: 
# 
#  Submete o Grid History dos membros NPT e PPT, respectivamente::
# ./run_grh.sh 4 TQ0126L028 2013010100 2020061600 NMC 1
#
# !REVISION HISTORY:
#
# 09 Julho de 2020    - C. F. Bastarz - Versão inicial.  
# 18 Junho de 2021    - C. F. Bastarz - Revisão geral.
# 06 Agosto de 2021   - C. F. Bastarz - Atualização e simplicação para o
#                                       membro controle.
# 01 Novembro de 2022 - C. F. Bastarz - Inclusão de diretivas do SLURM.
#
# !REMARKS:
#
# !BUGS:
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
#set -o xtrace

#
# Menu de opções/ajuda
#

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

cria_namelist() {

sed -e "s;#TRUNC#;${1};g" \
    -e "s;#LEV#;${2};g" \
    -e "s;#TIMESTEP#;${3};g" \
    -e "s;#TMEAN#;${4};g" \
    -e "s;#LABELI#;${5};g" \
    -e "s;#LABELF#;${6};g" \
    -e "s;#NMEM#;${7};g" \
    -e "s;#PATHIN#;${8};g" \
    -e "s;#PATHOUT#;${9};g" \
    -e "s;#PATHMAIN#;${10};g" \
    ${11}/PostGridHistory.nml.template > ${12}/PostGridHistory.nml

echo "Namelist criado em: ${12}/PostGridHistory.nml"

}

#
# Verificação dos argumentos de entrada
#

if [ -z "${1}" ]
then
  echo "MPPWIDTH esta faltando" 
  exit 3
else
  export MPPWIDTH=${1}  
fi

if [ -z "${2}" ]
then
  echo "RESOL esta faltando" 
  exit 3
else
  export RES=${2}  
fi

if [ -z "${3}" ]
then
  echo "LABELI esta faltando" 
  exit 3
else
  export LABELI=${3} 
fi

if [ -z "${4}" ]
then
  echo "LABELF esta faltando" 
  exit 3
else
  export LABELF=${4} 
fi

if [ -z "${5}" ]
then
  echo "ANLTYPE esta faltando" 
  exit 3
else
  export ANLTYPE=${5}  
fi

if [ -z "${6}" ]
then
  echo "ANLPERT esta faltando" 
else
  export ANLPERT=${6}  
fi

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ; pwd)

. ${FILEENV} ${RES} ${ANLTYPE}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

if [ ${TRCLV} == "TQ0126L028" ]
then
  export TIMESTEP=600
elif [ ${TRCLV} == "TQ0213L042" ]
then
  export TIMESTEP=360
elif [ ${TRCLV} == "TQ0299L064" ]
then
  export TIMESTEP=200
else
  echo "Erro na resolução ${TRCLV}"
  exit 1
fi

# 
# Intervalo de tempo entre as saídas (1 hora)
#

export TMEAN=3600

DIRRESOL=$(echo ${TRC} ${LV} | awk '{printf("TQ%4.4dL%3.3d\n",$1,$2)}')
MAQUI=$(hostname -s)

export SCRIPTFILEPATH=${HOME_suite}/run/setgrh${ANLTYPE}.${DIRRESOL}.${LABELI}.${MAQUI}
export NAMELISTFILEPATH=${HOME_suite}/run

export EXECFILEPATH=${DK_suite}/produtos/grh/exec

mkdir -p ${EXECFILEPATH}

export PATHMAIN=${DK_suite}

#
# Variáveis utilizadas no script de submissão
#

if [ ${ANLTYPE} == CTR -o ${ANLTYPE} == NMC -o ${ANLTYPE} == EIT -o ${ANLTYPE} == EIH ]
then

  PATHIN=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/
  PATHOUT=${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/

  EXECFILEPATH=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}

  mkdir -p ${EXECFILEPATH}/setout 
   
  ln -sf ${DK_suite}/produtos/grh/exec/PostGridHistory ${EXECFILEPATH}
  
  cria_namelist ${TRC} ${LV} ${TIMESTEP} ${TMEAN} ${LABELI} ${LABELF} ${ANLTYPE} ${PATHIN} ${PATHOUT} ${PATHMAIN} ${NAMELISTFILEPATH} ${EXECFILEPATH}

  #export PBSDIRECTIVENAME="#PBS -N GRH${ANLTYPE}"
  export PBSDIRECTIVENAME="#SBATCH --job-name=GRH${ANLTYPE}"
  export PBSDIRECTIVEARRAY=""
  export PBSMEM=""

  #export PBSOUTFILE="#PBS -o ${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.out"
  #export PBSERRFILE="#PBS -e ${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.err"
  export PBSOUTFILE="#SBATCH --output=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.out"
  export PBSERRFILE="#SBATCH --error=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.err"
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/"

else

  for MEM in $(seq -f %02g 1 ${ANLPERT})
  do

    export NMEM=${MEM}${ANLTYPE:0:1}

    PATHIN=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${NMEM}/
    PATHOUT=${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${NMEM}/

    EXECFILEPATH=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}
    EXECFILEPATHMEM=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/${NMEM}

    mkdir -p ${EXECFILEPATHMEM}
   
    ln -sf ${DK_suite}/produtos/grh/exec/PostGridHistory ${EXECFILEPATHMEM}
  
    cria_namelist ${TRC} ${LV} ${TIMESTEP} ${TMEAN} ${LABELI} ${LABELF} ${NMEM} ${PATHIN} ${PATHOUT} ${PATHMAIN} ${NAMELISTFILEPATH} ${EXECFILEPATHMEM}

#    export PBSDIRECTIVENAME="#PBS -N GRHENS${ANLTYPE}"
#    export PBSDIRECTIVEARRAY="#PBS -J 1-${ANLPERT}"
#    export PBSMEM="export MEM=\$(printf %02g \${PBS_ARRAY_INDEX})"
#  
#    export PBSOUTFILE="#PBS -o ${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/${NMEM}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.out"
#    export PBSERRFILE="#PBS -e ${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/${NMEM}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.err"
#    export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/\${MEM}${ANLTYPE:0:1}"

    export PBSDIRECTIVENAME="#SBATCH --job-name=GRHENS${ANLTYPE}"
    export PBSDIRECTIVEARRAY="#SBATCH --array=1-${ANLPERT}"
    export PBSMEM="export MEM=\$(printf %02g \${SLURM_ARRAY_TASK_ID})"
  
    export PBSOUTFILE="#SBATCH --ouput=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/${NMEM}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.out"
    export PBSERRFILE="#SBATCH --error=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/${NMEM}/setout/Out.grh.${LABELI}.${ANLTYPE}.MPI${MPPWIDTH}.err"
    export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/produtos/grh/exec_${LABELI}.${ANLTYPE}/\${MEM}${ANLTYPE:0:1}"

  done

fi

#
# Script de submissão
#

cat <<EOF0 > ${SCRIPTFILEPATH}
#! /bin/bash -x
###PBS -j oe
###PBS -l walltime=4:00:00
###PBS -l mppnppn=${MPPWIDTH}
###PBS -A CPTEC
###PBS -V
###PBS -S /bin/bash
##${PBSDIRECTIVENAME}
##${PBSDIRECTIVEARRAY}
###PBS -q ${AUX_QUEUE}

###SBATCH --output=${BAMRUN}/setout/Out.model.${PREFIX}.${LABELI}.${tmstp}.MPI${MPPWIDTH}.out
###SBATCH --error=${BAMRUN}/setout/Out.model.${PREFIX}.${LABELI}.${tmstp}.MPI${MPPWIDTH}.err
#SBATCH --time=4:00:00
#SBATCH --tasks-per-node=${MPPWIDTH}
#SBATCH --nodes=${MPPDEPTH}
${PBSDIRECTIVENAME}
${PBSDIRECTIVEARRAY}
#SBATCH --partition=${QUEUE}

#echo \${PBS_JOBID} > ${HOME_suite}/run/this.job.${LABELI}.${ANLTYPE}

# EGEON GNU
module purge
module load gnu9/9.4.0
module load ucx/1.11.2
module load openmpi4/4.1.1
module load netcdf/4.7.4
module load netcdf-fortran/4.5.3
module load phdf5/1.10.8
module load hwloc
module load libfabric/1.13.0
module load singularity

export PBS_SERVER=${pbs_server2}

${PBSMEM}
${PBSEXECFILEPATH}

mkdir -p \${EXECFILEPATH}/setout

cd \${EXECFILEPATH}

if [[ Linux == "Linux" || Linux == "linux" ]]
then
  export F_UFMTENDIAN=10,20,30,40,50,60,70,80
  export GFORTRAN_CONVERT_UNIT=big_endian:10,20,30,40,50,60,70,80
fi

export KMP_STACKSIZE=128m
ulimit -s unlimited

#aprun -n 1 -N 1 -d 1 \${EXECFILEPATH}/PostGridHistory < \${EXECFILEPATH}/PostGridHistory.nml > \${EXECFILEPATH}/setout/Print.grh.${LABELI}.MPI${MPPWIDTH}.log
singularity exec -e --bind /mnt/beegfs/carlos.bastarz:/mnt/beegfs/carlos.bastarz /mnt/beegfs/carlos.bastarz/containers/egeon_dev.sif mpirun -np ${MPPWIDTH} \${EXECFILEPATH}/PostGridHistory < \${EXECFILEPATH}/PostGridHistory.nml > \${EXECFILEPATH}/setout/Print.grh.${LABELI}.MPI${MPPWIDTH}.log
EOF0

#
# Submete o script e aguarda o fim da execução
#

chmod +x ${SCRIPTFILEPATH}

export PBS_SERVER=${pbs_server2}

#qsub -W block=true ${SCRIPTFILEPATH}
sbatch ${SCRIPTFILEPATH}

#if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC ]
#then
#
#  JOBID=$(cat ${HOME_suite}/run/this.job.${LABELI}.${ANLTYPE} | awk -F "[" '{print $1}')
#
#  for mem in $(seq 1 ${ANLPERT})
#  do
#
#    nmem=$(printf %02g ${mem})${ANLTYPE:0:1} 
#
#    jobidname="GRHENS${ANLTYPE}.o${JOBID}.${mem}"
#    grhoutname="Out.grh.${LABELI}.MPI${MPPWIDTH}.${mem}.out"
#
#    until [ -e "${HOME_suite}/run/${jobidname}" ]; do sleep 1s; done
#    mv -v ${HOME_suite}/run/${jobidname} ${EXECFILEPATH}/${nmem}/setout/${grhoutname}
#  
#  done
#
#else
#
#  JOBID=$(cat ${HOME_suite}/run/this.job.${LABELI}.${ANLTYPE} | awk -F "." '{print $1}')
#
#  jobidname="GRH${ANLTYPE}.o${JOBID}"
#  grhoutname="Out.grh.${LABELI}.MPI${MPPWIDTH}.out"
#
#  until [ -e "${HOME_suite}/run/${jobidname}" ]; do sleep 1s; done 
#  mv -v ${HOME_suite}/run/${jobidname} ${EXECFILEPATH}/setout/${grhoutname}
#
#fi
#
#rm ${HOME_suite}/run/this.job.${LABELI}.${ANLTYPE}

#
# Cria os links simbólicos dos arquivos GFGNMEMYYYYMMDDHHYYYYMMDDHHM.grh.TQ0126L028.* para fora do diretório dos membros
#

if [ ${ANLTYPE} == CTR -o ${ANLTYPE} == NMC -o ${ANLTYPE} == EIT -o ${ANLTYPE} == EIH ]
then

  ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/GFGN${ANLTYPE}${LABELI}${LABELF}M.grh.TQ0126L028.ctl ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/GFGN${ANLTYPE}${LABELI}${LABELF}M.grh.TQ0126L028.ctl
  ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/GFGN${ANLTYPE}${LABELI}${LABELF}M.grh.TQ0126L028 ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/GFGN${ANLTYPE}${LABELI}${LABELF}M.grh.TQ0126L028

  ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/Preffix${LABELI}${LABELF}.${DIRRESOL} ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/Preffix${ANLTYPE}${LABELI}${LABELF}.${DIRRESOL}
  ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/Localiz${LABELI}${LABELF}.${DIRRESOL} ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/Localiz${ANLTYPE}${LABELI}${LABELF}.${DIRRESOL}
  ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/Identif${LABELI}${LABELF}.${DIRRESOL} ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/Identif${ANLTYPE}${LABELI}${LABELF}.${DIRRESOL}
else

  for MEM in $(seq -f %02g 1 ${ANLPERT})
  do

    export NMEM=${MEM}${ANLTYPE:0:1}
    ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${NMEM}/GFGN${NMEM}${LABELI}${LABELF}M.grh.${DIRRESOL}.ctl ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/GFGN${NMEM}${LABELI}${LABELF}M.grh.${DIRRESOL}.ctl
    ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${NMEM}/GFGN${NMEM}${LABELI}${LABELF}M.grh.${DIRRESOL} ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/GFGN${NMEM}${LABELI}${LABELF}M.grh.${DIRRESOL}

    ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${NMEM}/Preffix${LABELI}${LABELF}.${DIRRESOL} ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/Preffix${NMEM}${LABELI}${LABELF}.${DIRRESOL}
    ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${NMEM}/Localiz${LABELI}${LABELF}.${DIRRESOL} ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/Localiz${NMEM}${LABELI}${LABELF}.${DIRRESOL}
    ln -sf ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${NMEM}/Identif${LABELI}${LABELF}.${DIRRESOL} ${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/Identif${NMEM}${LABELI}${LABELF}.${DIRRESOL}
  done

fi

#
# Scripts e Figuras (apenas para o membro controle)
#

if [ ${ANLTYPE} == CTR -o ${ANLTYPE} == NMC -o ${ANLTYPE} == EIT -o ${ANLTYPE} == EIH ]
then

  export GRHDATAOUT=${DK_suite}/produtos/grh/dataout/${RES}/${LABELI}
  
  mkdir -p ${GRHDATAOUT}/AC/; mkdir -p ${GRHDATAOUT}/AL/; mkdir -p ${GRHDATAOUT}/AM/;
  mkdir -p ${GRHDATAOUT}/AP/; mkdir -p ${GRHDATAOUT}/BA/; mkdir -p ${GRHDATAOUT}/CE/;
  mkdir -p ${GRHDATAOUT}/DF/; mkdir -p ${GRHDATAOUT}/ES/; mkdir -p ${GRHDATAOUT}/GO/;
  mkdir -p ${GRHDATAOUT}/MA/; mkdir -p ${GRHDATAOUT}/MG/; mkdir -p ${GRHDATAOUT}/MS/;
  mkdir -p ${GRHDATAOUT}/MT/; mkdir -p ${GRHDATAOUT}/PA/; mkdir -p ${GRHDATAOUT}/PB/;
  mkdir -p ${GRHDATAOUT}/PE/; mkdir -p ${GRHDATAOUT}/PI/; mkdir -p ${GRHDATAOUT}/PR/;
  mkdir -p ${GRHDATAOUT}/RJ/; mkdir -p ${GRHDATAOUT}/RN/; mkdir -p ${GRHDATAOUT}/RO/;
  mkdir -p ${GRHDATAOUT}/RR/; mkdir -p ${GRHDATAOUT}/RS/; mkdir -p ${GRHDATAOUT}/SC/;
  mkdir -p ${GRHDATAOUT}/SE/; mkdir -p ${GRHDATAOUT}/SP/; mkdir -p ${GRHDATAOUT}/TO/;
  mkdir -p ${GRHDATAOUT}/WW/; mkdir -p ${GRHDATAOUT}/ZZ/;
  
  DATE=$(echo ${LABELI} | cut -c 1-8)
  HH=$(echo ${LABELI} | cut -c 9-10)
  DATEF=$(echo ${LABELF} | cut -c 1-8)
  HHF=$(echo ${LABELF} | cut -c 9-10)
  
  time1=$(date -d "${DATE} ${HH}:00" +"%HZ%d%b%Y")
  time2=$(date -d "${DATEF} ${HHF}:00" +"%HZ%d%b%Y")
  
  echo "LABELI = ${LABELI}   LABELF = ${LABELF}   LABELR = ${labelr}"
  echo "PARAMETROS GRADS ==> ${LABELI} ${LABELF} ${name} ${ext} ${ps} ${labelr}"
  
  cd ${GRHDATAOUT}
  rm -f ${GRHDATAOUT}/umrs_min??????????.txt
  
  #
  # Christopher - 24/01/2005
  # OBS: O GrADS script abaixo e quem inicializa/cria o arquivo deltag.${LABELI}.out
  #
  
  export name=GFGNNMC
  export ext=$(echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }')
  export ps=psuperf #reduzida
  export DATE=$(echo $LABELI | cut -c1-8)
  export HH=$(echo $LABELI | cut -c9-10)
  export DATEF=$(echo $LABELF | cut -c1-8)
  export HHF=$(echo $LABELF | cut -c9-10)
  export labelr=$(date -d "${DATE} ${HH}:00 12 hour ago" +"%Y%m%d%H")
  export julday1=$(date -d "${DATE} ${HH}:00" +"%j")
  export julday2=$(date -d "${DATEF} ${HHF}:00" +"%j")
  export ndays=$(echo ${julday2} ${julday1} |awk '{{nday=$1-$2}if(nday < 0){nday = $1 + (365-$2)} if(nday >7){nday=7} {print nday}}')
  
  echo "${LABELI} ${LABELF} ${name} ${ext} ${ps} ${labelr}"

cat << EOF1 > ${HOME_suite}/produtos/grh/scripts/meteogr.gs
'reinit'

pull argumentos

_LABELI=subwrd(argumentos,1)
_LABELF=subwrd(argumentos,2)
_nomea=subwrd(argumentos,3)
_trlv=subwrd(argumentos,4)
_ps=subwrd(argumentos,5)
_labelr=subwrd(argumentos,6)
_time1=subwrd(argumentos,7)
_time2=subwrd(argumentos,8)

* nome do arquivo com prefixos dos pontos
_nomeb='${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/Preffix'%_LABELI%_LABELF%'.'%_trlv

* nomes dos arquivos com identificacao e local dos pontos
_nomec='${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/Identif'%_LABELI%_LABELF%'.'%_trlv
_nomed='${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/Localiz'%_LABELI%_LABELF%'.'%_trlv

nomectl ='${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}/'%_nomea%_LABELI%_LABELF%'M.grh.'%_trlv%'.ctl'

say nomectl

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

rec=read(_nomeb)
nloc=sublin(rec,2)
status=sublin(rec,1)

* Se status=1, ha problema na leitura do arquivo _nomeb
say _nomeb' 'status

rec=read(_nomec)
nloc1=sublin(rec,2)

rec=read(_nomed)
nloc2=sublin(rec,2)

_loc=0
while (_loc < nloc)
  _loc=_loc+1

  say 'nloc ' nloc _loc

  'clear'
  'set x '_loc
  'set time '_time1' '_time2

  routine=dimet()

  if (_faz=1)
    lixo=write('umrs_min'_LABELI'.txt',_linha)
    lixo=write('umrs_min'_LABELI'.txt','')
  endif  
endwhile

lixo=close('umrs_min'_LABELI'.txt')

***********************************************
********* Grid History Maps Finalized *********
***********************************************

function dimet()

'set display color white'
'clear'
'set vpage 0 8.5 10.2 11'
'set grads off'

routine=title()

'set vpage 0 8.5 8.75 10.70';'set parea 0.7 8.0 0.3 1.6'
'set grads off'

routine=prec()

'set vpage 0 8.5 7.00 8.95';'set parea 0.7 8.0 0.3 1.6'
'set grads off'

routine=temp()

'set vpage 0 8.5 5.25 7.20';'set parea 0.7 8.0 0.3 1.6'
'set grads off'

routine=umrl()

if (_faz=1)
  routine=umrl_min()
endif 

'set vpage 0 8.5 3.50 5.45';'set parea 0.7 8.0 0.3 1.6'
'set grads off'

routine=wvel()

'set vpage 0 8.5 1.75 3.70';'set parea 0.7 8.0 0.3 1.6'
'set grads off'

if (_ps=reduzida)
  routine=psnm()
else
  routine=pslc()
endif

'set vpage 0 8.5 0.0 1.95';'set parea 0.7 8.0 0.3 1.6'
'set grads off'

routine=cbnv()

label=_LABELI%_LABELF

rec=read(_nomeb)
lab=sublin(rec,2)

taga=png
tagb=png
tagc=png

say 'printim ${GRHDATAOUT}/'lab'.png'

'printim ${GRHDATAOUT}/'lab'.png' 

'!rm -f meteogram'

if (_loc=1)
  '!rm -f puttag.'_LABELI'.out'
  '!rm -f deltag.'_LABELI'.out'
endif

'!echo put '_state'/'lab''label'.'taga' >> puttag.'_LABELI'.out'

return

************************************************

function title()

rec=read(_nomec)
local=sublin(rec,2)
_state=estado(local)

rec=read(_nomed)
lonlat=sublin(rec,2)
loi=substr(lonlat,1,3)
lof=substr(lonlat,4,2)
lo=substr(lonlat,6,1)
lai=substr(lonlat,7,2)
laf=substr(lonlat,9,2)
la=substr(lonlat,11,1)

lalo=loi%':'%lof%lo%'-'%lai%':'%laf%la

say ''
say 'Plotando localizacao numero = '_loc' Local: 'local

'set string 8 l 6'
'set strsiz .13 .14'

'draw string 0.4 0.7 CPTEC:'
'draw string 1.4 0.7 'lalo
'draw string 3.4 0.7 'local

_faz=0

n=1
while (n<=_nlonlat2ur)
  latlonur=subwrd(_lonlat2ur,n)
  if (lonlat=latlonur)
    lixo=write('umrs_min'_LABELI'.txt',lonlat' - 'local)
    _faz=1
    _linha=""
    n=9999
  endif
  n=n+1
endwhile

'q files'

lbin=sublin(result,3)
bin=subwrd(lbin,2)
utc=substr(bin,67,2)

'set t 5'
'q dims'

tm = sublin(result,5)
tim = subwrd(tm,6)
tmm = substr(tim,7,9)

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

'set gxout line'
'set grads off'
'set axlim 0 100'
'set cmark 0'
'set ylint 20'
'set ccolor 4'

'd umrs'

'set string 6 l 5'
'set strsiz .12 .13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Relative Humidity (%)'

return

************************************************

function umrl_min()

t=1
urmin=200
datamin=xx

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
    urmin=200
    datamin=xx
  endif
  t=t+1
endwhile

'set time '_time1' '_time2

return

************************************************

function cbnv()

'set gxout bar'
'set bargap 0'
'set barbase 0'
'set vrange 0 100'
'set ylint 20'
'set grads off'
'set ccolor 15'

'd cbnv'

'set string 6 l 5'
'set strsiz 0.12 0.13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'
'draw recf 0 0 8.5 0.1'
'draw string 1 1.75 Cloud Cover (%)'

return

************************************************

function snof()

'set gxout bar'
'set bargap 0'
'set barbase 0'
'set vrange 0 10'
'set ylint 2'
'set grads off'
'set ccolor 4'

'd neve'

'set string 6 l 5'
'set strsiz 0.12 0.13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Snow Fall (mm/h)'

return

************************************************

function prec()

'set gxout bar'
'set bargap 0'
'set barbase 0'
'set vrange 0 5'
'set ylint 1'
'set grads off'
'set ccolor 4'

'd prec'

'set gxout stat'
'd neve'

lnv=sublin(result,8)
nv=subwrd(lnv,5)

'set gxout bar'
'set string 6 l 5'
'set strsiz 0.12 0.13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'

if (nv > 0.0001)
  'set ccolor 3'
  'd neve'
  'draw string 1 1.75 Precipitation (blue) and Snow Fall (green) (mm/h)'
else
  'draw string 1 1.75 Precipitation (mm/h)'
endif

'set string 8 l 6'
'set strsiz .13 .14'

'draw string 7.1 1.75 '_trlv

return

************************************************

function psnm()

rotina=maxmin(psnm)

'set gxout line'
'set vrange '_vmin' '_vmax''
'set cmark 0'
'set ylint '_itvl''
'set ccolor 4'
'set grads off'

'd psnm'

'set string 6 l 5'
'set strsiz 0.12 0.13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Mean Sea Level Pressure (hPa)'

return

************************************************

function pslc()

rotina=maxmin(pslc)

'set gxout line'
'set vrange '_vmin' '_vmax''
'set cmark 0'
'set ylint '_itvl''
'set ccolor 4'
'set grads off'

'd pslc'

'set string 6 l 5'
'set strsiz 0.12 0.13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Pressure (hPa)        Model Altitude: '_tpg' m'

return

************************************************

function wvel()

'set lev 1000 '
'set gxout vector'
'set ylab off'
'set grads off'
'set arrowhead 0.075'
'set z 0.5 1.5'

'd skip(uves,1,12);vves'

'set gxout line'
'set grads off'
'set z 1'
'set ylab on'
'set cmark 0'
'set ylint 2'
'set ccolor 4'

'd mag(uves,vves)'

'set string 6 l 5'
'set strsiz .12 .13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Wind (m/s)'

return

************************************************

function temp()

rotina=maxmin(tems)

'set gxout line'
'set vrange '_vmin' '_vmax''
'set grads off'
'set ylint '_itvl''
'set ccolor 4'
'set cmark 0'

'd tems'

'set string 6 l 5'
'set strsiz .12 .13'
'set line 0'

'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Temperature (\`aO\`nC)'

return

************************************************

function maxmin(var)

'set t 1'

'd max('var',t=1,t='_ntimes',1)'

linha=sublin(result,2)
imax=subwrd(linha,4)

'd min('var',t=1,t='_ntimes',1)'

linha=sublin(result,2);imin=subwrd(linha,4)
say linha
say imin

if(imin>0)
  imin=imin-1
else
  imin=imin+1
endif

if(imax>0)
  imax=imax+1
else
  imax=imax-1
endif

_vmax=math_nint(imax)
_vmin=math_nint(imin)
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

  DATE=$(echo ${LABELI} | cut -c 1-8)
  HH=$(echo ${LABELI} | cut -c 9-10)
  DATEF=$(echo ${LABELF} | cut -c 1-8)
  HHF=$(echo ${LABELF} | cut -c 9-10)
  
  time1=$(date -d "$DATE $HH:00" +"%HZ%d%b%Y")
  time2=$(date -d "$DATEF $HHF:00" +"%HZ%d%b%Y")
  
  echo ${LABELI} ${LABELF} ${name} ${ext} ${ps} ${labelr}
  echo ${time1} ${time2}
  
  if [ $GSSTEP = 1 ]
  then
  
echo "meteogr.gs ${LABELI} ${LABELF} ${name} ${ext} ${ps} ${labelr} ${time1} ${time2}"
${DIRGRADS}/grads -bp  << EOT
run ${HOME_suite}/produtos/grh/scripts/meteogr.gs
${LABELI} ${LABELF} ${name} ${ext} ${ps} ${labelr} ${time1} ${time2}
EOT
  
  fi

fi

exit 0
