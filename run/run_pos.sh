#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para submeter o pós-processamento das previsões do modelo 
# atmosférico para a integração das análises do Sistema de Previsão 
# por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_pos.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5>
#                    <opcao6> <opcao7>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> num_proc  -> número de processadores
#            
#            <opcao2> num_nos   -> número de cores por nó
#            
#            <opcao3> num_omp   -> número de processos omp por
#                                  processo mpi
#
#            <opcao4> resolucao -> resolução espectral do modelo
#                                
#            <opcao5> datai     -> data da análise corrente 
#
#            <opcao6> dataf     -> data final da previsão 
#
#            <opcao7> prefixo   -> sufixo que identifica o tipo de
#                                  análise
#
#            <opcao8> membro    -> tamanho do conjunto de perturbações
#
#            
#  Uso/Exemplos: 
# 
#  Membro controle:
#  - pós-processamento das previsões a partir das análise do NCEP (prefixo CTR):
#  ./run_pos.sh 48 24 1 TQ0126L028 2013010100 2013011600 CTR 
#  - pós-processamento das previsões a partir das análise do ECMWF (prefixo EIT, resolução de 1,5 graus):
#  ./run_pos.sh 48 24 1 TQ0126L028 2013010100 2013011600 EIT 
#  - pós-processamento das previsões a partir das análise do ECMWF (prefixo EIH, resolução de 0,75 graus):
#  ./run_pos.sh 48 24 1 TQ0126L028 2013010100 2013011600 EIH 
# 
#  Demais membros:
#  - pos-processamento das previsoes geradas a partir das analises perturbadas randomicamente:
#  ./run_pos.sh 48 24 1 TQ0126L028 2013010100 2013011600 RDP 7
#  - pos-processamento das previsoes geradas a partir das analises perturbadas por EOF (subtraidas):
#  ./run_pos.sh 48 24 1 TQ0126L028 2013010100 2013011600 NPT 7
#  - pos-processamento das previsoes geradas a partir das analises perturbadas por EOF (somadas):
#  ./run_pos.sh 48 24 1 TQ0126L028 2013010100 2013011600 PPT 7
#
# !REVISION HISTORY:
#
# XX Julho de 2017   - C. F. Bastarz - Versão inicial.  
# 16 Agosto de 2017  - C. F. Bastarz - Inclusão comentários.
# 18 Agosto de 2017  - C. F. Bastarz - Modificação na ordem dos argumentos.
# 26 Outubro de 2017 - C. F. Bastarz - Inclusão dos prefixos das análises do ECMWF (EIT/EIH).
# 25 Janeiro de 2018 - C. F. Bastarz - Ajustes nos prefixos NMC (controle 48h) e CTR (controle 120h)
# 18 Junho de 2021   - C. F. Bastarz - Revisão geral.
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

#
# Função para criar o namelist
#

cria_namelist() {

sed -e "s;#TRUNC#;${1};g" \
    -e "s;#LEV#;${2};g" \
    -e "s;#LABELI#;${3};g" \
    -e "s;#LABELF#;${4};g" \
    -e "s;#PREFIX#;${5};g" \
    -e "s;#DATAIN#;${6};g" \
    -e "s;#DATAOUT#;${7};g" \
    -e "s;#DATALIB#;${8};g" \
    -e "s;#Binary#;${9};g" \
    -e "s;#REQTB#;${10};g" \
    -e "s;#REGINT#;${11};g" \
    -e "s;#RES#;${12};g" \
    ${13}/POSTIN-GRIB.template > ${14}/POSTIN-GRIB

echo "Namelist criado em: ${14}/POSTIN-GRIB"

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
  echo "MPPNPPN esta faltando" 
  exit 3
else
  export MPPNPPN=${2}  
fi

if [ -z "${3}" ]
then
  echo "MPPDEPTH esta faltando" 
  exit 3
else
  export MPPDEPTH=${3}  
fi

if [ -z "${4}" ]
then
  echo "RESOL esta faltando" 
  exit 3
else
  export RES=${4}  
fi

if [ -z "${5}" ]
then
  echo "LABELI esta faltando" 
  exit 3
else
  export LABELI=${5} 
fi

if [ -z "${6}" ]
then
  echo "LABELF esta faltando" 
  exit 3
else
  export LABELF=${6}  
fi

if [ -z "${7}" ]
then
  echo "ANLTYPE esta faltando"
  exit 4 
else
  if [ "${7}" == "CTR" -o "${7}" == "NMC" -o "${7}" == "EIT" -o "${7}" == "EIH" ] # pode ser RDP, NPT ou PPT
  then 
    export ANLTYPE=${7}  
  else
    if [ -z "${8}" ]
    then
      echo "ANLPERT esta faltando" 
      exit 5
    else
      export ANLTYPE=${7}  
      export ANLPERT=${8}  
    fi
  fi
fi

export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ; pwd)

. ${FILEENV} ${RES} ${ANLTYPE}

#
# Variáveis utilizadas no script de submissão
#

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=$(echo ${TRCLV} | cut -c 1-6)
export NIVEL=$(echo ${TRCLV} | cut -c 7-11)

export DIRRESOL=$(echo ${TRC} ${LV} | awk '{printf("TQ%4.4dL%3.3d\n",$1,$2)}')
export MAQUI=$(hostname -s)

export SCRIPTFILEPATH=${HOME_suite}/run/set$(echo "${ANLTYPE}" | awk '{print tolower($0)}')${ANLPERT}posg.${DIRRESOL}.${LABELI}.${MAQUI}
export NAMELISTFILEPATH=${HOME_suite}/run

export BINARY=".FALSE."
export REQTB="p"
export REGINT=".FALSE."
export RESPOS="-0.50000"

#
# Cria o namelist para os casos determinístico e conjunto e linca o executável
#

if [ ${ANLTYPE} == CTR -o ${ANLTYPE} == NMC -o ${ANLTYPE} == EIT -o ${ANLTYPE} == EIH ]
then

  EXECFILEPATH=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}

  mkdir -p ${EXECFILEPATH}/setout
   
  ln -sf ${DK_suite}/pos/exec/PostGrib ${EXECFILEPATH}

  export DATALIB=${DK_suite}/pos/datain/

  export DATAIN=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}
  export DATAOUT=${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}

  mkdir -p ${DATAOUT}

  export PREFIX=${ANLTYPE}

  cria_namelist ${RESOL} ${NIVEL} ${LABELI} ${LABELF} ${PREFIX} ${DATAIN} ${DATAOUT} ${DATALIB} ${BINARY} ${REQTB} ${REGINT} ${RESPOS} ${NAMELISTFILEPATH} ${EXECFILEPATH} 
 
else

  for MEM in $(seq -f %02g 1 ${ANLPERT})
  do

    EXECFILEPATH=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/${MEM}${ANLTYPE:0:1}
    EXECFILEPATHMEM=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/${MEM}${ANLTYPE:0:1}

    mkdir -p ${EXECFILEPATH}/setout ${EXECFILEPATHMEM}
   
    ln -sf ${DK_suite}/pos/exec/PostGrib ${EXECFILEPATHMEM}

    export DATALIB=${DK_suite}/pos/datain/

    export DATAIN=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${MEM}${ANLTYPE:0:1}
    export DATAOUT=${DK_suite}/pos/dataout/${DIRRESOL}/${LABELI}/${MEM}${ANLTYPE:0:1}

    mkdir -p ${DATAOUT}

    export PREFIX=${MEM}${ANLTYPE:0:1}

    cria_namelist ${RESOL} ${NIVEL} ${LABELI} ${LABELF} ${PREFIX} ${DATAIN} ${DATAOUT} ${DATALIB} ${BINARY} ${REQTB} ${REGINT} ${RESPOS} ${NAMELISTFILEPATH} ${EXECFILEPATH} 

  done

fi

if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC -a ${ANLTYPE} != EIT -a ${ANLTYPE} != EIH ]
then
  #export PBSOUTFILE="#PBS -o ${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.out"
  #export PBSERRFILE="#PBS -e ${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.err"
  #export PBSDIRECTIVENAME="#PBS -N POSENS${ANLTYPE}"
  #export PBSDIRECTIVEARRAY="#PBS -J 1-${ANLPERT}"
  #export PBSMEM="export MEM=\$(printf %02g \${PBS_ARRAY_INDEX})"
  export PBSOUTFILE="#SBATCH --output=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.out"
  export PBSERRFILE="#SBATCH --error=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.err"
  export PBSDIRECTIVENAME="#SBATCH --job-name=POSENS${ANLTYPE}"
  export PBSDIRECTIVEARRAY="#SBATCH --array=1-${ANLPERT}"
  export PBSMEM="export MEM=\$(printf %02g \${SLURM_ARRAY_TASK_ID})"
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/\${MEM}${ANLTYPE:0:1}"
else
  #export PBSOUTFILE="#PBS -o ${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.out"
  #export PBSERRFILE="#PBS -e ${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.err"
  #export PBSDIRECTIVENAME="#PBS -N POS${ANLTYPE}"
  export PBSOUTFILE="#SBATCH --output=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.out"
  export PBSERRFILE="#SBATCH --error=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}/setout/Out.pos.${LABELI}.MPI${MPPWIDTH}.err"
  export PBSDIRECTIVENAME="#SBATCH --job-name=POS${ANLTYPE}"
  export PBSDIRECTIVEARRAY=""
  export PBSMEM=""
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/pos/exec_${ANLTYPE}${LABELI}.${ANLTYPE}"
fi

#
# Script de submissão
#

cat <<EOF0 > ${SCRIPTFILEPATH}
#! /bin/bash -x
###PBS -j oe
###PBS -l walltime=01:00:00
###PBS -l mppwidth=${MPPWIDTH}
###PBS -l mppnppn=${MPPNPPN}
###PBS -l mppdepth=${MPPDEPTH}
###PBS -A CPTEC
###PBS -V
###PBS -S /bin/bash
##${PBSDIRECTIVENAME}
##${PBSDIRECTIVEARRAY}
###PBS -q ${QUEUE}

###SBATCH --output=${BAMRUN}/setout/Out.model.${PREFIX}.${LABELI}.${tmstp}.MPI${MPPWIDTH}.out
###SBATCH --error=${BAMRUN}/setout/Out.model.${PREFIX}.${LABELI}.${tmstp}.MPI${MPPWIDTH}.err
${PBSOUTFILE}
${PBSERRFILE}
#SBATCH --time=${WALLTIME}
#SBATCH --tasks-per-node=${MPPWIDTH}
#SBATCH --nodes=${MPPDEPTH}
${PBSDIRECTIVENAME}
${PBSDIRECTIVEARRAY}
#SBATCH --partition=${QUEUE}

ulimit -s unlimited
ulimit -c unlimited

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

export PBS_SERVER=${pbs_server1}
export KMP_STACKSIZE=128m

${PBSMEM}
${PBSEXECFILEPATH}

cd \${EXECFILEPATH}

#echo \${PBS_JOBID} > ${HOME_suite}/run/this.pos.job.${LABELI}.${ANLTYPE}

date

#aprun -m500h -n ${MPPWIDTH} -N ${MPPNPPN} -d ${MPPDEPTH} \${EXECFILEPATH}/PostGrib < \${EXECFILEPATH}/POSTIN-GRIB > \${EXECFILEPATH}/setout/Print.pos.${LABELI}.MPI${MPPWIDTH}.log 

singularity exec -e --bind /mnt/beegfs/carlos.bastarz:/mnt/beegfs/carlos.bastarz /mnt/beegfs/carlos.bastarz/containers/egeon_dev.sif mpirun -np ${MPPWIDTH} \${EXECFILEPATH}/PostGrib < \${EXECFILEPATH}/POSTIN-GRIB > \${EXECFILEPATH}/setout/Print.pos.${LABELI}.MPI${MPPWIDTH}.log 

date
EOF0

#
# Submissão (gribmap é executado em outro script)
#

chmod +x ${SCRIPTFILEPATH}

#qsub -W block=true ${SCRIPTFILEPATH}
sbatch ${SCRIPTFILEPATH}

#if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC ]
#then
#
#  JOBID=$(cat ${HOME_suite}/run/this.pos.job.${LABELI}.${ANLTYPE} | awk -F "[" '{print $1}')
#
#  for mem in $(seq 1 ${ANLPERT})
#  do
#
#    jobidname="POSENS${ANLTYPE}.o${JOBID}.${mem}"
#    posoutname="Out.pos.${LABELI}.MPI${MPPWIDTH}.${mem}.out"
#
#    until [ -e "${HOME_suite}/run/${jobidname}" ]; do sleep 1s; done
#    mv -v ${HOME_suite}/run/${jobidname} ${EXECFILEPATH}/setout/${posoutname}
#
#  done
#
#else
#
#  JOBID=$(cat ${HOME_suite}/run/this.pos.job.${LABELI}.${ANLTYPE} | awk -F "." '{print $1}')
#
#  jobidname="POS${ANLTYPE}.o${JOBID}"
#  posoutname="Out.pos.${LABELI}.MPI${MPPWIDTH}.out"
#
#  until [ -e "${HOME_suite}/run/${jobidname}" ]; do sleep 1s; done 
#  mv -v ${HOME_suite}/run/${jobidname} ${EXECFILEPATH}/setout/${posoutname}
#
#fi
#
#rm ${HOME_suite}/run/this.pos.job.${LABELI}.${ANLTYPE}

exit 0
