#! /bin/ksh 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para submeter o modelo atmosférico para a integração das
# análises do Sistema de Previsão por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_model.ksh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5>
#                      <opcao6> <opcao7> <opcao8> <opcao9> <opcao10>
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
#            <opcao5> pref_topo -> prefixo que identifica o tipo de
#                                  análise de acordo com topografia
#
#            <opcao6> datai     -> data da análise corrente 
#
#            <opcao7> dataf     -> data da previsão final
#
#            <opcao8> prefixo   -> prefixo que identifica o tipo de análise 
#            
#            <opcao9> init      -> tipo de inicialização 
#
#            <opcao10> n_mem b  -> tamanho do conjunto de perturbações
#            
#  Uso/Exemplos: 
# 
#  Membro controle:
# - previsões a partir da análise controle (prefixo CTR - análises NCEP ou ECMWF): 
# ./run_model.ksh 48 24 1 TQ0126L028 SMT 2013010100 2013010300 CTR 2 1
# - previsões a partir da análise controle (prefixo NMC - análises NCEP):
# ./run_model.ksh 48 24 1 TQ0126L028 SMT 2013010100 2013011600 NMC 2 1
# - previsões a partir da análise controle (prefixo EIT - análises ECMWF de 1,5 graus): 
# ./run_model.ksh 48 24 1 TQ0126L028 SMT 2013010100 2013011600 EIT 2 1
#  - previsoes a partir das analises do ECMWF (prefixo EIH - análises ECMWF de 0,75 graus)
# ./run_model.ksh 48 24 1 TQ0126L028 EIH 2013010100 2013011600 EIH 2 1
# 
#  Demais membros:
#  - previsões a partir das analises perturbadas randomicamente:
# ./run_model.ksh 48 24 1 TQ0126L028 SMT 2013010100 2013010300 RDP 2 7
#  - previses a partir das analises perturbadas por EOF (subtraidas):
# ./run_model.ksh 48 24 1 TQ0126L028 SMT 2013010100 2013010300 NPT 2 7
#  - previsões a partir das analises perturbadas por EOF (somadas):
# ./run_model.ksh 48 24 1 TQ0126L028 SMT 2013010100 2013010300 PPT 2 7
#
# !REVISION HISTORY:
#
# XX Julho de 2017 - C. F. Bastarz - Versão inicial.  
# 16 Agosto de 2017 - C. F. Bastarz - Inclusão comentários.
# 17 Agosto de 2017 - C. F. Bastarz - Inclusão da opção <dataf>
# 18 Agosto de 2017 - C. F. Bastarz - Modificação nos argumentos de entrada.
# 22 Agosto de 2017 - C. F. Bastarz - Inclusão do sleep 10s no final do script
#                                     de submissão para aguardar o I/O do BAM
# 26 Outubro de 2017 - C. F. Bastarz - Inclusão dos prefixos das análises do ECMWF (EIT/EIH)
#
# !REMARKS:
#
# !BUGS:
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
set -o xtrace

# Menu de opções/ajuda
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

cria_namelist() {

sed  -e "s;#TRUNC#;${1};g" \
     -e "s;#NLEV#;${2};g" \
     -e "s;#DELT#;${3};g" \
     -e "s;#LABELI#;${4:8:2},${4:6:2},${4:4:2},${4:0:4};g" \
     -e "s;#LABELW#;${5:8:2},${5:6:2},${5:4:2},${5:0:4};g" \
     -e "s;#LABELF#;${6:8:2},${6:6:2},${6:4:2},${6:0:4};g" \
     -e "s;#DHFCT#;${7};g" \
     -e "s;#DHRES#;${8};g" \
     -e "s;#GENRES#;${9};g" \
     -e "s;#PREFIX#;${10};g" \
     -e "s;CPT;${11};g" \
     -e "s;#NMSST#;${12};g" \
     -e "s;#PATHIN#;${13};g" \
     -e "s;#PATHOU#;${14};g" \
     -e "s;#RSTIN#;${15};g" \
     -e "s;#RSTOU#;${16};g" \
     -e "s;#EIGENINIT#;${17};g" \
     -e "s;#MGIVEN#;${18};g" \
     -e "s;#GAUSSGIVEN#;${19};g" \
     -e "s;#INITLZ#;${20};g" \
     ${21}/MODELIN.template > ${22}/MODELIN

echo "Namelist criado em: ${22}/MODELIN"

}

# Verificação dos argumentos de entrada
if [ -z "${1}" ]
then
  echo "MPPWIDTH is not set" 
  exit 3
else
  export MPPWIDTH=${1}  
fi
if [ -z "${2}" ]
then
  echo "MPPNPPN is not set" 
  exit 3
else
  export MPPNPPN=${2}  
fi
if [ -z "${3}" ]
then
  echo "MPPDEPTH is not set" 
  exit 3
else
  export MPPDEPTH=${3}  
fi
if [ -z "${4}" ]
then
  echo "RESOL is not set" 
  exit 3
else
  export RES=${4}  
fi
if [ -z "${5}" ]
then
  echo "PREFIC is not set" 
  exit 3
else
  export PREFIC=${5}  
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
  echo "ANLTYPE is not set" 
  exit 3
else
  export ANLTYPE=${8}  
fi
if [ -z "${9}" ]
then
  echo "INITLZ is not set" 
  exit 3
else
  export INITLZ=${9}  
fi
if [ -z "${10}" ]
then
  echo "ANLPERT is not set" 
else
  export ANLPERT=${10}  
fi

# Diretórios principais
export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ../; pwd)

. ${FILEENV} ${RES} ${PREFIC}

cd ${HOME_suite}/../run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

# Se a previsão for a controle para a perturbação, integra o modelo por apenas 48 horas;
# Se a previsão for a controle final, integra o modelo por 360 horas (15 dias);
# Se a previsão for a partir do conjunto de perturbações por EOF, integra o modelo por 360 horas (15 dias).
export LABELW=${LABELF}

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

DIRRESOL=$(echo ${TRC} ${LV} | awk '{printf("TQ%4.4dL%3.3d\n",$1,$2)}')
MAQUI=$(hostname -s)

SCRIPTFILEPATH=${HOME_suite}/../run/set$(echo "${ANLTYPE}" | awk '{print tolower($0)}')${ANLPERT}modg.${DIRRESOL}.${LABELI}.${MAQUI}
NAMELISTFILEPATH=${HOME_suite}/../run

# As opções abaixo fazem referência à frequência de saída das previsões (DHFCT) e dos arquivos de restart (DHRES)
# Se ANLTYPE for igual a CTR ou RDP, então as previsões serão referentes à análise controle, com previsões para
#2 dias e com saídas a cada 3 horas;
# Se ANLTYPE for igual a NMC, NPT ou PPT, então as previsões serão referentes às análises controle e perturbadas
# por EOF (respectivamente), e serão feitas para 15 dias e com saída a cada 3 horas.
if [ ${ANLTYPE} == RDP -o ${ANLTYPE} == CTR ]
then
  export DHFCT=3
  export DHRES=3
elif [ ${ANLTYPE} == NMC -o ${ANLTYPE} == NPT -o ${ANLTYPE} == PPT -o ${ANLTYPE} == EIT -o ${ANLTYPE} == EIH ]
then
  export DHFCT=6
  export DHRES=6
else
  export DHFCT=6
  export DHRES=6 
fi

export NMSST="sstwkl"

export GENRES='.FALSE.'
export EIGENINIT=".FALSE."
export MGIVEN=".TRUE."      
export GAUSSGIVEN=".TRUE."  

export PATHIN=${DK_suite}/model/datain

# Variáveis utilizadas no script de submissão
if [ ${ANLTYPE} == CTR -o ${ANLTYPE} == NMC -o ${ANLTYPE} == EIT -o ${ANLTYPE} == EIH ]
then

  EXECFILEPATH=${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}

  mkdir -p ${EXECFILEPATH}/setout
   
  ln -sf ${DK_suite}/model/exec/ParModel_MPI ${EXECFILEPATH}

  export RSTIN=${DK_suite}/model/dataout/${TRCLV}/${LABELI}/${ANLTYPE}/RST
  export RSTOU=${DK_suite}/model/dataout/${TRCLV}/${LABELW}/${ANLTYPE}/RST
  export DIRFNAMEOUTPUT=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${ANLTYPE}

  mkdir -p ${DIRFNAMEOUTPUT}

  if [ ${ANLTYPE} == CTR ]
  then
    export PREFIY=CTR
  elif [ ${ANLTYPE} == EIT ]
  then
    export PREFIY=EIT
  elif [ ${ANLTYPE} == EIH ]
  then
    export PREFIY=EIH
  else
    export PREFIY=NMC
  fi
  export PREFIX=${PREFIC}

  cria_namelist ${TRC} ${LV} ${TIMESTEP} ${LABELI} ${LABELW} ${LABELF} ${DHFCT} ${DHRES} ${GENRES} ${PREFIX} ${PREFIY} ${NMSST} ${PATHIN} ${DIRFNAMEOUTPUT} ${RSTIN} ${RSTOU} ${EIGENINIT} ${MGIVEN} ${GAUSSGIVEN} ${INITLZ} ${NAMELISTFILEPATH} ${EXECFILEPATH}
 
else

  for MEM in $(seq -f %02g 1 ${ANLPERT})
  do

    EXECFILEPATH=${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}
    EXECFILEPATHMEM=${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}/${MEM}${ANLTYPE:0:1}

    mkdir -p ${EXECFILEPATH}/setout ${EXECFILEPATHMEM}
   
    ln -sf ${DK_suite}/model/exec/ParModel_MPI ${EXECFILEPATHMEM}

    export RSTIN=${DK_suite}/model/dataout/${TRCLV}/${LABELI}/${MEM}${ANLTYPE:0:1}/RST
    export RSTOU=${DK_suite}/model/dataout/${TRCLV}/${LABELW}/${MEM}${ANLTYPE:0:1}/RST
    export DIRFNAMEOUTPUT=${DK_suite}/model/dataout/${DIRRESOL}/${LABELI}/${MEM}${ANLTYPE:0:1}

    mkdir -p ${DIRFNAMEOUTPUT}

    export PREFIY=${MEM}${ANLTYPE:0:1}
    export PREFIX=${MEM}${ANLTYPE:0:1}

    cria_namelist ${TRC} ${LV} ${TIMESTEP} ${LABELI} ${LABELW} ${LABELF} ${DHFCT} ${DHRES} ${GENRES} ${PREFIX} ${PREFIY} ${NMSST} ${PATHIN} ${DIRFNAMEOUTPUT} ${RSTIN} ${RSTOU} ${EIGENINIT} ${MGIVEN} ${GAUSSGIVEN} ${INITLZ} ${NAMELISTFILEPATH} ${EXECFILEPATHMEM}

  done

fi

if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC -a ${ANLTYPE} != EIT -a ${ANLTYPE} != EIH ]
then
  export PBSOUTFILE="#PBS -o ${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}/setout/Out.model.${LABELI}.MPI${MPPWIDTH}.out"
  export PBSERRFILE="#PBS -e ${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}/setout/Out.model.${LABELI}.MPI${MPPWIDTH}.err"
  export PBSDIRECTIVENAME="#PBS -N BAMENS${ANLTYPE}"
  export PBSDIRECTIVEARRAY="#PBS -J 1-${ANLPERT}"
  export PBSMEM="export MEM=\$(printf %02g \${PBS_ARRAY_INDEX})"
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}/\${MEM}${ANLTYPE:0:1}"
  export MONITORFILE="${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}/model.\${PBS_ARRAY_INDEX}"
else
  export PBSOUTFILE="#PBS -o ${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}/setout/Out.model.${LABELI}.MPI${MPPWIDTH}.out"
  export PBSERRFILE="#PBS -e ${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}/setout/Out.model.${LABELI}.MPI${MPPWIDTH}.err"
  export PBSDIRECTIVENAME="#PBS -N BAM${ANLTYPE}"
  export PBSDIRECTIVEARRAY=""
  export PBSMEM=""
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK_suite}/model/exec_${PREFIC}${LABELI}.${ANLTYPE}"
  export MONITORFILE="${DK_suite}/model/exec_${PREFIC}${LABELI}.\${ANLTYPE}/model.\${ANLTYPE}"
fi

if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != RDP ]
then
  export walltime="04:00:00"
else
  export walltime="02:00:00"
fi

# Script de submissão
cat <<EOF0 > ${SCRIPTFILEPATH}
#! /bin/bash -x
#PBS -j oe
#PBS -l walltime=${walltime}
#PBS -l mppwidth=${MPPWIDTH}
#PBS -l mppnppn=${MPPNPPN}
#PBS -l mppdepth=${MPPDEPTH}
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
${PBSDIRECTIVENAME}
${PBSDIRECTIVEARRAY}
#PBS -q ${QUEUE}

export PBS_SERVER=eslogin13
export HUGETLB_MORECORE=yes
export HUGETLB_ELFMAP=W
export HUGETLB_FORCE_ELFMAP=yes+
export MPICH_ENV_DISPLAY=1
export HUGETLB_DEFAULT_PAGE_SIZE=2m

${PBSMEM}
${PBSEXECFILEPATH}

cd \${EXECFILEPATH}

export OMP_NUM_THREADS=6

ulimit -s unlimited

echo \${PBS_JOBID} > ${HOME_suite}/../run/this.job.${LABELI}.${ANLTYPE}

date

mkdir -p \${EXECFILEPATH}/setout

aprun -n ${MPPWIDTH} -N ${MPPNPPN} -d ${MPPDEPTH} -ss \${EXECFILEPATH}/ParModel_MPI < \${EXECFILEPATH}/MODELIN > \${EXECFILEPATH}/Print.model.${LABELI}.MPI${MPPWIDTH}.log

date

sleep 10s # espera para terminar todos os processos de I/O

touch ${MONITORFILE}
EOF0

# Submete o script e aguarda o fim da execução
chmod +x ${SCRIPTFILEPATH}

qsub ${SCRIPTFILEPATH}

if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC ]
then

  for i in $(seq 1 ${ANLPERT})
  do

    until [ -e ${EXECFILEPATH}/model.${i} ]; do sleep 1s; done
    rm ${EXECFILEPATH}/model.${i}

  done

else

  until [ -e ${EXECFILEPATH}/model.${ANLTYPE} ]; do sleep 1s; done
  rm ${EXECFILEPATH}/model.${ANLTYPE}

fi

#JOBID=$(cat ${HOME_suite}/../run/this.job.${LABELI}.${ANLTYPE} | awk -F "[" '{print $1}')
JOBID=$(cat ${HOME_suite}/../run/this.job.${LABELI}.${ANLTYPE} | cut -c 1-7)

if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC ]
then

  for i in $(seq 1 ${ANLPERT})
  do

    until [ -e ${HOME_suite}/../run/BAMENS${ANLTYPE}.o${JOBID}.${i} ]; do sleep 1s; done
    mv -v ${HOME_suite}/../run/BAMENS${ANLTYPE}.o${JOBID}.${i} ${EXECFILEPATH}/setout/Out.model.${LABELI}.MPI${MPPWIDTH}.${i}.out
  
  done

else

  until [ -e  ${HOME_suite}/../run/BAM${ANLTYPE}.o${JOBID} ]; do sleep 1s; done 
  mv -v ${HOME_suite}/../run/BAM${ANLTYPE}.o${JOBID} ${EXECFILEPATH}/setout/Out.model.${LABELI}.MPI${MPPWIDTH}.out

fi

rm ${HOME_suite}/../run/this.job.${LABELI}.${ANLTYPE}

exit 0
