#! /bin/bash -x
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para criar um subset dos arquivos pós-processados para as
# variáveis temperatura absoluta em 850 hPa, altura geopotencial em 
# 500 hPa e pressão em superfície. Cada arquivo contém apenas a
# variável especificada.
#
# !INTERFACE:
#      ./run_subset.sh <opcao1> <opcao2> <opcao3> <opcao4>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao -> resolução espectral do modelo
#                                
#            <opcao2> data      -> data da análise corrente (a partir
#                                  da qual as previsões foram feitas)
#
#            <opcao3> anlpert   -> número de perturbações do ensemble
#
#            <opcao4> anltype   -> prefixo da perturbação
#
#  Uso/Exemplos: ./run_subset.sh TQ0126L028 2012123118 7 NPT
#                ./run_subset.sh TQ0126L028 2012123118 7 PPT
#                ./run_subset.sh TQ0126L028 2012123118 1 NMC
#
# !REVISION HISTORY:
#
# 11 Setembro de 2020 - C. F. Bastarz - Versão inicial.  
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

# Menu de opções/ajuda
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

if [ -z "${1}" ]
then
  echo "First argument is not set (RES: TQXXXXLXXX)"
  exit
else
  RES=${1}
fi

if [ -z "${2}" ]
then
  echo "Second argument is not set (LABELI: yyyymmddhh)"
  exit
else
  LABELI=${2}
fi

if [ -z "${3}" ]
then
  echo "Third argument is not set (ANLPERT)"
  exit
else
  ANLPERT=${3}
fi

if [ -z "${4}" ]
then
  echo "Fourth argument is not set (ANLTYPE)"
  exit
else
  ANLTYPE=${4}
fi

export inctime=${HOME}/bin/inctime
export DIRGRADS=/cray_home/carlos_bastarz/bin/tools/opengrads-2.2.1.oga.1/Contents

export HOME_suite=/lustre_xc50/carlos_bastarz/CRPS/scripts/recorta_dados
export DK_suite=${HOME_suite}
export DK2=/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50

cd ${HOME_suite}/

RUNTM=$(date +"%s")

if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC -a ${ANLTYPE} != EIT -a ${ANLTYPE} != EIH ]
then
  export PBSDIRECTIVENAME="#PBS -N SUBENS${ANLTYPE}"
  export PBSDIRECTIVEARRAY="#PBS -J 1-${ANLPERT}"
  export PBSMEM="export MEM=\$(printf %02g \${PBS_ARRAY_INDEX})"
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK2}/pos/dataout/${RES}/${LABELI}/\${MEM}${ANLTYPE:0:1}"
else
  export PBSDIRECTIVENAME="#PBS -N SUB${ANLTYPE}"
  export PBSDIRECTIVEARRAY=""
  export PBSMEM="export MEM=${ANLTYPE}"
  export PBSEXECFILEPATH="export EXECFILEPATH=${DK2}/pos/dataout/${RES}/${LABELI}/${MEM}${ANLTYPE}"
fi

export MAQUI=$(uname -s)

# Script de submissão
SCRIPTSFILES=setsubset${ANLTYPE}.${RES}.${LABELI}.${MAQUI}

cat <<EOT0 > ${SCRIPTSFILES}
#! /bin/bash 
#PBS -o ${DK_suite}/setlats4d${ANLTYPE}${RES}${LABELI}.${MAQUI}.${RUNTM}.out
#PBS -e ${DK_suite}/setlats4d${ANLTYPE}${RES}${LABELI}.${MAQUI}.${RUNTM}.err
#PBS -l walltime=0:15:00
#PBS -l mppnppn=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
${PBSDIRECTIVENAME}
${PBSDIRECTIVEARRAY}
#PBS -q pesq

export lats4d=${DIRGRADS}/lats4d.sh

${PBSMEM}

if [ \${MEM} == "NMC" ]; then export TPERT=\${MEM}; else TPERT=\${MEM}${ANLTYPE:0:1}; fi

${PBSEXECFILEPATH}

cria_ctl(){
diretorio=\$(dirname \${1})
nome=\$(basename \${1})
var=\${2}

nomectl=\$(echo \${nome} | sed "s,.grads,.ctl,g")

if [ ! -e "\${diretorio}/\${nomectl}" ]
then

  dataanl=\$(echo \${nome} | cut -c 8-17)
  datafct=\$(echo \${nome} | cut -c 18-27)

  anoanl=\$(echo \${dataanl} | cut -c 1-4)
  mesanl=\$(echo \${dataanl} | cut -c 5-6)
  diaanl=\$(echo \${dataanl} | cut -c 7-8)
  hsnanl=\$(echo \${dataanl} | cut -c 9-10)

  anofct=\$(echo \${datafct} | cut -c 1-4)
  mesfct=\$(echo \${datafct} | cut -c 5-6)
  diafct=\$(echo \${datafct} | cut -c 7-8)
  hsnfct=\$(echo \${datafct} | cut -c 9-10)

  difdia=\$(expr \${diafct} - \${diaanl})
  diadiahoras=\$(expr \${difdia} \* 24)

  difhora=\$(expr \${hsnfct} - \${hsnanl})

  horasfctanl=\$(expr \${diadiahoras} + \${difhora})

  if [ \${mesanl} -eq "01" ]; then mesfmt="JAN"; fi
  if [ \${mesanl} -eq "02" ]; then mesfmt="FEB"; fi
  if [ \${mesanl} -eq "03" ]; then mesfmt="MAR"; fi
  if [ \${mesanl} -eq "04" ]; then mesfmt="APR"; fi
  if [ \${mesanl} -eq "05" ]; then mesfmt="MAY"; fi
  if [ \${mesanl} -eq "06" ]; then mesfmt="JUN"; fi
  if [ \${mesanl} -eq "07" ]; then mesfmt="JUL"; fi
  if [ \${mesanl} -eq "08" ]; then mesfmt="AUG"; fi
  if [ \${mesanl} -eq "09" ]; then mesfmt="SEP"; fi
  if [ \${mesanl} -eq "10" ]; then mesfmt="OCT"; fi
  if [ \${mesanl} -eq "11" ]; then mesfmt="NOV"; fi
  if [ \${mesanl} -eq "12" ]; then mesfmt="DEC"; fi

  datafmt=\${hsnanl}Z\${diaanl}\${mesfmt}\${anoanl}

  if [ \${horasfctanl} -lt 0 ]; then horasfctanl=\$(echo \${horasfctanl} | awk -F "-" '{print \$2}'); fi

  if [ \${horasfctanl} -eq 0 ]; then horasfctanl=6; fi

  if [ \${var} == "t850" ]
  then  
    descvar="t850 1 99 Temperatura do Ar em 850hPa"
  elif [ \${var} == "z500" ]
  then  
    descvar="z500 1 99 Altura Geopotencial em 500hPa"
  else
    descvar="psnm 1 99 Pressao ao Nivel Medio do Mar"
  fi

cat << EOF > \${diretorio}/\${nomectl} 
DSET ^\${nome}.bin
TITLE Previsoes de \${dataanl} para \${datafct}
OPTIONS sequential
UNDEF 9.999E+20
XDEF 384 linear    0.000   0.9375000000  
YDEF 192 levels 
 -89.28423 -88.35700 -87.42430 -86.49037 -85.55596 -84.62133 -83.68657 -82.75173
 -81.81684 -80.88191 -79.94696 -79.01199 -78.07701 -77.14201 -76.20701 -75.27199
 -74.33697 -73.40195 -72.46692 -71.53189 -70.59685 -69.66182 -68.72678 -67.79173
 -66.85669 -65.92165 -64.98660 -64.05155 -63.11650 -62.18145 -61.24640 -60.31135
 -59.37630 -58.44124 -57.50619 -56.57114 -55.63608 -54.70103 -53.76597 -52.83091
 -51.89586 -50.96080 -50.02574 -49.09069 -48.15563 -47.22057 -46.28551 -45.35045
 -44.41540 -43.48034 -42.54528 -41.61022 -40.67516 -39.74010 -38.80504 -37.86998
 -36.93492 -35.99986 -35.06480 -34.12974 -33.19468 -32.25962 -31.32456 -30.38950
 -29.45444 -28.51938 -27.58431 -26.64925 -25.71419 -24.77913 -23.84407 -22.90901
 -21.97395 -21.03889 -20.10383 -19.16876 -18.23370 -17.29864 -16.36358 -15.42852
 -14.49346 -13.55839 -12.62333 -11.68827 -10.75321  -9.81815  -8.88309  -7.94802
  -7.01296  -6.07790  -5.14284  -4.20778  -3.27272  -2.33765  -1.40259  -0.46753
   0.46753   1.40259   2.33765   3.27272   4.20778   5.14284   6.07790   7.01296
   7.94802   8.88309   9.81815  10.75321  11.68827  12.62333  13.55839  14.49346
  15.42852  16.36358  17.29864  18.23370  19.16876  20.10383  21.03889  21.97395
  22.90901  23.84407  24.77913  25.71419  26.64925  27.58431  28.51938  29.45444
  30.38950  31.32456  32.25962  33.19468  34.12974  35.06480  35.99986  36.93492
  37.86998  38.80504  39.74010  40.67516  41.61022  42.54528  43.48034  44.41540
  45.35045  46.28551  47.22057  48.15563  49.09069  50.02574  50.96080  51.89586
  52.83091  53.76597  54.70103  55.63608  56.57114  57.50619  58.44124  59.37630
  60.31135  61.24640  62.18145  63.11650  64.05155  64.98660  65.92165  66.85669
  67.79173  68.72678  69.66182  70.59685  71.53189  72.46692  73.40195  74.33697
  75.27199  76.20701  77.14201  78.07701  79.01199  79.94696  80.88191  81.81684
  82.75173  83.68657  84.62133  85.55596  86.49037  87.42430  88.35700  89.28423
ZDEF 1 LINEAR 1000 1
TDEF 1 LINEAR \${datafmt} \${horasfctanl}hr
VARS 1
\${descvar}
ENDVARS
EOF

fi
}

# Procura todos os arquivos *.grb
for arq in \$(find \${EXECFILEPATH} -name *.grb)
do

  arqdirin=\$(dirname \${arq})
  arqdirout=/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/pos/dataout/rec

  arqin=\$(echo \${arq} | sed "s,.grb,,g")

  LABELI=\$(basename \${arqin} | cut -c 8-17)
  LABELF=\$(basename \${arqin} | cut -c 18-27)

  t_arqout=\${arqdirout}/t850/\${LABELI}/GBRM\${TPERT}\${LABELI}\${LABELF}.grads
  t_arqlog=\${arqdirout}/t850/\${LABELI}/GBRM\${TPERT}\${LABELI}\${LABELF}.log

  z_arqout=\${arqdirout}/z500/\${LABELI}/GBRM\${TPERT}\${LABELI}\${LABELF}.grads
  z_arqlog=\${arqdirout}/z500/\${LABELI}/GBRM\${TPERT}\${LABELI}\${LABELF}.log

  p_arqout=\${arqdirout}/psnm/\${LABELI}/GBRM\${TPERT}\${LABELI}\${LABELF}.grads
  p_arqlog=\${arqdirout}/psnm/\${LABELI}/GBRM\${TPERT}\${LABELI}\${LABELF}.log

  mkdir -p \${arqdirout}/t850/\${LABELI}
  mkdir -p \${arqdirout}/z500/\${LABELI}
  mkdir -p \${arqdirout}/psnm/\${LABELI}

  cd \${arqdirin}

  \${lats4d} -v -i \${arqin} -o \${t_arqout} -levs 850 -vars temp -format sequential > \${t_arqlog}
  cria_ctl \${t_arqout} t850
  \${lats4d} -v -i \${arqin} -o \${z_arqout} -levs 500 -vars zgeo -format sequential > \${z_arqlog}
  cria_ctl \${z_arqout} z500
  \${lats4d} -v -i \${arqin} -o \${p_arqout} -vars psnm -format sequential > \${p_arqlog}
  cria_ctl \${p_arqout} psnm


done

echo "" > \${arqdirout}/monitor.t
EOT0

# Submete o script e aguarda o fim da execução
chmod +x ${HOME_suite}/${SCRIPTSFILES}

#export PBS_SERVER=${pbs_server2}

qsub -W block=true ${SCRIPTSFILES}

echo "SUBMIT: ${HOME_suite}/${SCRIPTSFILES}"

if [ ${ANLTYPE} != CTR -a ${ANLTYPE} != NMC ]
then

  for mem in $(seq -f %02g 1 ${ANLPERT})
  do

    until [ -e "${DK2}/pos/dataout/rec/monitor.t" ]; do sleep 1s; done

  done

else

  until [ -e "${DK2}/pos/dataout/rec/monitor.t" ]; do sleep 1s; done

fi

exit 0
