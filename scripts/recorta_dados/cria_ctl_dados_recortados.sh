#! /bin/bash

set -o xtrace

# Objetivo:
# Este script cria os arquivos descritores dos dados "recortados" para os campos abaixo 
# do EPS T126L28 MB09 (201503):
# - z500;
# - t850;
# - psnm.
#
# Este "recorte" e feito porque o script utilizado para calcular o CRPS
# precisa ler uma lista com os arquivos dos membros do EPS a serem abertos.
#
# Uso (exemplo):
# $ ./cria_ctl_dados_recortados.ksh 2015030100 2015033000
#
# Dependencias:
# - inctime ou caldate
# - o script recorta_dados.ksh precisa ser executado antes
#
# Historico:
# 17/08/2015 - First crack (cfbastarz)
# 07/10/2015 - Removidas variaveis psnm e z500
# 12/11/2015 - Corrigidos alguns bugs e substituido o comando \
#              find pelo ls (cfbastarz)
# 05/04/2016 - Comentarios e limpeza (cfbastarz)
# 10/09/2020 - Melhorias, conversão para o BASH e uso no XC50 (cfbastarz)

inctime=${HOME}/bin/inctime

if [ $# -ne "2" ]
then
  echo "Use: ./cria_ctl_dados_recortados.ksh YYYYMMDDHH YYYYMMDDHH"
  exit 1
fi

# Datas de inicio e fim
datai=${1}
dataf=${2}

# Define a variavel
var=t850

# Diretorios de leitura das saidas do modelo (conjunto) e escrita dos dados recortados
# Atencao para as variaveis e experimentos
#dataout=/scratchout/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/dados_recortados_oens_MB09_mcga-v4.0-novo_namelist-${var}
dataout=/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/pos/dataout/rec/${var}

data=${datai}

# Loop sobre as datas
while [ ${data} -le ${dataf} ]
do

  echo "${data}"

  cont=0

  # Lista todos os arquivos .bin (binarios, com os recortes de interesse)
  for arquivo in $(ls ${dataout}/${data}/*.grads.bin)
  do

    echo "${arquivo}"

    diretorio=$(dirname ${arquivo})
    nome=$(basename ${arquivo})

    # Define o nome do arquivo descritor
    nomectl=$(echo ${nome} | sed "s,.bin,.ctl,g")

    # Se o arquivo descritor ja nao existir, cria
    if [ ! -e "${diretorio}/${nomectl}" ]
    then

      dataanl=$(echo ${nome} | cut -c 8-17)
      datafct=$(echo ${nome} | cut -c 18-27)

      anoanl=$(echo ${dataanl} | cut -c 1-4)
      mesanl=$(echo ${dataanl} | cut -c 5-6)
      diaanl=$(echo ${dataanl} | cut -c 7-8)
      hsnanl=$(echo ${dataanl} | cut -c 9-10)

      anofct=$(echo ${datafct} | cut -c 1-4)
      mesfct=$(echo ${datafct} | cut -c 5-6)
      diafct=$(echo ${datafct} | cut -c 7-8)
      hsnfct=$(echo ${datafct} | cut -c 9-10)

      # Calculo do numero de horas entre dataanl e datafct:
      # (diafct - diaanl) * 24 + (hsnfct - hsnanl)
      difdia=$(expr ${diafct} - ${diaanl})
      diadiahoras=$(expr ${difdia} \* 24)

      difhora=$(expr ${hsnfct} - ${hsnanl})

      horasfctanl=$(expr ${diadiahoras} + ${difhora})

      # Com base no mes, define o nome
      if [ ${mesanl} -eq "01" ]; then mesfmt="JAN"; fi
      if [ ${mesanl} -eq "02" ]; then mesfmt="FEB"; fi
      if [ ${mesanl} -eq "03" ]; then mesfmt="MAR"; fi
      if [ ${mesanl} -eq "04" ]; then mesfmt="APR"; fi
      if [ ${mesanl} -eq "05" ]; then mesfmt="MAY"; fi
      if [ ${mesanl} -eq "06" ]; then mesfmt="JUN"; fi
      if [ ${mesanl} -eq "07" ]; then mesfmt="JUL"; fi
      if [ ${mesanl} -eq "08" ]; then mesfmt="AUG"; fi
      if [ ${mesanl} -eq "09" ]; then mesfmt="SEP"; fi
      if [ ${mesanl} -eq "10" ]; then mesfmt="OCT"; fi
      if [ ${mesanl} -eq "11" ]; then mesfmt="NOV"; fi
      if [ ${mesanl} -eq "12" ]; then mesfmt="DEC"; fi
    
      # Monta a data formatada (eg., 00Z10JAN2013)
      datafmt=${hsnanl}Z${diaanl}${mesfmt}${anoanl}
    
      # Ajusta o valor da quantidade de horas de previsao
      if [ ${horasfctanl} -lt 0 ]; then horasfctanl=$(echo ${horasfctanl} | awk -F "-" '{print $2}'); fi

      if [ ${horasfctanl} -eq 0 ]; then horasfctanl=6; fi
    
      echo "${datafct} - ${dataanl} = ${horasfctanl}"

      # De acordo com a variavel, define a descricao e numero de niveis (sempre 1)
      if [ ${var} == "t850" ]
      then  
        descvar="t850 1 99 Temperatura do Ar em 850hPa"
      else
        descvar="psnm 1 99 Pressao ao Nivel Medio do Mar"
      fi

# Monta o arquivo descritor (modificar conforme necessario)
cat << EOF > ${diretorio}/${nomectl} 
DSET ^${nome}
TITLE Previsoes de ${dataanl} para ${datafct} com remocao de vies
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
TDEF 1 LINEAR ${datafmt} ${horasfctanl}hr
VARS 1
${descvar}
ENDVARS
EOF

    # Atualiza o contador
    cont=$((${cont}+1))

    fi

  done

  echo "Total: ${cont}"

  # Atualiza a data
  data=$(${inctime} ${data} +12h %y4%m2%d2%h2)

  echo ""

done

exit 0
