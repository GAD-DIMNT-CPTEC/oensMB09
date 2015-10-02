#! /bin/ksh 
#
#set -o xtrace
#
# Este script extrai as seguintes variaveis do EPS T126L28 MB09 (201503):
# - z500;
# - t850;
# - psnm.
# Este "recorte" e feito porque o script utilizado para calcular o CRPS
# precisa ler uma lista com os arquivos dos membros do EPS a serem abertos.
#
# Uso (exemplo):
# $ ./recorta_dados.ksh 2015030100 2015033000
#
# Historico:
# 13/08/2015 - First crack (cfbastarz)

inctime=${HOME}/bin/inctime

if [ $# -ne "2" ]
then
  print "Use: ./recorta_dados.ksh YYYYMMDDHH YYYYMMDDHH"
  exit 1
fi

datai=${1}
dataf=${2}

#datain=/stornext/online1/ensemble_g/oens_new/oensMB09
datain=/stornext/online17/pnt/preoper/tempo/oensMB09
dataout=/scratchin/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/dados_recortados
dataout1=/scratchout/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/dados_recortados

data=${datai}

set -A Membros 01N 01P 02N 02P 03N 03P 04N 04P 05N 05P 06N 06P 07N 07P NMC

while [ ${data} -le ${dataf} ]
do

  print ""
  print "${data}"

  YYYYMM=`echo ${data} | cut -c 1-6`
  DDHH=`echo ${data} | cut -c 7-10`

  datafct=`${inctime} ${data} +6hr %y4%m2%d2%h2`

  data_dataout=${dataout}/${data}
  dataout2=${dataout1}/${data}

  mkdir -p ${dataout2}

#  if [ ! -d ${data_dataout} ]; then mkdir -p ${data_dataout}; fi

  cd ${data_dataout}

  for membro in ${Membros[@]}
  do

    dataa=`${inctime} ${data} +6hr %y4%m2%d2%h2`
    dataff=`${inctime} ${dataf} +348hr %y4%m2%d2%h2`

    lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/${membro}/GPOS${membro}${data}${data}P.icn.TQ0126L028 -o ${dataout2}/GBRM${membro}${data}${data}-psnm -levs 1000 -vars psnm -format sequential
    lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/${membro}/GPOS${membro}${data}${data}P.icn.TQ0126L028 -o ${dataout2}/GBRM${membro}${data}${data}-z500 -levs 500 -vars zgeo -format sequential
    lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/${membro}/GPOS${membro}${data}${data}P.icn.TQ0126L028 -o ${dataout2}/GBRM${membro}${data}${data}-t850 -levs 850 -vars temp -format sequential
    cat ${dataout2}/GBRM${membro}${data}${data}-psnm.bin ${dataout2}/GBRM${membro}${data}${data}-z500.bin ${dataout2}/GBRM${membro}${data}${data}-t850.bin  > ${dataout2}/GBRM${membro}${data}${data}.grads

    while [ ${dataa} -le ${dataff} ]
    do

      lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/${membro}/GPOS${membro}${data}${dataa}P.fct.TQ0126L028 -o ${dataout2}/GBRM${membro}${data}${dataa}-psnm -levs 1000 -vars psnm -format sequential
      lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/${membro}/GPOS${membro}${data}${dataa}P.fct.TQ0126L028 -o ${dataout2}/GBRM${membro}${data}${dataa}-z500 -levs 500 -vars zgeo -format sequential
      lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/${membro}/GPOS${membro}${data}${dataa}P.fct.TQ0126L028 -o ${dataout2}/GBRM${membro}${data}${dataa}-t850 -levs 850 -vars temp -format sequential
      cat ${dataout2}/GBRM${membro}${data}${dataa}-psnm.bin ${dataout2}/GBRM${membro}${data}${dataa}-z500.bin ${dataout2}/GBRM${membro}${data}${dataa}-t850.bin  > ${dataout2}/GBRM${membro}${data}${dataa}.grads

      dataa=`${inctime} ${dataa} +6hr %y4%m2%d2%h2`

    done

    print ""

  done

  cd ${dataout}

  data=`${inctime} ${data} +6hr %y4%m2%d2%h2`

done

exit 0
