#! /bin/ksh 

#set -o xtrace

# Objetivo:
# Este script extrai as seguintes variaveis do EPS T126L28 MB09/MCGA (201503):
# - z500;
# - t850;
# - psnm.
# Este "recorte" e feito porque o script utilizado para calcular o CRPS
# precisa ler uma lista com os arquivos dos membros do EPS a serem abertos.
#
# Uso (exemplo):
# $ ./recorta_dados.ksh 2015030100 2015033000
#
# Dependencias:
# - inctime ou caldate
#
# Historico:
# 13/08/2015 - First crack (cfbastarz)
# 06/10/2015 - Modificacoes para leituras dos dados no online1 (cfbastarz);
#              alterada a forma como o lats4d e executado (cfbastarz);
#              apenas T850 e extraida e apenas as previsoes de 00 e 12 sao lidas (cfbastarz).
# 05/04/2016 - Comentarios adicionais e limpeza (cfbastarz)

inctime=${HOME}/bin/inctime

if [ $# -ne "2" ]
then
  print "Use: ./recorta_dados.ksh YYYYMMDDHH YYYYMMDDHH"
  exit 1
fi

# Datas de inicio e fim
datai=${1}
dataf=${2}

# Define a variavel
var=t850

# Diretorios de leitura (das saida do modelo - conjunto) e escrita (dos dados recortados)
datain=/stornext/online1/ensemble_g/carlos/spcon_mb09_tq126l28_mcgav4.0_namelist_novo/backup_previsoes_pos_eof_oensMB09_mcga-v4.0
dataout=/scratchout/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/dados_recortados_oens_MB09_mcga-v4.0-novo_namelist-${var}


data=${datai}

# Array com os nomes dos membros do conjunto (ajustar conforme necessario)
set -A Membros 01N 01P 02N 02P 03N 03P 04N 04P 05N 05P 06N 06P 07N 07P NMC
#set -A Membros 01n 01p 02n 02p 03n 03p 04n 04p 05n 05p 06n 06p 07n 07p ctrl

# Loop sobre as datas
while [ ${data} -le ${dataf} ]
do

  print ""
  print "${data}"

  YYYYMM=`echo ${data} | cut -c 1-6`
  DDHH=`echo ${data} | cut -c 7-10`

  datafct=`${inctime} ${data} +6hr %y4%m2%d2%h2`

  data_dataout=${dataout}/${data}

  mkdir -p ${dataout}

  if [ ! -d ${data_dataout} ]; then mkdir -p ${data_dataout}; fi

  cd ${data_dataout}

    # Loop sobre os membros
    for membro in ${Membros[@]}
    do

      # Define os prefixos e troca maisculas por minusculas, caso necessario
      if [ ${membro} = "ctrl" ]
      then
        membrof="NMC"
      else
        membrof=`echo ${membro} | tr "n p" "N P"`
      fi

#      membrof=${membro}

      # Calcula as datas da analise e previsao
      dataa=`${inctime} ${data} +6hr %y4%m2%d2%h2`
      dataff=`${inctime} ${dataf} +360hr %y4%m2%d2%h2`

      # Verifica se o arquivo recortado ja nao existe
      if [ ! -e ${data_dataout}/GBRM${membrof}${data}${data}.grads.bin ]
      then

        # Recorta as analises
        if [ ${var} == "t850" ]
        then
          nohup lats4d.sh -v -i ${datain}/${YYYYMM}${DDHH}/${membro}/GPOS${membrof}${data}${data}P.icn.TQ0126L028 -o ${data_dataout}/GBRM${membrof}${data}${data}.grads -levs 850 -vars temp -format sequential > rec_icn_t850_${membro}${data}.log &
        else
          nohup lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/pos/${membro}/GPOS${membrof}${data}${data}P.icn.TQ0126L028 -o ${data_dataout}/GBRM${membrof}${data}${data}.grads -vars psnm -format sequential > rec_icn_psnm_${membro}${data}.log &
        fi

      fi

      # Loop sobre os arquivos de previsao
      while [ ${dataa} -le ${dataff} ]
      do
 
        # Verifica se o arquivo recortado ja nao existe 
        if [ ! -e ${data_dataout}/GBRM${membrof}${data}${dataa}.grads.bin ]
        then

          # Recorta as previsoes
          if [ ${var} == "t850" ]
          then
            nohup lats4d.sh -v -i ${datain}/${YYYYMM}${DDHH}/${membro}/GPOS${membrof}${data}${dataa}P.fct.TQ0126L028 -o ${data_dataout}/GBRM${membrof}${data}${dataa}.grads -levs 850 -vars temp -format sequential > rec_fct_t850_${membro}${data}.log &
          else
            nohup lats4d.sh -v -i ${datain}/${YYYYMM}/${DDHH}/${membro}/GPOS${membro}${data}${dataa}P.fct.TQ0126L028 -o ${data_dataout}/GBRM${membro}${data}${dataa}.grads -vars psnm -format sequential > rec_fct_psnm_${membro}${data}.log &
          fi

        fi

        # Incrementa a data do loop das previsoes
        dataa=`${inctime} ${dataa} +6hr %y4%m2%d2%h2`
  
      done
 
  done

  cd ${dataout}

  # Incrementa a data do loop principal
  data=`${inctime} ${data} +12hr %y4%m2%d2%h2`

done

exit 0
