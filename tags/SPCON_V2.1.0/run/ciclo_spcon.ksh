#!/bin/bash -x

# Script para realizar o SPCON-MB09 (sem correcao de vies, sem produtos)
# a partir de uma determinada analise.
#
# Versao inicial: Alex Almeida (Agosto, 2015)
# Modificacoes:   Carlos F. Bastarz (Agosto, 2015 - Comentarios)
#                                   (Setembro, 2015 - Pseudo-ciclo)
#                                   (Outubro, 2015 - Comandos atualizados (pre, model e pos)

# Todos os modulos de perturbacao (RAND e EOF) podem ser compilados com o PGI ne
# eslogin em que estiver, carregando o source /usr/bin/development_config.

inctime=${HOME}/bin/inctime

# Resolucao Ensemble:
RESOL=TQ0126L028
TRC=126
LV=28
PREFX=NMC

# Resolucao Analise:
ATRC=1534
ALEV=64

# Data inicial (data das analises):
LABELI=2015090100

# LABELF (data inicial + 15 dias):
# - LABELF e a data final de integracao partindo-se da primeira analise.
LABELF=`${inctime} ${LABELI} +15dy %y4%m2%d2%h2`

# Numero de processadores para integrar o MCGA-CPTEC/INPE:
NPROCF=72

# Numero de processadores para o pos-processaento do MCGA-CPTEC/INPE:
NPROCP=120

# DATAI e a data da primeira analise a ser realizaca;
# DATAF e a data da ultima analise a ser realizada.
DATAI=${LABELI}
DATAF=2015113118

DATA=${DATAI}

RUN=${SUBMIT_HOME}/oensMB09_mcga-v4.0/run

# Pseudo-ciclo:
while [ ${DATA} -le ${DATAF} ]
do

  # Variaveis perturbadas, tanto randomicamente quanto pela EOF: pslc, temp, umes, uvel e vvel;
  # Todas as perturbacoes sao feitas em ponto de grade;
  # Com excessao do MCGA, todos os outros processos sao realizados serialmente, nos nos auxiliares.
  
  # 1) Executa o pre-processamento do modelo:
  # Aqui apenas os processos CHOPPING SSTWEEKLYNCEP SSTWEEKLY e SNOWCLIMA sao realizados,
  # mas partindo-se do inicio (sem todos os outros campos auxiliares), deve-se realizar
  # todos os demais processos (apenas uma vez).
  # Obs.: Compilar o pre com o PGI na eslogin em que estiver, carregando o source /usr/bin/development_config.
  # Obs.: Para realizar todos os processos do Pre, utilizar o comando:
  # ./runPre ${RESOL} ${LABELI} CHOPPING SSTWEEKLYNCEP SSTWEEKLY SNOWCLIMA
  # ./runPre 126 28 2015091600 NMC 1 T F 1534 64 # Na primeira vez, rodar tudo (comando pre novo)
  ${RUN}/runPre ${TRC} ${LEV} ${LABELI} ${PREFX} 0 F F ${ATRC} ${ALEV}

  # 2) Executa o modelo (integra a analise inicial por 15 dias, com saida a 6 horas) - escreve na pasta NMC:
  # A principio, estas previsoes serao utilizadas apenas na analise de EOF, onde sao necessarias as diferencas
  # entre as previsoes controle e membro (em ponto de grade).
  # Obs.: Compilar o model com o PGI na eslogin em que estiver, carregando o source /usr/bin/development_config
  # No modelo novo, estas previsoes estao sendo escritas no scratchin (exec_NMC<data>)
  # Lembrar dos arquivos mwaves, gaussgiven e NMI
  ${RUN}/runModel ${NPROCF} 4 6 ENSFCTR ${TRC} ${LEV} ${LABELI} ${LABELF} ${LABELF} ${PREFX} sstwkl 2>/dev/null 1>&2 &

  # 3) Recompoe a analise para ponto de grade:
  ${RUN}/runrecanl.bash ${RESOL} NMC ANLNMC ${LABELI}                            

  # 4) Gera e soma as perturbacoes randomicas (YES=umes):
  ${RUN}/runrdpt.bash ${RESOL} NMC YES ${LABELI}                                

  # 5) Decompoem as analises perturbadas em coeficientes espectrais:
  ${RUN}/rundrpt.bash ${RESOL} NMC YES ${LABELI}                               

  # 6) Realiza o membro controle: 36 horas de previsao (na realidade sao 48h) com saida a cada 3 horas - escreve fora da pasta 
  # NMC (condicao inicial do membro controle e: GANLNMC2015040100S.unf.TQ0126L028)
  # Obs.: no modelo novo, estas previsoes esta sendo escritas no scratchout (exec_NMC)
  #  ./runctrmodgpro.bash ${NPROCF} 24 1 ${RESOL} NMC ${LABELI} CTR   
  ${RUN}/runctrmodgpro.bash ${NPROCF} 4 6 ${RESOL} NMC ${LABELI} CTR   

  # 7) Recompoem a analise do membro controle novamente para ponto de grade, sera utilizada na analise de EOF:
  # Obs.: este script vai apagar algumas previsoes do processo anterior (na realidade, vai deixar apenas os arquivos de previsao)
  ${RUN}/runrecfct.bash ${RESOL} CTR FCTCTR ${LABELI}           

  # 8) Calcula as EOFs, soma e subtrai as perturbacoes otimas e integra as analises "positivas"
  # (perturbacoes somadas) e "negativas" (subtraidas):
  for F in $(seq -w 01 07)
  do

      # 8.1) Realiza a analise controle e recompoem para ponto de grade as demais analises do conjunto;
      # Calcula as EOFs para as variaveis de interesse;
      # Soma e subtrai as perturbacoes otimas calculadas;
      # Decompoem em coeficientes espectrais os campos perturbadoes e prepara as analises finais.
      # Obs.: estas previsoes sao escritas no scratchout (exec_01R)
      # Estao sendo escritos as pastas pbs.5607672.eslogin13.x8z fora da instalacao do sistema
      ${RUN}/runperpntg2.bash ${RESOL} ${F}R YES ${LABELI} sstwkl  
   
      # 8.2) Realiza os membros com perturbacao subtraida - 15 dias, com saidas a cada 6 horas:
      # Obs.: estes arquivos estao sendo escritos no scratchin, e nao estao sendo colocados dentro das pastas $F[N,P], exec_$F[N,P}<data>
      ${RUN}/runModel ${NPROCF} 4 6 ENSF${F}N ${TRC} ${LEV} ${LABELI} ${LABELF} ${LABELF} ENSF${F}N sstwkl 2>/dev/null 1>&2 &

      # 8.3) Realiza os membros com perturbacao somada - 15 dias, com saidas a cada 6 horas:
      # Obs.: estes arquivos estao sendo escritos no scratchin, e nao estao sendo colocados dentro das pastas $F[N,P], exec_$F[N,P}<data>
      ${RUN}/runModel ${NPROCF} 4 6 ENSF${F}P ${TRC} ${LEV} ${LABELI} ${LABELF} ${LABELF} ENSF${F}P sstwkl 2>/dev/null 1>&2 &

  done

  wait

  # 10) Pos-processa os membros realizados a partir das perturbacoes com EOF:
  # Obs.: Compilar o pos com o PGI na eslogin01 (sem carregar o source /usr/bin/development_config)
  for F in $(seq -w 01 07)
  do

     # 10.1) Pos-processa os membros com perturbacao subtraida:
     sleep 2
     ${RUN}/runPos ${NPROCP} 12 1 ENSP${F}N ${TRC} ${LEV} ${LABELI} ${LABELF} ${F}N COLD 2>/dev/null 1>&2 &

     # 10.2) Pos-processa os membros com perturbacao somada:
     sleep 2
     ${RUN}/runPos ${NPROCP} 12 1 ENSP${F}P ${TRC} ${LEV} ${LABELI} ${LABELF} ${F}P COLD 2>/dev/null 1>&2 &

  done

  wait

  # Atualiza DATA, LABELI e LABELF:
  DATA=`${inctime} ${DATA} +6hr %y4%m2%d2%h2`

  LABELI=${DATA}
  LABELF=`${inctime} ${LABELI} +15dy %y4%m2%d2%h2`

done

exit 0 
