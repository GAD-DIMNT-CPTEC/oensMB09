##################################################
#
# Objetivo: Lista de padroes de arquivos/diretorios 
#                 para backup e remocao
#
# Autor: EBMB e DSM, 20070110
#
#<--#
#
#     SINTAXE:
#           Email = endereco de mail para envio notificacoes, separar
#                       enderecos por virgulas
#
#           To    = listas de arquivos com parametros para envio de dados
#                   remotos (ftp ou scp), separadas por virgulas
#           
#           Run   = diretorio onde serao gravadas as saidas do script
#
#           FLAG  ACOES
#           0     - Nenhuma
#           1     - Copia arquivos para disco remoto
#                   Remove arquivos antigos (> N dias) do disco Local
#           2     - Copia arquivos para disco remoto
#                   Remove arquivos antigos (> N dias) do disco Local
#                   Remove arquivos antigos (> M dias) do disco remoto
#           3     - Remove arquivos antigos (> N dias) do disco Local 
#                   SEM copiar para disco remoto
#           1?    - Envia arquivos via FTP para host remoto definido
#                   na variavel TO
#                   , onde 11 => envia para a 1a. lista,
#                          12 => envia para a 2a. lista...
#                   PS: coloque "<date>" nos diretorios para representar a data 
#           2?    - Envia arquivos via SCP para host remoto definido
#                   na variavel TO
#                   , onde 21 => envia para a 1a. lista,
#                          22 => envia para a 2a. lista...
#                   PS: coloque "<date>" nos diretorios para representar a data 
#
#                 +---------------------------------------------+
#                 => ATENCAO: Utilize Flag = 0 OU 1 para bangu <=
#                 +---------------------------------------------+
#
#           N = numero de dias mantidos no diretorio local
#           M = numero de dias mantidos no diretorio remoto
#                       soh considerado se Flag = 2
#           U = valor INDEFINIDO
#
#           Local = diretorio local com arquivos originais
#           File  = padrao para o nome de arquivo
#           Remote= diretorio remoto para onde os arquivos serao copiados
#
#     PADROES:
#           1o. Local, NAO deve conter interrogacoes
#           2o. Remoto, se necessario colocar interrogacoes para gerar
#                 estrutura de diretorios corretamente
#
#                 Exemplo: Local: /gfs/home3/io_dop/tmp
#                          File : arqname????????.txt
#                             Arquivo para copiar: 
#                             /gfs/home3/io_dop/tmp/200701/arqname20070101.txt
#
#                          Remote: /rede/nas/io_dop/tmp/??????
#                             O arquivo sera copiado para:
#                             /rede/nas/io_dop/tmp/200701
#
#-->#
#
##################################################
Email: alexalm@cptec.inpe.br
   To: modpar.www, modoper.itatim
  Run: /gfs/home3/modoper/tempo/global/oens/include/logs
#
#Flag N     M     Local/File/Remote
#
# PLUMES
#
2     1     28    /gfs/dk22/modoper/tempo/global/oens/plumes/dataout/T126L28/
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/PLUMES/<AAAA>/<MM>/<DD>
#
# SPREAD
#
2     1     28    /gfs/dk22/modoper/tempo/global/oens/spread/dataout/T126L28/
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/spread/<AAAA>/<MM>/<DD>
#
# CLUSTER
#
2     1     28     /gfs/dk22/modoper/tempo/global/oens/cluster/dataout/T126L28/clusters
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/cluster/<AAAA>/<MM>/<DD>/clusters
2     1     28    /gfs/dk22/modoper/tempo/global/oens/cluster/gif
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/cluster/<AAAA>/<MM>/<DD>/gif
#
# GIF
#
2     1     28    /gfs/dk22/modoper/tempo/global/oens/spread/gif
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/gif/<AAAA>/<MM>/<DD>
#
# PROBABILITY
#
2     1     28    /gfs/dk22/modoper/tempo/global/oens/probability/dataout/T126L28
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/probability/binctl/<AAAA>/<MM>/<DD>
2     1     28    /gfs/dk22/modoper/tempo/global/oens/probability/gif
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/probability/gif/<AAAA>/<MM>/<DD>
2     1     28    /gfs/dk22/modoper/tempo/global/oens/probagr/dataout/T126L28
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/probability/gif/<AAAA>/<MM>/<DD>
#
# SKILL
#
2     1     28    /gfs/dk22/modoper/tempo/global/oens/skill
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/skill/<AAAA>/<MM>/<DD>
2     1     970   /gfs/dk22/modoper/tempo/global/oens/skill
                  *<HH>Z<DD>???<AAAA>.ens.m02
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/skill/<AAAA>/<MM>/<DD>
#
# SPAGUETTI
#
2     1     28    /gfs/dk22/modoper/tempo/global/oens/spaguetti/gif
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/spaguetti/<AAAA>/<MM>/<DD>
#
# PRCMED
#
2     2     28    /gfs/dk22/modoper/tempo/global/oens/prcmed/dataout/T126L28
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/prcmed/<AAAA>/<MM>/<DD>
2     2     28    /gfs/home3/modoper/tempo/global/oens/prcmed/scripts
                  clim_prec_inmet<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/prcmed/<AAAA>/<MM>/<DD>
2     2     28    /gfs/home3/modoper/tempo/global/oens/prcmed/scripts
                  clim_temp_inmet<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/prcmed/<AAAA>/<MM>/<DD>
2     2     28    /gfs/home3/modoper/tempo/global/oens/prcmed/scripts
                  cprec<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/prcmed/<AAAA>/<MM>/<DD>
##
# REMOVIES
#
1     1     U     /rede/nas/modoper/tempo/global/oens/nmc/T126L28/removies/ensmed/<AAAA>/<MM>/<DD>
                  gposenmrmv<AAAA><MM><DD><HH>2*
                  /rede/hsm/oens/avn/T126L28/removies/ensmed/<AAAA>/<MM>/<DD>
1     1     U     /rede/nas/modoper/tempo/global/oens/nmc/T126L28/removies/vies/<AAAA>/<MM>/<DD>
                  BiasFile*h<AAAA><MM><DD><HH>*
                  /rede/hsm/oens/avn/T126L28/removies/vies/<AAAA>/<MM>/<DD>
1     1     U     /gfs/dk22/modoper/tempo/global/oens/removies/ensmed/<AAAA>/<MM>/<DD>
                  gposenmrmv<AAAA><MM><DD><HH>2*
                  /rede/hsm/oens/avn/T126L28/removies/ensmed/<AAAA>/<MM>/<DD>
#
# PROBAGR
#
2     1     28    /gfs/dk22/modoper/tempo/global/oens/probagr/dataout/T126L28
                  wmaprecprob<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/probability/binctl/<AAAA>/<MM>/<DD>
#
# GESF
#
2     2     17    /gfs/dk22/modoper/tempo/global/oens/produtos/recortes/dataout/t126l28
                  GSKL???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/produtos/skill/<AAAA><MM><DD><HH>
2     1     2     /gfs/dk22/modoper/tempo/global/oens/produtos/recortes/dataout/t126l28
                  GESF???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/RECPRD/GESFAM/<AAAA>/<MM>/<DD>
2     1     2     /gfs/dk22/modoper/tempo/global/oens/produtos/recortes/dataout/t126l28
                  PRECSC<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/RECPRD/GHPREC/<AAAA>/<MM>/<DD>
                 
