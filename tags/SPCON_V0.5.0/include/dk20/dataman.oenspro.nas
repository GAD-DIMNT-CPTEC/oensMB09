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
  Run: /gfs/home3/modoper/tempo/global/oens/includes/logs
#
#Flag N     M     Local/File/Remote
2     2000     1     /gfs/dk20/modoper/tempo/global/oens/model/dataout/T126L28
                  GFCT???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GFCT/<AAAA>/<MM>/<DD>
2     2000     3     /gfs/dk20/modoper/tempo/global/oens/model/dataout/T126L28
                  GPRG???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GPRG/<AAAA>/<MM>/<DD>
2     2000     1     /gfs/dk20/modoper/tempo/global/oens/model/dataout/T126L28
                  GFGH???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GFGH/<AAAA>/<MM>/<DD>
2     2000     1     /gfs/dk20/modoper/tempo/global/oens/pos/dataout/T126L28
                  GPOS???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GPOS/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/pos/dataout/T126L28
                  GPOS???<AAAA><MM><DD><HH>*.i*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/ANL/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/ensmed/dataout/T126L28
                  GPOS???<AAAA><MM><DD><HH>*.i*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/ANL/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/pos/dataout/T126L28
                  GFGN???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GRH/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/pos/dataout/T126L28
                  Identif???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GRH/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/pos/dataout/T126L28
                  Localiz???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GRH/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/pos/dataout/T126L28
                  Preffix???<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/GRH/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/ensmed/dataout/T126L28
                  GPOSENM<AAAA><MM><DD><HH>*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/ENSMED/<AAAA>/<MM>/<DD>
2     2000     2     /gfs/dk20/modoper/tempo/global/oens/plumes/dataout/T126L28
                  *<AAAA><MM><DD><HH>2*
                  /rede/nas/modoper/tempo/global/oens/avn/T126L28/PLUMES/<AAAA>/<MM>/<DD>
