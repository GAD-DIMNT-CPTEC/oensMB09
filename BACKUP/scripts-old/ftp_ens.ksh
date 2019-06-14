#!/bin/ksh

###############################################################
# Script para transferencia dos arquivos do Modelo ENSEMBLE   #
# para a area /rede/oper/ensemble.                            #
#                                                             #
#  Backup da Operacao                                         #
#                                                             #
# Responsavel - Fabiano - fabiano@cptec.inpe.br               #
#                                                             #
###############################################################

print "\n"
print "+------------------------------------------------------------------+"
print "+ Transferindo arquivo ensemble_$data.tar para /rede/oper/ensemble      +"
print "+------------------------------------------------------------------+"
print "\n"

data=$1


cat << EOT > ftp_arqtar
user operacao cecapau7
bin
cd /rede/oper 
mkdir ensemble
cd ensemble
lcd /gfs/dk20/modoper/tempo/global/oenspro/produtos/BACKUP
put ensemble_$data.tar
EOT

ftp -inv capivary.cptec.inpe.br < ftp_arqtar



