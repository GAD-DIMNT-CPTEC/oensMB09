#!/bin/ksh
#help#
#*****************************************************************#
#                                                                 #
#       Name:         mudaSetVar.ksh                              #
#                                                                 #
#       Function:     Este script muda as variaveis               #
#                     dos scripts operacionais do                 #
#                     ensemble.                                   #
#                     It runs in Korn Shell.                      #
#                                                                 #
#       Date:         October 19th, 2004.                         #
#       Last change:  October 19th, 2004.                         # 
#       Autor: Adma Raia (adma@cptec.inpe.br)                     #
#                                                                 #
#                                                                 #
#*****************************************************************#
#help#

#*****************************************************************#
#Definicao dos diretorios/parametros que serao trocados           #
#*****************************************************************#

#diretorio onde sera realizada a troca dos scripts
dir_in=/gfs/home3/modoper/tempo/global/oens/run

#Estrutura dos diretorios de entrada e saida


#home old
home_inio="home3/io_dop/tempo/global"

#home new
home_inin="home3/modoper/tempo/global"

#out old
douto="dk21/io_dop/tempo/global"

#out new
doutn="dk20/modoper/tempo/global"

#Diretorios do modelo global

#home global old
home_glbo="home0/global"  

#home global new
home_glbn="home0/global"  

#out global old
dout_glbo="dk02/global"

#out global new
dout_glbn="dk02/global"

#Parametros

#old
supero=sx6
#new
supern=sx6

#old
filao=Inter

#new
filan=PNT-EN

#old
maquinao=turi
#new
maquinan=turi

#old
dscro="gfs/home3/io_dop/tempo/global/oens/produtos/scripts"
#new
dscrn="gfs/home3/modoper/tempo/global/oens/produtos/scripts"


#*****************************************************************#
#  Realizacao das trocas                                          #
#*****************************************************************#

cd $dir_in

ls run* > lista

for script in `cat lista`
do
echo ${script}
cat ${script} | sed "s&${home_inio}/&${home_inin}/&g" | sed "s&${douto}/&${doutn}/&g" | sed "s&${home_glbo}/&${home_glbn}/&g" | sed "s&${dout_glbo}/&${dout_glbn}/&g" | sed "s&${supero}/&${supern}/&g"| sed "s&${dscro}/&${dscrn}/&g"| sed "s/${filao}/${filan}/g" | sed "s/${maquinao}/${maquinan}/g"> ${dir_in}/tmp/${script}

done

\rm lista

exit
