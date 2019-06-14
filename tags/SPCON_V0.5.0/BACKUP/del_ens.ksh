#!/bin/ksh 
######################################################################
# Shell       -> Korn Shell
# Objetivo    -> Remocao dos arquivos do modelo Ensemble
# Sintaxe     -> del_ens.ksh [aaaa] [mm] [dd] [fita] 
# Autor:      -> Paulo Henrique de Araujo Ribeiro
# e-mail:     -> pribeiro@cptec.inpe.br
# criacao:    -> 26/12/2004
# Adaptado por Fabiano em 21/05/2005
#######################################################################

area1=/gfs/dk20/modoper/tempo/global/oens
area_bkp=/gfs/home3/modoper/tempo/global/oens/BACKUP
area_arq=/gfs/home3/modoper/tempo/global/oens/BACKUP
ano=$1
mes=$2
dia=$3
fita=$4
data=$ano$mes$dia
aux=0


# Geracao dos arquivos de apoio

 cd $area1

 \ls -1R probability/dataout/T126L28/prob$data*.*.* >> $area_bkp/arqens_apoio_$data
 \ls -1R probability/gif/prec$data*.png >> $area_bkp/arqens_apoio_$data

 n_prob=`cat $area_bkp/arqens_apoio_$data | grep probability | wc -l | awk '{print $1}'`

 \ls -1R spread/dataout/T126L28/spread$data* >> $area_bkp/arqens_apoio_$data

 n_spre=`cat $area_bkp/arqens_apoio_$data | grep spread | wc -l | awk '{print $1}'`

# \ls -1R gif/dataout/T126L28/geop???$data*200*.gif >> $area_bkp/arqens_apoio_gif_$data
# \ls -1R gif/dataout/T126L28/psnm$data*200*.gif >>del_ens.ksh $area_bkp/arqens_apoio_gif_$data
# \ls -1R gif/dataout/T126L28/temp???$data*200*.gif >> $area_bkp/arqens_apoio_gif_$data

 \ls -1R spread/gif/geop???$data*200*.png >> $area_bkp/arqens_apoio_gif_$data
 \ls -1R spread/gif/psnm$data*200*.png >> $area_bkp/arqens_apoio_gif_$data
 \ls -1R spread/gif/temp???$data*200*.png >> $area_bkp/arqens_apoio_gif_$data

 n_gif=`cat $area_bkp/arqens_apoio_gif_$data | grep png | wc -l | awk '{print $1}'`

 cat $area_bkp/arqens_apoio_gif_$data >> $area_bkp/arqens_apoio_$data

 # Rotina para adequar nome de arquivo para o subdiretorio SKILL



     if [ $mes -eq 01 ]; then

           MES="JAN"

    fi

    if [ $mes -eq 02 ]; then

          MES="FEB"

    fi

    if [ $mes -eq 03 ]; then

          MES="MAR"

    fi

    if [ $mes -eq 04 ]; then

          MES="APR"

    fi

    if [ $mes -eq 05 ]; then

          MES="MAY"

    fi

    if [ $mes -eq 06 ]; then

          MES="JUN"

    fi

    if [ $mes -eq 07 ]; then

          MES="JUL"

    fi

    if [ $mes -eq 08 ]; then

          MES="AUG"

    fi

    if [ $mes -eq 09 ]; then

          MES="SEP"

    fi

    if [ $mes -eq 10 ]; then

          MES="OCT"

    fi


    if  [ $mes -eq 11 ]; then

          MES="NOV"

    fi

    if  [ $mes -eq 12 ]; then

          MES="DEC"

    fi

 \ls -1R skill/*.??Z$dia$MES$ano.*.* >> $area_bkp/arqens_apoio_$data

  n_skill=`cat $area_bkp/arqens_apoio_$data | grep skill | wc -l | awk '{print $1}'`

# \ls -1R spaguetti/dataout/T126L28/*$data*200*.gif >> $area_bkp/arqens_apoio_$data
 \ls -1R spaguetti/gif/*$data*200*.png >> $area_bkp/arqens_apoio_$data

 n_spa=`cat $area_bkp/arqens_apoio_$data | grep spaguetti | wc -l | awk '{print $1}'`

 
#\ls -1R cluster/gif/*$data*200*.gif >> $area_bkp/arqens_apoio2_$data
 \ls -1R cluster/gif/*$data*200*.png >> $area_bkp/arqens_apoio2_$data

 \ls -1R cluster/dataout/T126L28/clusters/clusters$data*200*.T126* >> $area_bkp/arqens_apoio2_$data

 n_cluster=`cat $area_bkp/arqens_apoio2_$data | grep cluster | wc -l | awk '{print $1}'`

 \ls -1R chievol/gif/chi_evol$data*200*.png >> $area_bkp/arqens_apoio_$data

 n_chievol=`cat $area_bkp/arqens_apoio_$data| grep chievol | wc -l | awk '{print $1}'`

 \ls -1R prcmed/dataout/T126L28/GPRCENM$data*200* >> $area_bkp/arqens_apoio_$data
 \ls -1R prcmed/gif/prec*$data*200* >> $area_bkp/arqens_apoio_$data
 \ls -1R prcmed/gif/lprec*$data*200* >> $area_bkp/arqens_apoio_$data

 n_prcmed=`cat $area_bkp/arqens_apoio_$data | grep prcmed | wc -l | awk '{print $1}'`

 let tot_arq=$n_prob+$n_spre+$n_gif+$n_skill+$n_spa+$n_cluster+$n_chievol+$n_prcmed
 

   clear
  
   print " +-------------------------------------------------------+"
   print " |         REMOCAO DOS PRODUTOS ENSEMBLE \033[57G |"
   print " +-------------------------------------------------------+"
   print " | Periodo:   $ano $mes $dia \033[57G |"
   print " +-------------------------------------------------------+"
   print " "
   print " +-------------------------------------------------------+"
   print " | COMPARACAO DOS DADOS DA AREA COM OS DADOS DAS FITAS \033[57G | "
   print " +-------------------------------------------------------+"
     
   
     if [[ -s $area_bkp/arqens_apoio_$data ]] || [[ -s $area_bkp/arqens_apoio2_$data ]] || [[ -s $area_bkp/arqens_apoio_gif_$data ]]; then 

           cat $area_bkp/arqens_apoio_$data >> $area_bkp/lista_rmv
	   cat $area_bkp/arqens_apoio2_$data >> $area_bkp/lista_rmv
           cat $area_bkp/arqens_apoio_gif_$data >> $area_bkp/lista_rmv

      else
      
	  print " +-------------------------------------------------------+"
          print " | LISTAGENS DE APOIO NAO ENCONTRADAS. \033[57G | "
          print " | PROCESSO ABORTADO.\033[57G | "
          print " +-------------------------------------------------------+"
          print "\n"          
     
          exit 

      fi
      
      
      # Puxando via FTP o arquivo contendo a listagem da fita backup

      print " |  Pegando arquivo $fita na tupinamba ...\033[57G | "
      print " +-------------------------------------------------------+ "
      print "\n"
      
      #$area_arq/ftp_arqrmv.ksh $fita

       scp operacao@tupinamba:/extra02/operacao/fitas/DAT/$fita.Z $area_bkp     
      
      #	Comparando os dados da fita com os dados da area

      for arquivo in `cat $area_bkp/lista_rmv`; do

         encontrado=`zcat $area_bkp/$fita.Z | grep $arquivo | awk '{print $6}'`
          

              echo $encontrado >> $area_bkp/pode_remover.txt
              num_arqs=`wc -l $area_bkp/pode_remover.txt | awk '{print $1}'`               

         let aux=$aux+1
           
      done

      arq_lista=`wc -l $area_bkp/lista_rmv | awk '{print $1}'`
        
      #num_arqs=`wc -l $area_bkp/pode_remover.txt | awk '{print $1}'`
      
      if [ $num_arqs -ne $aux ]; then
      
         print " "
	 print " +-------------------------------------------------------+"
	 print " | NENHUM ARQUIVO PARA SER REMOVIDO \033[57G | "
	 print " +-------------------------------------------------------+"
	 print " "
	 
      fi

echo "num_arqs=" $num_arqs
echo "arq_lista" $arq_lista 
echo "tot_arq" $tot_arq
echo "aux" $aux

     if [[ $num_arqs -eq 3330 ]] || [[ $num_arqs -eq $tot_arq ]]; then

  
            print "\n"     
            print " +-------------------------------------------------------+"
            print " | FORAM BACKUPEADOS $num_arqs ARQUIVOS \033[57G | "
            print " +-------------------------------------------------------+"

       else

            print " \n"
            print " +-------------------------------------------------------+"
            print " | DADOS EM FITA NAO BATEM COM LISTA A SER REMOVIDA      |"
            print " | FAVOR VERIFICAR.                                      |" 
            print " +-------------------------------------------------------+"
            print "\n"
            rm -fr $area_bkp/lista_rmv $area_bkp/pode_remover.txt $area_bkp/ftp_arqrmv
            rm -fr $area_bkp/arqens_apoio*
            rm -fr $area_bkp/*.Z
            
            exit

      fi

            print " \n"
            print " +-------------------------------------------------------+"
            print " | DADOS EM FITA CONFERIDOS. DATA: $dia/$mes/$ano \033[57G |"
            print " | DESEJA REMOVE-LOS DA AREA (S/N)? \033[57G |"
            print " +-------------------------------------------------------+"
 
      read opc
     
      if [[ $opc = "N" ]] || [[ $opc = "n" ]] ; then
      
       
         print "\n"
         print " +-------------------------------------------------------+"
         print " | DADOS NAO REMOVIDOS \033[57G | "
         print " +-------------------------------------------------------+"
         print "\n"

         rm -fr $area_bkp/lista_rmv $area_bkp/pode_remover.txt $area_bkp/ftp_arqrmv
         rm -fr $area_bkp/arqens_apoio*
         rm -fr $area_bkp/arqens_apoio2_$data
         rm -fr $area_bkp/*.Z

         exit
	 
      else
      	 
         print "\n"
	 print " +-------------------------------------------------------+"
	 print " | REMOVENDO OS ARQUIVOS referentes a $dia/$mes/$ano \033[57G | "
	 print " +-------------------------------------------------------+"
	 print "\n"

         cd $area1
 	 
	 cat $area_bkp/lista_rmv | xargs rm -R 

	 if [ $? -eq 0 ]; then
	 
	    print "\n"
	    print " +-------------------------------------------------------+"
	    print " |   OK -  DADOS REMOVIDOS COM SUCESSOS \033[57G | "
	    print " +-------------------------------------------------------+"
	    print "\n"
	    
	 fi      
      fi

rm -fr $area_bkp/lista_rmv $area_bkp/pode_remover.txt $area_bkp/ftp_arqrmv
rm -fr $area_bkp/arqens_apoio*_$data
rm -fr $area_bkp/*.Z
exit   


