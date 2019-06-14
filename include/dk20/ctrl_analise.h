cont=0
for anl in `ls -ltr ${ROPERM}/model/datain/GANL${PERT}[NP]${LABELI}S.unf.* | awk '{print $5}'`
do

      if [ $anl -ge 7413924 ]
      then
           let cont=$cont+1
      fi
done

if [ $cont -eq 2 ]
then
      smslabel Info "Analises Criadas: ${LABELI}"
else
      smslabel Info "Analises c/Problemas: ${LABELI}"
      exit 1
fi
