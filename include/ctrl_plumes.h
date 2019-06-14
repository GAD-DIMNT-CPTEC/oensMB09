soma=0

for iaaa in `ls -ltr ${ROPERM}/plumes/dataout/T126L28/*${LABELI}2* | awk '{print $5}'`
do
      let soma=$soma+$iaaa
done
if [ ${soma} -ge 649757347 ]
then
      echo "OK"
else
      echo "ERRO, ABORTANDO"
      exit 1
fi
