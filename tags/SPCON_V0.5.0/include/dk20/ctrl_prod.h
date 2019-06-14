#DIRVER    => Diretorio onde estah o produto a ser verificado
#SOMATOTAL => Soma do tamanho de todos os arquivos
#QTDTOTAL  => Quantidade total de arquivos a ser verificado

sleep 90

soma=0
qtd=0

for iaaa in `ls -ltr ${DIRVER}/*${LABELI}2* | awk '{print $5}'`
do
      let soma=$soma+$iaaa
      let qtd=$qtd+1
done
if [ \( ${soma} -ge $SOMATOTAL \) -a \( $qtd -ge ${QTDTOTAL} \) ]
then
      echo "PRODUTO OK"
else
      echo "ERRO, ABORTANDO"
      exit 1
fi
