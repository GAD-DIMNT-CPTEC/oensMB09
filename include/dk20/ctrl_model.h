set -A ctrlsum 0 0 0 0 0 0 0 0 0 0

set +x
set +e
set +u

for arq in `ls ${ROPERM}/model/dataout/T126L28/G*${PERT}${LABELI}*`
do
echo $arq
#GFCT
ctrl1=`ls -ltr $arq | grep GFCT | grep fct | awk '{print $5}'` 
ctrl2=`ls -ltr $arq | grep GFCT | grep dir | awk '{print $5}'` 
ctrl3=`ls -ltr $arq | grep GFCT | grep icn | awk '{print $5}'` 
ctrl4=`ls -ltr $arq | grep GFCT | grep inz | awk '{print $5}'` 

#GPRG
ctrl5=`ls -ltr $arq | grep GPRG | grep fct | awk '{print $5}'` 
ctrl6=`ls -ltr $arq | grep GPRG | grep dir | awk '{print $5}'` 
ctrl7=`ls -ltr $arq | grep GPRG | grep icn | awk '{print $5}'` 
ctrl8=`ls -ltr $arq | grep GPRG | grep inz | awk '{print $5}'` 

#GFGH
ctrl9=`ls -ltr $arq  | grep GFGH | grep unf | awk '{print $5}'` 
ctrl10=`ls -ltr $arq | grep GFGH | grep dir | awk '{print $5}'` 

if [ \( -n $ctrl1 \) -a \( $ctrl1 -ge 11542576 \) ]
then
      let ctrlsum[0]=${ctrlsum[0]}+1
      echo "SOMANDO 1"
fi

if [ \( -n $ctrl2 \) -a \( $ctrl2 -ge 2012 \) ]
then
      let ctrlsum[1]=${ctrlsum[1]}+1
      echo "SOMANDO 2"
fi

if [ \( -n $ctrl3 \) -a \( $ctrl3 -ge 10067976 \) ]
then
      let ctrlsum[2]=${ctrlsum[2]}+1
      echo "SOMANDO 3"
fi

if [ \( -n $ctrl4 \) -a \( $ctrl4 -ge 10067976 \) ]
then
      let ctrlsum[3]=${ctrlsum[3]}+1
      echo "SOMANDO 4"
fi

if [ \( -n $ctrl5 \) -a \( $ctrl5 -ge 10067976 \) ]
then
      let ctrlsum[4]=${ctrlsum[4]}+1
      echo "SOMANDO 5"
fi

if [ \( -n $ctrl6 \) -a \( $ctrl6 -ge 1657 \) ]
then
      let ctrlsum[5]=${ctrlsum[5]}+1
      echo "SOMANDO 6"
fi

if [ \( -n $ctrl7 \) -a \( $ctrl7 -ge 10067976 \) ]
then
      let ctrlsum[6]=${ctrlsum[6]}+1
      echo "SOMANDO 7"
fi

if [ \( -n $ctrl8 \) -a \( $ctrl8 -ge 10067976 \) ]
then
      let ctrlsum[7]=${ctrlsum[7]}+1
      echo "SOMANDO 8"
fi

if [ \( -n $ctrl9 \) -a \( $ctrl9 -ge 47174400 \) ]
then
      let ctrlsum[8]=${ctrlsum[8]}+1
      echo "SOMANDO 9"
fi

if [ \( -n $ctrl10 \) -a \( $ctrl10 -ge 30753 \) ]
then
      let ctrlsum[9]=${ctrlsum[9]}+1
      echo "SOMANDO 10"
fi

done

echo "\n\n\n"
echo "GFCT${PERT}${LABELI}*fct* ${ctrlsum[0]}"
echo "GFCT${PERT}${LABELI}*dir* ${ctrlsum[1]}"
echo "GFCT${PERT}${LABELI}*icn* ${ctrlsum[2]}"
echo "GFCT${PERT}${LABELI}*inz* ${ctrlsum[3]}"
echo "GPRG${PERT}${LABELI}*fct* ${ctrlsum[4]}"
echo "GPRG${PERT}${LABELI}*dir* ${ctrlsum[5]}"
echo "GPRG${PERT}${LABELI}*icn* ${ctrlsum[6]}"
echo "GPRG${PERT}${LABELI}*inz* ${ctrlsum[7]}"
echo "GFGH${PERT}${LABELI}*unf* ${ctrlsum[8]}"
echo "GFGH${PERT}${LABELI}*dir* ${ctrlsum[9]}"
echo "\n"

if [ \( ${ctrlsum[0]} -ge 30 \) -a \( ${ctrlsum[1]} -ge 31 \) -a \( ${ctrlsum[2]} -ge 1 \) -a \( ${ctrlsum[3]} -ge 1 \) -a  \( ${ctrlsum[4]} -ge 0 \) -a \( ${ctrlsum[5]} -ge 0 \) -a  \( ${ctrlsum[6]} -ge 0 \) -a \( ${ctrlsum[7]} -ge 0 \) -a  \( ${ctrlsum[8]} -ge 1 \) -a \( ${ctrlsum[9]} -ge 1 \) ]
then
      echo "ARQUIVOS OK"
      smslabel Info "Saida Ok: ${LABELI}"
      echo "\n\n"
else
      echo "ARQUIVOS COM ERROS"
      smslabel Info "Saida c/Problemas: ${LABELI}"
      echo "\n\n"
      exit 12
fi


%include <ctrl_pos.h>
