#!/bin/ksh


LABELI=$1

ftp -in tucupi.cptec.inpe.br << EOF
user pnt &arun/a*
cd /web1/ftp/pub/produtos/prod_eta/ensemble/GGHT126
dir RS/*${LABELI}2* grhrs.out
dir SC/*${LABELI}2* grhsc.out
dir PR/*${LABELI}2* grhpr.out
dir RJ/*${LABELI}2* grhrj.out
dir SP/*${LABELI}2* grhsp.out
dir ES/*${LABELI}2* grhes.out
dir MS/*${LABELI}2* grhms.out
dir DF/*${LABELI}2* grhdf.out
dir MG/*${LABELI}2* grhmg.out
dir GO/*${LABELI}2* grhgo.out
dir BA/*${LABELI}2* grhba.out
dir AL/*${LABELI}2* grhal.out
dir MT/*${LABELI}2* grhmt.out
dir SE/*${LABELI}2* grhse.out
dir RO/*${LABELI}2* grhro.out
dir AC/*${LABELI}2* grhac.out
dir PB/*${LABELI}2* grhpb.out
dir TO/*${LABELI}2* grhto.out
dir PI/*${LABELI}2* grhpi.out
dir RN/*${LABELI}2* grhrn.out
dir PE/*${LABELI}2* grhpe.out
dir CE/*${LABELI}2* grhce.out
dir PA/*${LABELI}2* grhpa.out
dir RR/*${LABELI}2* grhrr.out
dir AP/*${LABELI}2* grhap.out
dir MA/*${LABELI}2* grhma.out
dir WW/*${LABELI}2* grhww.out
dir AM/*${LABELI}2* grham.out
dir ZZ/*${LABELI}2* grhzz.out
quit
EOF

cat grhrs.out grhsc.out grhpr.out grhrj.out grhsp.out grhes.out grhms.out grhdf.out grhmg.out grhgo.out grhba.out grhal.out grhmt.out grhse.out grhro.out grhac.out grhpb.out grhto.out grhpi.out grhrn.out grhpe.out grhce.out grhpa.out grhrr.out grhap.out grhma.out grhww.out grham.out grhzz.out >> grhens.out
\rm -f grh??.out

set +x

echo "\n\nVerificando TUCUPI\n"

cont=0
for i in `cat grhens.out | awk '{print $5}'`
do
if [ $i -ge 130000 ] 
then
let cont=$cont+1
fi
done

if [ $cont -ge 545 ]
then
echo "Arquivos Gif Ok"
else
echo "Arquivos Gif com Problemas... Abortando"
exit 1
fi



exit 0
