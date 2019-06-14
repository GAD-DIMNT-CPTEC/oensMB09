#! /bin/ksh

. ./config.sx6

LABELI=$1
DIR=${ROPERM}/pos/dataout/T126L28/

cd $DIR

ls -l GPOSAVN${LABELI}* | awk '{print $9" "$5}' > SizeTupanIIAVN${LABELI}
ls -l GPOSAVN${LABELI}* | awk '{print $9" "$5}' > SizeTupanIIAVN${LABELI}
ls -l GPOS01N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII01N${LABELI}
ls -l GPOS01N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII01N${LABELI}
ls -l GPOS01P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII01P${LABELI}
ls -l GPOS01P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII01P${LABELI}
ls -l GPOS02N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII02N${LABELI}
ls -l GPOS02N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII02N${LABELI}
ls -l GPOS02P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII02P${LABELI}
ls -l GPOS02P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII02P${LABELI}
ls -l GPOS03N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII03N${LABELI}
ls -l GPOS03N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII03N${LABELI}
ls -l GPOS03P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII03P${LABELI}
ls -l GPOS03P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII03P${LABELI}
ls -l GPOS04N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII04N${LABELI}
ls -l GPOS04N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII04N${LABELI}
ls -l GPOS04P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII04P${LABELI}
ls -l GPOS04P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII04P${LABELI}
ls -l GPOS05N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII05N${LABELI}
ls -l GPOS05N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII05N${LABELI}
ls -l GPOS05P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII05P${LABELI}
ls -l GPOS05P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII05P${LABELI}
ls -l GPOS06N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII06N${LABELI}
ls -l GPOS06N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII06N${LABELI}
ls -l GPOS06P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII06P${LABELI}
ls -l GPOS06P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII06P${LABELI}
ls -l GPOS07N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII07N${LABELI}
ls -l GPOS07N${LABELI}* | awk '{print $9" "$5}' > SizeTupanII07N${LABELI}
ls -l GPOS07P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII07P${LABELI}
ls -l GPOS07P${LABELI}* | awk '{print $9" "$5}' > SizeTupanII07P${LABELI}

ls -ltr $DIR/SizeTupanII*${LABELI}

exit 0

