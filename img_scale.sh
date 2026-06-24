#! /bin/bash

# Utilizar este script no diretório /pesq/share/das/dist/carlos.bastarz/oensMB09/aval/scantec

# @cfbastarz, 21/02/2024

for i in $(find . -name "SCORECARD*.png")
do

  j=$(echo $i | sed "s,.png,_800x800.png,g")

  echo "$i $j"
  convert $i -resize 640000@ $j

done

exit 0
