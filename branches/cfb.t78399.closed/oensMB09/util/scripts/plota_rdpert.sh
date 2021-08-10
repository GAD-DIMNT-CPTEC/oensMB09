#! /bin/bash

#rm *txt > /dev/null
#
#Mems=(01R 02R 03R 04R 05R 06R 07R)
#
#Vars=("t:1" "u:2" "v:3" "q:4")
#
#for mem in ${Mems[@]}
#do
#
#  cat GANL${mem}2014110312R.unf.TQ0213L042.out | sed -n 2p | sed "s,#,,g" >> varp_mem${mem}.txt
#
#  for lev in $(seq 1 42)
#  do
#
#    if [ ${lev} -lt "10" ]
#    then
#
#      cat GANL${mem}2014110312R.unf.TQ0213L042.out | grep ^"    ${lev}" > mem${mem}_lev${lev}.tmp
#
#      for varlin in "${Vars[@]}"
#      do
#
#        var="${varlin%%:*}"
#        lin="${varlin##*:}"
#
#        echo "${mem} ${lev} ${var} ${lin}"
#
#        cat mem${mem}_lev${lev}.tmp | head -${lin} | tail -1 > var${var}_mem${mem}_lev${lev}.tmp
#
#        cat var${var}_mem${mem}_lev${lev}.tmp >> var${var}_mem${mem}.txt
#
#      done
#
#    else
#
#      cat GANL${mem}2014110312R.unf.TQ0213L042.out | grep ^"   ${lev}" > mem${mem}_lev${lev}.tmp
#
#      for varlin in "${Vars[@]}"
#      do
#
#        var="${varlin%%:*}"
#        lin="${varlin##*:}"
#
#        echo "${mem} ${lev} ${var} ${lin}"
#
#        cat mem${mem}_lev${lev}.tmp | head -${lin} | tail -1 > var${var}_mem${mem}_lev${lev}.tmp
#
#        cat var${var}_mem${mem}_lev${lev}.tmp >> var${var}_mem${mem}.txt
#
#      done
#
#    fi
#
#  done
#
#  echo
#
#done
#
#rm *tmp > /dev/null
#
cat << EOF > plot_min.gpl
#! /stornext/home/carlos.bastarz/.gnuplot-5.0.0/bin/gnuplot

set terminal png font Arial 10 size 1024,768
set output 'perfis_min.png'

set multiplot layout 1, 4 

set ytics 1,1,42
set xtics auto rotate by 45 right

unset key

set title "T"
plot "vart_mem01R.txt" using 2:1 with lines lw 2 title "01R", \
     "vart_mem02R.txt" using 2:1 with lines lw 2 title "02R", \
     "vart_mem03R.txt" using 2:1 with lines lw 2 title "03R", \
     "vart_mem04R.txt" using 2:1 with lines lw 2 title "04R", \
     "vart_mem05R.txt" using 2:1 with lines lw 2 title "05R", \
     "vart_mem06R.txt" using 2:1 with lines lw 2 title "06R", \
     "vart_mem07R.txt" using 2:1 with lines lw 2 title "07R"
     
set title "U"
plot "varu_mem01R.txt" using 2:1 with lines lw 2 title "01R", \
     "varu_mem02R.txt" using 2:1 with lines lw 2 title "02R", \
     "varu_mem03R.txt" using 2:1 with lines lw 2 title "03R", \
     "varu_mem04R.txt" using 2:1 with lines lw 2 title "04R", \
     "varu_mem05R.txt" using 2:1 with lines lw 2 title "05R", \
     "varu_mem06R.txt" using 2:1 with lines lw 2 title "06R", \
     "varu_mem07R.txt" using 2:1 with lines lw 2 title "07R"
     
set title "V"
plot "varv_mem01R.txt" using 2:1 with lines lw 2 title "01R", \
     "varv_mem02R.txt" using 2:1 with lines lw 2 title "02R", \
     "varv_mem03R.txt" using 2:1 with lines lw 2 title "03R", \
     "varv_mem04R.txt" using 2:1 with lines lw 2 title "04R", \
     "varv_mem05R.txt" using 2:1 with lines lw 2 title "05R", \
     "varv_mem06R.txt" using 2:1 with lines lw 2 title "06R", \
     "varv_mem07R.txt" using 2:1 with lines lw 2 title "07R"

set key

set title "Q"
plot "varq_mem01R.txt" using 2:1 with lines lw 2 title "01R", \
     "varq_mem02R.txt" using 2:1 with lines lw 2 title "02R", \
     "varq_mem03R.txt" using 2:1 with lines lw 2 title "03R", \
     "varq_mem04R.txt" using 2:1 with lines lw 2 title "04R", \
     "varq_mem05R.txt" using 2:1 with lines lw 2 title "05R", \
     "varq_mem06R.txt" using 2:1 with lines lw 2 title "06R", \
     "varq_mem07R.txt" using 2:1 with lines lw 2 title "07R"

unset multiplot
EOF

chmod +x plot_min.gpl
./plot_min.gpl

cat << EOF > plot_max.gpl
#! /stornext/home/carlos.bastarz/.gnuplot-5.0.0/bin/gnuplot

set terminal png font Arial 10 size 1024,768
set output 'perfis_max.png'

set multiplot layout 1, 4 

set ytics 1,1,42
set xtics auto rotate by 45 right

unset key

set title "T"
plot "vart_mem01R.txt" using 3:1 with lines lw 2 title "01R", \
     "vart_mem02R.txt" using 3:1 with lines lw 2 title "02R", \
     "vart_mem03R.txt" using 3:1 with lines lw 2 title "03R", \
     "vart_mem04R.txt" using 3:1 with lines lw 2 title "04R", \
     "vart_mem05R.txt" using 3:1 with lines lw 2 title "05R", \
     "vart_mem06R.txt" using 3:1 with lines lw 2 title "06R", \
     "vart_mem07R.txt" using 3:1 with lines lw 2 title "07R"
     
set title "U"
plot "varu_mem01R.txt" using 3:1 with lines lw 2 title "01R", \
     "varu_mem02R.txt" using 3:1 with lines lw 2 title "02R", \
     "varu_mem03R.txt" using 3:1 with lines lw 2 title "03R", \
     "varu_mem04R.txt" using 3:1 with lines lw 2 title "04R", \
     "varu_mem05R.txt" using 3:1 with lines lw 2 title "05R", \
     "varu_mem06R.txt" using 3:1 with lines lw 2 title "06R", \
     "varu_mem07R.txt" using 3:1 with lines lw 2 title "07R"
     
set title "V"
plot "varv_mem01R.txt" using 3:1 with lines lw 2 title "01R", \
     "varv_mem02R.txt" using 3:1 with lines lw 2 title "02R", \
     "varv_mem03R.txt" using 3:1 with lines lw 2 title "03R", \
     "varv_mem04R.txt" using 3:1 with lines lw 2 title "04R", \
     "varv_mem05R.txt" using 3:1 with lines lw 2 title "05R", \
     "varv_mem06R.txt" using 3:1 with lines lw 2 title "06R", \
     "varv_mem07R.txt" using 3:1 with lines lw 2 title "07R"

set key

set title "Q"
plot "varq_mem01R.txt" using 3:1 with lines lw 2 title "01R", \
     "varq_mem02R.txt" using 3:1 with lines lw 2 title "02R", \
     "varq_mem03R.txt" using 3:1 with lines lw 2 title "03R", \
     "varq_mem04R.txt" using 3:1 with lines lw 2 title "04R", \
     "varq_mem05R.txt" using 3:1 with lines lw 2 title "05R", \
     "varq_mem06R.txt" using 3:1 with lines lw 2 title "06R", \
     "varq_mem07R.txt" using 3:1 with lines lw 2 title "07R"

unset multiplot
EOF

chmod +x plot_max.gpl
./plot_max.gpl

cat << EOF > plot_mean.gpl
#! /stornext/home/carlos.bastarz/.gnuplot-5.0.0/bin/gnuplot

set terminal png font Arial 10 size 1024,768
set output 'perfis_mean.png'

set multiplot layout 1, 4 

set ytics 1,1,42
set xtics auto rotate by 45 right

unset key

set title "T"
plot "vart_mem01R.txt" using 4:1 with lines lw 2 title "01R", \
     "vart_mem02R.txt" using 4:1 with lines lw 2 title "02R", \
     "vart_mem03R.txt" using 4:1 with lines lw 2 title "03R", \
     "vart_mem04R.txt" using 4:1 with lines lw 2 title "04R", \
     "vart_mem05R.txt" using 4:1 with lines lw 2 title "05R", \
     "vart_mem06R.txt" using 4:1 with lines lw 2 title "06R", \
     "vart_mem07R.txt" using 4:1 with lines lw 2 title "07R"
     
set title "U"
plot "varu_mem01R.txt" using 4:1 with lines lw 2 title "01R", \
     "varu_mem02R.txt" using 4:1 with lines lw 2 title "02R", \
     "varu_mem03R.txt" using 4:1 with lines lw 2 title "03R", \
     "varu_mem04R.txt" using 4:1 with lines lw 2 title "04R", \
     "varu_mem05R.txt" using 4:1 with lines lw 2 title "05R", \
     "varu_mem06R.txt" using 4:1 with lines lw 2 title "06R", \
     "varu_mem07R.txt" using 4:1 with lines lw 2 title "07R"
     
set title "V"
plot "varv_mem01R.txt" using 4:1 with lines lw 2 title "01R", \
     "varv_mem02R.txt" using 4:1 with lines lw 2 title "02R", \
     "varv_mem03R.txt" using 4:1 with lines lw 2 title "03R", \
     "varv_mem04R.txt" using 4:1 with lines lw 2 title "04R", \
     "varv_mem05R.txt" using 4:1 with lines lw 2 title "05R", \
     "varv_mem06R.txt" using 4:1 with lines lw 2 title "06R", \
     "varv_mem07R.txt" using 4:1 with lines lw 2 title "07R"

set key

set title "Q"
plot "varq_mem01R.txt" using 4:1 with lines lw 2 title "01R", \
     "varq_mem02R.txt" using 4:1 with lines lw 2 title "02R", \
     "varq_mem03R.txt" using 4:1 with lines lw 2 title "03R", \
     "varq_mem04R.txt" using 4:1 with lines lw 2 title "04R", \
     "varq_mem05R.txt" using 4:1 with lines lw 2 title "05R", \
     "varq_mem06R.txt" using 4:1 with lines lw 2 title "06R", \
     "varq_mem07R.txt" using 4:1 with lines lw 2 title "07R"

unset multiplot
EOF

chmod +x plot_mean.gpl
./plot_mean.gpl

cat << EOF > plot_stdev.gpl
#! /stornext/home/carlos.bastarz/.gnuplot-5.0.0/bin/gnuplot

set terminal png font Arial 10 size 1024,768
set output 'perfis_stdev.png'

set multiplot layout 1, 4 

set ytics 1,1,42
set xtics auto rotate by 45 right

unset key

set title "T"
plot "vart_mem01R.txt" using 5:1 with lines lw 2 title "01R", \
     "vart_mem02R.txt" using 5:1 with lines lw 2 title "02R", \
     "vart_mem03R.txt" using 5:1 with lines lw 2 title "03R", \
     "vart_mem04R.txt" using 5:1 with lines lw 2 title "04R", \
     "vart_mem05R.txt" using 5:1 with lines lw 2 title "05R", \
     "vart_mem06R.txt" using 5:1 with lines lw 2 title "06R", \
     "vart_mem07R.txt" using 5:1 with lines lw 2 title "07R"
     
set title "U"
plot "varu_mem01R.txt" using 5:1 with lines lw 2 title "01R", \
     "varu_mem02R.txt" using 5:1 with lines lw 2 title "02R", \
     "varu_mem03R.txt" using 5:1 with lines lw 2 title "03R", \
     "varu_mem04R.txt" using 5:1 with lines lw 2 title "04R", \
     "varu_mem05R.txt" using 5:1 with lines lw 2 title "05R", \
     "varu_mem06R.txt" using 5:1 with lines lw 2 title "06R", \
     "varu_mem07R.txt" using 5:1 with lines lw 2 title "07R"
     
set title "V"
plot "varv_mem01R.txt" using 5:1 with lines lw 2 title "01R", \
     "varv_mem02R.txt" using 5:1 with lines lw 2 title "02R", \
     "varv_mem03R.txt" using 5:1 with lines lw 2 title "03R", \
     "varv_mem04R.txt" using 5:1 with lines lw 2 title "04R", \
     "varv_mem05R.txt" using 5:1 with lines lw 2 title "05R", \
     "varv_mem06R.txt" using 5:1 with lines lw 2 title "06R", \
     "varv_mem07R.txt" using 5:1 with lines lw 2 title "07R"

set key

set title "Q"
plot "varq_mem01R.txt" using 5:1 with lines lw 2 title "01R", \
     "varq_mem02R.txt" using 5:1 with lines lw 2 title "02R", \
     "varq_mem03R.txt" using 5:1 with lines lw 2 title "03R", \
     "varq_mem04R.txt" using 5:1 with lines lw 2 title "04R", \
     "varq_mem05R.txt" using 5:1 with lines lw 2 title "05R", \
     "varq_mem06R.txt" using 5:1 with lines lw 2 title "06R", \
     "varq_mem07R.txt" using 5:1 with lines lw 2 title "07R"

unset multiplot
EOF

chmod +x plot_stdev.gpl
./plot_stdev.gpl



