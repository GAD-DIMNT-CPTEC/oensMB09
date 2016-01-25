#! /bin/ksh

# set -o xtrace

var=PSNM
reg=HN

set -A Lags `echo $(seq 24 24 360)`
set -A HSinoticos 12

for lag in ${Lags[@]}
do

  for hsin in ${HSinoticos[@]}
  do

    nome_script="Figs-driven-${hsin}Z${lag}h.gs"

    arquivo="'exec crpsfilesin.aave-decjanfev${hsin}z-lag${lag}h.txt'"
    titulo="'draw title CRPSS DEC/JAN/FEV - 2014/2015\${var} ${reg} ${hsin}Z, lag: ${lag}h'"
    imprime="'printim crpss_decjanfev_20142015_${var}_${reg}_${hsin}Z_lag${lag}h.png'"

cat << EOF > ${nome_script}
rc=gsfallow("on")
'reinit'

${arquivo}

rc=countctls()
rc=getvar(1)
rc=xsure()
'q dims';say result;
dom=HN
'set stat on'
'set t 2 16'
_currctl=1
nrec=0
'soma=const('_var',0)'
'soma2=const('_var',0)'
*------------------------------------------------------------
while(_currctl<=_lastctl)
   'set dfile '_currctl
   'q file '_currctl;say result;
   say 'soma=soma+('_var'.'_currctl')'
   'soma=soma+('_var'.'_currctl')'
   'soma2=soma2+(crpsc.'_currctl')'
   'set vrange 0 100'
    'q dims';say result;
   _currctl=_currctl+1
endwhile

'q define';say result;
*------------------------------------------------------------
*say "Number of records written:"nrec
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'set x 1';'set y 1';
'crpseps=soma/'_lastctl
'crpscli=soma2/'_lastctl

'set rgb 20 150 150 150'
'set display color white';'c';
'set ylopts 1 5 0.20'
'set xlopts 1 5 0.20'
'set x 1'
'set y 1'
'set t 2 16'
_currctl=1
*------------------------------------------------------------
while(_currctl<=_lastctl)
   'set dfile '_currctl
   'set grads off'
   'set ccolor 20'
   'set cthick 5'
   'set cmark 0'
   'set vrange 0.5 4.5'
   'set xlabs 1|2|3|4|5|6|7|8|9|10|11|12|13|14|15'
   'd crpsf.'_currctl
   'set cmark 3'
   'set digsize 0.1'
   'd crpsc.'_currctl
   _currctl=_currctl+1
endwhile
*------------------------------------------------------------
'set cmark 2'
'set digsize 0.17'
'set cthick 7'
#'d crpseps'
'c'

'define zero=0'

'set vrange -0.2 1'

'set ylopts 1 5 0.20'
'set xlopts 1 5 0.20'
'set cmark 2'
'set digsize 0.17'
'set cthick 7'
'set xlabs 1|2|3|4|5|6|7|8|9|10|11|12|13|14|15'
'set grads off'
'd 1-(crpseps/crpsc.1)'
'set cmark 3'
'set digsize 0.08'
'set ccolor 1'
'd 1-(crpseps/crpsc.1)'

'set ccolor 1'
'set cmark 0'
'd zero'

${titulo}

${imprime}

'quit'
EOF

    nohup grads -blc "run ${nome_script}" > ${hsin}Z${lag}h.log &

  done
done
