#! /bin/ksh

# set -o xtrace

# Objetivo:
# Plotar a curva do CRPSS utilizando o Grads.
# Este script plota tambem a CDF dos membros
#
# Dependencias:
# Executar o script plotgraph.T126L28.bash antes
#
# Historico:
# XX/XX/XXXX - Versao original script GrADS (Christopher et al.)
# XX/09/2015 - Adaptacao e simplificados (cfbastarz)

# Escolha da variavel e regiao de interesse
var=PSNM
reg=HN

# Lags e o array com os lags de 24 a 360, em intervalos de 24
set -A Lags `echo $(seq 24 24 360)`

# A rigor, plota-se a curva do CRPS para as 12Z
set -A HSinoticos 12

# Loop sobre os lags
for lag in ${Lags[@]}
do
  # Loop sobre os horarios sinoticos
  for hsin in ${HSinoticos[@]}
  do
    # Monta o nome do script GrADS
    nome_script="Figs-driven-${hsin}Z${lag}h.gs"

    # Nome do arquivo que sera aberto pelo GrADS
    arquivo="'exec crpsfilesin.aave-decjanfev${hsin}z-lag${lag}h.txt'"
    # Titulo da figura (atencao ao periodo de avaliacao)
    titulo="'draw title CRPSS DEC/JAN/FEV - 2014/2015\${var} ${reg} ${hsin}Z, lag: ${lag}h'"
    # Nome da figura salva (atencao ao periodo de avaliacao)
    imprime="'printim crpss_decjanfev_20142015_${var}_${reg}_${hsin}Z_lag${lag}h.png'"

# Monta o script do GrADS
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

    # Executa o GrADS e gera as figuras
    nohup grads -blc "run ${nome_script}" > ${hsin}Z${lag}h.log &

  done
done
