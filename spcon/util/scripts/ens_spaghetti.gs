* Este script plota todos os membros de um ensemble em forma de spaghetti, 
* gerados a partir de uma analise
* carlos.frederico@cptec.inpe.br (28/03/2014)
'reinit'
'open template_fcts.ctl'
'set lon -60'
'set lat -45'
'set lev 500'
'set t 1 last'
e=1
cols='2 3 4 5 6 8 9 10 11 12 13 14 2 3 4 5' 
while (e<=15)
  'set e 'e
  c=subwrd(cols,e)
  'set ccolor 'c
  'set cmark 0'
#  'set vrange 0 50'
  'd temp'
  e=e+1
endwhile
