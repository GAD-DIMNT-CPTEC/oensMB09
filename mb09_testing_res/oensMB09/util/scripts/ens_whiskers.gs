* Este script calcula a media, desvio padrao, variancia, minimo e maximo (entre os) 
* dos membros de ensemble gerados a partir de uma analise e plota um blox plot da media
* com os maximos e minimos e a variancia do erro de cada membro
* carlos.frederico@cptec.inpe.br (28/03/2014)
'reinit'
'open template_fcts.ctl'
#'set lon -60'
#'set lat 45'
#'set lev 500'
* Calculate the ensemble mean 
* ---------------------------
*'set e 1'
'set t 2 last'
'define ensmean=ave(zgeo,e=1,e=15)'
*'define ensmean=zgeo(e=41)' 
* Calculate the variance
* ----------------------
diffsq = 'pow(zgeo-ensmean,2)'
variance = 'ave('diffsq',e=1,e=15)'
'define stddev=sqrt('variance')'
 
* Calculate the min/max 
* ---------------------
'define ensmin=tloop(min(zgeo,e=1,e=15))'
'define ensmax=tloop(max(zgeo,e=1,e=15))'
 
* Plot the results
* ----------------
*'clear'
*'set t 1 last'
'set vrange 5000 5600'
*'set ylint 100'
*'set tlsupp year'
*'set grads off'
 
* Draw error bars for min/max
* ---------------------------
'set gxout errbar'
'set ccolor 4'
'd ensmin;ensmax'
 
* Draw bars for +/- standard deviation
* ------------------------------------
plus  = '(ensmean+stddev)'
minus = '(ensmean-stddev)'
'set gxout bar'
'set bargap 50'
'set baropts outline'
'set ccolor 3'
'd 'minus';'plus
 
* Draw line for Ensemble mean 
* ---------------------------
'set gxout line'
'set cmark 0'
'set cthick 6'
'set digsiz 0.05'
'set ccolor 2'
'd ensmean'
