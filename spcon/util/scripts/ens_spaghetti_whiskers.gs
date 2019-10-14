'reinit'

'set display color white'
'c'

'open GFCTENS20190817002019081703R.fct.TQ0126L028.ctl'

#'set t 1 last'
'set t 1 15'

'set x 1'
'set y 1'

vars='uvel vvel temp umes pslc'
vv=1
while(vv<=5)
var=subwrd(vars,vv)

if(var='pslc'); ztop=1; else; ztop=28; endif
zz=1
while(zz<=ztop)
'set z 'zz

say 'var='var', z='zz''

# Controle
'define ensctr=aave('var'(e=8),global)'

# Membro Médio
'define ensmean=mean(aave('var',global),e=1,e=7))'

# Desvio Padrão (espalhamento dos membros com relação à média dos membros)
diffsq = 'pow(aave('var',global)-ensmean,2)'
variance = 'ave('diffsq',e=1,e=7)'
'define stddev=sqrt('variance')'
plus  = '(ensmean+stddev)'
minus = '(ensmean-stddev)'

# Membros Min/Max
'define ensmin=tloop(min(aave('var',global),e=1,e=7))'
'define ensmax=tloop(max(aave('var',global),e=1,e=7))'

'set gxout stat'
'd ensmin;ensmax'
linha = sublin(result,8)
min = subwrd(linha,4)
max = subwrd(linha,5)

say min
say max

val = (max - min) 
absval = math_abs(val)

say val
say absval

vmin = min - absval
vmax = max + absval

say vmin
say vmax

# Plotagem
'set vrange 'vmin' 'vmax''

if(vmax>0 & vmin<0)
'set gxout line'
'set ccolor 1'
'set cmark 0'
'set cthick 4'
'd 'var'(e=8)*0'
endif

# Controle
'set gxout line'
'set cmark 4'
'set cthick 4'
'set digsiz 0.08'
'set ccolor 1'
'd ensctr'

# Média Membros
'set gxout line'
'set cmark 4'
'set cthick 8'
'set digsiz 0.08'
'set ccolor 3'
'd ensmean'

# Desvio Padrão (espalhamento dos membros com relação à média dos membros)
'set gxout bar'
'set bargap 50'
'set baropts outline'
'set ccolor 3'
'd 'minus';'plus

# Membros Min/Max
'set gxout errbar'
'set ccolor 4'
'd ensmin;ensmax'

'cbar_line -c 3 1 -m 4 4 -l 8 8 -t "Media (7 membros)" "Controle" -x 2.40215 -y 7.42438'

'draw title 'var' z='zz''

'printim med_ctr_mens-'var'-z'zz'.png'
#pull x
'c'
zz=zz+1
endwhile
vv=vv+1
endwhile
