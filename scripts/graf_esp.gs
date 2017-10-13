
*******************************************************************
*****grafico de espaguete com a saída da previsão por conjunto*****
*******************************************************************

'reinit'

'open temp.ctl'
'sdfopen geo500hpa.nc'

en=1

**seleção da região, tempo e nível**

'set lon -60'
'set lat 45'
'set t 1 121'
'set z 22'

**seleção das características do gráfico**

'set font 5'
'set grads off'
'set grid off'
'set display color white'
'c'

**plotar os membros do conjunto**

while(en<=15)

'set e 'en''
'set cmark 0'
'set ccolor 8'
'set xaxis 1 31 1'
'd zgeo*10'

en=en+1

endwhile

say fazer' 'media

pull x

**plotar a média dos conjuntos**

'set cmark 0'
'set ccolor 1'
'set cthick 10'
'define media=ave(zgeo*10, e=1, e=15)'
'd media'

say fazer' 'obs

**plotar a reanálise para comparar com as saida do modelo**

pull x

*'set dfile 2'
'set cmark 0'
'set ccolor 3'
'set cthick 10'
'd z.2(lon=-60,lat=45,e=1,z=1)'

'cbar_line -x 0 -y 7.46761 -c 8 1 3 -m 0 0 0 -l 1 1 1 -t "MEMBROS" "MEDIA MEMBROS" "ERA"'

**calcuular correlação entre a média dos membros e o era**

say fazer' 'correlacao

pull x

'set t 1'
'define lag0=tcorr(media,z.2(lon=-60,lat=45,e=1,z=1),t=1,t=121)'
'd lag0'

r=subwrd(result,4)
valor=substr(r,1,4)
'set strsiz 0.2'
'draw string 9 7.5 r='valor''

**salvar figura**

'printim geo500_jan_31dias.gif'

say fim

