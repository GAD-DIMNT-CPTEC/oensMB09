'reinit'

'set grid off'

'set display color white'
'c'

'open oper/2020040200/template_ens_all.ctl'
'open exp1/2020040200/template_ens_all.ctl'
'open tq666l64/2020040200/GPOSNMC2020040200P.fct.TQ0666L064.ctl'
##'open gfs_0p25/2020040200/gfs025gr.pgrb2.2020040200-tmp.ctl'

'set t 1 last'

'set x 1'
'set y 1'

date='2020040200'

regs='hn tr hs as gl'
vars='zgeo pslc prec umes temp uvel vvel'

# sombreado dev (verde)
'set rgb 20 0 215 0 -80'
# linhas dev (verde)
'set rgb 21 0 215 0 -100'
#sombreado oper (vermelho)
'set rgb 22 251 52 53 -80'
# linhas oper (vermelho)
'set rgb 23 251 52 53 -100'

'set ylab off'
'set xlab off'

vv=1
while(vv<=7)
 
  var=subwrd(vars,vv)

  if(var='pslc'); levs='1000'; len=1; endif
  if(var='prec'); levs='1000'; len=1; endif
  if(var='umes'); levs='1000 925 850 700 500'; len=5; endif
  if(var='temp'); levs='1000 925 850 700 500 300 250 200 50'; len=9; endif
  if(var='uvel'); levs='1000 925 850 700 500 300 250 200 50'; len=9; endif
  if(var='vvel'); levs='1000 925 850 700 500 300 250 200 50'; len=9; endif
  if(var='zgeo'); levs='850 500 250'; len=3; endif

  ll=1
  while(ll<=len)
    lev=subwrd(levs,ll)
    'set lev 'll

    rr=1
    while(rr<=5)
      reg=subwrd(regs,rr)

      say var' 'lev' 'reg

      if(reg='hn'); rpts='lon=0,   lon=360, lat=30,  lat=90';  endif
      if(reg='tr'); rpts='lon=0,   lon=360, lat=-30, lat=30';  endif
      if(reg='hs'); rpts='lon=0,   lon=360, lat=-90, lat=-30'; endif
      if(reg='as'); rpts='lon=230, lon=360, lat=-80, lat=20';  endif
      if(reg='gl'); rpts='global'; endif

# Membro Controle
      'define ensctr1=aave('var'.1(e=1),'rpts')'
      'define ensctr2=aave('var'.2(e=1),'rpts')'
      'define ensctr3=aave('var'.3(e=1),'rpts')'
#      'define ensctr4=aave('var'.4(e=1),'rpts')'

# Membro Médio
      'define ensmean1=mean(aave('var'.1,'rpts'),e=2,e=15))'
      'define ensmean2=mean(aave('var'.2,'rpts'),e=2,e=15))'

# Desvio Padrão (espalhamento dos membros com relação à média)
      diffsq1 = 'pow(aave('var'.1,'rpts')-ensmean1,2)'
      diffsq2 = 'pow(aave('var'.2,'rpts')-ensmean2,2)'
      variance1 = 'ave('diffsq1',e=2,e=15)'
      variance2 = 'ave('diffsq2',e=2,e=15)'
      'define stddev1=sqrt('variance1')'
      'define stddev2=sqrt('variance2')'
      plus1  = '(ensmean1+stddev1)'
      plus2  = '(ensmean2+stddev2)'
      minus1 = '(ensmean1-stddev1)'
      minus2 = '(ensmean2-stddev2)'

# Membros Min/Max
      'define ensmin1=tloop(min(aave('var'.1,'rpts'),e=2,e=15))'
      'define ensmin2=tloop(min(aave('var'.2,'rpts'),e=2,e=15))'
      'define ensmax1=tloop(max(aave('var'.1,'rpts'),e=2,e=15))'
      'define ensmax2=tloop(max(aave('var'.2,'rpts'),e=2,e=15))'
    
      'set gxout stat'
      'd ensmin1;ensmax1'

      linha = sublin(result,8)
      min1 = subwrd(linha,4)
      max1 = subwrd(linha,5)
    
#      say min1
#      say max1
    
      'd ensmin2;ensmax2'
    
      linha = sublin(result,8)
      min2 = subwrd(linha,4)
      max2 = subwrd(linha,5)
    
#      say min2
#      say max2

      if(min1<=min2); min=min1; else; min=min2; endif
      if(max1>=max2); max=max1; else; max=max2; endif        

      val = (max - min) 
      absval = math_abs(val)
    
#      say val
#      say absval
    
      vmin = min - absval
      vmax = max + absval
    
#      say vmin
#      say vmax
    
# Plotagem
      if(var='pslc' & reg='gl'); 'set vrange 985 986.2'; else; 'set vrange 'vmin' 'vmax''; endif
#      'set vrange 'vmin' 'vmax''
  
# Média +/- Desvio Padrão dos Membros oper (sombreado vermelho)
      'set gxout linefill'
      'set lfcols 22 0'
      'd ensmean1+stddev1;ensmean1-stddev1'

# Média +/- Desvio Padrão dos Membros dev (sombreado verde)
      'set lfcols 20 0'
      'd ensmean2+stddev2;ensmean2-stddev2'

# Controle oper (curva vermelha, bolinha fechada)
      'set gxout line'
      'set ccolor 23'
      'set cthick 6'
      'set cmark 3'
      'd ensctr1'

# Controle dev (curva verde, bolinha fechada)
      'set ccolor 21'
      'set cthick 6'
      'set cmark 3'
      'd ensctr2'

# bam oper  (curva preta, bolinha fechada)
      'set ccolor 1'
      'set cthick 6'
      'set cmark 3'
      'd ensctr3'

# gfs (curva preta, bolinha aberta)
#      'set ccolor 1'
#      'set cthick 6'
#      'set cmark 2'
#      'd ensctr4'

# Média Membros oper (curva vermelha, bolinha aberta)
      'set gxout line'
      'set ccolor 23'
      'set cthick 8'
      'set cmark 2'
      'd ensmean1'

# Média Membros dev (curva verde, bolinha aberta)
      'set ccolor 21'
      'set cthick 8'
      'set cmark 2'
      'd ensmean2'

# Média +/- Desvio Padrão oper (linhas verdes máx/mín)
      'set gxout line'
      'set ccolor 23'
      'set cthick 4'
      'set cmark 0'
      'd ensmean1+stddev1'

      'set ylab on'
      'set xlab on'

      'set ccolor 23'
      'set cthick 4'
      'set cmark 0'
      'd ensmean1-stddev1'

# Média +/- Desvio Padrão dev (linhas verdes máx/mín)
      'set ccolor 21'
      'set cthick 4'
      'set cmark 0'
      'd ensmean2+stddev2'

      'set gxout line'
      'set ccolor 21'
      'set cthick 4'
      'set cmark 0'
      'd ensmean2-stddev2'

      'set line 1 3 2'
      'set strsiz 0.1'
      'draw line 5.975 0.2 5.975 7.75'
      'draw string 6 0.25 7 dias'
      'draw line 8.25 0.2 8.25 7.75'
      'draw string 8.275 0.25 11 dias'

     'cbar_line -c 22 22 20 20 -m 2 3 2 3 -l 8 8 8 8 -t "Media (14 membros) OPER" "Controle OPER" "Media (14 membros) DEV" "Controle DEV" -x 2.40215 -y 7.42438'
#     'cbar_line -c 21 21 22 23 -m 2 3 2 3 -l 8 8 8 8 -t "Media (14 membros) DEV" "Controle DEV" "Media (14 membros) OPER" "Controle OPER" -x 2.40215 -y 7.42438'
      'cbar_line -c 1 -m 3 -l 8 -t "BAM OPER" -x 2.40215 -y 1.1'
      
      'draw title 'var' 'lev' hPa 'reg' 'date''
      
      'printim png/mred_ctr_mens-'var'-lev'lev'-'reg'-'date'.png'
    
#      pull x
      'c'

      rr=rr+1
    endwhile  

    ll=ll+1
  endwhile

  vv=vv+1
endwhile

'quit'
