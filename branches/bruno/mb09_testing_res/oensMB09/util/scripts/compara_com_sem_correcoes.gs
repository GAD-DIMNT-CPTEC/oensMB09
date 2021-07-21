'reinit'

path='/cray_home/carlos_bastarz/oensMB09/pre/dataout'

exp1='testes_niveis_pre_bam_1.2.1'
exp2='testes_niveis_pre_bam_1.2.1-correcao'

resols='TQ0126L028 TQ0126L042 TQ0126L064 TQ0126L096'
vars='tvir umes uvel vvel pres'

date='2020042118'

rr=1
while(rr<=4)
  resol=subwrd(resols,rr)

  vv=1
  while(vv<=5)
    var=subwrd(vars,vv)

    if(resol='TQ0126L028'); levs=28; ylevs='1000 900 800 700 600 500 300 200 100 90 67 50 35 23 13.874 6.577'; endif
    if(resol='TQ0126L042'); levs=42; ylevs='1000 920 850 750 600 500 300 200 100 83 68 55 44 34 25 17 10.641 4.925'; endif
    if(resol='TQ0126L064'); levs=64; ylevs='1000 920 850 750 600 500 300 200 100 89 78 68 59 52 45 39 34 28 25 21 18 15 13 11 9 7 5.5 4.284 3.183 2.220 1.378 0.642'; endif
    if(resol='TQ0126L096'); levs=96; ylevs='1000 920 850 750 600 500 300 200 100 93 87 80 74 68 63 58 53 49 45 41 37 34 31 28 26 23 21 19  17 15 13 11 10 9 7 6 5 4 3.052 2.174 1.379 0.643'; endif

    say resol' 'var' 'levs

    if(resol='TQ0126L064')
      file1=''path'/'exp1'/'resol'/GANLNMC'date'S.unf.'resol'.GrADS.ctl'
      file2=''path'/'exp2'/'resol'/GANLNMC'date'S.unf.'resol'.GrADS.ctl'
    else
      file1=''path'/'exp1'/'resol'/GANLNMC'date'S.unf.'resol'.GrADSOut.ctl'
      file2=''path'/'exp2'/'resol'/GANLNMC'date'S.unf.'resol'.GrADSOut.ctl'
    endif

    'open 'file1''
    say file1
    'open 'file2''
    say file2

    'set grads off'

    'set z 1 'levs

    'define media1=aave('var'.1,g)'
    'define media2=aave('var'.2,g)'
    'define diff=media1-media2'

    'define diffmin=min(diff,z=1,z='levs')'
    'define diffmax=max(diff,z=1,z='levs')'

    'set x 1'
    'set gxout stat'

    'd media1'
    lin=sublin(result,8)
    say lin
    mmin1=subwrd(lin,4)
    mmax1=subwrd(lin,5)
    say mmin1' 'mmax1

    'd media2'
    lin=sublin(result,8)
    say lin
    mmin2=subwrd(lin,4)
    mmax2=subwrd(lin,5)
    say mmin2' 'mmax2

    if(mmin1<=mmin2); mmin=mmin1; endif
    if(mmax1>=mmax2); mmax=mmax1; endif

    say mmin' 'mmax

*    'd diffmin;diffmax'
   'd diff'
    lin=sublin(result,8)
    say lin
    dmin=subwrd(lin,4)
    dmax=subwrd(lin,5)
    say dmin' 'dmax
    if(dmin=0 & dmax=0)
      dmin=-1; dmax=1
    else 
      if((dmin=0 & dmax!=0))
        dmin=-1*dmax
      else
        if((dmin<0 & dmax=0))
          dmax=-1*dmin
        endif
      endif
    endif

    say dmin' 'dmax

    'set gxout contour'

    'set x 1' 
    'set z 1 'levs
    'set zlog on'
    'set ylevs 'ylevs

    'set parea 0.5 5.5 0.5 7.75'
    'set ccolor 3'
    'set vrange 'mmin' 'mmax''
    'd media1'
    'set ccolor 6'
    'd media2'
    'draw title 'levs' niveis'

    'cbar_line -c 3 6 -m 3 3 -l 8 8 -t "Velho" "Novo" -x 4.25 -y 7.5'

    'set parea 6.25 10.75 0.5 7.75'
    'set ccolor 1'
    'set vrange 'dmin' 'dmax''
    'd diff'
    'draw title Dif. Velho-Novo'

    'printim 'var'-'levs'.png'

    pull c
    'c' 

    'close 2'
    'close 1'

    vv=vv+1
  endwhile

  rr=rr+1
endwhile
