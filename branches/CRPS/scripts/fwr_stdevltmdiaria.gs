'reinit'
'open C:\dados\diarios\Reanalises-ECMWF\ci.ctl'
'set display color white'
'c'

'q file 1'
l7=sublin(result,7)
_var=subwrd(l7,1)
say 'The variable found is '_var

vc(_var)
*****************************************************************************
function vc(args)
var=subwrd(args,1)
if(var='');return;endif

monthschar='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
bis='1976 1980 1984 1988 1992 1996 2000'
dpm='31 28 31 30 31 30 31 31 30 31 30 31'
**************
* Ano Inicial
**************
yrb=1979
**************
* Ano Final
**************
yre=2002
ny = (yre-yrb)+1
say 'Nunber of years (ny)='ny
'set x 1 145'
'set y 1 49'
'q dims'
'set gxout fwrite'
*'set gxout shaded'
************************************
* loops over month, day, then year
************************************
month = 0
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
while(month < 13)
    month = month + 1
    mon = subwrd(monthschar,month)
    say '==> Processing 'mon
    'set fwrite dailyltmstdv_'var'_'mon'.bin'
    daymax=subwrd(dpm,month)
    day = 0
    while (day < daymax)
      day = day + 1
      'define vm = const('var',0)'
      'define soma=const('var',0)'
      year = yrb
********************************************
*     Calcula a media
********************************************
      while(year <= yre)
          'set time 'day%mon%year
*          say day%mon%year
          'define vm=vm+'var
          year = year + 1
      endwhile
      'define vm=vm/'ny
********************************************
*     Calcula o desvio padrao
********************************************
      year = yrb
      while(year <= yre)
          'set time 'day%mon%year
          'define soma=soma+pow(('var'-vm),2)'
          year = year + 1
      endwhile
      'define stdev=sqrt(soma/'ny-1')'
*      if(day<10)
*         'set fwrite dailyltm_'var'_0'day%mon'.bin'
*      else
*         'set fwrite dailyltm_'var'_'day%mon'.bin'
*      endif
********************************************
*     Escreve a media
********************************************
      'd vm'
*      'disable fwrite'
*      if(day<10)
*         'set fwrite dailystdev_'var'_0'day%mon'.bin'
*      else
*         'set fwrite dailystdev_'var'_'day%mon'.bin'
*      endif
********************************************
*     Escreve o desvio padrao
********************************************
      'd stdev'
*      'disable fwrite'
*      'draw title 'day%mon
*      'run cbar.gs'
    endwhile
    'disable fwrite'
endwhile
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
return

