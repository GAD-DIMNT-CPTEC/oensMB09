'reinit'
'open C:\dados\diarios\Reanalises-ECMWF\ci.ctl'
'open C:\dados\diarios\Reanalises-ECMWF\dailyltm_ci.ctl'
'open C:\dados\diarios\Reanalises-ECMWF\dailystdev_ci.ctl'

'q files';say result;

'q file 1'
l7=sublin(result,7)
_var=subwrd(l7,1)
say 'The variable found is '_var

vc(_var)
**************************************************************************
function vc(args)
var=subwrd(args,1)
say var
if(var='');return;endif

monthschar='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
bis='1976 1980 1984 1988 1992 1996 2000 2004'
dpm='31 28 31 30 31 30 31 31 30 31 30 31'
**************
* Ano Inicial
**************
yrb=1979
**************
* Ano Final
**************
yre=2001
ny = (yre-yrb)+1
say 'Nunber of years (ny)='ny
'set x 1 320'
'q dims'
*'set gxout shaded'
'set gxout fwrite'
*'set gxout stat'
************************************
* loops over month, day, then year
************************************
year = yrb
while(year <= yre)
yy=substr(year,3,2)
'set fwrite a'var'daynorm'yy'.bin'
month = 0
j = 0
while(month < 13)
    month = month + 1
    mon = subwrd(monthschar,month)
    daymax=subwrd(dpm,month)
    day = 0
*   Testa se um ano eh bissexto
    bsx=0
    while(bsx<8)
       bsx=bsx+1
       if((year = subwrd(bis,bsx)) & (mon = FEB));daymax=daymax+1;endif;
    endwhile
    while (day < daymax)
          day = day + 1
          j = j + 1
          'set time 'day%mon%year;say 'set time 'day%mon%year;
          'q dims'
          result=sublin(result,5)
          result=subwrd(result,6)
          nome=substr(result,4,9)
          bissxt=substr(nome,1,5)
          say nome' 'bissxt
          write('output.txt',nome' 'bissxt)
          if(bissxt='29FEB')
             'd ('var'.1-ltm.2(t='j-1',z=1))/stdev.3(t='j-1',z=1)'
             write('output.txt','d ('var'.1-ltm.2(t='j-1',z=1))/stdev.3(t='j-1',z=1)')
             j=j-1
          else
             'd ('var'.1-ltm.2(t='j',z=1))/stdev.3(t='j',z=1)'
             write('output.txt','d ('var'.1-ltm.2(t='j',z=1))/stdev.3(t='j',z=1)')
          endif
*'q pos'
*'c'
    endwhile
endwhile
year = year + 1
'disable fwrite'
endwhile
return

