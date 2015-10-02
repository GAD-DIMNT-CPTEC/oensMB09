function vc(args)
var=subwrd(args,1)
if(var="")
   say ""
   say "+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+"
   say "Wrong usage! Right usage is: "
   say "fwr_stdevdiaria var yearbegin yearend"
   say "var = variable to be averaged"
   say "yearbegin = first year from the time interval to calculate the stdev"
   say "yearend = last year from the time interval to calculate the stdev"
   say "+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+"
   say ""
   return
else

var=subwrd(args,1)
**************
* Ano Inicial
**************
yrb=subwrd(args,2)
**************
* Ano Final
**************
yre=subwrd(args,3)


monthschar='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
bis='1976 1980 1984 1988 1992 1996 2000 2004 2008'
dpm='31 28 31 30 31 30 31 31 30 31 30 31'
ny = (yre-yrb)+1
say 'Nunber of years (ny)='ny
'set x 1 320'
*'set y 1 49'
'q dims';say result;
say "If the environment is ok press ENTER"
say "Otherwise CTRL-C and ENTER"
pull l
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
    'set fwrite dailystdev_'var'_'mon'.bin'
    daymax=subwrd(dpm,month)
    day = 0
    while (day < daymax)
      day = day + 1
      'define vm = const('var'(time=01jan'yrb'),0)'
      'define soma=const('var'(time=01jan'yrb'),0)'
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
********************************************
*     Escreve o desvio padrao
********************************************
      'd stdev'
    endwhile
    'disable fwrite'
endwhile
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



endif

return

