rc=gsfallow("on")
'reinit'
_hh="00"
if (_hh="00")
   'open ../datain/ERA40.t850.GLOBAL.01JAN1979-31DEC2001.00Z.grads.ctl'
else
   if (_hh="12")
      'open ../datain/ERA40.t850.GLOBAL.01JAN1979-31DEC2001.12Z.grads.ctl'
   else
      'open ../datain/ERA40.TEMP850.GLOBAL.00Z01JAN1979-00Z31DEC2001.dailymean.ctl'
   endif
endif

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
yre=2001
ny = (yre-yrb)+1
say 'Number of years (ny)='ny
'set x 1 320'
'set y 1 161'
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
    if(month<10);mm='0'%month;else;mm=month;endif;
    say '==> Processing 'mon' 'mm
    daymax=subwrd(dpm,month)
    day = 0
    while (day < daymax)
      day = day + 1
      if(day<10);dd='0'%day;else;dd=day;endif;
      'set fwrite ../datain/ERA40/ERA40.DailyTimeSeries.1979-2001.'mm%dd%_hh'.grads'
      year = yrb
      while(year <= yre)
          'set time 'day%mon%year
          'd re('var',240,linear,0.0,1.5,121,linear,-90.0,1.5)'
*          say result
          year = year + 1
      endwhile
      'disable fwrite'
    endwhile
endwhile
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
return

