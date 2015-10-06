rc=gsfallow("on")
'reinit'
'sdfopen ../datain/LongTermSerie.ERAInterim.Monthly.TEMP850.198901-201012.nc'
rc=xsure()
rc=getvar(1)
'set t 1 last'
rc=titf()
'set time jan1989 dec1989'
'clim=ave('_var',t+0,t='_tf',12)'
say 'clim=ave('_var',t+0,t='_tf',12)'
'modify clim seasonal'
'q dims';say result;
monthstr="JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC"
'set stat on'
i=1
while (i<13)
   mmm=subwrd(monthstr,i)
   'set gxout fwrite'
   'set fwrite ../datain/MonthlyClimatology/LongTermMean.ERAInterim.TEMP850.Monthly.'mmm'.1989-2010.grads'
   'set time 'mmm'1989'
   'q dims';say result;
   'd clim';say result;
   'disable fwrite'
   i=i+1
endwhile
