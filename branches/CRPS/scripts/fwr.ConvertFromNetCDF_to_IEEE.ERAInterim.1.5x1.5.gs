rc=gsfallow("on")
'reinit'
***'sdfopen ERAInterim.Daily.TEMP850.OnlyDJF.00Z.1989to2009.nc'
'sdfopen ERAInterim.TEMP850.Daily.OnlyDecember.Only12Z.1989-2009.nc'
rc=countctls()

targetmonth="DEC"
'set t 1 last'
rc=titf()
rc=xsure()
'set gxout fwrite'
'set fwrite LongTermSerie.ERAInterim.Daily.TEMP850.Only'targetmonth'.Only12Z.1989-2009.grads'
'q dims';say result;
'set stat on'
tt=_ti
nrec=0
*------------------------------------------------------------
while(tt<=_tf)
   'set t 'tt
   'q time'
   l1=sublin(result,1)
   hhddmmmyyyy=subwrd(l1,3)
   mmm=substr(hhddmmmyyyy,6,3)
*   if(mmm=targetmonth)
   nrec=nrec+1
   say hhddmmmyyyy
   'd t';say result;
*   endif
   tt=tt+1
endwhile
*------------------------------------------------------------
say "Number of records written:"nrec
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
'quit'
