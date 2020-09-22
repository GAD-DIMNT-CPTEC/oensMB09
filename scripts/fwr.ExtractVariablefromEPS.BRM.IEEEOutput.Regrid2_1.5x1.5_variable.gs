function fwr(args)
rc=gsfallow("on")
datei=subwrd(args,1)
fctlag=subwrd(args,2)
varcrps=subwrd(args,3)

'reinit'
'exec ../datain/epsfilesin.'datei'.'fctlag'.txt'
rc=countctls()

rc=xsure()
'set gxout fwrite'
'set fwrite ../datain/CPTECEPS.'fctlag'ForecastFor'datei'.15Members.grads'
'q dims';say result;
'set stat on'
_currctl=1
nrec=0
*------------------------------------------------------------
while(_currctl<=_lastctl)
   'set dfile '_currctl
   nrec=nrec+1
   'd re('varcrps',240,linear,0.0,1.5,121,linear,-90.0,1.5)';say result;
   _currctl=_currctl+1
endwhile
*------------------------------------------------------------
say "Number of records written:"nrec
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
'quit'
return
