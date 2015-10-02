function fwr(args)
rc=gsfallow("on")
datei=subwrd(args,1)
filein=subwrd(args,2)
yyyy=substr(datei,1,4)
mm=substr(datei,5,2)
dd=substr(datei,7,2)
hh=substr(datei,9,2)

'reinit'
rc=mm2mmm(mm)
say 'open 'filein
'open 'filein
say result
rc=xsure()
'set gxout fwrite'
'set fwrite ../datain/ECMWFANL.TEMP850.'datei'.grads'
'q dims';say result;
'set stat on'
'd re(tmp850mb,240,linear,0.0,1.5,121,linear,-90.0,1.5)';say result;
*
* Writing the CTL
*
'!rm -f ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!touch ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo DSET ^CPTEC.TEMP850.'datei'.grads >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "UNDEF -9.99e+08" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "XDEF 240 LINEAR 0.0 1.5" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "YDEF 121 LINEAR -90.0 1.5" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "ZDEF 1 LEVELS 1000" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "TDEF 1 LINEAR 'hh'Z'dd%_mmm%yyyy'2008 24hr" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "VARS 1" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "T850 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'!echo "ENDVARS" >> ../datain/ECMWFANL.TEMP850.'datei'.ctl'
'quit'
return
