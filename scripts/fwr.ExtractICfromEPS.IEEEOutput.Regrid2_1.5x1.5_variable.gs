function fwr(args)
rc=gsfallow("on")
datei=subwrd(args,1)
filein=subwrd(args,2)
varcrps=subwrd(args,3)
varlev=subwrd(args,4)
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
'set fwrite ../datain/CPTEC.'varcrps'.'datei'.grads'
'q dims';say result;
'set stat on'
'd re('varcrps'(lev='varlev'),240,linear,0.0,1.5,121,linear,-90.0,1.5)';say result;

*
* Writing the CTL
*
'!rm -f ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!touch ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo DSET ^CPTEC.'varcrps'.'datei'.grads >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "UNDEF -9.99e+08" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "XDEF 240 LINEAR 0.0 1.5" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "YDEF 121 LINEAR -90.0 1.5" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "ZDEF 1 LEVELS 'varlev'" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "TDEF 1 LINEAR 'hh'Z'dd%_mmm%yyyy'2008 24hr" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "VARS 1" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "'varcrps' 1 99 'varcrps'" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'!echo "ENDVARS" >> ../datain/CPTEC.'varcrps'.'datei'.ctl'
'quit'
return
