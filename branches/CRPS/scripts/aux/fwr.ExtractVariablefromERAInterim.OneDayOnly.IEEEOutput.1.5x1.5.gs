function extract(args)
rc=gsfallow("on")
aaaammddhh=subwrd(args,1)
hh=substr(aaaammddhh,9,2)
dd=substr(aaaammddhh,7,2)
mm=substr(aaaammddhh,5,2)
aaaa=substr(aaaammddhh,1,4)
rc=mm2mmm(mm)
'reinit'
'sdfopen ../datain/ERAInterim.TEMP850.DJF20082009.Daily.00ZAND12Z.nc'
rc=countctls()
rc=getvar(1)
say 'set time 'hh'Z'dd%_mmm%aaaa
'set time 'hh'Z'dd%_mmm%aaaa
'q dims';say result;
rc=xsure()
'set gxout fwrite'
'set fwrite ../datain/ERAInterim.TEMP850.'aaaa%mm%dd%hh'.grads'
'set stat on'
'd '_var
say result
'disable fwrite'

*
* Writing the CTL
*
'!rm -f ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl' 
'!touch ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl' 
'!echo DSET ^ERAInterim.TEMP850.'aaaammddhh'.grads >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "UNDEF -9.99e+08" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "XDEF 240 LINEAR 0.0 1.5" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "YDEF 121 LINEAR -90.0 1.5" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "ZDEF 1 LEVELS 1000" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "TDEF 1 LINEAR 'hh'Z'dd%_mmm%aaaa'2008 24hr" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "VARS 1" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "T850 1 99 ABSOLUTE TEMPERATURE AT 850 hPa [ K ]" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'
'!echo "ENDVARS" >> ../datain/ERAInterim.TEMP850.'aaaammddhh'.ctl'

'quit'
return
