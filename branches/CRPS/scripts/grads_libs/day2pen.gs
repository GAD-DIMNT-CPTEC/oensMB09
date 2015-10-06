***************************************************************************************
*	$Id: day2pen.gs,v 1.3 2008/07/02 01:57:04 bguan Exp bguan $
*	Copyright (C) 2008 Bin Guan.
*	Distributed under GNU/GPL.
***************************************************************************************
function day2pen(arg)
*
* Daily-to-pentadal regridding.
*
rc=gsfallow('on')

tmpdir='/tmp'
tmpdir='~'
whoamifile='./.whoami.bGASL'
'!whoami>'whoamifile
whoami=sublin(read(whoamifile),2)
rc=close(whoamifile)
*'!unlink 'whoamifile
mytmpdir=tmpdir'/bGASL-'whoami
'!mkdir -p 'mytmpdir
*
* Parse -v option (variables to be saved).
*
_nvar=parseopt(arg,'-','v','var')
*say "numvar="_nvar
if(_nvar=0);usage();return;endif;
*
* For all variables in the CTL file
*
if(_.var.1="all")
   rc = varslevs(1)
endif
*
* Initialize other options.
*
cnt=1
while(cnt<=_nvar)
   _.name.cnt=_.var.cnt
   cnt=cnt+1
endwhile
_.undef.1=default_undef()
_.file.1=''
_.path.1='.'
*
* Parse -n option (name to be used in .ctl).
*
rc=parseopt(arg,'-','n','name')

*
* Parse -u option (undef value to be used in .dat and .ctl).
*
rc=parseopt(arg,'-','u','undef')

*
* Parse -o option (common name for .dat and .ctl pair).
*
rc=parseopt(arg,'-','o','file')

*
* Parse -p option (path to output files).
*
rc=parseopt(arg,'-','p','path')

*->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->-
*
'set gxout fwrite'
'set fwrite '_.path.1'/'_.file.1'.grads'
*
*->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->-
*
* Ensure x-coordinates are integers and there are no redundant grid points.
*
qdims()
_xs_old=_xs
_xe_old=_xe
if(math_int(_xs)!=_xs | math_int(_xe)!=_xe)
   xs_new=math_nint(_xs)
   xe_new=math_nint(_xe)
   'set x 'xs_new' 'xe_new
   qdims()
   endif
   if(_lone-_lons>=360)
   rddnt_points=(_lone-_lons-360)/_dlon+1
   'set x '_xs' '_xe-rddnt_points
   qdims()
endif

*
* Ensure y-coordinates are integers.
*
qdims()
_ys_old=_ys
_ye_old=_ye
if(math_int(_ys)!=_ys | math_int(_ye)!=_ye)
   ys_new=math_nint(_ys)
   ye_new=math_nint(_ye)
   'set y 'ys_new' 'ye_new
   qdims()
endif

_tims_old=_tims
_time_old=_time
**********************************************************************
*
* Setting the whole lenght of time dimension defined in the ENVIRONMENT
* 
'set t '_ts' '_te
**********************************************************************
tempof=_te
deltat=math_format('%5.2f',_te-_ts+1)
pos=0
while(1)
   pos=pos+1
   rec=read("data_das_pentadas.txt")
   if(sublin(rec,1)!=0);break;endif;
   linha=sublin(rec,2)
   datapent.pos=subwrd(linha,1)
endwhile
*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
*
* Loop to find the time corresponding to the first pentad date
* according to the time setting in the ENVIRONMENT
*
tt=_ts
while(1)
   'set t 'tt
   'q dims'
   l5=sublin(result,5)
   hhzddmmmyyyy=subwrd(l5,6)
   dd=substr(hhzddmmmyyyy,4,2)
   mmm=substr(hhzddmmmyyyy,6,3)
   pp=1
   while(pp<=pos-1)
      ddpent=substr(datapent.pp,4,2)
      mmmpent=substr(datapent.pp,6,3)
*     say dd" "ddpent
*     say mmm" "mmmpent
      if(dd=ddpent&mmm=mmmpent)
         tempoi=tt
         flag=1
         break
      endif
      pp=pp+1
   endwhile
   if(flag=1)
      _firstpentad=hhzddmmmyyyy
      break
   endif
   tt=tt+1
endwhile
say "The time is setting as: "_tims" to "_time
say "The first pentad ("dd%mmm") corresponds to the time "tempoi
*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

vcnt=1
while(vcnt<=_nvar)
   'dy2pntmp'vcnt'='_.var.vcnt
   vcnt=vcnt+1
endwhile

qdims()
_zi=_zs
_zf=_ze
*say   _tims
*say   _time
*say   _ts
*say   _te
*say   _zs
*say   _ze

tt=tempoi
ttf=tempof
*
* The variable pentads storages the count 
* for pentads actually processed
*
pentads=0
while(tt<=ttf)
   'set t 'tt
   pentads=pentads+1
   qdims()
   say "Processing pentad "_tims". It is the pentad number "pentads" "tt
   dd=substr(_tims,4,2)
   mmm=substr(_tims,6,3)
   vcnt=1
*->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
*
*  Variables loop
*
   while(vcnt<=_nvar)
      zcnt=_zi
*      say "Processing variable "_.var.vcnt
      
*-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%
*
*     Levels loop
*
      while(zcnt<=_.lev.vcnt)
         'set z 'zcnt
*         say "Processing level "zcnt
         'pentmp=ave('_.var.vcnt',t+0,t+4)'
         'display const(pentmp,'_.undef.1',-u)'
*        'q pos';'c';
*         say pentads" "sublin(result,1)
         zcnt=zcnt+1
      endwhile
*-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%
      vcnt=vcnt+1
   endwhile
*->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
*
*  DESVIO PARA ANOS BISSEXTOS
*
   if(_yrs=1972|_yrs=1976|_yrs=1980|_yrs=1984|_yrs=1988|_yrs=1992|_yrs=1996|_yrs=2000|_yrs=2004|_yrs=2008)
      if(mmm=FEB&dd=25)
         tt=tt+6
      else
         tt=tt+5
      endif
   else
      tt=tt+5
   endif
endwhile
'disable fwrite'
vcnt=1
*----------------------------------------------------------------------------
while(vcnt<=_nvar)
   'undefine dy2pntmp'vcnt
   vcnt=vcnt+1
endwhile
*----------------------------------------------------------------------------
'undefine pentmp'
'set gxout contour'

if(_.file.1!='')
*
*  The file to be written was defined in the script calling
*
   writectl(_.path.1'/'_.file.1'.ctl','^'_.file.1'.grads',pentads,_nvar,name)
*   '!cp 'mytmpdir'/day2pen.dat~ '_.path.1'/'_.file.1'.dat'
else
   say "Dentro do else"
   writectl(mytmpdir'/day2pen.ctl~','^day2pen.dat~',pentads,_nvar,name)
endif

*'set x '_xs_old' '_xe_old
*'set y '_ys_old' '_ye_old
*'set z '_zs' '_ze
*'set time '_tims_old' '_time_old

*dfile_old=dfile()
*'open 'mytmpdir'/day2pen.ctl~'
*file_num=file_number()
*'set dfile 'file_num
*vcnt=1
*while(vcnt<=_nvar)
*   _.name.vcnt'='_.name.vcnt'.'file_num
*   vcnt=vcnt+1
*endwhile
*'set dfile 'dfile_old

return
***************************************************************************************
function writectl(ctlfile,datfile,nt,nv,var)
*
* Write the .ctl file for the temporary .dat file
*
lines=9
line.1='DSET 'datfile
line.2='UNDEF '_.undef.1
line.3='OPTIONS 365_DAY_CALENDAR'
line.4=_xdef
line.5=_ydef
line.6='ZDEF 5 LEVELS 925 850 700 500 200'
* Note: 'nt' below is an argument of function 'writectl', not the global variable '_nt'.
line.7='TDEF 'nt' LINEAR '_firstpentad' 5dy'
line.8='VARS 'nv
line.9='ENDVARS'
cnt=1
while(cnt<=lines-1)
   if (cnt=5)
      write(ctlfile,"ydef    96 levels")
      write(ctlfile," -88.57217 -86.72253 -84.86197 -82.99894 -81.13498 -79.27056 -77.40589 -75.54106")
      write(ctlfile," -73.67613 -71.81113 -69.94608 -68.08099 -66.21587 -64.35073 -62.48557 -60.62040")
      write(ctlfile," -58.75521 -56.89001 -55.02481 -53.15960 -51.29438 -49.42915 -47.56393 -45.69869")
      write(ctlfile," -43.83346 -41.96822 -40.10298 -38.23774 -36.37249 -34.50724 -32.64199 -30.77674")
      write(ctlfile," -28.91149 -27.04624 -25.18099 -23.31573 -21.45048 -19.58522 -17.71996 -15.85470")
      write(ctlfile," -13.98945 -12.12419 -10.25893  -8.39367  -6.52841  -4.66315  -2.79789  -0.93263")
      write(ctlfile,"   0.93263   2.79789   4.66315   6.52841   8.39367  10.25893  12.12419  13.98945")
      write(ctlfile,"  15.85470  17.71996  19.58522  21.45048  23.31573  25.18099  27.04624  28.91149")
      write(ctlfile,"  30.77674  32.64199  34.50724  36.37249  38.23774  40.10298  41.96822  43.83346")
      write(ctlfile,"  45.69869  47.56393  49.42915  51.29438  53.15960  55.02481  56.89001  58.75521")
      write(ctlfile,"  60.62040  62.48557  64.35073  66.21587  68.08099  69.94608  71.81113  73.67613")
      write(ctlfile,"  75.54106  77.40589  79.27056  81.13498  82.99894  84.86197  86.72253  88.57217")
      cnt=cnt+1
   else
      status=write(ctlfile,line.cnt)
      cnt=cnt+1
   endif
endwhile
cnt=1
while(cnt<=nv)
   varline=_.var.cnt' '_.lev.cnt' 99 '
   if(cnt=1);status=write(ctlfile,varline%"surface TOPOGRAPHY [m]");endif;
   if(cnt=2);status=write(ctlfile,varline%"surface LAND SEA MASK [0,1]");endif;
   if(cnt=3);status=write(ctlfile,varline%"ZONAL WIND (U)                          (M/S             )");endif;
   if(cnt=4);status=write(ctlfile,varline%"MERIDIONAL WIND (V)                     (M/S             )");endif;
   if(cnt=5);status=write(ctlfile,varline%"OMEGA                                   (PA/S            )");endif;
   if(cnt=6);status=write(ctlfile,varline%"2 METRE TEMPERATURE                     (K               )");endif;
   if(cnt=7);status=write(ctlfile,varline%"TIME MEAN SURFACE PRESSURE              (HPA             )");endif;
   if(cnt=8);status=write(ctlfile,varline%"TIME MEAN ZONAL WIND (U)                (M/S             )");endif;
   if(cnt=9);status=write(ctlfile,varline%"TIME MEAN MERIDIONAL WIND (V)           (M/S             )");endif;
   if(cnt=10);status=write(ctlfile,varline%"TIME MEAN VIRTUAL TEMPERATURE           (K               )");endif;
   if(cnt=11);status=write(ctlfile,varline%"TIME MEAN GEOPOTENTIAL HEIGHT           (M               )");endif;
   if(cnt=12);status=write(ctlfile,varline%"TIME MEAN ABSOLUTE TEMPERATURE          (K               )");endif;
   if(cnt=13);status=write(ctlfile,varline%"TIME MEAN SPECIFIC HUMIDITY             (KG/KG           )");endif;
   if(cnt=14);status=write(ctlfile,varline%"TIME MEAN DERIVED OMEGA                 (PA/S            )");endif;
   if(cnt=15);status=write(ctlfile,varline%"TOTAL PRECIPITATION                     (KG/M2/DAY       )");endif;
   if(cnt=16);status=write(ctlfile,varline%"SENSIBLE HEAT FLUX FROM SURFACE         (W/M2            )");endif;
   if(cnt=17);status=write(ctlfile,varline%"LATENT HEAT FLUX FROM SURFACE           (W/M2            )");endif;
   if(cnt=18);status=write(ctlfile,varline%"DOWNWARD LONG WAVE AT BOTTOM            (W/M2            )");endif;
   if(cnt=19);status=write(ctlfile,varline%"UPWARD LONG WAVE AT BOTTOM              (W/M2            )");endif;
   if(cnt=20);status=write(ctlfile,varline%"OUTGOING LONG WAVE AT TOP               (W/M2            )");endif;
   if(cnt=21);status=write(ctlfile,varline%"DOWNWARD SHORT WAVE AT GROUND           (W/M2            )");endif;
   if(cnt=22);status=write(ctlfile,varline%"UPWARD SHORT WAVE AT GROUND             (W/M2            )");endif;
   if(cnt=23);status=write(ctlfile,varline%"UPWARD SHORT WAVE AT TOP                (W/M2            )");endif;
   if(cnt=24);status=write(ctlfile,varline%"TEMPERATURE OF CANOPY AIR SPACE         (K               )");endif;
   if(cnt=25);status=write(ctlfile,varline%"HORIZONTAL MOMENTUM TRANSPORT           (M**2 Sec**-2    )");endif;
   if(cnt=26);status=write(ctlfile,varline%"VERTICAL ZONAL MOMENTUM TRANSPORT       (M Cb Sec**-2    )");endif;
   if(cnt=27);status=write(ctlfile,varline%"VERTICAL MERIDIONAL MOMENTUM TRANSPORT  (M Cb Sec**-2    )");endif;
   if(cnt=28);status=write(ctlfile,varline%"MERIDIONAL SENSIBLE HEAT TRANSPORT      (K M Sec**-1     )");endif;
   if(cnt=29);status=write(ctlfile,varline%"ZONAL SENSIBLE HEAT TRANSPORT           (K M Sec**-1     )");endif;
   if(cnt=30);status=write(ctlfile,varline%"VERTICAL SENSIBLE HEAT TRANSPORT        (K Cb Sec**-1    )");endif;
   if(cnt=31);status=write(ctlfile,varline%"MERIDIONAL SPECIFIC HUMIDITY TRANSPORT  (M/Sec           )");endif;
   if(cnt=32);status=write(ctlfile,varline%"ZONAL SPECIFIC HUMIDITY TRANSPORT       (M/Sec           )");endif;
   if(cnt=33);status=write(ctlfile,varline%"VERTICAL SPECIFIC HUMIDITY TRANSPORT    (Cb/Sec          )");endif;
   cnt=cnt+1
endwhile
status=write(ctlfile,line.lines)
status=close(ctlfile)

return
***************************************************************************************
function dfile()
*
* Get the default file number.
*
'q file'

line1=sublin(result,1)
dfile=subwrd(line1,2)

return dfile
***************************************************************************************
function file_number()
*
* Get the number of files opened.
*
'q files'
if(result='No Files Open')
return 0
endif

lines=1
while(sublin(result,lines+1)!='')
lines=lines+1
endwhile

return lines/3
***************************************************************************************
function default_undef()
*
* Get undef value from the default .ctl file.
*
'q ctlinfo'
if(result='No Files Open')
return 'unknown'
endif

lines=1
while(1)
lin=sublin(result,lines)
if(subwrd(lin,1)='undef'|subwrd(lin,1)='UNDEF')
return subwrd(lin,2)
endif
lines=lines+1
endwhile

return
***************************************************************************************
function usage()
*
* Print usage information.
*
say '  Daily-to-pentadal regridding.'
say ''
say '  Usage: day2pen -v <var1> [<var2>...] [-n <name1> [<name2>...]] [-u <undef>] [-o <file>] [-p <path>]'
say '     <var>: daily variable.'
say '     <name>: name for pentadal variable. Same as <var> if unset.'
say '     <undef>: undef value for .dat and .ctl. Defaults to the value found in ctlinfo.'
say '     <file>: common name for output .dat and .ctl pair. No file output if unset.'
say '     <path>: path to output files. Do NOT include trailing "/". Defaults to current path.'
say ''
say '  Dependencies: parsestr.gsf, parseopt.gsf, qdims.gsf'
say ''
say '  Copyright (C) 2008 Bin Guan.'
say '  Distributed under GNU/GPL.'
return
***************************************************************************************
function varslevs(numfile)
'q file 'numfile
_varidx=0
lines=1
while(1)
   lin=sublin(result,lines)
   if(subwrd(lin,1)="Number"&subwrd(lin,2)="of"&subwrd(lin,3)='Variables')
      _varidx=1
      _nvar=subwrd(lin,5)
      say "Number of variables found: "_nvar
   else
      if(_varidx>=1)
         _.var._varidx=subwrd(lin,1)
         _.lev._varidx=subwrd(lin,2)
         if(_.lev._varidx=0);_.lev._varidx=1;endif; 
*         say "Variable "_.var._varidx" defined at "_.lev._varidx" level(s)"
         if(_varidx=_nvar);return;endif;
         _varidx=_varidx+1   
      endif
   endif   
   lines=lines+1
endwhile
return
