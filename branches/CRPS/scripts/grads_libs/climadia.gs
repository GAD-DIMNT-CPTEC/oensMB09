function ClimaDay(args)
*
var=subwrd(args,1)
if (var = 'help')
    say ' '
    say ' Usage:'
    say ' '
    say 'ClimaDay [var] [df] [lev] [hh] [dd] [mm] [yy] [fc] [fd] \'
    say '         [area] [lonE] [lonW] [latS] [latN]'
    say ' '
    say ' var  = variable name'
    say ' df   = file number (1, 2 or 3)'
    say ' lev  = level of the variable'
    say ' hh   = hour'
    say ' dd   = day'
    say ' mm   = month'
    say ' yy   = year'
    say ' fc   = first convertion factor (var*fc+fd)'
    say ' fd   = second convertion factor ( no convertion: fc=1, fd=0)'
    say ' area = glb for global or lim for limited'
    say ' lonE = eastern longitude for area=lim'
    say ' lonW = western longitude for area=lim'
    say ' latS = southern latitude for area=lim'
    say ' latN = northern latitude for area=lim'
    say ' '
    exit
endif
if (var = '')
    var=z
endif
*
df=subwrd(args,2)
if (df = '')
    df=1
endif
*
lev=subwrd(args,3)
if (lev = '')
    lev=200
endif
if (df = 2 | df = 3 | df = 4)
    lev=1000
endif
*
hh=subwrd(args,4)
if (hh = '')
    hh=00
endif
*
dd=subwrd(args,5)
if (dd = '')
    dd=16
endif
*
mm=subwrd(args,6)
if (mm = '')
    mm=09
endif
*
yy=subwrd(args,7)
if (yy = '')
    yy=1957
endif
*
fc=subwrd(args,8)
if (fc = '')
    fc=1.0
endif
if (var = z)
    fc=1.0/9.80665
endif
if (var = msl)
    fc=0.01
endif
*
fd=subwrd(args,9)
if (fd = '')
    fd=0.0
endif
*
area=subwrd(args,10)
if (area = '')
    area=glb
endif
*
if (area != glb) 
    area=lim
    lonE=subwrd(args,11)
    if (lonE = '')
        lonE=-120
    endif
    lonW=subwrd(args,12)
    if (lonW = '')
        lonW=0
    endif
    latS=subwrd(args,13)
    if (latS = '')
        latS=-60
    endif
    latN=subwrd(args,14)
    if (latN = '')
        latN=30
    endif
else
    lonE=0
    lonW=360
    latS=-90
    latN=90
endif
*
dir='/scratchin/comum/era40/'
'open 'dir'e4_moda_an_pl.ctl'
'open 'dir'e4_moda_an_sfc.ctl'
'open 'dir'e4_moda_fc_sfc.ctl'
*
rec=DateMean(hh,dd,mm,yy)
ma=subwrd(rec,1)
mb=subwrd(rec,2)
fa=subwrd(rec,3)
fb=subwrd(rec,4)
*
'set lon 'lonE' 'lonW
'set lat 'latS' 'latN
'set lev 'lev
'define vca=ave('var'.'df',t='ma',t=540,12)*('fc')+('fd')'
'define vcb=ave('var'.'df',t='mb',t=540,12)*('fc')+('fd')'
'define 'var'clm'lev'=vca*'fa'+vcb*'fb
'undefine vca'
'undefine vcb'
*
return
*
function DateMean(hh,dd,mm,yy)
*
* Month Length in days
  monl.1=31
  monl.2=28
  monl.3=31
  monl.4=30
  monl.5=31
  monl.6=30
  monl.7=31
  monl.8=31
  monl.9=30
  monl.10=31
  monl.11=30
  monl.12=31

  mon=math_int(mm)
  yday=dd+hh/24.0
  mf=mon-1
  mnl=monl.mon
  if (mon = 2 & math_fmod(yy,4) = 0) 
      mnl=29
  endif
  if (yday >= 1.0+0.5*mnl) 
      mf=mon
  endif
  mn=mf+1
  if (mf < 1) 
      mf=12
  endif
  if (mn > 12)
      mn=1
  endif
  mnlf=monl.mf
  if (mf = 2 & math_fmod(yy,4) = 0) 
      mnlf=29
  endif
  add=0.5*mnlf-1.0
  if (mf = mon) 
      add=-add-2.0
  endif
  mnln=monl.mn
  if (mn = 2 & math_fmod(yy,4) = 0) 
      mnln=29
  endif
  fb=2.0*(yday+add)/(mnlf+mnln)
  fa=1.0-fb

  if (mf < 9)
     mf=mf+4
  else
     mf=mf-8
  endif
  if (mf < 12)
     mn=mf+1
  else
     mn=1
  endif

return mf' 'mn' 'fa' 'fb
