dset ^g3dclm%m2.dat
options little_endian template
undef -9.99E+33
title NCEP/NCAR REANALYSIS PROJECT: climatology 50-98
xdef 144 linear   0.000  2.500
ydef  73 linear -90.000  2.500
zdef  17 levels
 1000  925  850  700  600  500  400  300  250  200
  150  100   70   50   30   20   10
tdef 12 linear jan50 1mo
vars  8
ZGEO 17 99 Geopotential height (gpm)
UVEL 17 99 u wind (m/s)
VVEL 17 99 v wind (m/s)
TEMP 17 99 Temperature (K)
OMEG 12 99 Pressure vertical velocity (Pa/s)
UMES  8 99 Specific humidity (kg/kg)
AGPL  0 99 Precipitable water (kg/m**2)
PSNM  0 99 Pressure reduced to MSL (Pa)
endvars
