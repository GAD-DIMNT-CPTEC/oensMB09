rc=gsfallow("on")
'q dims'
lin1=sublin(result,1)
defctl=subwrd(lin1,5)
rc=getvar(defctl)

********************************************************************************************
while(1)
"q gxinfo"
say result
xlims = sublin(result,3)
ylims = sublin(result,4)
x1 = subwrd(xlims,4)
x2 = subwrd(xlims,6)
y1 = subwrd(ylims,4)
y2 = subwrd(ylims,6)
"set rband 21 box "x1" "y1" "x2" "y2
"q pos";say result;
l1=sublin(result,1)
vpgx1=subwrd(l1,3);say vpgx1
vpgy1=subwrd(l1,4);say vpgy1
mousebutton=subwrd(l1,5);say "mousebutton ==>" mousebutton
vpgx2=subwrd(l1,8);say vpgx2
vpgy2=subwrd(l1,9);say vpgy2

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*               ZOOM
*
if(mousebutton=1)
*
* Converting from page coordinates to grid coordinates
*
"q xy2gr "vpgx1" "vpgy1
l1=sublin(result,1)
x1=subwrd(l1,3);say x1;
y1=subwrd(l1,6);say y1;
"q xy2gr "vpgx2" "vpgy2
l1=sublin(result,1)
x2=subwrd(l1,3);say x2;
y2=subwrd(l1,6);say y2;

xx1 = math_nint(x1);say "xx1= "xx1;
xx2 = math_nint(x2);say "xx2= "xx2;
iip=xx2-xx1;_ip = math_abs(iip)
yy1 = math_nint(y1);say "yy1= "yy1;
yy2 = math_nint(y2);say "yy2= "yy2;
jjp=yy2-yy1;_jp = math_abs(jjp)
*
* Converting from grid coordinates to world coordinates
*
"q gr2w "xx1" "yy1
l1=sublin(result,1)
lon1=subwrd(l1,3);say "lon1= "lon1;
lat1=subwrd(l1,6);say "lat1= "lat1;
"q gr2w "xx2" "yy2
l1=sublin(result,1)
lon2=subwrd(l1,3);say "lon2= "lon2;
lat2=subwrd(l1,6);say "lat2= "lat2;
if(lat1>lat2);ll=lat1;lat1=lat2;lat2=ll;endif;
if(lon1>lon2);ll=lon1;lon1=lon2;lon2=ll;endif;
"set lat "lat1" "lat2
"set lon "lon1" "lon2
'c';'d '_var;
endif
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if(mousebutton=3);break;endif;

endwhile
********************************************************************************************

