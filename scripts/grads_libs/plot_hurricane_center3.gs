*----------------------------------------------------------------------
*
*  Adapted from plot_hurricane_center2c.gs, which was adapted from:
*
*  (1) "plot_hurricane_center2[a][b].gs" (from "plot_hurricane_center.gs"),
*      locally modified versions derived from Joe Covert's script
*      "plot_tc_shi.gs" (Joe.Covert@noaa.gov), which was posted to the
*      GrADS Listserv on 9/13/2004 (his script was named plot_tc_shi.gs)
*      on 4/27/06, found via a Google search for "plot_tc_shi.gs":
*      http://caos.iisc.ernet.in/gslib/plot_tc_shi.gs
*
*  (2) code in "hurricane_tracking.txt" (taken from an email exchange
*      posted to the GrADS Listserv by Kun-Hsuan Chou and
*      Arturo Caracas Uribe in Oct, 2004)
*
*
*  To use: grads -l
*
*----------------------------------------------------------------------

function main()

  'reinit'

************************************************************************
*  Allow external GrADS functions
************************************************************************

  rc = gsfallow("on")

************************************************************************
*  Define dataset file information and plotting limits for base map...
************************************************************************

**  nc_file1 = ''
**  nc_file2 = ''
**  nc_file3 = ''
**  nc_file = ''nc_file1' 'nc_file2' 'nc_file3''
  nc_file = 'your_netCDF_file'

  lon1 = 298 ; lon2 = 308
  lat1 =  20 ; lat2 =  25

************************************************************************
*  Legend captions, colors, and marks for the plots
************************************************************************

**  nmodels = 1
**  model.1 = 'isabel_ras'
**  model.2 = 'isabel_isotke'
**  model.3 = 'isabel_my25'

**  titles.1 = model.1 ; colors.1 = 2 ; marks.1 = 2
**  titles.2 = model.2 ; colors.2 = 3 ; marks.2 = 2
**  titles.3 = model.3 ; colors.3 = 4 ; marks.3 = 2
***
***  Define the legend plotter input...
***
**  titles = '-t' ; colors = '-c' ; marks = '-m' ; lines = '-l'
**  n=1
**  while ( n <= nmodels )
**    titles = ''titles' "'titles.n'"'
**    colors = ''colors' 'colors.n
**    marks = ''marks' "'marks.n'"'
**    lines = ''lines' 1'
**    n=n+1
**  endwhile

**  legend_info = ''colors' 'lines' 'marks' 'titles' -p'

  nmodels = 1
  model.1 = 'title_name_for_your_netCDF_dataset'

  titles.1 = model.1 ; colors.1 = 2 ; marks.1 = 2
*
*  Define the legend plotter input...
*
  titles = '-t' ; colors = '-c' ; marks = '-m' ; lines = '-l'
  n=1
  while ( n <= nmodels )
    titles = ''titles' "'titles.n'"'
    colors = ''colors' 'colors.n
    marks = ''marks' "'marks.n'"'
    lines = ''lines' 1'
    n=n+1
  endwhile

  legend_info = ''colors' 'lines' 'marks' 'titles' -p'

*  ...other drawing primitives

  marksize = 0.1

  lstyle = 1 ; lthick = 5

************************************************************************
*  Use "cbar_line" or "cbar_line_box"?
************************************************************************

  legend_plotter = 'cbar_line'
*  legend_plotter = 'cbar_line_box'

************************************************************************
*  Plot page limits
************************************************************************

  plot_vpage = '0 11 0 8.5'
  plot_area  = '1 10 1 7.5'

  xl = subwrd(plot_area,1) ; xr = subwrd(plot_area,2)
  yb = subwrd(plot_area,3) ; yt = subwrd(plot_area,4)

  xc = xl + (xr-xl)/2

************************************************************************
*  GrADS metafile output...
************************************************************************

  'enable print hurricane_track.gx'

*-----------------------------------------------------------------------
*  Define and plot track of minimum surface pressure for each dataset...
*-----------------------------------------------------------------------

  nf=1
  while ( nf <= nmodels )

* Open dataset file and define parameters...

    fname = subwrd(nc_file,nf)
    'sdfopen 'fname
    'set dfile 'nf
    'q file'
    rec3 = sublin(result,3) ; binary_file = subwrd(rec3,2)
    say
    say 
    say '*** 'binary_file' ***'
    say 
    rec5 = sublin(result,5)
    nx = subwrd(rec5,3) ; ny = subwrd(rec5,6) ; nz = subwrd(rec5,9) ; nt = subwrd(rec5,12)

    if( nf = 1 )

* Plot base map...

      plot_base_map(plot_vpage,plot_area,lon1,lon2,lat1,lat2)

    endif

* Define locations of "hurricane center" for plotting points, and write
* locations out to ASCII text file for read-in for plotting track later...

    hurricane.nf = 'hurricane_'nf'.txt'

    ts=2
    tt=ts
    while ( tt <= nt )
      'set t 'tt
      say
      say '---------'
      say ' tt = 'tt
      say '---------'
      say
      'set lon 'lon1' 'lon2
      'set lat 'lat1' 'lat2
      'set z 1'
*
* here is where you put in the name of your sea-level pressure variable:
*
      'define psl = name_of_your_sea_level_pressure_variable'
      'set gxout shaded'
      'd psl'
*
* following interactive "box location" code adapted from "cbar_line_box.gs"...
*
      say 'Click where you want the left upper corner of the box'
      'query bpos'
      xb1o = subwrd(result,3)
      yb2o = subwrd(result,4)
      say 'Click where you want the right lower corner of the box'
      'query bpos'
      xb2o = subwrd(result,3)
      yb1o = subwrd(result,4)

      say
      say '...left upper corner of the box at X Y: 'xb1o' 'yb2o
      say '...right lower corner of the box at X Y: 'xb2o' 'yb1o
*
* draw box to show specified area for defining hurricane center...
*
      'set line 1 1'
      'draw rec 'xb1o' 'yb1o' 'xb2o' 'yb2o
*
* convert box "xy" coordinates to "grid" coordinates
* for finding minimum "pressure" (hurricane center)...
*
      'q xy2gr 'xb1o' 'yb2o
      x1o = subwrd(result,3) ; x1 = math_int(x1o+0.5)
      y2o = subwrd(result,6) ; y2 = math_int(y2o+0.5)
      'q xy2gr 'xb2o' 'yb1o
      x2o = subwrd(result,3) ; x2 = math_int(x2o+0.5)
      y1o = subwrd(result,6) ; y1 = math_int(y1o+0.5)
      say
      say 'Specified grid coordinates (x1o,x2o), (y1o,y2o) from box,'
      say 'rounded to (x1,x2), (y1,y2) for finding minimum "pressure"...'
      say
      say ' (x1o,x2o) = ('x1o','x2o')  -->  (x1,x2) = ('x1','x2')'
      say ' (y1o,y2o) = ('y1o','y2o')  -->  (y1,y2) = ('y1','y2')'
*
* find minimum "pressure" within box area (x1,x2), (y1,y2)...
*
      'set x 'x1
      'set y 'y1
      'set z 1'
      'set t 'tt
      'set gxout print'
      nxp = x2-x1+1
*      say '    nxp = 'nxp
      nyp = y2-y1+1
*      say '    nyp = 'nyp
      'd minloc(min(psl,y='y1',y='y2'),x='x1',x='x2')'
        rec=sublin(result,nxp+3)
        xc=subwrd(rec,1)
      'd minloc(min(psl,x='x1',x='x2'),y='y1',y='y2')'
        rec=sublin(result,nyp+3)
        yc=subwrd(rec,1)
      say
      say 'location of minimum "pressure"...'
      say
      say ' (xc,yc) = ('xc','yc')'
*
* find "world" coordinates of (xc,yc) and convert "world" coordinates
* to "xy" coordinates for plotting track of hurricane center...
*
      'set x 'xc
      lonval = subwrd(result,4)
      'set y 'yc
      latval = subwrd(result,4)
      say ' (lonval,latval) = ('lonval','latval')'
      'q w2xy 'lonval' 'latval
      xpos = subwrd(result,3)
      ypos = subwrd(result,6)
      say ' (xpos,ypos) = ('xpos','ypos')'
*
* write (xpos,ypos) to output file...
*
      res = write (''hurricane.nf'','t = 'tt' xpos = 'xpos' ypos = 'ypos'')

      pause()
*
* erase box showing area for defining hurricane center...
*
*      'set line 0 1'
*      'draw rec 'xb1o' 'yb1o' 'xb2o' 'yb2o
      tt=tt+1
    endwhile

    res = close (''hurricane.nf'')

    nf=nf+1
  endwhile

*  Clear frame and re-plot base map...

  plot_base_map(plot_vpage,plot_area,lon1,lon2,lat1,lat2)

*  Plot track(s)...

  nf=1
  while ( nf <= nmodels )

*  Specify drawing primitives

    marktype = marks.nf
    lcolor = colors.nf

    say ' marktype, marksize, lcolor, lstyle and lthick:'
    say ' 'marktype ' ' marksize ' ' lcolor ' ' lstyle ' ' lthick

    tt=ts
*
*  read (xpos,ypos) from ASCII text input file...
*
    res = read(''hurricane.nf'') ; rc = sublin(res,1)
    if( rc != 0 )
      say '"read" status = 'rc
      exit
    endif
    rec = sublin(res,2) ; say '"hurricane.'nf'" record: 'rec
    xc = subwrd(rec,6) ; yc = subwrd(rec,9)

    'set line 'lcolor' 'lstyle' 'lthick
    'draw mark 'marktype' 'xc' 'yc' 'marksize
    tt = tt+1
    while ( tt <= nt )
      res = read(''hurricane.nf'') ; rc = sublin(res,1)
      if( rc != 0 )
        say '"read" status = 'rc
        exit
      endif
      rec = sublin(res,2) ; say '"hurricane.'nf'" record: 'rec
      xn = subwrd(rec,6) ; yn = subwrd(rec,9)
      say ' 'xc' 'yc' 'xn' 'yn
      'draw line 'xc' 'yc' 'xn' 'yn
      'draw mark 'marktype' 'xn' 'yn' 'marksize
      tt=tt+1
      xc = xn
      yc = yn
    endwhile

    nf=nf+1
  endwhile

  say
  say '...plot legend'
  ''legend_plotter' -x 3 -y 6 'legend_info
  say

  'print'
  'disable print'

  say
  say '************************************************'
  say ' Finished with this script.'
  say '************************************************'
  say

return

function plot_base_map(plot_vpage,plot_area,lon1,lon2,lat1,lat2)

*  Plots base map...

  'c'
  'set vpage 'plot_vpage
  'set parea 'plot_area
  'set lon 'lon1' 'lon2
  'set lat 'lat1' 'lat2
  'set mproj latlon'
  'set mpt * 1 1 5'
  'set mpdset hires'
  'set poli on'
*  'set grid on'
  'set grads off'
  'set gxout contour'
  'draw map'

*  add lon/lat lines...

  'set ccolor 1'
  'set cstyle 5' ; 'd lon'
  'set cstyle 5' ; 'd lat'

*  add title, x- and y-labels...

  'draw title \\ Track of Minimum Surface Pressure'
  'draw xlab Longitude \\'
  'draw ylab \\ Latitude'

return
