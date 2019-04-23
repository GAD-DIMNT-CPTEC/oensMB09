* GrADS script to set a portion of the page (vpage)
* Paulo Nobre, 23 June 1996.  COLA.

function manyp(args)
* args are: row, column, No. of rows, No. of columns

* Firt two aguments are row  & column relative position
r = subwrd(args,1);    c = subwrd(args,2)

* Third and fourth arguments are total No. of rows & columns in the page
nrow = subwrd(args,3); ncol = subwrd(args,4)

setpage='true'
'set vpage off'

* Find out the page layout
if (c = '' | r = '')
  setpage = 'off'
else
* Check whether number of columns and rows are given
  if (ncol='' | nrow='')
    say 'Number of frames per page *must* be given: ncol & nrow'
    setpage = 'false'
  else
* Check whether relative position in the page is available
    if (c>ncol | r>nrow)
      say 'relative position 'c' 'r' not available nc='ncol' nrow='nrow
      setpage='false'
    else
* calculate page size atributes
      'q gxinfo'
      lin = sublin(result,2)
      xl = subwrd(lin,4)
      yl = subwrd(lin,6)
      xi = 0.5;                xf = xl - 0.5
      pw = xf - xi;            dx = pw/ncol

      yi = 0.5;                yf = yl - 0.5
      ph = yf - yi;            dy = ph/nrow
* Calculate actual x-y corners of the subframe
      x1 = xi + (c-1) * dx;    x2 = xi + c * dx
      y1 = yi + (nrow-r) * dy; y2 = yi + (nrow-r+1) * dy

      setpage = x1' 'x2' 'y1' 'y2
    endif
  endif
endif

* Set vpage accordingly
* say setpage
if (setpage!='false')
  'set vpage 'setpage
  'set grads off'
endif
return
