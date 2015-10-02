      subroutine wgt2d(a,ix,jy,weight)
c
c     input a(ix,jy) is location by latitude and longitude
c
c            0      90      180      270       360
c     0 (90N)-------------------------------------
c            |                 |                 |
c            |                 |                 |
c     90 (EQ)-------------------------------------
c            |                 |                 |
c            |                 |                 |
c    180(90S)-------------------------------------
c
c     output weight(ix,jy) is the weight of each location
c
      dimension a(ix,jy),weight(ix,jy)
c     d2r     = 3.14159/180.
c     d2r     = 3.142/180.
      d2r     = 3.1415926/180.
      do i = 1, ix
       do j = 1, jy
        weight(i,j) = cos(a(i,j)*d2r)
       enddo
      enddo
      return
      end
