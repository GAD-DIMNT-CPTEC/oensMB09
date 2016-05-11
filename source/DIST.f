C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      DIST
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2004-08-01
c
c This is main distribution sub-program for NAEFS project on IBM-SP
c
c  For: 1. Talagrand distribution
c       2. Relative position ( or near trues )
c       3. Ensemble spread
c       4. Ensemble mean RMS error
c       5. Perfection measurement ( option )
c
c   subroutine                                                    
c              IADDATE---> to add forecast hours to initial data    
c              GETGRB ---> to get GRIB format data                  
c              GETGRBE---> to get GRIB format ensemble data                  
c              WGT2D  ---> to create 2-dimensional weights
c              GRANGE ---> to calculate max. and min value of array
c              TALAGR ---> to calculate talagrand distribution and etc.
c   
c
c   special namelist parameter:
c      ictl  -- control parameter
c               1. standard grid to grid verification
c               2. grid to obs verfication
c   parameters:
c      ix    -- x-dimensional
c      iy    -- y-dimensional
c      im    -- ensemble members
c
c   Fortran 77 on IBMSP 
c
C--------+---------+---------+---------+---------+----------+---------+--
!CFB
!      subroutine dist(fcst,fanl,clim,wght,len,ib,im)
      subroutine dist(fcst,fanl,clim,wght,len,ib,im,ANLDATE,FCTLAG)
!CFB
      parameter (mxlen=10512)             
      dimension fcst(mxlen,im),fanl(mxlen),clim(mxlen,ib+1)
      dimension wght(mxlen)
      dimension fst(im)
      dimension fit(im+1),it(im+1)
      dimension fir(im),ir(im)

!CFB
      character anldate*10,fctlag*4
!CFB

      common /vscore/ infow(500),probs(500),dists(500)

      imp1 = im + 1
c ----
c to calculate talagrand histogram and N+1 distribution
c ----
      acwt = 0.0
      fit  = 0.0
      fir  = 0.0
      xx   = 0.0
      yy   = 0.0
      xy   = 0.0
      fsprd= 0.0
      frmsa= 0.0
      fmerr= 0.0
      fabse= 0.0
      faccs= 0.0

      do nxy = 1, len                  ! do loop for each latitude
       wfac = wght(nxy)
       acwt = acwt + wfac
       do ii = 1, im
        fst(ii) = fcst(nxy,ii)
        if (fst(ii).eq.-9999.99.or.fanl(nxy).eq.-9999.99) then
         dists(1+imp1+im) = -9.99 
         dists(2+imp1+im) = -9.99
         print *, 'fsprd=',fsprd
         go to 999
        endif
       enddo
       anl      = fanl(nxy)
!CFB
!       call talagr(im,im,fst,anl,it,ir,ave,xmed,sprd,rmsa)
      call talagr(im,im,fst,anl,it,ir,ave,xmed,sprd,rmsa,anldate,fctlag)
!CFB
       do ii = 1, imp1
        fit(ii) = fit(ii) + it(ii)*wfac  
       enddo
       do ii = 1, im
        fir(ii) = fir(ii) + ir(ii)*wfac 
       enddo
       fsprd = fsprd + sprd*wfac
       frmsa = frmsa + rmsa*wfac
       fmerr = fmerr + (ave-anl)*wfac
       fabse = fabse + abs(ave-anl)*wfac

ccc    to calculate ACC scores
ccc    using climatelogical mean instead of medium
       clm = 0.0
       if (ib.gt.1) then
c       if (mod(ib,2).eq.0) then
c        mp=ib/2+1
c        clm=clim(nxy,mp)
c       else
c        mp=ib/2
c        clm=(clim(nxy,mp)+clim(nxy,mp+1))/2.0
c       endif
        do ii = 2, ib 
         clm = clm + clim(nxy,ii)/float(ib-1)
        enddo
       endif
 
c      if (nxy.eq.1000) then
c       print *, "clm=",(clim(nxy,ii),ii=1,ib+1)
c       print *, "clm=",clm, " anl=",anl
c      endif

       xy = xy + (ave-clm)*(anl-clm)*wfac
       xx = xx + (ave-clm)*(ave-clm)*wfac
       yy = yy + (anl-clm)*(anl-clm)*wfac

      enddo
 
      do ii = 1, imp1
       fit(ii) = fit(ii)*100.00/acwt
      enddo
      do ii = 1, im
       fir(ii) = fir(ii)*100.00/2.0/acwt
      enddo
      fsprd = sqrt(fsprd/float(im-1)/acwt)
      frmsa = sqrt(frmsa/acwt       )
      fmerr = fmerr/acwt
      fabse = fabse/acwt
      if (xx.eq.0.0.or.yy.eq.0.0) then
       faccs = 1.0
      else
       faccs = xy/sqrt(xx*yy)
      endif

      do ii = 1, imp1
       dists(ii) = fit(ii)
      enddo
      do ii = 1, im
       dists(ii+imp1) = fir(ii)
      enddo
      dists(1+imp1+im) = fsprd
      dists(2+imp1+im) = frmsa
      dists(3+imp1+im) = fmerr
      dists(4+imp1+im) = fabse
      dists(5+imp1+im) = faccs

c     write (*,800) 
c     write (*,801) (fit(ii),ii=1,imp1)         
c     write (*,802) 
c     write (*,801) (fir(ii),ii=1,im)         
c     write (*,803) 
c     write (*,801) fsprd,frmsa,fmerr,fabse,faccs         
  800 format('Histogram Distribution (Talagrand) (N+1)')
  802 format('Relative Position')
  803 format('Ensemble Spread and RMS Error of Ens. Mean')
  801 format(11f6.2)     
      return
  999 print *, ' There is no data for this cycle (dist)!!!'
      return
      end
