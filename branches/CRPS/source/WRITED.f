      subroutine writed(im,cmdl,iwrite)
c
c     im  --  number of ensemble members
c     cmdl --  model version
c     iwrite -- write options
c               0. default, just print the output
c               1. write to formatted file - compare to global score archive
c               2. write to vsdb format
c
      dimension fit(im+1),fir(im)
      character*3 cmdl,cvsn,cres
      common /vscore/ infow(500),probs(500),dists(500)

      imp1=im+1

      ictl  = infow(1) 
      idate = infow(2)
      nfhrs = infow(3)
      ifd   = infow(4)
      ilv   = infow(5)
      ilv2  = infow(6)
      la1   = infow(7)
      la2   = infow(8)
      lo1   = infow(9)
      lo2   = infow(10)

      do ii = 1, imp1
       fit(ii) = dists(ii)
      enddo
      do ii = 1, im
       fir(ii) = dists(ii+imp1)
      enddo
      fsprd = dists(1+imp1+im)
      frmsa = dists(2+imp1+im)

      call iaddate(idate,nfhrs,jdate)

      if (iwrite.eq.0) then
       write (*,800) la1,la2,lo1,lo2
       write (*,801) idate,nfhrs,jdate,ifd,ilv,(fit(ii),ii=1,im+1)
       write (*,801) idate,nfhrs,jdate,ifd,ilv,(fir(ii),ii=1,im)
       write (*,801) idate,nfhrs,jdate,ifd,ilv,fsprd,frmsa
      endif

  800 format(' The Verification Scores for Region ',
     .       'la1=',i3,' la2=',i3,' lo1=',i3,' lo2=',i3)
  801 format(i10,1x,i3,1x,i10,1x,i2,1x,i4,1x,11f5.2)

      if (iwrite.eq.1) then
       write (51,800) la1,la2,lo1,lo2
       write (51,801) idate,nfhrs,jdate,ifd,ilv,(fit(ii),ii=1,im+1)
       write (51,801) idate,nfhrs,jdate,ifd,ilv,(fir(ii),ii=1,im)
       write (51,801) idate,nfhrs,jdate,ifd,ilv,fsprd,frmsa
      endif

      if (iwrite.eq.2) then
       cvsn='V01'
       cres='002'
       fcnt=3200.
       write(52,601) cvsn,cmdl,cres,nfhrs,idate,fcnt,
     *               (fit(ii),ii=1,imp1)
       write(52,601) cvsn,cmdl,cres,nfhrs,idate,fcnt,
     *               (fir(ii),ii=1,im)
       write(52,601) cvsn,cmdl,cres,nfhrs,idate,fcnt,
     *               fsprd,frmsa      
      endif

  601 format(a3,1x,a3,"/",a3,i3,i11,' ANL G29 SL1L2 Z P500 = ',f7.0,
     *20e16.9)


      return
      end
