      subroutine writep(im,cfield,cmdl,iwrite)
c
c     im  --  number of ensemble members
c     cfield - variable name
c     cmdl --  model version
c     iwrite -- write options
c               0. default, just print the output
c               1. write to formatted file - compare to global score archive
c               2. write to vsdb format
c
      dimension sfcp(im+1),sanp(im+1)
      dimension hr(im+1),fa(im+1),fp(im+1)
      dimension fv(18),fit(im+1),fir(im)
      dimension yprb(im+1),dummy(im+1)
      character*3 cmdl,cvsn,cres
      character*7 cfield
      character*2 reg

      common /vscore/ infow(500),probs(500),dists(500)

      imp1 = im + 1
      dummy = 0.0
      do ii = 1, imp1
       yprb(ii) = float(ii-1)/float(im) * 100.0
      enddo

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
      ib    = infow(11)
      if (la1.eq.6.and.la2.eq.29.and.lo1.eq.1.and.lo2.eq.144) reg='NH'
      if (la1.eq.45.and.la2.eq.69.and.lo1.eq.1.and.lo2.eq.144) reg='SH'
      if (la1.eq.29.and.la2.eq.45.and.lo1.eq.1.and.lo2.eq.144) reg='TR'
      if (la1.eq.13.and.la2.eq.29.and.lo1.eq.89.and.lo2.eq.125) reg='NA'
      if (la1.eq.6.and.la2.eq.25.and.lo1.eq.1.and.lo2.eq.17) reg='EU'
      if (la1.eq.6.and.la2.eq.33.and.lo1.eq.18.and.lo2.eq.56) reg='AS'
      if (la1.eq.21.and.la2.eq.37.and.lo1.eq.17.and.lo2.eq.49) reg='IN'

      do ii = 1, imp1
       sanp(ii) = probs(ii)
       sfcp(ii) = probs(ii+imp1)
       hr(ii)   = probs(ii+imp1*2)
       fa(ii)   = probs(ii+imp1*3)
      enddo

      rpfs  = probs(1+4*imp1)
      rpfc  = probs(2+4*imp1)
      rpss  = probs(3+4*imp1)
      crpsf = probs(4+4*imp1)
      crpsc = probs(5+4*imp1)
      crpss = probs(6+4*imp1)
      bsfa  = probs(7+4*imp1)
      bsca  = probs(8+4*imp1)
      bssa  = probs(9+4*imp1)
      creli = probs(10+4*imp1)
      creso = probs(11+4*imp1)
      cunce = probs(12+4*imp1)
      cscrf = probs(13+4*imp1)
      cinfm = probs(14+4*imp1)
      hkss  = probs(15+4*imp1)
      bssl  = probs(16+4*imp1)
      bssh  = probs(17+4*imp1)

      do kk = 1, 18
       fv(kk) =  probs(kk+17+4*imp1)
      enddo

      do ii = 1, imp1
       fit(ii) = dists(ii)
      enddo
      do ii = 1, im
       fir(ii) = dists(ii+imp1)
      enddo
      fsprd = dists(1+imp1+im)
      frmsa = dists(2+imp1+im)
      fmerr = dists(3+imp1+im)
      fabse = dists(4+imp1+im)
      faccs = dists(5+imp1+im)

      call iaddate(idate,nfhrs,jdate)

      if (iwrite.eq.0) then
      print *, '++++++++++++++++++++++++++++++++++++'
      print *, '     RELIABILITY DIAGRAM OUTPUT     '
      print *, '++++++++++++++++++++++++++++++++++++'
      write (*,800) la1,la2,lo1,lo2
      write (*,801) idate,nfhrs,jdate,ifd,ilv,(sanp(ii),ii=1,im+1)
      write (*,801) idate,nfhrs,jdate,ifd,ilv,(sfcp(ii),ii=1,im+1)
c     write (*,801) idate,nfhrs,jdate,ifd,ilv,(fp(ii),ii=1,im+1)
      print *, '++++++++++++++++++++++++++++++++++++'
      print *, '        BRIER SCORES OUTPUT         '
      print *, '++++++++++++++++++++++++++++++++++++'
      write (*,800) la1,la2,lo1,lo2
      write (*,803) idate,nfhrs,jdate,ifd,ilv,bsfa,bsca,bssa,creli,creso
     *              ,cunce,cscrf,cinfm
      print *, '++++++++++++++++++++++++++++++++++++'
      print *, ' RANKED PROBABILITY SCORES OUTPUT   '
      print *, '++++++++++++++++++++++++++++++++++++'
      write (*,800) la1,la2,lo1,lo2
      write (*,802) idate,nfhrs,jdate,ifd,ilv,rpfs,rpfc,rpss
      print *, '++++++++++++++++++++++++++++++++++++'
      print *, '    HIT RATE, FALSE ALARM OUTPUT    '
      print *, '++++++++++++++++++++++++++++++++++++'
      write (*,800) la1,la2,lo1,lo2
      write (*,802) idate,nfhrs,jdate,ifd,ilv,(hr(np),np=1,imp1)
      write (*,802) idate,nfhrs,jdate,ifd,ilv,(fa(np),np=1,imp1)
      print *, "==== Economic Values ===="
      print *, "C   1.00    1.00    1.00    1.00    1.00    1.00    1.00
     *    1.00    1.00"
      print *, "--------------------------------------------------------
     *----------------"
      print *, "L   1.05    1.10    1.25    1.50    2.00    3.00    5.00
     *    8.00    10.0"
      print *
      print "(' ',9f8.4)", (fv(kk),kk=1,9)
      print *
      print *, "C   1.00    1.00    1.00    1.00    1.00    1.00    1.00
     *    1.00    1.00"
      print *, "--------------------------------------------------------
     *----------------"
      print *, "L   18.0    27.0    40.0    60.0    90.0    140.    210.
     *    350.    500."
      print *
      print "(' ',9f8.4)", (fv(kk),kk=10,18)
      endif

  800 format(' The Verification Scores for Region ',
     .       'la1=',i3,' la2=',i3,' lo1=',i3,' lo2=',i3)
  801 format(i10,1x,i3,1x,i10,1x,i2,1x,i4,1x,11f6.0)
  802 format(i10,1x,i3,1x,i10,1x,i2,1x,i4,1x,11f6.3)
  803 format(i10,1x,i3,1x,i10,1x,i2,1x,i4,1x,8f8.5)

      if (iwrite.eq.1) then
       fp=0.0
       do ii = 1, imp1
        if (sfcp(ii).ne.0.0) then
         fp(ii)=sanp(ii)/sfcp(ii)*100.0
        endif
       enddo
       write(61,701) cmdl,la1,la2,lo1,lo2
       write(61,702) jdate,idate 
       write(61,703)
       write(61,704) (sanp(ii),ii=1,imp1)
       write(61,704) (sfcp(ii),ii=1,imp1)
       write(61,705)
       write(61,706) (yprb(ii),ii=1,imp1)
c      write(61,706) (sanp(ii)/sfcp(ii)*100.0,ii=1,imp1)
       write(61,706) (fp(ii),ii=1,imp1)
       write(61,707)
       write(61,708) (dummy(ii),ii=1,10),bsfa
       write(61,708) (dummy(ii),ii=1,10),bsca
       write(61,708) (dummy(ii),ii=1,10),bssa
       write(61,709) rpfs,rpfc,creli,creso,cunce,cinfm
       write(61,710) rpss       
       write(61,711)
       write(61,708) (hr(ii),ii=1,imp1)
       write(61,708) (fa(ii),ii=1,imp1)
       write(61,712)
       write(61,708) (dummy(ii),ii=1,11)
       write(61,708) (dummy(ii),ii=1,11)
       write(61,713)
       write(61,708) (dummy(ii),ii=1,11)
       write(61,708) (dummy(ii),ii=1,11)
      endif
  701 format(a4,' Verification Scores for Region ',
     .       'la1=',i3,' la2=',i3,' lo1=',i3,' lo2=',i3)
  702 format('SCORES  AT VALID TIME ',i10,' (ic : ',i10,')') 
  703 format(' RELIABILITY DIAGRAM')
  704 format(11f6.0)
  705 format(' Forecast Probability')
  706 format(11f6.1)
  707 format(' BRIER SCORES')
  708 format(11f6.3)
  709 format(' RANKED PROBABILITY SCORES = ',2f6.3,4f7.4)
  710 format(' RANKED PROBABILITY SKILL SCORES = ',e14.8)
  711 format(' HIT RATES AND FALSE ALARMS OF ENSEMBLE FORECASTS')
  712 format(' HIT RATES AND FALSE ALARMS FOR MRF')
  713 format(' HIT RATES AND FALSE ALARMS FOR LOW RESOLUTION MODEL')

      if (iwrite.eq.2) then
       cvsn='V01'
       cres='002'
       fcnt=3200.
       write(62,601) cvsn,cmdl,cres,nfhrs,idate,fcnt,
     *               (sanp(ii),ii=1,imp1)
      endif

  601 format(a3,1x,a3,"/",a3,i3,i11,' ANL G29 SL1L2 Z P500 = ',f7.0,
     *20e16.9)

      if (iwrite.eq.3) then
       fp=0.0
       do ii = 1, imp1
        if (sfcp(ii).ne.0.0) then
         fp(ii)=sanp(ii)/sfcp(ii)*100.0
        endif
       enddo
       write(63,501) cmdl,im,ib,reg,la1,la2,lo1,lo2
       write(63,502) cfield,jdate,idate,nfhrs
       write(63,503)
       write(63,504) (sanp(ii),ii=1,imp1)
       write(63,504) (sfcp(ii),ii=1,imp1)
       write(63,504) (yprb(ii),ii=1,imp1)
       write(63,504) (fp(ii),ii=1,imp1)
       write(63,505)
       write(63,506) (hr(ii),ii=1,imp1)
       write(63,506) (fa(ii),ii=1,imp1)
       write(63,507) rpfs,rpfc,rpss,crpsf,crpsc,crpss
       write(63,508) bsfa,bsca,bssa,creli,creso,cunce,cinfm,bssl,bssh
       write(63,509)
       write(63,510) (fv(ii),ii=1,18)
       write(63,511) fsprd,frmsa,fmerr,fabse,faccs
       write(63,512) 
       write(63,513) (fit(ii),ii=1,imp1)
       write(63,513) (fir(ii),ii=1,im)
      endif
  501 format('### ',a3,'(',i2,':',i4,'b)',' PROB SCORES for Region: ',
     .       a2,'(la1=',i3,' la2=',i3,' lo1=',i3,' lo2=',i3,')')
  502 format('### VAR:',a7,' AT VALID TIME ',i10,' (ic: ',i10,i4,'hrs)')
  503 format('RELIABILITY DIAGRAM:')
  504 format(51f6.0)
  505 format('HIT RATE and FALSE ALARM RATE:')
  506 format(51f6.3)
  507 format('RPS:',6f10.5)
  508 format('BSS:',9f10.5)                                  
  509 format('ECONOMIC VALUES FOR 18 COST/LOSS RATIOS:')
  510 format(18f6.2)
  511 format('SPREAD and RMSE:',5f10.5)
  512 format('HISTOGRAM and RELATIVE POSITION:')
  513 format(51f6.2)

      return
      end
