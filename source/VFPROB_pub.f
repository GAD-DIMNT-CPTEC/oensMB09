c
c  Main program    VFPROB
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2004-08-01
c
c This is main ensemble based probabilistic verification program
c         for NAEFS project on IBM-SP
c
c  For:   1. Reliability for m+1 probabilities ( m=members )
c         2. Ranked probability score ( and skill scores)
c         3. Brier score ( and Brier skill score )
c         4. Hit rate and false alarm rate
c         5. Resolution and reliability (option)
c         6. Economic values (option)
c         7. Information content (option)
c
c   subroutines                                                    
c              IADDATE---> to add forecast hours to initial data
c              GETGRB ---> to get GRIB format data
c              GETGRBE---> to get GRIB format ensemble data
c              WGT2D  ---> to create 2-dimensional weights
c              GRANGE ---> to calculate max. and min value of array
c              EVALUE ---> to calculate the economic value
c   
c   available climatological dataset:
c      only z500 and t850 available right now
c      future proposed levels and fields:
c      levels--1000,925,850,700,600,500,400,300,250,200,150,100,70,50,30,20,10
c      fields--z,u,v,t and etc.
c
c   special namelist parameter:
c      ictl  -- control parameter
c               1. standard grid to grid verification
c               2. grid to obs verfication
c   parameters:
c      ix    -- x-dimensional
c      iy    -- y-dimensional
c      im    -- ensemble members
c      jb    -- jb>=2: climatologically equally-a-likely-bin 
c               jb=1: climatological based, specified range consideration
c               jb=-1: no climatology, specified value consideration
c
c   Fortran 77 on IBMSP
c
C--------+---------+---------+---------+---------+----------+---------+--
      program VFPROB
c     parameter (ix=144,iy=73,im=10,jb=10)
c     parameter (ix=144,iy=73,im=14,jb=10)
      parameter (ix=144,iy=73,im=_IM,jb=_JB)
      parameter (imp1=im+1,ib=abs(jb))
c 100 numbers valued from low to high
c     parameter (llb=1,lrb=33)       !if jb=1, using these two parameters
      parameter (llb=_LLB,lrb=_LRB)       !if jb=1, using these two parameters
c specfied to >= or <= or somthing else
c     parameter (flb=5900.,frb=7000.)!if jb=-1, using these two parameters
      parameter (flb=_FLB,frb=_FRB)!if jb=-1, using these two parameters
      dimension iens(2,im),ibary(ib)
      dimension fcst(ix,iy,im),fanl(ix,iy)
      dimension rfcst(ix*iy,im),rfanl(ix*iy),rclim(ix*iy,ib+1)
      dimension wght(ix*iy)
      dimension clipm1(101),clipm2(101),clip(101)
      dimension weight(ix,iy),aloc(ix,iy),sumwgt(ix)
      character*80 cfilea,cfilef,cfilen
      character*80 clima1,clima2
      character*3  cmon(12)
      character*7  cfield
      character*3  cmdl
      logical*1    cindex(ix,iy)
      namelist/files/ cfilea,cfilef,cfilen,cfield,cmdl
      namelist/namin/ ictl,idate,nfhrs,ifd,isp,ilv,ilv2,la1,la2,lo1,lo2
c     data iens/2,1,3,1,2,2,3,2,2,3,3,3,2,4,3,4,2,5,3,5/
c     data iens/3,1,3,2,3,3,3,4,3,5,3,6,3,7,3,8,3,9,3,10,
c    *          3,11,3,12,3,13,3,14/
c     data clima1/'/global/shared/stat/CDAS/500HGT.'/
c     data clima2/'/global/shared/stat/CDAS/500HGT.'/
      data cmon/'JAN','FEB','MAR','APR','MAY','JUN',
     .          'JUL','AUG','SEP','OCT','NOV','DEC'/
      data nuc1/13/,nuc2/14/,nuc/15/
  
      common /vscore/ infow(500),probs(500),dists(500)              

c ----
c     job will be controled by read card
c ----
      read  (5,files,end=1000)
      write (6,files)
 1000 continue
      read  (5,namin,end=1020)
      write (6,namin)

      do i = 1, im
       iens(1,i)=3
       iens(2,i)=i
      enddo

      infow(1) = ictl
      infow(2) = idate
      infow(3) = nfhrs
      infow(4) = ifd
      infow(5) = ilv 
      infow(6) = ilv2 
      infow(7) = la1 
      infow(8) = la2 
      infow(9) = lo1 
      infow(10)= lo2 
      infow(11)= ib  

c ----
c     to set up verifying index
c ----
      cindex=.FALSE.
      do ny = la1, la2
       do nx = lo1, lo2
        cindex(nx,ny)=.TRUE.
       enddo
      enddo
    
      if (ictl.eq.1) then
c ----
c     to calculate the weight based on the latitudes
c ----
       do i = 1, ix
        do j = 1, iy
         aloc(i,j) = 90.0 - float(j-1)*2.5
        enddo
       enddo
       call wgt2d(aloc,ix,iy,weight)
       ngp  = (la2-la1+1)*(lo2-lo1+1)
       sumwgt  = 0.
       swgt    = 0.
       do nx = lo1, lo2
        nlat  = 0
        do ny = la1, la2
         nlat = nlat + 1
         sumwgt(nx) = sumwgt(nx) + weight(nx,ny)
         swgt       = swgt + weight(nx,ny)
        enddo
       enddo
       do nx = lo1, lo2
        do ny = la1, la2
         weight(nx,ny) = weight(nx,ny)*float(ngp)/swgt
        enddo
       enddo
      elseif (ictl.eq.2) then
       weight = 1.0
      else
       print *, 'ICTL=',ictl,' is not well defined, please verify!!!'
       stop 10
      endif

c ----
c     convert initial time + forecast time to verified time
c ----
      call iaddate(idate,nfhrs,jdate)
      call getgrb(fanl,ix,iy,cfilea,ifd,isp,ilv,jdate,0,10)
      call getgrbe(fcst,iens,im,ix,iy,cfilef,ifd,isp,ilv,idate,nfhrs,11)

c ----
c     to get climate data set
c ----
      if (jb.gt.0) then
       jmm = mod(jdate/10000,100)
       jdd = mod(jdate/100,  100)
       jm1 = jmm
       if (jdd.le.15) jm2=jmm - 1
       if (jm2.lt.1)  jm2=jm2 + 12
       if (jdd.gt.15) jm2=jmm + 1
       if (jm2.gt.12) jm2=jm2 - 12
 
       lcfn=len_trim(cfilen)
       clima1(1:lcfn)=cfilen   
       clima2(1:lcfn)=cfilen    
       clima1(lcfn+1:lcfn+3)=cmon(jm1)
       clima2(lcfn+1:lcfn+3)=cmon(jm2) 
c      clima1(21:23)=cmon(jm1)
c      clima2(21:23)=cmon(jm2) 
       print *," MONTH-1 is ",clima1
       print *," MONTH-2 is ",clima2
       open(unit=nuc1,file=clima1,form='unformatted',status='old')
       open(unit=nuc2,file=clima2,form='unformatted',status='old')
       rewind (nuc1)
       rewind (nuc2)
  
       fac2 = abs((float(jdd)-15.0)/30.)
       fac1 = 1.0-fac2
       print *,' DAY FACTORS (',jdate,')',fac1,fac2
 
       nxy = 0
       do ny = 1, iy
        do nx = 1, ix
         read (nuc1) clipm1
         read (nuc2) clipm2
         do n = 1, 101
          clip(n) = clipm1(n)*fac1+clipm2(n)*fac2
         enddo
         if (cindex(nx,ny)) then
          nxy = nxy + 1
          if (jb.eq.1) then
           rclim(nxy,1)=clip(llb)
           rclim(nxy,2)=clip(lrb)
          else
           rfa = 100./ib
           rclim(nxy,1)=clip(1)
           do nb = 2, ib
            rclim(nxy,nb)=clip((nb-1)*rfa)
           enddo
           rclim(nxy,ib+1)=clip(101)
          endif
         endif
        enddo
       enddo
       close (nuc1)
       close (nuc2)
      else
       nxy = 0
       do ny = 1, iy
        do nx = 1, ix
         if (cindex(nx,ny)) then
          nxy = nxy + 1
          rclim(nxy,1) = flb
          rclim(nxy,2) = frb
         endif
        enddo
       enddo
      endif   ! if (jb.gt.0) then
      
      tprob= 0.
      cprob= 0.
      nxy = 0
      do ny = la1, la2
       do nx = lo1, lo2
        nxy = nxy + 1
        do m = 1, im
         rfcst(nxy,m) = fcst(nx,ny,m)
        enddo
        rfanl(nxy) = fanl(nx,ny)
        wght(nxy) = weight(nx,ny)
        if (ib.eq.1) then
         tprob = tprob + 1.0*weight(nx,ny)
         if (rfanl(nxy).gt.rclim(nxy,1).and.
     .       rfanl(nxy).le.rclim(nxy,2)) then
          cprob = cprob + 1.0*wght(nxy)
         endif
        endif
       enddo
      enddo
      len = nxy
ccc   to calculate sample climatological relative frequence
      if (ib.gt.1) then
       scrf = 1.0/float(ib)
      else
       scrf = cprob/tprob
      endif

      call dist(rfcst,rfanl,rclim,wght,len,ib,im)
      call prob(rfcst,rfanl,rclim,wght,len,ib,im,scrf)

      call writed(im,cmdl,1)
      call writep(im,cfield,cmdl,3)
      goto 1000
 1020 stop
      end
