      subroutine getgrbe(glb,iens,jm,jx,jy,cpgb,ifd,isp,ilev,idate,ifh,
     &                   iunit)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                            C
C     USAGE: READ GRIB FORMATED PRESSURE FILE WITH NO EXTENSION              C
C     CODE : F77 on IBMSP --- Yuejian Zhu -WX20YZ(07/23/04)                  C
C                                                                            C
C     INPUT: unblocked GRIB FORMAT PRESSURE FILE                             C
C                                                                            C
C     OUTPUT:specified resolution cover globally ( JX*JY )                   C
C                                                                            C
C     Arguments:                                                             C
C               1. glb(jx,jy,jm) - 3 dimensional global data                 C
C               2. iens(2,jm) -  ensemble member                             C
C               2. jm - m global ensemble member                             C
C               2. jx - x dimemsional                                        C
C               3. jy - y dimemsional                                        C
C               4. cpgb ( pgb file name input )                              C
C               4. ifd ( field ID input )                                    C
C               5. ilev ( level input )                                      C
C               6. idate ( YYMMDDHH input with Y2K option )                  C
C               6. ifh  ( forecast hours at idate )                          C
C                                                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c--------+----------+----------+----------+----------+----------+----------+--
      parameter (mdata=100000)
      dimension kpds(200),kgds(200),kens(200)
      dimension jpds(200),jgds(200),jens(200)
      dimension iens(2,jm)
      dimension glb(jx,jy,jm),data(mdata)               
      character*80 cpgb    
      logical*1 lb(mdata)
c
      icnt=0
      ldata=jx*jy
      glb=-9999.99
      iy = mod(idate/1000000,100)
      im = mod(idate/10000,  100)
      id = mod(idate/100,    100)
      ih = mod(idate,        100)

      iy = iy - iy/100 *100
c     print *, iy,im,id,ih,ifh
c
  100 continue
      lpgb=len_trim(cpgb)
C     print *, 'PGB=',cpgb(1:lpgb)   
      call baopenr(iunit,cpgb(1:lpgb),iretb)
c
      do imem = 1, jm 
      jj      = 0
      jpds    = -1
      jgds    = -1
      jens    = -1
      jpds(5) = ifd
      jpds(6) = isp                                                  
      jpds(7) = ilev
      if (iy.eq.0) then
       jpds(8) = 100
      else
       jpds(8) = iy
      endif
      jpds(9) = im
      jpds(10)= id 
      jpds(11)= ih
      jpds(14)= ifh
c     commoned by Yuejian Zhu (10/02/2001)
      jpds(23)= 2              ! set up wild card for extension search
      jens(2) = iens(1,imem)
      jens(3) = iens(2,imem)
      if (iy.eq.73) jpds(14)= 1
c--------+----------+----------+----------+----------+----------+----------+--
      call getgbe(iunit,0,mdata,jj,jpds,jgds,jens,
     &            kf,k,kpds,kgds,kens,lb,data,iret)
      if(iret.eq.0) then
       call grange(kf,lb,data,dmin,dmax)
       if (icnt.eq.0) then
        write(*,886)
        icnt=1
       endif
c--------+----------+----------+----------+----------+----------+----------+--
       write(*,888) k,(kpds(i),i=5,11),kpds(14),(kens(i),i=2,3),kf,
     .              dmax,dmin
       do i = 1, 144
        do j = 1, 73
         ij=(j-1)*144 + i
         glb(i,j,imem)=data(ij)
        enddo
       enddo
      else if (iret.eq.99) then
       goto 881
      else
       goto 991
      endif
  881 continue
      enddo
      call baclose(iunit,iret)
      return
  886 format('Irec pds5 pds6 pds7 pds8 pds9 pd10 pd11 pd14',
     .       ' e2 e3  ndata  Maximun  Minimum')
  888 format (i4,8i5,2i3,i7,2f9.2)
  991 print *, ' there is a problem to open pgb file !!! '
      return
  992 print *, ' idate =',idate,' is not acceptable, please check!!!'
      return
  993 print *, ' level =',ilev,' is not acceptable, please check!!!'
      return
      end
