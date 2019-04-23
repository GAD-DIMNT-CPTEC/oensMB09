PROGRAM TopographyGradient

  USE InputParameters, ONLY: r4, r8, rad, dLon, EMRad1, &
                             nferr, nfprt, nfinp, nfout, nfctl, &
                             Imax, Jmax, Kmax, Mend1, Mend2, &
                             Undef, ForecastDay, TimeOfDay, Months, &
                             TGrADS, FileInp, FileOut,DirMain,DirModelIn,DirPreOut, &
                             InitParameters

  USE InputArrays, ONLY: qTopoInp, GrADSOut, qtopo, qTopoS, Topo, &
                         DTopoDx, DTopoDy, CosLatInv, &
                         InitialDate, CurrentDate, SigmaInteface, SigmaLayer, &
                         GetArrays, ClsArrays

  USE FastFourierTransform, ONLY : CreateFFT

  USE LegendreTransform, ONLY : CreateGaussRep, CreateSpectralRep, &
                                CreateLegTrans, gLats, snnp1

  USE Spectral2Grid, ONLY : Transpose, SpecCoef2Grid, SpecCoef2GridD
 
  IMPLICIT NONE

  INTEGER :: i, ll, mm, nn, LRec

  CALL InitParameters ()
  CALL GetArrays ()

  CALL CreateSpectralRep ()
  CALL CreateGaussRep ()
  CALL CreateFFT ()
  CALL CreateLegTrans ()

  CosLatInV=1.0_r8/COS(rad*gLats)
  WRITE(*,*)TRIM(DirMain)//TRIm(DirModelIn)//TRIM(FileInp)
  WRITE(*,*)TRIM(DirMain)//TRIM(DirModelIn)//TRIM(FileOut)
  OPEN(UNIT=nfinp, FILE=TRIM(DirMain)//TRIm(DirModelIn)//TRIM(FileInp), &
                   FORM='UNFORMATTED', STATUS='OLD')

  INQUIRE (IOLENGTH=LRec) GrADSOut
  OPEN(UNIT=nfout, FILE=TRIM(DirMain)//TRIM(DirModelIn)//TRIM(FileOut), &
                   FORM='UNFORMATTED', STATUS='REPLACE', &
                   ACCESS='DIRECT', RECL=LRec)

  ! Read Topography Coeficients From Model Spectral Initial Condition
  
  READ (UNIT=nfinp) ForecastDay, TimeOfDay, InitialDate, CurrentDate, &
                    SigmaInteface, SigmaLayer
  write (*,*) ForecastDay, TimeOfDay
  write (*,*) InitialDate
  write (*,*) CurrentDate
  READ (UNIT=nfinp) qTopoInp
  qTopo=REAL(qTopoInp,r8)
  CALL Transpose (qTopo)
  CALL SpecCoef2Grid (qTopo, Topo)
  DO i=1,Imax
     GrADSOut(i,Jmax:1:-1)=REAL(Topo(i,1:Jmax),r4)
  END DO
  write (*,*) minval(gradsout)
  write (*,*) maxval(gradsout)
  WRITE (UNIT=nfout, REC=1) GrADSOut

  ! Zonal Gradient off Topography
  ll=0
  DO mm=1,Mend1
     DO nn=1,Mend2-mm
        ll=ll+1
        qTopoS(2*ll-1)=-REAL(nn-1,r8)*qTopo(2*ll)
        qTopoS(2*ll  )=+REAL(nn-1,r8)*qTopo(2*ll-1)
     END DO
  END DO
  CALL SpecCoef2Grid (qTopoS, DTopoDx)
  DO i=1,Imax
     GrADSOut(i,Jmax:1:-1)=REAL(DTopoDx(i,1:Jmax)*CosLatInV(1:Jmax)*EMRad1,r4)
  END DO
  WRITE (UNIT=nfout, REC=2) GrADSOut

 ! Meridional Gradient off Topography
   CALL SpecCoef2GridD (qTopo, DTopoDy)
   DO i=1,Imax
      GrADSOut(i,Jmax:1:-1)=REAL(DTopoDy(i,1:Jmax)*CosLatInV(1:Jmax)*EMRad1,r4)
   END DO
   WRITE (UNIT=nfout, REC=3) GrADSOut

  CLOSE(UNIT=nfinp)
  CLOSE(UNIT=nfout)

  TGrADS='  Z         '
  WRITE (TGrADS(1:2), FMT='(I2.2)') InitialDate(1)
  WRITE (TGrADS(4:5), FMT='(I2.2)') InitialDate(3)
  TGrADS(6:8)=Months(InitialDate(2))
  WRITE (TGrADS(9:12), FMT='(I4.4)') InitialDate(4)

  OPEN(UNIT=nfctl, FILE=TRIM(DirMain)//TRIM(DirPreOut)//'.ctl', &
                   FORM='FORMATTED', STATUS='REPLACE')
  WRITE (UNIT=nfctl, FMT='(A)') 'dset '//TRIM(DirMain)//TRIM(DirModelIn)//TRIM(FileOut)
  WRITE (UNIT=nfctl, FMT='(A)') 'options big_endian'
  WRITE (UNIT=nfctl, FMT='(A,1P,G15.7)') 'undef ', Undef
  WRITE (UNIT=nfctl, FMT='(A)') 'title Topography and its Gradient'
  WRITE (UNIT=nfctl, FMT='(A,I5,A,F10.5)') 'xdef ', Imax, ' linear    0.0 ', dLon
  WRITE (UNIT=nfctl, FMT='(A,I5,A,5F10.5)') 'ydef ',Jmax, ' levels ', gLats(Jmax:Jmax-4:-1)
  WRITE (UNIT=nfctl, FMT='(18X,5F10.5)') gLats(Jmax-5:1:-1)
  WRITE (UNIT=nfctl, FMT='(A)') 'zdef 1 levels 1000'
  WRITE (UNIT=nfctl, FMT='(3A)') 'tdef 1 linear ', TGrADS, ' 06hr'
  WRITE (UNIT=nfctl, FMT='(A)') 'vars 3'
  WRITE (UNIT=nfctl, FMT='(A)') 'topo 1 99 Topography (m)'
  WRITE (UNIT=nfctl, FMT='(A)') 'dtpx 1 99 Zonal Gradient of Topography (m/m)'
  WRITE (UNIT=nfctl, FMT='(A)') 'dtpy 1 99 Meridional Gradient of Topography (m/m)'
  WRITE (UNIT=nfctl, FMT='(A)') 'endvars'
  CLOSE (UNIT=nfctl)

  CALL ClsArrays ()

END PROGRAM TopographyGradient
