MODULE m_Recomposition

  USE m_Parameters, ONLY: Ki4, Kr8, ICaseRec, ICaseDec, EaRad

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: Init_Recomposition, ClsMem_Recomposition, Recomposition_Scalar, &
            Recomposition_Vector, DivgVortToUV, glat, coslat

  INTEGER (KIND=Ki4) :: Mend, Imax, Jmax, Kmax, Jmaxhf, Mend1, Mend2, Mend3, &
                        Mends, Mendv, Mnwv2, Mnwv3, Mnwv1, Mnwv0

  INTEGER (KIND=Ki4), DIMENSION (:,:), ALLOCATABLE :: la0, la1

  REAL (KIND=Kr8), DIMENSION (:), ALLOCATABLE :: colrad, glat, coslat, &
                                                 eps, pln, xpln, ypln

  REAL (KIND=Kr8), DIMENSION (:,:), ALLOCATABLE :: qlns, qlnv, coskx, sinkx

  REAL (KIND=Kr8), DIMENSION (:,:,:), ALLOCATABLE :: FouCoefA, FouCoefB


CONTAINS


SUBROUTINE Init_Recomposition (MendI, ImaxI, JmaxI, KmaxI)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: MendI, ImaxI, JmaxI, KmaxI

  Mend=MendI
  Imax=ImaxI
  Jmax=JmaxI
  Kmax=KmaxI
  Jmaxhf=Jmax/2
  Mend1=Mend+1
  Mend2=Mend+2
  Mend3=Mend+3
  Mends=2*Mend1
  Mendv=2*Mend1
  Mnwv2=Mend1*Mend2
  Mnwv3=Mnwv2+2*Mend1
  Mnwv0=Mnwv2/2
  Mnwv1=Mnwv3/2

  ALLOCATE (la0 (Mend1,Mend1), la1 (Mend1,Mend2))
  ALLOCATE (eps (Mnwv1), colrad (Jmaxhf))
  ALLOCATE (glat (Jmax), coslat (Jmax))
  ALLOCATE (xpln (Mend1), ypln (Mend1), pln (Mnwv1))
  ALLOCATE (qlns (Mnwv2,Jmaxhf), qlnv (Mnwv3,Jmaxhf))
  ALLOCATE (coskx (Imax,Mend), sinkx (Imax,Mend))

  CALL Init_Legendre
  CALL Init_Fourier

END SUBROUTINE Init_Recomposition


SUBROUTINE ClsMem_Recomposition

  IMPLICIT NONE

  DEALLOCATE (la0, la1)
  DEALLOCATE (eps, colrad)
  DEALLOCATE (glat, coslat)
  DEALLOCATE (xpln, ypln, pln)
  DEALLOCATE (qlns, qlnv)
  DEALLOCATE (coskx, sinkx)

END SUBROUTINE ClsMem_Recomposition


SUBROUTINE Recomposition_Scalar (Kdim, qCoef, gGrid)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: Kdim

  REAL (KIND=Kr8), DIMENSION (Mnwv2,Kdim), INTENT (IN OUT) :: qCoef

  REAL (KIND=Kr8), DIMENSION (Imax,Jmax,Kdim), INTENT (OUT) :: gGrid

  ALLOCATE (FouCoefA (Mends,Jmaxhf,Kdim))
  ALLOCATE (FouCoefB (Mends,Jmaxhf,Kdim))

  CALL Trans_Scalar (Kdim, qCoef, ICaseRec)
  CALL Rec_Legendre_Scalar (Kdim, qCoef)
  CALL Rec_Fourier (Kdim, gGrid)
  CALL Trans_Scalar (Kdim, qCoef, IcaseDec)

  DEALLOCATE (FouCoefA, FouCoefB)

END SUBROUTINE Recomposition_Scalar


SUBROUTINE Recomposition_Vector (Kdim, qCoef, gGrid)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: Kdim

  REAL (KIND=Kr8), DIMENSION (Mnwv3,Kdim), INTENT (IN OUT) :: qCoef

  REAL (KIND=Kr8), DIMENSION (Imax,Jmax,Kdim), INTENT (OUT) :: gGrid

  ALLOCATE (FouCoefA (Mendv,Jmaxhf,Kdim))
  ALLOCATE (FouCoefB (Mendv,Jmaxhf,Kdim))

  CALL Trans_Vector (Kdim, qCoef, ICaseRec)
  CALL Rec_Legendre_Vector (Kdim, qCoef)
  CALL Rec_Fourier (Kdim, gGrid)
  CALL Trans_Vector (Kdim, qCoef, ICaseDec)

  DEALLOCATE (FouCoefA, FouCoefB)

END SUBROUTINE Recomposition_Vector


SUBROUTINE Init_Legendre

  IMPLICIT NONE

  INTEGER (KIND=Ki4) :: j, l, nn, Mmax, mm, lx

  REAL (KIND=Kr8) :: rd

  l=0
  DO nn=1,Mend1
    Mmax=Mend2-nn
    DO mm=1,Mmax
      l=l+1
      la0(mm,nn)=l
    END DO
  END DO
  l=0
  DO mm=1,Mend1
    l=l+1
    la1(mm,1)=l
  END DO
  DO nn=2,Mend2
    Mmax=Mend3-nn
    DO mm=1,Mmax
      l=l+1
      la1(mm,nn)=l
    END DO
  END DO
  CALL epslon
  CALL glats
  rd=45.0_Kr8/ATAN(1.0_Kr8)
  DO j=1,Jmaxhf
     glat(j)=90.0_Kr8-colrad(j)*rd
     coslat(j)=cos(glat(j)/rd)
     glat(Jmax-j+1)=-glat(j)
     coslat(Jmax-j+1)=coslat(j)
     CALL pln2 (j)
     l=0
     DO nn=1,Mend1
        Mmax=Mend2-nn
        DO mm=1,Mmax
           l=l+1
           lx=la1(mm,nn)
           qlns(2*l-1,j)=pln(lx)
           qlns(2*l  ,j)=pln(lx)
        END DO
     END DO
     DO l=1,Mnwv1
       qlnv(2*l-1,j)=pln(l)
       qlnv(2*l  ,j)=pln(l)
     END DO
  END DO

END SUBROUTINE Init_Legendre


SUBROUTINE epslon

  IMPLICIT NONE

  INTEGER (KIND=Ki4) :: l, nn, Mmax, mm

  REAL (KIND=Kr8) :: am, an

  DO l=1,Mend1
     eps(l)=0.0_Kr8
  END DO
  l=Mend1
  DO nn=2,Mend2
     Mmax=Mend3-nn
     DO mm=1,Mmax
        l=l+1
        am=REAL(mm-1)
        an=REAL(mm+nn-2)
        eps(l)=SQRT((an*an-am*am)/(4.0_Kr8*an*an-1.0_Kr8))
     END DO
  END DO

END SUBROUTINE epslon


SUBROUTINE glats

  IMPLICIT NONE

  INTEGER (KIND=Ki4) :: j

  REAL (KIND=Kr8) :: prec, scale, dradz, rad, drad, p2, p1

  prec=10.0_Kr8*EPSILON(prec)
  scale=2.0_Kr8/(REAL(Jmax)*REAL(Jmax))
!*JPB  dradz=ATAN(1.0_Kr8)/90.0_Kr8
  dradz=ATAN(1.0_Kr8)/REAL(Jmax)
  rad=0.0_Kr8
  DO j=1,Jmaxhf
     drad=dradz
10   CALL poly (Jmax, rad, p2)
20   p1=p2
     rad=rad+drad
     CALL poly (Jmax, rad, p2)
     IF (SIGN(1.0_Kr8,p1) == SIGN(1.0_Kr8,p2)) GO TO 20
     IF (drad > prec) THEN
        rad=rad-drad
        drad=drad*0.25_Kr8
        GO TO 10
     END IF
     colrad(j)=rad
  END DO

END SUBROUTINE glats


SUBROUTINE poly (n, rad, p)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN ) :: n

  REAL (KIND=Kr8), INTENT (IN ) :: rad

  REAL (KIND=Kr8), INTENT (OUT) :: p

  INTEGER (KIND=Ki4) :: i

  REAL (KIND=Kr8) :: x, y1, y2, y3, g

  x=COS(rad)
  y1=1.0_Kr8
  y2=x
  DO i=2,n
     g=x*y2
     y3=g-y1+g-(g-y1)/REAL(i)
     y1=y2
     y2=y3
  END DO
  p=y3

END SUBROUTINE poly


SUBROUTINE pln2 (lat)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN ) :: lat

  INTEGER (KIND=Ki4) :: mm, nn, Mmax, lx, ly, lz

  REAL (KIND=Kr8) :: colr, sinlat, coslat, prod

  INTEGER (KIND=Ki4), SAVE :: ifp = 1

  REAL (KIND=Kr8), SAVE :: rthf

  IF (ifp == 1) THEN
     ifp=0
     DO mm=1,Mend1
        xpln(mm)=SQRT(2.0_Kr8*mm+1.0_Kr8)
        ypln(mm)=SQRT(1.0_Kr8+0.5_Kr8/REAL(mm))
     END DO
     rthf=SQRT(0.5_Kr8)
  END IF
  colr=colrad(lat)
  sinlat=COS(colr)
  coslat=SIN(colr)
  prod=1.0
  DO mm=1,Mend1
     pln(mm)=rthf*prod
     !     line below should only be used where exponent range is limted
     !     IF (prod < flim) prod=0.0_Kr8
     prod=prod*coslat*ypln(mm)
  END DO

  DO mm=1,Mend1
     pln(mm+Mend1)=xpln(mm)*sinlat*pln(mm)
  END DO
  DO nn=3,Mend2
     Mmax=Mend3-nn
     DO mm=1,Mmax
        lx=la1(mm,nn  )
        ly=la1(mm,nn-1)
        lz=la1(mm,nn-2)
        pln(lx)=(sinlat*pln(ly)-eps(ly)*pln(lz))/eps(lx)
     END DO
  END DO

END SUBROUTINE pln2


SUBROUTINE Rec_Legendre_Scalar (Kdim, qCoef)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: Kdim

  REAL (KIND=Kr8), DIMENSION (Mnwv2,Kdim), INTENT (IN) :: qCoef

  INTEGER (KIND=Ki4) :: Mend1d, Mmax1, Mmax, Mstr, k, j, l, mm, mn, nn

  REAL (KIND=Kr8), DIMENSION (Mnwv2) :: SumCoef

  Mend1d=2*Mend1
  Mmax1=2*Mend

  DO k=1,Kdim
    DO j=1,Jmaxhf
      FouCoefA(:,j,k)=0.0_Kr8
      FouCoefB(:,j,k)=0.0_Kr8
      l=Mend1d+Mmax1
      DO mn=1,Mnwv2
         SumCoef(mn)=qlns(mn,j)*qCoef(mn,k)
      END DO
      DO nn=3,Mend1
         Mmax=2*(Mend2-nn)
         IF (MOD(nn-1,2) == 0) THEN
            Mstr=0
         ELSE
            Mstr=Mend1d
         END IF
         DO mm=1,Mmax
            l=l+1
            SumCoef(mm+Mstr)=SumCoef(mm+Mstr)+SumCoef(l)
         END DO
      END DO
      DO mm=1,Mend1d
         FouCoefA(mm,j,k)=SumCoef(mm)
         FouCoefB(mm,j,k)=SumCoef(mm)
      END DO
      DO mm=1,Mmax1
         FouCoefA(mm,j,k)=FouCoefA(mm,j,k)+SumCoef(mm+Mend1d)
         FouCoefB(mm,j,k)=FouCoefB(mm,j,k)-SumCoef(mm+Mend1d)
      END DO
    END DO
  END DO

END SUBROUTINE Rec_Legendre_Scalar


SUBROUTINE Rec_Legendre_Vector (Kdim, qCoef)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: Kdim

  REAL (KIND=Kr8), DIMENSION (Mnwv3,Kdim), INTENT (IN) :: qCoef

  INTEGER (KIND=Ki4) :: Mend1d, Mmax1, Mmax, Mstr, k, j, l, mm, mn, nn

  REAL (KIND=Kr8), DIMENSION (Mnwv3) :: SumCoef 

  Mend1d=2*Mend1
  Mmax1=2*Mend1

  DO k=1,Kdim
    DO j=1,Jmaxhf
      FouCoefA(:,j,k)=0.0_Kr8
      FouCoefB(:,j,k)=0.0_Kr8
      l=Mend1d+Mmax1
      DO mn=1,Mnwv3
         SumCoef(mn)=qlnv(mn,j)*qCoef(mn,k)
      END DO
      DO nn=3,Mend2
         Mmax=2*(Mend3-nn)
         IF (MOD(nn-1,2) == 0) THEN
            Mstr=0
         ELSE
            Mstr=Mend1d
         END IF
         DO mm=1,Mmax
            l=l+1
            SumCoef(mm+Mstr)=SumCoef(mm+Mstr)+SumCoef(l)
         END DO
      END DO
      DO mm=1,Mend1d
         FouCoefA(mm,j,k)=SumCoef(mm)
         FouCoefB(mm,j,k)=SumCoef(mm)
      END DO
      DO mm=1,Mmax1
         FouCoefA(mm,j,k)=FouCoefA(mm,j,k)+SumCoef(mm+Mend1d)
         FouCoefB(mm,j,k)=FouCoefB(mm,j,k)-SumCoef(mm+Mend1d)
      END DO
    END DO
  END DO

END SUBROUTINE Rec_Legendre_Vector


SUBROUTINE Trans_Scalar (Kdim, qCoef, ICase)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: Kdim, ICase

  REAL (KIND=Kr8), DIMENSION (Mnwv2,Kdim), INTENT (IN OUT) :: qCoef

  INTEGER (KIND=Ki4) :: k, l, lx, mn, mm, Nmax, nn

  REAL (KIND=Kr8), DIMENSION (Mnwv2) :: qWork

  qWork=0.0
  IF (Icase == 1) THEN
    DO k=1,Kdim
      l=0
      DO mm=1,Mend1
        Nmax=Mend2-mm
        DO nn=1,Nmax
          l=l+1
          lx=la0(mm,nn)
          qWork(2*l-1)=qCoef(2*lx-1,k)
          qWork(2*l  )=qCoef(2*lx  ,k)
         END DO
      END DO
      DO mn=1,Mnwv2
        qCoef(mn,k)=qWork(mn)
      END DO
    END DO
  ELSE
    DO k=1,Kdim
      l=0
      DO mm=1,Mend1
        Nmax=Mend2-mm
        DO nn=1,Nmax
          l=l+1
          lx=la0(mm,nn)
          qWork(2*lx-1)=qCoef(2*l-1,k)
          qWork(2*lx  )=qCoef(2*l  ,k)
        END DO
      END DO
      DO mn=1,Mnwv2
        qCoef(mn,k)=qWork(mn)
      END DO
    END DO
  END IF

END SUBROUTINE Trans_Scalar


SUBROUTINE Trans_Vector (Kdim, qCoef, ICase)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: Kdim, ICase

  REAL (KIND=Kr8), DIMENSION (Mnwv3,Kdim), INTENT (IN OUT) :: qCoef

  INTEGER (KIND=Ki4) :: k, l, lx, mn, mm, Nmax, nn

  REAL (KIND=Kr8), DIMENSION (Mnwv3) :: qWork

  qWork=0.0_Kr8
  IF (Icase == 1) THEN
    DO k=1,Kdim
      l=0
      DO mm=1,Mend1
        Nmax=Mend3-mm
        DO nn=1,Nmax
          l=l+1
          lx=la1(mm,nn)
          qWork(2*l-1)=qCoef(2*lx-1,k)
          qWork(2*l  )=qCoef(2*lx  ,k)
         END DO
      END DO
      DO mn=1,Mnwv3
        qCoef(mn,k)=qWork(mn)
      END DO
    END DO
  ELSE
    DO k=1,Kdim
      l=0
      DO mm=1,Mend1
        Nmax=Mend3-mm
        DO nn=1,Nmax
          l=l+1
          lx=la1(mm,nn)
          qWork(2*lx-1)=qCoef(2*l-1,k)
          qWork(2*lx  )=qCoef(2*l  ,k)
        END DO
      END DO
      DO mn=1,Mnwv3
        qCoef(mn,k)=qWork(mn)
      END DO
    END DO
  END IF

END SUBROUTINE Trans_Vector


SUBROUTINE Init_Fourier

  IMPLICIT NONE

  INTEGER (KIND=Ki4) :: i, m

  REAL (KIND=Kr8) :: ri2pi, ang

  ri2pi=8.0_Kr8*ATAN(1.0_Kr8)/REAL(Imax)

  DO i=1,Imax
    DO m=1,Mend
      ang=REAL((i-1)*m)*ri2pi
      coskx(i,m)=COS(ang)
      sinkx(i,m)=SIN(ang)
    END DO
  END DO

END SUBROUTINE Init_Fourier


SUBROUTINE Rec_Fourier (Kdim, gGrid)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: Kdim

  REAL (KIND=Kr8), DIMENSION (Imax,Jmax,Kdim), INTENT (OUT) :: gGrid

  INTEGER (KIND=Ki4) :: k, j, jj, i, m

  DO k=1,Kdim
    DO j=1,Jmaxhf
      jj=Jmax-j+1
      DO i=1,Imax
        gGrid(i,j,k)=FouCoefA(1,j,k)
        gGrid(i,jj,k)=FouCoefB(1,j,k)
        DO m=1,Mend
          gGrid(i,j,k) =gGrid(i,j,k)+ 2.0_Kr8*(FouCoefA(2*m+1,j,k)*coskx(i,m)- &
                                               FouCoefA(2*m+2,j,k)*sinkx(i,m))
          gGrid(i,jj,k)=gGrid(i,jj,k)+2.0_Kr8*(FouCoefB(2*m+1,j,k)*coskx(i,m)- &
                                               FouCoefB(2*m+2,j,k)*sinkx(i,m))
        END DO
      END DO
    END DO
  END DO

END SUBROUTINE Rec_Fourier


SUBROUTINE DivgVortToUV (qDivg, qVort, qUvel, qVvel)

  ! Calculates Spectral Representation of Cosine-Weighted
  ! Wind Components from Spectral Representation of
  ! Vorticity and Divergence.

  ! qDivg Input:  Divergence (Spectral)
  ! qVort Input:  Vorticity  (Spectral)
  ! qUvel Output: Zonal Pseudo-Wind (Spectral)
  ! qVvel Output: Meridional Pseudo-Wind (Spectral)

  IMPLICIT NONE

  REAL (KIND=Kr8), DIMENSION (2,Mnwv0,Kmax), INTENT (IN OUT) :: qDivg, qVort

  REAL (KIND=Kr8), DIMENSION (2,Mnwv1,Kmax), INTENT (OUT) :: qUvel, qVvel

  INTEGER (KIND=Ki4) :: l, k, l0, l1, mm, nn, l0m, l0p, l1p, Mmax, Nmax

  REAL (KIND=Kr8) :: an, am

  REAL (KIND=Kr8), DIMENSION (Mnwv1) :: e0

  REAL (KIND=Kr8), DIMENSION (Mnwv0) :: e1

  qUvel=0.0_Kr8
  qVvel=0.0_Kr8
  qDivg(2,1:Mend1,:)=0.0_Kr8
  qVort(2,1:Mend1,:)=0.0_Kr8
  CALL Trans_Scalar (Kmax, qDivg, ICaseRec)
  CALL Trans_Scalar (Kmax, qVort, ICaseRec)

  e0(1)=0.0_Kr8
  e1(1)=0.0_Kr8
  DO mm=2,Mend1
    e0(mm)=0.0_Kr8
    e1(mm)=EaRad/REAL(mm)
  END DO
  l=Mend1
  DO nn=2,Mend2
    Mmax=Mend2-nn+1
    DO mm=1,Mmax
      l=l+1
      e0(l)=EaRad*Eps(l)/REAL(nn+mm-2)
    END DO
  END DO
  l=Mend1
  DO nn=2,Mend1
    Mmax=Mend2-nn
    DO mm=1,Mmax
      l=l+1
      an=nn+mm-2
      am=mm-1
      e1(l)=EaRad*am/(an+an*an)
    END DO
  END DO
!cdir novector
  DO k=1,Kmax
    DO mm=1,Mend1
      Nmax=Mend2+1-mm
      qUvel(1,mm,k)= e1(mm)*qDivg(2,mm,k)
      qUvel(2,mm,k)=-e1(mm)*qDivg(1,mm,k)
      qVvel(1,mm,k)= e1(mm)*qVort(2,mm,k)
      qVvel(2,mm,k)=-e1(mm)*qVort(1,mm,k)
      IF (Nmax >= 3) THEN
        l=Mend1
        qUvel(1,mm,k)=qUvel(1,mm,k)+e0(mm+l)*qVort(1,mm+l,k)
        qUvel(2,mm,k)=qUvel(2,mm,k)+e0(mm+l)*qVort(2,mm+l,k)
        qVvel(1,mm,k)=qVvel(1,mm,k)-e0(mm+l)*qDivg(1,mm+l,k)
        qVvel(2,mm,k)=qVvel(2,mm,k)-e0(mm+l)*qDivg(2,mm+l,k)
      END IF
      IF (Nmax >= 4) THEN
        DO nn=2,Nmax-2
          l0 =La0(mm,nn)
          l0p=La0(mm,nn+1)
          l0m=La0(mm,nn-1)
          l1 =La1(mm,nn)
          l1p=La1(mm,nn+1)
          qUvel(1,l1,k)=-e0(l1)*qVort(1,l0m,k)+e0(l1p)*qVort(1,l0p,k)+ &
                         e1(l0)*qDivg(2,l0 ,k)
          qUvel(2,l1,k)=-e0(l1)*qVort(2,l0m,k)+e0(l1p)*qVort(2,l0p,k)- &
                         e1(l0)*qDivg(1,l0 ,k)
          qVvel(1,l1,k)= e0(l1)*qDivg(1,l0m,k)-e0(l1p)*qDivg(1,l0p,k)+ &
                         e1(l0)*qVort(2,l0 ,k)
          qVvel(2,l1,k)= e0(l1)*qDivg(2,l0m,k)-e0(l1p)*qDivg(2,l0p,k)- &
                         e1(l0)*qVort(1,l0 ,k)
        END DO
      END IF
      IF (Nmax >= 3) THEN
        nn=Nmax-1
        l0 =La0(mm,nn)
        l0m=La0(mm,nn-1)
        l1 =La1(mm,nn)
        qUvel(1,l1,k)=-e0(l1)*qVort(1,l0m,k)+e1(l0)*qDivg(2,l0 ,k)
        qUvel(2,l1,k)=-e0(l1)*qVort(2,l0m,k)-e1(l0)*qDivg(1,l0 ,k)
        qVvel(1,l1,k)= e0(l1)*qDivg(1,l0m,k)+e1(l0)*qVort(2,l0 ,k)
        qVvel(2,l1,k)= e0(l1)*qDivg(2,l0m,k)-e1(l0)*qVort(1,l0 ,k)
      END IF
      IF (Nmax >= 2) THEN
        nn=Nmax
        l0m=La0(mm,nn-1)
        l1 =La1(mm,nn)
        qUvel(1,l1,k)=-e0(l1)*qVort(1,l0m,k)
        qUvel(2,l1,k)=-e0(l1)*qVort(2,l0m,k)
        qVvel(1,l1,k)= e0(l1)*qDivg(1,l0m,k)
        qVvel(2,l1,k)= e0(l1)*qDivg(2,l0m,k)
      END IF
    END DO
  END DO

  CALL Trans_Vector (Kmax, qUvel, ICaseDec)
  CALL Trans_Vector (Kmax, qVvel, ICaseDec)
  CALL Trans_Scalar (Kmax, qDivg, ICaseDec)
  CALL Trans_Scalar (Kmax, qVort, ICaseDec)

END SUBROUTINE DivgVortToUV


END MODULE m_Recomposition
