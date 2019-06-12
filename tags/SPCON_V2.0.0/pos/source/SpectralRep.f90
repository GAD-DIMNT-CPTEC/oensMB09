!
!  $Author: pkubota $
!  $Date: 2006/10/30 18:41:21 $
!  $Revision: 1.2 $
!
MODULE SpectralRep

  USE Constants, ONLY : r8

  USE PrblSize, ONLY : Mend, Mend1, Mend2, Mnwv0, Mnwv1, Mnwv2, Mnwv3, Kmax

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: CreateSpectralRep, dellnp, dztouv, transs, la0, la1, snnp1, eps

  INTEGER, ALLOCATABLE, DIMENSION (:,:) :: la0, la1

  REAL (KIND=r8), ALLOCATABLE, DIMENSION (:) :: snnp1, eps


CONTAINS


  SUBROUTINE CreateSpectralRep ()

    USE Constants, ONLY : EMRad2

    IMPLICIT NONE

    INTEGER :: l, mm, nn

    REAL (KIND=r8) :: sn

    ALLOCATE (la0(Mend1,Mend1))
    ALLOCATE (la1(Mend1,Mend2))
    ALLOCATE (snnp1(Mnwv2))
    ALLOCATE (eps(Mnwv1))

    l=0
    DO nn=1,Mend1
       DO mm=1,Mend2-nn
          l=l+1
          la0(mm,nn)=l
          sn=REAL(mm+nn-2,r8)
          IF (sn /= 0.0_r8) THEN
             sn=-EMRad2/(sn*(sn+1.0_r8))
             snnp1(2*l-1)=sn
             snnp1(2*l)=sn
          ELSE
             snnp1(2*l-1)=0.0_r8
             snnp1(2*l)=0.0_r8
          END IF
       END DO
    END DO
    l=0
    DO mm=1,Mend1
       l=l+1
       la1(mm,1)=l
    END DO
    DO nn=2,Mend2
       DO mm=1,Mend+3-nn
          l=l+1
          la1(mm,nn)=l
       END DO
    END DO

    CALL epslon ()

  END SUBROUTINE CreateSpectralRep


  SUBROUTINE epslon ()

    IMPLICIT NONE

    ! array eps, which is used in a 
    ! recursion relation to calculate the 
    ! associated legendre functions.

    INTEGER :: l, mm, nn

    REAL (KIND=r8) :: am, an

    DO l=1,Mend1
       eps(l)=0.0_r8
    END DO
    l=Mend1
    DO nn=2,Mend2
       DO mm=1,Mend+3-nn
          l=l+1
          am=mm-1
          an=mm+nn-2
          eps(l)=SQRT((an*an-am*am)/(4.0_r8*an*an-1.0_r8))
       END DO
    END DO

  END SUBROUTINE epslon


  SUBROUTINE dellnp (qlnp, qdphi, qdlam)

    USE Constants, ONLY : EMRad1

    IMPLICIT NONE

    ! spectral representation of
    ! cos(lat) * grad(q) from the spectral representation
    ! of the field q, where q is natural logarithm of 
    ! surface pressure.
    ! 
    ! computes spherical harmonic coefficients of
    ! cos(lat) * grad(ln(pi))
    ! from spherical harmonic coefficients of ln(pi)
    ! used in nonlin terms a and b
    ! when grad(ln(pi)) is needed in zln and sigmadot
    ! divide by cos(lat) after expansion on lat. circle,
    ! note different truncation in gradient components.
    !     qdphi   = cos(lat) d(ln(pi)) /er*d(lat)
    !     qdlam   =          d(ln(pi)) /er*d(lon) = i*l*q

    REAL (KIND=r8), INTENT(IN ) :: qlnp (Mnwv2) ! log of surface pressure (gauss!!)
    REAL (KIND=r8), INTENT(OUT) :: qdphi(Mnwv3) ! cos * latitudinal dEMRad1ative of "q"
    REAL (KIND=r8), INTENT(OUT) :: qdlam(Mnwv2) ! longitudinal dEMRad1ative of "q" 

    INTEGER :: l, lreal, limag
    INTEGER :: nn
    INTEGER :: mm
    INTEGER :: nlast
    INTEGER :: l1
    INTEGER :: l1p
    INTEGER :: l0p
    INTEGER :: l0m
    INTEGER :: mn

    REAL (KIND=r8) :: am
    REAL (KIND=r8) :: an

    l=0
    DO nn=1,Mend1
       am=-1.0_r8
       DO mm=1,Mend2-nn
          l=l+1
          lreal=2*l-1
          limag=2*l
          am=am+1.0_r8
          qdlam(lreal)=-am*qlnp(limag)
          qdlam(limag)=am*qlnp(lreal)
       END DO
    END DO
    DO mm=1,Mend1
       am=REAL(mm-1,r8)
       an=am
       IF (mm < Mend1) THEN
          qdphi(2*mm-1)=qlnp(2*(mm+Mend1)-1)*(an+2.0_r8)*eps(mm+Mend1)
          qdphi(2*mm)=qlnp(2*(mm+Mend1))*(an+2.0_r8)*eps(mm+Mend1)
       ELSE
          qdphi(2*mm-1)=0.0_r8
          qdphi(2*mm)=0.0_r8
       END IF
       nlast=Mend+3-mm
       IF (nlast >= 4) THEN
          DO nn=2,nlast-2
             an=an+1.0_r8
             l1=la1(mm,nn)
             l1p=la1(mm,nn+1)
             l0p=la0(mm,nn+1)
             l0m=la0(mm,nn-1)
             qdphi(2*l1-1)=(an+2.0_r8)*eps(l1p)*qlnp(2*l0p-1) &
                  +(1.0_r8-an)*eps(l1)*qlnp(2*l0m-1)
             qdphi(2*l1)=(an+2.0_r8)*eps(l1p)*qlnp(2*l0p) &
                  +(1.0_r8-an)*eps(l1)*qlnp(2*l0m)
          END DO
       END IF
       IF (nlast >= 3) THEN
          nn=nlast-1
          an=an+1.0_r8
          l1=la1(mm,nn)
          l0m=la0(mm,nn-1)
          qdphi(2*l1-1)=(1.0_r8-an)*eps(l1)*qlnp(2*l0m-1)
          qdphi(2*l1)=(1.0_r8-an)*eps(l1)*qlnp(2*l0m)
       END IF
       IF (nlast >= 2) THEN
          nn=nlast
          an=an+1.0_r8
          l1=la1(mm,nn)
          l0m=la0(mm,nn-1)
          qdphi(2*l1-1)=(1.0_r8-an)*eps(l1)*qlnp(2*l0m-1)
          qdphi(2*l1)=(1.0_r8-an)*eps(l1)*qlnp(2*l0m)
       END IF
    END DO
    DO mn=1,Mnwv2
       qdlam(mn)=EMRad1*qdlam(mn)
    END DO
    DO mn=1,Mnwv3
       qdphi(mn)=EMRad1*qdphi(mn)
    END DO

  END SUBROUTINE dellnp


  SUBROUTINE dztouv (qdiv, qrot, qu, qv)

    USE Constants, ONLY : EMRad

    IMPLICIT NONE

    ! spectral representation of cosine-weighted
    ! wind components from spectral representation of
    ! vorticity and divergence.

    REAL (KIND=r8), INTENT(IN ) :: qdiv(Mnwv2,Kmax)   ! divergence
    REAL (KIND=r8), INTENT(IN ) :: qrot(Mnwv2,Kmax)   ! vorticity 
    REAL (KIND=r8), INTENT(OUT) :: qu  (Mnwv3,Kmax)   ! zonal pseudo-wind 
    REAL (KIND=r8), INTENT(OUT) :: qv  (Mnwv3,Kmax)   ! meridional pseudo-wind

    INTEGER :: l
    INTEGER :: nn
    INTEGER :: mm
    INTEGER :: k
    INTEGER :: nlast
    INTEGER :: l0
    INTEGER :: l0p
    INTEGER :: l0m
    INTEGER :: l1
    INTEGER :: l1p

    REAL (KIND=r8) :: an
    REAL (KIND=r8) :: am
    REAL (KIND=r8) :: e0(Mnwv1)
    REAL (KIND=r8) :: e1(Mnwv0)

    e0(1)=0.0_r8
    e1(1)=0.0_r8
    DO mm=2,Mend1
       e0(mm)=0.0_r8
       e1(mm)=EMRad/REAL(mm,r8)
    END DO
    l=Mend1
    DO nn=2,Mend2
       DO mm=1,Mend2-nn+1
          l=l+1
          e0(l)=EMRad*eps(l)/REAL(nn+mm-2,r8)
       END DO
    END DO
    l=Mend1
    DO nn=2,Mend1
       DO mm=1,Mend2-nn
          l=l+1
          an=nn+mm-2
          am=mm-1
          e1(l)=EMRad*am/(an+an*an)
       END DO
    END DO
    !cdir novector
    DO k=1,Kmax
       DO mm=1,Mend1
          nlast=Mend2+1-mm
          qu(2*mm-1,k)=e1(mm)*qdiv(2*mm,k)
          qu(2*mm,k)=-e1(mm)*qdiv(2*mm-1,k)
          qv(2*mm-1,k)=e1(mm)*qrot(2*mm,k)
          qv(2*mm,k)=-e1(mm)*qrot(2*mm-1,k)
          IF (nlast >= 3) THEN
             l=Mend1
             qu(2*mm-1,k)=qu(2*mm-1,k)+e0(mm+l)*qrot(2*(mm+l)-1,k)
             qu(2*mm,k)=qu(2*mm,k)+e0(mm+l)*qrot(2*(mm+l),k)
             qv(2*mm-1,k)=qv(2*mm-1,k)-e0(mm+l)*qdiv(2*(mm+l)-1,k)
             qv(2*mm,k)=qv(2*mm,k)-e0(mm+l)*qdiv(2*(mm+l),k)
          END IF
          IF (nlast >= 4) THEN
             DO nn=2,nlast-2
                l0=la0(mm,nn)
                l0p=la0(mm,nn+1)
                l0m=la0(mm,nn-1)
                l1=la1(mm,nn)
                l1p=la1(mm,nn+1)
                qu(2*l1-1,k)=-e0(l1)*qrot(2*l0m-1,k)+ &
                   e0(l1p)*qrot(2*l0p-1,k)+e1(l0)*qdiv(2*l0,k)
                qu(2*l1,k)=-e0(l1)*qrot(2*l0m,k)+ &
                   e0(l1p)*qrot(2*l0p,k)-e1(l0)*qdiv(2*l0-1,k)
                qv(2*l1-1,k)=e0(l1)*qdiv(2*l0m-1,k)- &
                   e0(l1p)*qdiv(2*l0p-1,k)+e1(l0)*qrot(2*l0,k)
                qv(2*l1,k)=e0(l1)*qdiv(2*l0m,k)- &
                   e0(l1p)*qdiv(2*l0p,k)-e1(l0)*qrot(2*l0-1,k)
             END DO
          END IF
          IF (nlast >= 3) THEN
             nn=nlast-1
             l0=la0(mm,nn)
             l0m=la0(mm,nn-1)
             l1=la1(mm,nn)
             qu(2*l1-1,k)=-e0(l1)*qrot(2*l0m-1,k)+e1(l0)*qdiv(2*l0,k)
             qu(2*l1,k)=-e0(l1)*qrot(2*l0m,k)-e1(l0)*qdiv(2*l0-1,k)
             qv(2*l1-1,k)= e0(l1)*qdiv(2*l0m-1,k)+e1(l0)*qrot(2*l0,k)
             qv(2*l1,k)= e0(l1)*qdiv(2*l0m,k)-e1(l0)*qrot(2*l0-1,k)
          END IF
          IF (nlast >= 2) THEN
             nn=nlast
             l0m=la0(mm,nn-1)
             l1=la1(mm,nn)
             qu(2*l1-1,k)=-e0(l1)*qrot(2*l0m-1,k)
             qu(2*l1,k)=-e0(l1)*qrot(2*l0m,k)
             qv(2*l1-1,k)=e0(l1)*qdiv(2*l0m-1,k)
             qv(2*l1,k)=e0(l1)*qdiv(2*l0m,k)
          END IF
       END DO
    END DO

  END SUBROUTINE dztouv


  SUBROUTINE transs (Ldim, isign, a)

    IMPLICIT NONE

    !     transp: after input, transposes scalar arrays
    !             of spectral coefficients by swapping
    !             the order of the subscripts
    !             representing the degree and order
    !             of the associated legendre functions.
    ! 
    !     argument(dimensions)        description
    ! 
    !     Ldim                 input: number of layers.
    !     a(Mnwv2,Ldim)        input: spectral representation of a
    !                                 global field at "n" levels.
    !                                 isign=+1 diagonalwise storage
    !                                 isign=-1 coluMnwise   storage
    !                         output: spectral representation of a
    !                                 global field at "n" levels.
    !                                 isign=+1 coluMnwise   storage
    !                                 isign=-1 diagonalwise storage

    INTEGER, INTENT(IN) :: Ldim
    INTEGER, INTENT(IN) :: isign

    REAL (KIND=r8), INTENT(INOUT) :: a(Mnwv2,Ldim)

    REAL (KIND=r8) :: w(Mnwv2)

    INTEGER :: k
    INTEGER :: l
    INTEGER :: lx
    INTEGER :: mn
    INTEGER :: mm
    INTEGER :: nlast
    INTEGER :: nn

    IF (isign == 1) THEN
       DO k=1,Ldim
          l=0
          DO mm=1,Mend1
             nlast=Mend2-mm
             DO nn=1,nlast
                l=l+1
                lx=la0(mm,nn)
                w(2*l-1)=a(2*lx-1,k)
                w(2*l)=a(2*lx,k)
             END DO
          END DO
          DO mn=1,Mnwv2
             a(mn,k)=w(mn)
          END DO
       END DO
    ELSE
       DO k=1,Ldim
          l=0
          DO mm=1,Mend1
             nlast=Mend2-mm
             DO nn=1,nlast
                l=l+1
                lx=la0(mm,nn)
                w(2*lx-1)=a(2*l-1,k)
                w(2*lx)=a(2*l,k)
             END DO
          END DO
          DO mn=1,Mnwv2
             a(mn,k)=w(mn)
          END DO
       END DO
    END IF

  END SUBROUTINE transs


END MODULE SpectralRep
