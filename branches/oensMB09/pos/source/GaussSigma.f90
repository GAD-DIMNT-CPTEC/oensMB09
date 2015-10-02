!
!  $Author: pkubota $
!  $Date: 2006/10/30 18:38:08 $
!  $Revision: 1.2 $
!
MODULE GaussSigma

  USE Constants, ONLY : r8, nfprt
  USE PrblSize, ONLY : Imax, Jmax, Kmax

  IMPLICIT NONE

  PRIVATE

  REAL (KIND=r8), ALLOCATABLE, DIMENSION(:), PUBLIC :: Si, Sl, Del

  PUBLIC :: CreateGaussSigma, Omegas, pWater, SetSig


CONTAINS


  SUBROUTINE CreateGaussSigma()

    IMPLICIT NONE

    ALLOCATE (Si(Kmax+1), Sl(Kmax), Del(Kmax))

  END SUBROUTINE CreateGaussSigma


  SUBROUTINE Omegas (dphi, dlam, ug, vg, dg, rcl, tau, ps)

    IMPLICIT NONE

    REAL (KIND=r8), INTENT(INOUT) :: dphi(Imax,Jmax)
    REAL (KIND=r8), INTENT(INOUT) :: dlam(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN   ) :: ug  (Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN   ) :: vg  (Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN   ) :: dg  (Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN   ) :: rcl (Jmax)
    REAL (KIND=r8), INTENT(OUT  ) :: tau (Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN   ) :: ps  (Imax,Jmax)

    INTEGER :: i,j,k

    REAL (KIND=r8) :: cg (Imax,Jmax,Kmax)
    REAL (KIND=r8) :: db (Imax,Jmax,Kmax)
    REAL (KIND=r8) :: cb (Imax,Jmax,Kmax)
    REAL (KIND=r8) :: dot(Imax,Jmax,Kmax+1)

    ! Compute c=v(true)*Del(ln(Ps)).Divide by Cos for Del Cos for v
    ! Tau Contains Contrib. to Omega in Layers

    dot=0.0_r8
    DO j=1,Jmax
       DO i=1,Imax
          dphi(i,j)=dphi(i,j)*rcl(j)
          dlam(i,j)=dlam(i,j)*rcl(j)
       END DO
    END DO
    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             cg(i,j,k)=ug(i,j,k)*dlam(i,j)+vg(i,j,k)*dphi(i,j)
          END DO
       END DO
    END DO
    DO j=1,Jmax
       DO i=1,Imax
          db(i,j,1)=Del(1)*dg(i,j,1)
          cb(i,j,1)=Del(1)*cg(i,j,1)
       END DO
    END DO
    DO k=1,Kmax-1
       DO j=1,Jmax
          DO i=1,Imax
             db(i,j,k+1)=db(i,j,k)+Del(k+1)*dg(i,j,k+1)
             cb(i,j,k+1)=cb(i,j,k)+Del(k+1)*cg(i,j,k+1)
          END DO
       END DO
    END DO

    ! Sigma Dot Computed Only at Interior Interfaces

    DO k=1,Kmax-1
       DO j=1,Jmax
          DO i=1,Imax
             dot(i,j,k+1)=dot(i,j,k)+Del(k)*(db(i,j,Kmax)+ &
                             cb(i,j,Kmax)-dg(i,j,k)-cg(i,j,k))
          END DO
       END DO
    END DO
    DO k =1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             tau(i,j,k)=Sl(k)*(cg(i,j,k)-cb(i,j,Kmax)-db(i,j,Kmax))- &
                        0.5_r8*(dot(i,j,k+1)+dot(i,j,k))
             tau(i,j,k)=tau(i,j,k)*ps(i,j)
          END DO
       END DO
    END DO

  END SUBROUTINE Omegas


  SUBROUTINE pWater (Imx, Imax, Kmax, Jmax, jjsh, Pw, Psmb)

    USE Constants, ONLY : CvMbPa, Grav

    IMPLICIT NONE

    ! Multiply Matrix jjsh by Vector Del and Scales Results by Psmb

    INTEGER, INTENT(IN)  :: Imx
    INTEGER, INTENT(IN)  :: Imax
    INTEGER, INTENT(IN)  :: Kmax
    INTEGER, INTENT(IN)  :: Jmax

    REAL (KIND=r8), INTENT(IN)  :: jjsh(Imx,Jmax,Kmax)
    REAL (KIND=r8), INTENT(OUT) :: Pw(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN)  :: Psmb(Imax,Jmax)

    INTEGER :: i, k, j

    REAL (KIND=r8) :: Fac

    Fac=CvMbPa/Grav
    Pw=0.0_r8
    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             Pw(i,j)=Pw(i,j)+jjsh(i,j,k)*Del(k)
          END DO
       END DO
    END DO

    DO j=1,Jmax
       DO i=1,Imax 
          Pw(i,j)=Pw(i,j)*Psmb(i,j)*Fac
       END DO
    END DO

  END SUBROUTINE pWater


  SUBROUTINE SetSig (dDel)

    USE Constants, ONLY : RdByCp, RdByCp1

    IMPLICIT NONE

    ! Calculates Quantities Related to the 
    ! Discretization of the Sigma Coordinate Axis
    ! 
    ! Del(Kmax)  : sigma spacing for each layer
    ! Ci(Kmax+1) : sigma value at each level
    ! Si(Kmax+1) : si(l)=1.0-Ci(l)
    ! Sl(Kmax)   : sigma value at midpoint of each layer
    !
    ! k=Rd/Cp=287.05/1004.6
    ! 
    !                                    1
    !         +-                      + ---
    !         z      k+1          k+1 z  k
    !         z Si(l)    - Si(l+1)    z
    ! Sl(l) = z --------------------- z
    !         z (k+1) (Si(l)-Si(l+1)) z
    !         +-                     -+
    ! 

    REAL (KIND=r8), INTENT(IN) :: dDel(Kmax)

    INTEGER :: k

    REAL (KIND=r8) :: SumDel
    REAL (KIND=r8) :: SiRdByCp
    REAL (KIND=r8) :: SiRdByCp1
    REAL (KIND=r8) :: Dif
    REAL (KIND=r8) :: Ci(Kmax+1)

    Del=dDel
    SumDel=SUM(Del)

    WRITE (UNIT=nfprt, FMT='(/,A,/)') ' Begin SetSig '

    Ci(1)=0.0_r8
    DO k=1,Kmax
       SumDel=SumDel+Del(k)
       Ci(k+1)=Ci(k)+Del(k)
    END DO
    Ci(Kmax+1)=1.0_r8

    DO k=1,Kmax+1
       Si(k)=1.0_r8-Ci(k)
    END DO

    DO k=1,Kmax
       ! Dif=Si(k)**RdByCp1-Si(k+1)**RdByCp1
       SiRdByCp=EXP(RdByCp1*LOG(Si(k)))
       IF (k <= Kmax-1) THEN
          SiRdByCp1=EXP(RdByCp1*LOG(Si(k+1)))
       ELSE
          SiRdByCp1=0.0_r8
       END IF
       Dif=SiRdByCp-SiRdByCp1
       Dif=Dif/(RdByCp1*(Si(k)-Si(k+1)))
       ! Sl(k)=Dif**(1.0_r8/RdByCp)
       Sl(k)=EXP(LOG(Dif)/RdByCp)
    END DO

    DO k=1,Kmax+1
       WRITE (UNIT=nfprt, FMT='(A,I2,2(A,F12.8))') &
             ' Level = ', k, '  Ci = ', Ci(k), ' Si = ', Si(k)
    END DO
    WRITE (UNIT=nfprt, FMT='(/)')
    DO k=1,Kmax
       WRITE (UNIT=nfprt, FMT='(A,I2,2(A,F12.8))') &
             ' Layer = ', k, '  Sl = ', Sl(k), '  Del = ', Del(k)
    END DO
    WRITE (UNIT=nfprt, FMT='(/,A,I3,A,1PG16.8,/)') &
          ' Kmax = ', Kmax, ' SUM DelSig = ', SumDel

  END SUBROUTINE SetSig


END MODULE GaussSigma
