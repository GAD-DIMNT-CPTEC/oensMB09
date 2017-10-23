!
!  $Author: pkubota $
!  $Date: 2006/10/30 18:38:08 $
!  $Revision: 1.2 $
!
MODULE GaussSigma

  USE Constants, ONLY : r8, nfprt
  USE Parallelism, ONLY : myid
  USE Sizes, ONLY : Kmax, ibmax, jbmax, ibmaxperjb

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

    REAL (KIND=r8), INTENT(IN   ) :: dphi(Ibmax,Jbmax)
    REAL (KIND=r8), INTENT(IN   ) :: dlam(Ibmax,Jbmax)
    REAL (KIND=r8), INTENT(IN   ) :: ug  (Ibmax,Kmax,Jbmax)
    REAL (KIND=r8), INTENT(IN   ) :: vg  (Ibmax,Kmax,Jbmax)
    REAL (KIND=r8), INTENT(IN   ) :: dg  (Ibmax,Kmax,Jbmax)
    REAL (KIND=r8), INTENT(IN   ) :: rcl (Ibmax,Jbmax)
    REAL (KIND=r8), INTENT(OUT  ) :: tau (Ibmax,Kmax,Jbmax)
    REAL (KIND=r8), INTENT(IN   ) :: ps  (Ibmax,Jbmax)

    INTEGER :: i,j,k

    REAL (KIND=r8) :: cg (Ibmax,Kmax)
    REAL (KIND=r8) :: db (Ibmax,Kmax)
    REAL (KIND=r8) :: cb (Ibmax,Kmax)
    REAL (KIND=r8) :: dot(Ibmax,Kmax+1)

    ! Compute c=v(true)*Del(ln(Ps)).Divide by Cos for Del Cos for v
    ! Tau Contains Contrib. to Omega in Layers

    dot=0.0_r8
    DO j=1,Jbmax
       DO k=1,Kmax
          DO i=1,Ibmaxperjb(j)
            cg(i,k)=rcl(i,j)*(ug(i,k,j)*dlam(i,j)+vg(i,k,j)*dphi(i,j))
          END DO
       END DO
       DO i=1,Ibmaxperjb(j)
          db(i,1)=Del(1)*dg(i,1,j)
          cb(i,1)=Del(1)*cg(i,1)
       END DO
       DO k=1,Kmax-1
          DO i=1,Ibmaxperjb(j)
             db(i,k+1)=db(i,k)+Del(k+1)*dg(i,k+1,j)
             cb(i,k+1)=cb(i,k)+Del(k+1)*cg(i,k+1)
          END DO
       END DO

    ! Sigma Dot Computed Only at Interior Interfaces

       DO k=1,Kmax-1
          DO i=1,Ibmaxperjb(j)
             dot(i,k+1)=dot(i,k)+Del(k)*(db(i,Kmax)+ &
                             cb(i,Kmax)-dg(i,k,j)-cg(i,k))
          END DO
       END DO
       DO k =1,Kmax
          DO i=1,Ibmaxperjb(j)
             tau(i,k,j)=Sl(k)*(cg(i,k)-cb(i,Kmax)-db(i,Kmax))- &
                        0.5_r8*(dot(i,k+1)+dot(i,k))
             tau(i,k,j)=tau(i,k,j)*ps(i,j)
          END DO
       END DO
    END DO

  END SUBROUTINE Omegas


  SUBROUTINE pWater (jjsh, Pw, Psmb)

    USE Constants, ONLY : CvMbPa, Grav

    IMPLICIT NONE

    ! Multiply Matrix jjsh by Vector Del and Scales Results by Psmb

    REAL (KIND=r8), INTENT(IN)  :: jjsh(Ibmax,Kmax,Jbmax)
    REAL (KIND=r8), INTENT(OUT) :: Pw(Ibmax,Jbmax)
    REAL (KIND=r8), INTENT(IN)  :: Psmb(Ibmax,Jbmax)

    INTEGER :: i, k, j

    REAL (KIND=r8) :: Fac

    Fac=CvMbPa/Grav
    Pw=0.0_r8
    DO j=1,Jbmax
       DO k=1,Kmax
          DO i=1,Ibmaxperjb(j)
             Pw(i,j)=Pw(i,j)+jjsh(i,k,j)*Del(k)
          END DO
       END DO
    END DO

    DO j=1,Jbmax
       DO i=1,Ibmaxperjb(j) 
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

    if(myid.eq.0) WRITE (UNIT=nfprt, FMT='(/,A,/)') ' Begin SetSig '

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
    if(myid.ne.0) return
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
