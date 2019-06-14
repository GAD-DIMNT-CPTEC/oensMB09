!
!  $Author: pkubota $
!  $Date: 2006/10/30 18:41:06 $
!  $Revision: 1.3 $
!
MODULE SigmaToPressure

  USE Constants, ONLY : r8, nferr
  USE PrblSize, ONLY : Imax, Imx, Jmax, Kmax, Lmax
  USE GaussSigma, ONLY : si, sl, del

  IMPLICIT NONE

  !     subprogram  documentation  block
  ! 
  !     subprogram: sigtop         sigma to pressure calculation
  !     author: j. sela            org: w/nmc42    date: 18 nov 83
  ! 
  !     abstract: interpolates values on the gaussian grid from the
  !     sigma coordinate system to the mandatory pressure levels.
  !     assumes that relative humidity, temperature, horizontal
  !     wind components and vertical velocity vary in the vertical
  !     with the log of pressure. obtains height at pressure levels
  !     by integrating the hydrostatic equation.
  ! 
  !     usage:  call sigtop (ts, psmb, top, tvp, zp, si, alnpmd)
  ! 
  !     input variables:
  ! 
  !     names       meaning/content/purpose/units/type        interface
  !     -----       ----------------------------------        ---------
  ! 
  !     ts          real array of absolute tvirt (k)          arg list
  !                 in sigma layers.
  !     psmb        real array of surface pressure            arg list
  !                 (milibars)
  !     top         real array of terrain height (m)          arg list
  !     si          real array of dimensionless values        arg list
  !                 used to define p at interface of
  !                 model layers.
  !     alnpmd      real array of dimensionless values        arg list
  !                 of log(pmand).
  ! 
  !     output variables:
  ! 
  !     names       meaning/content/purpose/units/type        interface
  !     -----       ----------------------------------        ---------
  ! 
  !     tvp         real array of virtual temperature on      arg list
  !                 mandatory pressure levels. (k)
  ! 
  !     zp          real array of geopotential height on      arg list
  !                 mandatory pressure levels. (m)
  ! 
  !     attributes: language: fortran

  PRIVATE

  PUBLIC :: sig2po, sig2pz, sigtop, gavint


CONTAINS


  SUBROUTINE sig2po (psmb, alnpmd, bfi1, bfo1, bfi2, bfo2, bfi3, bfo3)

    USE Constants, ONLY : Po, Pt, RdByCp, CpByRd

    IMPLICIT NONE

    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)
    REAL (KIND=r8), INTENT(IN) :: bfi1(Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(OUT) :: bfo1(Imax,Jmax,Lmax)

    REAL (KIND=r8), INTENT(IN), OPTIONAL :: bfi2(Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(OUT), OPTIONAL :: bfo2(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(IN), OPTIONAL :: bfi3(Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(OUT), OPTIONAL :: bfo3(Imax,Jmax,Lmax)

    REAL (KIND=r8) :: wtl

    INTEGER :: nb, i, j, k, kp
    INTEGER :: ks(Imax,Jmax,Lmax)

    REAL (KIND=r8) :: ddp(Imax,Jmax,Lmax)
    REAL (KIND=r8) :: pim(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: alnpm(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: alnp(Imax,Jmax,Kmax+1)
    REAL (KIND=r8) :: pi (Imax,Jmax,Kmax+1)
    REAL (KIND=r8) :: p(Imax,Jmax,Kmax+1)
    REAL (KIND=r8) :: rdelp(Imax,Jmax,Kmax-1)
    REAL (KIND=r8) :: b1(Imax,Jmax,Kmax-1)
    REAL (KIND=r8) :: b2(Imax,Jmax,Kmax-1)
    REAL (KIND=r8) :: b3(Imax,Jmax,Kmax-1)

    ! initialization

    nb=1
    IF (PRESENT(bfi2)) THEN
       IF (PRESENT(bfo2)) THEN
          nb=2
       ELSE
          WRITE (UNIT=nferr, FMT='(A)') ' sig2po: bfo2 required '
          STOP 9001
       END IF
       IF (PRESENT(bfi3)) THEN
          IF (PRESENT(bfo3)) THEN
             nb=3
          ELSE
             WRITE (UNIT=nferr, FMT='(A)') ' sig2po: bfo3 required '
             STOP 9001
          END IF
       END IF
    END IF

    wtl=LOG(Po)

    ! compute pressure on each sigma surface 
    ! set the highest to Pt (See Constants)

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             p(i,j,k)=si(k)*psmb(i,j)
          END DO
       END DO
    END DO
    p(:,:,Kmax+1)=Pt

    ! log of pressure and exner function on each sigma surface.

    DO k=1,Kmax+1
       DO j=1,Jmax
          DO i=1,Imax
             alnp(i,j,k)=LOG(p(i,j,k))
             pi(i,j,k)=EXP(RdByCp*(alnp(i,j,k)-wtl))
          END DO
       END DO
    END DO

    ! mean value of the exner function,
    ! log of pim and 
    ! log of the mean value of pressure 
    ! in each layer

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             pim(i,j,k)=(pi(i,j,k+1)-pi(i,j,k))/ &
                  (RdByCp*(alnp(i,j,k+1)-alnp(i,j,k)))
             alnpm(i,j,k)=wtl+CpByRd*LOG(pim(i,j,k))
          END DO
       END DO
    END DO

    ! interpolated t absolute. ts replaced by tm

    IF (nb == 3) THEN
       DO k=1,Kmax-1
          DO j=1,Jmax
             DO i=1,Imax
                rdelp(i,j,k)=1.0_r8/(alnpm(i,j,k+1)-alnpm(i,j,k))
                b1(i,j,k)=(bfi1(i,j,k+1)-bfi1(i,j,k))*rdelp(i,j,k)
                b2(i,j,k)=(bfi2(i,j,k+1)-bfi2(i,j,k))*rdelp(i,j,k)
                b3(i,j,k)=(bfi3(i,j,k+1)-bfi3(i,j,k))*rdelp(i,j,k)
             END DO
          END DO
       END DO
    ELSE IF (nb == 2) THEN
       DO k=1,Kmax-1
          DO j=1,Jmax
             DO i=1,Imax
                rdelp(i,j,k)=1.0_r8/(alnpm(i,j,k+1)-alnpm(i,j,k))
                b1(i,j,k)=(bfi1(i,j,k+1)-bfi1(i,j,k))*rdelp(i,j,k)
                b2(i,j,k)=(bfi2(i,j,k+1)-bfi2(i,j,k))*rdelp(i,j,k)
             END DO
          END DO
       END DO
    ELSE 
       DO k=1,Kmax-1
          DO j=1,Jmax
             DO i=1,Imax
                rdelp(i,j,k)=1.0_r8/(alnpm(i,j,k+1)-alnpm(i,j,k))
                b1(i,j,k)=(bfi1(i,j,k+1)-bfi1(i,j,k))*rdelp(i,j,k)
             END DO
          END DO
       END DO
    END IF

    ! heights to pressure surfaces  -  hydrostatic interpolation
    ! winds and temps by linear interpolation with ln(p)
    ! search for middle of sigma layer above kp

    ks(:,:,:)=Kmax
    DO kp=1,Lmax
       DO k=1,Kmax-1
          DO j=1,Jmax
             DO i=1,Imax
                IF ((ks(i,j,kp) == Kmax) .AND. (alnpmd(kp) > alnpm(i,j,k))) THEN
                   ks(i,j,kp)=k
                END IF
             END DO
          END DO
       END DO
    END DO

    ! find values and slopes for upward or downward extrapolation
    ! as well as for interpolation away from tropopause

    IF (nb == 3) THEN
       DO kp=1,Lmax
          DO j=1,Jmax
             DO i=1,Imax
                IF (ks(i,j,kp) /= 1) THEN
                   ddp(i,j,kp)=alnpmd(kp)-alnpm(i,j,ks(i,j,kp))
                   bfo1(i,j,kp)=bfi1(i,j,ks(i,j,kp))+b1(i,j,ks(i,j,kp)-1)*ddp(i,j,kp)
                   bfo2(i,j,kp)=bfi2(i,j,ks(i,j,kp))+b2(i,j,ks(i,j,kp)-1)*ddp(i,j,kp)
                   bfo3(i,j,kp)=bfi3(i,j,ks(i,j,kp))+b3(i,j,ks(i,j,kp)-1)*ddp(i,j,kp)
                ELSE
                   bfo1(i,j,kp)=bfi1(i,j,1)
                   bfo2(i,j,kp)=bfi2(i,j,1)
                   bfo3(i,j,kp)=bfi3(i,j,1)
                END IF
             END DO
          END DO
       END DO
    ELSE IF (nb == 2) THEN
       DO kp=1,Lmax
          DO j=1,Jmax
             DO i=1,Imax
                IF (ks(i,j,kp) /= 1) THEN
                   ddp(i,j,kp)=alnpmd(kp)-alnpm(i,j,ks(i,j,kp))
                   bfo1(i,j,kp)=bfi1(i,j,ks(i,j,kp))+b1(i,j,ks(i,j,kp)-1)*ddp(i,j,kp)
                   bfo2(i,j,kp)=bfi2(i,j,ks(i,j,kp))+b2(i,j,ks(i,j,kp)-1)*ddp(i,j,kp)
                ELSE
                   bfo1(i,j,kp)=bfi1(i,j,1)
                   bfo2(i,j,kp)=bfi2(i,j,1)
                END IF
             END DO
          END DO
       END DO
    ELSE
       DO kp=1,Lmax
          DO j=1,Jmax
             DO i=1,Imax
                IF (ks(i,j,kp) /= 1) THEN
                   ddp(i,j,kp)=alnpmd(kp)-alnpm(i,j,ks(i,j,kp))
                   bfo1(i,j,kp)=bfi1(i,j,ks(i,j,kp))+b1(i,j,ks(i,j,kp)-1)*ddp(i,j,kp)
                ELSE
                   bfo1(i,j,kp)=bfi1(i,j,1)
                END IF
             END DO
          END DO
       END DO
    END IF

  END SUBROUTINE sig2po


  SUBROUTINE sig2pz (ts, psmb, top, tvp, zp, alnpmd)

    USE Constants, ONLY : Po, Pt, RdByCp, RdByGrav, CpByRd, &
                          Tref, Zref, TVVTa, TVVTb

    IMPLICIT NONE

    REAL (KIND=r8), INTENT(IN) :: ts(Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN) :: top(Imax,Jmax)
    REAL (KIND=r8), INTENT(OUT) :: tvp(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: zp(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)

    INTEGER :: i, j, k, kp
    INTEGER :: ks(Imax,Jmax,Lmax)

    REAL (KIND=r8) :: wtl, ddp, hmh, part

    REAL (KIND=r8), DIMENSION(Imax,Jmax) :: gamma, tmsl, tstr, zlay
    REAL (KIND=r8), DIMENSION(Imax,Jmax,Kmax) :: pim, alnpm, dh, bz, zt
    REAL (KIND=r8), DIMENSION(Imax,Jmax,Kmax+1) :: alnp, pi, p, z, tlev, balnp
    REAL (KIND=r8), DIMENSION(Imax,Jmax,Kmax-1) :: rdelp, btv

    ! constants

    wtl=LOG(Po)

    ! compute pressure on each sigma surface and
    ! set the top of the model at Pt (See Constants)

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             p(i,j,k)=si(k)*psmb(i,j)
          END DO
       END DO
    END DO
    p(:,:,Kmax+1)=Pt

    ! compute the log of pressure and exner function on each sigma surface

    DO k=1,Kmax+1
       DO j=1,Jmax
          DO i=1,Imax
             alnp(i,j,k)=LOG(p(i,j,k))
             pi(i,j,k)=EXP(RdByCp*(alnp(i,j,k)-wtl))
          END DO
       END DO
    END DO

    ! compute the mean value of the exner function and
    ! the log of the mean value of pressure in each layer.

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             pim(i,j,k)=(pi(i,j,k+1)-pi(i,j,k))/ &
                  (RdByCp*(alnp(i,j,k+1)-alnp(i,j,k)))
             alnpm(i,j,k)=wtl+CpByRd*LOG(pim(i,j,k))
          END DO
       END DO
    END DO

    ! compute the height of the sigma surfaces.

    DO j=1,Jmax
       DO i=1,Imax
          z(i,j,1)=top(i,j)
       END DO
    END DO
    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             z(i,j,k+1)=z(i,j,k)+RdByGrav*ts(i,j,k)* &
                                    (alnp(i,j,k)-alnp(i,j,k+1))
          END DO
       END DO
    END DO

    ! compute the temperature on sigma surfaces. model the temp
    ! profile as linear in log of pressure. extrapolate to determine
    ! the value of temp at the highest and lowest sigma surface.

    DO k=2,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             tlev(i,j,k)=((alnp(i,j,k)-alnpm(i,j,k))*ts(i,j,k-1)+ &
                          (alnpm(i,j,k-1)-alnp(i,j,k))*ts(i,j,k))/ &
                          (alnpm(i,j,k-1)-alnpm(i,j,k))
          END DO
       END DO
    END DO
    DO j=1,Jmax
       DO i=1,Imax
          tlev(i,j,1)=((alnp(i,j,1)-alnpm(i,j,2))*ts(i,j,1)+ &
                          (alnpm(i,j,1)-alnp(i,j,1))*ts(i,j,2))/ &
                          (alnpm(i,j,1)-alnpm(i,j,2))
          tlev(i,j,Kmax+1)= &
               ((alnp(i,j,Kmax+1)-alnpm(i,j,Kmax))*ts(i,j,Kmax-1)+ &
               (alnpm(i,j,Kmax-1)-alnp(i,j,Kmax+1))*ts(i,j,Kmax))/ &
               (alnpm(i,j,Kmax-1)-alnpm(i,j,Kmax))
       END DO
    END DO
    DO j=1,Jmax
       DO i=1,Imax
          zlay(i,j)=0.5_r8*(z(i,j,1)+z(i,j,2))
          tstr(i,j)=ts(i,j,1)+TVVTa*(zlay(i,j)-z(i,j,1))
          tmsl(i,j)=ts(i,j,1)+TVVTa*zlay(i,j)
          gamma(i,j)=0.0_r8
          IF (tmsl(i,j) > Tref) THEN
             tmsl(i,j)=Tref
             IF (tstr(i,j) > Tref) tmsl(i,j)=Tref- &
                  TVVTb*(tstr(i,j)-Tref)*(tstr(i,j)-Tref)
          END IF
          IF (z(i,j,1) > Zref) gamma(i,j)=(tstr(i,j)-tmsl(i,j))/z(i,j,1)
       END DO
    END DO
    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             dh(i,j,k)=alnp(i,j,k+1)-alnp(i,j,k)
             bz(i,j,k)=RdByGrav*(tlev(i,j,k+1)-tlev(i,j,k))/dh(i,j,k)
             zt(i,j,k)=0.5_r8*(z(i,j,k+1)+z(i,j,k))+ &
                  0.125_r8*bz(i,j,k)*dh(i,j,k)*dh(i,j,k)
             balnp(i,j,k)=0.5_r8*(alnp(i,j,k+1)+alnp(i,j,k))
          END DO
       END DO
    END DO
    DO k=1,Kmax-1
       DO j=1,Jmax
          DO i=1,Imax
             rdelp(i,j,k)=1.0_r8/(alnpm(i,j,k+1)-alnpm(i,j,k))
             btv(i,j,k)=(ts(i,j,k+1)-ts(i,j,k))*rdelp(i,j,k)
          END DO
       END DO
    END DO

    ! heights to pressure surfaces  -  hydrostatic interpolation
    ! winds and temps by linear interpolation with ln(p).
    ! search for middle of sigma layer above kp
    ! if fall thru must extrapolate up

    ks(:,:,:) = Kmax
    DO kp=1,Lmax
       DO k=1,Kmax-1
          DO j=1,Jmax
             DO i=1,Imax
                IF ((ks(i,j,kp) == Kmax) .AND. (alnpmd(kp) > alnpm(i,j,k))) THEN
                   ks(i,j,kp) = k
                END IF
             END DO
          END DO
       END DO
    END DO

    ! find values and slopes for upward or downward extrapolation
    ! as well as for interpolation away from tropopause

    DO kp=1,Lmax
       DO j=1,Jmax
          DO i=1,Imax
             IF (ks(i,j,kp) /= 1) THEN
                ddp=alnpmd(kp)-alnpm(i,j,ks(i,j,kp))
                tvp(i,j,kp)=ts(i,j,ks(i,j,kp))+btv(i,j,ks(i,j,kp)-1)*ddp
             ELSE
                tvp(i,j,kp)=ts(i,j,1)+btv(i,j,1)*(alnpmd(kp)-alnpm(i,j,1))
             END IF
          END DO
       END DO
    END DO

    ! start with another search thru the sigma levels
    ! to find the level (ks)  above the desires mandatory level (kp)

    ks(:,:,:) = Kmax+1
    DO kp=1,Lmax
       DO k=1,Kmax
          DO j=1,Jmax
             DO i=1,Imax
                IF ((ks(i,j,kp) == Kmax+1) .AND. (alnpmd(kp) > alnp(i,j,k))) THEN
                   ks(i,j,kp) = k
                END IF
             END DO
          END DO
       END DO
    END DO
    DO kp=1,Lmax
       DO j=1,Jmax
          DO i=1,Imax
             IF (ks(i,j,kp) == 1) THEN
                ! zp(i,kp) is below ground.
                part=RdByGrav*(alnp(i,j,1)-alnpmd(kp))
                zp(i,j,kp)=z(i,j,1)+tstr(i,j)*part/(1.0_r8-0.5_r8*part*gamma(i,j))
             ELSE
                ! zp references a pressure level in the free air
                hmh=alnpmd(kp)-balnp(i,j,ks(i,j,kp)-1)
                zp(i,j,kp)=zt(i,j,ks(i,j,kp)-1)- &
                     (RdByGrav*ts(i,j,ks(i,j,kp)-1)+ &
                     0.5_r8*bz(i,j,ks(i,j,kp)-1)*hmh)*hmh
             END IF
          END DO
       END DO
    END DO

  END SUBROUTINE sig2pz


  SUBROUTINE sigtop (tm, gts, gsh, gss, psmb, tg, rg, pmand, alnpmd)

    USE Constants, ONLY : Po, Pt, CTv, a, b, To, Eo, eps, eps1, &
                          RdByCp, RdByGrav, CpByRd, PRHcut, RHmin, RHmax, SHmin

    IMPLICIT NONE

    REAL (KIND=r8), INTENT(OUT) :: tm(Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN) :: gts(Imx,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN) :: gsh(Imx,Jmax,Kmax)
    REAL (KIND=r8), INTENT(OUT) :: gss(Imx,Jmax,Kmax)
    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(OUT) :: tg(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: rg(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(IN) :: pmand (Lmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)

    INTEGER :: i, j, k
    INTEGER :: kp
    INTEGER :: ks(Imax,Jmax,Lmax)

    REAL (KIND=r8) :: ddp(Imax,Jmax,Lmax)
    REAL (KIND=r8) :: pim(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: alnpm(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: alnp(Imax,Jmax,Kmax+1)
    REAL (KIND=r8) :: pi(Imax,Jmax,Kmax+1)
    REAL (KIND=r8) :: p(Imax,Jmax,Kmax+1)
    REAL (KIND=r8) :: rdelp(Imax,Jmax,Kmax-1)
    REAL (KIND=r8) :: brh(Imax,Jmax,Kmax-1)
    REAL (KIND=r8) :: bt(Imax,Jmax,Kmax-1)

    REAL (KIND=r8) :: es
    REAL (KIND=r8) :: ee
    REAL (KIND=r8) :: wtl

    wtl=LOG(Po)

    ! compute absolute temperature t=tv/(1.0_r8+0.61_r8*q)

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             tm(i,j,k)=gts(i,j,k)/(1.0_r8+CTv*gsh(i,j,k))
          END DO
       END DO
    END DO

    ! vapour pressure form specific humidity

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             gss(i,j,k)=MAX(gsh(i,j,k),SHmin)
             es=Eo*EXP(a*(tm(i,j,k)-To)/(tm(i,j,k)-b))
             ee=sl(k)*psmb(i,j)*gss(i,j,k)/(eps+eps1*gss(i,j,k))
             gss(i,j,k)=ee/es
             gss(i,j,k)=MIN(gss(i,j,k),1.0_r8)
          END DO
       END DO
    END DO

    ! compute pressure on each sigma surface except the highest.

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             p(i,j,k)=si(k)*psmb(i,j)
          END DO
       END DO
    END DO

    ! set the top of the model at Pt (See Constants).

    p(:,:,Kmax+1)=Pt

    ! log of pressure and exner function on each sigma surface

    DO k=1,Kmax+1
       DO j=1,Jmax
          DO i=1,Imax
             alnp(i,j,k)=LOG(p(i,j,k))
             pi(i,j,k)=EXP(RdByCp*(alnp(i,j,k)-wtl))
          END DO
       END DO
    END DO

    ! mean value of the exner function, log of pim and
    ! the log of the mean value of pressure in each layer

    DO k=1,Kmax
       DO j=1,Jmax
          DO i=1,Imax
             pim(i,j,k)=(pi(i,j,k+1)-pi(i,j,k))/ &
                  (RdByCp*(alnp(i,j,k+1)-alnp(i,j,k)))
             alnpm(i,j,k)=wtl+CpByRd*LOG(pim(i,j,k))
          END DO
       END DO
    END DO

    ! return interpolated t absolute. ts replaced by tm

    DO k=1,Kmax-1
       DO j=1,Jmax
          DO i=1,Imax
             rdelp(i,j,k)=1.0_r8/(alnpm(i,j,k+1)-alnpm(i,j,k))
             bt(i,j,k)=(tm(i,j,k+1)-tm(i,j,k))*rdelp(i,j,k)
          END DO
       END DO
    END DO
    DO k=1,Kmax-1
       DO j=1,Jmax
          DO i=1,Imax
             brh(i,j,k)=(gss(i,j,k+1)-gss(i,j,k))*rdelp(i,j,k)
          END DO
       END DO
    END DO

    ! heights to pressure surfaces  -  hydrostatic interpolation
    ! winds and temps by linear interpolation with ln(p)
    ! search for middle of sigma layer above kp

    ks(:,:,:)=Kmax
    DO kp=1,Lmax
       DO k=1,Kmax-1
          DO j=1,Jmax
             DO i=1,Imax
                IF ((ks(i,j,kp) == Kmax) .AND. (alnpmd(kp) > alnpm(i,j,k))) THEN
                   ks(i,j,kp)=k
                END IF
             END DO
          END DO
       END DO
    END DO

    ! find values and slopes for upward or downward extrapolation
    ! as well as for interpolation away from tropopause
    ! ts to tm for absolute t

    DO kp=1,Lmax
       DO j=1,Jmax
          DO i=1,Imax
             IF (ks(i,j,kp) /= 1) THEN
                IF (pmand(kp) < PRHcut) THEN
                   rg(i,j,kp)=0.0_r8
                ELSE
                   IF (ks(i,j,kp) <= Kmax) THEN
                      rg(i,j,kp)=gss(i,j,ks(i,j,kp))+&
                           brh(i,j,ks(i,j,kp)-1)*(alnpmd(kp)-alnpm(i,j,ks(i,j,kp)))
                   ELSE
                      rg(i,j,kp)=gss(i,j,Kmax)
                   END IF
                END IF
                ddp(i,j,kp)=alnpmd(kp)-alnpm(i,j,ks(i,j,kp))
                tg(i,j,kp)=tm(i,j,ks(i,j,kp))+bt(i,j,ks(i,j,kp)-1)*ddp(i,j,kp)
             ELSE
                rg(i,j,kp)=gss(i,j,1)
                tg(i,j,kp)=tm(i,j,1)
             END IF
             rg(i,j,kp)=MAX(rg(i,j,kp),RHmin)
             rg(i,j,kp)=MIN(rg(i,j,kp),RHmax)
          END DO
       END DO
    END DO

  END SUBROUTINE sigtop


  SUBROUTINE gavint (nlevs, nlevr, gausin, gauout, psmb, pmand)

    IMPLICIT NONE

    ! vertical interpolation of gaussian grid fields 

    INTEGER, INTENT(IN) :: nlevs
    INTEGER, INTENT(IN) :: nlevr

    REAL (KIND=r8), INTENT(INOUT) :: gausin(Imax,Jmax,Kmax)
    REAL (KIND=r8), INTENT(OUT) :: gauout(Imax,Jmax,nlevr)
    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN) :: pmand(Lmax)

    INTEGER :: k
    INTEGER :: j
    INTEGER :: i
    INTEGER :: lat
    INTEGER :: int

    REAL (KIND=r8) :: deltap
    REAL (KIND=r8) :: df
    REAL (KIND=r8) :: dp
    REAL (KIND=r8) :: work(Imax,Lmax)
    REAL (KIND=r8) :: pin(Imax,Kmax)

    LOGICAL :: Above(Imax,Lmax)

    gauout=0.0_r8
    IF (nlevs > 1 .AND. nlevs < Kmax) THEN
       DO k=nlevs+1,Kmax
          DO j=1,Jmax
             DO i=1,Imax
                gausin(i,j,k)=0.0_r8
             END DO
          END DO
       END DO
    END IF

    DO lat=1,Jmax

       ! single level inputs

       IF (nlevs ==  1) THEN
          DO i=1,Imax
             work(i,1)=gausin(i,lat,1)
          END DO

          ! vertical interpolation

       ELSE
          DO j=1,Kmax
             DO i=1,Imax
                pin(i,j)=sl(j)*psmb(i,lat)
             END DO
          END DO

          Above = .FALSE.
          DO k=1,Lmax
             DO i=1,Imax
                IF (pmand(k) > pin(i,1)) THEN
                   work(i,k)=gausin(i,lat,1)
                   Above(i,k)=.TRUE.
                END IF
             END DO
          END DO

          DO k=1,Lmax
             DO i=1,Imax
                IF (.NOT. Above(i,k) .AND. pmand(k) <= pin(i,Kmax)) THEN
                   work(i,k)=gausin(i,lat,Kmax)
                   Above(i,k)=.TRUE.
                END IF
             END DO
          END DO

          DO k=1,Lmax
             DO int=1,Kmax-1
                DO i=1,Imax
                   IF (.NOT. Above(i,k) .AND. &
                        pin(i,int)  >= pmand(k) .AND. &
                        pin(i,int+1) < pmand(k)) THEN

                      ! interpolation linear in p

                      deltap=pmand(k)-pin(i,int)
                      df=gausin(i,lat,int+1)-gausin(i,lat,int)
                      dp=pin(i,int+1)-pin(i,int)
                      work(i,k)=gausin(i,lat,int)+(df/dp)*deltap
                   END IF
                END DO
             END DO
          END DO
       END IF

       DO k=1,nlevr
          DO i=1,Imax
             gauout(i,lat,k)=work(i,k)
          END DO
       END DO

    END DO

  END SUBROUTINE gavint


END MODULE SigmaToPressure
