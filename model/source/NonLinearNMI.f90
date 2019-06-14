!
!  $Author: panetta $
!  $Date: 2007/08/12 13:52:18 $
!  $Revision: 1.10 $
!
MODULE NonLinearNMI
  USE Constants, ONLY: gasr, grav, tov, er,  pai, eriv, twomg, pscons,i8,r8
  USE Utils, ONLY: &
       Rg,         &
       Tql2,       &
       Tred2,      &
       IJtoIBJB,   &
       LinearIJtoIBJB, &
       NearestIJtoIBJB, &
       SeaMaskIJtoIBJB, &
       SplineIJtoIBJB, &
       AveBoxIJtoIBJB

  USE SpecDynamics, ONLY: tm, sv, am, bm, cm, p1, p2, h1, h2, &
       Bmcm, dk, hm, snnp1
  USE IOLowLevel, ONLY: ReadGetNFTGZ
  USE ModTimeStep, ONLY: TimeStep, SfcGeoTrans
  USE Sizes, ONLY: kmax, nmax, mmax, imax, jmax, mymnMap, &
       ibmax ,   &
       jbmax,    &
       mymnMax,  &
       mnmax,    &
       mymMax,   &
       lm2m,     &
       nodeHasM, &
       cl   ,    &
       si   ,    &
       sl   ,    &
       del  ,    &
       ThreadDecomp,&
       ThreadDecompms,&
       ibMaxPerJB

  USE FieldsDynamics, ONLY: qdivt, qdivp, &
       qrott, qrotp, &
       qtmpt, qtmpp, &
       qlnpt, qlnpp,   qlnpl, &
       qqp,   qdiaten, qgzs
  USE FieldsPhysics, ONLY: &
       tg1   , &
       tg2   , &
       tg3   , &
       zorl  , &
       avisd , &
       gtsea , &
       geshem, &
       sheleg, &
       soilm , &
       o3mix 

  USE Options, ONLY: &
       delt  ,&
       nfprt ,&
       nstep ,&
       nfin0 ,&
       nfin1 ,&
       nfnmi ,&
       ifalb ,&
       ifsst ,&
       ifslm ,&
       ifsnw ,&
       ifozone, &
       sstlag,&
       intsst,&
       fint  ,&
       nftgz0,&
       dt    ,&
       percut,&
       jdt   ,&
       yrl   ,&
       monl  ,&
       dodyn ,&
       nfdyn ,&
       nfsst ,&
       filta ,&
       filtb ,&
       ndord ,&
       istrt ,&
       ifilt ,&
       kt    ,&
       ktm   ,&
       reducedGrid

  USE InputOutput, ONLY: &
       getsbc,&
       gread ,&
       gread4,&
       fsbc

  USE Parallelism, ONLY: &
       myid  

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Nlnmi
  PUBLIC :: Diaten
  PUBLIC :: Getmod

  INTEGER :: mods, niter

  INCLUDE "mpif.h"

CONTAINS


  !
  ! diaten :computation of the diabatic terms for normal mode
  !         initialization.
  !


  SUBROUTINE Diaten(slagr,fName0,&
       ifday, tod, idate, idatec)

    LOGICAL,             INTENT(IN) :: slagr
    CHARACTER(LEN=200),  INTENT(IN) :: fName0
    INTEGER, INTENT(OUT) :: ifday
    REAL(KIND=r8)   , INTENT(OUT) :: tod
    INTEGER, INTENT(OUT), DIMENSION(4) :: idate, idatec
    !
    INTEGER :: ierr
    REAL(KIND=r8)    :: fa, fb,fb1
    REAL(KIND=r8)    :: tice=271.16e0_r8
    REAL(KIND=r8) ::   buf (iMax,jMax,4)

    buf=0.0_r8
    fsbc=.false.
    !
    !     start reading initial values
    !
    OPEN(UNIT=nfin0, FILE=fName0, FORM='unformatted',ACCESS='sequential',&
         ACTION='read',STATUS='old', IOSTAT=ierr)
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            fName0, ierr
       STOP "**(ERROR)**"
    END IF
    !
    CALL gread4 (nfin0, ifday, tod  , idate, idatec,qgzs  ,qlnpp , &
         qtmpp, qdivp, qrotp, qqp  , sl    , si   ,dodyn , &
         nfdyn)
    REWIND nfin0
    !
    !     cold start: reset precip. to zero.
    !
    geshem = 0.0_r8
    !
    !     calculates laplacian of topography
    !
    CALL SfcGeoTrans(slagr)
    !
    !     read climatology data
    !
    CALL ReadGetNFTGZ(nftgz0,buf(:,:,1),buf(:,:,2),buf(:,:,3),buf(:,:,4))

    IF (reducedGrid) THEN
       CALL AveBoxIJtoIBJB(buf(:,:,1),tg1)
    ELSE
       CALL IJtoIBJB(buf(:,:,1) ,tg1 )
    END IF

    IF (reducedGrid) THEN
       CALL AveBoxIJtoIBJB(buf(:,:,2) ,tg2)
    ELSE
       CALL IJtoIBJB(buf(:,:,2) ,tg2 )
    END IF

    IF (reducedGrid) THEN
       CALL AveBoxIJtoIBJB(buf(:,:,3) ,tg3)
    ELSE
       CALL IJtoIBJB(buf(:,:,3) ,tg3 )
    END IF

    IF (reducedGrid) THEN
       CALL AveBoxIJtoIBJB(buf(:,:,4),zorl)
    ELSE
       CALL IJtoIBJB(buf(:,:,4),zorl )
    END IF


    CALL getsbc (iMax ,jMax  ,kMax, avisd,gtsea,soilm,sheleg,o3mix,&
         ifday , tod  ,idate ,idatec, &
         ifalb,ifsst,ifslm ,ifsnw,ifozone, &
         sstlag,intsst,fint ,tice  , &
         yrl  ,monl,ibMax,jbMax,ibMaxPerJB)
    !
    !     cold start (at first delt/4 ,then delt/2 )
    !
    dt= delt /4.0_r8

    ! filter arguments for first time step

    fa = 0.0_r8
    fb = 1.0_r8
    fb1 = 1.0_r8
    ifilt=0

    DO jdt=1,2
       istrt=jdt
       !
       !     calculate matrices for semi-implicit integration
       !
       CALL bmcm(dt, slagr)
       !      perform time step
       !$OMP PARALLEL
       CALL TimeStep(fb1,fa,fb,slagr,.FALSE.,.FALSE.,.FALSE.,dt,jdt, &
            ifday,tod, idatec)
       !$OMP END PARALLEL


       ! prepare next time step, including filter arguments

       dt=dt*2.0_r8
       ktm=kt
       fb1 = 0.0_r8
    END DO
    !
    !     smooth start
    !
    istrt=0
    !
    ! semi-implicit matrices
    !
    CALL bmcm(dt, slagr)
    !
    ! filter arguments for all remaining time steps
    !
    fa = filta
    fb = filtb
    fb1 = 0.0_r8
    ifilt=1
    !
    ! time step loop
    !
    DO jdt=1,nstep
       !
       !     step loop starts
       !
       ! perform time step
       !$OMP PARALLEL
       CALL TimeStep(fb1,fa,fb,slagr,.FALSE.,.TRUE.,.FALSE.,dt,jdt, &
            ifday,tod,idatec)
       !$OMP END PARALLEL

       ktm=kt
       !
       fb1 = fb
    ENDDO
    qdiaten = qdiaten / (2.0_r8*dt*nstep)

    !
  END SUBROUTINE Diaten


  ! nlnmi :  nonlinear normal mode
  !          initialization of the model,
  !          possibly diabatic. Initial
  !          values of vorticity, divergence,
  !          temperature and log of surface
  !          pressure are adjusted to supress
  !          gravity modes, according to the
  !          normal mode method of Machenhauer.



  SUBROUTINE Nlnmi(nlnminit,diabatic,slagr,fName, & 
       ifday, tod, idatec,ktm)
    LOGICAL,          INTENT(IN) :: nlnminit
    LOGICAL,          INTENT(IN) :: diabatic
    LOGICAL,          INTENT(IN) :: slagr
    CHARACTER(LEN=*), INTENT(IN) :: fName
    INTEGER, INTENT(IN) :: ifday
    REAL(KIND=r8)   , INTENT(IN) :: tod
    INTEGER, INTENT(IN), DIMENSION(4) :: idatec
    INTEGER, INTENT(INOUT) :: ktm
    !
    REAL(KIND=r8), ALLOCATABLE :: per(:), g(:)
    INTEGER, ALLOCATABLE :: indper(:,:)
    INTEGER(KIND=i8), ALLOCATABLE :: jg(:,:)
    INTEGER, ALLOCATABLE :: indg(:,:)
    INTEGER(KIND=i8), ALLOCATABLE :: nas(:,:)
    REAL(KIND=r8) :: dt, tor, fa, fb, fb1, fcon
    REAL(KIND=r8) :: eigg(kmax,kmax),eiggt(kmax,kmax)
    REAL(KIND=r8) :: dotpro(kmax),gh(kmax),verin(kmax),to(kmax)
    REAL(KIND=r8) :: qgenp(2*mymnMax,kmax)
    CHARACTER(LEN=10) :: c0, c1, c2
    CHARACTER(LEN=*), PARAMETER :: h="**(Nlnmi)**"

    INTEGER :: i, ig, ijg, iper, iter, k, j, l, kmod, lev, ll, ierr, ip, kp
    INTEGER(KIND=i8) :: inc_ig, next_ig, inc_per, next_per
    
    REAL(KIND=r8), ALLOCATABLE :: one_per(:), one_g(:)
    LOGICAL :: mine_per_g(mMax)
    INTEGER(KIND=i8), ALLOCATABLE :: dim_1(:,:,:), dim_2(:,:,:)
    INTEGER(KIND=i8), ALLOCATABLE :: first_g(:,:,:), first_per(:,:,:)
    INTEGER(KIND=i8) :: tot_g, tot_per
    INTEGER :: twice
    INTEGER :: myms(mymmax),nms,mnFirst,mnLast,mnRIFirst,mnRILast

    ! file to read normal modes

    OPEN(UNIT=nfnmi, FILE=fName, FORM='unformatted',ACCESS='sequential',&
         ACTION='read', STATUS='old', IOSTAT=ierr) 
    IF (ierr /= 0) THEN
       WRITE(UNIT=nfprt,FMT="('**(ERROR)** Open file ',a,' returned iostat=',i4)") &
            fName, ierr
       STOP "**(ERROR)**"
    END IF
    READ(UNIT=nfnmi)eigg,eiggt,gh,dotpro,to
    WRITE(UNIT=nfprt, FMT='(/,A)') ' From Nlnmi :'

    ! how many modes

    CALL SetMods(gh)

    IF (mods <= 2) THEN
       niter=2
    ELSE
       niter=3
    END IF
    WRITE(UNIT=nfprt,FMT='(/,2(A,I3),/)') ' mods = ', mods, ' niter = ', niter

    CALL bmcm(1.0_r8,slagr)

    
    ! find out sizes of arrays g and per

    ALLOCATE(one_g(3*(mMax+1)*(mMax+1)))
    ALLOCATE(one_per(3*(mMax+1)*(mMax+1)))
    ALLOCATE(dim_1(mMax,2,mods))
    ALLOCATE(dim_2(mMax,2,mods))
    ALLOCATE(first_g(mMax,2,mods))
    ALLOCATE(first_per(mMax,2,mods))

    DO ll=1,mMax
       mine_per_g(ll) = nodeHasM(ll) == myId
    END DO

    DO i=1,mods 
       DO ll=1,Mmax
          READ(UNIT=nfnmi) dim_1(ll,1,i), dim_2(ll,1,i)
          IF (dim_1(ll,1,i) /= 0_i8) THEN
             CALL getperg(dim_2(ll,1,i), one_per, one_g, dim_1(ll,1,i))
          END IF
          READ(UNIT=nfnmi) dim_1(ll,2,i), dim_2(ll,2,i)
          IF (dim_1(ll,2,i) /= 0_i8) THEN
             CALL getperg(dim_2(ll,2,i), one_per, one_g, dim_1(ll,2,i))
          END IF
       END DO
    END DO

    tot_g = 0_i8
    tot_per = 0_i8
    DO i=1,mods 
       DO ll=1,Mmax
          IF (mine_per_g(ll)) THEN
             first_per(ll,1,i) = tot_per + 1_i8
             tot_per = tot_per + dim_1(ll,1,i)
             first_per(ll,2,i) = tot_per + 1_i8
             tot_per = tot_per + dim_1(ll,2,i)
             first_g(ll,1,i) = tot_g + 1_i8
             tot_g = tot_g + dim_1(ll,1,i)*dim_2(ll,1,i)
             first_g(ll,2,i) = tot_g + 1_i8
             tot_g = tot_g + dim_1(ll,2,i)*dim_2(ll,2,i)
          ELSE
             first_per(ll,1,i) = 0_i8
             first_g(ll,1,i) = 0_i8
             first_per(ll,2,i) = 0_i8
             first_g(ll,2,i) = 0_i8
          END IF
       END DO
    END DO

    ! reset file position for reading

    REWIND(UNIT=nfnmi)
    !
    !  Read in the vertical Normal Modes and related stuff
    !  ---------------------------------------------------
    READ(UNIT=nfnmi)eigg,eiggt,gh,dotpro,to
    DO k=1, kmax
       verin(k)=0.0_r8
       DO j=1, kmax
          verin(k)=verin(k)+sv(j)*eigg(j,k)
       END DO
    END DO


    !
    !   Read all periods and gravity modes
    !

    ALLOCATE(indper(2*mymMax,mods))
    ALLOCATE(indg(2*mymMax,mods))
    ALLOCATE(jg(2*mymMax+1,mods))
    ALLOCATE(nas(2*mymMax+1,mods))
    ALLOCATE(per(tot_per))
    ALLOCATE(g(tot_g), stat=ierr)
    IF (ierr /= 0) THEN
       WRITE(c0,"(i10)") ierr
       WRITE(*,"(a)") h//"ERROR** allocate g returns stat="//TRIM(ADJUSTL(c0))
       WRITE(c0,"(i10)") myMMax
       WRITE(c1,"(i10)") Mmax
       WRITE(c2,"(i10)") mods
       WRITE(*,"(a)") h//"ERROR** myMMax="//trim(adjustl(c0))//&
            &"; MMax="//trim(adjustl(c1))//&
            &"; mods="//trim(adjustl(c2))
       WRITE(c0,"(i10)") tot_g
       WRITE(*,"(a)") h//"ERROR** size(g) in r8 elements is "//trim(adjustl(c0))
       WRITE(c0,"(i10)") tot_g/(1024*128)
       WRITE(*,"(a)") h//"ERROR** size(g) in MBytes is "//trim(adjustl(c0))
       CALL mpi_abort(MPI_COMM_WORLD, ierr)
       STOP
    END IF
    ig = 1
    iper = 1
    DO i=1,mods 
       ijg = 1
       DO ll=1,Mmax
          DO twice = 1, 2
             READ(UNIT=nfnmi) jg(ijg,i),nas(ijg,i)
             inc_ig = INT(jg(ijg,i) * nas(ijg,i),i8)
             next_ig = ig + inc_ig
             inc_per = INT(jg(ijg,i),i8)
             next_per = iper + inc_per
             IF (nodeHasM(ll) == myId) THEN
                IF (next_ig-1 > tot_g) then
                   WRITE(*,"(a)") h//"ERROR** estourou g"
                   CALL mpi_abort(MPI_COMM_WORLD, ierr)
                   STOP
                ELSE IF (next_per-1 > tot_per) then
                   WRITE(*,"(a)") h//"ERROR** estourou per"
                   CALL mpi_abort(MPI_COMM_WORLD, ierr)
                   STOP
                END IF
                IF (jg(ijg,i) /= 0) THEN
                   CALL getperg(nas(ijg,i),per(iper),g(ig),jg(ijg,i))
                END IF
                indper(ijg,i) = iper
                indg(ijg,i) = ig
                ig = next_ig
                iper = next_per
                ijg = ijg + 1
             ELSE IF (jg(ijg,i) /= 0) THEN
                CALL getperg(nas(ijg,i), one_per, one_g, jg(ijg,i))
             END IF
          END DO
       ENDDO
    ENDDO
    CLOSE(nfnmi)
    fb1 = 1.0_r8
    fa = 0.0_r8
    fb = 0.0_r8
    dt = 1.0_r8
    !$OMP PARALLEL PRIVATE(myms,nms,mnFirst,mnLast,mnRIFirst,mnRILast,iter,l,ip,kp,kmod,fcon,tor,lev)
    !
    !  Compute the iterations of the non-linear initialization
    !  -------------------------------------------------------

    CALL ThreadDecompms(mymMax, myms, nms)
    CALL ThreadDecomp(1, mymnMax, mnFirst, mnLast, "Nlnmi")
    CALL ThreadDecomp(1, 2*mymnMax, mnRIFirst, mnRILast, "Nlnmi")
    !
    !  Compute the iterations of the non-linear initialization
    !  -------------------------------------------------------
    DO iter=1,niter
       !
       !  Compute non-linear complete tendencies
       !  --------------------------------------
       CALL TimeStep(fb1,fa,fb,slagr,nlnminit,.FALSE.,.FALSE.,dt,0,ifday, tod, idatec)
       !
       !     add diabatic heating rate to temperature tendency
       !     -------------------------------------------------
       IF (diabatic) THEN
          DO l=mnRIFirst, mnRILast
            qtmpt(l,:) = qtmpt(l,:) + qdiaten(l,:)
          END DO
       ENDIF
       !
       !     create generalized pressure=(phi+r*to*q)(dot)
       !     ---------------------------------------------
       DO l=1, kmax
          tor=to(l)* gasr
          DO ip=mnRIFirst, mnRILast
             qgenp(ip,l)=tor*qlnpt(ip)
          END DO
          DO kp=1, kmax
             DO ip=mnRIFirst, mnRILast
                qgenp(ip,l)=qgenp(ip,l) + qtmpt(ip,kp)*hm(l,kp)
             END DO
          END DO
       END DO
       !$OMP BARRIER
       CALL vertic(qdivt,eigg,eiggt,dotpro,-1,mnFirst,mnLast)
       CALL vertic(qrott,eigg,eiggt,dotpro,-1,mnFirst,mnLast)
       CALL vertic(qgenp,eigg,eiggt,dotpro,-1,mnFirst,mnLast)
       CALL primes(qrott,qdivt,qgenp,gh,-1,mnFirst,mnLast)
       !$OMP BARRIER
       CALL horiz1(qrott,qdivt,qgenp,percut,per,g,indper,indg, &
                                           jg,nas,myms,nms)
       !$OMP BARRIER
       CALL primes(qrott,qdivt,qgenp,gh,+1,mnFirst,mnLast)
       !
       !     compute delta(q) from composite variable
       !
       DO ip=mnRIFirst, mnRILast
          qlnpt(ip)= 0.0_r8
       END DO
       !$OMP BARRIER
       DO kmod=1,mods
          fcon=verin(kmod)/gh(kmod)
          DO ip=mnRIFirst, mnRILast
             qlnpt(ip)=qlnpt(ip)-fcon*qgenp(ip,kmod)
          END DO
       END DO
       !$OMP BARRIER
       CALL vertic(qdivt,eigg,eiggt,dotpro,+1,mnFirst,mnLast)
       CALL vertic(qrott,eigg,eiggt,dotpro,+1,mnFirst,mnLast)
       CALL vertic(qgenp,eigg,eiggt,dotpro,+1,mnFirst,mnLast)
       !
       !     compute delta(phi) from composite variable
       !
       !$OMP BARRIER
       DO kp=1, kmax
          tor=to(kp)*gasr
          DO ip=mnRIFirst, mnRILast
             qgenp(ip,kp)=qgenp(ip,kp)-tor*qlnpt(ip)
          END DO
       END DO
       !
       !     compute delta(t) from phi
       !
       qtmpt(mnRIFirst:mnRILast,:) = 0.0_r8
       DO lev=1,kmax
          DO kp=1, kmax
             DO ip=mnRIFirst, mnRILast
                qtmpt(ip,lev)=qtmpt(ip,lev)+tm(lev,kp)*qgenp(ip,kp)
             END DO
          END DO
       END DO

       IF (slagr) THEN
          DO ip=mnRIFirst, mnRILast
             qlnpl(ip)=qlnpl(ip)-qlnpt(ip)
          END DO
       END IF
       DO ip=mnRIFirst, mnRILast
          qlnpp(ip)=qlnpp(ip)-qlnpt(ip)
          qtmpp(ip,:)=qtmpp(ip,:)-qtmpt(ip,:)
          qrotp(ip,:)=qrotp(ip,:)-qrott(ip,:)
          qdivp(ip,:)=qdivp(ip,:)-qdivt(ip,:)
       END DO

    END DO
    !$OMP END PARALLEL
    ktm=0
    CLOSE(UNIT=nfnmi)
    DEALLOCATE(indper)
    DEALLOCATE(indg)
    DEALLOCATE(jg)
    DEALLOCATE(nas)
    DEALLOCATE(per)
    DEALLOCATE(g)

    DEALLOCATE(one_g)
    DEALLOCATE(one_per)
    DEALLOCATE(dim_1)
    DEALLOCATE(dim_2)
    DEALLOCATE(first_g)
    DEALLOCATE(first_per)
  END SUBROUTINE Nlnmi



  ! vertic : performs projection of the spectral representation of all
  !          model levels of a field onto the vertical normal modes.
  !          also performs expansion of the spectral representation
  !          of a vertically projected field into a field at all model
  !          levels using the vertical normal modes.
  !
  !     input=-1 to obtain vertical mode expansion
  !     input=+1 to obtain spet. coefs. from vertical expansion



  SUBROUTINE vertic(f,eigg,eiggt,dotpro,input,mnFirst,mnLast)
    REAL(KIND=r8), INTENT(IN) :: eigg(kmax,kmax)
    REAL(KIND=r8), INTENT(IN) :: eiggt(kmax,kmax)
    REAL(KIND=r8), INTENT(IN) :: dotpro(kmax)
    REAL(KIND=r8), INTENT(INOUT) :: f(2,mymnMax,kmax)
    INTEGER, INTENT(IN) :: input
    INTEGER, INTENT(IN) :: mnFirst
    INTEGER, INTENT(IN) :: mnLast
    REAL(KIND=r8) :: col(2,kmax)
    INTEGER :: mn, kmod, lev
    REAL(KIND=r8) :: sum1, sum2

    IF (input .lt. 0) THEN
       DO mn=mnFirst,mnLast
          DO kmod=1,mods
             sum1=0.0_r8
             sum2=0.0_r8
             DO lev=1, kmax
                sum1=sum1+eiggt(lev,kmod)* f(1,mn,lev)
                sum2=sum2+eiggt(lev,kmod)* f(2,mn,lev)
             END DO
             col(1,kmod)=dotpro(kmod)*sum1
             col(2,kmod)=dotpro(kmod)*sum2
          END DO
          DO kmod=1,mods
             f(1,mn,kmod)=col(1,kmod)
             f(2,mn,kmod)=col(2,kmod)
          END DO
       END DO
    ELSE IF (input .gt. 0) THEN
       DO mn=mnFirst,mnLast
          DO kmod=1,mods
             col(1,kmod)=f(1,mn,kmod)
             col(2,kmod)=f(2,mn,kmod)
          END DO
          DO lev=1, kmax
             f(1,mn,lev)=0.0_r8
             f(2,mn,lev)=0.0_r8
             DO kmod=1,mods
                f(1,mn,lev)=f(1,mn,lev)+eigg(lev,kmod)*col(1,kmod)
                f(2,mn,lev)=f(2,mn,lev)+eigg(lev,kmod)*col(2,kmod)
             END DO
          END DO
       END DO
    END IF
  END SUBROUTINE vertic



  ! primes : performs scaling of the vertically projected tendencies
  !          of vorticity, divergence, and composite mass variable
  !          to convert them to the correct form for the calculation
  !          of the adjustment to these fields before so doing during
  !          the current iteration of the machenauer initialization
  !          technique.
  !          also performs descaling of the vertically projected
  !          tendencies of vorticity, divergence, and composite mass
  !          variable to convert them from the correct form for the
  !          calculation of the adjustment to these fields after
  !          so doing during the current iteration of the Machenhauer
  !          initialization technique.



  SUBROUTINE primes(vord,divd,comd,gh,input,mnFirst,mnLast)
    REAL(KIND=r8), INTENT(INOUT) :: vord(2,mymnMax,kmax)
    REAL(KIND=r8), INTENT(INOUT) :: divd(2,mymnMax,kmax)
    REAL(KIND=r8), INTENT(INOUT) :: comd(2,mymnMax,kmax)
    REAL(KIND=r8), INTENT(IN) :: gh(kmax)
    INTEGER, INTENT(IN) :: input
    INTEGER, INTENT(IN) :: mnFirst
    INTEGER, INTENT(IN) :: mnLast
    REAL(KIND=r8) :: w(mymnMax), t, divdr, divdi
    INTEGER :: mn, k, m1
    IF (myid.eq.0.and.mnFirst.eq.1) THEN
       w(1) = 0.0_r8
       m1 = 2
      ELSE
       m1 = mnFirst
    ENDIF
    IF (input .lt. 0) THEN
       DO  mn=m1,mnLast
          w(mn)=1.0_r8/SQRT(REAL(snnp1(2*mn-1),r8))
       END DO
       DO k=1,mods
          t=1.0_r8/(er*SQRT(gh(k)))
          DO mn=mnFirst,mnLast
             vord(1,mn,k)=w(mn)*vord(1,mn,k)
             vord(2,mn,k)=w(mn)*vord(2,mn,k)
             divdr=-w(mn)*divd(2,mn,k)
             divdi= w(mn)*divd(1,mn,k)
             divd(1,mn,k)=divdr
             divd(2,mn,k)=divdi
             comd(1,mn,k)=t*comd(1,mn,k)
             comd(2,mn,k)=t*comd(2,mn,k)
          END DO
       END DO
    ELSE IF (input .gt. 0) THEN
       DO mn=m1,mnLast
          w(mn)=SQRT(REAL(snnp1(2*mn-1),r8))
       END DO
       DO k=1,mods
          t=er*SQRT(gh(k))
          DO mn=mnFirst,mnLast
             vord(1,mn,k)=w(mn)*vord(1,mn,k)
             vord(2,mn,k)=w(mn)*vord(2,mn,k)
             divdr= w(mn)*divd(2,mn,k)
             divdi=-w(mn)*divd(1,mn,k)
             divd(1,mn,k)=divdr
             divd(2,mn,k)=divdi
             comd(1,mn,k)=t*comd(1,mn,k)
             comd(2,mn,k)=t*comd(2,mn,k)
          END DO
       END DO
    END IF
  END SUBROUTINE primes



  ! horiz1 : arranges the vertically projected, scaled coefficients
  !          of vorticity, divergence, and composite mass variable
  !          for calculation of the adjustment to these fields by
  !          routine "horiz2". next, rearranges the calculated
  !          adjustment of vorticity, divergence, and composite
  !          variable back into the same form (and overwrites) the
  !          original input fields.



  SUBROUTINE horiz1(vord,divd,comd,v,per,g,indper,indg,jg,nas,myms,nms)
    REAL(KIND=r8), INTENT(INOUT) :: vord(2,mymnMax,kmax)
    REAL(KIND=r8), INTENT(INOUT) :: divd(2,mymnMax,kmax)
    REAL(KIND=r8), INTENT(INOUT) :: comd(2,mymnMax,kmax)
    REAL(KIND=r8), INTENT(IN)    :: v
    REAL(KIND=r8), INTENT(IN)    :: per(3*(myMmax+1)*(Mmax+1)/2*mods)
    REAL(KIND=r8), INTENT(IN)    :: g((myMmax+1)*(3*(Mmax+1)/2)**2*mods)
    INTEGER, INTENT(IN) :: nms
    INTEGER, INTENT(IN) :: myms(nms)
    INTEGER, INTENT(IN) :: indper(2*mymMax,mods)
    INTEGER(KIND=i8), INTENT(IN) :: jg(2*mymMax+1,mods)
    INTEGER, INTENT(IN) :: indg(2*mymMax,mods)
    INTEGER(KIND=i8), INTENT(IN) :: nas(2*mymMax+1,mods)
    !
    REAL(KIND=r8) :: sdot(2,3*(mMax+1)/2), adot(2,3*(mMax+1)/2)
    INTEGER :: nn, k, modes, l, ll, jsod, jsev, jevpod, i, mi
    INTEGER :: nends, nenda, lx, ir, nnmax, mglob
    !
    DO modes=1,mods
       DO mi=1,nms
          ll = myms(mi)
          i = 2*ll-1
          k=0
          mglob=lm2m(ll)
          l=mglob-1
          nnmax=mmax+1-mglob
          jsod=nnmax/2
          jsev=nnmax-jsod
          jevpod=jsev+jsod
          nends=jevpod+jsev
          nenda=jevpod+jsod
          DO nn=mglob+1,mmax,2
             lx=mymnMap(ll,nn)
             k=k+1
             DO ir=1,2
                sdot(ir,k       )=vord(ir,lx,modes)
                adot(ir,k+jsev  )=divd(ir,lx,modes)
                adot(ir,k+jevpod)=comd(ir,lx,modes)
             END DO
          END DO
          k=0
          DO nn=mglob,mmax,2
             lx=mymnMap(ll,nn)
             k=k+1
             DO ir=1,2
                adot(ir,k       )=vord(ir,lx,modes)
                sdot(ir,k+jsod  )=divd(ir,lx,modes)
                sdot(ir,k+jevpod)=comd(ir,lx,modes)
             END DO
          END DO
          IF (jg(i,modes).ne.0) &
             CALL horiz2(sdot,nas(i,modes),per(indper(i,modes)),&
                         g(indg(i,modes)),jg(i,modes),v,l)
          i = i+1
          IF (jg(i,modes).ne.0) &
             CALL horiz2(adot,nas(i,modes),per(indper(i,modes)),&
                         g(indg(i,modes)),jg(i,modes),v,l)
          k=0
          DO nn=mglob+1,mmax,2
             lx=mymnMap(ll,nn)
             k=k+1
             DO ir=1,2
                vord(ir,lx,modes)=sdot(ir,k)
                divd(ir,lx,modes)=adot(ir,k+jsev)
                comd(ir,lx,modes)=adot(ir,k+jevpod)
             END DO
          END DO
          k=0
          DO nn=mglob,mmax,2
             lx=mymnMap(ll,nn)
             k=k+1
             DO ir=1,2
                vord(ir,lx,modes)=adot(ir,k)
                divd(ir,lx,modes)=sdot(ir,k+jsod)
                comd(ir,lx,modes)=sdot(ir,k+jevpod)
             END DO
          END DO
       END DO
    END DO
  END SUBROUTINE horiz1

  SUBROUTINE getperg(nas,per,g,jg)
    INTEGER(KIND=i8), INTENT(IN) :: jg, nas
    REAL(KIND=r8), INTENT(INOUT) :: per(jg), g(jg,nas)
    !
    !     per stores periods of gravity modes,g stores eigenvectors
    !     both are read in this routine
    !

    !
    READ(UNIT=nfnmi) per,g

  END SUBROUTINE getperg

  ! horiz2 : calculates for one zonal wave number the Machenhauer
  !          adjustment to the vertically projected, properly scaled
  !          and rearranged spectral coefficients of vorticity,
  !          divergence, and composite mass variable.



  SUBROUTINE horiz2(dot,nas,per,g,jg,percut,l)
    INTEGER, INTENT(IN) :: l
    INTEGER(KIND=i8), INTENT(IN) :: jg, nas
    REAL(KIND=r8), INTENT(INOUT) :: dot(2,nas)
    REAL(KIND=r8), INTENT(IN) :: per(jg), g(jg,nas)
    REAL(KIND=r8), INTENT(IN) :: percut
    REAL(KIND=r8) :: period, dif, difcut
    REAL(KIND=r8) :: y(2,3*(mMax+1)/2), yi, yr
    INTEGER :: init(3*(mMax+1)/2)
    INTEGER :: ndho, i, j, k, n
    !
    !     nas=vector size of sym. or  asy. tendencies stored in dot
    !     jg=nunber of gravity modes. jg=jcap for l=0 sym and asy cases
    !     for l.ne.0 jg=jcap2 for sym,jg=jcap for asy.
    !     per stores periods of gravity modes,g stores eigenvectors
    !
    dif=dk
    ndho=ndord/2
    DO j=1,jg
       init(j)=0
       y(1,j)=0.0_r8
       y(2,j)=0.0_r8
    END DO
    n=l
    DO i=1,jg
       n=n+1
       period=ABS(per(i))
       IF (period-percut.le.0.0_r8) THEN
          !
          !     arbitray even order horizontal diffusion now used.
          !     smaller "tweek" constant used to avoid inaccuracies when
          !     reciprocal is taken.
          !     ________________________________________________________
          difcut=dif*(REAL(n*(n+1),r8)**ndho) + 1.0e-7_r8
          difcut=0.5_r8/difcut
          IF (difcut-period.ge.0.0_r8) THEN
             init(i)=i
             DO j=1,nas
                y(1,i)=y(1,i)+g(i,j)*dot(1,j)
                y(2,i)=y(2,i)+g(i,j)*dot(2,j)
             END DO
             yr=y(1,i)
             yi=y(2,i)
             y(1,i)= per(i)*yi
             y(2,i)=-per(i)*yr
          END IF
       END IF
    END DO
    DO j=1,nas
       dot(1,j)=0.0_r8
       dot(2,j)=0.0_r8
    END DO
    DO j=1,nas
       DO k=1,jg
          IF (init(k) .ne. 0) THEN
             dot(1,j)=dot(1,j)+g(k,j)*y(1,k)
             dot(2,j)=dot(2,j)+g(k,j)*y(2,k)
          END IF
       END DO
    END DO
  END SUBROUTINE horiz2



  !Getmod : computes vertical and horizontal
  !         modes to be used in the
  !         initialization of the model.
  !         The modes are stored in files
  !         which can be used by the model,
  !         without calling getmod again.
  !         The modes depend not only on the
  !         resolution of the model, but
  !         also on the Eulerian or
  !         Semi-Lagrangian option.



  SUBROUTINE Getmod(slagr,fName,rsettov)
    LOGICAL,          INTENT(IN) :: slagr
    CHARACTER(LEN=*), INTENT(IN) :: fName
    LOGICAL,          INTENT(IN) :: rsettov

    INTEGER, PARAMETER :: matz=1
    REAL(KIND=r8),    PARAMETER :: eps=2.0_r8**(-50)
    CHARACTER(LEN=*), PARAMETER :: h="**(GetMod)**"
    REAL(KIND=r8)    :: gh(kmax)
    INTEGER :: ierr
    
    OPEN(UNIT=nfnmi,FILE=fName,FORM='unformatted',ACCESS='sequential',&
         ACTION='readwrite', STATUS='replace',IOSTAT=ierr)
    CALL Vermod(gh,eps,matz,slagr,rsettov)

    ! how many modes

    CALL SetMods(gh)

    CALL Hormod(gh,eps)
    CLOSE(UNIT=nfnmi)
  END SUBROUTINE Getmod






  SUBROUTINE Vermod(gh,eps,matz,slagr,rsettov)
    REAL(KIND=r8),    INTENT(OUT) :: gh(kmax)
    REAL(KIND=r8),    INTENT(IN ) :: eps
    INTEGER, INTENT(IN ) :: matz
    LOGICAL, INTENT(IN ) :: slagr
    LOGICAL, INTENT(IN ) :: rsettov

    INTEGER :: j
    INTEGER :: k
    REAL(KIND=r8)    :: p
    REAL(KIND=r8)    :: siman
    REAL(KIND=r8)    :: soma
    REAL(KIND=r8)    :: er2
    REAL(KIND=r8)    :: eigg(kmax,kmax)
    REAL(KIND=r8)    :: eiggt(kmax,kmax)
    REAL(KIND=r8)    :: eigvc(kmax,kmax)
    REAL(KIND=r8)    :: dotpro(kmax)
    REAL(KIND=r8)    :: to(kmax)
    REAL(KIND=r8)    :: g(kmax,kmax)
    REAL(KIND=r8)    :: gt(kmax,kmax)

    IF(rsettov) THEN
       to = tov
       DO k=1,kmax
          p=sl(k)*pscons
          CALL W3fa03(p,tov(k))
       END DO
    END IF
    CALL bmcm(1.0_r8,slagr)
    er2=er*er
    g = cm * er2
    gt = TRANSPOSE (g)
    siman=-1.0_r8
    CALL Vereig(g,siman,eigvc,cl,eigg,gh,eps,matz)
    siman=1.0_r8
    CALL Vereig(gt,siman,eigvc,cl,eiggt,dotpro,eps,matz)

    ! dotpro=inverse dot prod. of eigenvec(g)*eigenvec(gtranspose)

    DO k=1,kmax
       soma=0.0_r8
       DO j=1,kmax
          soma=soma+eigg(j,k)*eiggt(j,k)
       END DO
       dotpro(k)=1.0_r8/soma
    END DO
    WRITE(UNIT=nfnmi)eigg,eiggt,gh,dotpro,tov
    IF (rsettov) THEN
       tov = to
    END IF
  END SUBROUTINE Vermod






  SUBROUTINE Hormod(gh,eps)
    REAL(KIND=r8),    INTENT(IN) :: eps
    REAL(KIND=r8),    INTENT(IN) :: gh(mods)

    INTEGER, PARAMETER :: ipr=-1
    INTEGER :: nxsy
    INTEGER :: nxas
    INTEGER :: k
    INTEGER :: m
    INTEGER :: nlx
    INTEGER :: nmd
    INTEGER :: lmax
    INTEGER :: klmx
    INTEGER :: mmmax
    INTEGER :: nnmax
    INTEGER(KIND=i8) :: nsy
    INTEGER(KIND=i8) :: nas
    INTEGER :: n
    INTEGER(KIND=i8) :: ncuts
    INTEGER(KIND=i8) :: ncuta
    INTEGER :: mend
    INTEGER :: modd
    INTEGER :: lend
    INTEGER :: kend
    REAL(KIND=r8)    :: alfa(mMax)
    REAL(KIND=r8)    :: beta(mMax)
    REAL(KIND=r8)    :: gama(mMax)
    REAL(KIND=r8)    :: dgl(mMax)
    REAL(KIND=r8)    :: sdg(mMax)
    REAL(KIND=r8)    :: xx(mMax,mMax)
    REAL(KIND=r8)    :: wk(3*(mMax+1)/2)
    REAL(KIND=r8)    :: ws(3*(mMax+1)/2)
    REAL(KIND=r8)    :: wa(3*(mMax+1)/2)
    REAL(KIND=r8)    :: xs(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL(KIND=r8)    :: xa(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL(KIND=r8)    :: es(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL(KIND=r8)    :: ea(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL(KIND=r8)    :: rm
    REAL(KIND=r8)    :: rn

    mend = mMax-1
    modd = MOD(mMax-1,2)
    lend = (mend+modd)/2
    kend = lend+1-modd
    nxsy = lend+2*kend
    nxas = kend+2*lend
    DO k=1,mods
       DO m=1,mMax
          nnmax=mMax-m+1
          nlx=mMax-m
          nmd=MOD(nlx,2)
          lmax=(nlx+nmd)/2
          mmmax=lmax+1-nmd
          klmx=lmax+mmmax
          nsy=lmax+2*mmmax
          nas=mmmax+2*lmax
          rm=REAL(m-1,r8)
          DO n=1,nnmax
             rn=rm+REAL(n-1,r8)
             IF (rn .EQ. 0.0_r8 ) THEN
                alfa(n)=0.0_r8
                beta(n)=0.0_r8
                gama(n)=0.0_r8
             ELSE
                alfa(n)=twomg*rm/(rn*(rn+1.0_r8))
                beta(n)=(twomg/rn)*SQRT((rn*rn-1.0_r8)*(rn*rn-rm*rm)/&
                     (4.0_r8*rn*rn-1.0_r8))
                gama(n)=eriv*SQRT(rn*(rn+1.0_r8)*gh(k))
             END IF
          END DO
          IF (m .EQ. 1) THEN

             ! symmetric case

             CALL symg0(nxsy,nsy,mMax,lmax,klmx,nmd,ipr,ncuts, &
                  eps,twomg,beta,gama, &
                  ws,sdg,dgl,xs,xx)
             CALL record(nxsy,ncuts,nsy,ws,xs,wk,es)

             ! asymmetric case

             CALL asyg0(nxas,nas,mMax,lmax,klmx,mmmax,nmd,ipr, &
                  ncuta,eps,twomg,beta,gama, &
                  wa,sdg,dgl,xa,xx)
             CALL record(nxas,ncuta,nas,wa,xa,wk,ea)
          ELSE

             ! symmetric case

             CALL symrg(nxsy,nsy,lmax,mmmax,nmd,ipr,ncuts, &
                  eps,twomg,percut,alfa,beta,gama, &
                  ws,wk,es,xs)
             CALL record(nxsy,ncuts,nsy,ws,xs,wk,es)

             ! asymmetric case

             CALL asyrg(nxas,nas,lmax,mmmax,nmd,ipr,ncuta, &
                  eps,twomg,percut,alfa,beta,gama, &
                  wa,wk,ea,xa)
             CALL record(nxas,ncuta,nas,wa,xa,wk,ea)
          END IF
       END DO
    END DO
  END SUBROUTINE Hormod






  SUBROUTINE asyg0(nxas,nas,nend1,lmax,klmx,mmax,nmd,ipr, &
       ncuta,eps,twomg,beta,gama,wa,sdg,dgl,xa,xx)
    INTEGER, INTENT(IN ) :: nxas
    INTEGER(KIND=i8), INTENT(IN ) :: nas
    INTEGER, INTENT(IN ) :: nend1
    INTEGER, INTENT(IN ) :: lmax
    INTEGER, INTENT(IN ) :: klmx
    INTEGER, INTENT(IN ) :: mmax
    INTEGER, INTENT(IN ) :: nmd
    INTEGER, INTENT(IN ) :: ipr
    INTEGER(KIND=i8), INTENT(OUT) :: ncuta
    REAL(KIND=r8),    INTENT(IN ) :: eps
    REAL(KIND=r8),    INTENT(IN ) :: twomg
    REAL(KIND=r8),    INTENT(IN ) :: beta(:)
    REAL(KIND=r8),    INTENT(IN ) :: gama(:)
    REAL(KIND=r8),    INTENT(OUT) :: wa(:)
    REAL(KIND=r8),    INTENT(OUT) :: sdg(:)
    REAL(KIND=r8),    INTENT(OUT) :: dgl(:)
    REAL(KIND=r8),    INTENT(OUT) :: xa(nxas,*)
    REAL(KIND=r8),    INTENT(OUT) :: xx(nend1,*)

    INTEGER :: n
    INTEGER :: nmx
    INTEGER :: nn
    INTEGER :: j
    INTEGER :: jj
    INTEGER :: ierr


    ! asymmetric case

    nmx=1
    n=1
    nn=2
    sdg(n)=beta(nn-1)*beta(nn)
    dgl(n)=beta(nn)*beta(nn)+beta(nn+1)*beta(nn+1)+ &
         gama(nn)*gama(nn)
    nmx=lmax-1
    DO  n=2,nmx
       nn=2*n
       sdg(n)=beta(nn-1)*beta(nn)
       dgl(n)=beta(nn)*beta(nn)+beta(nn+1)*beta(nn+1)+ &
            gama(nn)*gama(nn)
    END DO
    nmx=lmax
    n=lmax
    nn=2*n
    sdg(n)=beta(nn-1)*beta(nn)
    IF (nmd .EQ. 0) THEN
       dgl(n)=beta(nn)*beta(nn)+beta(nn+1)*beta(nn+1)+ &
            gama(nn)*gama(nn)
    ELSE
       dgl(n)=beta(nn)*beta(nn)+gama(nn)*gama(nn)
    END IF

    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' sdg:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(sdg(n),n=1,nmx)
       WRITE(UNIT=nfprt,FMT=*)' dga:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(dgl(n),n=1,nmx)
    END IF

    CALL ident(nend1,nmx,xx)
    CALL tql2(nend1,nmx,dgl,sdg,xx,eps,ierr)

    DO j=1,nmx
       jj=2*j
       wa(jj-1)=-SQRT(dgl(j))
       wa(jj)=SQRT(dgl(j))
       xa(1,jj-1)=0.0_r8
       xa(1,jj)=-xa(1,jj-1)
       xa(mmax+1,jj-1)=xx(1,j)
       xa(mmax+1,jj)=xa(mmax+1,jj-1)
       xa(klmx+1,jj-1)=gama(2)*xx(1,j)/wa(jj-1)
       xa(klmx+1,jj)=-xa(klmx+1,jj-1)
       DO n=2,nmx
          nn=2*n
          xa(n,jj-1)=(beta(nn-1)*xx(n-1,j)+beta(nn)*xx(n,j))/wa(jj-1)
          xa(n,jj)=-xa(n,jj-1)
          xa(mmax+n,jj-1)=xx(n,j)
          xa(mmax+n,jj)=xa(mmax+n,jj-1)
          xa(klmx+n,jj-1)=gama(nn)*xx(n,j)/wa(jj-1)
          xa(klmx+n,jj)=-xa(klmx+n,jj-1)
       END DO
       n=nmx
       nn=2*n+1
       IF (nmd .EQ. 0) THEN
          xa(mmax,jj-1)=beta(nn)*xx(n,j)/wa(jj-1)
          xa(mmax,jj)=-xa(mmax,jj-1)
       END IF
    END DO

    ncuta=2*nmx
    CALL filter(nxas,nas,ncuta,xa,0.0_r8,eps)

    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' frequency: nas=',nas,' ncuta=',ncuta
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(wa(n)/twomg,n=1,ncuta)
       WRITE(UNIT=nfprt,FMT=*)' period:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(1.0_r8/wa(n),n=1,ncuta)
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' xa: ierr=',ierr
       DO n=1,nmx
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(xx(n,nn),nn=1,nmx)
       END DO
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' xa:'
       DO n=1,nas
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(xa(n,nn),nn=1,MIN(6_i8,ncuta))
       END DO
    END IF
  END SUBROUTINE asyg0






  SUBROUTINE asyrg(nxas,nas,lmax,mmax,nmd,ipr,ncuta, &
       eps,twomg,percut,alfa,beta,gama,wa,wk,ea,xa)
    INTEGER, INTENT(IN) :: nxas,lmax,mmax,nmd,ipr
    INTEGER(KIND=i8), INTENT(IN) :: nas
    INTEGER(KIND=i8), INTENT(OUT) :: ncuta
    REAL(KIND=r8), INTENT(IN) :: eps,twomg,percut
    REAL(KIND=r8), INTENT(IN) ::alfa(*),beta(*),gama(*)
    REAL(KIND=r8), INTENT(OUT) :: wa(*),wk(*)
    REAL(KIND=r8), INTENT(OUT) :: ea(nxas,*),xa(nxas,*)
    !
    INTEGER :: n,nn,mm,jj,ierr
    !
    !   asymmetric case
    !
    DO nn=1,nxas
       DO mm=1,nxas
          ea(mm,nn)=0.0_r8
       END DO
       wa(nn)=0.0_r8
       wk(nn)=0.0_r8
    END DO
    !
    DO n=1,mmax
       ea(n,n)=alfa(2*n-1)
    END DO
    DO n=1,lmax
       nn=2*n
       jj=mmax+n
       ea(n,jj)=beta(nn)
       ea(jj,n)=ea(n,jj)
       IF (n.LT.lmax .OR. nmd.NE.1) THEN
          ea(n+1,jj)=beta(nn+1)
          ea(jj,n+1)=ea(n+1,jj)
       END IF
    END DO
    DO n=1,lmax
       nn=2*n
       jj=mmax+n
       ea(jj,jj)=alfa(nn)
       mm=jj+lmax
       ea(jj,mm)=gama(nn)
       ea(mm,jj)=ea(jj,mm)
    END DO
    !
    IF (ipr .GE. 3) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' ea:'
       DO n=1,nas
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(ea(n,nn),nn=1,nas)
       END DO
    END IF
    !
    CALL tred2(nxas,nas,ea,wa,wk,xa)
    CALL tql2(nxas,nas,wa,wk,xa,eps,ierr)
    !
    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' wa: ierr=',ierr
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(wa(n),n=1,nas)
    END IF
    !
    !   reordering frequencies
    !
    CALL order(nxas,nas,wa,wk,xa,ea,percut,ncuta)
    CALL filter(nxas,nas,ncuta,xa,0.0_r8,eps)
    !
    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' frequency: nas=',nas,' ncuta=',ncuta
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(wa(n)/twomg,n=1,ncuta)
       WRITE(UNIT=nfprt,FMT=*)' period:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(1.0_r8/wa(n),n=1,ncuta)
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' xa:'
       DO n=1,nas
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(xa(n,nn),nn=1,MIN(6_i8,ncuta))
       END DO
    END IF
  END SUBROUTINE asyrg






  SUBROUTINE filter(nm,ni,nj,a,b,c)
    !
    INTEGER, INTENT(IN) ::  nm
    INTEGER(KIND=i8), INTENT(IN) ::  ni,nj
    REAL(KIND=r8), INTENT(IN) ::  b,c
    REAL(KIND=r8), INTENT(INOUT) :: a(nm,*)
    INTEGER :: n,j
    !
    DO j=1,nj
       DO n=1,ni
          IF (ABS(a(n,j)) .LE. c) a(n,j)=b
       END DO
       !
    END DO
  END SUBROUTINE filter






  SUBROUTINE ident(nm,n,z)
    !
    !
    !   ** initialize z to identity matrix by sqrt(2)
    !
    INTEGER, INTENT(IN) ::  nm,n
    REAL(KIND=r8), INTENT(OUT) ::  z(nm,*)
    !
    INTEGER ::  i,j
    REAL(KIND=r8) :: sqrt2
    !
    sqrt2=1.0_r8/SQRT(2.0_r8)
    DO i=1,n
       DO j=1,n
          z(i,j)=0.0_r8
       END DO
       z(i,i)=sqrt2
    END DO
  END SUBROUTINE ident






  SUBROUTINE order(nm,n,fr,fw,z,zw,percut,nf)
    !
    INTEGER, INTENT(IN) :: nm
    INTEGER(KIND=i8), INTENT(IN) :: n
    REAL(KIND=r8), INTENT(IN) :: percut
    INTEGER(KIND=i8), INTENT(OUT) :: nf
    REAL(KIND=r8), INTENT(INOUT) :: fr(nm),fw(nm),z(nm,*),zw(nm,*)
    !
    INTEGER :: nm1,k,j,j1,i,jc,nc
    REAL(KIND=r8) :: chg
    !
    nm1=n-1
10  k=0
    DO j=1,nm1
       j1=j+1
       IF (ABS(fr(j)) .GT. ABS(fr(j1))) THEN
          chg=fr(j)
          DO i=1,n
             fw(i)=z(i,j)
          END DO
          fr(j)=fr(j1)
          DO i=1,n
             z(i,j)=z(i,j1)
          END DO
          fr(j1)=chg
          DO i=1,n
             z(i,j1)=fw(i)
          END DO
          k=1
       END IF
    END DO
    IF (k .NE. 0) GOTO 10
    !
    IF (percut .LE. 0.0_r8) THEN
       nf=n
       RETURN
    END IF
    !
    nc=0
    DO j=1,n
       IF (ABS(1.0_r8/fr(j)) .GT. percut) nc=j
    END DO
    nf=n-nc
    nc=nc+1
    !
    DO j=1,n
       fw(j)=fr(j)
       DO i=1,n
          zw(i,j)=z(i,j)
       END DO
    END DO
    !
    DO i=1,nm
       DO j=1,n
          z(i,j)=0.0_r8
       END DO
       fr(i)=0.0_r8
    END DO
    !
    DO jc=nc,n
       j=jc+1-nc
       fr(j)=fw(jc)
       DO i=1,n
          z(i,j)=zw(i,jc)
       END DO
    END DO
    !
    nc=nc-1
    DO jc=1,nc
       j=n+jc-nc
       fr(j)=fw(jc)
       DO i=1,n
          z(i,j)=zw(i,jc)
       END DO
    END DO
  END SUBROUTINE order






  SUBROUTINE record(nx,nc,nm,ww,xx,pp,gg)
    !
    INTEGER, INTENT(IN) :: nx
    INTEGER(KIND=i8), INTENT(IN) :: nc,nm
    REAL(KIND=r8), INTENT(IN) :: ww(nx),xx(nx,nx)
    REAL(KIND=r8), INTENT(OUT) :: pp(nc),gg(nc,nm)
    !
    INTEGER :: n,nn
    !
    WRITE(UNIT=nfnmi)nc,nm
    IF (nc .EQ. 0) RETURN
    !
    pp = 0.0_r8
    gg = 0.0_r8
    !
    DO n=1,nc
       pp(n)=1.0_r8/ww(n)
       DO nn=1,nm
          gg(n,nn)=xx(nn,n)
       END DO
    END DO
    WRITE(UNIT=nfnmi)pp,gg
  END SUBROUTINE record







  SUBROUTINE symg0(nxsy,nsy,nend1,lmax,klmx,nmd,ipr,ncuts, &
       eps,twomg,beta,gama, &
       ws,sdg,dgl,xs,xx)
    INTEGER, INTENT(IN) ::  nxsy,nend1,lmax,klmx,nmd,ipr
    INTEGER(KIND=i8), INTENT(IN) ::  nsy
    INTEGER(KIND=i8), INTENT(OUT) ::  ncuts
    REAL(KIND=r8), INTENT(IN) :: eps,twomg
    REAL(KIND=r8), INTENT(IN) :: beta(*),gama(*)
    REAL(KIND=r8), INTENT(OUT) :: ws(*),sdg(*),dgl(*)
    REAL(KIND=r8), INTENT(OUT) :: xs(nxsy,*),xx(nend1,*)
    !
    INTEGER :: n,nmx,nn,j,jj,ierr
    !
    !     symmetric case
    !
    nmx=1
    n=1
    sdg(n)=0.0_r8
    nn=2*n+1
    dgl(n)=beta(nn)*beta(nn)+beta(nn+1)*beta(nn+1)+ &
         gama(nn)*gama(nn)
    nmx=lmax-1
    DO n=2,nmx
       nn=2*n+1
       sdg(n)=beta(nn-1)*beta(nn)
       dgl(n)=beta(nn)*beta(nn)+beta(nn+1)*beta(nn+1)+ &
            gama(nn)*gama(nn)
    END DO
    IF (nmd .EQ. 0) THEN
       nmx=lmax
       n=lmax
       nn=2*n+1
       sdg(n)=beta(nn-1)*beta(nn)
       dgl(n)=beta(nn)*beta(nn)+gama(nn)*gama(nn)
    END IF
    !
    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' sdg:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(sdg(n),n=1,nmx)
       WRITE(UNIT=nfprt,FMT=*)' dgs:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(dgl(n),n=1,nmx)
    END IF
    !
    CALL ident(nend1,nmx,xx)
    CALL tql2(nend1,nmx,dgl,sdg,xx,eps,ierr)
    !
    DO j=1,nmx
       jj=2*j
       ws(jj-1)=-SQRT(dgl(j))
       ws(jj)=SQRT(dgl(j))
       xs(1,jj-1)=beta(3)*xx(1,j)/ws(jj-1)
       xs(1,jj)=-xs(1,jj-1)
       xs(lmax+1,jj-1)=0.0_r8
       xs(lmax+1,jj)=xs(lmax+1,jj-1)
       xs(klmx+1,jj-1)=0.0_r8
       xs(klmx+1,jj)=-xs(klmx+1,jj-1)
       DO n=2,nmx
          nn=2*n+1
          xs(n,jj-1)=(beta(nn-1)*xx(n-1,j)+beta(nn)*xx(n,j))/ws(jj-1)
          xs(n,jj)=-xs(n,jj-1)
          xs(lmax+n,jj-1)=xx(n-1,j)
          xs(lmax+n,jj)=xs(lmax+n,jj-1)
          xs(klmx+n,jj-1)=gama(nn-2)*xx(n-1,j)/ws(jj-1)
          xs(klmx+n,jj)=-xs(klmx+n,jj-1)
       END DO
       n=lmax
       nn=2*n-1
       IF (nmd .EQ. 1) THEN
          xs(n,jj-1)=beta(nn+1)*xx(n-1,j)/ws(jj-1)
          xs(n,jj)=-xs(n,jj-1)
          xs(klmx,jj-1)=xx(n-1,j)
          xs(klmx,jj)=xs(klmx,jj-1)
          xs(nsy,jj-1)=gama(nn)*xx(n-1,j)/ws(jj-1)
          xs(nsy,jj)=-xs(nsy,jj-1)
       ELSE
          xs(klmx,jj-1)=xx(n,j)
          xs(klmx,jj)=xs(klmx,jj-1)
          xs(nsy,jj-1)=gama(nn+2)*xx(n,j)/ws(jj-1)
          xs(nsy,jj)=-xs(nsy,jj-1)
       END IF
    END DO
    !
    ncuts=2*nmx
    CALL filter(nxsy,nsy,ncuts,xs,0.0_r8,eps)
    !
    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' frequency: nsy=',nsy,' ncuts=',ncuts
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(ws(n)/twomg,n=1,ncuts)
       WRITE(UNIT=nfprt,FMT=*)' period:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(1.0_r8/ws(n),n=1,ncuts)
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' xs: ierr=',ierr
       DO nn=1,nmx
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(xx(nn,n),n=1,nmx)
       END DO
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' xs:'
       DO n=1,nsy
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(xs(n,nn),nn=1,MIN(6_i8,ncuts))
       END DO
    END IF
  END SUBROUTINE symg0






  SUBROUTINE symrg(nxsy,nsy,lmax,mmax,nmd,ipr,ncuts, &
       eps,twomg,percut,alfa,beta,gama, &
       ws,wk,es,xs)
    !
    INTEGER, INTENT(IN) ::  nxsy,lmax,mmax,nmd,ipr
    INTEGER(KIND=i8), INTENT(IN) ::  nsy
    INTEGER(KIND=i8), INTENT(OUT) ::  ncuts
    REAL(KIND=r8), INTENT(IN) :: eps,twomg,percut
    REAL(KIND=r8), INTENT(IN) :: alfa(*),beta(*),gama(*)
    REAL(KIND=r8), INTENT(OUT) :: ws(*),wk(*)
    REAL(KIND=r8), INTENT(OUT) :: es(nxsy,*),xs(nxsy,*)
    !
    INTEGER ::  n,nn,mm,jj,ierr
    !
    !   symmetric case
    !
    DO nn=1,nxsy
       DO mm=1,nxsy
          es(mm,nn)=0.0_r8
       END DO
       ws(nn)=0.0_r8
       wk(nn)=0.0_r8
    END DO
    !
    DO n=1,lmax
       es(n,n)=alfa(2*n)
    END DO
    DO n=1,lmax
       nn=2*n
       jj=lmax+n
       es(n,jj)=beta(nn)
       es(jj,n)=es(n,jj)
       IF (n.LT.lmax .OR. nmd.NE.1) THEN
          es(n,jj+1)=beta(nn+1)
          es(jj+1,n)=es(n,jj+1)
       END IF
    END DO
    DO n=1,mmax
       nn=2*n-1
       jj=lmax+n
       es(jj,jj)=alfa(nn)
       mm=jj+mmax
       es(jj,mm)=gama(nn)
       es(mm,jj)=es(jj,mm)
    END DO
    !
    IF (ipr .GE. 3) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' es:'
       DO n=1,nsy
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(es(n,nn),nn=1,nsy)
       END DO
    END IF
    !
    CALL tred2(nxsy,nsy,es,ws,wk,xs)
    CALL tql2(nxsy,nsy,ws,wk,xs,eps,ierr)
    !
    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' ws: ierr=',ierr
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(ws(n),n=1,nsy)
    END IF
    !
    !   reordering frequencies
    !
    CALL order(nxsy,nsy,ws,wk,xs,es,percut,ncuts)
    CALL filter(nxsy,nsy,ncuts,xs,0.0_r8,eps)
    !
    IF (ipr .GE. 1) THEN
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' frequency: nsy=',nsy,' ncuts=',ncuts
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(ws(n)/twomg,n=1,ncuts)
       WRITE(UNIT=nfprt,FMT=*)' period:'
       WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(1.0_r8/ws(n),n=1,ncuts)
       WRITE(UNIT=nfprt,FMT=*)' '
       WRITE(UNIT=nfprt,FMT=*)' xs:'
       DO n=1,nsy
          WRITE(UNIT=nfprt,FMT='(1p,6g12.5)')(xs(n,nn),nn=1,MIN(6_i8,ncuts))
       END DO
    END IF
  END SUBROUTINE symrg






  SUBROUTINE vereig(gg,siman,eigvc,col,vec,val,eps,matz)
    !
    INTEGER, INTENT(IN) :: matz
    REAL(KIND=r8), INTENT(OUT) ::  vec(kmax,kmax),val(kmax)
    REAL(KIND=r8), INTENT(OUT) ::  col(kmax)
    REAL(KIND=r8), INTENT(INOUT) ::  gg(kmax,kmax),eigvc(kmax,kmax)
    REAL(KIND=r8), INTENT(IN) ::  eps,siman
    !
    INTEGER :: kk(kmax)
    REAL(KIND=r8) :: eigvr(kmax),eigvi(kmax),wk1(kmax),wk2(kmax)
    !
    INTEGER :: k,ier,i,j,kkk
    REAL(KIND=r8) soma,rmax,e20
    !
    e20=-1.0e20_r8
    !
    CALL rg(kmax,kmax,gg,eigvr,eigvi,matz,eigvc,ier,eps,wk1,wk2)
    !
    DO k=1,kmax
       kk(k)=0
       col(k)=siman*eigvr(k)
       soma=0.0_r8
       DO j=1,kmax
          soma=soma+eigvc(j,k)*eigvc(j,k)
       END DO
       !* soma=length of eigenvector k
       soma=1.0_r8/SQRT(soma)
       DO j=1,kmax
          eigvc(j,k)=soma*eigvc(j,k)
       END DO
    END DO
    !
    !   eigenvalues now have unit length.k th vector is eigvc(j,k)
    !   eigenvalues are now in col(k)
    !   next arrange in descending order
    !
    DO j=1,kmax
       rmax=e20
       kkk=0
       DO k=1,kmax
          IF(ABS(col(k)).GT.rmax) THEN
             kkk=k
             rmax=ABS(col(k))
          END IF
       END DO
       val(j)=col(kkk)
       col(kkk)=0.0_r8
       DO i=1,kmax
          vec(i,j)=eigvc(i,kkk)
       END DO
       kk(j)=kkk
    END DO
  END SUBROUTINE vereig







  SUBROUTINE w3fa03(press,temp)
    !
    REAL(KIND=r8), INTENT(IN) :: press
    REAL(KIND=r8), INTENT(OUT) ::  temp
    !
    !    this subroutine (w3fa03) computes the standard
    !    temperature (deg k) given the
    !    pressure in mb.
    !     u. s. standard atmosphere, 1962
    !    icao std atm to 20km
    !    proposed extension to 32km
    !    not valid for  pressure.lt.8.68mb
    !
    REAL(KIND=r8) grav,piso,ziso,salp,pzero,t0,alp, &
         ptrop,tstr,htrop
    REAL(KIND=r8) rovg,fkt,ar,pp0,height
    !
    DATA grav /9.80665e0_r8/, &
         piso /54.7487e0_r8/, ziso /20000.0e0_r8/, salp /-0.0010e0_r8/, &
         pzero /1013.25e0_r8/, t0 /288.15e0_r8/, alp /0.0065e0_r8/, &
         ptrop/226.321e0_r8/, tstr/216.65e0_r8/, htrop /11000.0e0_r8/
    !
    rovg=gasr/grav
    fkt=rovg*tstr
    ar=alp*rovg
    pp0=pzero**ar
    !
    IF(press.GE.piso.AND.press.LE.ptrop) THEN
       !
       !     compute isothermal cases
       !
       temp = tstr
    ELSE
       IF(press.LT.piso) THEN
          !
          !     compute lapse rate=-.0010_r8 cases
          !
          ar=salp*rovg
          pp0=piso**ar
          height=(tstr/(pp0*salp))*(pp0-(press**ar))+ziso
          temp=tstr-(height-ziso)*salp
       ELSE
          height=(t0/(pp0*alp))*(pp0-(press**ar))
          temp=t0-height*alp
       END IF
    END IF
  END SUBROUTINE w3fa03

  ! how many normal modes

  SUBROUTINE SetMods(gh)
    REAL(KIND=r8), INTENT(IN) :: gh(:)
    REAL(KIND=r8), PARAMETER :: HnCut=1000.0_r8
    CHARACTER(LEN=*), PARAMETER :: h="**(SetMods)**"
    INTEGER :: k
    mods=0
    DO k=1,kmax
       IF (gh(k)/grav > HnCut) THEN
          mods=mods+ 1
          WRITE(UNIT=nfprt,FMT='(A,I3,2(A,F8.2))') ' n = ', k, ' HnCut = ', HnCut, ' Hn = ', gh(k)/grav
       END IF
    END DO
    IF (mods == 0) THEN
       WRITE(UNIT=nfprt,FMT='(A)') ' ERROR: The Equivalent Heights of Normal Modes is Wrong'
       STOP 'SetMods  ==> (mods == 0)'
    END IF
  END SUBROUTINE SetMods
END MODULE NonLinearNMI
