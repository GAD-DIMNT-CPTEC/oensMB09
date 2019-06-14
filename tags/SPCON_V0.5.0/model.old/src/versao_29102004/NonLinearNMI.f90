!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE NonLinearNMI
  USE Constants, ONLY: gasr, grav, tov, er,  pai, eriv, twomg, pscons
  USE Utils, ONLY: &
       Rg,         &
       Tql2,       &
       Tred2,      &
       IJtoIBJB,   &
       IBJBtoIJ
      
  USE SpecDynamics, ONLY: tm, sv, am, bm, cm, p1, p2, h1, h2, Bmcm, dk, hm
  USE IOLowLevel, ONLY: GetUnit, ReadGetNFTGZ
  USE ModTimeStep, ONLY: TimeStep, SfcGeoTrans
  USE Sizes, ONLY: mnMax, kmax, nmax, mmax, imax, jmax, mnMap, snnp1, &
      ibmax , &
      jbmax,  &
      cl   , &
      si   , &
      sl   , &
      del 

  USE FieldsDynamics, ONLY: qdivt, qdivp, &
      qrott, qrotp, &
      qtmpt, qtmpp, &
      qlnpt, qlnpp, &
      qqp,   qdiaten, qgzs
  USE FieldsPhysics, ONLY: &
      tg1   , &
      tg2   , &
      tg3   , &
      zorl  , &
      tg1_in, &
      tg2_in, &
      tg3_in, &
      zorl_in, &
      avisd , &
      gtsea , &
      geshem, &
      sheleg, & 
      soilm , &      
      avisd_in , &
      gtsea_in , &
      geshem   , &
      sheleg_in, &
      soilm_in 
  USE Options, ONLY: &
      delt  ,&
      nstep ,&
      nfin0 ,&
      nfin1 ,&
      ifalb ,&
      ifsst ,&
      ifslm ,&
      ifsnw ,&
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
      istrt     
  USE InputOutput, ONLY: &
      ifprt ,&
      nfprt ,&
      getsbc,&
      gread ,&
      gread4,&
      fsbc
  USE Diagnostics, ONLY: &
      rmsgt

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Nlnmi
  PUBLIC :: Diaten   
  PUBLIC :: Getmod  

  INTEGER :: mods, niter

CONTAINS


  !
  ! diaten :computation of the diabatic terms for normal mode
  !         initialization.
  !


  SUBROUTINE Diaten(slagr,fName0,fName1,&
                   ifday, tod, idate, idatec,kt,ktm)

    LOGICAL,             INTENT(IN) :: slagr
    CHARACTER(LEN=200),  INTENT(IN) :: fName0
    CHARACTER(LEN=200),  INTENT(IN) :: fName1
    INTEGER, INTENT(OUT) :: ifday
    REAL   , INTENT(OUT) :: tod
    INTEGER, INTENT(OUT), DIMENSION(4) :: idate, idatec 
    INTEGER, INTENT(INOUT) :: kt,ktm
    !
    INTEGER :: mfalb, mfsst, mfslm, mfsnw
    INTEGER :: ierr,kdt
    REAL    :: fa, fb,fb1
    REAL    :: tice=271.16e0
    fsbc=.false.
    !
    !     start reading initial values
    !
    OPEN(unit=nfin0, file=fName0, form='unformatted', &
         ACTION='read', IOSTAT=ierr)
    !
    !**(SB & JP)** nao faz sentido nfin0 /= nfin1, aqui dentro
    !o else deveria ser removido, pois nfin0 /= nfin1 significa
    !restart; nele, nao faz sentido chamar diaten
    !
    IF (nfin0 .ne. nfin1) THEN
      OPEN(unit=nfin1, file=fName1, form='unformatted', &
           ACTION='read', IOSTAT=ierr)
    ENDIF
    IF (nfin0 .eq. nfin1) THEN
       CALL gread4 (nfin0, ifday, tod  , idate, idatec,qgzs  ,qlnpp , &
                    qtmpp, qdivp, qrotp, qqp  , sl    , si   ,dodyn , &
                    nfdyn)
       CALL rmsgt(qlnpp,qdivp,qtmpp,qrotp,del,qqp)
       REWIND nfin0
     ELSE
       CALL gread (nfin0 , ifday, tod  , idate, idatec,qgzs  , qlnpp, &
                   qtmpp , qdivp, qrotp, qqp  , sl    , si)
       CALL rmsgt(qlnpp,qdivp,qtmpp,qrotp,del,qqp)
       fb1 = 1.
       fa = 0.
       fb = 1.
!$OMP PARALLEL
       CALL TimeStep(fb1,fa,fb,slagr,.FALSE.,.TRUE.,.FALSE.,.FALSE.,dt,jdt, &
                     ifday,tod,idatec)
!$OMP END PARALLEL
       CALL gread (nfin1 , ifday, tod  , idate, idatec,qgzs  , qlnpp, &
                   qtmpp , qdivp, qrotp, qqp  , sl    , si)
       CALL rmsgt(qlnpp,qdivp,qtmpp,qrotp,del,qqp)
       REWIND nfin0
       REWIND nfin1
    ENDIF
    !
    !     cold start: reset precip. to zero.
    !
    geshem = 0.0
    !
    !     calculates laplacian of topography
    !
    CALL SfcGeoTrans()
    !
    !     read climatology data
    !
    CALL ReadGetNFTGZ(nftgz0,tg1_in,tg2_in,tg3_in,zorl_in)
    CALL IJtoIBJB(tg1_in ,tg1  )
    CALL IJtoIBJB(tg2_in ,tg2  )
    CALL IJtoIBJB(tg3_in ,tg3  )
    CALL IJtoIBJB(zorl_in,zorl )
    mfalb=ifalb
    mfsst=ifsst
    mfslm=ifslm
    mfsnw=ifsnw          
    CALL IBJBtoIJ(avisd ,avisd_in )
    CALL IBJBtoIJ(gtsea ,gtsea_in )
    CALL IBJBtoIJ(soilm ,soilm_in )
    CALL IBJBtoIJ(sheleg,sheleg_in)
    CALL getsbc(iMax ,jMax ,avisd_in ,gtsea_in ,soilm_in ,sheleg_in,ifday ,tod  , &
                idate ,idatec,nfprt ,ifprt   ,mfalb ,mfsst ,mfslm, &
                mfsnw ,sstlag,intsst,fint  ,tice  ,yrl   ,monl)   
    CALL IJtoIBJB(avisd_in ,avisd )
    CALL IJtoIBJB(gtsea_in ,gtsea )
    CALL IJtoIBJB(soilm_in ,soilm )
    CALL IJtoIBJB(sheleg_in,sheleg)
  !
  !     check files
  !     if nfin0=nfin1   then  cold start
  !
  IF(nfin0.eq.nfin1)THEN
    !
    !     cold start (at first delt/4 ,then delt/2 )
    !
    dt= delt /4.0

  ! filter arguments for first time step

    fa = 0.0
    fb = 1.0
    fb1 = 1.0

    DO jdt=1,2
      istrt=jdt
      kdt=jdt
      !     
      !     calculate matrices for semi-implicit integration
      !     
      CALL bmcm(dt, slagr)

      ! perform time step 
!$OMP PARALLEL
      CALL TimeStep(fb1,fa,fb,slagr,.FALSE.,.FALSE.,.FALSE.,.FALSE.,dt,jdt, & 
                    ifday,tod, idatec)
!$OMP END PARALLEL


      ! prepare next time step, including filter arguments

      dt=dt*2.0
      ktm=kt   
      fb1 = 0.0
    END DO
  END IF
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
  fb1 = 0.0
  !
  ! time step loop
  !
  DO jdt=1,nstep
      !
      !     step loop starts
      !     
      kdt=jdt     
      ! perform time step 
!$OMP PARALLEL
      CALL TimeStep(fb1,fa,fb,slagr,.FALSE.,.FALSE.,.TRUE.,.FALSE.,dt,jdt, &
                    ifday,tod,idatec)
!$OMP END PARALLEL

      ktm=kt
      !
      fb1 = fb
  ENDDO
  qdiaten = qdiaten / (2.0*dt*nstep)
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



  SUBROUTINE Nlnmi(nlnminit,diabatic,slagr,fName, & ! fName2,&
                   ifday, tod, idatec,ktm)
    LOGICAL,          INTENT(IN) :: nlnminit
    LOGICAL,          INTENT(IN) :: diabatic
    LOGICAL,          INTENT(IN) :: slagr
    CHARACTER(LEN=*), INTENT(IN) :: fName
!    CHARACTER(LEN=*), INTENT(IN) :: fName2
    INTEGER, INTENT(IN) :: ifday
    REAL   , INTENT(IN) :: tod
    INTEGER, INTENT(IN), DIMENSION(4) :: idatec 
    INTEGER, INTENT(INOUT) :: ktm
    CHARACTER(LEN=*), PARAMETER :: h="**(ReadNNMIFile)**"
    !
    REAL, PARAMETER :: HnCut=1000.0
    REAL :: dt, tor, fa, fb, fb1, fcon 
    REAL :: eigg(kmax,kmax),eiggt(kmax,kmax)
    REAL :: dotpro(kmax),gh(kmax),verin(kmax),to(kmax)
    REAL :: qgenp(2*mnMax,kmax)
    INTEGER :: iter, k, j, l, kmod, lev, nfnmi
!    INTEGER :: nfdbh

    CALL GetUnit(nfnmi)
    OPEN(unit=nfnmi, file=fName, form='unformatted', &
         ACTION='read')
    READ(nfnmi)eigg,eiggt,gh,dotpro,to
    REWIND (nfnmi)
    WRITE (nfprt, '(/,A)') ' From Nlnmi :'
    mods=0
    DO k=1,kmax
       IF (gh(k)/grav > HnCut) THEN
          mods=mods+ 1
          WRITE (nfprt, '(A,I3,2(A,F8.2))') ' n = ', k, ' HnCut = ', HnCut, ' Hn = ', gh(k)/grav
       END IF
    END DO
    IF (mods == 0) THEN
       WRITE (nfprt, '(A)') ' ERROR: The Equivalent Heights of Normal Modes is Wrong'
       STOP ' Nlnmi : (mods == 0)'
    END IF
    IF (mods <= 2) THEN
       niter=2
    ELSE
       niter=3
    END IF
    WRITE (nfprt, '(/,2(A,I3),/)') ' mods = ', mods, ' niter = ', niter

!    IF (diabatic) THEN
!       CALL GetUnit(nfdbh)
!       OPEN(unit=nfdbh, file=fName2, form='unformatted', &
!            ACTION='read')
!    ENDIF
    CALL bmcm(1.,slagr)
    !
    !  Compute the iterations of the non-linear initialization
    !  -------------------------------------------------------
    DO iter=1,niter
       !
       !  Read in the vertical Normal Modes and related stuff
       !  ---------------------------------------------------
       READ(nfnmi)eigg,eiggt,gh,dotpro,to
       DO k=1, kmax
          verin(k)=0.
          DO j=1, kmax
             verin(k)=verin(k)+sv(j)*eigg(j,k)
          END DO
       END DO
       !
       !  Compute non-linear complete tendencies
       !  --------------------------------------
       fb1 = 1.
       fa = 0.
       fb = 0.
       dt = 1.
!$OMP PARALLEL
       CALL TimeStep(fb1,fa,fb,slagr,nlnminit,.FALSE.,.FALSE.,.FALSE.,dt,0,ifday, tod, idatec)
!$OMP END PARALLEL
       !
       !     add diabatic heating rate to temperature tendency
       !     -------------------------------------------------
       IF (diabatic) THEN
!          rewind nfdbh
!          read(nfdbh) qdiaten
          qtmpt = qtmpt + qdiaten
!          rewind nfdbh
       ENDIF
       !
       !     create generalized pressure=(phi+r*to*q)(dot)
       !     ---------------------------------------------
       DO l=1, kmax
          tor=to(l)* gasr
          qgenp(:,l)=tor*qlnpt
          DO k=1, kmax
             qgenp(:,l)=qgenp(:,l) + qtmpt(:,k)*hm(l,k)
          END DO
       END DO
       CALL vertic(qdivt,eigg,eiggt,dotpro,mods,-1)
       CALL vertic(qrott,eigg,eiggt,dotpro,mods,-1)
       CALL vertic(qgenp,eigg,eiggt,dotpro,mods,-1)
       CALL primes(qrott,qdivt,qgenp,gh,mods,-1)
       CALL horiz1(qrott,qdivt,qgenp,percut,nfnmi,mods)
       CALL primes(qrott,qdivt,qgenp,gh,mods,+1)
       !
       !     compute delta(q) from composite variable
       !
       qlnpt = 0.
       DO kmod=1,mods
          fcon=verin(kmod)/gh(kmod)
          qlnpt=qlnpt-fcon*qgenp(:,kmod)
       END DO
       CALL vertic(qdivt,eigg,eiggt,dotpro,mods,+1)
       CALL vertic(qrott,eigg,eiggt,dotpro,mods,+1)
       CALL vertic(qgenp,eigg,eiggt,dotpro,mods,+1)
       !
       !     compute delta(phi) from composite variable
       !
       DO k=1, kmax
          tor=to(k)*gasr
          qgenp(:,k)=qgenp(:,k)-tor*qlnpt
       END DO
       !
       !     compute delta(t) from phi
       !
       qtmpt = 0.
       DO lev=1,kmax
          DO k=1, kmax
             qtmpt(:,lev)=qtmpt(:,lev)+tm(lev,k)*qgenp(:,k)
          END DO
       END DO

       qlnpp=qlnpp-qlnpt
       qtmpp=qtmpp-qtmpt
       qrotp=qrotp-qrott
       qdivp=qdivp-qdivt
       
       ktm=0
    END DO
    CLOSE(nfnmi)
!    IF (diabatic) CLOSE(nfdbh)
  END SUBROUTINE Nlnmi



  ! vertic : performs projection of the spectral representation of all
  !          model levels of a field onto the vertical normal modes.
  !          also performs expansion of the spectral representation
  !          of a vertically projected field into a field at all model
  !          levels using the vertical normal modes.
  !
  !     input=-1 to obtain vertical mode expansion
  !     input=+1 to obtain spet. coefs. from vertical expansion



  SUBROUTINE vertic(f,eigg,eiggt,dotpro,mods,input)
    REAL, INTENT(IN) :: eigg(kmax,kmax)
    REAL, INTENT(IN) :: eiggt(kmax,kmax)
    REAL, INTENT(IN) :: dotpro(kmax)
    REAL, INTENT(INOUT) :: f(2,mnMax,kmax)
    INTEGER, INTENT(IN) :: input, mods
    REAL :: col(2,kmax)
    INTEGER :: mn, kmod, lev
    REAL :: sum1, sum2

    IF (input .lt. 0) THEN
       DO mn=1, mnMax
          DO kmod=1,mods
             sum1=0.
             sum2=0.
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
       DO mn=1, mnMax
          DO kmod=1,mods
             col(1,kmod)=f(1,mn,kmod)
             col(2,kmod)=f(2,mn,kmod)
          END DO
          DO lev=1, kmax
             f(1,mn,lev)=0.
             f(2,mn,lev)=0.
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



  SUBROUTINE primes(vord,divd,comd,gh,mods,input)
    REAL, INTENT(INOUT) :: vord(2,mnMax,kmax)
    REAL, INTENT(INOUT) :: divd(2,mnMax,kmax)
    REAL, INTENT(INOUT) :: comd(2,mnMax,kmax)
    REAL, INTENT(IN) :: gh(kmax)
    INTEGER, INTENT(IN) :: input, mods
    REAL :: w(mnMax), t, divdr, divdi
    INTEGER :: mn, k
    IF (input .lt. 0) THEN
       w(1)=0.
       DO  mn=2,mnMax
          w(mn)=1./SQRT(FLOAT(snnp1(2*mn-1)))
       END DO
       DO k=1,mods
          t=1./(er*SQRT(gh(k)))
          DO mn=1,mnMax
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
       w(1)=0.0 
       DO mn=2,mnMax
          w(mn)=SQRT(FLOAT(snnp1(2*mn-1)))
       END DO
       DO k=1,mods
          t=er*SQRT(gh(k))
          DO mn=1,mnMax
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



  SUBROUTINE horiz1(vord,divd,comd,v,nf,mods)
    REAL, INTENT(INOUT) :: vord(2,mnMax,kmax)
    REAL, INTENT(INOUT) :: divd(2,mnMax,kmax)
    REAL, INTENT(INOUT) :: comd(2,mnMax,kmax)
    REAL, INTENT(IN) :: v
    INTEGER, INTENT(IN) :: mods, nf
    !
    REAL :: sdot(2,mnMax*kmax), adot(2,mnMax*kmax)
    REAL :: per(3*(mMax+1)/2),g(3*(mMax+1)/2*3*(mMax+1)/2)
    INTEGER :: nn, k, modes, l, ll, jsod, jsev, jevpod
    INTEGER :: nends, nenda, lx, ir, jg, nas, nnmax
    !
    DO modes=1,mods
       DO ll=1,mmax 
          k=0
          l=ll-1
          nnmax=mmax+1-ll
          jsod=nnmax/2
          jsev=nnmax-jsod
          jevpod=jsev+jsod
          nends=jevpod+jsev
          nenda=jevpod+jsod
          DO nn=2,nnmax,2
             lx=mnMap(ll,nn+ll-1)
             k=k+1
             DO ir=1,2
                sdot(ir,k       )=vord(ir,lx,modes)
                adot(ir,k+jsev  )=divd(ir,lx,modes)
                adot(ir,k+jevpod)=comd(ir,lx,modes)
             END DO
          END DO
          k=0
          DO nn=1,nnmax,2
             lx=mnMap(ll,nn+ll-1)
             k=k+1
             DO ir=1,2
                adot(ir,k       )=vord(ir,lx,modes)
                sdot(ir,k+jsod  )=divd(ir,lx,modes)
                sdot(ir,k+jevpod)=comd(ir,lx,modes)
             END DO
          END DO
          READ(nf)jg,nas
          IF (jg.ne.0) CALL horiz2(sdot,nas,per,g,jg,v,l,nf)
          READ(nf)jg,nas
          IF (jg.ne.0) CALL horiz2(adot,nas,per,g,jg,v,l,nf)
          k=0
          DO nn=2,nnmax,2
             lx=mnMap(ll,nn+ll-1)
             k=k+1
             DO ir=1,2
                vord(ir,lx,modes)=sdot(ir,k)
                divd(ir,lx,modes)=adot(ir,k+jsev)
                comd(ir,lx,modes)=adot(ir,k+jevpod)
             END DO
          END DO
          k=0
          DO nn=1,nnmax,2
             lx=mnMap(ll,nn+ll-1)
             k=k+1
             DO ir=1,2
                vord(ir,lx,modes)=adot(ir,k)
                divd(ir,lx,modes)=sdot(ir,k+jsod)
                comd(ir,lx,modes)=sdot(ir,k+jevpod)
             END DO
          END DO
       END DO
    END DO
    REWIND nf
  END SUBROUTINE horiz1



  ! horiz2 : calculates for one zonal wave number the Machenhauer
  !          adjustment to the vertically projected, properly scaled
  !          and rearranged spectral coefficients of vorticity,
  !          divergence, and composite mass variable.



  SUBROUTINE horiz2(dot,nas,per,g,jg,percut,l,nfg)
    INTEGER, INTENT(IN) :: jg, nas, nfg, l
    REAL, INTENT(INOUT) :: dot(2,nas)
    REAL, INTENT(OUT) :: per(jg), g(jg,nas)
    REAL, INTENT(IN) :: percut
    REAL :: period, dif, difcut
    REAL :: y(2,3*(mMax+1)/2), yi, yr
    INTEGER :: init(3*(mMax+1)/2)
    INTEGER :: ndho, i, j, k, n
    !
    !     nas=vector size of sym. or  asy. tendencies stored in dot
    !     jg=nunber of gravity modes. jg=jcap for l=0 sym and asy cases
    !     for l.ne.0 jg=jcap2 for sym,jg=jcap for asy.
    !     per stores periods of gravity modes,g stores eigenvectors
    !     
    READ(nfg) per,g
    !
    dif=dk
    ndho=ndord/2
    DO j=1,jg
       init(j)=0
       y(1,j)=0.  
       y(2,j)=0.  
    END DO
    n=l
    DO i=1,jg
       n=n+1
       period=ABS(per(i))
       IF (period-percut.le.0.) THEN
          !     
          !     arbitray even order horizontal diffusion now used.
          !     smaller "tweek" constant used to avoid inaccuracies when
          !     reciprocal is taken.
          !     ________________________________________________________
          difcut=dif*(FLOAT(n*(n+1))**ndho) + 1.0e-7
          difcut=.5/difcut
          IF (difcut-period.ge.0.) THEN
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
       dot(1,j)=0.  
       dot(2,j)=0.  
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
    REAL,    PARAMETER :: eps=2.**(-50)
    CHARACTER(LEN=*), PARAMETER :: h="**(GetMod)**"
    INTEGER :: nfg
    REAL    :: gh(kmax)
    
    CALL GetUnit(nfg)
    OPEN(nfg,FILE=fName,FORM='unformatted',STATUS='unknown')
    CALL Vermod(nfg,gh,eps,matz,slagr,rsettov)
    CALL Hormod(nfg,gh,mods,eps)
    CLOSE(nfg)
  END SUBROUTINE Getmod






  SUBROUTINE Vermod(nfg,gh,eps,matz,slagr,rsettov)
    INTEGER, INTENT(IN ) :: nfg
    REAL,    INTENT(OUT) :: gh(kmax)
    REAL,    INTENT(IN ) :: eps
    INTEGER, INTENT(IN ) :: matz
    LOGICAL, INTENT(IN ) :: slagr
    LOGICAL, INTENT(IN ) :: rsettov

    INTEGER :: j
    INTEGER :: k
    REAL    :: p
    REAL    :: siman
    REAL    :: soma
    REAL    :: er2
    REAL    :: eigg(kmax,kmax)
    REAL    :: eiggt(kmax,kmax)
    REAL    :: eigvc(kmax,kmax)
    REAL    :: dotpro(kmax)
    REAL    :: to(kmax)
    REAL    :: g(kmax,kmax)
    REAL    :: gt(kmax,kmax)
 
    IF(rsettov) THEN
       to = tov
       DO k=1,kmax
          p=sl(k)*pscons
          CALL W3fa03(p,tov(k))
       END DO
    END IF
    CALL bmcm(1.,slagr)
    er2=er*er
    g = cm * er2
    gt = TRANSPOSE (g)
    siman=-1.0
    CALL Vereig(g,siman,eigvc,cl,eigg,gh,eps,matz)
    siman=1.0
    CALL Vereig(gt,siman,eigvc,cl,eiggt,dotpro,eps,matz)

    ! dotpro=inverse dot prod. of eigenvec(g)*eigenvec(gtranspose)

    DO k=1,kmax
       soma=0.0 
       DO j=1,kmax
          soma=soma+eigg(j,k)*eiggt(j,k)
       END DO
       dotpro(k)=1.0/soma
    END DO
    WRITE(nfg)eigg,eiggt,gh,dotpro,tov
    IF (rsettov) THEN
       tov = to
    END IF
  END SUBROUTINE Vermod






  SUBROUTINE Hormod(nfg,gh,mods,eps)
    INTEGER, INTENT(IN) :: nfg
    INTEGER, INTENT(IN) :: mods
    REAL,    INTENT(IN) :: eps
    REAL,    INTENT(IN) :: gh(mods)

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
    INTEGER :: nsy
    INTEGER :: nas
    INTEGER :: n
    INTEGER :: ncuts
    INTEGER :: ncuta
    INTEGER :: mend
    INTEGER :: modd
    INTEGER :: lend
    INTEGER :: kend
    REAL    :: alfa(mMax)
    REAL    :: beta(mMax)
    REAL    :: gama(mMax)
    REAL    :: dgl(mMax)
    REAL    :: sdg(mMax)
    REAL    :: xx(mMax,mMax)
    REAL    :: wk(3*(mMax+1)/2)
    REAL    :: ws(3*(mMax+1)/2)
    REAL    :: wa(3*(mMax+1)/2)
    REAL    :: xs(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL    :: xa(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL    :: es(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL    :: ea(3*(mMax+1)/2*3*(mMax+1)/2)
    REAL    :: rm
    REAL    :: rn

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
          rm=FLOAT(m-1)
          DO n=1,nnmax
             rn=rm+FLOAT(n-1)
             IF (rn .EQ. 0.0 ) THEN
                alfa(n)=0.0 
                beta(n)=0.0 
                gama(n)=0.0 
             ELSE
                alfa(n)=twomg*rm/(rn*(rn+1.0))
                beta(n)=(twomg/rn)*SQRT((rn*rn-1.0)*(rn*rn-rm*rm)/&
                     (4.0*rn*rn-1.0))
                gama(n)=eriv*SQRT(rn*(rn+1.0)*gh(k))
             END IF
          END DO
          IF (m .EQ. 1) THEN

             ! symmetric case

             CALL symg0(nxsy,nsy,mMax,lmax,klmx,nmd,ipr,ncuts, &
                  eps,twomg,beta,gama, &
                  ws,sdg,dgl,xs,xx)
             CALL record(nfg,nxsy,ncuts,nsy,ws,xs,wk,es)

             ! asymmetric case

             CALL asyg0(nxas,nas,mMax,lmax,klmx,mmmax,nmd,ipr, &
                  ncuta,eps,twomg,beta,gama, &
                  wa,sdg,dgl,xa,xx)
             CALL record(nfg,nxas,ncuta,nas,wa,xa,wk,ea)
          ELSE

             ! symmetric case

             CALL symrg(nxsy,nsy,lmax,mmmax,nmd,ipr,ncuts, &
                  eps,twomg,percut,alfa,beta,gama, &
                  ws,wk,es,xs)
             CALL record(nfg,nxsy,ncuts,nsy,ws,xs,wk,es)

             ! asymmetric case

             CALL asyrg(nxas,nas,lmax,mmmax,nmd,ipr,ncuta, &
                  eps,twomg,percut,alfa,beta,gama, &
                  wa,wk,ea,xa)
             CALL record(nfg,nxas,ncuta,nas,wa,xa,wk,ea)
          END IF
       END DO
    END DO
  END SUBROUTINE Hormod






  SUBROUTINE asyg0(nxas,nas,nend1,lmax,klmx,mmax,nmd,ipr, &
       ncuta,eps,twomg,beta,gama,wa,sdg,dgl,xa,xx)
    INTEGER, INTENT(IN ) :: nxas
    INTEGER, INTENT(IN ) :: nas
    INTEGER, INTENT(IN ) :: nend1
    INTEGER, INTENT(IN ) :: lmax
    INTEGER, INTENT(IN ) :: klmx
    INTEGER, INTENT(IN ) :: mmax
    INTEGER, INTENT(IN ) :: nmd
    INTEGER, INTENT(IN ) :: ipr
    INTEGER, INTENT(OUT) :: ncuta
    REAL,    INTENT(IN ) :: eps
    REAL,    INTENT(IN ) :: twomg
    REAL,    INTENT(IN ) :: beta(:)
    REAL,    INTENT(IN ) :: gama(:)
    REAL,    INTENT(OUT) :: wa(:)
    REAL,    INTENT(OUT) :: sdg(:)
    REAL,    INTENT(OUT) :: dgl(:)
    REAL,    INTENT(OUT) :: xa(nxas,*)
    REAL,    INTENT(OUT) :: xx(nend1,*)

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
       WRITE(*,*)' '
       WRITE(*,*)' sdg:'
       WRITE(*,'(1p,6g12.5)')(sdg(n),n=1,nmx)
       WRITE(*,*)' dga:'
       WRITE(*,'(1p,6g12.5)')(dgl(n),n=1,nmx)
    END IF

    CALL ident(nend1,nmx,xx)
    CALL tql2(nend1,nmx,dgl,sdg,xx,eps,ierr)

    DO j=1,nmx
       jj=2*j
       wa(jj-1)=-SQRT(dgl(j))
       wa(jj)=SQRT(dgl(j))
       xa(1,jj-1)=0.0
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
    CALL filter(nxas,nas,ncuta,xa,0.0,eps)

    IF (ipr .GE. 1) THEN
       WRITE(*,*)' '
       WRITE(*,*)' frequency: nas=',nas,' ncuta=',ncuta
       WRITE(*,'(1p,6g12.5)')(wa(n)/twomg,n=1,ncuta)
       WRITE(*,*)' period:'
       WRITE(*,'(1p,6g12.5)')(1.0/wa(n),n=1,ncuta)
       WRITE(*,*)' '
       WRITE(*,*)' xa: ierr=',ierr
       DO n=1,nmx
          WRITE(*,'(1p,6g12.5)')(xx(n,nn),nn=1,nmx)
       END DO
       WRITE(*,*)' '
       WRITE(*,*)' xa:'
       DO n=1,nas
          WRITE(*,'(1p,6g12.5)')(xa(n,nn),nn=1,MIN(6,ncuta))
       END DO
    END IF
  END SUBROUTINE asyg0






  SUBROUTINE asyrg(nxas,nas,lmax,mmax,nmd,ipr,ncuta, &
       eps,twomg,percut,alfa,beta,gama,wa,wk,ea,xa)
    INTEGER, INTENT(IN) :: nxas,nas,lmax,mmax,nmd,ipr
    INTEGER, INTENT(OUT) :: ncuta
    REAL, INTENT(IN) :: eps,twomg,percut
    REAL, INTENT(IN) ::alfa(*),beta(*),gama(*)
    REAL, INTENT(OUT) :: wa(*),wk(*)
    REAL, INTENT(OUT) :: ea(nxas,*),xa(nxas,*)
    !
    INTEGER :: n,nn,mm,jj,ierr
    !
    !   asymmetric case
    !
    DO nn=1,nxas
       DO mm=1,nxas
          ea(mm,nn)=0.
       END DO
       wa(nn)=0.
       wk(nn)=0.
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
       WRITE(*,*)' '
       WRITE(*,*)' ea:'
       DO n=1,nas
          WRITE(*,'(1p,6g12.5)')(ea(n,nn),nn=1,nas)
       END DO
    END IF
    !
    CALL tred2(nxas,nas,ea,wa,wk,xa)
    CALL tql2(nxas,nas,wa,wk,xa,eps,ierr)
    !
    IF (ipr .GE. 1) THEN
       WRITE(*,*)' '
       WRITE(*,*)' wa: ierr=',ierr
       WRITE(*,'(1p,6g12.5)')(wa(n),n=1,nas)
    END IF
    !
    !   reordering frequencies
    !
    CALL order(nxas,nas,wa,wk,xa,ea,percut,ncuta)
    CALL filter(nxas,nas,ncuta,xa,0.0,eps)
    !
    IF (ipr .GE. 1) THEN
       WRITE(*,*)' '
       WRITE(*,*)' frequency: nas=',nas,' ncuta=',ncuta
       WRITE(*,'(1p,6g12.5)')(wa(n)/twomg,n=1,ncuta)
       WRITE(*,*)' period:'
       WRITE(*,'(1p,6g12.5)')(1.0/wa(n),n=1,ncuta)
       WRITE(*,*)' '
       WRITE(*,*)' xa:'
       DO n=1,nas
          WRITE(*,'(1p,6g12.5)')(xa(n,nn),nn=1,MIN(6,ncuta))
       END DO
    END IF
  END SUBROUTINE asyrg






  SUBROUTINE filter(nm,ni,nj,a,b,c)
    !
    INTEGER, INTENT(IN) ::  nm,ni,nj
    REAL, INTENT(IN) ::  b,c
    REAL, INTENT(INOUT) :: a(nm,*)
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
    REAL, INTENT(OUT) ::  z(nm,*)
    !
    INTEGER ::  i,j 
    REAL :: sqrt2
    !
    sqrt2=1.0/SQRT(2.0)
    DO i=1,n
       DO j=1,n
          z(i,j)=0.0 
       END DO
       z(i,i)=sqrt2
    END DO
  END SUBROUTINE ident






  SUBROUTINE order(nm,n,fr,fw,z,zw,percut,nf)
    !
    INTEGER, INTENT(IN) :: nm,n
    REAL, INTENT(IN) :: percut
    INTEGER, INTENT(OUT) :: nf 
    REAL, INTENT(INOUT) :: fr(nm),fw(nm),z(nm,*),zw(nm,*)
    !
    INTEGER :: nm1,k,j,j1,i,jc,nc
    REAL :: chg
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
    IF (percut .LE. 0.0) THEN
       nf=n
       RETURN
    END IF
    !
    nc=0
    DO j=1,n
       IF (ABS(1.0/fr(j)) .GT. percut) nc=j
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
          z(i,j)=0.0
       END DO
       fr(i)=0.0
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






  SUBROUTINE record(nf,nx,nc,nm,ww,xx,pp,gg)
    !
    INTEGER, INTENT(IN) :: nf,nx,nc,nm
    REAL, INTENT(IN) :: ww(nx),xx(nx,nx)
    REAL, INTENT(OUT) :: pp(nc),gg(nc,nm)
    !
    INTEGER :: n,nn
    REAL :: aux1, aux2
    !
    ! WRITE(*,*)' from record: nx,nc,nm:',nx,nc,nm
    WRITE(nf)nc,nm
    IF (nc .EQ. 0) RETURN
    !
    pp = 0.
    gg = 0.
    !
    aux1 =0.
    aux2 =0.
    DO n=1,nc
       pp(n)=1.0/ww(n)
       aux1=MAX(ABS(pp(n)),aux1)
       DO nn=1,nm
          gg(n,nn)=xx(nn,n)
          aux2=MAX(ABS(gg(n,nn)),aux2)
       END DO
    END DO
    WRITE(nf)pp,gg
  END SUBROUTINE record







  SUBROUTINE symg0(nxsy,nsy,nend1,lmax,klmx,nmd,ipr,ncuts, &
       eps,twomg,beta,gama, &
       ws,sdg,dgl,xs,xx)
    INTEGER, INTENT(IN) ::  nxsy,nsy,nend1,lmax,klmx,nmd,ipr
    INTEGER, INTENT(OUT) ::  ncuts
    REAL, INTENT(IN) :: eps,twomg
    REAL, INTENT(IN) :: beta(*),gama(*)
    REAL, INTENT(OUT) :: ws(*),sdg(*),dgl(*)
    REAL, INTENT(OUT) :: xs(nxsy,*),xx(nend1,*)
    ! 
    INTEGER :: n,nmx,nn,j,jj,ierr
    ! 
    !     symmetric case
    ! 
    nmx=1
    n=1
    sdg(n)=0.
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
       WRITE(*,*)' '
       WRITE(*,*)' sdg:'
       WRITE(*,'(1p,6g12.5)')(sdg(n),n=1,nmx)
       WRITE(*,*)' dgs:'
       WRITE(*,'(1p,6g12.5)')(dgl(n),n=1,nmx)
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
       xs(lmax+1,jj-1)=0.0
       xs(lmax+1,jj)=xs(lmax+1,jj-1)
       xs(klmx+1,jj-1)=0.0
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
    CALL filter(nxsy,nsy,ncuts,xs,0.0,eps)
    ! 
    IF (ipr .GE. 1) THEN
       WRITE(*,*)' '
       WRITE(*,*)' frequency: nsy=',nsy,' ncuts=',ncuts
       WRITE(*,'(1p,6g12.5)')(ws(n)/twomg,n=1,ncuts)
       WRITE(*,*)' period:'
       WRITE(*,'(1p,6g12.5)')(1.0/ws(n),n=1,ncuts)
       WRITE(*,*)' '
       WRITE(*,*)' xs: ierr=',ierr
       DO nn=1,nmx
          WRITE(*,'(1p,6g12.5)')(xx(nn,n),n=1,nmx)
       END DO
       WRITE(*,*)' '
       WRITE(*,*)' xs:'
       DO n=1,nsy
          WRITE(*,'(1p,6g12.5)')(xs(n,nn),nn=1,MIN(6,ncuts))
       END DO
    END IF
  END SUBROUTINE symg0






  SUBROUTINE symrg(nxsy,nsy,lmax,mmax,nmd,ipr,ncuts, &
       eps,twomg,percut,alfa,beta,gama, &
       ws,wk,es,xs)
    !
    INTEGER, INTENT(IN) ::  nxsy,nsy,lmax,mmax,nmd,ipr
    INTEGER, INTENT(OUT) ::  ncuts
    REAL, INTENT(IN) :: eps,twomg,percut
    REAL, INTENT(IN) :: alfa(*),beta(*),gama(*)
    REAL, INTENT(OUT) :: ws(*),wk(*)
    REAL, INTENT(OUT) :: es(nxsy,*),xs(nxsy,*)
    !
    INTEGER ::  n,nn,mm,jj,ierr
    !
    !   symmetric case
    !
    DO nn=1,nxsy
       DO mm=1,nxsy
          es(mm,nn)=0.
       END DO
       ws(nn)=0.
       wk(nn)=0.
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
       WRITE(*,*)' '
       WRITE(*,*)' es:'
       DO n=1,nsy
          WRITE(*,'(1p,6g12.5)')(es(n,nn),nn=1,nsy)
       END DO
    END IF
    !
    CALL tred2(nxsy,nsy,es,ws,wk,xs)
    CALL tql2(nxsy,nsy,ws,wk,xs,eps,ierr)
    !
    IF (ipr .GE. 1) THEN
       WRITE(*,*)' '
       WRITE(*,*)' ws: ierr=',ierr
       WRITE(*,'(1p,6g12.5)')(ws(n),n=1,nsy)
    END IF
    !
    !   reordering frequencies
    !
    CALL order(nxsy,nsy,ws,wk,xs,es,percut,ncuts)
    CALL filter(nxsy,nsy,ncuts,xs,0.0,eps)
    !
    IF (ipr .GE. 1) THEN
       WRITE(*,*)' '
       WRITE(*,*)' frequency: nsy=',nsy,' ncuts=',ncuts
       WRITE(*,'(1p,6g12.5)')(ws(n)/twomg,n=1,ncuts)
       WRITE(*,*)' period:'
       WRITE(*,'(1p,6g12.5)')(1.0/ws(n),n=1,ncuts)
       WRITE(*,*)' '
       WRITE(*,*)' xs:'
       DO n=1,nsy
          WRITE(*,'(1p,6g12.5)')(xs(n,nn),nn=1,MIN(6,ncuts))
       END DO
    END IF
  END SUBROUTINE symrg






  SUBROUTINE vereig(gg,siman,eigvc,col,vec,val,eps,matz)
    !
    INTEGER, INTENT(IN) :: matz
    REAL, INTENT(OUT) ::  vec(kmax,kmax),val(kmax)
    REAL, INTENT(OUT) ::  col(kmax)
    REAL, INTENT(INOUT) ::  gg(kmax,kmax),eigvc(kmax,kmax)
    REAL, INTENT(IN) ::  eps,siman
    !
    INTEGER :: kk(kmax)
    REAL :: eigvr(kmax),eigvi(kmax),wk1(kmax),wk2(kmax)
    !
    INTEGER :: k,ier,i,j,kkk
    REAL soma,rmax,e20
    !
    e20=-1.0e20
    !
    CALL rg(kmax,kmax,gg,eigvr,eigvi,matz,eigvc,ier,eps,wk1,wk2)
    !
    DO k=1,kmax
       kk(k)=0
       col(k)=siman*eigvr(k)
       soma=0.0
       DO j=1,kmax
          soma=soma+eigvc(j,k)*eigvc(j,k)
       END DO
       !* soma=length of eigenvector k
       soma=1.0/SQRT(soma)
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
       col(kkk)=0.0
       DO i=1,kmax
          vec(i,j)=eigvc(i,kkk)
       END DO
       kk(j)=kkk
    END DO
  END SUBROUTINE vereig







  SUBROUTINE w3fa03(press,temp)
    !
    REAL, INTENT(IN) :: press
    REAL, INTENT(OUT) ::  temp
    !
    !    this subroutine (w3fa03) computes the standard 
    !    temperature (deg k) given the
    !    pressure in mb.
    !     u. s. standard atmosphere, 1962
    !    icao std atm to 20km
    !    proposed extension to 32km
    !    not valid for  pressure.lt.8.68mb
    !
    REAL grav,piso,ziso,salp,pzero,t0,alp, &
         ptrop,tstr,htrop
    REAL rovg,fkt,ar,pp0,height
    !
    DATA grav /9.80665e0/, &
         piso /54.7487e0/, ziso /20000.0e0/, salp /-0.0010e0/, &
         pzero /1013.25e0/, t0 /288.15e0/, alp /0.0065e0/, &
         ptrop/226.321e0/, tstr/216.65e0/, htrop /11000.0e0/
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
          !     compute lapse rate=-.0010 cases
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
END MODULE NonLinearNMI
