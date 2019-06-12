!
!  $Author: pkubota $
!  $Date: 2010/06/07 21:10:46 $
!  $Revision: 1.14 $
!
MODULE Convection

  !   InitConvection
  !
  !   cumulus_driver|--qnegat
  !                 |--Cu_Ara
  !                 |--Cu_Kuolcl
  !                 |--Cu_Grellens
  !                 |--Shall_Tied
  !                 |--Shall_Souza
  !                 |--lrgscl
  !                 


  USE Constants, ONLY :  &
       delq             ,&
       r8,i8,qmin
       

  USE Diagnostics, ONLY:   &
       dodia             , &
       updia             , &
       StartStorDiag     , &
       nDiag_toprec      , & ! total precipiation
       nDiag_cvprec      , & ! convective precipitation
       nDiag_lsprec      , & ! large scale precipitation
       nDiag_snowfl      , & ! snowfall
       nDiag_clheat      , & ! convective latent heating
       nDiag_cmchan      , & ! convective moisture change
       nDiag_lslhea      , & ! large scale latent heating
       nDiag_lsmcha      , & ! large scale moisture change
       nDiag_sclhea      , & ! shallow convective latent heating
       nDiag_scmcha      , & ! shallow convective moisture change
       nDiag_nshcrm          ! negative specific humidity correction moisture source


  USE GridHistory, ONLY:   &
       IsGridHistoryOn   , &
       StoreGridHistory  , &
       dogrh             , &
       nGHis_cvprec     , &
       nGHis_clheat     , &
       nGHis_cvmosr     , &
       nGHis_sclhea     , &
       nGHis_shcvmo     , &
       nGHis_toprec     , &
       nGHis_snowfl     , &
       nGHis_sslaht     , &
       nGHis_spstms

  USE Options, ONLY :       &
       rccmbl            , &
       mlrg              , &
       iccon             , &
       ilcon             , &
       iscon             , &
       doprec            , &
       cflric            , &
       dt                , &
       kt                , & 
       ktp               , &
       ktm               , & 
       jdt 

  USE Init, ONLY :       &
       nls

  USE Cu_Kuolcl, ONLY :       &
      InitCu_Kuolcl         , &
      kuolcl

  USE Shall_Tied, ONLY:       &
      InitShall_Tied        , &
      shalv2
       
  USE Cu_Grellens, ONLY: grellens,InitGrellens
  USE Shall_Souza, ONLY: shallsouza
  USE ModConRas, ONLY: arprep,shllcl,InitArakawa
  USE LrgSclPrec, ONLY: InitLrgScl,lrgscl
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: InitConvection
  PUBLIC :: cumulus_driver

CONTAINS

  SUBROUTINE InitConvection(si, del, sl, cl, kMax)
    INTEGER, INTENT(IN) :: kMax
    REAL(KIND=r8),    INTENT(IN) :: si (kMax+1)
    REAL(KIND=r8),    INTENT(IN) :: del(kMax  )
    REAL(KIND=r8),    INTENT(IN) :: sl (kMax  )
    REAL(KIND=r8),    INTENT(IN) :: cl (kMax  )     

    IF(TRIM(iccon).EQ.'ARA')  CALL InitArakawa   ()
    IF(TRIM(iccon).EQ.'KUO')  CALL InitCu_Kuolcl () 
    IF(TRIM(iccon).EQ.'GRE')  CALL InitGrellens()
    IF(TRIM(ISCON).EQ.'TIED') CALL InitShall_Tied(si, del, sl, cl, kMax)
    IF(TRIM(ILCON).EQ.'YES') CALL InitLrgScl()
  END SUBROUTINE InitConvection
  SUBROUTINE cumulus_driver (&
            iMax     ,kMax     ,ta       ,tb       ,tc       ,qa       , &
            qb       ,qc       ,ub       ,vb       ,omgb     ,psb      , &
            psb2     ,del      ,sl       ,si       ,zs       ,sens     , &
            evap     ,mask     ,latco    ,fac2x    ,convc    ,convt    , &
            convb    ,prcp1    ,prcp2    ,prcp3    ,prcpt    ,toplv    , &
            botlv    ,convts   ,convcs   ,convbs   ,fac2     ,fac      , &
            geshem   ,ppli     ,ppci     ,prct     ,prcc     ,snowfl     )

    !************************************************************************
    !   The cumulus_driver subroutine calls deep and shallow cumulus
    !   parameterization schemes.
    !   more information nilo@cptec.inpe.br
    !   NOTE: This version is not official. You can use only for test.
    !************************************************************************
    !
    !  Definition/
    !---------------
    !             I N P U T  O U T P U T  F O R   G C M
    !             -------------------------------------
    ! INPUT
    !
    !** integer
    !    iMax                   ! end index for longitude domain
    !    kMax                   ! end index for u,v,t,p sigma levels
    !    jdt                   ! number of time step
    !    iccon                  ! cu schemes ex. KUO, ARA, GRE ..
    !   kuo                      ! convection yes(1) or not(0) for shallow convection
    !
    !** real
    !    dt                  ! time step (s)
    !    ta                     ! temperature (K) at time t-1
    !    tb                     ! temperature (K) at time t
    !    tc                     ! temperature (K) at time t+1
    !    qa                     ! water vapor mixing ratio (kg/kg) at time t-1
    !    qb                     ! water vapor mixing ratio (kg/kg) at time t
    !    qc                     ! water vapor mixing ratio (kg/kg) at time t+1
    !    psb                    ! surface pressure (cb)     at time t
    !    ub                     ! u-velocity (m/s) at time t
    !    vb                     ! v-velocity (m/s) at time t
    !    omgb                   ! vertical omega velocity (Pa/s) at time t
    !                           ! it is in half levels along with U,V,T,Q
    !    sl                     ! half sigma layers
    !    si                     ! full sigma layers
    !    del                    ! difference between full sigma layers
    !    xland                  ! land-sea mask (1 for land; 0 for water)
    !    zs                     ! topography (m)
    !    DX                     ! horizontal space interval (m)
    !    qrem,cldm              ! local variables for  RAS-Scheme
    !
    !    hrem,qrem              ! these arrays are needed for the heating 
    !                           ! and mostening from ras  scheme
    !
    !
    !    ktops, kbots           ! these arrays are needed for the new 
    !                           ! shallow convection scheme
    !    cldm                   ! needed for cloud fraction based on mass 
    !                           ! flux
    !    noshal1, kctop1, kcbot1! needed for cloud fraction based on mass 
    !                           ! flux new arrays needed for shallow 
    !                           ! convective clouds
    !     
    !
    !
    !   OUTPUT
    !**  integer
    !    kuo                    ! indix for shalow convection KUO,RAS,KUOG, GRELL
    !    ktop                   ! level of convective cloud top
    !    kbot                   ! level of convective cloud base
    !    plcl                   ! pressure at convection levl for shallow convection
    !                           ! in Kuo 
    !
    !** real
    !   RAINCV                  ! cumulus scheme precipitation (mm)
    !   tc                      ! new temperature (K) at time t+1  after CU precip
    !   qc                      ! new  water vapor mixing ratio (kg/kg) at time t+1.
    !
    !
    !*********************************************************************************
    IMPLICIT NONE
    !              I N P U T     O U T P U T    V A R I A B L E S
    !              ----------------------------------------------
    !              Xa at t-1   Xb at t    Xc at t+1

    ! Dimensions
    INTEGER, INTENT(IN) :: iMax
    INTEGER, INTENT(IN) :: kMax

    ! Sizes
    REAL(KIND=r8), INTENT(IN) :: sl   (kMax)
    REAL(KIND=r8), INTENT(IN) :: del  (kMax)
    REAL(KIND=r8), INTENT(IN) :: si   (kMax+1)

    ! Fixed fields: latitudes, mask and topography
    INTEGER, INTENT(IN) :: latco
    INTEGER(KIND=i8), INTENT(IN) :: mask (iMax) 
    REAL(KIND=r8),    INTENT(IN) :: zs   (iMax)

    ! Temperature (K) and specific humidity (kg/kg) 
    ! at times (a) = T-1, (b) = T and (c) = T+1
    REAL(KIND=r8), INTENT(INOUT) :: ta (iMax,kMax)
    REAL(KIND=r8), INTENT(INOUT) :: tb (iMax,kMax)
    REAL(KIND=r8), INTENT(INOUT) :: tc (iMax,kMax)
    REAL(KIND=r8), INTENT(INOUT) :: qa (iMax,kMax)
    REAL(KIND=r8), INTENT(INOUT) :: qb (iMax,kMax)
    REAL(KIND=r8), INTENT(INOUT) :: qc (iMax,kMax)

    ! Wind at time T
    ! in half levels along with U,V,T,Q
    REAL(KIND=r8), INTENT(IN) :: ub   (iMax,kMax) ! (m/s) 
    REAL(KIND=r8), INTENT(IN) :: vb   (iMax,kMax) ! (m/s)
    REAL(KIND=r8), INTENT(IN) :: omgb (iMax,kMax) ! (Pa/s)

    ! Surface pressure (cb) at time T
    REAL(KIND=r8), INTENT(IN) :: psb  (iMax)
    REAL(KIND=r8), INTENT(IN) :: psb2 (iMax)

    ! Heat/Water sfc fluxes
    REAL(KIND=r8), INTENT(IN) :: sens (iMax)
    REAL(KIND=r8), INTENT(IN) :: evap (iMax)

    ! UNCLASSIFIED VARIABLES
    REAL(KIND=r8), INTENT(IN)      :: fac2x  
    REAL(KIND=r8), INTENT(IN)      :: fac
    REAL(KIND=r8), INTENT(IN)      :: fac2

    REAL(KIND=r8), INTENT(INOUT) :: convc  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: convt  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: convb  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: prcp1  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: prcp2  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: prcp3  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: prcpt  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: toplv  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: botlv  (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: convts (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: convcs (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: convbs (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: geshem (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: ppli   (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: ppci   (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: prcc   (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: snowfl (iMax)
    REAL(KIND=r8), INTENT(INOUT) :: prct   (iMax)

    !
    !               L O C A L    V A R I A B L E S
    !              -------------------------------
    INTEGER       :: i
    INTEGER       :: k
    INTEGER       :: mask2   (iMax)
    REAL(KIND=r8) :: ps1     (iMax)
    REAL(KIND=r8) :: ps2     (iMax)
    REAL(KIND=r8) :: PS_work (iMax)
    REAL(KIND=r8) :: terr    (iMax)  

    ! (T,q) before any convection
    REAL(KIND=r8) :: tBegin (iMax,kMax)
    REAL(KIND=r8) :: qBegin (iMax,kMax)

    ! (T,q) after deep convection
    REAL(KIND=r8) :: qDeep(iMax,kMax)
    REAL(KIND=r8) :: tDeep(iMax,kMax)
    ! (T,q) after shallow convection
    REAL(KIND=r8) :: tShal(iMax,kMax)
    REAL(KIND=r8) :: qShal(iMax,kMax)
    ! (T,q) after large scale adjustment
    REAL(KIND=r8) :: tLrgs(iMax,kMax)
    REAL(KIND=r8) :: qLrgs(iMax,kMax)

    ! Shallow heat/moist
    REAL(KIND=r8) :: sclhea(iMax,kMax)
    REAL(KIND=r8) :: scmcha(iMax,kMax)
    ! Convective heat/moist
    REAL(KIND=r8) :: clheat(iMax,kMax)
    REAL(KIND=r8) :: cmchan(iMax,kMax)
    ! Large scale heat/moist
    REAL(KIND=r8) :: lslhea(iMax,kMax)
    REAL(KIND=r8) :: lsmcha(iMax,kMax)

    ! Working copies of input fields (T,q) at times (a,b,c)
    REAL(KIND=r8) :: q1(iMax,kMax)
    REAL(KIND=r8) :: q2(iMax,kMax)
    REAL(KIND=r8) :: q3(iMax,kMax)
    REAL(KIND=r8) :: t1(iMax,kMax)
    REAL(KIND=r8) :: t2(iMax,kMax)
    REAL(KIND=r8) :: t3(iMax,kMax)

    ! Wind components for grell ensemble
    REAL(KIND=r8) :: u2(iMax,kMax)
    REAL(KIND=r8) :: v2(iMax,kMax)
    REAL(KIND=r8) :: w2(iMax,kMax)

    !
    ! UNCLASSIFIED VARIABLES
    !
    REAL(KIND=r8) :: fdqn (iMax,kMax)
    REAL(KIND=r8) :: RAINCV     (iMax)
    REAL(KIND=r8) :: Total_Rain (iMax)

    !*******************************************
    !               Ktopos nao usado fora
    !            kctop1  usado para ARA fora
    !*******************************************
    REAL(KIND=r8)    :: hrem  (iMax,kMax)
    REAL(KIND=r8)    :: qrem  (iMax,kMax)
    REAL(KIND=r8)    :: cldm  (iMax)
    INTEGER          :: kctop1(iMax)
    INTEGER          :: kcbot1(iMax)
    INTEGER          :: noshal(iMax)
    !**********
    !others
    !*********
    INTEGER          :: ktop (iMax)
    INTEGER          :: kuo  (iMax)
    INTEGER          :: ktops(iMax)
    REAL(KIND=r8)    :: plcl (iMax)
    INTEGER          :: kbot (iMax)
    INTEGER          :: kbots(iMax) 
    REAL(KIND=r8)    :: dq   (iMax,kMax)
    REAL(KIND=r8)    :: rdt
    LOGICAL          :: newr
    LOGICAL          :: ghl_local
    REAL(KIND=r8)    :: snowflg(iMax)   

    !-----------------------------------------------------------------
    !-----------------------------------------------------------------

    rdt=1.0_r8/dt
    ghl_local = IsGridHistoryOn()

    ! Check for negative values of specific humidity
    ! Convert virtual temperature to thermodinamic temperature
    CALL qnegat (qa, fdqn, ta, (1.0_r8/dt), del, iMax, kMax)! time t-1
    CALL qnegat (qb, fdqn, tb, (1.0_r8/dt), del, iMax, kMax)! time t
    CALL qnegat (qc, fdqn, tc, (1.0_r8/dt), del, iMax, kMax)! time t+1

    ! Initialize cloud variables with unrealistic values
    DO i=1,iMax
       ktop (i) = -1000
       ktops(i) = -1000
       kuo  (i) = -1000
       plcl (i) = -1.0e3_r8
    END DO   

    ! Initialize surface variables
    DO i=1,iMax
       !surface pressure
       !!T+1                ps2(i)=psb(i)
       !!T                  ps2(i)=psb2(i)
       ps1(i)    =psb2(i)!T+1 
       ps2(i)    =psb(i) !T   
       PS_work(i)=psb(i)

       terr(i)   =MAX(zs(i),0.0_r8)

       RAINCV(i)     = 0.0_r8
       Total_Rain(i) = 0.0_r8
    END DO   

    ! Copy (T,q) at t-1, t and t+1 to work arrays
    DO i=1,iMax
       DO k=1,kMax
          T1(i,k)=ta(i,k)
          T2(i,k)=tb(i,k)
          T3(i,k)=tc(i,k)

          q1(i,k)=qa(i,k)
          q2(i,k)=qb(i,k)
          q3(i,k)=qc(i,k)
       END DO
    END DO

    DO i=1,iMax
       DO k=1,kMax
          tBegin(i,k)= tc(i,k)! time t+1
          qBegin(i,k)= qc(i,k)! time t+1
       END DO
    END DO

    !-----------------------------------------------------------------
    ! Deep Convection
    !-----------------------------------------------------------------
    IF(TRIM(iccon).EQ.'ARA') THEN
       CALL arprep( &
            tc    ,qc    ,sl    ,si   ,ps2  ,ktop  ,kbot  ,RAINCV, &
            hrem  ,qrem  ,dt    ,T3   ,q3   ,del   ,kuo   ,cldm  , &
            cflric,kMax+1,kMax-1,iMax ,kMax ,nls)

       CALL shllcl(dt    ,ps2   ,sl   ,si   ,q3    ,T3    ,kuo   , &
            plcl  ,ktops ,kbots ,iMax ,kMax  )
    END IF
    
    IF(TRIM(iccon).EQ.'KUO')THEN
       CALL  kuolcl(dt, ps2, del, sl, si, q1, q3, &
            T3, dq, RAINCV, kuo, plcl, ktop, &
            kbot, iMax, kMax)
    END IF

    IF(TRIM(iccon).EQ.'GRE')THEN

       ! grell mask
       DO i=1,iMax      
          IF(mask(i).GT.0_i8)THEN
             mask2(i)=0 ! land
          ELSE
             mask2(i)=1 ! water/ocean
          END IF
       END DO

       ! grell wind
       DO i=1,iMax
          DO k=1,kMax
             u2(i,k)=ub(i,k)
             v2(i,k)=vb(i,k)
             w2(i,k)=omgb(i,k)*1000.0_r8  ! (Pa/s)
          END DO
       END DO

       CALL grellens(ps2, sl,u2,v2,w2,T2,T3, q2,q3,         &
            terr,mask2, dt, RAINCV,kuo,ktop,kbot,plcl,      &
            iMax,kMax)
    END IF

    ! Save humd/temp after deep convection
    DO k=1,kMax
       DO i=1,iMax
          qDeep(i,k)=q3(i,k)
          tDeep(i,k)=t3(i,k)
       END DO
    END DO

    ! Copy deep convection rain to total rain
    Total_Rain=RAINCV
    
    !-----------------------------------------------------------------
    ! Shallow Convection
    !-----------------------------------------------------------------
    IF(TRIM(ISCON).EQ.'TIED')THEN
       IF(TRIM(iccon).EQ.'KUO'.OR.TRIM(iccon).EQ.'GRE')THEN
          newr=.FALSE.
          CALL shalv2(si, sl, t3, q3, PS_work, dt, &
               ktop, plcl, kuo, kMax+1, kctop1, kcbot1, noshal, &
               newr, iMax, kMax)
       END IF

       IF(TRIM(iccon).EQ.'ARA') THEN
          newr=.TRUE.
          CALL shalv2(si, sl, t3, q3, PS_work, dt, &
               ktops, plcl, kuo, kMax+1, kctop1, kcbot1, noshal, &
               newr, iMax, kMax)
       END IF
    END IF
    
    IF(TRIM(ISCON).EQ.'SOUZ')THEN
       WRITE(*,*)"it is not available yet. Problem in the water balance'"
       STOP "ERROR"
       !CALL Shallsouza(t3,q3,PS_work,sl,sens,evap,dt,iMax,kMax,kuo, &
       !     noshal, kctop1, kcbot1, 560.0_r8, 1.6_r8)
    END IF

    ! Save humd/temp after shallow convection
    DO k=1,kMax
      DO i=1,iMax
        tShal(i,k)=t3(i,k)
        qShal(i,k)=q3(i,k)
      END DO
    END DO

    !-----------------------------------------------------------------
    ! Large Scale Precipitation
    !-----------------------------------------------------------------
    
    IF(TRIM(ILCON).EQ.'YES') THEN
      CALL lrgscl(Total_Rain, t3, dq, q3, ps2, del, sl, dt, &
                mlrg, latco, iMax, kMax)
    END IF

    ! Save humd/temp after large scale convection
    DO k=1,kMax
      DO i=1,iMax
        tLrgs(i,k)=t3(i,k)
        qLrgs(i,k)=q3(i,k)
      END DO
    END DO

    !-----------------------------------------------------------------
    ! Convective Cloud Cover
    !-----------------------------------------------------------------
    
    IF(TRIM(iccon).EQ.'ARA'.OR.TRIM(iccon).EQ.'KUO'.OR.TRIM(iccon).EQ.'GRE')THEN
      CALL CLOUD_COVER( &
             kt     ,ktp    ,iMax   ,kbot   ,ktop   ,noshal ,kctop1 , &
             kcbot1 ,RAINCV ,fac2x  ,rccmbl ,iccon  ,convc  ,convt  , &
             convb  ,prcp1  ,prcp2  ,prcp3  ,prcpt  ,toplv  ,botlv  , &
             convts ,convcs ,convbs )
    END IF

    !-----------------------------------------------------------------
    ! DIAGNOSTICS AND MODEL OUTPUT
    !-----------------------------------------------------------------

    !---------------------
    ! Calculate deep convection moistening and heating profiles
    DO k=1,kMax
       DO i=1,iMax
          clheat(i,k)=fac*rdt*(tDeep(i,k)-tBegin(i,k))
          cmchan(i,k)=fac*rdt*(qDeep(i,k)-qBegin(i,k))
       END DO
    END DO

    !---------------------
    ! Calculate shallow convection moistening and heating profiles
    DO k=1,kMax
       DO i=1,iMax
          sclhea(i,k)=fac*rdt*(tShal(i,k)-tDeep(i,k))
          scmcha(i,k)=fac*rdt*(qShal(i,k)-qDeep(i,k))
       END DO
    END DO

    !---------------------
    ! Calculate large scale convection moistening and heating profiles
    DO k=1,kMax
       DO i=1,iMax
          lslhea(i,k)=fac*rdt*(tLrgs(i,k)-tShal(i,k))
          lsmcha(i,k)=fac*rdt*(qLrgs(i,k)-qShal(i,k))
       END DO
    END DO

    !***********************************
    ! move qDeep to qb and tDeep to tb
    ! move T3 to Tc and q3 to qc
    !***********************************
    DO i=1,iMax
       DO k=1,kMax
          ! Update T
          qb(i,k)=qDeep(i,k)!q  after deep convection
          tb(i,k)=tDeep(i,k)*(1.0_r8+delq*qDeep(i,k))!t  after deep convection
          ! Update T+1
          qc(i,k) = qLrgs(i,k)
          tc(i,k) = tLrgs(i,k)*(1.0_r8+delq*qLrgs(i,k))
       END DO
    END DO

    ! Calculate precipiation in mm/s
    DO i=1,iMax
       ppci(i)=2.0e0_r8*1.0e3_r8*RAINCV(i) ! deep
       ppli(i)=2.0e0_r8*1.0e3_r8*Total_Rain(i)-ppci(i) ! large
    END DO

    ! Calculate precipiation in mm
    DO i=1,iMax
       geshem(i)=geshem(i)+fac2x*Total_Rain(i)
    END DO

    ! Time-step output of precipitation
    IF (doprec) THEN
       DO i=1,iMax
          prcc(i)=fac2*rdt*1.0e3_r8*RAINCV(i)
          prct(i)=fac2*rdt*1.0e3_r8*Total_Rain(i)
       END DO
    END IF
    
    ! Diagnose snow field
    DO i=1,iMax          
       IF(0.35_r8*tLrgs(i,1)+0.65_r8*tLrgs(i,2).LE.273.2_r8)THEN
          snowflg(i) = Total_Rain(i)
          snowfl (i) = fac2*rdt*1.0e3_r8*Total_Rain(i)
       ELSE
          snowflg(i) = 0.0_r8
          snowfl (i) = 0.0_r8
       END IF
    END DO

    !-----------------
    ! Storage Diagnostic Fields
    !------------------
    IF (StartStorDiag)THEN
       CALL ConvecDiagnStorage (&
            iMax     ,kMax     ,latco     ,rdt      ,fac2     , &
            fdqn     ,RAINCV   ,Total_Rain,snowfl   ,sclhea   , &
            scmcha   ,clheat   ,cmchan    ,lslhea   ,lsmcha)
    END IF
    !-----------------
    ! Storage GridHistory Fields
    !------------------
    IF(ghl_local)THEN 
       CALL ConvecGridHistStorage(&
            iMax     ,kMax      ,latco    ,rdt      ,fac2     , &
            RAINCV   ,Total_Rain,snowflg  ,sclhea   ,scmcha   , &
            clheat   ,cmchan    ,lslhea   ,lsmcha)
    END IF

  END SUBROUTINE cumulus_driver
  


  SUBROUTINE CLOUD_COVER( &
      kt     ,ktp    ,ncols  ,kbot   ,ktop   ,noshal1,kctop1 , &
      kcbot1 ,rrr    ,fac2x  ,rccmbl ,iccon  ,convc  ,convt  , &
      convb  ,prcp1  ,prcp2  ,prcp3  ,prcpt  ,toplv  ,botlv  , &
      convts ,convcs ,convbs )
   IMPLICIT NONE 
    INTEGER, INTENT(in   ) :: kt     
    INTEGER, INTENT(in   ) :: ktp    
    INTEGER, INTENT(in   ) :: ncols  
    INTEGER, INTENT(in   ) :: kbot   (ncols)
    INTEGER, INTENT(in   ) :: ktop   (ncols)
    INTEGER, INTENT(in   ) :: noshal1(ncols)
    INTEGER, INTENT(in   ) :: kctop1 (ncols)
    INTEGER, INTENT(in   ) :: kcbot1 (ncols)
    REAL(KIND=r8),    INTENT(in   ) :: rrr    (ncols)
    REAL(KIND=r8),    INTENT(in   ) :: fac2x  
    REAL(KIND=r8),    INTENT(in   ) :: rccmbl ! radiative convective cloud minimum base layer index
    CHARACTER(LEN=*),INTENT(in) :: iccon

    REAL(KIND=r8),    INTENT(inout) :: convc  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: convt  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: convb  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: prcp1  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: prcp2  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: prcp3  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: prcpt  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: toplv  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: botlv  (ncols)
    REAL(KIND=r8),    INTENT(inout) :: convts (ncols)
    REAL(KIND=r8),    INTENT(inout) :: convcs (ncols)
    REAL(KIND=r8),    INTENT(inout) :: convbs (ncols)
     
    REAL(KIND=r8)   , PARAMETER :: fp2457 = 0.2457_r8
    REAL(KIND=r8)   , PARAMETER :: fp1253 = 0.1253_r8
    REAL(KIND=r8)   , PARAMETER :: f0p8   = 0.8_r8
    REAL(KIND=r8)   , PARAMETER :: f8p0e3 = 8.0e3_r8
    INTEGER :: i      
    INTEGER :: is     
    INTEGER :: ijk    

    !--------------------------------------------------------
    !   CLOUD COVER, Cloud TOP-BOT FOR RADIATION (sub cldgen)
    !             DUE CONV PRECIPITATION
    !--------------------------------------------------------
    !   TO cldgen are necessary:
    !   a) cloud top and base   (convt, convb in cldgen)
    !   b) cloud amount is calculated convc (only rrr>0). It is calculate below.
    !     a+b used to defined high clouds due to strong convection
    !   prcpt=precipitation at each time step.(rrr)
    !   convt=ktop
    !   conbt=kbot (>=2) for radiation
    !*****************************************************************

    IF (kt .NE. ktp) THEN
       DO IJK=1,ncols
          convc(IJK) = 0.0_r8   ! call reset(convc(1),ncols)
          convt(IJK) = 0.0_r8   ! call reset(convt(1),ncols)
          convb(IJK) = 0.0_r8   ! call reset(convb(1),ncols)
       ENDDO

       DO i = 1, ncols
          prcpt(i) = prcpt(i) - prcp1(i) &
               + prcp3(i)
       END DO
       IF(TRIM(iccon).EQ.'ARA')THEN
         DO i = 1, ncols 
           IF (prcpt(i) .GT. 0.0e0_r8) THEN
             convc(i) = 0.2_r8+0.038_r8*prcpt(i)*23000.0_r8
             convc(i) = MAX(convc(i), 0.0e0_r8)
             convc(i) = MIN(convc(i), f0p8)
           END IF
         END DO
       ELSE
         DO i = 1, ncols 
           IF (prcpt(i) .GT. 0.0e0_r8) THEN
             convc(i) = fp2457 + fp1253 * LOG(prcpt(i) * f8p0e3)
             convc(i) = MAX(convc(i), 0.0e0_r8)
             convc(i) = MIN(convc(i), f0p8)
           END IF
         END DO        
       END IF  
       !--faltou   
       DO i = 1, ncols
          IF (prcp3(i) .GT. 0.0e0_r8) THEN
             convt(i)=toplv(i) / prcp3(i)
             convb(i)=botlv(i) / prcp3(i)
          END IF
       END DO
       DO i = 1, ncols
          convb(i) = MAX(convb(i),rccmbl)
          IF (convb(i) .GT. convt(i)) &
               convb(i) = convt(i)
       END DO
       !-----
       DO i = 1, ncols
          prcp1(i) = prcp2(i)
          prcp2(i) = prcp3(i)
       END DO
       DO IJK=1,ncols
          prcp3(IJK) = 0.0_r8   !call reset(prcp3(1),ncols)
          toplv(IJK) = 0.0_r8   !call reset(toplv(1),ncols)
          botlv(IJK) = 0.0_r8   !call reset(botlv(1),ncols)
       ENDDO
    END IF
    !*****************************************************
    IF(TRIM(iccon).EQ.'ARA') THEN
      DO i = 1, ncols
        IF (rrr(i) .GT. 0.0_r8) THEN
          prcp3(i) = prcp3(i) + fac2x * rrr(i)          
          toplv(i) = toplv(i) + fac2x * rrr(i)  &
                   * ktop(i)
          botlv(i) = botlv(i) + fac2x * rrr(i)  &
                   * kbot(i)        
        END IF
      END DO
    ELSE
      DO i = 1, ncols
        IF (rrr(i) .GT. 0.0_r8) THEN
          prcp3(i) = prcp3(i) + fac2x * rrr(i)          
          toplv(i) = toplv(i) +  &
                     fac2x * rrr(i) * ktop(i)
          botlv(i) = botlv(i) +  &
                     fac2x * rrr(i) * kbot(i)
        END IF
      END DO
    END IF
    !
    IF(TRIM(iccon).EQ.'ARA') THEN
       DO IJK=1,ncols
          convts(IJK) = 0.0_r8 
          convbs(IJK) = 0.0_r8 
          convcs(IJK) = 0.0_r8 
       ENDDO

       DO is=1,ncols
          IF(noshal1(is).EQ.0) THEN
             convts(is)=kctop1(is)
             convbs(is)=kcbot1(is)
             !
             !     for mass flux   convcs(is)=0.5
             !
             convcs(is)= 0.3_r8
          ENDIF
       END DO
    ENDIF
  END SUBROUTINE CLOUD_COVER

  SUBROUTINE ConvecGridHistStorage(&
                   ncols    ,kMax     ,latco    ,rdt      ,fac2     , &
                   rrr      ,Total_Rain,snowflg  ,sclhea   ,scmcha   , &
                   clheat   ,cmchan   ,lslhea   ,lsmcha) 
                   
    IMPLICIT NONE
    INTEGER, INTENT(IN   )    :: ncols
    INTEGER, INTENT(IN   )    :: kMax
    INTEGER, INTENT(IN   )    :: latco    
    REAL(KIND=r8), INTENT(IN   ) :: rdt
    REAL(KIND=r8), INTENT(IN   ) :: fac2
    REAL(KIND=r8), INTENT(IN   ) :: rrr       (nCols)
    REAL(KIND=r8), INTENT(in   ) :: Total_Rain (nCols)
    REAL(KIND=r8), INTENT(in   ) :: snowflg   (nCols)
    REAL(KIND=r8), INTENT(in   ) :: sclhea    (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: scmcha    (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: clheat    (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: cmchan    (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: lslhea    (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: lsmcha    (nCols,kMax)

    IF(TRIM(iccon).EQ.'ARA'.OR.TRIM(iccon).EQ.'KUO'.OR.TRIM(iccon).EQ.'GRE')THEN
       IF(dogrh(nGHis_cvprec,latco)) &
            CALL StoreGridHistory(rrr(1:ncols),nGHis_cvprec,latco,fac2*rdt*1.0e3_r8)
       IF(dogrh(nGHis_clheat,latco)) &
            CALL StoreGridHistory(clheat(1:nCols,:),nGHis_clheat,latco)
       IF(dogrh(nGHis_cvmosr,latco)) &
            CALL StoreGridHistory(cmchan(1:nCols,:),nGHis_cvmosr,latco)
    END IF

    IF(TRIM(ISCON).EQ.'TIED' .or. TRIM(ISCON).EQ.'SOUZ')THEN
       IF(dogrh(nGHis_sclhea,latco)) &
            CALL StoreGridHistory(sclhea,nGHis_sclhea,latco)
       IF(dogrh(nGHis_shcvmo,latco)) &
            CALL StoreGridHistory(scmcha,nGHis_shcvmo,latco)
    END IF
    !---------------
    !     gdivn,gtmpn,grotn,gun,gvn are temporary working space
    !     
    IF(TRIM(ILCON).EQ.'YES') THEN
       IF(dogrh(nGHis_toprec,latco)) &
            CALL StoreGridHistory(Total_Rain,nGHis_toprec,latco,fac2*rdt*1.0e3_r8)    
       IF(dogrh(nGHis_snowfl,latco)) &
            CALL StoreGridHistory(snowflg  ,nGHis_snowfl,latco,fac2*rdt*1.0e3_r8)
       IF(dogrh(nGHis_sslaht,latco)) &
            CALL StoreGridHistory(lslhea   ,nGHis_sslaht,latco)
       IF(dogrh(nGHis_spstms,latco)) &
            CALL StoreGridHistory(lsmcha   ,nGHis_spstms,latco)
    END IF
  END SUBROUTINE ConvecGridHistStorage

  SUBROUTINE ConvecDiagnStorage(&
                   ncols    ,kMax     ,latco    ,rdt      ,fac2     , &
                   fdqn     ,rrr      ,Total_Rain,snowfl   ,sclhea   , &
                   scmcha   ,clheat   ,cmchan   ,lslhea   ,lsmcha     )    
                                      
    IMPLICIT NONE
    INTEGER, INTENT(IN   )    :: ncols
    INTEGER, INTENT(IN   )    :: kMax
    INTEGER, INTENT(IN   )    :: latco    
    REAL(KIND=r8), INTENT(IN   ) :: rdt
    REAL(KIND=r8), INTENT(IN   ) :: fac2
    REAL(KIND=r8), INTENT(IN   ) :: fdqn      (nCols,kMax)
    REAL(KIND=r8), INTENT(IN   ) :: rrr       (nCols)
    REAL(KIND=r8), INTENT(in   ) :: Total_Rain (nCols)
    REAL(KIND=r8), INTENT(in   ) :: snowfl    (nCols)
    REAL(KIND=r8), INTENT(in   ) :: sclhea    (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: scmcha    (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: clheat (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: cmchan (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: lslhea (nCols,kMax)
    REAL(KIND=r8), INTENT(in   ) :: lsmcha (nCols,kMax)

    REAL(KIND=r8)    :: bfr1   (nCols)
    REAL(KIND=r8)    :: bfr3   (nCols)
    INTEGER :: i
    
    ! "negative specific humidity" correction
    IF(dodia(nDiag_nshcrm))CALL updia(fdqn,nDiag_nshcrm,latco)

    IF(TRIM(iccon).EQ.'ARA'.OR.TRIM(iccon).EQ.'KUO'.OR.TRIM(iccon).EQ.'GRE')THEN
       IF(dodia(nDiag_cvprec)) THEN
          DO i=1,ncols
             bfr1(i)=fac2*rdt*1.0e3_r8*rrr(i)
          END DO
          CALL updia(bfr1,nDiag_cvprec,latco)
       END IF
       IF(dodia(nDiag_clheat))CALL updia(clheat,nDiag_clheat,latco)
       IF(dodia(nDiag_cmchan))CALL updia(cmchan,nDiag_cmchan,latco)
    END IF

    IF(TRIM(ISCON).EQ.'TIED' .or. TRIM(ISCON).EQ.'SOUZ')THEN
       IF(dodia(nDiag_sclhea))CALL updia(sclhea,nDiag_sclhea,latco)
       IF(dodia(nDiag_scmcha))CALL updia(scmcha,nDiag_scmcha,latco)
    END IF
    !---------------
    !     gdivn,gtmpn,grotn,gun,gvn are temporary working space
    !     
    IF(TRIM(ILCON).EQ.'YES') THEN
       DO i=1,ncols
          IF(dodia(nDiag_toprec))bfr1(i)=fac2*rdt*1.0e3_r8*Total_Rain(i)

          IF(dodia(nDiag_lsprec))bfr3(i)=fac2*rdt*1.0e3_r8*(Total_Rain(i)-rrr(i))
       END DO

       IF(dodia(nDiag_toprec))CALL updia(bfr1,nDiag_toprec,latco)

       IF(dodia(nDiag_snowfl))CALL updia(snowfl,nDiag_snowfl,latco)

       IF(dodia(nDiag_lsprec))CALL updia(bfr3,nDiag_lsprec,latco)

       IF(dodia(nDiag_lslhea))CALL updia(lslhea,nDiag_lslhea,latco)

       IF(dodia(nDiag_lsmcha))CALL updia(lsmcha,nDiag_lsmcha,latco)
    END IF
  END SUBROUTINE ConvecDiagnStorage


  ! qnegat : routine for dealing with negative values of specific humidity
  !          for data on latitude circle.



  SUBROUTINE  qnegat (fq, fdq, fft, rdt, del, iMax, kMax)
    !
    ! input: fq  specific humidity (dimensionless mixing ratio)
    !        fp  surface pressure (cb)
    ! ouput: fq  adjusted specific humidity
    !        fp  unchanged
    !        fdq distribution of moisture modification
    !
    ! iMax......Number of grid points on a gaussian latitude circle   
    ! kMax......Number of sigma levels  
    ! imx.......=iMax+1 or iMax+2   :this dimension instead of iMax
    !              is used in order to avoid bank conflict of memory
    !              access in fft computation and make it efficient. the
    !              choice of 1 or 2 depends on the number of banks and
    !              the declared type of grid variable (real*4,real*8)
    !              to be fourier transformed.
    !              cyber machine has the symptom.
    !              cray machine has no bank conflict, but the argument
    !              'imx' in subr. fft991 cannot be replaced by iMax    
    ! del.......sigma spacing for each layer computed in routine "setsig".  
    ! dfact.....del(k+1)/del(k)
    !
    INTEGER, INTENT(in   ) :: iMax  
    INTEGER, INTENT(in   ) :: kMax
    REAL(KIND=r8)   , INTENT(in   ) :: rdt

    REAL(KIND=r8),    INTENT(inout) :: fq   (iMax,kMax)
    REAL(KIND=r8),    INTENT(inout) :: fdq  (iMax,kMax)  
    REAL(KIND=r8),    INTENT(inout) :: fft  (iMax,kMax)   
    REAL(KIND=r8),    INTENT(in   ) :: del  (kMax)

    REAL(KIND=r8)   :: dfact(kMax)

    INTEGER :: klev
    INTEGER :: kblw
    INTEGER :: i
    INTEGER :: k  

    DO k=1,kMax-1
       dfact(k+1) = del(k+1)/del(k)
    END DO
    !     
    !     ecmwf vertical borrowing scheme
    !     fdq contains compensated borrowing above first level, uncompensated
    !     borrowing in first level
    !     
    DO k=1,kMax-1
       klev = kMax-k+1
       kblw = klev - 1
       DO i=1,iMax
          fdq(i,klev) = fq(i,klev)
          IF(fq(i,klev).LT.0.0e0_r8) fq(i,klev) = 1.0e-12_r8
          fdq(i,klev) = fq(i,klev) - fdq(i,klev)
          fq(i,kblw) = fq(i,kblw) - fdq(i,klev)*dfact(klev)
       END DO
    END DO

    DO i=1,iMax
       fdq(i,1) = fq(i,1)
       IF(fq(i,1).LT.0.0e0_r8) fq(i,1) = 1.0e-12_r8
       fdq(i,1) = fq(i,1) - fdq(i,1)
    END DO

    DO k=1,kMax
       DO i=1,iMax
          fft(i,k)=fft(i,k)/(1.0_r8+delq*fq(i,k))
       END DO
    END DO

    IF(dodia(nDiag_nshcrm))THEN
       DO k=1,kMax
          DO i=1,iMax
             fdq(i,k)=fdq(i,k)*rdt
          END DO
       END DO
    END IF

  END SUBROUTINE qnegat
  
END MODULE Convection
