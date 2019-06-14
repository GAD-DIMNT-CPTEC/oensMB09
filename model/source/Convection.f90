!
!  $Author: pkubota $
!  $Date: 2007/07/13 18:47:54 $
!  $Revision: 1.9 $
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
            geshem   ,ppli     ,ppci     ,prct     ,prcc     ,snow       )

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

    INTEGER, INTENT(IN   )      :: iMax
    INTEGER, INTENT(IN   )      :: kMax
    REAL(KIND=r8),    INTENT(IN   )      :: ub   (1:iMax,1:kMax)
    REAL(KIND=r8),    INTENT(IN   )      :: vb   (1:iMax,1:kMax)
    REAL(KIND=r8),    INTENT(IN   )      :: omgb (1:iMax,1:kMax)
    INTEGER(KIND=i8), INTENT(IN   )      :: mask (1:iMax) 
    REAL(KIND=r8),    INTENT(IN   )      :: psb  (1:iMax)
    REAL(KIND=r8),    INTENT(IN   )      :: psb2 (1:iMax)
    REAL(KIND=r8),    INTENT(IN   )      :: zs   (1:iMax)
    REAL(KIND=r8),    INTENT(IN   )      :: sens (1:iMax)
    REAL(KIND=r8),    INTENT(IN   )      :: evap (1:iMax)
    REAL(KIND=r8),    INTENT(IN   )      :: sl   (1:kMax)
    REAL(KIND=r8),    INTENT(IN   )      :: del  (1:kMax)
    REAL(KIND=r8),    INTENT(IN   )      :: si   (1:kMax+1)
    INTEGER, INTENT(IN   )      :: latco
    REAL(KIND=r8),    INTENT(in   )      :: fac2x  
    REAL(KIND=r8), INTENT(IN   )      :: fac
    REAL(KIND=r8), INTENT(IN   )      :: fac2

    REAL(KIND=r8),    INTENT(inout)      :: convc  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: convt  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: convb  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: prcp1  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: prcp2  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: prcp3  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: prcpt  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: toplv  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: botlv  (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: convts (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: convcs (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: convbs (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: geshem (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: ppli   (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: ppci   (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: prcc   (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: snow   (iMax)
    REAL(KIND=r8),    INTENT(inout)      :: prct   (iMax)    
    REAL(KIND=r8),    INTENT(INOUT)      :: ta   (1:iMax,1:kMax)
    REAL(KIND=r8),    INTENT(INOUT)      :: tb   (1:iMax,1:kMax)
    REAL(KIND=r8),    INTENT(INOUT)      :: tc   (1:iMax,1:kMax)
    REAL(KIND=r8),    INTENT(INOUT)      :: qa   (1:iMax,1:kMax)
    REAL(KIND=r8),    INTENT(INOUT)      :: qb   (1:iMax,1:kMax)
    REAL(KIND=r8),    INTENT(INOUT)      :: qc   (1:iMax,1:kMax)
    !
    !               L O C A L    V A R I A B L E S
    !              -------------------------------
    INTEGER                     :: i
    INTEGER                     :: k
    INTEGER                     :: mask2   (iMax)
    REAL(KIND=r8)                        :: ps1     (iMax)
    REAL(KIND=r8)                        :: ps2     (iMax)
    REAL(KIND=r8)                        :: PS_work (iMax)
    REAL(KIND=r8)                        :: terr    (iMax)  
    REAL(KIND=r8)                        :: q33  (iMax,kMax)
    REAL(KIND=r8)                        :: t33  (iMax,kMax)
    REAL(KIND=r8)                        :: q1   (iMax,kMax)
    REAL(KIND=r8)                        :: q2   (iMax,kMax)
    REAL(KIND=r8)                        :: q3   (iMax,kMax)
    REAL(KIND=r8)                        :: t1   (iMax,kMax)
    REAL(KIND=r8)                        :: t2   (iMax,kMax)
    REAL(KIND=r8)                        :: t3   (iMax,kMax)
    REAL(KIND=r8)                        :: u2   (iMax,kMax)
    REAL(KIND=r8)                        :: v2   (iMax,kMax)
    REAL(KIND=r8)                        :: w2   (iMax,kMax)
    REAL(KIND=r8)                        :: q22d (iMax,kMax)
    REAL(KIND=r8)                        :: t22d (iMax,kMax)
    REAL(KIND=r8)                        :: q33d (iMax,kMax)
    REAL(KIND=r8)                        :: t33d (iMax,kMax)
    REAL(KIND=r8)                        :: cu_temp   (iMax,kMax)
    REAL(KIND=r8)                        :: cu_umes   (iMax,kMax)
    REAL(KIND=r8)                        :: Work_Temp (iMax,kMax)
    REAL(KIND=r8)                        :: Work_Umes (iMax,kMax)
    REAL(KIND=r8)                        :: fdqn      (iMax,kMax)
    !
    !   SHAL
    !
    REAL(KIND=r8)                        :: cl(kMax) 
    REAL(KIND=r8)                        :: RAINCV    (1:iMax)
    REAL(KIND=r8)                        :: Cuml_Rain (iMax)
    INTEGER                     :: nkuo
    INTEGER                     :: msta
    !*******************************************
    !               Ktopos nao usado fora
    !            kctop1  usado para ARA fora
    !*******************************************
    REAL(KIND=r8)                        :: hrem  (iMax,kMax)
    REAL(KIND=r8)                        :: qrem  (iMax,kMax)
    REAL(KIND=r8)                        :: cldm  (iMax)
    INTEGER                     :: kctop1(iMax)
    INTEGER                     :: kcbot1(iMax)
    INTEGER                     :: noshal(iMax)
    !***************
    !  SHALOW-BMJ
    !**************
    REAL(KIND=r8)                        :: q_work  (iMax,kMax)
    REAL(KIND=r8)                        :: t_work  (iMax,kMax)
    !**********
    !others
    !*********
    INTEGER                     :: ktop (iMax)
    INTEGER                     :: kuo  (iMax)
    INTEGER                     :: ktops(iMax)
    REAL(KIND=r8)                        :: plcl (iMax)
    INTEGER                     :: kbot (iMax)
    INTEGER                     :: kbots(iMax) 
    REAL(KIND=r8)                        :: dq   (iMax,kMax)
    REAL(KIND=r8)                        :: rdt
    LOGICAL                     :: newr

    rdt=1.0_r8/dt
    RAINCV=0.0_r8
    Cuml_Rain=0.0_r8

   !------------------------------------------------
    CALL qnegat (qa, fdqn, ta, (1.0_r8/dt), del, iMax, kMax)! time t-1
    CALL qnegat (qb, fdqn, tb, (1.0_r8/dt), del, iMax, kMax)! time t
    CALL qnegat (qc, fdqn, tc, (1.0_r8/dt), del, iMax, kMax)! time t+1
   !---------------------------------------------------------
    DO k=1,kMax
       DO i=1,iMax
          !ta(i,k)=ta(i,k)/(1.0_r8+delq*qa(i,k))
          !tb(i,k)=tb(i,k)/(1.0_r8+delq*qb(i,k))
          !tc(i,k)=tc(i,k)/(1.0_r8+delq*qc(i,k))
       END DO
    END DO

    DO i=1,iMax
       !surface pressure
       !!T+1                ps2(i)=psb(i)
       !!T                  ps2(i)=psb2(i)
       ps1(i)    =psb2(i)!T+1 
       ps2(i)    =psb(i) !T   
       PS_work(i)=psb(i)
       terr(i)   =MAX(zs(i),0.0_r8)
       ktop (i)  =-1000
       ktops(i)  =-1000
       kuo  (i)  =-1000
       plcl (i)  =-1.0e3_r8
    END DO   
    DO i=1,iMax      
       IF(mask(i).GT.0_i8)THEN
          mask2(i)=0          !land
       ELSE
          mask2(i)=1          !water/ocean
       END IF
    END DO    
    DO i=1,iMax
       DO k=1,kMax
          Work_Temp(i,k)= tc(i,k)! time t+1
          Work_Umes(i,k)= qc(i,k)! time t+1
          q1(i,k)=qa(i,k)
          q2(i,k)=qb(i,k)
          q3(i,k)=qc(i,k)

          u2(i,k)=ub(i,k)
          v2(i,k)=vb(i,k)
          w2(i,k)=omgb(i,k)*1000.0_r8  !!      (Pa/s)

          T1(i,k)=ta(i,k)
          T2(i,k)=tb(i,k)
          T3(i,k)=tc(i,k)

          !-----------------------------------------
          ! Temporary files
          !------------------------------------------
          !  for 1D model

          q22d(i,k)=q2(i,k)
          t22d(i,k)=t2(i,k)

          q33d(i,k)=q3(i,k)
          t33d(i,k)=t3(i,k)
          !----------------------------

          q_work(i,k)=q3(i,k)             !temporary file before all convection
          t_work(i,k)=t3(i,k)             !temporary file before all convection

       END DO
    END DO
    !--------------------------
    ! for shallow convection TIED
    !----------------------------
    DO k=1,kMax
       cl(k)=1.0_r8-sl(k)
    END DO
    msta=0
    nkuo=0
    !-----------------
    ! Deep Convection
    !------------------
    IF(TRIM(iccon).EQ.'ARA') THEN
       CALL arprep( &
            tc    ,qc    ,sl    ,si   ,ps2  ,ktop  ,kbot  ,RAINCV, &
            hrem  ,qrem  ,dt ,t3   ,q3   ,del   ,kuo   ,cldm  , &
            cflric,kMax+1,kMax-1,iMax ,kMax  ,nls)
       CALL shllcl(dt    ,ps2   ,sl    ,si    ,q3    ,t3   ,msta  ,kuo   , &
            plcl  ,ktops ,kbots ,iMax ,kMax  )
    END IF
    !***********************************************
    
    IF(TRIM(iccon).EQ.'KUO')THEN
       CALL  kuolcl(dt, nkuo, ps2, del, sl, si, q1, q3, &
            t3, dq, RAINCV, msta, kuo, plcl, ktop, &
            kbot, iMax, kMax)
    END IF
    !***********************************************


    IF(TRIM(iccon).EQ.'GRE')THEN
       CALL grellens(ps2, sl,u2,v2,w2,t2,t3, q2,q3,                   &
            terr,mask2, dt, RAINCV,kuo,ktop,kbot,plcl,      &
            iMax,kMax)
    END IF
    !-----------------
    ! Shallow Convection
    !------------------
    DO k=1,kMax
       DO i=1,iMax
          q33(i,k)=q3(i,k)               ! temporary file q after deep conv
          t33(i,k)=t3(i,k)               ! temporary file T after deep conv
       END DO
    END DO
    !---------------------
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
       CALL Shallsouza(t3,q3,PS_work,sl,sens,evap,dt,iMax,kMax,kuo, &
            noshal, kctop1, kcbot1, 560.0_r8, 1.6_r8)
    END IF
    !-----------------
    ! Large Scale Precipitation
    !------------------

    Cuml_Rain=RAINCV
    DO k=1,kMax
      DO i=1,iMax
        cu_temp(i,k)=t3(i,k)
        cu_umes(i,k)=q3(i,k)
      END DO
    END DO
    
    IF(TRIM(ILCON).EQ.'YES') THEN
      CALL lrgscl(Cuml_Rain, cu_temp, dq, cu_umes, ps2, del, sl, dt, &
                mlrg, latco, iMax, kMax)

    END IF

    !-----------------
    ! Convective Cloud Computation
    !------------------
    
    IF(TRIM(iccon).EQ.'ARA'.OR.TRIM(iccon).EQ.'KUO'.OR.TRIM(iccon).EQ.'GRE')THEN
      CALL CLOUD_COVER( &
             kt     ,ktp    ,iMax   ,kbot   ,ktop   ,noshal ,kctop1 , &
             kcbot1 ,RAINCV ,fac2x  ,rccmbl ,iccon  ,convc  ,convt  , &
             convb  ,prcp1  ,prcp2  ,prcp3  ,prcpt  ,toplv  ,botlv  , &
             convts ,convcs ,convbs )
    END IF
    !***********************************
    ! move q33 to qb and t33 to tb
    ! move T3 to Tc and q3 to qc
    !***********************************
    DO i=1,iMax
       DO k=1,kMax
          qb(i,k)=q33(i,k)!q  after deep convection
          tb(i,k)=t33(i,k)!t  after deep convection
          qc(i,k)=q3 (i,k)!q after deep and shallow convection
          Tc(i,k)=T3 (i,k)!T after deep and shallow convection
       END DO
    END DO
    !-----------------
    ! Storage Diagnostic Fields
    !------------------
    CALL StoreDiagn(&
                   iMax     ,kMax     ,latco    ,rdt      ,fac2     ,fac      , &
                   fac2x    ,fdqn     ,RAINCV   ,tc       ,Work_Temp,qc       , &
                   Work_Umes,t33      ,q33      ,Cuml_Rain,cu_temp  ,cu_umes  , &
                   geshem   ,ppli     ,ppci     ,prcc     ,prct                 )    

    DO i=1,iMax
       snow(i)=fac2*rdt*1.0e3_r8*Cuml_Rain(i)
    END DO

    DO k=1,kMax
       DO i=1,iMax
          tc(i,k)=cu_temp(i,k)*(1.0_r8+delq*cu_umes(i,k))
       END DO
    END DO

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



  SUBROUTINE StoreDiagn(&
                   ncols    ,kMax     ,latco    ,rdt      ,fac2     ,fac      , &
                   fac2x    ,fdqn     ,rrr      ,StpP_Temp,Work_Temp,StpP_Umes, &
                   Work_Umes,StpC_Temp,StpC_Umes,Cuml_Rain,cu_temp  ,cu_umes  , &
                   geshem   ,ppli     ,ppci     ,prcc     ,prct                 )
                   
                   
                   
                   
    IMPLICIT NONE
    INTEGER, INTENT(IN   )    :: ncols
    INTEGER, INTENT(IN   )    :: kMax
    INTEGER, INTENT(IN   )    :: latco    
    REAL(KIND=r8)   , INTENT(IN   )    :: rdt
    REAL(KIND=r8)   , INTENT(IN   )    :: fac2
    REAL(KIND=r8)   , INTENT(IN   )    :: fac
    REAL(KIND=r8)   , INTENT(IN   )    :: fac2x
    REAL(KIND=r8)   , INTENT(IN   )    :: fdqn      (nCols,kMax)
    REAL(KIND=r8)   , INTENT(IN   )    :: rrr       (nCols)
    REAL(KIND=r8),    INTENT(in   )    :: Work_Temp (nCols,kMax)
    REAL(KIND=r8),    INTENT(in   )    :: Work_Umes (nCols,kMax)
    REAL(KIND=r8),    INTENT(in   )    :: StpC_Temp (nCols,kMax)
    REAL(KIND=r8),    INTENT(in   )    :: StpC_Umes (nCols,kMax)
    REAL(KIND=r8),    INTENT(in   )    :: Cuml_Rain (nCols)
    REAL(KIND=r8),    INTENT(in   )    :: cu_temp   (nCols,kMax)
    REAL(KIND=r8),    INTENT(in   )    :: cu_umes   (nCols,kMax)
    REAL(KIND=r8),    INTENT(inout)    :: StpP_Temp (nCols,kMax)
    REAL(KIND=r8),    INTENT(inout)    :: StpP_Umes (nCols,kMax)
    REAL(KIND=r8),    INTENT(inout)    :: geshem    (nCols)
    REAL(KIND=r8),    INTENT(inout)    :: ppli      (nCols)
    REAL(KIND=r8),    INTENT(inout)    :: ppci      (nCols)
    REAL(KIND=r8),    INTENT(inout)    :: prcc      (nCols)
    REAL(KIND=r8),    INTENT(inout)    :: prct      (ncols)

    REAL(KIND=r8)    :: bfrg   (nCols)
    REAL(KIND=r8)    :: bfr1   (nCols)
    REAL(KIND=r8)    :: bfr2   (nCols)
    REAL(KIND=r8)    :: bfr3   (nCols)
    REAL(KIND=r8)    :: dtwrk  (nCols,kMax)
    REAL(KIND=r8)    :: dqwrk  (nCols,kMax)
    LOGICAL :: ghl_local
    INTEGER :: i
    INTEGER :: k 
    ghl_local = IsGridHistoryOn()

    IF(dodia(nDiag_nshcrm))CALL updia(fdqn,nDiag_nshcrm,latco)

    IF(TRIM(iccon).EQ.'ARA'.OR.TRIM(iccon).EQ.'KUO'.OR.TRIM(iccon).EQ.'GRE')THEN

      DO i=1,ncols
         ppci  (i)=2.0e0_r8*1.0e3_r8*rrr(i)
         IF(dodia(nDiag_cvprec))bfr1(i)=fac2*rdt*1.0e3_r8*rrr(i)
      END DO

      IF (ghl_local) THEN
         IF(dogrh(nGHis_cvprec,latco))CALL StoreGridHistory(rrr(1:ncols),nGHis_cvprec,latco,fac2*rdt*1.0e3_r8)
      END IF

      IF(dodia(nDiag_cvprec))CALL updia(bfr1,nDiag_cvprec,latco)
      !
      ! snf  Calculate and save diff temp/humid due convective cloud
      !---------
      DO k=1,kMax
         DO i=1,nCols
            dtwrk(i,k)=fac*rdt*(StpP_Temp(i,k)-Work_Temp(i,k))
         END DO
      END DO

      DO k=1,kMax
         DO i=1,nCols
            dqwrk(i,k)=fac*rdt*(StpP_Umes(i,k)-Work_Umes(i,k))
         END DO
      END DO
      IF(ghl_local)THEN
         IF(dogrh(nGHis_clheat,latco))CALL StoreGridHistory(dtwrk(1:nCols,:),nGHis_clheat,latco)
         IF(dogrh(nGHis_cvmosr,latco))CALL StoreGridHistory(dqwrk(1:nCols,:),nGHis_cvmosr,latco)
      END IF
      IF(dodia(nDiag_clheat))CALL updia(dtwrk,nDiag_clheat,latco)
      IF(dodia(nDiag_cmchan))CALL updia(dqwrk,nDiag_cmchan,latco)
    END IF
    !
    !-----------
    !
    IF (doprec) THEN
       DO i=1,ncols
          prcc(i)=fac2*rdt*1.0e3_r8*rrr(i)
       END DO
    END IF
    !
    ! Calculate and save diff tem and humid due shallow convection
    !------------------

    DO k=1,kMax
       DO i=1,ncols
          dtwrk(i,k)=fac*rdt*(StpP_Temp(i,k)-StpC_Temp(i,k))
          dqwrk(i,k)=fac*rdt*(StpP_Umes(i,k)-StpC_Umes(i,k))
       END DO
    END DO

    IF(ghl_local)THEN
       IF(dogrh(nGHis_sclhea,latco))CALL StoreGridHistory(dtwrk,nGHis_sclhea,latco)
       IF(dogrh(nGHis_shcvmo,latco))CALL StoreGridHistory(dqwrk,nGHis_shcvmo,latco)
    END IF
    IF(dodia(nDiag_sclhea))CALL updia(dtwrk,nDiag_sclhea,latco)
    IF(dodia(nDiag_scmcha))CALL updia(dqwrk,nDiag_scmcha,latco)
    !---------------
    !     gdivn,gtmpn,grotn,gun,gvn are temporary working space
    !     
    IF(TRIM(ILCON).EQ.'YES') THEN
       DO k=1,kMax
          DO i=1,ncols
             dtwrk(i,k)=StpP_Temp(i,k)
             dqwrk(i,k)=StpP_Umes(i,k)
          END DO
       END DO 
       DO i=1,ncols
          ppli  (i)=2.0e0_r8*1.0e3_r8*Cuml_Rain(i)-ppci(i)
          geshem(i)=geshem(i)+fac2x*Cuml_Rain(i)
          IF(dodia(nDiag_toprec))bfr1(i)=fac2*rdt*1.0e3_r8*Cuml_Rain(i)
          IF(dodia(nDiag_lsprec))bfr3(i)=fac2*rdt*1.0e3_r8*(Cuml_Rain(i)-rrr(i))
          IF(ghl_local.AND.dogrh(nGHis_snowfl,latco))bfrg(i)=0.0e0_r8
          IF(dodia(nDiag_snowfl))bfr2(i)=0.0e0_r8
          IF(0.35_r8*cu_temp(i,1)+0.65_r8*cu_temp(i,2).LE.273.2_r8)THEN
             IF(ghl_local.AND.dogrh(nGHis_snowfl,latco))bfrg(i)=Cuml_Rain(i)
             IF(dodia(nDiag_snowfl))bfr2(i)=fac2*rdt*1.0e3_r8*Cuml_Rain(i)
          END IF
       END DO
       
       IF(ghl_local)THEN
          IF(dogrh(nGHis_toprec,latco))CALL StoreGridHistory( Cuml_Rain,nGHis_toprec,latco,fac2*rdt*1.0e3_r8)    
          IF(dogrh(nGHis_snowfl,latco))CALL StoreGridHistory( bfrg     ,nGHis_snowfl,latco,fac2*rdt*1.0e3_r8)
       END IF
       IF(dodia(nDiag_toprec))CALL updia(bfr1,nDiag_toprec,latco)
       IF(dodia(nDiag_snowfl))CALL updia(bfr2,nDiag_snowfl,latco)
       IF(dodia(nDiag_lsprec))CALL updia(bfr3,nDiag_lsprec,latco)

       DO k=1,kMax
          DO i=1,ncols
             dtwrk(i,k)=fac*rdt*(cu_temp(i,k)-dtwrk(i,k))
             dqwrk(i,k)=fac*rdt*(cu_umes(i,k)-dqwrk(i,k))
          END DO
       END DO
       
       IF(ghl_local)THEN
          IF(dogrh(nGHis_sslaht,latco))CALL StoreGridHistory(dtwrk,nGHis_sslaht,latco)
          IF(dogrh(nGHis_spstms,latco))CALL StoreGridHistory(dqwrk,nGHis_spstms,latco)
       END IF

       IF(dodia(nDiag_lslhea))CALL updia(dtwrk,nDiag_lslhea,latco)
       IF(dodia(nDiag_lsmcha))CALL updia(dqwrk,nDiag_lsmcha,latco)

    END IF
    
    IF (doprec) THEN
       DO i=1,ncols
          prct(i)=fac2*rdt*1.0e3_r8*Cuml_Rain(i)
       END DO
    END IF 
    
    DO k=1,kMax
       DO i=1,ncols 
          StpP_Umes(i,k) = cu_umes(i,k)
       END DO
    END DO    
  END SUBROUTINE StoreDiagn


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
