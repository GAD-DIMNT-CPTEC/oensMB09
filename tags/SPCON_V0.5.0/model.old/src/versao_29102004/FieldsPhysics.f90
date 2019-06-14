!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE FieldsPhysics
  USE Surface, ONLY: &
       vegin,        &
       sibwet

  USE IOLowLevel, ONLY: &
       ReadVar      , &
       ReadGetNFTGZ

  USE InputOutput, ONLY: &
       WillGetSbc, &
       getsbc, &
       nfprt , &
       nferr , &
       ifprt 

  USE Options, ONLY: &      
       isimp ,&
       nfcnv0,&
       nfsibt,&
       initlz,&        
       ifalb ,&        
       ifsst ,&        
       ifslm ,&
       ifsnw ,&
       sstlag,&
       intsst,&
       fint  ,&
       yrl   ,&      
       monl  ,&
       nftgz0,&
       mxiter,&
       nfsibi

  USE Utils, ONLY: &      
       IJtoIBJB, &
       IBJBtoIJ

  USE Sizes, ONLY: &
       sl

  USE Constants, ONLY: &
       rk

  IMPLICIT NONE

  ! Gaussian fields: 28 3D , 12 2D and 12 1D

  PRIVATE

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   convc_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   convt_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   convb_in   
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rvisbc_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rvisdc_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rnirbc_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rnirdc_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   avisd_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   dswtop_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   gl0_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   zorl_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   sheleg_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tseam_in   
  INTEGER, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: imask_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rvisb_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rvisd_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rnirb_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   rnird_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   gtsea_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tc0_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg0_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   td0_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: w0_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: capac0_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tcm_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tgm_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tdm_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: wm_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: capacm_in    
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   ppli_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   ppci_in   
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   var_in
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp1_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp2_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp3_in  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcpt_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   toplv_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   botlv_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg1_in 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg2_in   
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg3_in    
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   soilm_in





  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:)   :: sigki 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xvisb 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xvisd 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xnirb 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xnird 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xswtop
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xvisbc
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xvisdc
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xnirbc
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: xnirdc

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yvisb  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yvisd  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ynirb  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ynird  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yswtop 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yvisbc 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: yvisdc 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ynirbc 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ynirdc 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: cldsav 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ustr
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: vstr
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ssib
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convc
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convt
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convb
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convts
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convcs
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: convbs
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: htrc  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rvisbc  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rvisdc  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rnirbc  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rnirdc  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: dlwclr
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: uswtpc
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rsclr 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ultclr

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: avisb 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: avisd
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: anirb 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: anird 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: dswtop
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rs    
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: ulwtop  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: gl0   
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: zorl  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: sheleg
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: tseam

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: htr 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: clr 
  INTEGER, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: imask   
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rvisb 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rvisd 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rnirb 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: rnird 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: dlwbot
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: gtsea

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tc0 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg0 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   td0 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: w0 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: capac0 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tcm 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tgm 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tdm 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: wm 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: capacm 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   ppli   
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   ppci

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   var  

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prct  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcc  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp1 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp2 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcp3 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   prcpt 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   toplv 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   botlv 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   geshem

  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg1  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg2  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   tg3  
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   soilm 
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   sens
  REAL, PUBLIC, ALLOCATABLE, DIMENSION(:,:) ::   evap

  INTEGER, PRIVATE :: iMax 
  INTEGER, PRIVATE :: jMax

  PUBLIC ::  InitFieldsPhyscs 
  PUBLIC ::  InitVariancia
  PUBLIC ::  InitBoundCond
  PUBLIC ::  InitCheckfile
  PUBLIC ::  InitSurfTemp
  PUBLIC ::  InitGetsbc
  PUBLIC ::  restartphyscs
CONTAINS






  SUBROUTINE InitFieldsPhyscs(ibMax, kMax, jbMax,iMax_in,jMax_in)
    INTEGER, INTENT(IN) :: ibMax
    INTEGER, INTENT(IN) :: kMax
    INTEGER, INTENT(IN) :: jbMax    
    INTEGER, INTENT(IN) :: iMax_in
    INTEGER, INTENT(IN) :: jMax_in
    INTEGER :: k

    iMax=iMax_in
    jMax=jMax_in
    !
    ! map (i,j)
    !
    ALLOCATE(convc_in (iMax,jMax)) 
    convc_in=0.0
    ALLOCATE(convt_in (iMax,jMax)) 
    convt_in=0.0  
    ALLOCATE(convb_in (iMax,jMax))
    convb_in=0.0
    ALLOCATE(rvisbc_in(iMax,jMax))
    rvisbc_in=0.0
    ALLOCATE(rvisdc_in(iMax,jMax))
    rvisdc_in=0.0
    ALLOCATE(rnirbc_in(iMax,jMax))
    rnirbc_in=0.0
    ALLOCATE(rnirdc_in(iMax,jMax))
    rnirdc_in=0.0
    ALLOCATE(avisd_in (iMax,jMax)) 
    avisd_in =0.0
    ALLOCATE(dswtop_in(iMax,jMax)) 
    dswtop_in=0.0  
    ALLOCATE(gl0_in   (iMax,jMax)) 
    gl0_in=0.0 
    ALLOCATE(zorl_in  (iMax,jMax)) 
    zorl_in=0.0 
    ALLOCATE(sheleg_in(iMax,jMax)) 
    sheleg_in=0.0
    ALLOCATE(tseam_in (iMax,jMax)) 
    tseam_in=0.0 
    ALLOCATE(gtsea_in (iMax,jMax))  
    gtsea_in =0.0
    ALLOCATE(tc0_in   (iMax,jMax))   
    tc0_in   =0.0
    ALLOCATE(tg0_in   (iMax,jMax))   
    tg0_in   =0.0
    ALLOCATE(td0_in   (iMax,jMax))   
    td0_in   =0.0
    ALLOCATE(w0_in    (iMax,3,jMax)) 
    w0_in    =0.0  
    ALLOCATE(capac0_in(iMax,2,jMax)) 
    capac0_in=0.0
    ALLOCATE(tcm_in   (iMax,jMax))   
    tcm_in   =0.0
    ALLOCATE(tgm_in   (iMax,jMax))   
    tgm_in   =0.0
    ALLOCATE(tdm_in   (iMax,jMax))   
    tdm_in   =0.0 
    ALLOCATE(wm_in    (iMax,3,jMax)) 
    wm_in    =0.0
    ALLOCATE(capacm_in(iMax,2,jMax)) 
    capacm_in=0.0
    ALLOCATE(ppli_in  (iMax,jMax))  
    ppli_in =0.0
    ALLOCATE(ppci_in  (iMax,jMax))   
    ppci_in =0.0
    ALLOCATE(var_in   (iMax,jMax))   
    var_in  =0.0
    ALLOCATE(prcp1_in (iMax,jMax))
    prcp1_in=0.0
    ALLOCATE(prcp2_in (iMax,jMax))  
    prcp2_in=0.0
    ALLOCATE(prcp3_in (iMax,jMax)) 
    prcp3_in=0.0
    ALLOCATE(prcpt_in (iMax,jMax)) 
    prcpt_in=0.0
    ALLOCATE(toplv_in (iMax,jMax)) 
    toplv_in=0.0
    ALLOCATE(botlv_in (iMax,jMax)) 
    botlv_in=0.0
    ALLOCATE(tg1_in   (iMax,jMax))
    tg1_in  =0.0
    ALLOCATE(tg2_in   (iMax,jMax))
    tg2_in  =0.0
    ALLOCATE(tg3_in   (iMax,jMax))
    tg3_in  =0.0
    ALLOCATE(soilm_in (iMax,jMax))
    soilm_in=0.0
    ALLOCATE(imask_in (iMax,jMax))  
    imask_in =0
    ALLOCATE(rvisb_in (iMax,jMax))
    rvisb_in=0
    ALLOCATE(rnirb_in (iMax,jMax))
    rnirb_in=0.0
    ALLOCATE(rvisd_in (iMax,jMax))
    rvisd_in=0.0
    ALLOCATE(rnird_in (iMax,jMax))
    rnird_in=0.0
    !
    !   map(ib,jb)
    !
    ALLOCATE(sigki (kMax))
    DO k=1,kmax
       sigki (k)=1.0e0/EXP(rk*LOG(sl(k)))
    END DO
    ALLOCATE(xvisb (ibMax,jbMax))
    xvisb=0.0
    ALLOCATE(xvisd (ibMax,jbMax))
    xvisd=0.0
    ALLOCATE(xnirb (ibMax,jbMax))
    xnirb=0.0
    ALLOCATE(xnird (ibMax,jbMax))
    xnird=0.0
    ALLOCATE(xswtop(ibMax,jbMax))
    xswtop=0.0
    ALLOCATE(xvisbc(ibMax,jbMax))
    xvisbc=0.0
    ALLOCATE(xvisdc(ibMax,jbMax))
    xvisdc=0.0
    ALLOCATE(xnirbc(ibMax,jbMax))
    xnirbc=0.0
    ALLOCATE(xnirdc(ibMax,jbMax))
    xnirdc=0.0
    ALLOCATE(yvisb (ibMax,jbMax)) 
    yvisb =0.0
    ALLOCATE(yvisd (ibMax,jbMax)) 
    yvisd =0.0
    ALLOCATE(ynirb (ibMax,jbMax)) 
    ynirb =0.0
    ALLOCATE(ynird (ibMax,jbMax)) 
    ynird =0.0
    ALLOCATE(yswtop(ibMax,jbMax)) 
    yswtop=0.0
    ALLOCATE(yvisbc(ibMax,jbMax)) 
    yvisbc=0.0
    ALLOCATE(yvisdc(ibMax,jbMax)) 
    yvisdc=0.0
    ALLOCATE(ynirbc(ibMax,jbMax)) 
    ynirbc=0.0
    ALLOCATE(ynirdc(ibMax,jbMax)) 
    ynirdc=0.0
    ALLOCATE(cldsav(ibMax,jbMax)) 
    cldsav=0.0
    ALLOCATE(ustr  (ibMax,jbMax)) 
    ustr=0.0
    ALLOCATE(vstr  (ibMax,jbMax)) 
    vstr=0.0
    ALLOCATE(ssib  (ibMax,jbMax)) 
    ssib=0.0
    ALLOCATE(convc (ibMax,jbMax)) 
    convc=0.0
    ALLOCATE(convt (ibMax,jbMax)) 
    convt=0.0 
    ALLOCATE(convb (ibMax,jbMax)) 
    convb=0.0
    ALLOCATE(convts(ibMax,jbMax))
    convts=0.0
    ALLOCATE(convcs(ibMax,jbMax))
    convcs=0.0
    ALLOCATE(convbs(ibMax,jbMax))
    convbs=0.0
    ALLOCATE(htrc  (ibMax,kMax,jbMax))
    htrc  =0.0 
    ALLOCATE(rvisbc(ibMax,jbMax))
    rvisbc=0.0 
    ALLOCATE(rvisdc(ibMax,jbMax))      
    rvisdc=0.0        
    ALLOCATE(rnirbc(ibMax,jbMax))      
    rnirbc=0.0                
    ALLOCATE(rnirdc(ibMax,jbMax))          
    rnirdc=0.0
    ALLOCATE(dlwclr(ibMax,jbMax))
    dlwclr=0.0
    ALLOCATE(uswtpc(ibMax,jbMax))
    uswtpc=0.0
    ALLOCATE(rsclr (ibMax,jbMax))
    rsclr =0.0
    ALLOCATE(ultclr(ibMax,jbMax))
    ultclr=0.0
    ALLOCATE(avisb (ibMax,jbMax)) 
    avisb =0.0     
    ALLOCATE(avisd (ibMax,jbMax)) 
    avisd =0.0
    ALLOCATE(anirb (ibMax,jbMax)) 
    anirb =0.0
    ALLOCATE(anird (ibMax,jbMax)) 
    anird =0.0 
    ALLOCATE(dswtop(ibMax,jbMax)) 
    dswtop=0.0
    ALLOCATE(rs    (ibMax,jbMax)) 
    rs   =0.0
    ALLOCATE(ulwtop(ibMax,jbMax)) 
    ulwtop=0.0  
    ALLOCATE(gl0   (ibMax,jbMax)) 
    gl0   =0.0    
    ALLOCATE(zorl  (ibMax,jbMax)) 
    zorl  =0.0 
    ALLOCATE(sheleg(ibMax,jbMax)) 
    sheleg=0.0
    ALLOCATE(tseam (ibMax,jbMax)) 
    tseam=0.0
    ALLOCATE(htr   (ibMax,kMax,jbMax))  
    htr   =0.0
    ALLOCATE(clr   (ibMax,kMax,jbMax))  
    clr   =0.0
    ALLOCATE(imask (ibMax,jbMax))  
    imask =0
    ALLOCATE(rvisb (ibMax,jbMax))  
    rvisb =0.0
    ALLOCATE(rvisd (ibMax,jbMax))  
    rvisd =0.0
    ALLOCATE(rnirb (ibMax,jbMax))  
    rnirb =0.0
    ALLOCATE(rnird (ibMax,jbMax))  
    rnird =0.0
    ALLOCATE(dlwbot(ibMax,jbMax))  
    dlwbot=0.0
    ALLOCATE(gtsea (ibMax,jbMax))  
    gtsea =0.0
    ALLOCATE(tc0   (ibMax,jbMax))   
    tc0   =0.0
    ALLOCATE(tg0   (ibMax,jbMax))   
    tg0   =0.0  
    ALLOCATE(td0   (ibMax,jbMax))   
    td0   =0.0 
    ALLOCATE(w0    (ibMax,3,jbMax)) 
    w0    =0.0 
    ALLOCATE(capac0(ibMax,2,jbMax)) 
    capac0=0.0 
    ALLOCATE(tcm   (ibMax,jbMax))   
    tcm   =0.0  
    ALLOCATE(tgm   (ibMax,jbMax))   
    tgm   =0.0
    ALLOCATE(tdm   (ibMax,jbMax))   
    tdm   =0.0     
    ALLOCATE(wm    (ibMax,3,jbMax)) 
    wm    =0.0
    ALLOCATE(capacm(ibMax,2,jbMax)) 
    capacm=0.0
    ALLOCATE(ppli  (ibMax,jbMax))   
    ppli  =0.0    
    ALLOCATE(ppci  (ibMax,jbMax))   
    ppci  =0.0
    ALLOCATE(var   (ibMax,jbMax))   
    var   =0.0
    ALLOCATE(prct  (ibMax,jbMax))  
    prct   =0.0
    ALLOCATE(prcc  (ibMax,jbMax))  
    prcc    =0.0
    ALLOCATE(prcp1 (ibMax,jbMax))  
    prcp1  =0.0
    ALLOCATE(prcp2 (ibMax,jbMax))  
    prcp2  =0.0
    ALLOCATE(prcp3 (ibMax,jbMax))  
    prcp3  =0.0
    ALLOCATE(prcpt (ibMax,jbMax))  
    prcpt   =0.0
    ALLOCATE(toplv (ibMax,jbMax))  
    toplv   =0.0
    ALLOCATE(botlv (ibMax,jbMax))  
    botlv  =0.0
    ALLOCATE(geshem(ibMax,jbMax))  
    geshem =0.0 
    ALLOCATE(tg1   (ibMax,jbMax))
    tg1    =0.0
    ALLOCATE(tg2   (ibMax,jbMax))
    tg2    =0.0
    ALLOCATE(tg3   (ibMax,jbMax))
    tg3    =0.0
    ALLOCATE(soilm (ibMax,jbMax))
    soilm  =0.0
    ALLOCATE(sens  (ibMax,jbMax))
    sens  =0.0
    ALLOCATE(evap  (ibMax,jbMax))
    evap  =0.0
  END SUBROUTINE InitFieldsPhyscs







  SUBROUTINE InitVariancia(igwd,nfvar,fNameOrgvar)
    CHARACTER(LEN=*) , INTENT(in   ) :: igwd
    INTEGER          , INTENT(in   ) :: nfvar
    CHARACTER(LEN=*) , INTENT(in   ) :: fNameOrgvar
    OPEN(nfvar, file=TRIM(fNameOrgvar),ACTION="read",FORM="UNFORMATTED")
    IF(igwd.EQ.'YES ') THEN
       CALL ReadVar(nfvar,var_in)
       CALL IJtoIBJB(var_in,var)
    END IF
  END SUBROUTINE InitVariancia







  SUBROUTINE InitBoundCond(&
       ibMax,jbMax,ifdy,todcld,ids,idc,ifday, &
       tod,totm,todsib,idate,idatec,si,sl, &
       fNameSibmsk,fNameTg3zrl  ,ibMaxPerJB)

    INTEGER         , INTENT(IN   ) :: ibMax   
    INTEGER         , INTENT(IN   ) :: jbMax   
    INTEGER         , INTENT(OUT  ) :: ifdy
    REAL            , INTENT(OUT  ) :: todcld
    INTEGER         , INTENT(OUT  ) :: ids(:)
    INTEGER         , INTENT(OUT  ) :: idc(:)
    INTEGER         , INTENT(IN   ) :: ifday
    REAL            , INTENT(IN   ) :: tod
    REAL            , INTENT(OUT  ) :: totm 
    REAL            , INTENT(OUT  ) :: todsib
    INTEGER         , INTENT(IN   ) :: idate(:) 
    INTEGER         , INTENT(IN   ) :: idatec(:)

    REAL            , INTENT(IN   ) :: si(:)
    REAL            , INTENT(IN   ) :: sl(:)
    INTEGER         , INTENT(IN   ) :: ibMaxPerJB(:)
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameSibmsk
    CHARACTER(LEN=*), INTENT(IN   ) :: fNameTg3zrl

    REAL                            :: tice  =271.16e0
    REAL                            :: t0    
    REAL                            :: sinmax
    INTEGER                         :: j     
    INTEGER                         :: i   
    INTEGER                         :: ncount
    REAL                            :: wsib  (ibMax,jbMax)
    REAL                            :: zero  =0.0e3
    REAL                            :: thousd=1.0e3
    REAL            , PARAMETER     :: xl0   =10.

    IF(isimp.NE.'YES ') THEN
       IF(nfcnv0.NE.0) THEN
          READ(nfcnv0) ifdy,todcld,ids,idc          
          READ(nfcnv0) convc,convt,convb,prcp1,prcp2,prcp3, &
               prcpt,toplv,botlv
          IF(ifday.GT.0.OR.tod.GT.0.0)READ(nfcnv0)rvisd,rvisb,rnird, &
               rnirb,rvisdc,rvisbc,rnirdc,rnirbc,dswtop,totm

          REWIND nfcnv0

          IF(ifprt(4) .GE. 1)WRITE(nfprt,555)ifdy,todcld,ids,idc
          !CALL IJtoIBJB( convc_in ,convc )
          !CALL IJtoIBJB( convt_in ,convt )
          !CALL IJtoIBJB( convb_in ,convb )
          !CALL IJtoIBJB( prcp1_in ,prcp1 )
          !CALL IJtoIBJB( prcp2_in ,prcp2 )
          !CALL IJtoIBJB( prcp3_in ,prcp3 )
          !CALL IJtoIBJB( prcpt_in ,prcpt )
          !CALL IJtoIBJB( toplv_in ,toplv )
          !CALL IJtoIBJB( botlv_in ,botlv )
          !CALL IJtoIBJB( rvisd_in ,rvisd )
          !CALL IJtoIBJB( rvisb_in ,rvisb )
          !CALL IJtoIBJB( rnird_in ,rnird )
          !CALL IJtoIBJB( rnirb_in ,rnirb )
          !CALL IJtoIBJB( rvisdc_in,rvisdc)
          !CALL IJtoIBJB( rvisbc_in,rvisbc)
          !CALL IJtoIBJB( rnirdc_in,rnirdc)
          !CALL IJtoIBJB( rnirbc_in,rnirbc)
          !CALL IJtoIBJB( dswtop_in,dswtop)
       ELSE
          convc=0.0
          convt=0.0
          convb=0.0
          prcp1=0.0
          prcp2=0.0
          prcp3=0.0
          prcpt=0.0
          toplv=0.0
          botlv=0.0
       END IF

       sheleg=0.0
       sheleg_in=0.0

       CALL vegin (si(1) ,sl(1))

       OPEN(nfsibt, file=TRIM(fNameSibmsk),ACTION="read",FORM="UNFORMATTED")   
       OPEN(nftgz0, file=TRIM(fNameTg3zrl),ACTION="read",FORM="UNFORMATTED")     

       READ  (nfsibt) imask_in

       CALL IJtoIBJB( imask_in,imask)

       REWIND nfsibt 
       !
       !
       !     initialize sib variables
       !
       IF(ifday.EQ.0.AND.tod.EQ.zero.AND.initlz.GE.0) THEN

          CALL IBJBtoIJ(avisd ,avisd_in )
          CALL IBJBtoIJ(gtsea ,gtsea_in )
          CALL IBJBtoIJ(soilm ,soilm_in )
          CALL IBJBtoIJ(sheleg,sheleg_in)

          CALL getsbc (iMax ,jMax  ,avisd_in,gtsea_in,soilm_in,sheleg_in,ifday , &
               tod  ,idate ,idatec,nfprt ,ifprt  ,ifalb , &
               ifsst,ifslm ,ifsnw ,sstlag,intsst,fint ,tice  , &
               yrl  ,monl)


          CALL IJtoIBJB(avisd_in ,avisd )
          CALL IJtoIBJB(gtsea_in ,gtsea )
          CALL IJtoIBJB(soilm_in ,soilm )
          CALL IJtoIBJB(sheleg_in,sheleg)

          CALL ReadGetNFTGZ(nftgz0,tg1_in,tg2_in,tg3_in,zorl_in)

          CALL IJtoIBJB(tg1_in ,tg1  )
          CALL IJtoIBJB(tg2_in ,tg2  )
          CALL IJtoIBJB(tg3_in ,tg3  )
          CALL IJtoIBJB(zorl_in,zorl )

          t0    =271.17
          sinmax=150.0
          !
          !     use rvisd as temporary for abs(soilm)
          !
          DO j=1,jbMax
             DO i=1,ibMaxPerJB(j)
                rvisd(i,j)=ABS(soilm(i,j))
             END DO
          END DO

          CALL sibwet(ibMax,jbMax,rvisd,sinmax,imask,wsib,ssib, &
                      mxiter,nfprt,ifprt,ibMaxPerJB)

          ppli=0.0
          ppci=0.0  
          capac0=0.0  
          capacm=0.0  
          !
          !     td0 (deep soil temp) is temporarily defined as tg3
          !
          DO j=1,jbMax
             ncount=0
             DO i=1,ibMaxPerJB(j)
                gl0(i,j)=xl0
                tseam(i,j)=gtsea(i,j)

                IF(imask(i,j).EQ.0) THEN
                   IF(-gtsea(i,j).LT.t0) THEN
                      imask(i,j)=-1
                   END IF
                ELSE
                   ncount=ncount+1
                   w0    (ncount,1,j)=wsib(i,j)
                   w0    (ncount,2,j)=wsib(i,j)
                   w0    (ncount,3,j)=wsib(i,j)
                   td0   (ncount,  j)=tg3 (i,j)
                   wm    (ncount,1,j)=wsib(i,j)
                   wm    (ncount,2,j)=wsib(i,j)
                   wm    (ncount,3,j)=wsib(i,j)
                   tdm   (ncount,  j)=tg3 (i,j)
                   tgm   (ncount,  j)=tg3 (i,j)
                   tcm   (ncount,  j)=tg3 (i,j)
                   ssib  (ncount,j  )=0.0
                   IF(soilm(i,j).LT.0.0)ssib(ncount,j)=wsib(i,j)

                   IF(sheleg(i,j).GT.zero) THEN
                      capac0(ncount,2,j)=sheleg(i,j)/thousd
                      capacm(ncount,2,j)=sheleg(i,j)/thousd
                   END IF

                END IF
             END DO
          END DO

       ELSE

          READ(nfsibi)ifdy,todsib,ids,idc
          READ(nfsibi) td0   ,tdm
          READ(nfsibi) tg0   ,tgm
          READ(nfsibi) tc0   ,tcm
          READ(nfsibi) w0    ,wm
          READ(nfsibi) capac0,capacm
          READ(nfsibi) ppci  ,ppli 
          READ(nfsibi) gl0   ,zorl  ,gtsea ,tseam

          !CALL IJtoIBJB(td0_in    ,td0     )
          !CALL IJtoIBJB(tg0_in    ,tg0     )
          !CALL IJtoIBJB(tc0_in    ,tc0     )
          !CALL IJtoIBJB(w0_in(:,1,:),w0(:,1,:)) 
          !CALL IJtoIBJB(w0_in(:,2,:),w0(:,2,:)) 
          !CALL IJtoIBJB(w0_in(:,3,:),w0(:,3,:)) 
          !CALL IJtoIBJB(capac0_in(:,1,:),capac0(:,1,:))
          !CALL IJtoIBJB(capac0_in(:,2,:),capac0(:,2,:))   
          !CALL IJtoIBJB(tdm_in    ,tdm     )
          !CALL IJtoIBJB(tgm_in    ,tgm     )
          !CALL IJtoIBJB(tcm_in    ,tcm     )
          !CALL IJtoIBJB(wm_in(:,1,:),wm(:,1,:))
          !CALL IJtoIBJB(wm_in(:,2,:),wm(:,2,:))
          !CALL IJtoIBJB(wm_in(:,3,:),wm(:,3,:))
          !CALL IJtoIBJB(capacm_in(:,1,:),capacm(:,1,:))
          !CALL IJtoIBJB(capacm_in(:,2,:),capacm(:,2,:))
          !CALL IJtoIBJB(ppci_in   ,ppci    )
          !CALL IJtoIBJB(ppli_in   ,ppli    )
          !CALL IJtoIBJB(gl0_in    ,gl0     )
          !CALL IJtoIBJB(zorl_in   ,zorl    )
          !CALL IJtoIBJB(gtsea_in  ,gtsea   )
          !CALL IJtoIBJB(tseam_in  ,tseam   )

          REWIND nfsibi





          IF(initlz.LT.0.AND.initlz.GT.-3)THEN

             CALL IBJBtoIJ(avisd ,avisd_in )
             CALL IBJBtoIJ(gtsea ,gtsea_in )
             CALL IBJBtoIJ(soilm ,soilm_in )
             CALL IBJBtoIJ(sheleg,sheleg_in)

             CALL getsbc(iMax ,jMax ,avisd_in ,gtsea_in ,soilm_in ,sheleg_in,ifday ,tod  , &
                  idate ,idatec,nfprt ,ifprt  ,ifalb ,ifsst ,ifslm, &
                  ifsnw ,sstlag,intsst,fint  ,tice  ,yrl   ,monl)


             CALL IJtoIBJB(avisd_in   ,avisd    )
             CALL IJtoIBJB(gtsea_in   ,gtsea    )
             CALL IJtoIBJB(soilm_in   ,soilm    )
             CALL IJtoIBJB(sheleg_in  ,sheleg   )
          END IF

          DO j=1,jbMax
             ncount=0
             DO i=1,ibMaxPerJB(j)
                IF(imask(i,j).GT.0)THEN
                   ncount=ncount+1
                   ssib(ncount,j)=0.0
                   IF(w0(ncount,1,j).LT.0.0)THEN
                      ssib(ncount,j)=ABS(w0(ncount,1,j))
                      w0(ncount,1,j)=ABS(w0(ncount,1,j))
                      w0(ncount,2,j)=ABS(w0(ncount,2,j))
                      w0(ncount,3,j)=ABS(w0(ncount,3,j)) 
                      wm(ncount,1,j)=ABS(wm(ncount,1,j))
                      wm(ncount,2,j)=ABS(wm(ncount,2,j)) 
                      wm(ncount,3,j)=ABS(wm(ncount,3,j)) 
                   END IF
                END IF
             END DO
          END DO

          IF(ifprt(5).GE.1)WRITE(nfprt,444) ifdy,todsib,ids,idc

       END IF
    END IF

444 FORMAT(' SIB PROGNOSTIC VARIABLES READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)
555 FORMAT(' CLOUD PROGNOSTIC DATA READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)  
  END SUBROUTINE InitBoundCond







  SUBROUTINE InitCheckfile(&
       jbMax  ,ifdy  ,todcld,ids   ,idc   ,ifday , &
       tod   ,idate ,idatec   ,todsib,ibMaxPerJB )

    INTEGER, INTENT(IN   ) :: jbMax 
    INTEGER, INTENT(OUT  ) :: ifdy  
    REAL   , INTENT(OUT  ) :: todcld
    INTEGER, INTENT(OUT  ) :: ids   (:)    
    INTEGER, INTENT(OUT  ) :: idc   (:) 
    INTEGER, INTENT(IN   ) :: ifday 
    REAL   , INTENT(IN   ) :: tod   
    INTEGER, INTENT(IN   ) :: idate (:) 
    INTEGER, INTENT(IN   ) :: idatec(:)
    REAL   , INTENT(OUT  ) :: todsib
    INTEGER, INTENT(IN   ) :: ibMaxPerJB(:)

    INTEGER                :: j  
    INTEGER                :: ncount
    INTEGER                :: i  
    REAL                   :: tice  =271.16e0
    !
    !     read cloud dataset for cold start
    !     
    IF(nfcnv0.NE.0) THEN
       READ(nfcnv0) ifdy,todcld,ids,idc
       READ(nfcnv0) convc,convt,convb,prcp1,prcp2,prcp3, &
            prcpt,toplv,botlv

       !CALL IJtoIBJB(convc_in,convc )
       !CALL IJtoIBJB(convt_in,convt )
       !CALL IJtoIBJB(convb_in,convb )
       !CALL IJtoIBJB(prcp1_in,prcp1 )
       !CALL IJtoIBJB(prcp2_in,prcp2 )
       !CALL IJtoIBJB(prcp3_in,prcp3 )
       !CALL IJtoIBJB(prcpt_in,prcpt )
       !CALL IJtoIBJB(toplv_in,toplv )
       !CALL IJtoIBJB(botlv_in,botlv )

       REWIND nfcnv0

       IF(ifprt(4) .GE. 1) WRITE(nfprt,555)ifdy,todcld,ids,idc

    ELSE
       convc=0.0
       convt=0.0
       convb=0.0
       prcp1=0.0
       prcp2=0.0
       prcp3=0.0
       prcpt=0.0
       toplv=0.0
       botlv=0.0
    END IF


    IF(initlz.LT.0)THEN

       READ(nfsibi) ifdy,todsib,ids,idc
       READ(nfsibi) td0   ,tdm
       READ(nfsibi) tg0   ,tgm
       READ(nfsibi) tc0   ,tcm
       READ(nfsibi) w0    ,wm
       READ(nfsibi) capac0,capacm
       READ(nfsibi) ppci  ,ppli
       READ(nfsibi) gl0   ,zorl  ,gtsea,tseam

       !CALL IJtoIBJB(td0_in   ,td0    )  
       !CALL IJtoIBJB(tg0_in   ,tg0    ) 
       !CALL IJtoIBJB(tc0_in   ,tc0    )
       !CALL IJtoIBJB(w0_in(:,1,:),w0(:,1,:)) 
       !CALL IJtoIBJB(w0_in(:,2,:),w0(:,2,:)) 
       !CALL IJtoIBJB(w0_in(:,3,:),w0(:,3,:)) 
       !CALL IJtoIBJB(capac0_in(:,1,:),capac0(:,1,:))
       !CALL IJtoIBJB(capac0_in(:,2,:),capac0(:,2,:) )
       !CALL IJtoIBJB(tdm_in   ,tdm    )
       !CALL IJtoIBJB(tgm_in   ,tgm    )
       !CALL IJtoIBJB(tcm_in   ,tcm    )
       !CALL IJtoIBJB(wm_in(:,1,:),wm(:,1,:))
       !CALL IJtoIBJB(wm_in(:,2,:),wm(:,2,:))
       !CALL IJtoIBJB(wm_in(:,3,:),wm(:,3,:))
       !CALL IJtoIBJB(capacm_in(:,1,:),capacm(:,1,:) )
       !CALL IJtoIBJB(capacm_in(:,2,:),capacm(:,2,:) )
       !CALL IJtoIBJB(ppci_in  ,ppci   )
       !CALL IJtoIBJB(ppli_in  ,ppli   )
       !CALL IJtoIBJB(gl0_in   ,gl0    )
       !CALL IJtoIBJB(zorl_in  ,zorl   )
       !CALL IJtoIBJB(gtsea_in ,gtsea  )
       !CALL IJtoIBJB(tseam_in ,tseam  )   

       REWIND nfsibi

       CALL IBJBtoIJ(avisd ,avisd_in )
       CALL IBJBtoIJ(gtsea ,gtsea_in )
       CALL IBJBtoIJ(soilm ,soilm_in )
       CALL IBJBtoIJ(sheleg,sheleg_in)

       CALL getsbc (iMax ,jMax ,avisd_in ,gtsea_in ,soilm_in ,sheleg_in,ifday,tod   , &
            idate ,idatec,nfprt ,ifprt    ,ifalb ,ifsst,ifslm , &
            ifsnw ,sstlag,intsst,fint  ,tice  ,yrl  ,monl)


       CALL IJtoIBJB(avisd_in  ,avisd )   
       CALL IJtoIBJB(gtsea_in  ,gtsea )   
       CALL IJtoIBJB(soilm_in  ,soilm )   
       CALL IJtoIBJB(sheleg_in ,sheleg)   

       DO j=1,jbMax
          ncount=0
          DO i=1,ibMaxPerJB(j)
             IF(imask(i,j).GT.0)THEN
                ncount=ncount+1
                ssib(ncount,j)=0.0
                IF(w0(ncount,1,j).LT.0.0)THEN
                   ssib(ncount,  j)=ABS(w0(ncount,1,j))
                   w0  (ncount,1,j)=ABS(w0(ncount,1,j))
                   w0  (ncount,2,j)=ABS(w0(ncount,2,j))
                   w0  (ncount,3,j)=ABS(w0(ncount,3,j))
                   wm  (ncount,1,j)=ABS(wm(ncount,1,j))
                   wm  (ncount,2,j)=ABS(wm(ncount,2,j))
                   wm  (ncount,3,j)=ABS(wm(ncount,3,j))
                END IF
             END IF
          END DO
       END DO
       IF(ifprt(5).GE.1)WRITE(nfprt,444) ifdy,todsib,ids,idc
    END IF

444 FORMAT(' SIB PROGNOSTIC VARIABLES READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)
555 FORMAT(' CLOUD PROGNOSTIC DATA READ IN. AT FORECAST DAY', &
         I8,' TOD ',F8.1/' STARTING',3I3,I5,' CURRENT',3I3,I5)  

  END SUBROUTINE InitCheckfile








  SUBROUTINE InitSurfTemp (jbMax ,ibMaxPerJB)

    INTEGER, INTENT(IN   ) :: jbMax
    INTEGER, INTENT(IN   ) :: ibMaxPerJB(:)
    INTEGER                :: i
    INTEGER                :: j
    INTEGER                :: ncount 
    REAL                   :: zero  =0.0e3
    REAL                   :: thousd=1.0e3
    REAL                   :: tf    =273.16e0
    capacm=0.0
    capac0=0.0
    DO j=1,jbMax
       ncount=0
       DO i=1,ibMaxPerJB(j)
          IF(imask(i,j).GT.0) THEN
             ncount=ncount+1
             IF(sheleg(i,j).GT.zero) THEN
                capac0(ncount,2,j) = sheleg(i,j)/thousd
                capacm(ncount,2,j) = sheleg(i,j)/thousd
                tg0   (ncount,  j) = MIN(tg0(ncount,j),tf-0.01e0)
                tgm   (ncount,  j) = MIN(tgm(ncount,j),tf-0.01e0)
             END IF
          END IF
       END DO
    END DO
  END SUBROUTINE InitSurfTemp







  SUBROUTINE InitGetsbc(ifday ,tod   ,idate ,idatec)
    !
    ! getsbc :read surface boundary conditions.
    !
    INTEGER, INTENT(in   ) :: ifday
    REAL   , INTENT(in   ) :: tod
    INTEGER, INTENT(in   ) :: idate (4) 
    INTEGER, INTENT(in   ) :: idatec(4)

    REAL                   :: tice  =271.16e0
    !

    IF (WillGetSbc(idate, tod, fint)) THEN
       CALL IBJBtoIJ(avisd ,avisd_in )
       CALL IBJBtoIJ(gtsea ,gtsea_in )
       CALL IBJBtoIJ(soilm ,soilm_in )
       CALL IBJBtoIJ(sheleg,sheleg_in)
       
       !$OMP SINGLE
       CALL getsbc (iMax ,jMax  ,avisd_in,gtsea_in,soilm_in,sheleg_in,ifday , &
            tod  ,idate ,idatec,nfprt ,ifprt   ,ifalb , &
            ifsst,ifslm ,ifsnw ,sstlag,intsst,fint ,tice  , &
            yrl  ,monl)
       !$OMP END SINGLE
       
       CALL IJtoIBJB(avisd_in ,avisd )
       CALL IJtoIBJB(gtsea_in ,gtsea )
       CALL IJtoIBJB(soilm_in ,soilm )
       CALL IJtoIBJB(sheleg_in,sheleg)
    END IF

  END SUBROUTINE InitGetsbc






  SUBROUTINE restartphyscs (jbMax,ifday,tod,idate ,idatec, &
       nfsibo,nfcnv1,totm,ibMaxPerJB)

    INTEGER           ,INTENT(IN   ) :: jbMax
    INTEGER           ,INTENT(IN   ) :: ifday
    REAL              ,INTENT(IN   ) :: tod
    INTEGER           ,INTENT(IN   ) :: idate(:)
    INTEGER           ,INTENT(IN   ) :: idatec(:)
    INTEGER           ,INTENT(IN   ) :: nfsibo
    INTEGER           ,INTENT(IN   ) :: nfcnv1
    REAL              ,INTENT(IN   ) :: totm
    INTEGER           ,INTENT(IN   ) :: ibMaxPerJB(:)
    INTEGER                         :: i
    INTEGER                         :: j
    INTEGER                         :: ncount

    IF(isimp.NE.'YES ') THEN

       !$OMP DO PRIVATE(ncount, i)
       DO j=1,jbMax
          ncount=0
          DO i=1,ibMaxPerJB(j)
             IF(imask(i,j).GT.0)THEN
                ncount=ncount+1
                IF(ssib(ncount,j).GT.0.0)THEN        
                   w0  (ncount,1,j)=-ssib(ncount,j)
                   w0  (ncount,2,j)=-ssib(ncount,j)
                   w0  (ncount,3,j)=-ssib(ncount,j)
                   wm  (ncount,1,j)=-ssib(ncount,j)
                   wm  (ncount,2,j)=-ssib(ncount,j)
                   wm  (ncount,3,j)=-ssib(ncount,j)
                END IF
             END IF
          END DO
       END DO

       !CALL IBJBtoIJ(imask ,imask_in ) 
       !CALL IBJBtoIJ(w0(:,1,:),w0_in (:,1,:)   )
       !CALL IBJBtoIJ(w0(:,2,:),w0_in (:,2,:)   )
       !CALL IBJBtoIJ(w0(:,3,:),w0_in (:,3,:)   )
       !CALL IBJBtoIJ(wm(:,1,:),wm_in (:,1,:)   )
       !CALL IBJBtoIJ(wm(:,2,:),wm_in (:,2,:)   )
       !CALL IBJBtoIJ(wm(:,3,:),wm_in (:,3,:)   )
       !CALL IBJBtoIJ(td0   ,td0_in   )
       !CALL IBJBtoIJ(tdm   ,tdm_in   )
       !CALL IBJBtoIJ(tg0   ,tg0_in   )
       !CALL IBJBtoIJ(tgm   ,tgm_in   )
       !CALL IBJBtoIJ(tc0   ,tc0_in   )
       !CALL IBJBtoIJ(tcm   ,tcm_in   )
       !CALL IBJBtoIJ(capac0(:,1,:),capac0_in(:,1,:))
       !CALL IBJBtoIJ(capac0(:,2,:),capac0_in(:,2,:))
       !CALL IBJBtoIJ(capacm(:,1,:),capacm_in(:,1,:))
       !CALL IBJBtoIJ(capacm(:,2,:),capacm_in(:,2,:))
       !CALL IBJBtoIJ(ppci  ,ppci_in  )
       !CALL IBJBtoIJ(ppli  ,ppli_in  )
       !CALL IBJBtoIJ(gl0   ,gl0_in   )
       !CALL IBJBtoIJ(zorl  ,zorl_in  )
       !CALL IBJBtoIJ(gtsea ,gtsea_in )
       !CALL IBJBtoIJ(tseam ,tseam_in )
       !CALL IBJBtoIJ(convc ,convc_in )
       !CALL IBJBtoIJ(convt ,convt_in )
       !CALL IBJBtoIJ(convb ,convb_in )
       !CALL IBJBtoIJ(prcp1 ,prcp1_in ) 
       !CALL IBJBtoIJ(prcp2 ,prcp2_in )
       !CALL IBJBtoIJ(prcp3 ,prcp3_in )
       !CALL IBJBtoIJ(prcpt ,prcpt_in )
       !CALL IBJBtoIJ(toplv ,toplv_in )
       !CALL IBJBtoIJ(botlv ,botlv_in )
       !CALL IBJBtoIJ(rvisd ,rvisd_in )
       !CALL IBJBtoIJ(rvisb ,rvisb_in )
       !CALL IBJBtoIJ(rnird ,rnird_in )
       !CALL IBJBtoIJ(rnirb ,rnirb_in )
       !CALL IBJBtoIJ(rvisdc,rvisdc_in)
       !CALL IBJBtoIJ(rvisbc,rvisbc_in)
       !CALL IBJBtoIJ(rnirdc,rnirdc_in)
       !CALL IBJBtoIJ(rnirbc,rnirbc_in)
       !CALL IBJBtoIJ(dswtop,dswtop_in)

       !$OMP SINGLE
       WRITE(nfsibo) ifday,tod,idate,idatec
       WRITE(nfsibo) td0,tdm
       WRITE(nfsibo) tg0,tgm
       WRITE(nfsibo) tc0,tcm
       WRITE(nfsibo) w0 ,wm
       WRITE(nfsibo) capac0,capacm
       WRITE(nfsibo) ppci,ppli
       WRITE(nfsibo) gl0 ,zorl,gtsea,tseam

       WRITE(nfcnv1) ifday,tod,idate,idatec
       WRITE(nfcnv1) convc,convt,convb,prcp1,prcp2,prcp3, &
            prcpt,toplv,botlv
       WRITE(nfcnv1) rvisd,rvisb,rnird,rnirb, &
            rvisdc,rvisbc,rnirdc,rnirbc,dswtop,totm
       !$OMP END SINGLE
    END IF

  END SUBROUTINE restartphyscs
END MODULE FieldsPhysics
