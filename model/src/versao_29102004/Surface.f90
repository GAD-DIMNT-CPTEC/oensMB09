!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE Surface

  ! InitSurface 
  !
  ! fysiks ------| pbl ------| root
  !              |           |
  !              |           | raduse
  !              |           |
  !              |           | stomat
  !              |           |
  !              |           | interc
  !              |           |
  !              |           | sflxes ------| vntlax
  !              |                          |
  !              |                          | rbrd
  !              |                          |
  !              |                          | cut
  !              |                          |
  !              |                          | stres2
  !              |                          |
  !              |                          | temres
  !              |                          |
  !              |                          | update
  !              |                          |
  !              |                          | airmod
  !              |              
  !              | snowm
  !              |
  !              | runoff
  !
  ! seasfc ------| vntlt1
  !
  ! sextrp
  !
  ! sibwet ------| extrak
  !
  ! radalb 
  !
  ! vegin
  !
  ! wheat

  USE Constants, ONLY :     &
       gasr,          &
       pie,           &
       cp,            &
       hl,            &
       grav,          &
       stefan,        &
       snomel,        &
       tf,            &
       epsfac,        &
       clai,          &
       athird,        &
       cw,            &
       z0ice

  USE InputOutput, ONLY: &
       nfprt

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: InitSurface
  PUBLIC :: fysiks
  PUBLIC :: seasfc
  PUBLIC :: sextrp
  PUBLIC :: radalb
  PUBLIC :: vegin
  PUBLIC :: wheat
  PUBLIC :: sibwet
  PUBLIC :: ityp
  PUBLIC :: imon
  PUBLIC :: icg
  PUBLIC :: x0x   
  PUBLIC :: xd    
  PUBLIC :: xdc   
  PUBLIC :: xbc
  PUBLIC :: CopySurfaceData

  REAL   :: expcut
  REAL   :: rbyg

  REAL   , ALLOCATABLE :: cedfu (:,:,:)       
  REAL   , ALLOCATABLE :: cedir (:,:,:,:)     
  REAL   , ALLOCATABLE :: cedfu1(:,:,:,:,:)   
  REAL   , ALLOCATABLE :: cedir1(:,:,:,:,:,:) 
  REAL   , ALLOCATABLE :: cedfu2(:,:,:,:,:)   
  REAL   , ALLOCATABLE :: cedir2(:,:,:,:,:,:) 
  REAL   , ALLOCATABLE :: cledfu(:,:,:)       
  REAL   , ALLOCATABLE :: cledir(:,:,:,:)     
  REAL   , ALLOCATABLE :: xmiu  (:,:)         
  REAL   , ALLOCATABLE :: cether(:,:,:)       
  REAL   , ALLOCATABLE :: xmiw  (:,:)         

  REAL   , ALLOCATABLE :: ystpar(:,:)            
  REAL   , ALLOCATABLE :: yopt  (:)            
  REAL   , ALLOCATABLE :: yll   (:)            
  REAL   , ALLOCATABLE :: yu    (:)            
  REAL   , ALLOCATABLE :: yefac (:)            
  REAL   , ALLOCATABLE :: yh1   (:)            
  REAL   , ALLOCATABLE :: yh2   (:)            
  REAL   , ALLOCATABLE :: yootd (:)            
  REAL   , ALLOCATABLE :: yreen (:,:)         
  REAL   , ALLOCATABLE :: ycover(:,:)         
  REAL   , ALLOCATABLE :: ylt   (:,:)            


  REAL   , ALLOCATABLE :: rstpar_fixed    (:,:,:)   
  REAL   , ALLOCATABLE :: chil_fixed      (:,:)     
  REAL   , ALLOCATABLE :: topt_fixed      (:,:)     
  REAL   , ALLOCATABLE :: tll_fixed          (:,:)     
  REAL   , ALLOCATABLE :: tu_fixed          (:,:)     
  REAL   , ALLOCATABLE :: defac_fixed     (:,:)     
  REAL   , ALLOCATABLE :: ph1_fixed          (:,:)     
  REAL   , ALLOCATABLE :: ph2_fixed          (:,:)     
  REAL   , ALLOCATABLE :: rootd     (:,:)     
  REAL   , ALLOCATABLE :: bee          (:)            
  REAL   , ALLOCATABLE :: phsat     (:)       
  REAL   , ALLOCATABLE :: satco     (:)       
  REAL   , ALLOCATABLE :: poros     (:)       
  REAL   , ALLOCATABLE :: zdepth    (:,:)     
  REAL   , ALLOCATABLE :: green_fixed     (:,:,:)   
  REAL   , ALLOCATABLE :: xcover_fixed    (:,:,:)   
  REAL   , ALLOCATABLE :: zlt_fixed          (:,:,:)   
  REAL   , ALLOCATABLE :: x0x          (:,:)      
  REAL   , ALLOCATABLE :: xd          (:,:)      
  REAL   , ALLOCATABLE :: z2          (:,:)      
  REAL   , ALLOCATABLE :: z1          (:,:)      
  REAL   , ALLOCATABLE :: xdc          (:,:)      
  REAL   , ALLOCATABLE :: xbc          (:,:)      

  INTEGER, PARAMETER :: ityp   = 13
  INTEGER, PARAMETER :: imon   = 12
  INTEGER, PARAMETER :: icg    =  2
  INTEGER, PARAMETER :: iwv    =  3
  INTEGER, PARAMETER :: ild    =  2
  INTEGER, PARAMETER :: idp    =  3
  INTEGER, PARAMETER :: ibd    =  2
  INTEGER, PARAMETER :: nfsibd = 88   !  sib vegetation parameter
  INTEGER, PARAMETER :: nalbyx = 89   !  sib albedo parameter Xue
  CHARACTER(LEN=200) :: path_in
  CHARACTER(LEN=200) :: fNameSibVeg
  CHARACTER(LEN=200) :: fNameSibAlb
CONTAINS


  SUBROUTINE InitSurface(delsig,path,fNameSibVeg_in,fNameSibAlb_in)
    REAL            , INTENT(IN   ) ::  delsig(:)
    CHARACTER(LEN=*), INTENT(IN   ) ::  path
    CHARACTER(LEN=*), INTENT(IN   ) ::  fNameSibVeg_in
    CHARACTER(LEN=*), INTENT(IN   ) ::  fNameSibAlb_in
    expcut=- LOG(1.0e53)    
    rbyg  =gasr/grav*delsig(1)*0.5
    path_in=path
    fNameSibVeg = fNameSibVeg_in
    fNameSibAlb = fNameSibAlb_in
  END SUBROUTINE InitSurface


  SUBROUTINE CopySurfaceData(xcover, zlt, green, ph2, ph1, defac, tu, tll, topt, rstpar, chil)
    REAL, INTENT(OUT) :: xcover(ityp, imon, icg)
    REAL, INTENT(OUT) :: zlt   (ityp, imon, icg)
    REAL, INTENT(OUT) :: green (ityp,imon,icg)
    REAL, INTENT(OUT) :: ph2(ityp,icg)
    REAL, INTENT(OUT) :: ph1(ityp,icg)
    REAL, INTENT(OUT) :: defac(ityp,icg)
    REAL, INTENT(OUT) :: tu(ityp,icg)
    REAL, INTENT(OUT) :: tll(ityp,icg)
    REAL, INTENT(OUT) :: topt(ityp,icg)
    REAL, INTENT(OUT) :: rstpar(ityp,icg,iwv)
    REAL, INTENT(OUT) :: chil(ityp,icg)
    xcover = xcover_fixed
    zlt    = zlt_fixed
    green  = green_fixed
    ph2    = ph2_fixed
    ph1    = ph1_fixed
    defac  = defac_fixed
    tu     = tu_fixed
    tll    = tll_fixed
    topt   = topt_fixed
    rstpar = rstpar_fixed
    chil   = chil_fixed
  END SUBROUTINE CopySurfaceData



  ! airmod :alteration of aerodynamic transfer properties in case of snow
  !         accumulation.



  SUBROUTINE airmod (tg, capac, z0x, d, rdc, rbc, itype, &
       mon, nmax, ncols)
    !
    !
    !-----------------------------------------------------------------------
    !       input parameters
    !-----------------------------------------------------------------------
    !   tg............ground temperature
    !   tf............freezing point
    !   z2............height of canopy top
    !   capac(cg).....liquid water stored on canopy/ground cover foliage
    !                                                            (m)
    !   d.............displacement height                        (m)
    !   z0x...........roughness length                           (m)
    !   rdc...........constant related to aerodynamic resistance
    !                 between ground and canopy air space
    !   rbc...........constant related to bulk boundary layer
    !                 resistance
    !-----------------------------------------------------------------------
    !      output parameters
    !-----------------------------------------------------------------------
    !   d.............displacement height                        (m)
    !   z0x...........roughness length                           (m)
    !   rdc...........constant related to aerodynamic resistance
    !                 between ground and canopy air space
    !   rbc...........constant related to bulk boundary layer
    !                 resistance
    !-----------------------------------------------------------------------
    !=======================================================================
    !   ncols.........Numero de ponto por faixa de latitude
    !   ityp..........Numero do tipo de solo      13 
    !   imon..........Numero maximo de meses no ano (12)
    !   mon...........Numero do mes do ano (1-12) 
    !   nmax
    !   xd............Deslocamento do plano zero (m)   
    !   itype.........Classe de textura do solo
    !=======================================================================
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: mon(ncols)         
    INTEGER, INTENT(in   ) :: nmax               
    !
    !     vegetation and soil parameters
    ! 
    INTEGER, INTENT(in   ) :: itype (ncols)       
    REAL,    INTENT(inout) :: z0x   (ncols)       
    REAL,    INTENT(inout) :: d     (ncols)       
    REAL,    INTENT(inout) :: rdc   (ncols)       
    REAL,    INTENT(inout) :: rbc   (ncols)       
    !
    !     prognostic variables
    !     
    REAL,    INTENT(in   ) :: tg   (ncols)        
    REAL,    INTENT(in   ) :: capac(ncols,2)      
    !
    REAL    :: sdep(ncols)                        
    REAL    :: xz  (ncols)                        
    !
    INTEGER :: i 
    INTEGER :: ntyp

    DO i = 1, nmax
       IF( (tg(i) <= tf) .AND. (capac(i,2) >= 0.001) )THEN
          ntyp=itype(i)
          xz  (i)=z2(ntyp,mon(i))
          sdep(i)=capac(i,2)*5.0
          sdep(i)=MIN( sdep(i) , xz(i)*0.95 )
          d  (i)=xz (i)-( xz(i)- d(i) )/xz(i)*(xz(i)-sdep(i))
          z0x(i)=z0x(i)/( xz(i)-xd(ntyp,mon(i)))*(xz(i)-d   (i))
          rdc(i)=rdc(i)*( xz(i)-sdep(i) )/xz(i)
          rbc(i)=rbc(i)*xz(i)/( xz(i)-sdep(i) )
       END IF
    END DO
  END SUBROUTINE airmod





  SUBROUTINE temres(&
       bps   ,psb   ,em    ,gmt   ,gmq   ,psy   ,rcp   ,wc    ,wg    , &
       fc    ,fg    ,hr    ,hgdtg ,hgdtc ,hgdtm ,hcdtg ,hcdtc ,hcdtm , &
       egdtg ,egdtc ,egdqm ,ecdtg ,ecdtc ,ecdqm ,deadtg,deadtc,deadqm, &
       ect   ,eci   ,egt   ,egi   ,egs   ,ec    ,eg    ,hc    ,hg    , &
       ecidif,egidif,ra    ,rb    ,rd    ,rc    ,rg    ,ta    ,ea    , &
       etc   ,etg   ,btc   ,btg   ,radt  ,rst   ,rsoil ,hrr   ,cc    , &
       cg    ,satcap,dtc   ,dtg   ,dtm   ,dqm   ,thermk,tc    ,tg    , &
       td    ,capac ,qm    ,tm    ,psur  ,itype ,dtc3x ,mon   , &
       nmax  ,vcover,ncols , xcover)
    !
    !-----------------------------------------------------------------------
    ! temres :performs temperature tendency equations with interception loss.
    !-----------------------------------------------------------------------
    !     ncols.......Numero de ponto por faixa de latitude
    !     ityp........numero das classes de solo 13 
    !     imon........Numero maximo de meses no ano (12)
    !     icg.........Parametros da vegetacao (icg=1 topo e icg=2 base)
    !     pie.........Constante Pi=3.1415926e0
    !     stefan .....Constante de Stefan Boltzmann
    !     cp.......specific heat of air (j/kg/k)
    !     hl........heat of evaporation of water   (j/kg)
    !     grav........gravity constant      (m/s**2)
    !     tf..........Temperatura de congelamento (K)
    !     epsfac......Constante 0.622 Razao entre as massas moleculares do vapor
    !                 de agua e do ar seco
    !     dtc3x.......time increment dt 
    !     mon.........Numero do mes do ano (1-12)   
    !     nmax........
    !     xcover......Fracao de cobertura vegetal icg=1 topo
    !     xcover......Fracao de cobertura vegetal icg=2 base
    !     itype ......Classe de textura do solo
    !     vcover......Fracao de cobertura vegetal icg=1 topo
    !     vcover......Fracao de cobertura vegetal icg=2 topo
    !     qm..........specific humidity of reference (fourier)  
    !     tm..........Temperature of reference (fourier) 
    !     psur........surface pressure in mb
    !     tc..........Temperatura da copa "dossel" canopy leaf temperature(K)
    !     tg..........Temperatura da superficie do solo ground temperature (K)
    !     td .........Temperatura do solo profundo (K)
    !     capac(iv)...Agua interceptada iv=1 no dossel "water store capacity of leaves"(m)
    !     capac(iv)...Agua interceptada iv=2 na cobertura do solo (m) 
    !     ra..........Resistencia Aerodinamica (s/m)
    !     rb..........bulk boundary layer resistance             (s/m)
    !     rd..........aerodynamic resistance between ground
    !                 and canopy air space                       (s/m)
    !     rc..........Resistencia do topo da copa (s/m)
    !     rg..........Resistencia da base da copa (s/m)
    !     ta..........Temperatura no nivel de fonte de calor do dossel (K) 
    !     ea..........Pressao de vapor
    !     etc.........Pressure of vapor at top of the copa
    !     etg.........Pressao de vapor no base da copa
    !     btc.........btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i)).
    !     btg.........btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i))
    !     radt........net heat received by canopy/ground vegetation  
    !     rst.........Resisttencia Estomatica "Stomatal resistence" (s/m)
    !     rsoil ......Resistencia do solo (s/m)
    !     hrr.........rel. humidity in top layer
    !     cc..........heat capacity of the canopy
    !     cg..........heat capacity of the ground
    !     satcap......saturation liquid water capacity         (m)
    !     dtc.........dtc(i)=pblsib(i,2,5)*dtc3x  
    !     dtg.........dtg(i)=pblsib(i,1,5)*dtc3x   
    !     dtm.........dtm(i)=pblsib(i,3,5)*dtc3x 
    !     dqm.........dqm(i)=pblsib(i,4,5)*dtc3x 
    !     thermk......canopy emissivity
    !     ect.........Transpiracao(J/m*m)
    !     eci.........Evaporacao da agua interceptada (J/m*m)
    !     egt.........Transpiracao na base da copa (J/m*m)   
    !     egi.........Evaporacao da neve (J/m*m)
    !     egs.........Evaporacao do solo arido (J/m*m)
    !     ec..........Soma da Transpiracao e Evaporacao da agua interceptada pelo
    !                 topo da copa   ec   (i)=eci(i)+ect(i) 
    !     eg..........Soma da transpiracao na base da copa +  Evaporacao do solo arido
    !                 +  Evaporacao da neve  " eg   (i)=egt(i)+egs(i)+egi(i)"
    !     hc..........total sensible heat lost of top from the veggies. 
    !     hg..........total sensible heat lost of base from the veggies.
    !     ecidif......check if interception loss term has exceeded canopy storage
    !                 ecidif(i)=MAX(0.0   , eci(i)-capac(i,1)*hl3 )
    !     egidif......check if interception loss term has exceeded canopy storage
    !                 ecidif(i)=MAX(0.0   , egi(i)-capac(i,1)*hl3 )
    !     hgdtg ......n.b. fluxes expressed in joules m-2
    !     hgdtc.......n.b. fluxes expressed in joules m-2
    !     hgdtm.......n.b. fluxes expressed in joules m-2
    !     hcdtg.......n.b. fluxes expressed in joules m-2
    !     hcdtc.......n.b. fluxes expressed in joules m-2
    !     hcdtm.......n.b. fluxes expressed in joules m-2
    !     egdtg.......partial derivative calculation for latent heat
    !     egdtc.......partial derivative calculation for latent heat
    !     egdqm.......partial derivative calculation for latent heat
    !     ecdtg ......partial derivative calculation for latent heat
    !     ecdtc ......partial derivative calculation for latent heat
    !     ecdqm.......partial derivative calculation for latent heat
    !     deadtg......
    !     deadtc......
    !     deadqm......
    !     bps.........
    !     psb.........
    !     em..........Pressao de vapor da agua
    !     gmt.........
    !     gmq.........specific humidity of reference (fourier)  
    !     psy.........(cp/(hl*epsfac))*psur(i)
    !     rcp.........densidade do ar vezes o calor especifico do ar
    !     wc..........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !                 indice de area foliar no topo da copa 
    !     wg..........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !                 indice de area foliar na base da copa 
    !     fc..........Condicao de oravalho 0 ou 1 na topo da copa
    !     fg..........Condicao de oravalho 0 ou 1 na base da copa
    !     hr..........rel. humidity in top layer
    !-----------------------------------------------------------------------

    INTEGER, INTENT(in   ) :: ncols

    REAL,    INTENT(in   ) :: dtc3x               
    INTEGER, INTENT(in   ) :: mon(ncols)          
    INTEGER, INTENT(in   ) :: nmax                
    !
    !     vegetation and soil parameters
    !
    INTEGER, INTENT(in   ) :: itype (ncols)        
    REAL,    INTENT(inout) :: vcover(ncols,icg)    
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL, INTENT(in   ) :: qm  (ncols)             
    REAL, INTENT(in   ) :: tm  (ncols)             
    REAL, INTENT(in   ) :: psur(ncols)             
    !
    !     prognostic variables
    !
    REAL, INTENT(in   ) :: tc   (ncols)            
    REAL, INTENT(in   ) :: tg   (ncols)            
    REAL, INTENT(in   ) :: td   (ncols)            
    REAL, INTENT(in   ) :: capac(ncols,2)          
    !
    !     variables calculated from above and ambient conditions
    !
    REAL, INTENT(in   ) :: ra    (ncols)           
    REAL, INTENT(in   ) :: rb    (ncols)           
    REAL, INTENT(in   ) :: rd    (ncols)           
    REAL, INTENT(out  ) :: rc    (ncols)           
    REAL, INTENT(out  ) :: rg    (ncols)           
    REAL, INTENT(out  ) :: ta    (ncols)           
    REAL, INTENT(out  ) :: ea    (ncols)           
    REAL, INTENT(in   ) :: etc   (ncols)           
    REAL, INTENT(in   ) :: etg   (ncols)           
    REAL, INTENT(in   ) :: btc   (ncols)           
    REAL, INTENT(in   ) :: btg   (ncols)           
    REAL, INTENT(inout) :: radt  (ncols,icg)       
    REAL, INTENT(inout) :: rst   (ncols,icg)       
    REAL, INTENT(in   ) :: rsoil (ncols)           
    REAL, INTENT(in   ) :: hrr   (ncols)           
    REAL, INTENT(in   ) :: cc    (ncols)           
    REAL, INTENT(in   ) :: cg    (ncols)           
    REAL, INTENT(in   ) :: satcap(ncols,icg)       
    REAL, INTENT(out  ) :: dtc   (ncols)           
    REAL, INTENT(out  ) :: dtg   (ncols)           
    REAL, INTENT(out  ) :: dtm   (ncols)           
    REAL, INTENT(out  ) :: dqm   (ncols)           
    REAL, INTENT(in   ) :: thermk(ncols)           
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL, INTENT(out  ) :: ect   (ncols)           
    REAL, INTENT(out  ) :: eci   (ncols)           
    REAL, INTENT(out  ) :: egt   (ncols)           
    REAL, INTENT(out  ) :: egi   (ncols)           
    REAL, INTENT(out  ) :: egs   (ncols)           
    REAL, INTENT(out  ) :: ec    (ncols)           
    REAL, INTENT(out  ) :: eg    (ncols)           
    REAL, INTENT(out  ) :: hc    (ncols)      
    REAL, INTENT(out  ) :: hg    (ncols)      
    REAL, INTENT(out  ) :: ecidif(ncols)      
    REAL, INTENT(out  ) :: egidif(ncols)      
    !
    !     derivatives
    !
    REAL, INTENT(out  ) :: hgdtg (ncols)           
    REAL, INTENT(out  ) :: hgdtc (ncols)           
    REAL, INTENT(out  ) :: hgdtm (ncols)           
    REAL, INTENT(out  ) :: hcdtg (ncols)           
    REAL, INTENT(out  ) :: hcdtc (ncols)           
    REAL, INTENT(out  ) :: hcdtm (ncols)           
    REAL, INTENT(out  ) :: egdtg (ncols)           
    REAL, INTENT(out  ) :: egdtc (ncols)           
    REAL, INTENT(out  ) :: egdqm (ncols)           
    REAL, INTENT(out  ) :: ecdtg (ncols)           
    REAL, INTENT(out  ) :: ecdtc (ncols)           
    REAL, INTENT(out  ) :: ecdqm (ncols)           
    REAL, INTENT(out  ) :: deadtg(ncols)           
    REAL, INTENT(out  ) :: deadtc(ncols)           
    REAL, INTENT(out  ) :: deadqm(ncols)           
    !
    !     this is for coupling with closure turbulence model
    !
    REAL, INTENT(in   ) :: bps   (ncols)           
    REAL, INTENT(in   ) :: psb   (ncols)           
    REAL, INTENT(in   ) :: em    (ncols)           
    REAL, INTENT(in   ) :: gmt   (ncols,3)         
    REAL, INTENT(in   ) :: gmq   (ncols,3)         
    REAL, INTENT(in   ) :: psy   (ncols)           
    REAL, INTENT(in   ) :: rcp   (ncols)           
    REAL, INTENT(out  ) :: wc    (ncols)             
    REAL, INTENT(out  ) :: wg    (ncols)             
    REAL, INTENT(in   ) :: fc    (ncols)           
    REAL, INTENT(in   ) :: fg    (ncols)           
    REAL, INTENT(out  ) :: hr    (ncols)           


    REAL, INTENT(IN   ) :: xcover(ityp, imon, icg)
    REAL    :: pblsib(ncols,4,5)       
    REAL    :: ai
    REAL    :: coc
    REAL    :: rsurf
    REAL    :: cog1
    REAL    :: cog2
    REAL    :: d1
    REAL    :: d2
    REAL    :: d1i
    REAL    :: top
    REAL    :: ak    (ncols)           
    REAL    :: ah    (ncols)           
    REAL    :: cci   (ncols)           
    REAL    :: cgi   (ncols)           
    REAL    :: ecpot (ncols)           
    REAL    :: egpot (ncols)           
    REAL    :: ecf
    REAL    :: egf
    REAL    :: coct
    REAL    :: cogt
    REAL    :: cogs1
    REAL    :: cogs2
    REAL    :: psyi  (ncols)           
    REAL    :: fac1
    REAL    :: rcdtc (ncols)           
    REAL    :: rcdtg (ncols)           
    REAL    :: rgdtc (ncols)           
    REAL    :: rgdtg (ncols)           
    REAL, PARAMETER :: capi  =1.0   /4.0e-3
    REAL    :: timcon 
    REAL    :: timcn2 
    REAL    :: tim 
    REAL    :: dtc3xi 
    REAL    :: fak 
    REAL    :: fah
    INTEGER :: i
    REAL    :: stb4 
    REAL    :: stb8 
    REAL    :: hlat3

    timcon = pie/86400.0
    timcn2 = 2.0   *timcon
    tim    = 1.0   +timcn2*dtc3x
    dtc3xi = 1.0   /dtc3x
    fak    = 0.01   *grav/cp
    fah    = 0.01   *grav/hl

    DO i = 1, nmax
       wc  (i)=MIN( 1.0   , capac(i,1)/satcap(i,1))
       wg  (i)=MIN( 1.0   , capac(i,2)/satcap(i,2))
       IF (tg(i) <= tf) THEN
          vcover(i,2)=1.0
          wg    (i)  =MIN(1.0   ,capac(i,2)*capi)
          rst   (i,2)=rsoil(i)
       END IF
       ak  (i) =fak/(psb(i)*bps(i))
       ah  (i) =fah/ psb(i)
       cgi (i) =1.0   /cg(i)
       cci (i) =1.0   /cc(i)
       psyi(i) =rcp(i)/psy(i)
    END DO
    !     
    !     partial derivative calculations for sensible heat
    !     
    DO i = 1, nmax
       d1     =1.0   /ra(i)+1.0   /rb(i)+1.0   /rd(i)
       d1i    =rcp(i)/d1
       ta(i)=(tg(i)/rd(i)+tc(i)/rb(i)+tm(i)*bps(i)/ra(i))/d1
       hc(i)=rcp(i)*(tc(i)-ta(i))/rb(i)*dtc3x
       hg(i)=rcp(i)*(tg(i)-ta(i))/rd(i)*dtc3x
       !     
       !     n.b. fluxes expressed in joules m-2
       !     
       hcdtc(i)= d1i   /rb(i)*( 1.0   /ra(i)+1.0   /rd(i) )
       hcdtg(i)=-d1i   /( rb(i)*rd(i) )
       hcdtm(i)=-d1i   /( rb(i)*ra(i) ) *bps(i)
       hgdtg(i)= d1i   /rd(i)*(1.0   /ra(i)+1.0   /rb(i))
       hgdtc(i)=-d1i   /( rd(i)*rb(i) )
       hgdtm(i)=-d1i   /( rd(i)*ra(i) ) *bps(i)
    END DO
    !     
    !     partial derivative calculations for longwave radiation flux
    !     
    stb4=4.0   *stefan
    stb8=8.0   *stefan
    DO i = 1, nmax
       fac1     =vcover(i,1)*(1.0   -thermk(i))
       rcdtc(i) = fac1   *stb8*tc(i)*tc(i)*tc(i)
       rcdtg(i) =-fac1   *stb4*tg(i)*tg(i)*tg(i)
       rgdtc(i) =-fac1   *stb4*tc(i)*tc(i)*tc(i)
       rgdtg(i) =         stb4*tg(i)*tg(i)*tg(i)
    END DO
    DO i = 1, nmax
       !     
       !     partial derivative calculation for latent heat
       !     modification for soil dryness : hr=rel. humidity in top layer
       !     
       hr(i)=hrr(i)*fg(i)+1.0   -fg(i)
       rc   (i)=rst(i,1)*fc(i)+2.0*rb(i)
       coc     =(1.0   -wc(i))/rc(i)+wc(i)/(2.0   *rb(i))
       rg   (i)=rst(i,2)*fg(i)
       rsurf   =rsoil(i)*fg(i)
       cog1    =vcover(i,2)*(1.0   -wg(i))/(rg(i)+rd(i)) &
            +(1.0   -vcover(i,2))/(rsurf   +rd(i))*hr(i) &
            +vcover(i,2)/(rsurf   +rd(i)+44.0)*hr(i)
       cog2    =vcover(i,2)*(1.0   -wg(i))/(rg(i)+rd(i)) &
            +(1.0   -vcover(i,2))/(rsurf   +rd(i)) &
            +vcover(i,2)/(rsurf   +rd(i)+44.)
       cog1    =cog1   +wg(i)/rd(i)*vcover(i,2)
       cog2    =cog2   +wg(i)/rd(i)*vcover(i,2)
       d2    =1.0   /ra(i)+coc   +cog2   
       top   =coc   *etc(i)+cog1   *etg(i)+em(i)/ra(i)
       ea (i)=top   /d2
       ec (i)=(etc(i)-ea(i))*coc   *psyi(i)*dtc3x
       eg (i)=(etg(i)*cog1   -ea(i)*cog2   )*psyi(i)*dtc3x
       deadtc(i)=btc(i)*coc    /d2
       deadtg(i)=btg(i)*cog1   /d2
       deadqm(i)=epsfac*psur(i)/( (epsfac+qm(i))**2 * ra(i)*d2    )
       ecdtc(i)=( btc(i)-deadtc(i) )*coc   *psyi(i)
       ecdtg(i)=- deadtg(i)*coc   *psyi(i)
       ecdqm(i)=- deadqm(i)*coc   *psyi(i)
       egdtg(i)=(btg(i)*cog1   -deadtg(i)*cog2   )*psyi(i)
       egdtc(i)=-deadtc(i)*cog2   *psyi(i)
       egdqm(i)=-deadqm(i)*cog2   *psyi(i)
    END DO
    !
    !     solve for time changes of pbl and sib variables,
    !     using a semi-implicit scheme.
    !     
    DO i = 1, nmax
       !     
       !     tg equation
       !     
       pblsib(i,1,1)=tim+dtc3x*cgi(i)*(hgdtg(i)+egdtg(i)+rgdtg(i))
       pblsib(i,1,2)=dtc3x*cgi(i)*(hgdtc(i)+egdtc(i)+rgdtc(i))
       pblsib(i,1,3)=dtc3x*cgi(i)* hgdtm(i)
       pblsib(i,1,4)=dtc3x*cgi(i)* egdqm(i)
       !     
       !     tc equation
       !     
       pblsib(i,2,1)=dtc3x*cci(i)*(hcdtg(i)+ecdtg(i)+rcdtg(i))
       pblsib(i,2,2)=1.0+dtc3x*cci(i)*(hcdtc(i)+ecdtc(i)+rcdtc(i))
       pblsib(i,2,3)=dtc3x*cci(i)* hcdtm(i)
       pblsib(i,2,4)=dtc3x*cci(i)*ecdqm(i)
       !
       !     tm equation
       !     
       pblsib(i,3,1)=-dtc3x*ak(i)*(hgdtg(i)+hcdtg(i))
       pblsib(i,3,2)=-dtc3x*ak(i)*(hgdtc(i)+hcdtc(i))
       pblsib(i,3,3)=gmt(i,2)-dtc3x*ak(i)*(hgdtm(i)+hcdtm(i))
       pblsib(i,3,4)=0.0
       !     
       !     qm equation
       !     
       pblsib(i,4,1)=-dtc3x*ah(i)*(egdtg(i)+ecdtg(i))
       pblsib(i,4,2)=-dtc3x*ah(i)*(egdtc(i)+ecdtc(i))
       pblsib(i,4,3)=0.0
       pblsib(i,4,4)=gmq(i,2)-dtc3x*ah(i)*(egdqm(i)+ecdqm(i))
       pblsib(i,1,5)=(radt(i,2) -(hg(i)+eg(i))*dtc3xi)*cgi(i) &
            -timcn2*(tg(i)-td(i))
       pblsib(i,2,5)=(radt(i,1) -(hc(i)+ec(i))*dtc3xi)*cci(i)
       pblsib(i,3,5)=gmt(i,3)+ak(i)*(hg(i)+hc(i))*dtc3xi
       pblsib(i,4,5)=gmq(i,3)+ah(i)*(eg(i)+ec(i))*dtc3xi
    END DO
    !     
    !     solve 4 x 5 matrix equation
    !     
    DO i = 1, nmax
       ai         =1.0   /pblsib(i,1,1)
       pblsib(i,1,2) =pblsib(i,1,2)*ai
       pblsib(i,1,3) =pblsib(i,1,3)*ai
       pblsib(i,1,4) =pblsib(i,1,4)*ai
       pblsib(i,1,5) =pblsib(i,1,5)*ai
       pblsib(i,1,1) =1.0
       pblsib(i,2,2) =pblsib(i,2,2)-pblsib(i,2,1)*pblsib(i,1,2)
       pblsib(i,2,3) =pblsib(i,2,3)-pblsib(i,2,1)*pblsib(i,1,3)
       pblsib(i,2,4) =pblsib(i,2,4)-pblsib(i,2,1)*pblsib(i,1,4)
       pblsib(i,2,5) =pblsib(i,2,5)-pblsib(i,2,1)*pblsib(i,1,5)
       pblsib(i,3,2) =pblsib(i,3,2)-pblsib(i,3,1)*pblsib(i,1,2)
       pblsib(i,3,3) =pblsib(i,3,3)-pblsib(i,3,1)*pblsib(i,1,3)
       pblsib(i,3,4) =pblsib(i,3,4)-pblsib(i,3,1)*pblsib(i,1,4)
       pblsib(i,3,5) =pblsib(i,3,5)-pblsib(i,3,1)*pblsib(i,1,5)
       pblsib(i,4,2) =pblsib(i,4,2)-pblsib(i,4,1)*pblsib(i,1,2)
       pblsib(i,4,3) = pblsib(i,4,3)-pblsib(i,4,1)*pblsib(i,1,3)
       pblsib(i,4,4) = pblsib(i,4,4)-pblsib(i,4,1)*pblsib(i,1,4)
       pblsib(i,4,5) = pblsib(i,4,5)-pblsib(i,4,1)*pblsib(i,1,5)
       ai         = 1.0   /pblsib(i,2,2)
       pblsib(i,2,3) = pblsib(i,2,3)*ai
       pblsib(i,2,4) = pblsib(i,2,4)*ai
       pblsib(i,2,5) = pblsib(i,2,5)*ai
       pblsib(i,2,2) = 1.0
       pblsib(i,3,3) = pblsib(i,3,3)-pblsib(i,3,2)*pblsib(i,2,3)
       pblsib(i,3,4) = pblsib(i,3,4)-pblsib(i,3,2)*pblsib(i,2,4)
       pblsib(i,3,5) = pblsib(i,3,5)-pblsib(i,3,2)*pblsib(i,2,5)
       pblsib(i,4,3) = pblsib(i,4,3)-pblsib(i,4,2)*pblsib(i,2,3)
       pblsib(i,4,4) = pblsib(i,4,4)-pblsib(i,4,2)*pblsib(i,2,4)
       pblsib(i,4,5) = pblsib(i,4,5)-pblsib(i,4,2)*pblsib(i,2,5)
       ai         = 1.0   / pblsib(i,3,3)
       pblsib(i,3,4) = pblsib(i,3,4)*ai
       pblsib(i,3,5) = pblsib(i,3,5)*ai
       pblsib(i,3,3) = 1.0
       pblsib(i,4,4) = pblsib(i,4,4)-pblsib(i,4,3)*pblsib(i,3,4)
       pblsib(i,4,5) = pblsib(i,4,5)-pblsib(i,4,3)*pblsib(i,3,5)
       pblsib(i,4,5) = pblsib(i,4,5)/pblsib(i,4,4)
       pblsib(i,3,5) = pblsib(i,3,5)-pblsib(i,3,4)*pblsib(i,4,5)
       pblsib(i,2,5) = pblsib(i,2,5)-pblsib(i,2,4)*pblsib(i,4,5) &
            -pblsib(i,2,3)*pblsib(i,3,5)
       pblsib(i,1,5) = pblsib(i,1,5)-pblsib(i,1,4)*pblsib(i,4,5) &
            -pblsib(i,1,3)*pblsib(i,3,5) &
            -pblsib(i,1,2)*pblsib(i,2,5)
    END DO
    DO i = 1, nmax
       dtg(i)=pblsib(i,1,5)*dtc3x
       dtc(i)=pblsib(i,2,5)*dtc3x
       dtm(i)=pblsib(i,3,5)*dtc3x
       dqm(i)=pblsib(i,4,5)*dtc3x
       hc (i)=hc(i) &
            +dtc3x*(hcdtc(i)*dtc(i)+hcdtg(i)*dtg(i)+hcdtm(i)*dtm(i))
       hg (i)=hg(i) &
            +dtc3x*(hgdtc(i)*dtc(i)+hgdtg(i)*dtg(i)+hgdtm(i)*dtm(i))
       !     
       !     check if interception loss term has exceeded canopy storage
       !     
       ecpot(i)=(etc(i)-ea(i))+(btc(i)-deadtc(i))*dtc(i) &
            -deadtg(i)*dtg(i)-deadqm(i) *dqm(i)
       egpot(i)=(etg(i)-ea(i))+(btg(i)-deadtg(i))*dtg(i) &
            -deadtc(i)*dtc(i)-deadqm(i) *dqm(i)
    END DO
    !---------------------------------------------------------------------- 
    !     EVAPORATION LOSSES ARE EXPRESSED IN J M-2 : WHEN DIVIDED BY       
    !     ( hl*1000.) LOSS IS IN M M-2 (hl(J/kg))(1 J/kg ==> 1000J/m-3)                                   
    !     MASS TERMS ARE IN KG M-2 DT-1                                     
    !----------------------------------------------------------------------   
    hlat3=1.0e+03*hl
    DO i = 1, nmax
       eci   (i)=ecpot(i)*wc(i)*psyi(i)/(2.0   *rb(i))*dtc3x
       ecidif(i)=MAX(0.0   , eci(i)-capac(i,1)*hlat3 )
       hc    (i)=hc(i)+ecidif(i)
       eci   (i)=MIN(eci(i) ,        capac(i,1)*hlat3 )
       egi   (i)=egpot(i)*vcover(i,2)*wg(i)*psyi(i)/rd(i)*dtc3x
       egidif(i)=MAX(0.0   , egi(i)-capac(i,2)*hlat3 )
       hg    (i)=hg(i)+egidif(i)
       egi   (i)=MIN(egi(i) ,        capac(i,2)*hlat3 )
       !     
       !     evaporation is given in j m-2, calculated from gradients
       !     
       rsurf    =rsoil(i)*fg(i)
       coct     =(1.0   -wc(i))/rc(i)
       cogt     =vcover(i,2)*(1.0   -wg(i))/(rg(i)+rd   (i))
       cogs1    =(1.0   -vcover(i,2))*hr(i)/(rd(i)+rsurf   ) &
            +vcover(i,2)/(rd(i)+rsurf   +44.)*hr(i)
       cogs2    =cogs1   /hr(i)
       ect  (i) =ecpot(i)*coct   *psyi(i)*dtc3x
       ec   (i) =eci(i)+ect(i)
       egt  (i) =egpot(i)*cogt   *psyi(i)*dtc3x
       egs  (i) =(etg(i)+btg(i)*dtg(i))*cogs1    &
            -(ea(i)+deadtg(i)*dtg(i)+deadtc(i)*dtc(i)+deadqm(i)*dqm(i) &
            )   *cogs2
       egs  (i) =egs(i)*psyi(i)*dtc3x
       eg   (i) =egt(i)+egs(i)+egi(i)
       vcover(i,2)=xcover(itype(i),mon(i),2)
    END DO
    !
    !     test of dew condition. recalculation ensues if necessary.
    !
    DO i = 1, nmax
       radt(i,1) = radt(i,1)-rcdtc(i)*dtc(i)-rcdtg(i)*dtg(i)
       radt(i,2) = radt(i,2)-rgdtc(i)*dtc(i)-rgdtg(i)*dtg(i)
       ecf    = SIGN(1.0   ,ecpot(i))*(fc(i)*2.0   -1.0   )
       egf    = SIGN(1.0   ,egpot(i))*(fg(i)*2.0   -1.0   )
       IF (ecf    <= 0.0) THEN
          hc (i) = hc(i)+eci(i)+ect(i)
          eci(i) = 0.0
          ect(i) = 0.0
          ec (i) = 0.0
       END IF
       IF (egf    <= 0.0) THEN
          hg (i) = hg(i)+egi(i)+egt(i)+egs(i)
          egi(i) = 0.0
          egt(i) = 0.0
          egs(i) = 0.0
          eg (i) = 0.0
       END IF
    END DO
  END SUBROUTINE temres



  ! cut    :performs vapor pressure calculation at level "a".



  SUBROUTINE cut( &
       icheck,em    ,rhoair,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    , &
       ra    ,rb    ,rd    ,rc    ,rg    ,ea    ,etc   ,etg   ,rst   , &
       rsoil ,vcover,nmax  ,ncols )
    !     
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    ! input parameters
    !   fc      fg      hr      wc      wg      rhoair  cp
    !   rst     ra      rb      rg      rd      rsurf   vcover
    !   etc     etg     em
    !-----------------------------------------------------------------------
    ! output parameters
    !   ea
    !-----------------------------------------------------------------------
    ! ncols......Numero de ponto por faixa de latitude
    ! icg........Parametros da vegetacao (icg=1 topo e icg=2 base)
    ! cp.........specific heat of air (j/kg/k)
    ! nmax.......
    ! vcover(iv).Fracao de cobertura da vegetacao iv=1 topo ()
    ! vcover(iv).Fracao de cobertura da vegetacao iv=2 bottom ()
    ! ra.........Resistencia Aerodinamica (s/m)
    ! rb.........bulk boundary layer resistance             (s/m)
    ! rd.........aerodynamic resistance between ground     
    !            and canopy air space                       (s/m)
    ! rc.........Resistencia do topo da copa 
    ! rg.........Resistencia da base da copa 
    ! ea.........Pressao de vapor 
    ! etc........Pressao de vapor no topo da copa
    ! etg........Pressao de vapor no base da copa
    ! rst........Resistencia stomatal (s/m)
    ! rsoil......Resistencia do solo (s/m)
    ! em.........Pressao de vapor da agua
    ! rhoair.....Desnsidade do ar
    ! rcp........densidade do ar vezes o calor especifico do ar
    ! wc.........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !            indice de area foliar no topo da copa 
    ! wg.........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !             indice de area foliar na parte inferior da copa 
    ! fc.........Condicao de oravalho 0 ou 1 no topo da copa                                                
    ! fg.........Condicao de oravalho 0 ou 1 na base da copa    
    ! hr.........Rel. humidity in top layer   
    ! icheck
    !----------------------------------------------------------------------- 
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: nmax           
    REAL,    INTENT(in   ) :: vcover(ncols,icg)
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(in   ) :: ra    (ncols)   
    REAL,    INTENT(in   ) :: rb    (ncols)   
    REAL,    INTENT(in   ) :: rd    (ncols)   
    REAL,    INTENT(out  ) :: rc    (ncols)   
    REAL,    INTENT(out  ) :: rg    (ncols)   
    REAL,    INTENT(out  ) :: ea    (ncols)   
    REAL,    INTENT(in   ) :: etc   (ncols)   
    REAL,    INTENT(in   ) :: etg   (ncols)   
    REAL,    INTENT(in   ) :: rst   (ncols,icg)
    REAL,    INTENT(in   ) :: rsoil (ncols)   
    !
    !     this is for coupling with closure turbulence model
    !  
    REAL,    INTENT(in   ) :: em    (ncols)   
    REAL,    INTENT(in   ) :: rhoair(ncols)   
    REAL,    INTENT(out  ) :: rcp   (ncols)   
    REAL,    INTENT(in   ) :: wc    (ncols)   
    REAL,    INTENT(in   ) :: wg    (ncols)   
    REAL,    INTENT(in   ) :: fc    (ncols)   
    REAL,    INTENT(in   ) :: fg    (ncols)   
    REAL,    INTENT(in   ) :: hr    (ncols)   
    INTEGER, INTENT(in   ) :: icheck(ncols)   

    REAL :: coc
    REAL :: rsurf
    REAL :: cog1
    REAL :: cog2
    REAL :: d2
    REAL :: top
    REAL :: xnum
    REAL :: tem
    INTEGER :: i

    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          rcp  (i) = rhoair(i)*cp
          rc   (i) = rst(i,1)*fc(i)+rb(i)+rb(i)*fc(i)
          coc      = (1.0 -wc(i))/rc(i)+wc(i)/(2.0 *rb(i))
          rg   (i) = rst(i,2)*fg(i)
          rsurf    = rsoil(i)*fg(i)
          tem      = vcover(i,2)*(1.0-wg(i))/(rg(i)+rd(i))
          cog2     = tem    &
               + (1.0 -vcover(i,2))/(rsurf   +rd(i)) &
               + vcover(i,2)/(rsurf   +rd(i)+44.)
          cog1     = (cog2   -tem   )*hr(i)+tem   
          xnum     = wg(i)/rd(i)*vcover(i,2)
          cog1     = cog1   +xnum   
          cog2     = cog2   +xnum   
          d2       = 1.0 /ra(i)+coc+cog2   
          top      = coc*etc(i)+em(i)/ra(i)+cog1   *etg(i)
          !     
          !     vapor pressure at level "a"
          !     
          ea (i)  = top   /d2   
       END IF
    END DO
  END SUBROUTINE cut



  ! rbrd   :calculates bulk boundary layer resistance and aerodynamic
  !         resistence betweenground and canopi air space4 as functions
  !         of wind speed at top of canopy and temperatures.



  SUBROUTINE rbrd(rb    ,rd    ,tcta  ,tgta  ,u2    ,tg    ,rdc   ,rbc   ,itype , &
       z2    ,mon   ,nmax  ,ncols , zlt2)
    !
    !
    !         rb and rd as functions of u2 and temperatures. simplified( xue et
    !         al. 1991)
    !
    !-----------------------------------------------------------------------
    !       input parameters
    !-----------------------------------------------------------------------
    !   tcta..........diferenca entre tc-ta                      (k)
    !   tgta..........diferenca entre tg-ta                      (k)
    !   tg............ground temperature                         (k)
    !   u2............wind speed at top of canopy                (m/s)
    !   z2............height of canopy top                       (m)
    !   zlt(cg).......canopy/ground cover leaf and stem area density
    !                                                            (m**2/m**3)
    !   rbc...........constant related to bulk boundary layer
    !                 resistance
    !   rdc...........constant related to aerodynamic resistance
    !                 between ground and canopy air space
    !-----------------------------------------------------------------------
    !      output parameters
    !-----------------------------------------------------------------------
    !   rb............bulk boundary layer resistance             (s/m)
    !   rd............aerodynamic resistance between ground      (s/m)
    !                 and canopy air space
    !-----------------------------------------------------------------------
    !   ncols.........Numero de ponto por faixa de latitude
    !   ityp..........numero das classes de solo 13 
    !   imon...........Numero maximo de meses no ano (12)
    !   icg...........Parametros da vegetacao (icg=1 topo e icg=2 base)
    !   mon...........Numero do mes do ano (1-12) 
    !   nmax .........
    !   itype.........Classe de textura do solo
    !======================================================================= 
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: mon (ncols)            
    INTEGER, INTENT(in   ) :: nmax                   
    !
    !     vegetation and soil parameters
    !
    REAL,    INTENT(in   ) :: z2    (ityp,imon)      
    INTEGER, INTENT(in   ) :: itype (ncols)          
    REAL,    INTENT(in   ) :: rdc   (ncols)          
    REAL,    INTENT(in   ) :: rbc   (ncols)          
    !
    !     prognostic variables
    !
    REAL,    INTENT(in   ) :: tg   (ncols)           
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(out  ) :: rb    (ncols)          
    REAL,    INTENT(out  ) :: rd    (ncols)          
    REAL,    INTENT(in   ) :: tcta  (ncols)          
    REAL,    INTENT(in   ) :: tgta  (ncols)          
    REAL,    INTENT(in   ) :: u2    (ncols)          
    REAL,    INTENT(in  ) :: zlt2(ncols,icg)

    REAL :: temdif(ncols)                            
    REAL :: fih   (ncols)                            

    REAL, PARAMETER :: factg=88.29
    INTEGER :: i
    INTEGER :: ntyp

    DO i = 1, nmax
       ntyp=itype(i)
       IF (tcta(i) > 0.0 ) THEN
          temdif(i)=tcta(i)+0.1
       ELSE
          temdif(i)=        0.1
       END IF
       rb (i)=1.0  /(SQRT(u2(i))/rbc(i)+zlt2(i,1)*0.004 )
       IF (tgta(i) > 0) THEN
          temdif(i)=tgta(i)+0.1
       ELSE
          temdif(i)=        0.1
       END IF
       fih(i)=sqrt &
            (1.0 +factg*temdif(i)*z2(ntyp,mon(i))/(tg(i)*u2(i)*u2(i)))
       rd(i) =rdc(i)/(u2(i)*fih(i))
    END DO
  END SUBROUTINE rbrd



  ! vntlax :performs ventilation mass flux, based on deardorff, mwr, 1972?.


  SUBROUTINE vntlax(ustarn, &
       icheck,bps   ,dzm   ,cu    ,cuni  ,ctni  ,ustar ,ra    ,ta    , &
       u2    ,tm    ,um    ,vm    ,d     ,z0    ,itype ,z2    , &
       mon   ,nmax  ,jstneu,ncols )
    !
    !
    !-----------------------------------------------------------------------
    !         input parameters
    !-----------------------------------------------------------------------
    !
    !   ea..........Pressao de vapor       
    !   ta..........Temperatura no nivel de fonte de calor do dossel (K)
    !   um..........Razao entre zonal pseudo-wind (fourier) e seno da
    !               colatitude    
    !   vm..........Razao entre meridional pseudo-wind (fourier) e seno da
    !               colatitude      
    !   qm..........specific humidity of reference (fourier)  
    !   tm..........Temperature of reference (fourier)  
    !   dzm  .......Altura media de referencia  para o vento para o calculo
    !               da estabilidade do escoamento   
    !   grav........gravity constant      (m/s**2) 
    !   cpair.......specific heat of air (j/kg/k)
    !   gasr........gas constant of dry air      (j/kg/k)  
    !   bps ........    
    !   z2..........height of canopy top
    !   d...........displacement height                        (m)
    !   epsfac......parametro para o gas 0.622  
    !      
    !   
    !
    !-----------------------------------------------------------------------
    !        output parameters
    !-----------------------------------------------------------------------
    !
    !   ustar.........surface friction velocity  (m/s)
    !   ra............Resistencia Aerodinamica (s/m)
    !   u2............wind speed at top of canopy                (m/s)
    !   ventmf........ventilation mass flux
    !-----------------------------------------------------------------------
    !=======================================================================
    !   ncols........Numero de ponto por faixa de latitude
    !   ityp.........Numero do tipo de solo  
    !   imon.........Numero maximo de meses no ano (12)
    !   jstneu.......The first call to vntlat just gets the neutral values 
    !                of ustar and ventmf para jstneu=.TRUE..
    !   mon..........Numero do mes do ano (1-12)   
    !   nmax.........
    !   itype........Classe de textura do solo
    !   z0...........roughness length    
    !   bps..........bps   (i)=sigki(1)=1.0e0/EXP(akappa*LOG(sig(k)))
    !   cu...........friction  transfer coefficients.
    !   ct...........heat transfer coefficients.
    !   cuni.........neutral friction transfer  coefficients.
    !   ctni.........neutral heat transfer coefficients.
    !   icheck.......this version assumes dew-free conditions "icheck=1" to 
    !                estimate ea for buoyancy term in vntmf or ra.
    !=======================================================================
    INTEGER, INTENT(in   ) :: ncols

    LOGICAL, INTENT(in   ) :: jstneu            
    INTEGER, INTENT(in   ) :: mon(ncols)        
    INTEGER, INTENT(in   ) :: nmax              
    !
    !     vegetation and soil parameters
    !
    REAL,    INTENT(in   ) :: z2    (ityp,imon) 
    INTEGER, INTENT(in   ) :: itype (ncols)      
    REAL,    INTENT(in   ) :: d     (ncols)      
    REAL,    INTENT(in   ) :: z0    (ncols)      
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL,    INTENT(in   ) :: tm  (ncols)        
    REAL,    INTENT(in   ) :: um  (ncols)        
    REAL,    INTENT(in   ) :: vm  (ncols)        
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(inout) :: ra    (ncols)      
    REAL,    INTENT(in   ) :: ta    (ncols)      
    REAL,    INTENT(inout) :: u2    (ncols)      
    !
    !     this is for coupling with closure turbulence model
    !
    REAL,    INTENT(in   ) :: bps   (ncols)      
    REAL,    INTENT(in   ) :: dzm   (ncols)      
    REAL,    INTENT(inout) :: cu    (ncols)      
    REAL,    INTENT(inout) :: cuni  (ncols)      
    REAL,    INTENT(inout) :: ctni  (ncols)      
    REAL,    INTENT(inout) :: ustar (ncols)      
    INTEGER, INTENT(in   ) :: icheck(ncols)      
    REAL,    INTENT(inout) :: ustarn(ncols)      


    !REAL :: thm(ncols)      !**(JP)** scalar
    REAL :: thm
    !REAL :: ros(ncols)      !**(JP)** unused
    REAL :: speedm(ncols)
    !REAL :: thvgm(ncols)    !**(JP)** scalar
    REAL :: thvgm
    !REAL :: rib(ncols)      !**(JP)** scalar
    REAL :: rib
    !REAL :: cui(ncols)      !**(JP)** scalar
    REAL :: cui
    !REAL :: ran(ncols)      !**(JP)** unused
    !REAL :: cti(ncols)      !**(JP)** scalar
    REAL :: cti
    !REAL :: ct      (ncols)      !**(JP)** unused

    REAL, PARAMETER ::  vkrmn=0.40  
    REAL :: rfac
    REAL :: vkrmni
    REAL :: x 
    REAL :: g2 
    REAL :: zl 
    REAL :: xct1 
    REAL :: xct2 
    REAL :: xctu1 
    REAL :: xctu2 
    REAL :: grib
    REAL :: grzl 
    REAL :: grz2 
    REAL :: fvv 
    REAL :: ftt 
    REAL :: rzl 
    REAL :: rz2 
    REAL :: fv 
    REAL :: ft 
    REAL :: fs
    INTEGER :: i 
    INTEGER :: ntyp



    ! statement functions

    fs(x) = 66.85  * x
    ft(x) = 0.904  * x
    fv(x) = 0.315  * x


    rfac  =1.0e2 /gasr

    vkrmni=1.0  /vkrmn
    g2 = 0.75

    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          speedm(i)=SQRT(um(i)**2+vm(i)**2)
          speedm(i)=MAX(2.0  ,speedm(i))
       END IF
    END DO
    !     
    !     cu and ct are the friction and heat transfer coefficients.
    !     cun and ctn are the neutral friction and heat transfer
    !     coefficients.
    !     
    IF (jstneu) THEN
       DO i = 1, nmax
          ntyp=itype(i)
          zl = z2(ntyp,mon(i)) + 11.785  * z0(i)
          cuni(i)=LOG((dzm(i)-d(i))/z0(i))*vkrmni
          ustarn(i)=speedm(i)/cuni(i)
          IF (zl < dzm(i)) THEN
             xct1 = LOG((dzm(i)-d(i))/(zl-d(i)))
             xct2 = LOG((zl-d(i))/z0(i))
             xctu1 = xct1
             xctu2 = LOG((zl-d(i))/(z2(ntyp,mon(i))-d(i)))
             ctni(i) = (xct1 + g2 * xct2) *vkrmni
          ELSE
             xct2 =  LOG((dzm(i)-d(i))/z0(i))
             xctu1 =  0.
             xctu2 =  LOG((dzm(i)-d(i))/(z2(ntyp,mon(i))-d(i)))
             ctni(i) = g2 * xct2 *vkrmni
          END IF
          !
          !     neutral values of ustar and ventmf
          !
          u2(i) = speedm(i) - ustarn(i)*vkrmni*(xctu1 + g2*xctu2)
       END DO
       RETURN
    END IF
    !     
    !     stability branch based on bulk richardson number.
    !     
    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          !     
          !     freelm(i)=.false.
          !     
          thm= tm(i)*bps(i)
          ntyp=itype(i)
          zl = z2(ntyp,mon(i)) + 11.785  * z0(i)
          thvgm   = ta(i)-thm
          rib     =-thvgm   *grav*(dzm(i)-d(i)) &
               /(thm*(speedm(i)-u2(i))**2)
          ! Manzi Suggestion:
          ! rib   (i)=max(-10.  ,rib(i))
          rib      =MAX(-1.5  ,rib   )        
          rib      =MIN( 0.165  ,rib   )
          IF (rib    < 0.0) THEN
             grib = -rib   
             grzl = -rib   *(zl-d(i))/(dzm(i)-d(i))
             grz2 = -rib   *z0(i)/(dzm(i)-d(i))
             fvv = fv(grib)
             IF (zl < dzm(i)) THEN
                ftt = ft(grib) + (g2-1.) * ft(grzl) - g2 * ft(grz2)
             ELSE
                ftt = g2*(ft(grib) - ft(grz2))
             END IF
             cui    = cuni(i) - fvv
             cti    = ctni(i) - ftt
          ELSE
             rzl = rib   /(dzm(i)-d(i))*(zl-d(i))
             rz2 = rib   /(dzm(i)-d(i))*z0(i)
             fvv = fs(rib   )
             IF (zl < dzm(i)) THEN
                ftt = fs(rib   ) + (g2-1) * fs(rzl) - g2 * fs(rz2)
             ELSE
                ftt = g2 * (fs(rib   ) - fs(rz2))
             END IF
             cui    = cuni(i) + fvv
             cti    = ctni(i) + ftt
          ENDIF
          cu    (i)=1.0/cui   
          !**(JP)** ct is not used anywhere else
          !ct    (i)=1.0/cti(i)
          !     
          !     
          !     surface friction velocity and ventilation mass flux
          !     
          ustar (i)=speedm(i)*cu(i)
          ra(i) = cti    / ustar(i)
          !**(JP)** ran is not used anywhere else
          !ran(i) = ctni(i) / ustarn(i)
          !ran(i) = MAX(ran(i), 0.8 )
          ra(i) = MAX(ra(i), 0.8 )
       END IF
    END DO
  END SUBROUTINE vntlax




  ! runoff :performs inter-layer moisture exchanges.



  SUBROUTINE runoff( &
       roff  ,tg    ,td    ,capac ,w     ,itype ,dtc3x ,nmax  ,ncols    )
    !
    !-----------------------------------------------------------------------
    ! input parameters
    !-----------------------------------------------------------------------
    !   w(3)     roff     slope    bee      satco     zdepth
    !   phsat    poros    pie      dtc3x    snomel
    !   w(3)     
    !
    !-----------------------------------------------------------------------
    ! output parameters
    !-----------------------------------------------------------------------
    !   w(3)     roff
    !-----------------------------------------------------------------------
    !
    ! roff.......Runoff (escoamente superficial e drenagem)(m)
    ! slope......Inclinacao de perda hidraulica na camada profunda do solo
    ! bee........Fator de retencao da umidade no solo (expoente da umidade do 
    !            solo)    
    ! satco......Condutividade hidraulica do solo saturado(m/s)
    ! zdepth(id).Profundidade das camadas de solo id=1 superficial
    ! zdepth(id).Profundidade das camadas de solo id=2 camada de raizes
    ! zdepth(id).Profundidade das camadas de solo id=3 camada de drenagem
    ! phsat......Potencial matricial do solo saturado(m) (tensao do solo em
    !            saturacao)
    ! poros......Porosidade do solo
    ! pie........pi = 3.1415926e0
    ! dtc3x......time increment dt        
    ! snomel.....Calor latente de fusao(J/kg)
    ! w(id)......Grau de saturacao de umidade do solo id=1 na camada superficial
    ! w(id)......Grau de saturacao de umidade do solo id=2 na camada de raizes
    ! w(id)......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    ! capac(iv)..Agua interceptada iv=1 no dossel (m)
    ! capac(iv)..Agua interceptada iv=2 na cobertura do solo (m)
    ! tg.........Temperatura da superficie do solo  (K)
    ! td.........Temperatura do solo profundo (K)
    ! itype......Classe de textura do solo
    ! tf.........Temperatura de congelamento (K)
    ! idp........Parametro para as camadas de solo idp=1->3
    ! nmax.......
    ! ncols......Numero de ponto por faixa de latitude
    ! ityp.......13 
    !-----------------------------------------------------------------------
    INTEGER, INTENT(in   ) :: ncols  

    REAL,    INTENT(in   ) :: dtc3x             
    INTEGER, INTENT(in   ) :: nmax              

    INTEGER, INTENT(in   ) :: itype (ncols)      
    !
    !     prognostic variables
    !     
    REAL,    INTENT(in   ) :: tg   (ncols)        
    REAL,    INTENT(in   ) :: td   (ncols)        
    REAL,    INTENT(in   ) :: capac(ncols,2)      
    REAL,    INTENT(inout) :: w    (ncols,3)      
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL,    INTENT(inout) :: roff (ncols)        

    REAL    :: q3g   (ncols)                      
    REAL    :: div   (ncols)                      
    REAL    :: twi   (ncols,3)                    
    REAL    :: twip  (ncols,3)                    
    REAL    :: twipp (ncols,3)                    
    REAL    :: avk   (ncols)                      
    REAL    :: aaa_1, aaa_2                       
    REAL    :: bbb_1, bbb_2                       
    REAL    :: ccc_1, ccc_2                       
    REAL    :: qqq_1, qqq_2                       

    REAL    :: subdt 
    REAL    :: subdti 
    REAL    :: slop 
    REAL    :: pows
    REAL    :: wmax 
    REAL    :: wmin 
    REAL    :: pmax 
    REAL    :: pmin 
    REAL    :: dpdw 
    REAL    :: rsame
    REAL    :: tsnow 
    REAL    :: areas 
    REAL    :: tgs 
    REAL    :: ts 
    REAL    :: props 
    REAL    :: dpdwdz 
    REAL    :: denom 
    REAL    :: rdenom
    REAL    :: qmax 
    REAL    :: qmin 
    REAL    :: excess 
    REAL    :: deficit
    INTEGER :: n 
    INTEGER :: i 
    INTEGER :: ntyp
    REAL,    PARAMETER     :: smal2 = 1.0e-3

    subdt =dtc3x
    subdti=1.0 /dtc3x
    q3g=0.0  
    !
    !     eliinate negative soil moisture
    !
    DO n = 1, nmax
       IF (w(n,1) < 0.) w(n,1)=smal2
       IF (w(n,2) < 0.) w(n,2)=smal2
       IF (w(n,3) < 0.) w(n,3)=smal2
    END DO

    DO i = 1, 3
       DO n = 1, nmax
          ntyp      =itype(n)
          twi(n,i)=MIN(1.0, MAX(.03,w(n,i)))
          twip(n,i) =EXP(-bee(ntyp)*LOG(twi(n,i)))
          twipp(n,i)=EXP((2.0*bee(ntyp)+3.0)*LOG(MIN(1.0,twi(n,i))))
       END DO
    END DO

    DO n = 1, nmax
       ntyp = itype(n)
       slop = 0.1736
       IF (poros(ntyp) == 0.4352) slop = 0.0872
       IF (poros(ntyp) == 0.4577) slop = 0.3420
       !
       !     calculation of gravitationally driven drainage from w(3) : taken
       !     as an integral of time varying conductivity.addition of liston
       !     baseflow term to original q3g to insure flow in
       !     dry season. modified liston baseflow constant scaled
       !     by available water.
       !     
       !     q3g (q3) : equation (62) , se-86
       !
       pows    = 2.0 *bee(ntyp)+2.0
       q3g (n) = EXP(-pows*LOG(twi(n,3))) &
            +satco(ntyp)/(zdepth(ntyp,3)*poros(ntyp))* &
            slop*pows*subdt
       q3g (n) = EXP(LOG(q3g(n))/pows)
       q3g (n) =-(1.0 /q3g(n)-w(n,3)) &
            *poros(ntyp)*zdepth(ntyp,3)*subdti
       q3g (n) = MAX(0.0 ,q3g(n))
       q3g (n) = MIN(q3g(n), w(n,3)*poros(ntyp)*zdepth(ntyp,3) &
            *subdti)
       q3g (n) = q3g(n)+0.002*poros(ntyp)*zdepth(ntyp,3)*0.5 &
            /86400.*w(n,3)
    END DO
    !
    !     calculation of inter-layer exchanges of water due to gravitation   
    !     and hydraulic gradient. the values of w(x) + dw(x) are used to    
    !     calculate the potential gradients between layers.                 
    !     modified calculation of mean conductivities follows milly and     
    !     eagleson (1982 ), reduces recharge flux to top layer.             
    !                                                                       
    !      dpdw           : estimated derivative of soil moisture potential
    !                       with respect to soil wetness. assumption of    
    !                       gravitational drainage used to estimate likely 
    !                       minimum wetness over the time step.           
    !                                                                    
    !      qqq  (q     )  : equation (61) , s-86                          
    !             i,i+1                                                  
    !            -                                                        
    !      avk  (k     )  : equation (4.14) , milly and eagleson (1982)  
    !             i,i+1                                                 
    !                                                                      
    DO n = 1, nmax
       ntyp=itype(n)
       wmax = MAX( w(n,1), w(n,2), w(n,3), 0.05 )   
       wmax = MIN( wmax, 1. )    
       pmax = EXP(-bee(ntyp)*LOG(wmax))      
       wmin = EXP(-1./bee(ntyp)*LOG(pmax-2./(phsat(ntyp) &
            *(zdepth(ntyp,1)+2.*zdepth(ntyp,2)+zdepth(ntyp,3)))))        
       wmin = MIN( w(n,1), w(n,2), w(n,3), wmin ) 
       wmin = MAX( wmin, 0.02 )               
       pmin = EXP(-bee(ntyp)*LOG(wmin))                  
       dpdw = phsat(ntyp)*( pmax-pmin )/( wmax-wmin )    

       ! hand unrolling of next do loop, first iteration

       rsame = 0.
       avk(n)    =twip(n,1)*twipp(n,1)-twip(n,1+1)*twipp(n,1+1)
       div(n)    =twip(n,1+1) - twip(n,1)
       IF(ABS(div(n)).LE.1.0e-7) rsame = 1.
       avk(n)=satco(ntyp)*avk(n)/ &
            ((1.0 +3.0 /bee(ntyp))*div(n)+ rsame)
       avk(n)=MAX(avk(n),satco(ntyp)*MIN(twipp(n,1),twipp(n,1+1)))
       avk(n)=MIN(avk(n),1.01*(satco(ntyp) &
            *MAX(twipp(n,1),twipp(n,1+1))))
       !
       !     conductivities and base flow reduced when temperature drops below 
       !     freezing                                                      
       !
       tsnow = MIN (tf-0.01, tg(n))                                  
       areas = MIN (0.999,13.2*capac(n,2))                              
       tgs = tsnow*areas + tg(n)*(1.-areas)           
       ts  = tgs*(2-1) + td(n)*(1-1)      
       props = (ts-(tf-10.))/10.      
       props = MAX(0.05,MIN(1., props))  
       avk(n) = avk(n) * props      
       q3g(n) = q3g(n) * props                    
       !
       !     backward implicit calculation of flows between soil layers      
       !                                                                    
       dpdwdz= dpdw * 2./( zdepth(ntyp,1) + zdepth(ntyp,1+1) )
       aaa_1=1.+avk(n)*dpdwdz* &
            (1./zdepth(ntyp,1)+1./zdepth(ntyp,1+1))      &
            *subdt/poros(ntyp)                                 
       bbb_1 =-avk(n)* dpdwdz*1./zdepth(ntyp,2)*subdt/poros(ntyp)      
       ccc_1 = avk(n) * (dpdwdz * ( w(n,1)-w(n,1+1) )+1.+(1-1) &
            *dpdwdz*q3g(n)*1./zdepth(ntyp,3)*subdt/poros(ntyp))  

       ! hand unrolling of next do loop, second iteration

       rsame = 0.
       avk(n)    =twip(n,2)*twipp(n,2)-twip(n,2+1)*twipp(n,2+1)
       div(n)    =twip(n,2+1) - twip(n,2)
       IF(ABS(div(n)).LE.1.0e-7) rsame = 1.
       avk(n)=satco(ntyp)*avk(n)/ &
            ((1.0 +3.0 /bee(ntyp))*div(n)+ rsame)
       avk(n)=MAX(avk(n),satco(ntyp)*MIN(twipp(n,2),twipp(n,2+1)))
       avk(n)=MIN(avk(n),1.01*(satco(ntyp) &
            *MAX(twipp(n,2),twipp(n,2+1))))
       !
       !     conductivities and base flow reduced when temperature drops below 
       !     freezing                                                      
       !
       tsnow = MIN (tf-0.01, tg(n))                                  
       areas = MIN (0.999,13.2*capac(n,2))                              
       tgs = tsnow*areas + tg(n)*(1.-areas)           
       ts  = tgs*(2-2) + td(n)*(2-1)      
       props = (ts-(tf-10.))/10.      
       props = MAX(0.05,MIN(1., props))  
       avk(n) = avk(n) * props      
       q3g(n) = q3g(n) * props                    
       !
       !     backward implicit calculation of flows between soil layers      
       !                                                                    
       dpdwdz= dpdw * 2./( zdepth(ntyp,2) + zdepth(ntyp,2+1) )
       aaa_2=1.+avk(n)*dpdwdz* &
            (1./zdepth(ntyp,2)+1./zdepth(ntyp,2+1))      &
            *subdt/poros(ntyp)                                 
       bbb_2 =-avk(n)* dpdwdz*1./zdepth(ntyp,2)*subdt/poros(ntyp)      
       ccc_2 = avk(n) * (dpdwdz * ( w(n,2)-w(n,2+1) )+1.+(2-1) &
            *dpdwdz*q3g(n)*1./zdepth(ntyp,3)*subdt/poros(ntyp))  


       !       DO i = 1, 2
       !          rsame = 0.
       !          avk(n)    =twip(n,i)*twipp(n,i)-twip(n,i+1)*twipp(n,i+1)
       !          div(n)    =twip(n,i+1) - twip(n,i)
       !          IF(ABS(div(n)).LE.1.0e-7) rsame = 1.
       !          avk(n)=satco(ntyp)*avk(n)/ &
       !               ((1.0 +3.0 /bee(ntyp))*div(n)+ rsame)
       !          avk(n)=MAX(avk(n),satco(ntyp)*MIN(twipp(n,i),twipp(n,i+1)))
       !          avk(n)=MIN(avk(n),1.01*(satco(ntyp) &
       !               *MAX(twipp(n,i),twipp(n,i+1))))
       !          !
       !          !     conductivities and base flow reduced when temperature drops below 
       !          !     freezing                                                      
       !          !
       !          tsnow = MIN (tf-0.01, tg(n))                                  
       !          areas = MIN (0.999,13.2*capac(n,2))                              
       !          tgs = tsnow*areas + tg(n)*(1.-areas)           
       !          ts  = tgs*(2-i) + td(n)*(i-1)      
       !          props = (ts-(tf-10.))/10.      
       !          props = MAX(0.05,MIN(1., props))  
       !          avk(n) = avk(n) * props      
       !          q3g(n) = q3g(n) * props                    
       !          !
       !          !     backward implicit calculation of flows between soil layers      
       !          !                                                                    
       !          dpdwdz= dpdw * 2./( zdepth(ntyp,i) + zdepth(ntyp,i+1) )
       !          aaa(i)=1.+avk(n)*dpdwdz* &
       !               (1./zdepth(ntyp,i)+1./zdepth(ntyp,i+1))      &
       !               *subdt/poros(ntyp)                                 
       !          bbb(i) =-avk(n)* dpdwdz*1./zdepth(ntyp,2)*subdt/poros(ntyp)      
       !          ccc(i) = avk(n) * (dpdwdz * ( w(n,i)-w(n,i+1) )+1.+(i-1) &
       !               *dpdwdz*q3g(n)*1./zdepth(ntyp,3)*subdt/poros(ntyp))  
       !       END DO
       denom    = ( aaa_1*aaa_2 - bbb_1*bbb_2 )           
       rdenom   = 0.                                         
       IF (ABS(denom) < 1.e-6 ) rdenom = 1.           
       rdenom   = ( 1.-rdenom)/( denom + rdenom )          
       qqq_1   = ( aaa_2*ccc_1 - bbb_1*ccc_2 ) * rdenom  
       qqq_2   = ( aaa_1*ccc_2 - bbb_2*ccc_1 ) * rdenom 
       !
       !     update wetness of each soil moisture layer due to layer interflow
       !     and base flow.
       !
       w(n,3)  = w(n,3) - q3g(n)*subdt/(poros(ntyp)*zdepth(ntyp,3))       
       roff(n) = roff(n) + q3g(n) * subdt                            

       ! hand unrolling of next do loop, first iteration

       qmax     =  w(n,1)   * (poros(ntyp)*zdepth(ntyp,1)  /subdt)
       qmin     = -w(n,1+1) * (poros(ntyp)*zdepth(ntyp,1+1)/subdt)       
       qqq_1   =  MIN( qqq_1,qmax)
       qqq_1   =  MAX( qqq_1,qmin)
       w(n,1)   =  w(n,1)  -qqq_1/(poros(ntyp)*zdepth(ntyp,1) /subdt)    
       w(n,1+1) =  w(n,1+1)+ &
            qqq_1/(poros(ntyp)*zdepth(ntyp,1+1)/subdt)   

       ! hand unrolling of next do loop, second iteration

       qmax     =  w(n,2)   * (poros(ntyp)*zdepth(ntyp,2)  /subdt)
       qmin     = -w(n,2+1) * (poros(ntyp)*zdepth(ntyp,2+1)/subdt)       
       qqq_2   =  MIN( qqq_2,qmax)
       qqq_2   =  MAX( qqq_2,qmin)
       w(n,2)   =  w(n,2)  -qqq_2/(poros(ntyp)*zdepth(ntyp,2) /subdt)    
       w(n,2+1) =  w(n,2+1)+ &
            qqq_2/(poros(ntyp)*zdepth(ntyp,2+1)/subdt)   
       !     DO i = 1, 2                         
       !        qmax     =  w(n,i)   * (poros(ntyp)*zdepth(ntyp,i)  /subdt)
       !        qmin     = -w(n,i+1) * (poros(ntyp)*zdepth(ntyp,i+1)/subdt)       
       !        qqq(i)   =  MIN( qqq(i),qmax)
       !        qqq(i)   =  MAX( qqq(i),qmin)
       !        w(n,i)   =  w(n,i)  -qqq(i)/(poros(ntyp)*zdepth(ntyp,i) /subdt)    
       !        w(n,i+1) =  w(n,i+1)+ &
       !             qqq(i)/(poros(ntyp)*zdepth(ntyp,i+1)/subdt)   
       !     END DO

       ! hand unrolling of next do loop, first iteration

       excess   = MAX(0.,(w(n,1) - 1.))                      
       w(n,1)   = w(n,1) - excess                             
       roff(n)  = roff(n) + excess * poros(ntyp)*zdepth(ntyp,1)        

       ! hand unrolling of next do loop, second iteration

       excess   = MAX(0.,(w(n,2) - 1.))                      
       w(n,2)   = w(n,2) - excess                             
       roff(n)  = roff(n) + excess * poros(ntyp)*zdepth(ntyp,2)        

       ! hand unrolling of next do loop, third iteration

       excess   = MAX(0.,(w(n,3) - 1.))                      
       w(n,3)   = w(n,3) - excess                             
       roff(n)  = roff(n) + excess * poros(ntyp)*zdepth(ntyp,3)        

       !     DO i = 1, 3                                       
       !        excess   = MAX(0.,(w(n,i) - 1.))                      
       !        w(n,i)   = w(n,i) - excess                             
       !        roff(n)  = roff(n) + excess * poros(ntyp)*zdepth(ntyp,i)        
       !     END DO

       ! hand unrolling of next do loop, first iteration

       deficit   = MAX (0.,(1.e-12 - w(n,1)))
       w(n,1)    = w(n,1) + deficit
       w(n,1+1)  = w(n,1+1)-deficit*zdepth(ntyp,1)/zdepth(ntyp,1+1)

       ! hand unrolling of next do loop, second iteration

       deficit   = MAX (0.,(1.e-12 - w(n,2)))
       w(n,2)    = w(n,2) + deficit
       w(n,2+1)  = w(n,2+1)-deficit*zdepth(ntyp,2)/zdepth(ntyp,2+1)

       !
       !     prevent negative values of www(i)
       !     

       !       DO i = 1,2
       !          deficit   = MAX (0.,(1.e-12 - w(n,i)))
       !          w(n,i)    = w(n,i) + deficit
       !          w(n,i+1)  = w(n,i+1)-deficit*zdepth(ntyp,i)/zdepth(ntyp,i+1)
       !       END DO

       w(n,3)      = MAX (w(n,3),1.e-12)
    END DO
  END SUBROUTINE runoff


  ! stres2 :calculates the adjustment to light dependent stomatal resistance
  !         by temperature, humidity and stress factors (simplified).



  SUBROUTINE stres2( &
       icount,ft1   ,fp1   ,icheck,ta    ,ea    ,rst   ,phsoil,stm   , &
       tc    ,tg    ,w     ,vcover,itype , &
       rootd ,zdepth,nmax  ,ncols ,topt2 ,tll2  ,tu2   , &
       defac2,ph12  ,ph22)
    !
    !
    !-----------------------------------------------------------------------
    ! ityp........numero das classes de solo 13 
    ! icg.........Parametros da vegetacao (icg=1 topo e icg=2 base)
    ! idp.........Parametro para as camadas de solo idp=1->3
    ! icount......     
    ! ft1.........temperature  factor   simplified
    ! fp1.........soil water potential factor simplified 
    ! hl........heat of evaporation of water   (j/kg)
    ! nmax........
    ! topt........Temperatura ideal de funcionamento estomatico 
    ! tll.........Temperatura minima de funcionamento estomatico
    ! tu..........Temperatura maxima de funcionamento estomatico
    ! defac.......Parametro de deficit de pressao de vapor d'agua
    ! ph1.........Coeficiente para o efeito da agua no solo
    ! ph2 ........Potencial de agua no solo para ponto de Wilting
    ! rootd.......Profundidade das raizes
    ! zdepth......Profundidade para as tres camadas de solo
    ! itype.......Classe de textura do solo
    ! vcover(iv)..Fracao de cobertura de vegetacao iv=1 Top 
    ! vcover(iv)..Fracao de cobertura de vegetacao iv=2 Bottom 
    ! tc..........Temperatura da copa "dossel"(K)
    ! tg .........Temperatura da superficie do solo (K)
    ! w(id).......Grau de saturacao de umidade do solo id=1 na camada superficial
    ! w(id).......Grau de saturacao de umidade do solo id=2 na camada de raizes
    ! w(id).......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    ! ta..........Temperatura no nivel de fonte de calor do dossel (K) 
    ! ea..........Pressao de vapor 
    ! rst ........Resisttencia Estomatica "Stomatal resistence" (s/m)
    ! phsoil......soil moisture potential of the i-th soil layer
    ! stm.........Resisttencia Estomatica "Stomatal resistence" (s/m)  
    ! icheck......this version assumes dew-free conditions "icheck=1" to 
    !             estimate ea for buoyancy term in vntmf or ra.
    !-----------------------------------------------------------------------
    INTEGER, INTENT(IN   ) :: ncols
    INTEGER, INTENT(IN   ) :: icount
    REAL,    INTENT(INOUT) :: ft1   (ncols)
    REAL,    INTENT(INOUT) :: fp1   (ncols)
    !
    INTEGER, INTENT(in   ) :: nmax                 
    !
    !     vegetation and soil parameters
    !
    REAL,    INTENT(in   ) :: rootd (ityp,icg)     
    REAL,    INTENT(in   ) :: zdepth(ityp,idp)     
    INTEGER, INTENT(in   ) :: itype (ncols)         
    REAL,    INTENT(in   ) :: vcover(ncols,icg)     
    !
    !     prognostic variables
    !     
    REAL,    INTENT(in   ) :: tc   (ncols)          
    REAL,    INTENT(in   ) :: tg   (ncols)          
    REAL,    INTENT(in   ) :: w    (ncols,3)        
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(in   ) :: ta    (ncols)         
    REAL,    INTENT(in   ) :: ea    (ncols)         
    REAL,    INTENT(inout) :: rst   (ncols,icg)     
    REAL,    INTENT(in   ) :: phsoil(ncols,idp)     
    REAL,    INTENT(in   ) :: stm   (ncols,icg)     
    INTEGER, INTENT(in   ) :: icheck(ncols)         
    REAL   , INTENT(in   ) :: topt2 (ncols,icg)
    REAL   , INTENT(in   ) :: tll2  (ncols,icg)
    REAL   , INTENT(in   ) :: tu2   (ncols,icg)
    REAL   , INTENT(in   ) :: defac2(ncols,icg)
    REAL   , INTENT(in   ) :: ph12  (ncols,icg)
    REAL   , INTENT(in   ) :: ph22  (ncols,icg)    
    !
    REAL    :: tv  (ncols)                          
    REAL    :: d2  (ncols)                          
    REAL    :: ft  (ncols)                          
    REAL    :: drop(ncols)                          
    REAL    :: fd  (ncols)                          
    REAL    :: fp  (ncols)                          
    REAL    :: ftpd(ncols)                          
    REAL    :: dep(3)
    REAL    :: hl3i 
    REAL    :: xrot 
    REAL    :: xdr 
    REAL    :: arg
    INTEGER :: iveg
    INTEGER :: i 
    INTEGER :: ntyp
    !     
    !     humidity, temperature and transpiration factors
    !     
    tv=0.0   !  CALL reset(tv,ncols*13)
    hl3i=1.0   /(hl*1000.0  )
    iveg=1

    IF (icount == 1) THEN
       !cdir novector
       DO i = 1, nmax
          IF (icheck(i) == 1) THEN
             ntyp=itype(i)
             IF ((ntyp == 11) .OR. (ntyp == 13)) THEN
                CONTINUE
             ELSE
                IF (iveg == 1) THEN
                   tv  (i)=tc(i)
                ELSE
                   tv  (i)=tg(i)
                END IF
                tv(i)=MIN((tu2 (i,iveg)-0.1   ),tv(i))
                tv(i)=MAX((tll2(i,iveg)+0.1   ),tv(i))
                d2(i)=(tu2  (i,iveg)-topt2(i,iveg)) &
                     /(topt2(i,iveg)-tll2 (i,iveg))
                ft(i)=(tv(i)-tll2(i,iveg))/ &
                     (topt2(i,iveg)-tll2(i,iveg)) &
                     *EXP(d2(i)*LOG( &
                     (tu2 (i,iveg)-tv(i))/ &
                     (tu2(i,iveg)-topt2(i,iveg)) ) )
                ft(i) = MIN(ft(i), 1.e0)
                ft(i) = MAX(ft(i), 1.e-5)
                ft1(i) = ft(i)
                !     
                !  simplified calculation of soil water potential factor, fp
                !     
                xrot = rootd(ntyp,iveg)
                dep(1) = 0.0e0
                dep(2) = 0.0e0
                dep(3) = 0.0e0
                dep(1) = MIN(zdepth(ntyp,1), xrot)
                xrot = xrot - zdepth(ntyp,1)
                IF (xrot > 0.0e0) THEN
                   dep(2) = MIN(zdepth(ntyp,2), xrot)
                   xrot = xrot - zdepth(ntyp,2)
                ENDIF
                IF (xrot > 0.0e0) THEN
                   dep(3) = MIN(zdepth(ntyp,3), xrot)
                   xrot = xrot - zdepth(ntyp,3)
                ENDIF
                xdr = (phsoil(i,1) * dep(1) + phsoil(i,2) * dep(2) &
                     +phsoil(i,3) * dep(3)) / rootd(ntyp,iveg)
                xdr = - xdr
                IF (xdr <= 1.0e-5) xdr = 1.0e-5
                xdr = LOG (xdr)
                arg = -ph12(i,1)*(ph22(i,1)-xdr)
                arg = MIN(arg,0.0)
                fp(i) = 1.e0 - EXP(arg)
                IF ((w(i,2) > 0.15e0) .AND. (fp(i) < 0.05e0)) fp(i)=0.05e0
                fp(i) = MIN(fp(i), 1.e0)
                fp(i) = MAX(fp(i), 1.e-5)
                fp1(i) = fp(i)
             END IF
          END IF
       END DO
    END IF

    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          ntyp=itype(i)
          drop(i)=EXP(21.65605   -5418.0   /ta(i))      -ea(i)
          fd(i) = MAX( 1.0e-5,  1.0/(1.0+ defac2(i,iveg)*drop(i)))
          fd(i) = MIN(fd(i), 1.e0)
       END IF
    END DO

    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          ntyp=itype(i)
          rst(i,2) = 1.e5
          IF ((ntyp == 11) .OR. (ntyp == 13)) THEN
             rst(i,1) = 1.0e5
             CYCLE
          END IF
          ftpd(i)    =  fd(i)* ft1(i) * fp1(i)
          rst(i,iveg)=stm(i,iveg)/(ftpd(i)*vcover(i,iveg))
          rst(i,iveg)=MIN(rst(i,iveg),1.0e5)
       END IF
    END DO

  END SUBROUTINE stres2



  ! update :performs the updating of soil moisture stores 
  !         and interception capacity.


  SUBROUTINE update( &
       bps   ,deadtg,deadtc,deadqm,ect   ,eci   ,egt   ,egi   ,egs   , &
       eg    ,hc    ,hg    ,ecmass,egmass,etmass,hflux ,chf   ,shf   , &
       ra    ,rb    ,rd    ,ea    ,etc   ,etg   ,btc   ,btg   ,cc    , &
       cg    ,dtc   ,dtg   ,dtm   ,dqm   ,tc    ,tg    ,td    ,capac , &
       tm    ,nmax  ,dtc3x ,ncols)
    !     
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    !   ncols.......Numero de ponto por faixa de latitude
    !   pie.........Constante Pi=3.1415926e0
    !   hl..........heat of evaporation of water   (j/kg)  
    !   snomel......heat of melting 
    !   tf..........Temperatura de congelamento (K)  
    !   dtc3x.......time increment dt 
    !   nmax........
    !   tm..........Temperature of reference (fourier)  
    !   tc..........Temperatura da copa "dossel" canopy leaf temperature(K)   
    !   tg..........Temperatura da superficie do solo ground temperature (K) 
    !   td..........Temperatura do solo profundo (K)
    !   capac ......Agua interceptada iv=1 no dossel "water store capacity of leaves"(m)
    !   capac.......Agua interceptada iv=2 na cobertura do solo (m) 
    !   ra..........Resistencia Aerodinamica (s/m) 
    !   rb .........bulk boundary layer resistance             (s/m)
    !   rd..........aerodynamic resistance between ground
    !               and canopy air space                      
    !   ea..........Pressao de vapor 
    !   etc.........Pressure of vapor at top of the copa  
    !   etg.........Pressao de vapor no base da copa
    !   btc.........btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i))
    !   btg.........btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i)) 
    !   cc..........heat capacity of the canopy
    !   cg..........heat capacity of the ground 
    !   dtc.........dtc(i)=pblsib(i,2,5)*dtc3x
    !   dtg.........dtg(i)=pblsib(i,1,5)*dtc3x
    !   dtm.........dtm(i)=pblsib(i,3,5)*dtc3x
    !   dqm.........dqm(i)=pblsib(i,4,5)*dtc3x
    !   ect.........Transpiracao(J/m*m)   
    !   eci.........Evaporacao da interceptacao da agua (J/m*m)   
    !   egt ........Transpiracao na base da copa (J/m*m)  .  
    !   egi.........Evaporacao da neve (J/m*m)   
    !   egs.........Evaporacao do solo arido (J/m*m)   
    !   eg..........Soma da transpiracao na base da copa +  Evaporacao do solo arido
    !              +  Evaporacao da neve  " eg   (i)=egt(i)+egs(i)+egi(i)"    
    !   hc..........total sensible heat lost of top from the veggies.     
    !   hg..........total sensible heat lost of base from the veggies.     
    !   ecmass......Mass of water lost of top from the veggies.
    !   egmass......Mass of water lost of base from the veggies.
    !   etmass......total mass of water lost from the veggies.
    !   hflux.......total sensible heat lost from the veggies. 
    !   chf.........heat fluxes into the canopy  in w/m**2   
    !   shf.........heat fluxes into the ground, in w/m**2      
    !   deadtg......
    !   deadtc......
    !   deadqm......
    !   bps.........
    !-----------------------------------------------------------------------
    INTEGER, INTENT(in   ) :: ncols

    REAL,    INTENT(in   ) :: dtc3x         
    INTEGER, INTENT(in   ) :: nmax          
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL,    INTENT(in   ) :: tm  (ncols)    
    !
    !     prognostic variables
    !     
    REAL,    INTENT(in   ) :: tc   (ncols)   
    REAL,    INTENT(in   ) :: tg   (ncols)   
    REAL,    INTENT(in   ) :: td   (ncols)   
    REAL,    INTENT(inout) :: capac(ncols,2) 
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(in   ) :: ra  (ncols)        
    REAL,    INTENT(in   ) :: rb  (ncols)        
    REAL,    INTENT(in   ) :: rd  (ncols)        
    REAL,    INTENT(in   ) :: ea  (ncols)        
    REAL,    INTENT(in   ) :: etc (ncols)        
    REAL,    INTENT(in   ) :: etg (ncols)        
    REAL,    INTENT(in   ) :: btc (ncols)        
    REAL,    INTENT(in   ) :: btg (ncols)        
    REAL,    INTENT(in   ) :: cc  (ncols)        
    REAL,    INTENT(in   ) :: cg  (ncols)        
    REAL,    INTENT(in   ) :: dtc (ncols)        
    REAL,    INTENT(in   ) :: dtg (ncols)        
    REAL,    INTENT(in   ) :: dtm (ncols)        
    REAL,    INTENT(in   ) :: dqm (ncols)        
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL,    INTENT(inout) :: ect   (ncols)   
    REAL,    INTENT(inout) :: eci   (ncols)   
    REAL,    INTENT(inout) :: egt   (ncols)   
    REAL,    INTENT(inout) :: egi   (ncols)   
    REAL,    INTENT(inout) :: egs   (ncols)   
    REAL,    INTENT(in   ) :: eg    (ncols)   
    REAL,    INTENT(in   ) :: hc    (ncols)   
    REAL,    INTENT(in   ) :: hg    (ncols)   
    REAL,    INTENT(out  ) :: ecmass(ncols)   
    REAL,    INTENT(out  ) :: egmass(ncols)   
    REAL,    INTENT(out  ) :: etmass(ncols)   
    REAL,    INTENT(out  ) :: hflux (ncols)   
    REAL,    INTENT(out  ) :: chf   (ncols)   
    REAL,    INTENT(out  ) :: shf   (ncols)   
    !
    !     derivatives
    !
    REAL,    INTENT(in   ) :: deadtg(ncols)   
    REAL,    INTENT(in   ) :: deadtc(ncols)   
    REAL,    INTENT(in   ) :: deadqm(ncols)   
    !
    !     this is for coupling with closure turbulence model
    !
    REAL,    INTENT(in   ) :: bps  (ncols)    




    REAL :: tgen  (ncols)                     
    REAL :: tcen  (ncols)                     
    REAL :: tmen  (ncols)                     
    REAL :: taen  (ncols)                     
    REAL :: eaen  (ncols)                     
    REAL :: d1    (ncols)                     
    REAL :: estarc(ncols)                     
    REAL :: estarg(ncols)                     
    REAL :: facks (ncols)                     
    INTEGER :: i
    REAL :: timcon 
    REAL :: dtc3xi 
    REAL :: hlati 
    REAL :: hlat3i 
    REAL :: snofac
    !     
    !     adjustment of temperatures and vapor pressure ,
    !     sensible heat fluxes. n.b. latent heat fluxes cannot be derived
    !     from estarc, estarg, ea due to linear result of implicit method
    !     
    !
    !
    !
    DO i = 1, nmax
       tgen(i)=tg(i)+dtg(i)
       tcen(i)=tc(i)+dtc(i)
       tmen(i)=tm(i)+dtm(i)
       d1(i)=1.0   /ra(i)+1.0   /rb(i)+1.0   /rd(i)
       !     
       !     compute the fluxes consistent with the differencing scheme.
       !     
       taen(i)=(tgen(i)/rd(i)+tcen(i)/ &
            rb(i)+tmen(i)*bps(i)/ra(i))/d1(i)
       eaen(i)=ea(i)+deadtc(i)*dtc(i)+deadtg(i)* &
            dtg(i)+deadqm(i)*dqm(i)
       !     
       !     vapor pressures within the canopy and the moss.
       !     
       estarc(i)=etc(i)+btc(i)*dtc(i)
       estarg(i)=etg(i)+btg(i)*dtg(i)
    END DO
    DO i = 1, nmax
       IF (tgen(i) <= tf) THEN
          egs(i)=eg(i)-egi(i)
          egt(i)=0.0
       END IF
    END DO
    !     
    !     heat fluxes into the canopy and the ground, in w/m**2
    !     
    timcon=2.0   *pie/86400.0
    dtc3xi=1.0   /dtc3x
    hlati =1.0   /        hl
    hlat3i=1.0   /(1.0e3*hl)
    DO i = 1, nmax
       chf(i)=dtc3xi*cc(i)*dtc(i)
       shf(i)=dtc3xi*cg(i)*dtg(i) + timcon*cg(i)*(tg(i)+dtg(i)-td(i))
    END DO
    !     
    !     evaporation losses are expressed in j m-2 : when divided by
    !     ( hl*1000.) loss is in m m-2
    !     
    snofac=1.0   /( 1.0   +snomel*hlat3i)
    DO i = 1, nmax
       facks(i)=1.0
       IF (tcen(i) <= tf) facks(i)=snofac
       IF ((ect(i)+eci(i)) <= 0.0) THEN 
          eci(i)  =ect(i)+eci(i)
          ect(i)  =0.0
          facks(i)=1.0   /facks(i)
       END IF
    END DO
    DO i = 1, nmax
       capac(i,1)=capac(i,1)-eci(i)*facks(i)*hlat3i
       !     
       !     mass terms are in kg m-2 dt-1
       !     
       ecmass(i)=(ect(i)+eci(i)*facks(i))*hlati
    END DO
    DO i = 1, nmax
       facks(i)=1.0
       IF (tgen(i) <= tf) facks(i)=snofac
       IF ((egt(i)+egi(i)) <= 0.0) THEN
          egi(i)  =egt(i)+egi(i)
          egt(i)  =0.0
          facks(i)=1.0  /facks(i)
       END IF
    END DO
    DO i = 1, nmax
       capac(i,2)=capac(i,2)-egi(i)*facks(i)*hlat3i
       egmass(i)=(egt(i)+egs(i)+egi(i)*facks(i))*hlati
       !     
       !     total mass of water and total sensible heat lost from the veggies.
       !     
       etmass(i)=ecmass(i)+egmass(i)
       hflux (i)=hc(i)+hg(i)
    END DO
  END SUBROUTINE update






  SUBROUTINE sflxes(&
       hgdtg ,hgdtc ,hgdtm ,hcdtg ,hcdtc ,hcdtm ,egdtg ,egdtc ,egdqm , &
       ecdtg ,ecdtc ,ecdqm ,deadtg,deadtc,deadqm,icheck,bps   ,psb   , &
       dzm   ,em    ,gmt   ,gmq   ,cu    ,cuni  ,ctni  ,ustar ,rhoair, &
       psy   ,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    ,ect   ,eci   , &
       egt   ,egi   ,egs   ,ec    ,eg    ,hc    ,hg    ,ecidif,egidif, &
       ecmass,egmass,etmass,hflux ,chf   ,shf   ,ra    ,rb    ,rd    , &
       rc    ,rg    ,tcta  ,tgta  ,ta    ,ea    ,etc   ,etg   ,btc   , &
       btg   ,u2    ,radt  ,rst   ,rsoil ,hrr   ,phsoil,cc    ,cg    , &
       satcap,dtc   ,dtg   ,dtm   ,dqm   ,stm   ,thermk,tc    ,tg    , &
       td    ,capac ,w     ,qm    ,tm    ,um    ,vm    ,psur  ,vcover, &
       z0x   ,d     ,rdc   ,rbc   ,z0    ,itype ,dtc3x ,mon   ,nmax  , &
       jstneu,ncols ,zlt2 ,topt2  ,tll2  ,tu2   ,defac2,ph12  ,ph22  , &
       xcover) 

    !-----------------------------------------------------------------------
    ! sflxes :performs surface flux parameterization.
    !-----------------------------------------------------------------------
    !
    !  ncols........Numero de ponto por faixa de latitude
    !  ityp........numero das classes de solo 13 
    !  imon........Numero maximo de meses no ano (12)
    !  icg.........Parametros da vegetacao (icg=1 topo e icg=2 base)
    !  idp.........Camadas de solo (1 a 3)
    !  pie.........Constante Pi=3.1415926e0   
    !  stefan......Constante de Stefan Boltzmann
    !  cp..........specific heat of air (j/kg/k) 
    !  hl..........heat of evaporation of water   (j/kg)  
    !  grav........gravity constant      (m/s**2)  
    !  snomel......heat of melting (j m-1)
    !  tf..........Temperatura de congelamento (K)      
    !  gasr........Constant of dry air      (j/kg/k)  
    !  epsfac......Constante 0.622 Razao entre as massas moleculares do vapor
    !              de agua e do ar seco
    !  jstneu......The first call to vntlat just gets the neutral values of ustar
    !              and ventmf para jstneu=.TRUE..
    !  dtc3x.......time increment dt 
    !  mon.........Number of month at year (1-12)      
    !  nmax........  
    !  topt........Temperatura ideal de funcionamento estomatico  
    !  tll.........Temperatura minima de funcionamento estomatico   
    !  tu..........Temperatura maxima de funcionamento estomatico   
    !  defac.......Parametro de deficit de pressao de vapor d'agua 
    !  ph1.........Coeficiente para o efeito da agua no solo   
    !  ph2.........Potencial de agua no solo para ponto de Wilting   
    !  rootd.......Profundidade das raizes
    !  bee.........Expoente da curva de retencao "expoente para o solo umido" 
    !  phsat.......Tensao do solo saturado " Potencial de agua no solo saturado"  
    !  zdepth......Profundidade para as tres camadas de solo
    !  zlt(icg)....Indice de area foliar "LEAF AREA INDEX" icg=1 topo da copa  
    !  zlt(icg)....Indice de area foliar "LEAF AREA INDEX" icg=2 base da copa     
    !  x0x.........Comprimento de rugosidade   
    !  xd..........Deslocamento do plano zero
    !  z2..........Altura do topo do dossel    
    !  xdc.........Constant related to aerodynamic resistance
    !              between ground and canopy air space
    !  xbc.........Constant related to bulk boundary layer resistance
    !  itype.......Classe de textura do solo
    !  vcover(iv)..Fracao de cobertura de vegetacao iv=1 Top 
    !  vcover(iv)..Fracao de cobertura de vegetacao iv=2 Botto
    !  z0x.........roughness length                           (m)   
    !  d...........Displacement height                        (m)  
    !  rdc.........Constant related to aerodynamic resistance
    !              between ground and canopy air space
    !  rbc.........Constant related to bulk boundary layer resistance   
    !  z0..........Roughness length  
    !  qm..........reference specific humidity (fourier)  
    !  tm .........reference temperature    (fourier)                (k)  
    !  um..........Razao entre zonal pseudo-wind (fourier) e seno da
    !              colatitude    
    !  vm..........Razao entre meridional pseudo-wind (fourier) e seno da
    !              colatitude      
    !  psur........surface pressure in mb
    !  tc..........Temperatura da copa "dossel"(K)   
    !  tg..........Temperatura da superficie do solo (K)   
    !  td..........Temperatura do solo profundo (K)   
    !  capac(iv)...Agua interceptada iv=1 no dossel "water store capacity of leaves"(m)
    !  capac(iv)...Agua interceptada iv=2 na cobertura do solo (m) 
    !  w(id).......Grau de saturacao de umidade do solo id=1 na camada superficial
    !  w(id).......Grau de saturacao de umidade do solo id=2 na camada de raizes
    !  w(id).......Grau de saturacao de umidade do solo id=3 na camada de drenagem       
    !  ra..........Resistencia Aerodinamica (s/m)   
    !  rb..........bulk boundary layer resistance             (s/m)   
    !  rd..........aerodynamic resistance between ground      (s/m)
    !              and canopy air space   
    !  rc..........Resistencia do topo da copa     
    !  rg......... Resistencia da base da copa    
    !  tcta........Diferenca entre tc-ta                      (k) 
    !  tgta........Diferenca entre tg-ta                      (k)  
    !  ta..........Temperatura no nivel de fonte de calor do dossel (K)    
    !  ea..........Pressure of vapor           
    !  etc.........Pressure of vapor at top of the copa   
    !  etg.........Pressao de vapor no base da copa   
    !  btc.........btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i)).   
    !  btg.........btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i))   
    !  u2..........wind speed at top of canopy                (m/s)    
    !  radt........net heat received by canopy/ground vegetation    
    !  rst ........Resisttencia Estomatica "Stomatal resistence" (s/m)   
    !  rsoil ......Resistencia do solo (s/m) 
    !  hrr.........rel. humidity in top layer   
    !  phsoil......soil moisture potential of the i-th soil layer
    !  cc..........heat capacity of the canopy    
    !  cg..........heat capacity of the ground   
    !  satcap......saturation liquid water capacity         (m)
    !  dtc.........dtc(i)=pblsib(i,2,5)*dtc3x   
    !  dtg.........dtg(i)=pblsib(i,1,5)*dtc3x   
    !  dtm.........dtm(i)=pblsib(i,3,5)*dtc3x   
    !  dqm.........dqm(i)=pblsib(i,4,5)*dtc3x   
    !  stm ........Variavel utilizada mo cal. da Resistencia     
    !  thermk......canopy emissivity
    !  ect.........Transpiracao no topo da copa (J/m*m)   
    !  eci.........Evaporacao da agua interceptada no topo da copa (J/m*m)   
    !  egt.........Transpiracao na base da copa (J/m*m)   
    !  egi.........Evaporacao da neve (J/m*m)   
    !  egs.........Evaporacao do solo arido (J/m*m)   
    !  ec..........Soma da Transpiracao e Evaporacao da agua interceptada pelo
    !              topo da copa   ec   (i)=eci(i)+ect(i)   
    !  eg..........Soma da transpiracao na base da copa +  Evaporacao do solo arido
    !              +  Evaporacao da neve  " eg   (i)=egt(i)+egs(i)+egi(i)"
    !  hc..........total sensible heat lost of top from the veggies.     
    !  hg..........total sensible heat lost of base from the veggies.    
    !  ecidif......check if interception loss term has exceeded canopy storage
    !              ecidif(i)=MAX(0.0   , eci(i)-capac(i,1)*hlat3 )
    !  egidif......check if interception loss term has exceeded canopy storage
    !              ecidif(i)=MAX(0.0   , egi(i)-capac(i,1)*hlat3 )
    !  ecmass......Mass of water lost of top from the veggies.
    !  egmass......Mass of water lost of base from the veggies.
    !  etmass......total mass of water lost from the veggies.
    !  hflux.......total sensible heat lost from the veggies. 
    !  chf.........heat fluxes into the canopy  in w/m**2    
    !  shf.........heat fluxes into the ground, in w/m**2   
    !  bps.........   
    !  psb.........   
    !  dzm.........Altura media de referencia  para o vento para o calculo
    !               da estabilidade do escoamento       
    !  em..........Pressao de vapor da agua   
    !  gmt.........   
    !  gmq.........specific humidity of reference (fourier)   
    !  cu..........Friction  transfer coefficients.
    !  cuni........neutral friction transfer  coefficients.  
    !  ctni........neutral heat transfer coefficients.  
    !  ustar.......surface friction velocity  (m/s) 
    !  rhoair......Desnsidade do ar
    !  psy.........(cp/(hl*epsfac))*psur(i)   
    !  rcp.........densidade do ar vezes o calor especifico do ar  
    !  wc..........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !              indice de area foliar no topo da copa     
    !  wg..........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !              indice de area foliar na base da copa     
    !  fc..........Condicao de oravalho 0 ou 1 na topo da copa    
    !  fg..........Condicao de oravalho 0 ou 1 na base da copa    
    !  hr..........rel. humidity in top layer    
    !  icheck......this version assumes dew-free conditions "icheck=1" to 
    !              estimate ea for buoyancy term in vntmf or ra.
    !  hgdtg.......n.b. fluxes expressed in joules m-2 
    !  hgdtc.......n.b. fluxes expressed in joules m-2 
    !  hgdtm.......n.b. fluxes expressed in joules m-2 
    !  hcdtg.......n.b. fluxes expressed in joules m-2 
    !  hcdtc.......n.b. fluxes expressed in joules m-2 
    !  hcdtm.......n.b. fluxes expressed in joules m-2 
    !  egdtg.......partial derivative calculation for latent heat
    !  egdtc.......partial derivative calculation for latent heat 
    !  egdqm.......partial derivative calculation for latent heat 
    !  ecdtg.......partial derivative calculation for latent heat 
    !  ecdtc.......partial derivative calculation for latent heat 
    !  ecdqm.......partial derivative calculation for latent heat 
    !  deadtg......
    !  deadtc......
    !  deadqm......
    !
    !-----------------------------------------------------------------------
    INTEGER, INTENT(in   ) :: ncols

    LOGICAL, INTENT(out  ) :: jstneu              

    REAL   , INTENT(in   ) :: dtc3x               
    INTEGER, INTENT(in   ) :: mon(ncols)          
    INTEGER, INTENT(in   ) :: nmax                
    !
    INTEGER, INTENT(in   ) :: itype (ncols)        
    !
    REAL,    INTENT(inout) :: vcover(ncols,icg)    
    REAL,    INTENT(inout) :: z0x   (ncols)        
    REAL,    INTENT(inout) :: d     (ncols)        
    REAL,    INTENT(inout) :: rdc   (ncols)        
    REAL,    INTENT(inout) :: rbc   (ncols)        
    REAL,    INTENT(inout) :: z0    (ncols)        
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL,    INTENT(inout) :: qm  (ncols)          
    REAL,    INTENT(inout) :: tm  (ncols)          
    REAL,    INTENT(in   ) :: um  (ncols)          
    REAL,    INTENT(in   ) :: vm  (ncols)          
    REAL,    INTENT(inout) :: psur(ncols)          
    !
    !     prognostic variables
    !     
    REAL,    INTENT(inout) :: tc   (ncols)         
    REAL,    INTENT(inout) :: tg   (ncols)         
    REAL,    INTENT(inout) :: td   (ncols)         
    REAL,    INTENT(inout) :: capac(ncols,2)       
    REAL,    INTENT(inout) :: w    (ncols,3)       
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(inout) :: ra    (ncols)        
    REAL,    INTENT(out  ) :: rb    (ncols)        
    REAL,    INTENT(out  ) :: rd    (ncols)        
    REAL,    INTENT(out  ) :: rc    (ncols)        
    REAL,    INTENT(out  ) :: rg    (ncols)        
    REAL,    INTENT(out  ) :: tcta  (ncols)        
    REAL,    INTENT(out  ) :: tgta  (ncols)        
    REAL,    INTENT(out  ) :: ta    (ncols)        
    REAL,    INTENT(out  ) :: ea    (ncols)        
    REAL,    INTENT(out  ) :: etc   (ncols)        
    REAL,    INTENT(out  ) :: etg   (ncols)        
    REAL,    INTENT(out  ) :: btc   (ncols)        
    REAL,    INTENT(out  ) :: btg   (ncols)        
    REAL,    INTENT(inout) :: u2    (ncols)        
    REAL,    INTENT(inout) :: radt  (ncols,icg)    
    REAL,    INTENT(inout) :: rst   (ncols,icg)    
    REAL,    INTENT(out  ) :: rsoil (ncols)        
    REAL,    INTENT(out  ) :: hrr   (ncols)        
    REAL,    INTENT(inout) :: phsoil(ncols,idp)    
    REAL,    INTENT(inout) :: cc    (ncols)        
    REAL,    INTENT(inout) :: cg    (ncols)        
    REAL,    INTENT(inout) :: satcap(ncols,icg)    
    REAL,    INTENT(out  ) :: dtc   (ncols)        
    REAL,    INTENT(out  ) :: dtg   (ncols)        
    REAL,    INTENT(out  ) :: dtm   (ncols)        
    REAL,    INTENT(out  ) :: dqm   (ncols)        
    REAL,    INTENT(out  ) :: stm   (ncols,icg)    
    REAL,    INTENT(inout) :: thermk(ncols)        
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL,    INTENT(inout) :: ect   (ncols)        
    REAL,    INTENT(inout) :: eci   (ncols)        
    REAL,    INTENT(inout) :: egt   (ncols)        
    REAL,    INTENT(inout) :: egi   (ncols)        
    REAL,    INTENT(inout) :: egs   (ncols)        
    REAL,    INTENT(out  ) :: ec    (ncols)        
    REAL,    INTENT(out  ) :: eg    (ncols)        
    REAL,    INTENT(out  ) :: hc    (ncols)        
    REAL,    INTENT(out  ) :: hg    (ncols)        
    REAL,    INTENT(out  ) :: ecidif(ncols)        
    REAL,    INTENT(out  ) :: egidif(ncols)        
    REAL,    INTENT(out  ) :: ecmass(ncols)        
    REAL,    INTENT(out  ) :: egmass(ncols)        
    REAL,    INTENT(out  ) :: etmass(ncols)        
    REAL,    INTENT(out  ) :: hflux (ncols)        
    REAL,    INTENT(out  ) :: chf   (ncols)        
    REAL,    INTENT(out  ) :: shf   (ncols)        
    !
    !     this is for coupling with closure turbulence model
    !
    REAL,    INTENT(in   ) :: bps   (ncols)        
    REAL,    INTENT(in   ) :: psb   (ncols)        
    REAL,    INTENT(in   ) :: dzm   (ncols)        
    REAL,    INTENT(in   ) :: em    (ncols)        
    REAL,    INTENT(inout) :: gmt   (ncols,3)      
    REAL,    INTENT(inout) :: gmq   (ncols,3)      
    REAL,    INTENT(inout) :: cu    (ncols)        
    REAL,    INTENT(inout) :: cuni  (ncols)        
    REAL,    INTENT(inout) :: ctni  (ncols)        
    REAL,    INTENT(inout) :: ustar (ncols)        
    REAL,    INTENT(in   ) :: rhoair(ncols)        
    REAL,    INTENT(in   ) :: psy   (ncols)        
    REAL,    INTENT(out  ) :: rcp   (ncols)        
    REAL,    INTENT(inout) :: wc    (ncols)        
    REAL,    INTENT(inout) :: wg    (ncols)        
    REAL,    INTENT(inout) :: fc    (ncols)        
    REAL,    INTENT(inout) :: fg    (ncols)        
    REAL,    INTENT(inout) :: hr    (ncols)        

    INTEGER, INTENT(out  ) :: icheck(ncols)        
    !
    !     derivatives
    !
    REAL,    INTENT(out  ) :: hgdtg (ncols)        
    REAL,    INTENT(out  ) :: hgdtc (ncols)        
    REAL,    INTENT(out  ) :: hgdtm (ncols)        
    REAL,    INTENT(out  ) :: hcdtg (ncols)        
    REAL,    INTENT(out  ) :: hcdtc (ncols)        
    REAL,    INTENT(out  ) :: hcdtm (ncols)        
    REAL,    INTENT(out  ) :: egdtg (ncols)        
    REAL,    INTENT(out  ) :: egdtc (ncols)        
    REAL,    INTENT(out  ) :: egdqm (ncols)        
    REAL,    INTENT(out  ) :: ecdtg (ncols)        
    REAL,    INTENT(out  ) :: ecdtc (ncols)        
    REAL,    INTENT(out  ) :: ecdqm (ncols)        
    REAL,    INTENT(out  ) :: deadtg(ncols)        
    REAL,    INTENT(out  ) :: deadtc(ncols)        
    REAL,    INTENT(out  ) :: deadqm(ncols)        
    !
    REAL,    INTENT(in   ) :: zlt2    (ncols,icg)
    REAL   , INTENT(in   ) :: topt2   (ncols,icg)
    REAL   , INTENT(in   ) :: tll2    (ncols,icg)
    REAL   , INTENT(in   ) :: tu2     (ncols,icg)
    REAL   , INTENT(in   ) :: defac2  (ncols,icg)
    REAL   , INTENT(in   ) :: ph12    (ncols,icg)
    REAL   , INTENT(in   ) :: ph22    (ncols,icg)    
    !
    REAL   , INTENT(IN   ) :: xcover(ityp, imon, icg)

    REAL :: ustarn(ncols)      

    REAL :: psit
    REAL :: fac
    REAL :: y1
    REAL :: y2
    REAL :: ecf (ncols)         
    REAL :: egf (ncols)         
    REAL :: dewc(ncols)         
    REAL :: dewg(ncols)         
    !     
    REAL :: tcsav (ncols)
    REAL :: tgsav (ncols)
    REAL :: tmsav (ncols)
    REAL :: qmsav (ncols)
    REAL :: tsav  (ncols)
    REAL :: esav  (ncols)
    REAL :: rdsav (ncols,2)
    REAL :: wt
    REAL :: ft1   (ncols)
    REAL :: fp1   (ncols)
    INTEGER :: idewco(ncols)
    !
    INTEGER, PARAMETER :: icmax = 10
    REAL,    PARAMETER :: small = 1.0e-3
    REAL    :: gxx 
    REAL    :: capaci 
    REAL    :: eee 
    REAL    :: dtmdt 
    REAL    :: dqmdt
    INTEGER :: i 
    INTEGER :: ntyp 
    INTEGER :: ncount 
    INTEGER :: icount
    ! 

    DO i = 1, nmax
       tcsav(i)=tc(i)
       tgsav(i)=tg(i)
       tmsav(i)=tm(i)
       qmsav(i)=qm(i)
       rdsav(i,1)=radt(i,1)
       rdsav(i,2)=radt(i,2)
       stm(i,1)=rst(i,1)
       stm(i,2)=rst(i,2)
    END DO
    !
    !     airmod checks for the effects of snow
    !     
    CALL airmod( &
         tg    ,capac ,z0x   ,d     ,rdc   ,rbc   ,itype , &
         mon   ,nmax  ,ncols )
    !     
    !     sib roughness length
    !     
    DO i = 1, nmax
       z0    (i)=z0x(i)
    END DO

    gxx   =grav/461.5
    capaci= 1.0 /0.004

    DO i = 1, nmax
       ntyp   =itype(i)
       wc  (i)=MIN(1.0 ,capac(i,1)/satcap(i,1))
       wg  (i)=MIN(1.0 ,capac(i,2)/satcap(i,2))
       !
       !     rsoil function from fit to camillo and gurney (1985) data.
       !     wetness of upper 0.5 cm of soil calculated from approximation
       !     to milly flow equation with reduced (1/50) conductivity in
       !     top layer.
       !     
       wt = MAX(small,w(i,1))
       wt=wt+(0.75*zdepth(ntyp,1)/(zdepth(ntyp,1)+ &
            zdepth(ntyp,2)))*(wt-(w(i,2)*w(i,2)/wt))*0.5*50.0
       fac   =MIN(wt,0.99)
       fac   =MAX(fac   ,small)
       rsoil(i)=101840.0*(1.0-EXP(0.0027 *LOG(fac   )))
       psit    =phsat(ntyp)*EXP(-bee(ntyp)*LOG(fac   ))
       eee=psit   *gxx/tg(i)

       hrr  (i)=MAX (small,EXP(eee))
       hr   (i)=hrr(i)

       IF (tg(i) <= tf) THEN
          vcover(i,2)=1.0
          wg    (i)  =MIN(1.0 ,capac(i,2)*capaci)
          rst   (i,2)=rsoil(i)
          stm   (i,2)=rsoil(i)
       END IF

       fc(i)=1.0
       fg(i)=1.0
    END DO
    !
    !     this is the start of iteration of time integration
    !     to avoid oscillation
    !     
    ncount=0
7000 CONTINUE
    ncount=ncount+1
    DO i = 1, nmax
       icheck(i)=1
       etc(i)=EXP(21.65605  -5418.0  /tc(i))
       etg(i)=EXP(21.65605  -5418.0  /tg(i))
    END DO
    !     
    !     first guesses for ta and ea
    !     
    IF (ncount == 1) THEN
       DO i = 1, nmax
          ta (i)=tc(i)
          ea (i)=qm(i)*psur(i)/(epsfac+qm(i))
       END DO
    END IF
    !     
    !     the first call to vntlat just gets the neutral values of ustar
    !     and ventmf.
    !     
    jstneu=.TRUE.

    CALL vntlax(ustarn, &
         icheck,bps   ,dzm   ,cu    ,cuni  ,ctni  ,ustar ,ra    ,ta    , &
         u2    ,tm    ,um    ,vm    ,d     ,z0    ,itype ,z2    , &
         mon   ,nmax  ,jstneu,ncols )

    jstneu=.FALSE.

    CALL vntlax(ustarn, &
         icheck ,bps  ,dzm   ,cu    ,cuni  ,ctni  ,ustar ,ra    ,ta    , &
         u2     ,tm   ,um    ,vm    ,d     ,z0    ,itype ,z2    , &
         mon    ,nmax ,jstneu,ncols )

    DO i = 1, nmax
       tcta(i)=tc(i)/bps(i)-tm(i)
       tgta(i)=tg(i)/bps(i)-tm(i)
    END DO
    CALL rbrd( &
         rb    ,rd    ,tcta  ,tgta  ,u2    ,tg    ,rdc   ,rbc   ,itype , &
         z2    ,mon   ,nmax  ,ncols ,zlt2)
    !     
    !     iterate for air temperature and ventilation mass flux
    !     n.b. this version assumes dew-free conditions to estimate ea
    !     for buoyancy term in vntmf or ra.
    !     
    icount = 0
2000 icount = icount + 1
    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          tsav(i) = ta (i)
          esav(i) = ea (i)
       END IF
    END DO
    CALL vntlax(ustarn, &
         icheck,bps   ,dzm   ,cu    ,cuni  ,ctni  ,ustar ,ra    ,ta    , &
         u2    ,tm    ,um    ,vm    ,d     ,z0    ,itype ,z2    , &
         mon   ,nmax  ,jstneu,ncols )

    CALL cut( &
         icheck,em    ,rhoair,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    , &
         ra    ,rb    ,rd    ,rc    ,rg    ,ea    ,etc   ,etg   ,rst   , &
         rsoil ,vcover,nmax  ,ncols )

    CALL stres2( &
         icount,ft1   ,fp1   ,icheck,ta    ,ea    ,rst   ,phsoil,stm   , &
         tc    ,tg    ,w     ,vcover,itype ,&
         rootd ,zdepth,nmax  ,ncols ,topt2 ,tll2  ,tu2   , &
       defac2,ph12  ,ph22)

    CALL cut(  &
         icheck,em    ,rhoair,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    , &
         ra    ,rb    ,rd    ,rc    ,rg    ,ea    ,etc   ,etg   ,rst   , &
         rsoil ,vcover,nmax  ,ncols )

    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          ta(i)= (tg(i)/rd(i)+tc(i)/rb(i)+tm(i)/ra(i)*bps(i)) &
               /(1.0 /rd(i)+1.0 /rb(i)+1.0 /ra(i))
       END IF
    END DO

    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          y1   =ABS(ta(i)-tsav(i))
          y2   =ABS(ea(i)-esav(i))
          IF((y1    <= 1.0e-2 .AND. y2    <= 5.0e-3) &
               .OR. icount > icmax) THEN
             icheck(i)=0
          END IF
       END IF
    END DO

    DO i = 1, nmax
       IF (icheck(i) == 1) GOTO 2000
    END DO

    DO i = 1, nmax
       fc    (i)=1.0
       fg    (i)=1.0
       idewco(i)=0
       icheck(i)=1
    END DO

    DO i = 1, nmax
       tc(i)    =tcsav(i)
       tg(i)    =tgsav(i)
       tm(i)    =tmsav(i)
       qm(i)    =qmsav(i)
       radt(i,1)=rdsav(i,1)
       radt(i,2)=rdsav(i,2)
       etc(i)=EXP(21.65605  -5418.0  /tc(i))
       etg(i)=EXP(21.65605  -5418.0  /tg(i))
       btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i))
       btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i))
    END DO

3000 CONTINUE

    CALL cut( &
         icheck,em    ,rhoair,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    , &
         ra    ,rb    ,rd    ,rc    ,rg    ,ea    ,etc   ,etg   ,rst   , &
         rsoil ,vcover,nmax  ,ncols )

    DO i = 1, nmax
       IF (icheck(i) == 1) THEN
          ecf (i)=SIGN(1.0  ,etc(i)-ea(i))
          egf (i)=SIGN(1.0  ,etg(i)-ea(i))
          dewc(i)=fc(i)*2.0  -1.0
          dewg(i)=fg(i)*2.0  -1.0
          ecf (i)=ecf(i)*dewc(i)
          egf (i)=egf(i)*dewg(i)
       END IF
    END DO

    DO i = 1, nmax
       IF ( (ecf(i) > 0.0  .AND. egf(i) > 0.0 ).OR. &
            idewco(i) == 3) THEN
          icheck(i)=0
       ELSE
          idewco(i)=idewco(i)+1
          IF (idewco(i) == 1) THEN
             fc(i)=0.0 
             fg(i)=1.0
          ELSE IF (idewco(i) == 2) THEN
             fc(i)=1.0
             fg(i)=0.0
          ELSE IF (idewco(i) == 3) THEN
             fc(i)=0.0
             fg(i)=0.0
          END IF
       END IF
    END DO

    DO i=1,nmax
       IF (icheck(i) == 1) go to 3000
    END DO

    CALL temres(&
         bps   ,psb   ,em    ,gmt   ,gmq   ,psy   ,rcp   ,wc    ,wg    , &
         fc    ,fg    ,hr    ,hgdtg ,hgdtc ,hgdtm ,hcdtg ,hcdtc ,hcdtm , &
         egdtg ,egdtc ,egdqm ,ecdtg ,ecdtc ,ecdqm ,deadtg,deadtc,deadqm, &
         ect   ,eci   ,egt   ,egi   ,egs   ,ec    ,eg    ,hc    ,hg    , &
         ecidif,egidif,ra    ,rb    ,rd    ,rc    ,rg    ,ta    ,ea    , &
         etc   ,etg   ,btc   ,btg   ,radt  ,rst   ,rsoil ,hrr   ,cc    , &
         cg    ,satcap,dtc   ,dtg   ,dtm   ,dqm   ,thermk,tc    ,tg    , &
         td    ,capac ,qm    ,tm    ,psur  ,itype ,dtc3x ,mon   , &
         nmax  ,vcover,ncols ,xcover)

    IF (ncount <= 1) THEN
       DO i = 1, nmax
          tc(i)=tc(i)+dtc(i)
          tg(i)=tg(i)+dtg(i)
          tm(i)=tm(i)+dtm(i)
          qm(i)=qm(i)+dqm(i)
       END DO
       go to 7000
    END IF

    CALL update( &
         bps   ,deadtg,deadtc,deadqm,ect   ,eci   ,egt   ,egi   ,egs   , &
         eg    ,hc    ,hg    ,ecmass,egmass,etmass,hflux ,chf   ,shf   , &
         ra    ,rb    ,rd    ,ea    ,etc   ,etg   ,btc   ,btg   ,cc    , &
         cg    ,dtc   ,dtg   ,dtm   ,dqm   ,tc    ,tg    ,td    ,capac , &
         tm    ,nmax  ,dtc3x ,ncols)

    DO i = 1, nmax
       fac     =grav/(100.0 *psb(i)*dtc3x)
       dtmdt   =(gmt(i,3)+hflux (i)*fac   /(cp*bps(i)))/gmt(i,2)
       dqmdt   =(gmq(i,3)+etmass(i)*fac                  )/gmq(i,2)
       dtm  (i)=dtmdt   *dtc3x
       dqm  (i)=dqmdt   *dtc3x
       gmt(i,3)=dtmdt
       gmq(i,3)=dqmdt
       tm   (i)=tm(i)+dtm(i)
       qm   (i)=qm(i)+dqm(i)
    END DO
    DO i = 1, nmax
       ntyp=itype(i)
       vcover(i,2)=xcover(ntyp,mon(i),2)
       d  (i)=xd (ntyp,mon(i))
       z0x(i)=x0x(ntyp,mon(i))
       rdc(i)=xdc(ntyp,mon(i))
       rbc(i)=xbc(ntyp,mon(i))
    END DO
  END SUBROUTINE sflxes



  ! interc :calculation of (1) interception and drainage of rainfall and snow
  !                        (2) specific heat terms fixed for time step
  !                        (3) modifications for 4-th order model may not
  !                            conserve energy;
  !         modification: non-uniform precipitation convective ppn
  !                       is described by area-intensity relationship :-
  !
  !                       f(x)=a*exp(-b*x)+c
  !
  !                       throughfall, interception and infiltration
  !                       excess are functional on this relationship
  !                       and proportion of large-scale ppn.



  SUBROUTINE interc( &
       roff  ,cc    ,cg    ,satcap,snow  ,extk  ,tc    ,tg    ,td    , &
       capac ,w     ,tm    ,ppc   ,ppl   ,vcover,itype ,dtc3x , &
       nmax  ,ncols ,zlt2 )
    !
    !
    !        input parameters
    !-----------------------------------------------------------------------
    !   ppc.............precipitation rate ( cumulus )           (mm/s)
    !   ppl.............precipitation rate ( large scale )       (mm/s)
    !   w(1)............soil wetnessof ground surface
    !   poros...........porosity
    !   pie.............pai=3.14159..
    !   cw..............liquid water heat capacity               (j/m**3)
    !   clai............heat capacity of foliage
    !   capac(cg).......canopy/ground cover liquid water capacity(m)
    !   satcap(cg)......saturation liquid water capacity         (m)
    !   extk(cg,  ,  )..extinction coefficient
    !   zlt(1)..........canopy leaf and stem area density        (m**2/m**3)
    !   zlt(2)..........ground cover leaf and stem area index    (m**2/m**2)
    !   vcover(cg)......vegetation cover
    !   tm..............reference temperature                    (k)
    !   tc..............canopy temperature                       (k)
    !   tg..............ground temperature                       (k)
    !   tf..............freezing point                           (k)
    !   satco............mean soil hydraulic conductivity in the root zone
    !                                                            (m/s)
    !   dtc3x...........time increment dt
    !   snomel..........heat of melting                          (j/kg)
    !-----------------------------------------------------------------------
    !     in subr. parameters
    !-----------------------------------------------------------------------
    !   chisl...........soil conductivity
    !   difsl...........soil diffusivity
    !-----------------------------------------------------------------------
    !       output parameters
    !-----------------------------------------------------------------------
    !   roff............runoff
    !   snow............snow amount
    !   capac(cg).......canopy/ground cover liquid water capacity(m)
    !   cc..............heat capacity of the canopy
    !   cg..............heat capacity of the ground
    !   w(1)............soil wetnessof ground surface
    !-----------------------------------------------------------------------
    !   ncols...........Numero de ponto por faixa de latitude
    !   ityp............numero das classes de solo 13 
    !   imon............Numero maximo de meses no ano (12)
    !   icg.............Parametros da vegetacao (icg=1 topo e icg=2 base)
    !   iwv.............Compriment de onda iwv=1=visivel, iwv=2=infravermelho
    !                   proximo, iwv=3 infravermelho termal
    !   idp.............Camadas de solo (1 a 3)
    !   ibd.............Estado da vegetacao  ibd=1 verde / ibd=2 seco
    !   mon.............Numero do mes do ano (1-12)        
    !   nmax  
    !   zdepth..........Profundidade para as tres camadas de solo
    !   itype...........Classe de textura do solo
    !   td..............Temperatura do solo profundo (K)      
    !-----------------------------------------------------------------------
    INTEGER, INTENT(in   ) :: ncols

    REAL,    INTENT(in   ) :: dtc3x                  
    INTEGER, INTENT(in   ) :: nmax                   

    INTEGER, INTENT(in   ) :: itype (ncols)          
    REAL,    INTENT(in   ) :: vcover(ncols,icg)      
    !
    !     the size of working area is ncols*187
    !
    !     atmospheric parameters as boudary values for sib
    !
    REAL,    INTENT(in   ) :: tm  (ncols)             
    REAL,    INTENT(in   ) :: ppc (ncols)             
    REAL,    INTENT(in   ) :: ppl (ncols)             
    !
    !     prognostic variables
    !
    REAL,    INTENT(inout) :: tc   (ncols)            
    REAL,    INTENT(inout) :: tg   (ncols)            
    REAL,    INTENT(in   ) :: td   (ncols)            
    REAL,    INTENT(inout) :: capac(ncols,2)          
    REAL,    INTENT(inout) :: w    (ncols,3)          
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(out  ) :: cc    (ncols)           
    REAL,    INTENT(out  ) :: cg    (ncols)           
    REAL,    INTENT(in   ) :: satcap(ncols,icg)       
    REAL,    INTENT(inout) :: snow  (ncols,icg)       
    REAL,    INTENT(in   ) :: extk  (ncols,icg,iwv,ibd)
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL,   INTENT(inout) :: roff  (ncols)            
    REAL   , INTENT(in   ) :: zlt2    (ncols,icg)

    !    
    REAL    :: ap         (ncols)                          
    REAL    :: cp         (ncols)                          
    REAL    :: totalp(ncols)                          
    REAL    :: thru  (ncols)                          
    REAL    :: fpi   (ncols)                          
    REAL    :: chisl (ncols)                          
    REAL    :: csoil (ncols)                          
    REAL    :: p0         (ncols)                          
    REAL    :: ts         (ncols)                          
    REAL    :: specht(ncols)                          
    REAL    :: spwet1(ncols)                          
    REAL    :: zload (ncols)                          
    REAL    :: ccp   (ncols)                          
    REAL    :: cct   (ncols)                          
    REAL    :: zmelt (ncols)                          
    REAL    :: xsc   (ncols)                          
    REAL    :: tti   (ncols)                          
    REAL    :: xs         (ncols)                          
    REAL    :: arg   (ncols)                          
    REAL    :: tex   (ncols)                          
    REAL    :: tsd   (ncols)                          
    REAL    :: pinf  (ncols)                          
    REAL    :: equdep(ncols)                          
    REAL    :: roffo (ncols)                          
    REAL    :: tsf   (ncols)                          
    REAL    :: diff  (ncols)                          
    REAL    :: freeze(ncols)                          
    REAL    :: ccc   (ncols)                          
    REAL    :: spwet (ncols)                          
    REAL    :: snowp (ncols,2)                        
    REAL    :: capacp(ncols,2)                        

    REAL, PARAMETER :: pcoefs(2,2) = RESHAPE ( &
         (/20.0    ,0.0001  ,0.206e-8,0.9999  /), &
         (/2,2/))
    REAL, PARAMETER :: bp = 20.0
    REAL, PARAMETER :: difsl = 5.0e-7
    REAL    :: d1x
    REAL    :: theta
    INTEGER :: i 
    INTEGER :: iveg
    INTEGER :: ntyp
    !     
    !     diffusivity of the soil
    !     
    d1x   =SQRT(86400.0 /(pie*difsl))*0.5
    ap    = 0.0 !  CALL reset(ap,ncols*33)
    DO i = 1, nmax
       ap(i)=pcoefs(2,1)
       cp(i)=pcoefs(2,2)
       totalp(i)=ppc(i)+ppl(i)
       IF (totalp(i) >= 1.0e-8) THEN
          ap(i)=(ppc(i)*pcoefs(1,1)+ppl(i)*pcoefs(2,1))/totalp(i)
          cp(i)=(ppc(i)*pcoefs(1,2)+ppl(i)*pcoefs(2,2))/totalp(i)
       END IF
       roff(i)=0.0
       thru(i)=0.0
       fpi (i)=0.0
       !     
       !     conductivity of the soil, taking into account porosity
       !     
       ntyp=itype(i)
       theta   =w(i,1)*poros(ntyp)
       chisl(i)=( 9.8e-4 +1.2e-3 *theta )/( 1.1 -0.4 *theta )
       chisl(i)=chisl(i)*4.186e2
       !     
       !     heat capacity of the soil
       !     
       csoil(i)=chisl(i)*d1x
       !     
       !     precipitation is given in mm
       !     
       p0(i)=totalp(i)*0.001
    END DO
    DO iveg = 1, 2
       IF (iveg == 1) THEN
          DO i = 1, nmax
             ntyp     =itype(i)
             ts    (i)=tc (i)
             specht(i)=zlt2(i,1)*clai
          END DO
       ELSE
          DO i = 1, nmax
             ts    (i)=tg (i)
             specht(i)=csoil(i)
          END DO
       END IF
       DO i = 1, nmax
          IF (iveg == 1 .OR. ts(i) > tf) THEN
             xsc(i)=MAX(0.0  , capac(i,iveg)-satcap(i,iveg))
             capac(i,iveg)=capac(i,iveg)-xsc(i)
             roff(i)=roff(i)+xsc(i)
          END IF
       END DO
       DO i = 1, nmax
          ntyp=itype(i)
          spwet1(i)=MIN(0.05 ,capac(i,iveg))*cw
          capacp(i,iveg)=0.0
          snowp (i,iveg)=0.0
          IF (ts(i) > tf) THEN
             capacp(i,iveg)=capac (i,iveg)
          ELSE
             snowp (i,iveg)=capac (i,iveg)
          END IF
          capac (i,iveg)=capacp(i,iveg)
          snow  (i,iveg)=snowp (i,iveg)
          zload (i)     =capac (i,iveg)+snow(i,iveg)
          fpi(i)=( 1.0 -EXP(-extk(i,iveg,3,1)*zlt2(i,iveg) &
               /vcover(i,iveg))) *vcover(i,iveg)
          tti(i)=p0(i)*( 1.0 -fpi(i) )
          IF (iveg.EQ.2) tti(i) = p0(i)
       END DO
       !     
       !     proportional saturated area (xs) and leaf drainage(tex)
       !     
       DO i = 1, nmax
          xs(i)=1.0
          IF (p0(i) >= 1.0e-9) THEN
             arg(i)=(satcap(i,iveg)-zload(i))/ &
                  (p0(i)*fpi(i)*ap(i))-cp(i)/ap(i)
             IF (arg(i) >= 1.0e-9) THEN
                xs(i)=-1.0 /bp* LOG( arg(i) )
                xs(i)= MIN ( xs(i) , 1.0 )
                xs(i)= MAX ( xs(i) , 0.0 )
             END IF
          END IF
       END DO
       DO i = 1, nmax
          tex(i)=p0(i)*fpi(i) &
               *(ap(i)/bp*(1.0 -EXP(-bp*xs(i)))+cp(i)*xs(i))- &
               (satcap(i,iveg)-zload(i))*xs(i)
          tex(i)= MAX ( tex(i), 0.0 )
          IF (iveg == 2) tex(i) = 0.0
          !     
          !     total throughfall (thru) and store augmentation
          !     
          thru(i)=tti(i)+tex(i)
          IF (iveg == 2 .AND. tg(i) <= tf) THEN
             thru(i)=0.0
          END IF
          pinf(i)=p0(i)-thru(i)
          IF (tm(i) > tf) THEN
             capac(i,iveg)=capac(i,iveg)+pinf(i)
          ELSE
             snow (i,iveg)=snow (i,iveg)+pinf(i)
          END IF
       END DO
       IF (iveg == 2)   THEN
          DO i = 1, nmax
             ntyp=itype(i)
             IF (tm(i) <= tf) THEN
                snow  (i,iveg)=snowp(i,iveg)+p0(i)
                thru  (i)=0.0
             ELSE
                !
                !     instantaneous overland flow contribution ( roff )
                !     
                equdep(i)=satco(ntyp)*dtc3x
                xs(i)=1.0
                IF (thru(i) >= 1.0e-9) THEN
                   arg(i)=equdep(i)/( thru(i)*ap(i) ) -cp(i)/ap(i)
                   IF (arg(i) >= 1.0e-9) THEN
                      xs(i)=-1.0 /bp* LOG( arg(i) )
                      xs(i)= MIN ( xs(i), 1.0 )
                      xs(i)= MAX ( xs(i), 0.0 )
                   END IF
                END IF
                roffo(i)=thru(i)* &
                     (ap(i)/bp*(1.0 -EXP(-bp*xs(i)))+cp(i)*xs(i)) &
                     -equdep(i)*xs(i)
                roffo(i)= MAX ( roffo(i), 0.0 )
                roff (i)= roff (i)+roffo(i)
                w(i,1)=w(i,1)+(thru(i)-roffo(i))/ &
                     ( poros(ntyp)*zdepth(ntyp,1))
             END IF
          END DO
       END IF
       !     
       !     temperature change due to addition of precipitation
       !     
       DO i = 1, nmax
          diff(i)=(capac (i,iveg)+snow (i,iveg) &
               -capacp(i,iveg)-snowp(i,iveg))*cw
          ccp(i)=specht(i)+spwet1(i)
          cct(i)=specht(i)+spwet1(i)+diff(i)
          tsd(i)=( ts(i)*ccp(i)+tm(i)*diff(i) )/cct(i)
          tsf(i)=( ts(i)-tf)*( tm(i)-tf)
       END DO
       DO i = 1, nmax
          IF (tsf(i) < 0.0) THEN
             IF (tsd(i) <= tf) THEN
                !
                !     freezing of water on canopy or ground
                !     
                ccc(i)=capacp(i,iveg)*snomel
                IF (ts(i) < tm(i)) ccc(i)=diff(i)*snomel/cw
                tsd   (i)=( ts(i)*ccp(i)+tm(i)*diff(i)+ccc(i) )/cct(i)
                freeze(i)= tf*cct(i)-( ts(i)*ccp(i)+tm(i)*diff(i) )
                freeze(i)=( MIN ( ccc(i), freeze(i) ))/snomel
                IF (tsd(i) > tf) tsd(i) = tf - 0.1
                snow (i,iveg)=snow (i,iveg)+freeze(i)
                capac(i,iveg)=capac(i,iveg)-freeze(i)
             ELSE
                !
                !     melting of water on canopy or ground
                !     
                ccc(i)=- snow(i,iveg)*snomel
                IF (ts(i) > tm(i)) ccc(i)=- diff(i)*snomel/cw
                tsd   (i)=( ts(i)*ccp(i)+tm(i)*diff(i)+ccc(i) )/cct(i)
                freeze(i)=( tf*cct(i)-( ts(i)*ccp(i)+tm(i)*diff(i) ))
                freeze(i)= MAX ( ccc(i), freeze(i) ) /snomel
                IF (tsd(i) <= tf) tsd(i) = tf - 0.1
                snow (i,iveg)=snow (i,iveg)+freeze(i)
                capac(i,iveg)=capac(i,iveg)-freeze(i)
             END IF
          END IF
       END DO
       DO i = 1, nmax
          IF (iveg == 1) THEN
             tc(i)=tsd(i)
          ELSE
             tg(i)=tsd(i)
          END IF
       END DO
       DO i = 1, nmax
          IF (snow(i,iveg) >= 0.0000001 .OR. iveg == 2) THEN
             zmelt(i) = 0.0
             IF (td(i) > tf) THEN
                zmelt(i)=capac(i,iveg)
             ELSE
                roff (i)=roff(i)+capac(i,iveg)
             END IF
             capac(i,iveg)=0.0
             !     
             !     if tg is less than tf water accumulates as snowpack in capac(2)
             !     
             ntyp=itype(i)
             w(i,1)=w(i,1)+zmelt(i)/( poros(ntyp)*zdepth(ntyp,1))
          END IF
       END DO
       DO i = 1, nmax
          !
          !     these lines exist to eliminate a cray compiler error
          !
          IF (iveg == 2) THEN
             IF (snow(i,2) > 0. .AND. tg(i) > 273.16) THEN
             END IF
             IF (capac(i,2) > 0. .AND. tg(i) > 273.16) THEN
             END IF
          END IF
          capac(i,iveg)=capac(i,iveg)+snow(i,iveg)
          snow (i,iveg)=0.0
          p0(i)=thru(i)
       END DO
    END DO
    !
    !     calculation of canopy and ground heat capacities.
    !     
    DO i = 1, nmax
       ntyp=itype(i)
       cc(i)=zlt2(i,1)*clai+capac(i,1)*cw
       spwet(i)=MIN( 0.05 , capac(i,2))*cw
       cg(i)=csoil(i)+spwet(i)
    END DO
  END SUBROUTINE interc



  ! stomat :performs stomatal resistance.



  SUBROUTINE stomat( &
       cosz  ,par   ,pd    ,rst   ,extk  ,vcover,itype ,nmax  ,ncols ,&
       zlt2  ,green2,chil2 ,rstpar2)
    !
    !
    !-----------------------------------------------------------------------
    !      input parameters
    !-----------------------------------------------------------------------
    !   cosz.............cosine of zenith angle
    !   extk(cg,vnt,bd)..extinction coefficient
    !   zlt   (cg).......leaf area index
    !   vcover(cg).......fraction of vegetation cover
    !   green (cg).......fraction of grenn leaves
    !   chil  (cg).......leaf orientation pameter
    !   rstpar(cg,3).....coefficints related to par influence on
    !                    stomatal resistance
    !   radn   (vnt,bd)..downward sw/lw radiation at the surface
    !   par   (cg).......par( photo-synthetic active radiation)
    !   pd    (cg).......ratio of par(beam) to par(beam+diffuse)
    !-----------------------------------------------------------------------
    !     output parameters
    !-----------------------------------------------------------------------
    !   rst(cg)..........stomatal reistance
    !-----------------------------------------------------------------------
    !   itype............Classe de textura do solo 
    !   nmax  
    !   pie..............Constante Pi=3.1415926e0   
    !   athird...........Constante athird=1.0e0 /3.0e0
    !   ncols............Numero de ponto por faixa de latitude  
    !   ityp.............numero das classes de solo 13   
    !   imon.............Numero maximo de meses no ano (12)  
    !   icg..............Parametros da vegetacao (icg=1 topo e icg=2 base)  
    !   iwv..............Compriment de onda iwv=1=visivel, iwv=2=infravermelho
    !                    proximo, iwv=3 infravermelho termal   
    !   ibd..............Estado da vegetacao ibd=1 verde / ibd=2 seco
    !-----------------------------------------------------------------------
    !
    INTEGER, INTENT(in   ) :: ncols 
    INTEGER, INTENT(in   ) :: nmax                   
    REAL   , INTENT(in   ) :: rstpar2(ncols,icg,iwv) 
    INTEGER, INTENT(in   ) :: itype (ncols)           
    REAL,    INTENT(in   ) :: vcover(ncols,icg)       
    REAL   , INTENT(in   ) :: zlt2    (ncols,icg)
    REAL   , INTENT(in   ) :: green2  (ncols,icg)
    REAL   , INTENT(in   ) :: chil2   (ncols,icg)

    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(in   ) :: par   (ncols,icg)       
    REAL,    INTENT(in   ) :: pd    (ncols,icg)       
    REAL,    INTENT(out  ) :: rst   (ncols,icg)       
    REAL,    INTENT(inout) :: extk  (ncols,icg,iwv,ibd)
    !
    !     this is for coupling with closure turbulence model
    !
    REAL,    INTENT(in   ) :: cosz  (ncols)           
    !
    REAL    :: f     (ncols)                          
    REAL    :: gamma (ncols)                          
    REAL    :: at    (ncols)                          
    REAL    :: power1(ncols)                          
    REAL    :: power2(ncols)                          
    REAL    :: aa    (ncols)                          
    REAL    :: bb    (ncols)                          
    REAL    :: zat   (ncols)                          
    REAL    :: zk    (ncols)                          
    REAL    :: ekat  (ncols)                          
    REAL    :: rho4  (ncols)                          
    REAL    :: avflux(ncols)                          
    !
    INTEGER :: i 
    INTEGER :: iveg 
    INTEGER :: irad 
    INTEGER :: ntyp
    REAL    :: fcon 
    REAL    :: xabc 
    REAL    :: xabd 
    REAL    :: ftemp
    !
    !     
    !     bounding of product of extinction coefficient and local l.a.i.
    !     
    DO i = 1, nmax
       f(i) = MAX( cosz(i), 0.01746 )
    END DO
    !
    DO iveg = 1, 2
       DO irad = 1, 2
          DO i = 1, nmax
             ntyp = itype(i)
             extk(i,iveg,1,irad)=min &
                  (extk(i,iveg,1,irad),150.0 / &
                  zlt2(i,iveg)*vcover(i,iveg))
          END DO
       END DO
    END DO
    !
    fcon  =0.25 *pie+athird
    iveg=1
    !
    DO i = 1, nmax
       ntyp=itype(i)
       IF (ntyp == 13 .OR. ntyp == 11) THEN
          rst(i,iveg) = 1.0e5
       ELSE
          at(i)=zlt2(i,iveg)/vcover(i,iveg)
          IF (par(i,iveg) <= 0.00101) THEN
             xabc = rstpar2(i,iveg,1) / rstpar2(i,iveg,2) + &
                  rstpar2(i,iveg,3)
             xabd = .5  / xabc * at(i)
             rst(i,iveg) = 1. / xabd
          ELSE
             gamma(i)  = (rstpar2(i,iveg,1) +rstpar2(i,iveg,2) &
                  * rstpar2(i,iveg,3))/rstpar2(i,iveg,3)
             !     
             !     single extinction coefficient using weighted
             !     values of direct and diffus contributions to p.a.r.
             !     
             at(i)     = zlt2(i,iveg)/vcover(i,iveg)
             power1(i) = at(i)*extk(i,iveg,1,1)
             power2(i) = at(i)*extk(i,iveg,1,2)
             aa(i)     = 0.5 -(0.633 +0.33 *chil2(i,iveg))*chil2(i,iveg)
             bb(i)     = 0.877 -1.754 *aa(i)
             zat(i)    = LOG(( EXP(-power1(i))+1.0 )*0.5 ) &
                  *          pd(i,iveg) /extk(i,iveg,1,1)
             zat(i)    = zat(i)+ LOG(( EXP(-power2(i))+1.0 )*0.5 ) &
                  * ( 1.0 -pd(i,iveg))/extk(i,iveg,1,2)
             zk(i)     = 1.0 /zat(i) &
                  * LOG(     pd(i,iveg) *EXP( power1(i)*zat(i)/at(i) ) &
                  + (1.0 -pd(i,iveg))*EXP( power2(i)*zat(i)/at(i) ))
             !     
             !     canopy and ground cover bulk resistances using
             !     ross-goudriaan leaf function , total par flux (avflux) and
             !     mean extinction coefficient (zk)
             !     
             ftemp    = MIN( zk(i)*at(i),20.0 )
             ekat (i) = EXP( ftemp )
             avflux(i)= par(i,iveg)*( pd(i,iveg)*( aa(i)/f(i)+bb(i)) &
                  + ( 1.0 -pd(i,iveg))*( bb(i)*fcon+aa(i)*1.5 ))
             rho4(i)  = gamma(i)/avflux(i)
             rst(i,iveg)=rstpar2(i,iveg,2) &
                  / gamma(i)*LOG((rho4(i)*ekat(i)+1.0 )/(rho4(i)+1.0 ))
             rst(i,iveg)=rst(i,iveg) &
                  - LOG((rho4(i)+1.0 /ekat(i))/(rho4(i)+1.0 ))
             rst(i,iveg)=rst(i,iveg)/(zk(i)*rstpar2(i,iveg,3))
             rst(i,iveg)=1.0 /( rst(i,iveg)*green2(i,iveg))
          END IF
       END IF
    END DO
    !
    DO i = 1, nmax
       rst(i,2) = 1.0e5
    END DO
    !
  END SUBROUTINE stomat



  ! raduse :performs the absorption of radiation by surface.



  SUBROUTINE raduse(radt  ,par   ,pd    ,radfac,closs ,gloss ,thermk,p1f   , &
       p2f   ,radn  ,vcover,nmax  ,ncols )
    !
    !-----------------------------------------------------------------------
    ! input parameters
    !-----------------------------------------------------------------------
    !   tf...............freezing temperature
    !   tg...............ground   temperature
    !   polar............
    !   radsav...........passesd from subr.radalb
    !   radfac(cg,vn,bd).fractions of downward solar radiation at surface
    !                    passed from subr.radalb
    !   radn(vnt,bd).....downward sw/lw radiation at the surface
    !   vcover(cg).......vegetation cover
    !-----------------------------------------------------------------------
    ! output parameters
    !-----------------------------------------------------------------------
    !   radt(cg).........net heat received by canopy/ground vegetation
    !                    by radiation & conduction
    !   par(cg)..........par incident on canopy
    !   pd(cg)...........ratio of par beam to total par
    !-----------------------------------------------------------------------
    !
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: nmax                    
    REAL,    INTENT(in   ) :: vcover(ncols,icg)       
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL,    INTENT(in   ) :: radn  (ncols,3,2)         
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(out  ) :: radt  (ncols,icg)       
    REAL,    INTENT(out  ) :: par   (ncols,icg)       
    REAL,    INTENT(out  ) :: pd    (ncols,icg)       
    REAL,    INTENT(in   ) :: radfac(ncols,icg,iwv,ibd)
    REAL,    INTENT(in   ) :: closs (ncols)           
    REAL,    INTENT(in   ) :: gloss (ncols)           
    REAL,    INTENT(in   ) :: thermk(ncols)           
    REAL,    INTENT(in   ) :: p1f   (ncols)           
    REAL,    INTENT(in   ) :: p2f   (ncols)           


    REAL :: p1 (ncols)                                
    REAL :: p2 (ncols)                                


    INTEGER :: i 
    INTEGER :: iveg 
    INTEGER :: iwave 
    INTEGER :: irad
    !     
    !     summation of radiation fractions for canopy and ground
    !     
    DO i = 1, nmax
       radt(i,1)=0.0
       radt(i,2)=0.0
    END DO

    DO iveg = 1, 2
       DO iwave = 1, 2
          DO irad = 1, 2
             DO i = 1, nmax
                radt(i,iveg)=radt(i,iveg) &
                     +radfac(i,iveg,iwave,irad)*radn(i,iwave,irad)
             END DO
          END DO
       END DO
    END DO
    !     
    !     total long wave ( and polar ice conduction ) adjustments to
    !     canopy and ground net radiation terms
    !     
    DO i = 1, nmax
       radt(i,1) = radt(i,1) &
            + radn(i,3,2) * vcover(i,1)*(1.0 -thermk(i)) &
            - closs(i)
       radt(i,2) = radt(i,2) &
            + radn(i,3,2)*(1.0 -vcover(i,1)*(1.0 -thermk(i))) &
            - gloss(i)
       par(i,1)  = radn(i,1,1)+radn(i,1,2)+0.001
       pd (i,1)  = (radn(i,1,1)+0.001 )/par(i,1)
       p1(i)     = p1f(i)*radn(i,1,1)+0.001
       p2(i)     = p2f(i)*radn(i,1,2)
       par(i,2)  = p1(i)+p2(i)
       IF (par(i,1) <= 0.000001) par(i,1) = 0.000001
       IF (par(i,2) <= 0.000001) par(i,2) = 0.000001
       pd (i,2)  = p1(i)/par(i,2)
    END DO
  END SUBROUTINE raduse



  ! root   :performs soil moisture potentials in root zone of each
  !         vegetation layer and summed soil+root resistance.



  SUBROUTINE root(phroot,phsoil,w     ,itype ,nmax  , &
       ncols )
    !
    ! input parameters
    !-----------------------------------------------------------------------
    !   w(1).............wetness of surface store
    !   w(2).............wetness of root zone
    !   w(3).............wetness of recharge zone
    !   phsat............soil moisture potential at saturation   (m)
    !   bee..............empirical constant
    !   zdepth(3)........depth of the i-th soil layer            (m)
    !   rootd (cg).......rooting depth                           (m)
    !   satco............mean soil hydraulic conductivity in the root zone
    !                                                            (m/s)
    !   rootl(cg)........root density                            (m/m**3)
    !   rootca(cg).......root cross section                      (m**2)
    !   rdres(cg)........resistance per unit root length         (s/m)
    !   rplant(cg).......area averaged resistance imposed by the plant
    !                    vascular system                         (s)
    !-----------------------------------------------------------------------
    ! output parameters
    !-----------------------------------------------------------------------
    !   vroot............root volume density                     (m**3/m**3)
    !-----------------------------------------------------------------------
    ! output parameters
    !-----------------------------------------------------------------------
    !   phsoil(3)........soil moisture potential of the i-th soil layer
    !                                                            (m)
    !   rootr(cg)........root resistance                         (s)
    !-----------------------------------------------------------------------
    !
    !   imax.............Numero de ponto por faixa de latitude  
    !   ityp.............numero das classes de solo 13 
    !   icg..............Parametros da vegetacao (icg=1 topo e icg=2 base)
    !   idp..............Camadas de solo (1 a 3)
    !   nmax.............
    !   itype............Classe de textura do solo
    !   phroot...........Soil moisture potentials in root zone of each
    !                    vegetation layer and summed soil+root resistance.
    !
    INTEGER, INTENT(in   ) :: ncols  
    INTEGER, INTENT(in   ) :: nmax               

    INTEGER, INTENT(in   ) :: itype (ncols)      
    !
    !     prognostic variables
    !     
    REAL,    INTENT(in   ) :: w    (ncols,3)     
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(out  ) :: phroot(ncols,icg)  
    REAL,    INTENT(out  ) :: phsoil(ncols,idp)  


    REAL     :: www   (ncols,3)                  
    INTEGER  :: i 
    INTEGER  :: n 
    INTEGER  :: ntyp

    DO i = 1, 3
       DO n = 1, nmax
          ntyp        = itype(n)
          www   (n,i) = MAX(0.10 ,w(n,i))
          phsoil(n,i) = phsat(ntyp)*EXP(-bee(ntyp)*LOG(www(n,i)))
       END DO
    END DO

    DO n = 1, nmax
       phroot(n,1)   = phsoil(n,1)-0.01
       DO i = 2, 3
          phroot(n,1) = MAX( phroot(n,1), phsoil(n,i))
       END DO
       phroot(n,2)   = phroot(n,1)
    END DO

  END SUBROUTINE root



  ! pbl    :performs planetary boundary layer parameterization. 



  SUBROUTINE pbl(jstneu, hgdtg , hgdtc , hgdtm , hcdtg , hcdtc , hcdtm , &
       egdtg , egdtc , egdqm , ecdtg , ecdtc , ecdqm , deadtg, &
       deadtc, deadqm,icheck , ect   , eci   , egt   , egi   , &
       egs   , ec    , eg    , hc    , hg    , ecidif, egidif, &
       ecmass, egmass, etmass, hflux , chf   , shf   , roff  , &
       bps   , psb   , dzm   , em    , gmt   , gmq   ,  cu   , &
       cuni  , ctni  , ustar , cosz  , rhoair, psy   , rcp   , &
       wc    , wg    , fc    , fg    , hr    , vcover, z0x   , &
       d     , rdc   , rbc   , z0    , qm    , tm    , um    , &
       vm    , psur  , ppc   , ppl   , radn  , ra    , rb    , &
       rd    , rc    , rg    , tcta  , tgta  , ta    , ea    , &
       etc   , etg   , btc   , btg   , u2    , radt  , par   , &
       pd    , rst   , rsoil , phroot,  hrr  , phsoil, cc    , &
       cg    , satcap, snow  , dtc   , dtg   , dtm   , dqm   , &
       stm   , extk  , radfac, closs , gloss , thermk, p1f   , &
       p2f   , tc    , tg    , td    , capac , w     , itype , &
       dtc3x , mon   , nmax  , ncols ,zlt2  ,green2,chil2,rstpar2,&
       topt2,tll2  ,tu2   , defac2,ph12  ,ph22, xcover)
    !
    ! jstneu......The first call to vntlat just gets the neutral values of ustar
    !              and ventmf para jstneu=.TRUE..
    ! hgdtg.......n.b. fluxes expressed in joules m-2 
    ! hgdtc.......n.b. fluxes expressed in joules m-2  
    ! hgdtm.......n.b. fluxes expressed in joules m-2
    ! hcdtg.......n.b. fluxes expressed in joules m-2  
    ! hcdtc.......n.b. fluxes expressed in joules m-2  
    ! hcdtm.......n.b. fluxes expressed in joules m-2 
    ! egdtg.......partial derivative calculation for latent heat 
    ! egdtc.......partial derivative calculation for latent heat 
    ! egdqm.......partial derivative calculation for latent heat  
    ! ecdtg.......partial derivative calculation for latent heat  
    ! ecdtc.......partial derivative calculation for latent heat 
    ! ecdqm.......partial derivative calculation for latent heat  
    ! deadtg
    ! deadtc
    ! deadqm
    ! icheck......this version assumes dew-free conditions "icheck=1" to 
    !              estimate ea for buoyancy term in vntmf or ra.
    ! ect.........Transpiracao no topo da copa (J/m*m)         
    ! eci.........Evaporacao da agua interceptada no topo da copa (J/m*m)       
    ! egt.........Transpiracao na base da copa (J/m*m)         
    ! egi.........Evaporacao da neve (J/m*m)         
    ! egs.........Evaporacao do solo arido (J/m*m)         
    ! ec..........Soma da Transpiracao e Evaporacao da agua interceptada pelo
    !              topo da copa   ec   (i)=eci(i)+ect(i)         
    ! eg..........Soma da transpiracao na base da copa +  Evaporacao do solo arido
    !              +  Evaporacao da neve  " eg   (i)=egt(i)+egs(i)+egi(i)"      
    ! hc..........total sensible heat lost of top from the veggies.        
    ! hg..........total sensible heat lost of base from the veggies.          
    ! ecidif......check if interception loss term has exceeded canopy storage
    !              ecidif(i)=MAX(0.0   , eci(i)-capac(i,1)*hlat3 )
    ! egidif......check if interception loss term has exceeded canopy storage
    !              ecidif(i)=MAX(0.0   , egi(i)-capac(i,1)*hlat3 )
    ! ecmass......Mass of water lost of top from the veggies.
    ! egmass......Mass of water lost of base from the veggies.
    ! etmass......total mass of water lost from the veggies.
    ! hflux.......total sensible heat lost from the veggies.  
    ! chf.........heat fluxes into the canopy  in w/m**2       
    ! shf.........heat fluxes into the ground, in w/m**2         
    ! roff........runoff  
    ! pie.........Constante Pi=3.1415926e0         
    ! stefan......Constante de Stefan Boltzmann
    ! cpair.......specific heat of air (j/kg/k)  
    ! hlat........heat of evaporation of water   (j/kg)    
    ! grav........gravity constant      (m/s**2)  
    ! snomel......heat of melting (j m-1)
    ! tf..........Temperatura de congelamento (K)          
    ! clai........heat capacity of foliage  
    ! cw..........liquid water heat capacity               (j/m**3)       
    ! gasr........Constant of dry air      (j/kg/k)    
    ! epsfac......Constante 0.622 Razao entre as massas moleculares do vapor
    !              de agua e do ar seco
    ! athird......Constante athird=1.0e0 /3.0e0
    ! bps   
    ! psb   
    ! dzm.........Altura media de referencia  para o vento para o calculo
    !               da estabilidade do escoamento   
    ! em..........Pressao de vapor da agua          
    ! gmt(i,k,3)..virtual temperature tendency due to vertical diffusion
    ! gmq.........specific humidity of reference (fourier)    
    ! cu..........Friction  transfer coefficients.      
    ! cuni........neutral friction transfer  coefficients.  
    ! ctni........neutral heat transfer coefficients.    
    ! ustar.......surface friction velocity  (m/s)  
    ! cosz........cosine of zenith angle  
    ! rhoair......Desnsidade do ar
    ! psy ........(cp/(hl*epsfac))*psur(i)   
    ! rcp.........densidade do ar vezes o calor especifico do ar     
    ! wc..........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !              indice de area foliar no topo da copa            
    ! wg..........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !              indice de area foliar na base da copa           
    ! fc..........Condicao de oravalho 0 ou 1 na topo da copa        
    ! fg..........Condicao de oravalho 0 ou 1 na base da copa           
    ! hr..........rel. humidity in top layer           
    ! vcover(iv)..Fracao de cobertura de vegetacao iv=1 Top   
    ! vcover(iv)..Fracao de cobertura de vegetacao iv=2 Botto
    ! z0x.........roughness length       
    ! d...........Displacement height            
    ! rdc.........Constant related to aerodynamic resistance
    !              between ground and canopy air space   
    ! rbc.........Constant related to bulk boundary layer resistance     
    ! z0..........Roughness length     
    ! qm..........reference specific humidity (fourier)      
    ! tm .........reference temperature    (fourier)                (k)      
    ! um..........Razao entre zonal pseudo-wind (fourier) e seno da
    !              colatitude        
    ! vm..........Razao entre meridional pseudo-wind (fourier) e seno da
    !              colatitude          
    ! psur........surface pressure in mb  
    ! ppc.........precipitation rate ( cumulus )           (mm/s)   
    ! ppl.........precipitation rate ( large scale )       (mm/s)   
    ! radn........downward sw/lw radiation at the surface  
    ! ra..........Resistencia Aerodinamica (s/m)          
    ! rb..........bulk boundary layer resistance             (s/m)          
    ! rd..........aerodynamic resistance between ground      (s/m)
    !              and canopy air space          
    ! rc..........Resistencia do topo da copa            
    ! rg......... Resistencia da base da copa               
    ! tcta........Diferenca entre tc-ta                      (k)   
    ! tgta........Diferenca entre tg-ta                      (k)    
    ! ta..........Temperatura no nivel de fonte de calor do dossel (K)        
    ! ea..........Pressure of vapor         
    ! etc.........Pressure of vapor at top of the copa      
    ! etg.........Pressao de vapor no base da copa      
    ! btc.........btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i)).   
    ! btg.........btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i))      
    ! u2..........wind speed at top of canopy                (m/s)           
    ! radt........net heat received by canopy/ground vegetation      
    ! par.........par incident on canopy   
    ! pd..........ratio of par beam to total par      
    ! rst ........Resisttencia Estomatica "Stomatal resistence" (s/m)   
    ! rsoil ......Resistencia do solo (s/m)  
    ! phroot......Soil moisture potentials in root zone of each
    !                    vegetation layer and summed soil+root resistance.
    ! hrr.........rel. humidity in top layer   
    ! phsoil......soil moisture potential of the i-th soil layer
    ! cc..........heat capacity of the canopy          
    ! cg..........heat capacity of the ground         
    ! satcap......saturation liquid water capacity         (m)
    ! snow........snow amount  
    ! dtc.........dtc(i)=pblsib(i,2,5)*dtc3x     
    ! dtg.........dtg(i)=pblsib(i,1,5)*dtc3x     
    ! dtm.........dtm(i)=pblsib(i,3,5)*dtc3x     
    ! dqm ........dqm(i)=pblsib(i,4,5)*dtc3x    
    ! stm ........Variavel utilizada mo cal. da Resisttencia     
    ! extk........extinction coefficient  
    ! radfac......fractions of downward solar radiation at surface
    !             passed from subr.radalb
    ! closs.......radiation loss from canopy  
    ! gloss.......radiation loss from ground  
    ! thermk......canopy emissivity
    ! p1f  
    ! p2f  
    ! tc..........Temperatura da copa "dossel"(K)         
    ! tg..........Temperatura da superficie do solo (K)         
    ! td..........Temperatura do solo profundo (K)         
    ! capac(iv)...Agua interceptada iv=1 no dossel "water store capacity 
    !             of leaves"(m) 
    ! capac(iv)...Agua interceptada iv=2 na cobertura do solo (m) 
    ! w(id).......Grau de saturacao de umidade do solo id=1 na camada superficial
    ! w(id).......Grau de saturacao de umidade do solo id=2 na camada de raizes
    ! w(id).......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    ! itype ......Classe de textura do solo
    ! rstpar(cg,3).coefficints related to par influence on
    !                    stomatal resistance
    ! chil........leaf orientation pameter  
    ! topt........Temperatura ideal de funcionamento estomatico    
    ! tll.........Temperatura minima de funcionamento estomatico     
    ! tu..........Temperatura maxima de funcionamento estomatico      
    ! defac.......Parametro de deficit de pressao de vapor d'agua  
    ! ph1.........Coeficiente para o efeito da agua no solo     
    ! ph2.........Potencial de agua no solo para ponto de Wilting   
    ! rootd.......Profundidade das raizes 
    ! bee.........Expoente da curva de retencao "expoente para o solo umido"   
    ! phsat.......Tensao do solo saturado " Potencial de agua no solo saturado"  
    ! satco.......mean soil hydraulic conductivity in the root zone 
    ! poros.......porosity 
    ! zdepth......Profundidade para as tres camadas de solo
    ! green.......fraction of grenn leaves
    ! xcover(iv)..Fracao de cobertura de vegetacao iv=1 Top 
    ! xcover(iv)..Fracao de cobertura de vegetacao iv=2 Bottom 
    ! zlt(icg)....Indice de area foliar "LEAF AREA INDEX" icg=1 topo da copa  
    ! zlt(icg)....Indice de area foliar "LEAF AREA INDEX" icg=2 base da copa 
    ! x0x.........Comprimento de rugosidade     
    ! xd..........Deslocamento do plano zero      
    ! z2..........Altura do topo do dossel          
    ! xdc.........Constant related to aerodynamic resistance
    !             between ground and canopy air space  
    ! xbc.........Constant related to bulk boundary layer resistance 
    ! dtc3x.......time increment dt  
    ! mon.........Number of month at year (1-12)      
    ! nmax  
    ! ityp........numero das classes de solo 13   
    ! imon........Numero maximo de meses no ano (12)  
    ! icg.........Parametros da vegetacao (icg=1 topo e icg=2 base)  
    ! iwv.........Compriment de onda iwv=1=visivel, iwv=2=infravermelho
    !             proximo, iwv=3 infravermelho termal  
    ! idp.........Camadas de solo (1 a 3)  
    ! ibd.........Estado da vegetacao ibd=1 verde / ibd=2 seco  
    ! ncols.......Numero de ponto por faixa de latitude    
    !
    !
    INTEGER, INTENT(in   ) :: ncols

    REAL   , INTENT(in   ) :: dtc3x                     
    INTEGER, INTENT(in   ) :: mon(ncols)                
    INTEGER, INTENT(in   ) :: nmax                      

    INTEGER, INTENT(in   ) :: itype(ncols)              
    !
    !     prognostic variables
    !     
    REAL,    INTENT(inout) :: tc   (ncols)              
    REAL,    INTENT(inout) :: tg   (ncols)              
    REAL,    INTENT(inout) :: td   (ncols)              
    REAL,    INTENT(inout) :: capac(ncols,2)            
    REAL,    INTENT(inout) :: w    (ncols,3)            
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(inout) :: ra    (ncols)             
    REAL,    INTENT(out  ) :: rb    (ncols)             
    REAL,    INTENT(out  ) :: rd    (ncols)             
    REAL,    INTENT(out  ) :: rc    (ncols)             
    REAL,    INTENT(out  ) :: rg    (ncols)             
    REAL,    INTENT(out  ) :: tcta  (ncols)             
    REAL,    INTENT(out  ) :: tgta  (ncols)             
    REAL,    INTENT(out  ) :: ta    (ncols)             
    REAL,    INTENT(out  ) :: ea    (ncols)             
    REAL,    INTENT(out  ) :: etc   (ncols)             
    REAL,    INTENT(out  ) :: etg   (ncols)             
    REAL,    INTENT(out  ) :: btc   (ncols)             
    REAL,    INTENT(out  ) :: btg   (ncols)             
    REAL,    INTENT(inout) :: u2    (ncols)             
    REAL,    INTENT(out  ) :: radt  (ncols,icg)         
    REAL,    INTENT(inout) :: par   (ncols,icg)         
    REAL,    INTENT(out  ) :: pd    (ncols,icg)         
    REAL,    INTENT(out  ) :: rst   (ncols,icg)         
    REAL,    INTENT(out  ) :: rsoil (ncols)             
    REAL,    INTENT(out  ) :: phroot(ncols,icg)         
    REAL,    INTENT(out  ) :: hrr   (ncols)             
    REAL,    INTENT(out  ) :: phsoil(ncols,idp)         
    REAL,    INTENT(out  ) :: cc    (ncols)             
    REAL,    INTENT(out  ) :: cg    (ncols)             
    REAL,    INTENT(inout) :: satcap(ncols,icg)         
    REAL,    INTENT(inout) :: snow  (ncols,icg)         
    REAL,    INTENT(out  ) :: dtc   (ncols)             
    REAL,    INTENT(out  ) :: dtg   (ncols)             
    REAL,    INTENT(out  ) :: dtm   (ncols)             
    REAL,    INTENT(out  ) :: dqm   (ncols)             
    REAL,    INTENT(out  ) :: stm   (ncols,icg)         
    REAL,    INTENT(inout) :: extk  (ncols,icg,iwv,ibd) 
    REAL,    INTENT(in   ) :: radfac(ncols,icg,iwv,ibd) 
    REAL,    INTENT(in   ) :: closs (ncols)             
    REAL,    INTENT(in   ) :: gloss (ncols)             
    REAL,    INTENT(inout) :: thermk(ncols)             
    REAL,    INTENT(in   ) :: p1f   (ncols)             
    REAL,    INTENT(in   ) :: p2f   (ncols)             
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL,    INTENT(inout) :: qm  (ncols)                  
    REAL,    INTENT(inout) :: tm  (ncols)                  
    REAL,    INTENT(in   ) :: um  (ncols)                  
    REAL,    INTENT(in   ) :: vm  (ncols)                  
    REAL,    INTENT(inout) :: psur(ncols)                  
    REAL,    INTENT(in   ) :: ppc (ncols)                  
    REAL,    INTENT(in   ) :: ppl (ncols)                  
    REAL,    INTENT(in   ) :: radn(ncols,3,2)             

    REAL   , INTENT(in   ) :: zlt2    (ncols,icg)
    REAL   , INTENT(in   ) :: green2  (ncols,icg)
    REAL   , INTENT(in   ) :: chil2   (ncols,icg)
    REAL   , INTENT(in   ) :: rstpar2(ncols,icg,iwv)       
    REAL,    INTENT(inout) :: vcover  (ncols,icg)         
    REAL,    INTENT(inout) :: z0x(ncols)                  
    REAL,    INTENT(inout) :: d  (ncols)                  
    REAL,    INTENT(inout) :: rdc(ncols)                  
    REAL,    INTENT(inout) :: rbc(ncols)                  
    REAL,    INTENT(inout) :: z0 (ncols)                    
    REAL   , INTENT(in   ) :: topt2   (ncols,icg)
    REAL   , INTENT(in   ) :: tll2    (ncols,icg)
    REAL   , INTENT(in   ) :: tu2     (ncols,icg)
    REAL   , INTENT(in   ) :: defac2  (ncols,icg)
    REAL   , INTENT(in   ) :: ph12    (ncols,icg)
    REAL   , INTENT(in   ) :: ph22    (ncols,icg)    
    !
    !     this is for coupling with closure turbulence model
    !
    REAL,    INTENT(in   ) :: bps   (ncols)             
    REAL,    INTENT(in   ) :: psb   (ncols)             
    REAL,    INTENT(in   ) :: dzm   (ncols)             
    REAL,    INTENT(in   ) :: em    (ncols)             
    REAL,    INTENT(inout) :: gmt   (ncols,3)           
    REAL,    INTENT(inout) :: gmq   (ncols,3)           
    REAL,    INTENT(inout) :: cu    (ncols)             
    REAL,    INTENT(inout) :: cuni  (ncols)             
    REAL,    INTENT(inout) :: ctni  (ncols)             
    REAL,    INTENT(inout) :: ustar (ncols)             
    REAL,    INTENT(in   ) :: cosz  (ncols)             
    REAL,    INTENT(in   ) :: rhoair(ncols)             
    REAL,    INTENT(in   ) :: psy   (ncols)             
    REAL,    INTENT(out  ) :: rcp   (ncols)             
    REAL,    INTENT(inout) :: wc    (ncols)             
    REAL,    INTENT(inout) :: wg    (ncols)             
    REAL,    INTENT(inout) :: fc    (ncols)             
    REAL,    INTENT(inout) :: fg    (ncols)             
    REAL,    INTENT(inout) :: hr    (ncols)             

    REAL,    INTENT(inout) :: ect   (ncols)             
    REAL,    INTENT(inout) :: eci   (ncols)             
    REAL,    INTENT(inout) :: egt   (ncols)             
    REAL,    INTENT(inout) :: egi   (ncols)             
    REAL,    INTENT(inout) :: egs   (ncols)             
    REAL,    INTENT(out  ) :: ec    (ncols)             
    REAL,    INTENT(out  ) :: eg    (ncols)             
    REAL,    INTENT(out  ) :: hc    (ncols)             
    REAL,    INTENT(out  ) :: hg    (ncols)             
    REAL,    INTENT(out  ) :: ecidif(ncols)             
    REAL,    INTENT(out  ) :: egidif(ncols)             
    REAL,    INTENT(out  ) :: ecmass(ncols)             
    REAL,    INTENT(out  ) :: egmass(ncols)             
    REAL,    INTENT(out  ) :: etmass(ncols)             
    REAL,    INTENT(out  ) :: hflux (ncols)             
    REAL,    INTENT(out  ) :: chf   (ncols)             
    REAL,    INTENT(out  ) :: shf   (ncols)             
    REAL,    INTENT(inout) :: roff  (ncols)             

    INTEGER, INTENT(out  ) :: icheck(ncols)             
    !
    !     derivatives
    !
    REAL,    INTENT(out  ) :: hgdtg (ncols)             
    REAL,    INTENT(out  ) :: hgdtc (ncols)             
    REAL,    INTENT(out  ) :: hgdtm (ncols)            
    REAL,    INTENT(out  ) :: hcdtg (ncols)            
    REAL,    INTENT(out  ) :: hcdtc (ncols)            
    REAL,    INTENT(out  ) :: hcdtm (ncols)            
    REAL,    INTENT(out  ) :: egdtg (ncols)            
    REAL,    INTENT(out  ) :: egdtc (ncols)            
    REAL,    INTENT(out  ) :: egdqm (ncols)            
    REAL,    INTENT(out  ) :: ecdtg (ncols)            
    REAL,    INTENT(out  ) :: ecdtc (ncols)            
    REAL,    INTENT(out  ) :: ecdqm (ncols)            
    REAL,    INTENT(out  ) :: deadtg(ncols)             
    REAL,    INTENT(out  ) :: deadtc(ncols)             
    REAL,    INTENT(out  ) :: deadqm(ncols)             

    LOGICAL, INTENT(out  ) :: jstneu                    

    REAL   , INTENT(IN   ) :: xcover(ityp,imon,icg)
    !
    CALL root(phroot,phsoil,w     ,itype ,nmax  ,ncols  )

    CALL raduse(radt  ,par   ,pd    ,radfac,closs ,gloss ,thermk,p1f   , &
         p2f   ,radn  ,vcover,nmax  ,ncols   ) 

    CALL stomat(cosz  ,par   ,pd    ,rst   ,extk  ,vcover,itype , &
         nmax  ,ncols ,zlt2  ,green2,chil2 ,rstpar2)

    CALL interc( &
         roff  ,cc    ,cg    ,satcap,snow  ,extk  ,tc    ,tg    ,td    , &
         capac ,w     ,tm    ,ppc   ,ppl   ,vcover,itype ,dtc3x , &
       nmax  ,ncols ,zlt2 )
    !
    !     surface flux
    !     
    CALL sflxes( &
         hgdtg ,hgdtc ,hgdtm ,hcdtg ,hcdtc ,hcdtm ,egdtg ,egdtc ,egdqm , &
         ecdtg ,ecdtc ,ecdqm ,deadtg,deadtc,deadqm,icheck,bps   ,psb   , &
         dzm   ,em    ,gmt   ,gmq   ,cu    ,cuni  ,ctni  ,ustar ,rhoair, &
         psy   ,rcp   ,wc    ,wg    ,fc    ,fg    ,hr    ,ect   ,eci   , &
         egt   ,egi   ,egs   ,ec    ,eg    ,hc    ,hg    ,ecidif,egidif, &
         ecmass,egmass,etmass,hflux ,chf   ,shf   ,ra    ,rb    ,rd    , &
         rc    ,rg    ,tcta  ,tgta  ,ta    ,ea    ,etc   ,etg   ,btc   , &
         btg   ,u2    ,radt  ,rst   ,rsoil ,hrr   ,phsoil,cc    ,cg    , &
         satcap,dtc   ,dtg   ,dtm   ,dqm   ,stm   ,thermk,tc    ,tg    , &
         td    ,capac ,w     ,qm    ,tm    ,um    ,vm    ,psur  ,vcover, &
         z0x   ,d     ,rdc   ,rbc   ,z0    ,itype ,dtc3x ,mon   ,nmax  , &
       jstneu,ncols ,zlt2  ,topt2 ,tll2  ,tu2   , defac2,ph12  ,ph22 , &
         xcover)
  END SUBROUTINE pbl






  SUBROUTINE snowm(&
       chf   ,shf   ,fluxef,roff  ,cc    ,cg    ,snow  ,dtc   ,dtg   , &
       tc    ,tg    ,td    ,capac ,w     ,itype ,dtc3x ,nmax  ,ncols  )
    !
    ! snowm  :calculates snowmelt and modification of temperatures;
    !         this version deals with refreezing of water;
    !         version modified to use force-restore heat fluxes.
    !
    !-----------------------------------------------------------------------
    ! chf.........Fluxo de calor na copa (J/m*m)
    ! shf.........Fluxo de calor no solo (J/m*m)
    ! fluxef......modified to use force-restore heat fluxes 
    !             fluxef(i) = shf(i) - cg(i)*dtg(i)*dtc3xi " Garrat pg. 227"
    ! roff........runoff (escoamente superficial e drenagem)(m)
    ! cc..........heat capacity of the canopy    
    ! cg..........heat capacity of the ground     
    ! snow........snow amount  
    ! dtc ........dtc(i)=pblsib(i,2,5)*dtc3x     
    ! dtg ........dtg(i)=pblsib(i,1,5)*dtc3x     
    ! tc..........Temperatura da copa "dossel"(K)
    ! tg..........Temperatura da superficie do solo (K)
    ! td..........Temperatura do solo profundo (K)  
    ! capac(iv)...Agua interceptada iv=1 no dossel (m)
    ! capac(iv)...Agua interceptada iv=2 na cobertura do solo (m)
    ! w(id).......Grau de saturacao de umidade do solo id=1 na camada superficial
    ! w(id).......Grau de saturacao de umidade do solo id=2 na camada de raizes
    ! w(id).......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    ! poros.......Porosidade do solo (m"3/m"3)
    ! zdepth(id)..Profundidade das camadas de solo id=1 superficial
    ! zdepth(id)..Profundidade das camadas de solo id=2 camada de raizes
    ! zdepth(id)..Profundidade das camadas de solo id=3 camada de drenagem
    ! itype.......Classe de textura do solo
    ! ncols.......Numero de ponto por faixa de latitude
    ! ityp........13 
    ! icg.........Parametros da vegetacao (icg=1 topo e icg=2 base)
    ! idp.........Camadas de solo (1 a 3)
    ! snomel......Calor latente de fusao(J/kg)
    ! tf..........Temperatura de congelamento (K)
    ! dtc3x.......time increment dt  
    ! nmax........  
    !-----------------------------------------------------------------------
    INTEGER, INTENT(in   ) :: ncols 
    REAL,    INTENT(in   ) :: dtc3x             
    INTEGER, INTENT(in   ) :: nmax              

    INTEGER, INTENT(in   ) :: itype (ncols)     
    !
    !     prognostic variables
    !     
    REAL,    INTENT(inout) :: tc   (ncols)      
    REAL,    INTENT(inout) :: tg   (ncols)      
    REAL,    INTENT(in   ) :: td   (ncols)      
    REAL,    INTENT(inout) :: capac(ncols,2)    
    REAL,    INTENT(inout) :: w    (ncols,3)    
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(in   ) :: cc    (ncols)     
    REAL,    INTENT(in   ) :: cg    (ncols)     
    REAL,    INTENT(inout) :: snow  (ncols,icg) 
    REAL,    INTENT(in   ) :: dtc   (ncols)     
    REAL,    INTENT(in   ) :: dtg   (ncols)     
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL,    INTENT(in   ) :: chf   (ncols)     
    REAL,    INTENT(in   ) :: shf   (ncols)     
    REAL,    INTENT(out  ) :: fluxef(ncols)     
    REAL,    INTENT(inout) :: roff  (ncols)     

    REAL    :: cct   (ncols)                    
    REAL    :: ts         (ncols)                    
    REAL    :: dts   (ncols)                    
    REAL    :: flux  (ncols)                    
    REAL    :: tta   (ncols)                    
    REAL    :: ttb   (ncols)                    
    REAL    :: dtf   (ncols)                    
    REAL    :: work  (ncols)                    
    REAL    :: hf         (ncols)                    
    REAL    :: fcap  (ncols)                    
    REAL    :: spwet (ncols)                    
    REAL    :: dtf2  (ncols)                    
    REAL    :: tn         (ncols)                    
    REAL    :: change(ncols)                    
    REAL    :: dtime1(ncols)                    
    REAL    :: dtime2(ncols)                    

    INTEGER :: i 
    INTEGER :: iveg 
    INTEGER :: ntyp
    REAL    :: dtc3xi


    cct=0.0
    dtc3xi=1.0 /dtc3x

    DO iveg = 1, 2
       IF (iveg == 1) THEN

          DO i = 1, nmax
             cct (i)=cc (i)
             ts  (i)=tc (i)
             dts (i)=dtc(i)
             flux(i)=chf(i)
          END DO

       ELSE

          DO i = 1, nmax
             cct (i)=cg (i)
             ts  (i)=tg (i)
             dts (i)=dtg(i)
             flux(i)=cct(i)*dtg(i)*dtc3xi
          END DO

       END IF

       DO i = 1, nmax
          tta(i) = ts(i) - dts(i)
          ttb(i) = ts(i)
       END DO

       DO i = 1, nmax
          IF (tta(i) <= tf) THEN
             snow (i,iveg) = capac(i,iveg)
             capac(i,iveg) = 0.0
          ELSE
             snow (i,iveg) = 0.0
          END IF
       END DO

       DO i = 1, nmax
          work(i)=(tta(i)-tf)*(ttb(i)-tf)
       END DO

       DO i = 1, nmax
          IF (work(i) < 0.0) THEN
             ntyp=itype(i)
             dtf   (i)= tf - tta(i)
             dtime1(i)= cct (i)* dtf(i)/ flux(i)
             hf    (i)= flux(i)*(dtc3x-dtime1(i))
             spwet (i)=  MIN ( 5.0 , snow(i,iveg) )
             IF (dts(i) <= 0.0) THEN
                fcap (i) =-capac(i,iveg)* snomel
             ELSE
                fcap (i) = spwet(i)     * snomel
             END IF
             dtime2(i)= fcap(i) / flux(i)
             dtf2  (i)= flux(i) * (dtc3x-dtime1(i)-dtime2(i))/cct(i)
             tn(i)    = tf + dtf2(i)
             IF (ABS(hf(i)) < ABS(fcap(i))) THEN
                ts(i)    = tf -0.1
             ELSE
                ts(i)    = tn(i)
             END IF
             IF (ABS(hf(i)) < ABS(fcap(i))) THEN
                change(i) = hf  (i)
             ELSE
                change(i) = fcap(i)
             END IF
             change(i)     =change(i)      / snomel
             snow  (i,iveg)=snow  (i,iveg) - change(i)
             capac (i,iveg)=capac (i,iveg) + change(i)
             IF (snow(i,iveg) < 1.e-10) snow(i,iveg)=0.0e0
             IF (iveg == 1)THEN
                tc(i)=ts(i)
             ELSE
                tg(i)=ts(i)
             END IF
             IF (iveg == 2) THEN
                IF (td(i) > tf) THEN
                   w (i,1)=w (i,1)+capac(i,iveg) &
                        /(poros(ntyp)*zdepth(ntyp,1))
                ELSE
                   roff(i)=roff(i)+capac(i,iveg)
                END IF
                capac(i,iveg) = 0.0
             END IF
          END IF
       END DO
       DO i = 1, nmax
          capac(i,iveg) =  capac(i,iveg) + snow(i,iveg)
       END DO
    END DO

    !   modified to use force-restore heat fluxes

    DO i = 1, nmax
       fluxef(i) = shf(i) - cg(i)*dtg(i)*dtc3xi
    END DO

  END SUBROUTINE snowm




  ! fysiks :it is a physics driver; performs the following:
  !         a) soil water budget prior to calling pbl
  !         b) planetary boundary layer (pbl) parameterization
  !         c) update sib variables
  !         d) dumping of small capac values onto soil surface store
  !         e) snowmelt/refreeze calculation
  !         f) update deep soil temperature using effective soil heat flux
  !         g) bare soil evaporation loss
  !         h) extraction of transpiration loss from root zone
  !         i) interflow, infiltration excess and loss to groundwater
  !         j) increment prognostic variables and
  !            adjust theta and sh to be consistent with dew formation
  !         k) calculates soil water budget after calling pbl
  !            and compares with previous budget.


  SUBROUTINE fysiks(vcover, z0x  , d    , rdc  , rbc  , z0   ,ndt   , &
       latitu, nfprt, bps  ,psb   ,dzm   ,em    ,gmt   ,gmq   , &
       gmu   ,cu    , cuni ,ctni  ,ustar ,cosz  ,sinclt,rhoair, &
       psy   ,rcp   , wc   ,wg    ,fc    ,fg    ,hr    , ect  , &
       eci   , egt  , egi  , egs  , ec   , eg   , hc   , hg   , &
       ecidif,egidif,ecmass,egmass,etmass,hflux , chf  , shf  , &
       fluxef, roff , drag ,ra    , rb   , rd   , rc   , rg   , &
       tcta  , tgta , ta   , ea   , etc  , etg  , btc  , btg  , &
       u2    , radt , par  , pd   , rst  ,rsoil ,phroot, hrr  , &
       phsoil, cc   , cg   ,satcap, snow , dtc  , dtg  , dtm  , &
       dqm   , stm  , extk ,radfac, closs,gloss ,thermk, p1f  , &
       p2f   , tc   , tg   , td   , capac, w    ,  qm  , tm   , &
       um    , vm   , psur , ppc  , ppl  , radn ,itype ,dtc3x , &
       mon   , nmax , ncols,zlt2  ,green2,chil2 ,rstpar2,topt2, &
       tll2  ,tu2   , defac2,ph12  ,ph22, xcover,zlt)
    !     
    !
    !-----------------------------------------------------------------------
    !
    !  roff.......Runoff (escoamente superficial e drenagem)(m)
    !  slope......Inclinacao de perda hidraulica na camada profunda do solo
    !  bee........Fator de retencao da umidade no solo (expoente da umidade do 
    !             solo)    
    !  satco......Condutividade hidraulica do solo saturado(m/s)
    !  zdepth(id).Profundidade das camadas de solo id=1 superficial
    !  zdepth(id).Profundidade das camadas de solo id=2 camada de raizes
    !  zdepth(id).Profundidade das camadas de solo id=3 camada de drenagem
    !  phsat......Potencial matricial do solo saturado(m) (tensao do solo em
    !             saturacao)
    !  poros......Porosidade do solo
    !  dtc3x......time increment dt    
    !  snomel.....Calor latente de fusao(J/kg)
    !  w(id)......Grau de saturacao de umidade do solo id=1 na camada superficial
    !  w(id)......Grau de saturacao de umidade do solo id=2 na camada de raizes
    !  w(id)......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    !  capac(iv)..Agua interceptada iv=1 no dossel (m)
    !  capac(iv)..Agua interceptada iv=2 na cobertura do solo (m)
    !  tg.........Temperatura da superficie do solo  (K)
    !  td.........Temperatura do solo profundo (K)
    !  itype......Classe de textura do solo
    !  tf.........Temperatura de congelamento (K)
    !  idp........Parametro para as camadas de solo idp=1->3
    !  nmax.......
    !  ncols......Number of grid points on a gaussian latitude circle
    !  ityp.......Numero das classes de solo 13 
    !  imon.......Numero maximo de meses no ano (12)
    !  icg........Parametros da vegetacao (icg=1 topo e icg=2 base)
    !  iwv........Compriment de onda iwv=1=visivel, iwv=2=infravermelho
    !             proximo, iwv=3 infravermelho termal
    !  idp........Camadas de solo (1 a 3) 
    !  ibd........Estado da vegetacao ibd=1 verde / ibd=2 seco 
    !  pie........Constante Pi=3.1415926e0       
    !  stefan.....Constante de Stefan Boltzmann  
    !  cp.........specific heat of air (j/kg/k)     
    !  hl ........heat of evaporation of water   (j/kg)    
    !  grav.......gravity constant      (m/s**2)      
    !  snomel.....heat of melting (j m-1)  
    !  tf.........Temperatura de congelamento (K)       
    !  clai.......heat capacity of foliage      
    !  cw.........liquid water heat capacity               (j/m**3)      
    !  gasr.......Constant of dry air      (j/kg/k)        
    !  epsfac.....Constante 0.622 Razao entre as massas moleculares do vapor
    !             de agua e do ar seco  
    !  athird.....Constante athird=1.0e0 /3.0e0  
    !  dtc3x......time increment dt     
    !  mon........Number of month at year (1-12)        
    !  nmax    
    !  rstpar.....Coefficints related to par influence on
    !             stomatal resistance
    !  chil.......Leaf orientation parameter    
    !  topt.......Temperatura ideal de funcionamento estomatico      
    !  tll........Temperatura minima de funcionamento estomatico    
    !  tu.........Temperatura maxima de funcionamento estomatico          
    !  defac......Parametro de deficit de pressao de vapor d'agua  
    !  ph1........Coeficiente para o efeito da agua no solo        
    !  ph2........Potencial de agua no solo para ponto de Wilting    
    !  rootd......Profundidade das raizes  
    !  bee........Expoente da curva de retencao "expoente para o solo umido"     
    !  phsat......Tensao do solo saturado " Potencial de agua no solo saturado"   
    !  satco......mean soil hydraulic conductivity in the root zone  
    !  poros......Porosity  
    !  zdepth.....Profundidade para as tres camadas de solo
    !  green......Fraction of grenn leaves 
    !  xcover(iv).Fracao de cobertura de vegetacao iv=1 Top 
    !  xcover(iv).Fracao de cobertura de vegetacao iv=2 Bottom   
    !  zlt(icg)...Indice de area foliar "LEAF AREA INDEX" icg=1 topo da copa  
    !  zlt(icg)...Indice de area foliar "LEAF AREA INDEX" icg=2 base da copa 
    !  x0x........Comprimento de rugosidade        
    !  xd.........Deslocamento do plano zero          
    !  z2.........Altura do topo do dossel    
    !  xdc........Constant related to aerodynamic resistance
    !             between ground and canopy air space     
    !  xbc........Constant related to bulk boundary layer resistance   
    !  itype......Classe de textura do solo 
    !  qm.........Reference specific humidity (fourier)        
    !  tm.........Reference temperature    (fourier)                (k)    
    !  um.........Razao entre zonal pseudo-wind (fourier) e seno da
    !             colatitude  
    !  vm.........Razao entre meridional pseudo-wind (fourier) e seno da
    !             colatitude       
    !  psur.......Surface pressure in mb  
    !  ppc........Precipitation rate ( cumulus )           (mm/s)  
    !  ppl........Precipitation rate ( large scale )       (mm/s) 
    !  radn.......Downward sw/lw radiation at the surface  
    !  tc.........Temperatura da copa "dossel"(K)     
    !  tg.........Temperatura da superficie do solo (K)    
    !  td.........Temperatura do solo profundo (K)  
    !  capac(iv)..Agua interceptada iv=1 no dossel "water store capacity 
    !             of leaves"(m) 
    !  capac(iv)..Agua interceptada iv=2 na cobertura do solo (m)   
    !  w(id)......Grau de saturacao de umidade do solo id=1 na camada superficial
    !  w(id)......Grau de saturacao de umidade do solo id=2 na camada de raizes
    !  w(id)......Grau de saturacao de umidade do solo id=3 na camada de drenagem
    !  ra.........Resistencia Aerodinamica (s/m)            
    !  rb.........bulk boundary layer resistance                
    !  rd.........Aerodynamic resistance between ground      (s/m)
    !             and canopy air space             
    !  rc.........Resistencia do topo da copa        
    !  rg.........Resistencia da base da copa          
    !  tcta.......Diferenca entre tc-ta                      (k)    
    !  tgta.......Diferenca entre tg-ta                      (k)     
    !  ta.........Temperatura no nivel de fonte de calor do dossel (K)   
    !  ea.........Pressure of vapor     
    !  etc........Pressure of vapor at top of the copa    
    !  etg........Pressao de vapor no base da copa   
    !  btc........btc(i)=EXP(30.25353  -5418.0  /tc(i))/(tc(i)*tc(i)).   
    !  btg........btg(i)=EXP(30.25353  -5418.0  /tg(i))/(tg(i)*tg(i))      
    !  u2.........wind speed at top of canopy    
    !  radt.......net heat received by canopy/ground vegetation   
    !  par........par incident on canopy     
    !  pd.........ratio of par beam to total par         
    !  rst .......Resisttencia Estomatica "Stomatal resistence" (s/m)  
    !  rsoil......Resistencia do solo (s/m)  
    !  phroot.....Soil moisture potentials in root zone of each
    !             vegetation layer and summed soil+root resistance.
    !  hrr........rel. humidity in top layer      
    !  phsoil.....soil moisture potential of the i-th soil layer
    !  cc.........heat capacity of the canopy              
    !  cg.........heat capacity of the ground             
    !  satcap.....saturation liquid water capacity         (m)
    !  snow.......snow amount    
    !  dtc........dtc(i)=pblsib(i,2,5)*dtc3x     
    !  dtg........dtg(i)=pblsib(i,1,5)*dtc3x    
    !  dtm........dtm(i)=pblsib(i,3,5)*dtc3x   
    !  dqm .......dqm(i)=pblsib(i,4,5)*dtc3x       
    !  stm .......Variavel utilizada mo cal. da Resisttencia        
    !  extk.......extinction coefficient    
    !  radfac.....Fractions of downward solar radiation at surface
    !             passed from subr.radalb
    !  closs......Radiation loss from canopy   
    !  gloss......Radiation loss from ground  
    !  thermk.....Canopy emissivity
    !  p1f   
    !  p2f   
    !  ect........Transpiracao no topo da copa (J/m*m)   
    !  eci........Evaporacao da agua interceptada no topo da copa (J/m*m)   
    !  egt........Transpiracao na base da copa (J/m*m)   
    !  egi........Evaporacao da neve (J/m*m)    
    !  egs........Evaporacao do solo arido (J/m*m)   
    !  ec.........Soma da Transpiracao e Evaporacao da agua interceptada pelo
    !             topo da copa   ec   (i)=eci(i)+ect(i)    
    !  eg.........Soma da transpiracao na base da copa +  Evaporacao do solo arido
    !             +  Evaporacao da neve  " eg   (i)=egt(i)+egs(i)+egi(i)"    
    !  hc.........Total sensible heat lost of top from the veggies.            
    !  hg.........Total sensible heat lost of base from the veggies.      
    !  ecidif.....check if interception loss term has exceeded canopy storage
    !             ecidif(i)=MAX(0.0   , eci(i)-capac(i,1)*hlat3 )
    !  egidif.....check if interception loss term has exceeded canopy storage
    !             ecidif(i)=MAX(0.0   , egi(i)-capac(i,1)*hlat3 )
    !  ecmass.....Mass of water lost of top from the veggies.
    !  egmass.....Mass of water lost of base from the veggies.
    !  etmass.....Total mass of water lost from the veggies.
    !  hflux......Total sensible heat lost from the veggies 
    !  chf........Heat fluxes into the canopy  in w/m**2          
    !  shf........Heat fluxes into the ground, in w/m**2     
    !  fluxef.....Modified to use force-restore heat fluxes 
    !             fluxef(i) = shf(i) - cg(i)*dtg(i)*dtc3xi " Garrat pg. 227"
    !  roff.......runoff (escoamente superficial e drenagem)(m)  
    !  drag.......tensao superficial  
    !  bps   
    !  psb   
    !  dzm........Altura media de referencia  para o vento para o calculo
    !             da estabilidade do escoamento      
    !  em.........Pressao de vapor da agua    
    !  gmt(i,k,3).temperature related matrix virtual temperature tendency 
    !             due to vertical diffusion   
    !  gmq........specific humidity related matrix specific humidity of 
    !             reference (fourier)       
    !  gmu........wind related matrix   
    !  cu.........Friction  transfer coefficients.    
    !  cuni.......Neutral friction transfer  coefficients.    
    !  ctni.......Neutral heat transfer coefficients.   
    !  ustar......Surface friction velocity  (m/s)  
    !  cosz.......Cosine of zenith angle    
    !  sinclt.....sinclt=SIN(colrad(latco))"seno da colatitude"
    !  rhoair.....Desnsidade do ar
    !  psy........(cp/(hl*epsfac))*psur(i)      
    !  rcp........densidade do ar vezes o calor especifico do ar        
    !  wc.........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !             indice de area foliar no topo da copa    
    !  wg.........Minimo entre 1 e a razao entre a agua interceptada pelo 
    !             indice de area foliar na base da copa    
    !  fc.........Condicao de oravalho 0 ou 1 na topo da copa     
    !  fg.........Condicao de oravalho 0 ou 1 na base da copa    
    !  hr.........rel. humidity in top layer      
    !  nfprt......unidade de escrita standard print out unit   
    !  ndt     
    !  latitu  
    !  jstneu.....The first call to vntlat just gets the neutral values of ustar
    !             and ventmf para jstneu=.TRUE..  
    !  hgdtg.......n.b. fluxes expressed in joules m-2 
    !  hgdtc.......n.b. fluxes expressed in joules m-2  
    !  hgdtm.......n.b. fluxes expressed in joules m-2
    !  hcdtg.......n.b. fluxes expressed in joules m-2  
    !  hcdtc.......n.b. fluxes expressed in joules m-2  
    !  hcdtm.......n.b. fluxes expressed in joules m-2 
    !  egdtg.......partial derivative calculation for latent heat 
    !  egdtc.......partial derivative calculation for latent heat 
    !  egdqm.......partial derivative calculation for latent heat  
    !  ecdtg.......partial derivative calculation for latent heat  
    !  ecdtc.......partial derivative calculation for latent heat 
    !  ecdqm.......partial derivative calculation for latent heat  
    !  deadtg
    !  deadtc
    !  deadqm
    !  icheck......this version assumes dew-free conditions "icheck=1" to 
    !              estimate ea for buoyancy term in vntmf or ra.
    !  vcover(iv)..Fracao de cobertura de vegetacao iv=1 Top   
    !  vcover(iv)..Fracao de cobertura de vegetacao iv=2 Botto
    !  z0x.........roughness length   
    !  d...........Displacement height    
    !  rdc.........Constant related to aerodynamic resistance
    !              between ground and canopy air space
    !  rbc.........Constant related to bulk boundary layer resistance  
    !  z0..........Roughness length    
    !-----------------------------------------------------------------------
    !
    INTEGER, INTENT(in   ) :: ncols

    REAL   , INTENT(in   ) :: dtc3x                   
    INTEGER, INTENT(in   ) :: mon(ncols)                     
    INTEGER, INTENT(in   ) :: nmax                    

    INTEGER, INTENT(in   ) :: itype (ncols)            
    !
    !     the size of working area is ncols*187
    !     atmospheric parameters as boudary values for sib
    !
    REAL,    INTENT(inout) :: qm  (ncols)             
    REAL,    INTENT(inout) :: tm  (ncols)             
    REAL,    INTENT(in   ) :: um  (ncols)             
    REAL,    INTENT(in   ) :: vm  (ncols)             
    REAL,    INTENT(inout) :: psur(ncols)             
    REAL,    INTENT(in   ) :: ppc (ncols)             
    REAL,    INTENT(in   ) :: ppl (ncols)             
    REAL,    INTENT(in   ) :: radn(ncols,3,2)         
    !
    !     prognostic variables
    !     
    REAL,    INTENT(inout) :: tc   (ncols)            
    REAL,    INTENT(inout) :: tg   (ncols)            
    REAL,    INTENT(inout) :: td   (ncols)            
    REAL,    INTENT(inout) :: capac(ncols,2)          
    REAL,    INTENT(inout) :: w    (ncols,3)          
    !
    !     variables calculated from above and ambient conditions
    !
    REAL,    INTENT(inout) :: ra    (ncols)         
    REAL,    INTENT(out  ) :: rb    (ncols)         
    REAL,    INTENT(out  ) :: rd    (ncols)         
    REAL,    INTENT(out  ) :: rc    (ncols)         
    REAL,    INTENT(out  ) :: rg    (ncols)         
    REAL,    INTENT(out  ) :: tcta  (ncols)         
    REAL,    INTENT(out  ) :: tgta  (ncols)         
    REAL,    INTENT(out  ) :: ta    (ncols)         
    REAL,    INTENT(out  ) :: ea    (ncols)         
    REAL,    INTENT(out  ) :: etc   (ncols)         
    REAL,    INTENT(out  ) :: etg   (ncols)         
    REAL,    INTENT(out  ) :: btc   (ncols)         
    REAL,    INTENT(out  ) :: btg   (ncols)         
    REAL,    INTENT(inout) :: u2    (ncols)         
    REAL,    INTENT(out  ) :: radt  (ncols,icg)         
    REAL,    INTENT(inout) :: par   (ncols,icg)       
    REAL,    INTENT(out  ) :: pd    (ncols,icg)         
    REAL,    INTENT(out  ) :: rst   (ncols,icg)         
    REAL,    INTENT(out  ) :: rsoil (ncols)         
    REAL,    INTENT(out  ) :: phroot(ncols,icg)         
    REAL,    INTENT(out  ) :: hrr   (ncols)         
    REAL,    INTENT(out  ) :: phsoil(ncols,idp)         
    REAL,    INTENT(out  ) :: cc    (ncols)         
    REAL,    INTENT(out  ) :: cg    (ncols)         
    REAL,    INTENT(inout) :: satcap(ncols,icg)         
    REAL,    INTENT(inout) :: snow  (ncols,icg)       
    REAL,    INTENT(out  ) :: dtc   (ncols)         
    REAL,    INTENT(out  ) :: dtg   (ncols)         
    REAL,    INTENT(out  ) :: dtm   (ncols)         
    REAL,    INTENT(out  ) :: dqm   (ncols)         
    REAL,    INTENT(out  ) :: stm   (ncols,icg)         
    REAL,    INTENT(inout) :: extk  (ncols,icg,iwv,ibd)
    REAL,    INTENT(in   ) :: radfac(ncols,icg,iwv,ibd)
    REAL,    INTENT(in   ) :: closs (ncols)           
    REAL,    INTENT(in   ) :: gloss (ncols)           
    REAL,    INTENT(inout) :: thermk(ncols)           
    REAL,    INTENT(in   ) :: p1f   (ncols)           
    REAL,    INTENT(in   ) :: p2f   (ncols)           
    !
    !     heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
    !
    REAL,    INTENT(inout) :: ect   (ncols)         
    REAL,    INTENT(inout) :: eci   (ncols)         
    REAL,    INTENT(inout) :: egt   (ncols)         
    REAL,    INTENT(inout) :: egi   (ncols)         
    REAL,    INTENT(inout) :: egs   (ncols)         
    REAL,    INTENT(out  ) :: ec    (ncols)         
    REAL,    INTENT(out  ) :: eg    (ncols)         
    REAL,    INTENT(out  ) :: hc    (ncols)         
    REAL,    INTENT(out  ) :: hg    (ncols)         
    REAL,    INTENT(out  ) :: ecidif(ncols)         
    REAL,    INTENT(out  ) :: egidif(ncols)         
    REAL,    INTENT(out  ) :: ecmass(ncols)         
    REAL,    INTENT(out  ) :: egmass(ncols)         
    REAL,    INTENT(out  ) :: etmass(ncols)         
    REAL,    INTENT(out  ) :: hflux (ncols)         
    REAL,    INTENT(out  ) :: chf   (ncols)         
    REAL,    INTENT(out  ) :: shf   (ncols)         
    REAL,    INTENT(out  ) :: fluxef(ncols)         
    REAL,    INTENT(inout) :: roff  (ncols)         
    REAL,    INTENT(inout) :: drag  (ncols)           
    !
    !     this is for coupling with closure turbulence model
    !
    REAL,    INTENT(in   ) :: bps   (ncols)           
    REAL,    INTENT(in   ) :: psb   (ncols)           
    REAL,    INTENT(in   ) :: dzm   (ncols)           
    REAL,    INTENT(in   ) :: em    (ncols)           
    REAL,    INTENT(inout) :: gmt   (ncols,3)         
    REAL,    INTENT(inout) :: gmq   (ncols,3)         
    REAL,    INTENT(inout) :: gmu   (ncols,4)         
    REAL,    INTENT(inout) :: cu    (ncols)           
    REAL,    INTENT(inout) :: cuni  (ncols)           
    REAL,    INTENT(inout) :: ctni  (ncols)           
    REAL,    INTENT(inout) :: ustar (ncols)           
    REAL,    INTENT(in   ) :: cosz  (ncols)           
    REAL,    INTENT(in   ) :: sinclt(ncols)          
    REAL,    INTENT(in   ) :: rhoair(ncols)           
    REAL,    INTENT(in   ) :: psy   (ncols)           
    REAL,    INTENT(out  ) :: rcp   (ncols)           
    REAL,    INTENT(inout) :: wc    (ncols)           
    REAL,    INTENT(inout) :: wg    (ncols)           
    REAL,    INTENT(inout) :: fc    (ncols)           
    REAL,    INTENT(inout) :: fg    (ncols)           
    REAL,    INTENT(inout) :: hr    (ncols)           

    INTEGER, INTENT(in   ) :: nfprt                  

    INTEGER, INTENT(in   ) :: ndt                    
    INTEGER, INTENT(in   ) :: latitu                 

    REAL   , INTENT(in   ) :: rstpar2 (ncols,icg,iwv)       
    REAL   , INTENT(in   ) :: zlt2    (ncols,icg)
    REAL   , INTENT(in   ) :: green2  (ncols,icg)
    REAL   , INTENT(in   ) :: chil2   (ncols,icg)
    REAL,    INTENT(inout) :: vcover  (ncols,icg)         
    REAL,    INTENT(inout) :: z0x(ncols)               
    REAL,    INTENT(inout) :: d  (ncols)               
    REAL,    INTENT(inout) :: rdc(ncols)               
    REAL,    INTENT(inout) :: rbc(ncols)               
    REAL,    INTENT(inout) :: z0 (ncols)               
    REAL   , INTENT(in   ) :: topt2   (ncols,icg)
    REAL   , INTENT(in   ) :: tll2    (ncols,icg)
    REAL   , INTENT(in   ) :: tu2     (ncols,icg)
    REAL   , INTENT(in   ) :: defac2  (ncols,icg)
    REAL   , INTENT(in   ) :: ph12    (ncols,icg)
    REAL   , INTENT(in   ) :: ph22    (ncols,icg)    

    REAL   , INTENT(IN   ) :: xcover(ityp, imon, icg)
    REAL   , INTENT(IN   ) :: zlt(ityp, imon, icg)
    LOGICAL :: jstneu                 
    INTEGER :: icheck(ncols)           

    !
    !     derivatives
    !
    REAL :: hgdtg (ncols)       
    REAL :: hgdtc (ncols)       
    REAL :: hgdtm (ncols)       
    REAL :: hcdtg (ncols)       
    REAL :: hcdtc (ncols)       
    REAL :: hcdtm (ncols)       
    REAL :: egdtg (ncols)       
    REAL :: egdtc (ncols)       
    REAL :: egdqm (ncols)       
    REAL :: ecdtg (ncols)       
    REAL :: ecdtc (ncols)       
    REAL :: ecdqm (ncols)       
    REAL :: deadtg(ncols)       
    REAL :: deadtc(ncols)       
    REAL :: deadqm(ncols)       

    REAL    :: ef    (ncols,3)                        
    REAL    :: absoil(ncols)                          
    REAL    :: totdep(ncols)                          
    REAL    :: div   (ncols)                          
    REAL    :: eft   (ncols)                          
    REAL    :: aaa   (ncols)                          
    REAL    :: dep   (ncols)                          


    INTEGER :: i 
    INTEGER :: il 
    INTEGER :: ntyp 
    INTEGER :: iveg
    REAL    :: hlat3i
    REAL    :: gby100
    REAL    :: timcon
    REAL    :: totwb(ncols)
    REAL    :: endwb(ncols)
    REAL    :: cbal (ncols)
    REAL    :: gbal (ncols)
    !     
    !     calculates soil water budget prior to calling pbl
    !     
    DO i = 1, nmax
       ntyp=itype(i)
       totwb(i)=w(i,1)*poros(ntyp)*zdepth(ntyp,1) &
            +w(i,2)*poros(ntyp)*zdepth(ntyp,2) &
            +w(i,3)*poros(ntyp)*zdepth(ntyp,3) &
            +capac(i,1) + capac(i,2)
    END DO
    !     
    !     planetary boundary layer parameterization
    !     
    CALL pbl(jstneu, hgdtg , hgdtc , hgdtm , hcdtg , hcdtc , hcdtm , &
         egdtg , egdtc , egdqm , ecdtg , ecdtc , ecdqm , deadtg, &
         deadtc, deadqm,icheck , ect   , eci   , egt   , egi   , &
         egs   , ec    , eg    , hc    , hg    , ecidif, egidif, &
         ecmass, egmass, etmass, hflux , chf   , shf   , roff  , &
         bps   , psb   , dzm   , em    , gmt   , gmq   ,  cu   , &
         cuni  , ctni  , ustar , cosz  , rhoair, psy   , rcp   , &
         wc   , wg       , fc       , fg       , hr       , vcover, z0x   , &
         d       , rdc   , rbc   , z0       , qm       , tm       , um       , &
         vm       , psur  , ppc   , ppl   , radn  , ra       , rb       , &
         rd       , rc       , rg       , tcta  , tgta  , ta       , ea       , &
         etc   , etg   , btc   , btg   , u2       , radt  , par   , &
         pd       , rst   , rsoil , phroot,  hrr  , phsoil, cc       , &
         cg       , satcap, snow  , dtc   , dtg   , dtm   , dqm   , &
         stm   , extk  , radfac, closs , gloss , thermk, p1f   , &
         p2f   , tc       , tg       , td       , capac , w       , itype , &
         dtc3x , mon   , nmax  , ncols ,zlt2  ,green2,chil2 ,rstpar2,&
       topt2 ,tll2  ,tu2   , defac2,ph12  ,ph22 , xcover)
    !     
    !     continue to update sib variables
    !     
    DO i = 1, nmax
       tc(i) = tc(i) + dtc(i)
       tg(i) = tg(i) + dtg(i)
    END DO
    !     
    !     dumping of small capac values onto soil surface store
    !     
    DO iveg = 1, 2
       DO i = 1, nmax
          ntyp  =itype(i)
          IF (capac(i,iveg) <= 1.e-6)THEN
             w(i,1)=w(i,1)+capac(i,iveg)/(poros(ntyp)*zdepth(ntyp,1))
             capac(i,iveg)=0.0
          END IF
       END DO
    END DO
    !
    !     snowmelt/refreeze calculation
    !
    CALL snowm(&
         chf   ,shf   ,fluxef,roff  ,cc    ,cg    ,snow  ,dtc   ,dtg   , &
         tc    ,tg    ,td    ,capac ,w     ,itype ,dtc3x ,nmax  ,ncols   )
    !
    !     update deep soil temperature using effective soil heat flux
    !
    timcon=dtc3x/(2.0 *SQRT(pie*365.0 ))

    DO i = 1, nmax
       td(i)=td(i)+fluxef(i)/cg(i)*timcon
    END DO
    !     
    !     bare soil evaporation loss
    !     
    hlat3i=1./(hl*1000.0 )
    DO i = 1, nmax
       ntyp=itype(i)
       w(i,1)=w(i,1)-egs(i)*hlat3i/(poros(ntyp)*zdepth(ntyp,1))
    END DO
    !
    !        extraction of transpiration loss from root zone
    !     
    DO iveg = 1, 2
       IF (iveg == 1) THEN
          DO i = 1, nmax
             absoil(i)=ect(i)*hlat3i
          END DO
       ELSE
          DO i = 1, nmax
             absoil(i)=egt(i)*hlat3i
          END DO
       END IF
       DO i = 1, nmax
          ntyp=itype(i)
          ef(i,2)=0.0
          ef(i,3)=0.0
          totdep(i)=zdepth(ntyp,1)
       END DO
       DO il = 2, 3
          DO i = 1, nmax
             ntyp=itype(i)
             totdep(i)=totdep(i)+zdepth(ntyp,il)
             div(i)=rootd(ntyp,iveg)
             dep(i)=MAX(0.0  ,rootd(ntyp,iveg)-totdep(i)+ &
                  zdepth(ntyp,il))
             dep(i)=MIN(dep(i),zdepth(ntyp,il))
             ef(i,il)=dep(i)/div(i)
          END DO
       END DO
       DO i = 1, nmax
          eft(i  )=ef(i,2)+ef (i,3)
          eft(i) = MAX(eft(i),0.1e-5)
          ef (i,2)=ef(i,2)/eft(i)
          ef (i,3)=ef(i,3)/eft(i)
       END DO
       DO il = 2, 3
          DO i = 1, nmax
             ntyp=itype(i)
             w(i,il)=w(i,il)-absoil(i)*ef(i,il)/ &
                  (poros(ntyp)*zdepth(ntyp,il))
          END DO
       END DO
    END DO
    !     
    !     interflow, infiltration excess and loss to
    !     groundwater .  all losses are assigned to variable 'roff' .
    !     
    DO il = 1, 2
       DO i = 1, nmax
          IF (w(i,il) <= 0.0) THEN
             ntyp=itype(i)
             w(i,il+1)=w(i,il+1)+w(i,il)* &
                  zdepth(ntyp,il)/zdepth(ntyp,il+1)
             w(i,il  )=0.0
          END IF
       END DO
    END DO

    CALL runoff(&
         roff  ,tg    ,td    ,capac ,w     ,itype ,dtc3x ,nmax  ,ncols )

    DO i = 1, nmax
       ntyp = itype(i)
       IF (w(i,1) > 1.0) THEN
          w(i,2)=w(i,2)+(w(i,1)-1.0 )*zdepth(ntyp,1)/zdepth(ntyp,2)
          w(i,1)=1.0
       ENDIF
       IF (w(i,2) > 1.0) THEN
          w(i,3)= w(i,3)+(w(i,2)-1.0 )*zdepth(ntyp,2)/zdepth(ntyp,3)
          w(i,2)=1.0
       ENDIF
       IF (w(i,3) > 1.0) THEN
          roff(i)=roff(i)+(w(i,3)-1.0 )*poros(ntyp)*zdepth(ntyp,3)
          w(i,3)=1.0
       END IF
    END DO
    !     
    !     increment prognostic variables
    !     
    !     adjust theta and sh to be consistent with dew formation
    !     
    gby100=0.01  *grav

    DO i = 1, nmax
       !     
       !     solve implicit system for winds
       !     
       drag(i)  =rhoair(i)*cu(i)*ustar(i)
       aaa (i)  =drag  (i)*gby100/psb(i)
       gmu (i,2)=gmu   (i,2)+dtc3x*aaa(i)
       gmu (i,3)=(gmu(i,3)-aaa(i)*um(i)*sinclt(i))/gmu(i,2)
       gmu (i,4)=(gmu(i,4)-aaa(i)*vm(i)*sinclt(i))/gmu(i,2)
    END DO
    !
    !     calculates soil water budget after calling pbl
    !     and compares with previous budget
    !
    DO i = 1, nmax
       ntyp=itype(i)
       endwb(i)=w(i,1)*poros(ntyp)*zdepth(ntyp,1) &
            +w(i,2)*poros(ntyp)*zdepth(ntyp,2) &
            +w(i,3)*poros(ntyp)*zdepth(ntyp,3) &
            +capac(i,1)+capac(i,2) &
            -(ppl(i)+ppc(i))/1000.0 + etmass(i)/1000.0 + roff(i)
       !IF (ABS(totwb(i)-endwb(i)) > 0.0001) THEN
       !  WRITE(nfprt,998) latitu,i,ntyp,ndt, &
       !       totwb(i),endwb(i),(totwb(i)-endwb(i)),w(i,1),w(i,2), &
       !       w(i,3),capac(i,1),capac(i,2),ppl(i),ppc(i),etmass(i), &
       !       roff(i),zlt(ntyp,12,1),zlt(ntyp,12,2), &
       !       tc(i),tg(i),td(i),tm(i)
       !END IF
       !
       !     calculates and compares energy budgets
       !     
       cbal(i)=radt(i,1)-chf(i)-(ect(i)+hc(i)+eci(i))/dtc3x
       gbal(i)=radt(i,2)-shf(i)-(egt(i)+egi(i)+hg(i)+egs(i))/dtc3x
       !IF (ABS(cbal(i)-gbal(i)) > 5.0) &
       !    WRITE(nfprt,999)latitu,i,ntyp,ndt, &
       !    radt(i,1),radt(i,2),chf(i),shf(i),hflux(i), &
       !    ect(i),eci(i),egt(i),egi(i),egs(i)
    END DO
    !cdir critical
    DO i=1,nmax
       ntyp=itype(i)
       ! if(abs(totwb(i)-endwb(i)).gt.0.0001) then
       IF(ABS(totwb(i)-endwb(i)).GT.0.0005) THEN
          WRITE(nfprt,998) latitu,i,ntyp,ndt, &
               totwb(i),endwb(i),(totwb(i)-endwb(i)),w(i,1),w(i,2), &
               w(i,3),capac(i,1),capac(i,2),ppl(i),ppc(i),etmass(i), &
               roff(i),zlt(ntyp,12,1),zlt(ntyp,12,2), &
               tc(i),tg(i),td(i),tm(i)
       END IF
       IF(ABS(cbal(i)-gbal(i)).GT.5.0) &
            WRITE(nfprt,999)latitu,i,ntyp,ndt, &
            radt(i,1),radt(i,2),chf(i),shf(i),hflux(i), &
            ect(i),eci(i),egt(i),egi(i),egs(i)
    END DO
    !cdir end critical
998 FORMAT(3I4,1X,'WATER BAL.',I8, &
         /3E12.4/3E12.4/2E12.4/4E12.4/2E12.4/4E12.4)
999 FORMAT(3I4,1X,'ENERGY BAL.',I8/4E12.3/6E12.3)
  END SUBROUTINE fysiks



  ! vntlt1 :performs ventilation mass flux, based on deardorff, mwr, 1972?.



  SUBROUTINE vntlt1 &
       (rmi   ,rhi   ,gu    ,gv    ,gt    ,tsurf ,tsea  ,ncols , &
       sigki ,cuni  ,cui   ,cu    ,ctni  ,cti   ,ct    ,speedm,tha   , &
       thm   ,dzm   ,thvgm ,rib   ,z0    ,zorl  ,ustar ,sinclt  )
    !     
    !==========================================================================
    !==========================================================================
    !==========================================================================
    ! imax..........number of grid points on a gaussian latitude circle
    ! z0ice.........Roughness length of ice
    ! sinclt........sinclt=SIN(colrad(latco))
    ! rmi...........rmi   (i)=cu(i)*ustar(i), where 
    !               cu is friction  transfer coefficients
    !               ustar is surface friction velocity  (m/s)   
    ! rhi...........rhi   (i)=ct(i)*ustar(i), where
    !               ct is heat transfer coefficients.
    !               ustar is surface friction velocity  (m/s)   
    ! gu............(zonal      velocity)*sin(colat)       
    ! gv............(meridional velocity)*sin(colat)       
    ! gt............temperature       
    ! tsurf.........earth's surface temperature used for radiation
    !               for the first time step when ground temperature is not yet
    !               computed (this is done by subr.tsinit ), 
    ! tsea..........effective surface radiative temperature ( tgeff )  
    ! zorl..........zorl (i)= 100.0 *zgrav*speedm(i)*rhi(i) 
    !               zgrav =0.032 /grav  
    ! delsig
    ! sigki ........sigki (k)=1.0e0/EXP(akappa*LOG(sig(k))),  where "sig" 
    !               sigma coordinate at middle of layer and akappa=gasr/cp
    ! cuni..........neutral friction transfer  coefficients.  
    ! cui...........cui   (i)=cuni(i)*EXP( aa-SQRT(aa*aa+bb*f))
    !               cui   (i)=cuni(i)*EXP(-tt+SQRT(tt*tt+ss*f))   
    ! cu............Friction  transfer coefficients.       
    ! ctni..........neutral heat transfer coefficients.  
    ! cti...........cti   (i)=ctni(i)*EXP( qq-SQRT(qq*qq+rr*g))
    !               cti   (i)=cui (i)   
    ! ct............heat transfer coefficients.       
    ! speedm........speedm(i)=SQRT(gu(i)**2+gv(i)**2)*sincli, where
    !               sincli=1.0 /sinclt             
    ! tha...........tha   (i)= tsurf(i)  
    ! thm...........thm   (i)= gt(i)*sigki(1)   
    ! dzm...........dzm   (i)=gt(i)*rbyg 
    !               rbyg  =gasr/grav*delsig(1)*0.5  
    ! thvgm.........thvgm (i)= tha(i)-thm(i)
    ! rib...........bulk richardson number.   
    ! z0............Roughness length  
    ! ustarr........surface friction velocity  (m/s) 
    ! gasr..........gas constant of dry air        (j/kg/k)   
    ! grav..........grav   gravity constant        (m/s**2) 
    !========================================================================== 
    !     
    INTEGER, INTENT(in   ) :: ncols
    REAL,    INTENT(in   ) :: sinclt(ncols)
    REAL,    INTENT(out  ) :: rmi   (ncols)
    REAL,    INTENT(out  ) :: rhi   (ncols)
    REAL,    INTENT(in   ) :: gu    (ncols)
    REAL,    INTENT(in   ) :: gv    (ncols)
    REAL,    INTENT(in   ) :: gt    (ncols)
    REAL,    INTENT(in   ) :: tsurf (ncols)
    REAL,    INTENT(in   ) :: tsea  (ncols)
    REAL,    INTENT(in   ) :: zorl  (ncols)
    REAL,    INTENT(in   ) :: sigki   (1)
    REAL,    INTENT(out  ) :: cuni  (ncols)
    REAL,    INTENT(out  ) :: cui   (ncols)
    REAL,    INTENT(out  ) :: cu    (ncols)
    REAL,    INTENT(out  ) :: ctni  (ncols)
    REAL,    INTENT(out  ) :: cti   (ncols)
    REAL,    INTENT(out  ) :: ct    (ncols)
    REAL,    INTENT(out  ) :: speedm(ncols)
    REAL,    INTENT(out  ) :: tha   (ncols)
    REAL,    INTENT(out  ) :: thm   (ncols)
    REAL,    INTENT(out  ) :: dzm   (ncols)
    REAL,    INTENT(out  ) :: thvgm (ncols)
    REAL,    INTENT(out  ) :: rib   (ncols)
    REAL,    INTENT(inout) :: z0    (ncols)
    REAL,    INTENT(out  ) :: ustar (ncols)

    REAL,        PARAMETER :: vkrmn=0.40
    REAL,        PARAMETER :: ribc=3.05
    REAL,        PARAMETER :: aa=1.2270 
    REAL,        PARAMETER :: bb=1.2642 
    REAL,        PARAMETER :: tt=1.8900 
    REAL,        PARAMETER :: ss=5.0519 
    REAL,        PARAMETER :: ee=1.2743 
    REAL,        PARAMETER :: ff=3.4805
    REAL,        PARAMETER :: gg=0.87581
    REAL,        PARAMETER :: hh=-1.5630 
    REAL,        PARAMETER :: pp=10.815 
    REAL,        PARAMETER :: qq=1.3462 
    REAL,        PARAMETER :: rr=1.8380
    REAL                   :: sincli(ncols) 
    REAL                   :: f
    REAL                   :: g
    INTEGER                :: i

    DO i = 1, ncols
       IF (tsea(i) < 0.0 .AND. ABS(tsea(i)) >= 271.17) THEN
          z0(i)=0.01 *zorl(i)
       ELSE IF (tsea(i) < 0.0 .AND. ABS(tsea(i)) < 271.17) THEN
          z0(i)=z0ice
       END IF
       sincli(i)=1.0 /sinclt(i)
    END DO

    DO i = 1, ncols
       IF (tsea(i) <= 0.0) THEN
          speedm(i)=SQRT(gu(i)**2+gv(i)**2)*sincli(i)
          speedm(i)=MAX(2.0 ,speedm(i))
          dzm   (i)=gt(i)*rbyg
          cuni(i)=LOG(dzm(i)/z0(i))/vkrmn*gg+hh
          ctni(i)=cuni(i)
          !     
          !     stability branch based on bulk richardson number.
          !     
          thm   (i)= gt(i)*sigki(1)
          tha   (i)= tsurf(i)
          thvgm (i)= tha(i)-thm(i)
          rib   (i)=-thvgm(i)*grav*dzm(i)/ (thm(i)*speedm(i)**2)
          rib   (i)=MAX(-1.25 ,rib(i))
          rib   (i)=MIN( 1.25 ,rib(i))
          IF (rib(i) < 0.0) THEN
             f        =LOG(1.0-ee*rib(i))
             cui   (i)=cuni(i)*EXP( aa-SQRT(aa*aa+bb*f))
             g        =LOG(1.0-ff*rib(i))
             cti   (i)=ctni(i)*EXP( qq-SQRT(qq*qq+rr*g))
          ELSE
             f        =LOG(1.0+pp*rib(i))
             cui   (i)=cuni(i)*EXP(-tt+SQRT(tt*tt+ss*f))
             cti   (i)=cui (i)
          END IF
          cu    (i)=1.0/cui(i)
          ct    (i)=1.0/cti(i)
          !     
          !     surface friction velocity and ventilation mass flux
          !     
          ustar (i)=speedm(i)*cu(i)
          rmi   (i)=cu(i)*ustar(i)
          rhi   (i)=ct(i)*ustar(i)
       END IF
    END DO
  END SUBROUTINE vntlt1






  SUBROUTINE seasfc( &
       tmtx  ,umtx  ,qmtx  ,kpbl  ,kqpbl ,slrad ,tsurf ,qsurf , &
       gu    ,gv    ,gt    ,gq    ,gps   ,tsea  ,dtc3x ,sinclt, &
       sigki ,delsig,sens  ,evap  ,umom  ,vmom  ,rmi   ,rhi   , &
       cond  ,stor  ,zorl  ,rnet  ,ncols)
    !       
    !==========================================================================
    ! ncols......Number of grid points on a gaussian latitude circle
    ! kpbl.......Number of layers pbl process is included( for u v,t ) 
    ! kqpbl......Number of layers pbl process is included( for q     )
    ! tmtx.......Temperature related matrix
    !            gmt(i,k,1)*d(gt(i,k-1))/dt+gmt(i,k,2)*d(gt(i,k))/dt=gmt(i,k,3)
    !            gmt(i,1,1)=0.
    !            gmt(*,*,1)...dimensionless
    !            gmt(*,*,2)...dimensionless
    !            gmt(*,*,3)...deg/sec 
    ! umtx.......Wind related matrix
    !            gmu(i,k,1)*d(gu(i,k-1))/dt+gmu(i,k,2)*d(gu(i,k))/dt=gmu(i,k,3)
    !            gmu(i,k,1)*d(gv(i,k-1))/dt+gmu(i,k,2)*d(gv(i,k))/dt=gmu(i,k,4)
    !            gmu(i,1,1)=0.
    !            gmu(*,*,1)...dimensionless
    !            gmu(*,*,2)...dimensionless
    !            gmu(*,*,3)...m/sec**2
    !            gmu(*,*,4)...m/sec**2 
    ! qmtx.......specific humidity related matrix
    !            gmq(i,k,1)*d(gq(i,k-1))/dt+gmq(i,k,2)*d(gq(i,k))/dt=gmq(i,k,3)
    !            gmq(i,1,1)=0.
    !            gmq(*,*,1)...dimensionless
    !            gmq(*,*,2)...dimensionless
    !            gmq(*,*,3)...kg/kg/sec 
    ! slrad......radiation interpolation
    ! tsurff.....earth's surface temperature used for radiation 
    !            for the first time step when ground temperature is not yet
    !            computed (this is done by subr.tsinit ), 
    ! qsurf......qsurf(i)=0.622e0*EXP(21.65605e0 -5418.0e0 /tsurf(i))/gps(i)
    ! gu.........(zonal      velocity)*sin(colat)             
    ! gv.........(meridional velocity)*sin(colat)      
    ! gt.........Temperature      
    ! gq.........Specific humidity      
    ! gps........Surface pressure in mb  
    ! tsea.......effective surface radiative temperature ( tgeff ) 
    ! dtc3x......time increment dt
    ! sinclt.....sinclt=SIN(colrad(latco))
    ! sigki......sigki (k)=1.0e0/EXP(akappa*LOG(sig(k))),  where "sig" 
    !            sigma coordinate at middle of layer and akappa=gasr/cp
    ! delsig
    ! sens.......sensible heat flux                                 
    ! evap.......latent heat flux  "evaporation"     
    ! umom.......umom(i)=fmom*um(ncount), 
    !            where .fmom  momentum flux      in n/m**2 
    !            fmom= rhoair(ncount)*cu(ncount)*ustar(ncount) 
    !            um  (ncount)=gu (i,1)/sinclt  
    !            gu          = (zonal velocity)*sin(colat)
    ! vmom.......vmom(i)=rho(i)*gv(i)*rmi(i) 
    !            rho  (i)=gps(i)/(gr100*gt(i)) 
    !            gr100 =gasr*0.01
    ! z0ice.......Roughness length of ice
    ! rmi.........rmi   (i)=cu(i)*ustar(i), where 
    !             cu is friction  transfer coefficients
    !             ustar is surface friction velocity  (m/s)
    ! rhi.........rhi   (i)=ct(i)*ustar(i), where
    !             ct is heat transfer coefficients.
    !             ustar is surface friction velocity  (m/s)
    ! cond........cond(i)=gice*(tsurf(i)-tice) or
    !             cond(i)=(2.03/2.0)*(tsurf(i)-271.16)
    ! stor........stor(i)=hscap*c0(i) 
    ! zorl........zorl (i)= 100.0 *zgrav*speedm(i)*rhi(i) 
    !             zgrav =0.032 /grav
    ! rnet........rnet=-697.58*slrad(i)
    !             rnet(i)=rnet(i)-stefan*tsurf(i)**4
    ! cp..........specific heat of air           (j/kg/k)         
    ! hl..........heat of evaporation of water     (j/kg)         
    ! gasr........gas constant of dry air        (j/kg/k)    
    ! grav........grav   gravity constant        (m/s**2)    
    ! stefan......Stefan Boltzman constant     
    !==========================================================================
    !
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(IN   ) :: kpbl 
    INTEGER, INTENT(IN   ) :: kqpbl
    REAL,    INTENT(INOUT) :: tmtx (ncols,kpbl,3)
    REAL,    INTENT(INOUT) :: umtx (ncols,kpbl,4)
    REAL,    INTENT(INOUT) :: qmtx (ncols,kqpbl,3)
    REAL,    INTENT(IN   ) :: slrad(ncols)
    REAL,    INTENT(INOUT) :: tsurf(ncols)
    REAL,    INTENT(IN   ) :: qsurf(ncols)
    REAL,    INTENT(IN   ) :: gu   (ncols)
    REAL,    INTENT(IN   ) :: gv   (ncols)
    REAL,    INTENT(INOUT) :: gt   (ncols)
    REAL,    INTENT(INOUT) :: gq   (ncols)
    REAL,    INTENT(IN   ) :: gps  (ncols)
    REAL,    INTENT(INOUT) :: tsea (ncols)
    REAL,    INTENT(IN   ) :: dtc3x
    REAL,    INTENT(IN   ) :: sinclt(ncols)
    REAL,    INTENT(IN   ) :: sigki(1)
    REAL,    INTENT(IN   ) :: delsig(1)
    REAL,    INTENT(OUT  ) :: sens (ncols)
    REAL,    INTENT(OUT  ) :: evap (ncols)
    REAL,    INTENT(OUT  ) :: umom (ncols)
    REAL,    INTENT(OUT  ) :: vmom (ncols)
    REAL,    INTENT(OUT  ) :: rmi  (ncols)
    REAL,    INTENT(OUT  ) :: rhi  (ncols)
    REAL,    INTENT(OUT  ) :: cond (ncols)
    REAL,    INTENT(OUT  ) :: stor (ncols)
    REAL,    INTENT(INOUT) :: zorl (ncols)
    REAL,    INTENT(OUT  ) :: rnet (ncols)


    REAL    :: rho   (ncols)                 
    REAL    :: ah    (ncols)                 
    REAL    :: al    (ncols)                 
    REAL    :: am    (ncols)                 
    REAL    :: cuni  (ncols)                 
    REAL    :: cui   (ncols)                 
    REAL    :: cu    (ncols)                 
    REAL    :: ctni  (ncols)                 
    REAL    :: cti   (ncols)                 
    REAL    :: ct    (ncols)                 
    REAL    :: speedm(ncols)                 
    REAL    :: tha   (ncols)                 
    REAL    :: thm   (ncols)                 
    REAL    :: dzm   (ncols)                 
    REAL    :: thvgm (ncols)                 
    REAL    :: rib   (ncols)                 
    REAL    :: z0    (ncols)                 
    REAL    :: ustar (ncols)                 
    REAL    :: gtsav (ncols)                 
    REAL    :: gqsav (ncols)                 
    REAL    :: tmsav (ncols)                 
    REAL    :: qmsav (ncols)                 
    REAL    :: tssav (ncols)                 
    REAL    :: dqg0  (ncols)                 
    REAL    :: b00   (ncols)                 
    REAL    :: b03   (ncols)                 
    REAL    :: b04   (ncols)                 
    REAL    :: c0    (ncols)                 
    REAL    :: b30   (ncols)                 
    REAL    :: b33   (ncols)                 
    REAL    :: c3    (ncols)                 
    REAL    :: b40   (ncols)                 
    REAL    :: b44   (ncols)                 
    REAL    :: c4    (ncols)                 

    INTEGER :: i 
    INTEGER :: ncount 
    REAL    :: gbyhl 
    REAL    :: gbycp 
    REAL    :: gr100 
    REAL    :: gb100
    REAL    :: zgrav 
    REAL    :: gice 
    REAL    :: hscap 
    REAL    :: sl1kap
    REAL    :: st4 
    REAL    :: dti

    REAL, PARAMETER :: dd=0.05
    REAL, PARAMETER :: tice=271.16
    REAL, PARAMETER :: dice=2.0
    REAL, PARAMETER :: hice=2.03
    REAL, PARAMETER :: rhoice=920.0
    REAL, PARAMETER :: cice=2093.0


    gr100 =gasr*0.01
    gbycp =grav/(cp*delsig(1)*100.0 *sigki(1))
    gbyhl =grav/(hl*delsig(1)*100.0 )
    gb100 =grav/(   delsig(1)*100.0 )
    zgrav =0.032 /grav
    gice  =hice/dice ! 2.03/2.0
    hscap =rhoice*cice*dd/dtc3x
    sl1kap=sigki(1)
    st4   =stefan*4.0
    dti   =1.0 /dtc3x

    DO i = 1, ncols
       rnet (i)=-697.58*slrad(i)
       rho  (i)=gps(i)/(gr100*gt(i))
       ah   (i)=gbycp/gps(i)
       al   (i)=gbyhl/gps(i)
       dqg0 (i)=0.622 *EXP(30.25353 -5418.0 /tsurf(i)) &
            /(tsurf(i)*tsurf(i)*gps(i))
       gtsav(i)=gt   (i)
       gqsav(i)=gq   (i)
       tssav(i)=tsurf(i)
       tmsav(i)=tmtx (i,1,3)
       qmsav(i)=qmtx (i,1,3)
    END DO

    c0  =0.0  
    cond=0.0  
    stor=0.0  

    ncount=0
8000 CONTINUE
    ncount=ncount+1
    CALL vntlt1 ( &
         rmi   ,rhi   ,gu    ,gv    ,gt    ,tsurf ,tsea  ,ncols , &
         sigki ,cuni  ,cui   ,cu    ,ctni  ,cti   ,ct    ,speedm,tha   , &
         thm   ,dzm   ,thvgm ,rib   ,z0    ,zorl  ,ustar ,sinclt )
    DO i = 1, ncols
       gt  (i)    =gtsav(i)
       gq  (i)    =gqsav(i)
       tsurf(i)   =tssav(i)
       tmtx(i,1,3)=tmsav(i)
       qmtx(i,1,3)=qmsav(i)
    END DO
    DO i = 1, ncols
       IF (tsea(i) < 0.0 .AND. ABS(tsea(i)) < tice+0.01) THEN
          b00(i)=   hscap+cp*rho(i)*rhi(i) &
               +hl*rho(i)*rhi(i)*dqg0(i) &
               +gice+st4*tsurf(i)**3
          b03(i)=        -cp*rho(i)*rhi(i)*sl1kap
          b04(i)=-hl*rho(i)*rhi(i)
          c0 (i)=rnet(i) -cp*rho(i)*rhi(i)*(tsurf(i)-sl1kap*gt(i)) &
               -hl*rho(i)*rhi(i)*(qsurf(i)-       gq(i)) &
               -gice*(tsurf(i)-tice)-stefan*tsurf(i)**4
          b30(i)=               -ah (i)*cp*rho(i)*rhi(i)
          b33(i)=tmtx(i,1,2)*dti-b30(i)*          sl1kap
          c3 (i)=tmtx(i,1,3)    -b30(i)*(tsurf(i)-sl1kap*gt(i))
          b40(i)=               -al(i)*hl*rho(i)*rhi(i)* dqg0 (i)
          b44(i)=qmtx(i,1,2)*dti+al(i)*hl*rho(i)*rhi(i)
          c4 (i)=qmtx(i,1,3)    + &
               al(i)*hl*rho(i)*rhi(i)*(qsurf(i)-gq(i))
          b00(i)=b00(i)-b30(i)*b03(i)/b33(i)-b40(i)*b04(i)/b44(i)
          c0 (i)=c0 (i)-c3 (i)*b03(i)/b33(i)-c4 (i)*b04(i)/b44(i)
          c0 (i)=c0 (i)/b00(i)
          tsurf(i)=tsurf(i)+c0(i)
          tmtx(i,1,3)=(c3(i)-b30(i)*c0(i))/(b33(i)*dtc3x)
          qmtx(i,1,3)=(c4(i)-b40(i)*c0(i))/(b44(i)*dtc3x)
       ELSE IF (tsea(i) < 0.0 .AND. ABS(tsea(i)) > tice+0.01) THEN
          zorl (i)= 100.0 *zgrav*speedm(i)*rhi(i)
          sens (i)= rho(i)*cp*(tsurf(i)-gt(i)*sigki(1))*rhi(i)
          evap (i)= rho(i)*hl*(qsurf(i)-gq(i))*rhi(i)
          tmtx(i,1,3)=(tmtx(i,1,3)+ah(i)*sens(i)) &
               /(tmtx(i,1,2)+dtc3x*ah(i)*rho(i)*cp*rhi(i))
          qmtx(i,1,3)=(qmtx(i,1,3)+al(i)*evap(i)) &
               /(qmtx(i,1,2)+dtc3x*al(i)*rho(i)*hl*rhi(i))
       END IF
    END DO

    DO i = 1, ncols
       gt(i)=gt(i)+tmtx(i,1,3)*dtc3x
       gq(i)=gq(i)+qmtx(i,1,3)*dtc3x
    END DO

    IF (ncount == 1) go to 8000

    DO i = 1, ncols
       sens(i)=rho(i)*cp*(tsurf(i)-gt(i)*sigki(1))*rhi(i)
       evap(i)=rho(i)*hl*(qsurf(i)-gq(i)         )*rhi(i)
       gt  (i)=gtsav(i)
       gq  (i)=gqsav(i)
       rnet(i)=rnet(i)-stefan*tsurf(i)**4
       IF (tsea(i) < 0.0 .AND. ABS(tsea(i)) < tice+0.01) THEN
          cond(i)=gice*(tsurf(i)-tice)
          stor(i)=hscap*c0(i)
          rnet(i)=rnet(i)-stefan*tsurf(i)**3*4.0 *c0(i)
          tsurf(i)=MIN(tsurf(i),tice)
          tsea (i)=-   tsurf(i)
       END IF
    END DO

    DO i = 1, ncols
       umom(i)=rho(i)*gu(i)*rmi(i)
       vmom(i)=rho(i)*gv(i)*rmi(i)
       am  (i)=gb100/gps(i)
       umtx(i,1,3)=(umtx(i,1,3)-am(i)*umom(i)) &
            /(umtx(i,1,2)+dtc3x*am(i)*rho(i)*rmi(i))
       umtx(i,1,4)=(umtx(i,1,4)-am(i)*vmom(i)) &
            /(umtx(i,1,2)+dtc3x*am(i)*rho(i)*rmi(i))
       !
       !     set surface stress use of pseudo winds to true winds 
       !     for output diagnostics
       !
       umom(i)=umom(i)/sinclt(i)
       vmom(i)=vmom(i)/sinclt(i)
    END DO
  END SUBROUTINE seasfc






  SUBROUTINE sextrp &
       (td    ,tg    ,tc    ,w     ,capac ,td0   ,tg0   ,tc0   ,w0    , &
       capac0,tdm   ,tgm   ,tcm   ,wm    ,capacm,istrt ,ncols ,nmax  , &
       epsflt,intg  ,latco ,imask ,nfprt)
    INTEGER, INTENT(in   ) :: istrt
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: nmax
    REAL   , INTENT(in   ) :: epsflt
    INTEGER, INTENT(in   ) :: intg
    INTEGER, INTENT(in   ) :: latco
    REAL,    INTENT(in   ) :: td    (ncols)
    REAL,    INTENT(in   ) :: tg    (ncols)
    REAL,    INTENT(in   ) :: tc    (ncols)
    REAL,    INTENT(in   ) :: w     (ncols,3)
    REAL,    INTENT(in   ) :: capac (ncols,2)
    REAL,    INTENT(inout) :: td0   (ncols)
    REAL,    INTENT(inout) :: tg0   (ncols)
    REAL,    INTENT(inout) :: tc0   (ncols)
    REAL,    INTENT(inout) :: w0    (ncols,3)
    REAL,    INTENT(inout) :: capac0(ncols,2)
    REAL,    INTENT(inout) :: tdm   (ncols)
    REAL,    INTENT(inout) :: tgm   (ncols)
    REAL,    INTENT(inout) :: tcm   (ncols)
    REAL,    INTENT(inout) :: wm    (ncols,3)
    REAL,    INTENT(inout) :: capacm(ncols,2)
    INTEGER, INTENT(in   ) :: imask (ncols)
    INTEGER, INTENT(in   ) :: nfprt             
    INTEGER :: i, nc, ii

    IF (intg == 2) THEN
       IF (istrt >= 1) THEN
          DO i = 1, nmax
             td0   (i)  =td   (i)
             tg0   (i)  =tg   (i)
             tc0   (i)  =tc   (i)
             w0    (i,1)=w    (i,1)
             w0    (i,2)=w    (i,2)
             w0    (i,3)=w    (i,3)
             capac0(i,1)=capac(i,1)
             capac0(i,2)=capac(i,2)
             IF (capac0(i,2) > 0.0 .AND. tg0(i) > 273.16) THEN
                nc=0
                DO ii = 1, ncols
                   IF (imask(ii) >= 1) nc=nc+1
                   IF (nc == i) EXIT
                END DO
                WRITE(nfprt,200)ii,latco,i,capac0(i,2),tg0(i)
             END IF
          END DO
       ELSE
          DO i = 1, nmax
             td0(i)=td0(i)+epsflt*(td(i)+tdm(i)-2.0  *td0(i))
             tg0(i)=tg0(i)+epsflt*(tg(i)+tgm(i)-2.0  *tg0(i))
             tc0(i)=tc0(i)+epsflt*(tc(i)+tcm(i)-2.0  *tc0(i))
             IF(w0    (i,1) > 0.0 ) THEN
                w0(i,1)=w0(i,1)+epsflt*(w(i,1)+wm(i,1)-2.0  *w0(i,1))
             END IF
             IF(w0    (i,2) > 0.0 ) THEN
                w0(i,2)=w0(i,2)+epsflt*(w(i,2)+wm(i,2)-2.0  *w0(i,2))
             END IF
             IF(w0    (i,3) > 0.0 ) THEN
                w0(i,3)=w0(i,3)+epsflt*(w(i,3)+wm(i,3)-2.0  *w0(i,3))
             END IF
             IF(capac0(i,1) > 0.0 ) THEN
                capac0(i,1)=capac0(i,1) &
                     +epsflt*(capac(i,1)+capacm(i,1)-2.0*capac0(i,1))
             END IF
             IF(capac0(i,2) > 0.0 ) THEN
                capac0(i,2)=capac0(i,2) &
                     +epsflt*(capac(i,2)+capacm(i,2)-2.0*capac0(i,2))
             END IF
          END DO
          DO i = 1, nmax
             tdm   (i)  =td0   (i)
             tgm   (i)  =tg0   (i)
             tcm   (i)  =tc0   (i)
             wm    (i,1)=w0    (i,1)
             wm    (i,2)=w0    (i,2)
             wm    (i,3)=w0    (i,3)
             capacm(i,1)=capac0(i,1)
             capacm(i,2)=capac0(i,2)
             IF (capacm(i,2) > 0.0) tgm(i)=MIN(tgm(i),273.06)
          END DO
          DO i = 1, nmax
             td0   (i)  =td    (i)
             tg0   (i)  =tg    (i)
             tc0   (i)  =tc    (i)
             w0    (i,1)=w     (i,1)
             w0    (i,2)=w     (i,2)
             w0    (i,3)=w     (i,3)
             capac0(i,1)=capac (i,1)
             capac0(i,2)=capac (i,2)
             IF (capac0(i,2) > 0.0 .AND. tg0(i) > 273.16) THEN
                nc=0
                DO ii = 1, ncols
                   IF (imask(ii) >= 1) nc=nc+1
                   IF (nc == i) EXIT
                END DO
                WRITE(nfprt,200)ii,latco,i,capac0(i,2),tg0(i)
             END IF
          END DO
       END IF
    ELSE
       DO i = 1, nmax
          tdm   (i)  =td   (i)
          tgm   (i)  =tg   (i)
          tcm   (i)  =tc   (i)
          wm    (i,1)=w    (i,1)
          wm    (i,2)=w    (i,2)
          wm    (i,3)=w    (i,3)
          capacm(i,1)=capac(i,1)
          capacm(i,2)=capac(i,2)
          IF (capacm(i,2) > 0.0 .AND. tgm(i) > 273.16) THEN
             nc=0
             DO ii = 1, ncols
                IF (imask(ii) >= 1) nc=nc+1
                IF (nc == i) EXIT
             END DO
             WRITE(nfprt,650)ii,latco,i,capacm(i,2),tgm(i)
          END IF
       END DO
       DO i = 1, nmax
          td0   (i)  =td   (i)
          tg0   (i)  =tg   (i)
          tc0   (i)  =tc   (i)
          w0    (i,1)=w    (i,1)
          w0    (i,2)=w    (i,2)
          w0    (i,3)=w    (i,3)
          capac0(i,1)=capac(i,1)
          capac0(i,2)=capac(i,2)
       END DO
    END IF
200 FORMAT(' CAPAC0 AND TG0 NOT CONSISTENT AT I,J,IS=',3I4, &
         ' CAPAC=',G16.8,' TG=',G16.8)
650 FORMAT(' CAPACM AND TGM NOT CONSISTENT AT I,J,IS=',3I4, &
         ' CAPAC=',G16.8,' TG=',G16.8)
  END SUBROUTINE sextrp




  ! radalb :surface albedos via two stream approximation (direct and diffuse).



  SUBROUTINE radalb ( &
       ncols ,mon   ,nmax  ,itype ,vcover,tc    , &
       tg    ,capac ,satcap,extk  ,radfac,closs ,gloss ,thermk,p1f   , &
       p2f   ,zlwup ,salb  ,tgeff ,cosz  ,nsx   ,zlt2)
    !
    !
    ! reference  : a simple biosphere model (xue et al 1991)
    !-----------------------------------------------------------------------
    !     *** indices ***
    !   cg =1...canopy
    !   cg =2...ground cover
    !   vn =1...visible      (0.0-0.7 micron)
    !   vn =2...near-infrared(0.7-3.0 micron)
    !   bd =1...beam
    !   bd =2...diffuse
    !   ld =1...live leaves
    !   ld =2...dead leaves
    !   vnt=1...visible      (0.0-0.7 micron)
    !   vnt=2...near-infrared(0.7-3.0 micron)
    !   vnt=3...thermal
    !-----------------------------------------------------------------------
    !        input parameters
    !-----------------------------------------------------------------------
    !   zlt(cg)..........leaf area index
    !   z1...............bottom height of canopy
    !   z2...............top    height of canopy
    !   ref (cg,vnt,ld)..reflectance   of vegetation
    !   tran(cg,vnt,ld)..transmittance of vegetation
    !   green (cg).......fraction of green leaf area
    !   chil  (cg).......leaf orientation factor
    !   vcover(cg).......fraction of vegetation cover
    !   soref (vnt)......ground albedo
    !   chil  (cg).......leaf orientation factor
    !   cosz.............cosine of solar zenith angle
    !   tf...............water freezing temperature
    !   tg...............ground temperature
    !   tc...............canopy leaf temperature
    !   capac(cg)........water store capacity of leaves
    !   stefan...........stefan-boltsman constant
    !-----------------------------------------------------------------------
    !     in-subr. parameters
    !-----------------------------------------------------------------------
    !   albedo(cg,vnt,bd)
    !-----------------------------------------------------------------------
    !       output parameters
    !-----------------------------------------------------------------------
    !   extk(cg,vnt,bd)..extinction coefficient
    !                    passed to subr.raduse through radsave
    !   radfac(cg,vn,bd).fractions of downward solar radiation at surface
    !                    passed to subr.raduse
    !   salb(vn,bd)......surface albedo
    !                    passed to subr.spmrad
    !   tgeff............effective ground temperature
    !                    passed to subr.spmrad
    !   thermk...........canopy emissivity
    !   radsav(1)........beam    extinction coefficient (par)
    !   radsav(2)........diffuse extinction coefficient (par)
    !   closs............radiation loss from canopy
    !   gloss............radiation loss from ground
    !-----------------------------------------------------------------------
    !
    !   ityp.......Numero das classes de solo 13 
    !   imon.......Numero maximo de meses no ano (12)
    !   icg........Parametros da vegetacao (icg=1 topo e icg=2 base)
    !   iwv........Compriment de onda iwv=1=visivel, iwv=2=infravermelho
    !              proximo, iwv=3 infravermelho termal
    !   ibd........Estado da vegetacao ibd=1 verde / ibd=2 seco 
    !   ncols......Number of grid points on a gaussian latitude circle    
    !   mon........Number of month at year (1-12)        
    !   nmax    
    !   itype......Classe de textura do solo 
    !   satcap.....saturation liquid water capacity         (m)
    !   p1f........ 
    !   p2f........ 
    !   zlwup......zlwup(i)= stefan*( fac1(i)*tc4(i)+ &
    !              (1.0  -vcover(i,1)*(1.0  -thermk(i)))*fac2(i)*tg4(i))
    !   nsx........  
    !

    INTEGER, INTENT(IN   ) :: ncols
    INTEGER, INTENT(INOUT) :: mon(ncols)                        
    INTEGER, INTENT(IN   ) :: nmax                       
    INTEGER, INTENT(IN   ) :: itype (ncols)              
    REAL,    INTENT(IN   ) :: vcover(ncols,icg)          
    REAL,    INTENT(IN   ) :: tc    (ncols)              
    REAL,    INTENT(IN   ) :: tg    (ncols)              
    REAL,    INTENT(IN   ) :: capac (ncols,2)            
    REAL,    INTENT(OUT  ) :: satcap(ncols,icg)          
    REAL,    INTENT(OUT  ) :: extk  (ncols,icg,iwv,ibd)  
    REAL,    INTENT(OUT  ) :: radfac(ncols,icg,iwv,ibd)  
    REAL,    INTENT(OUT  ) :: closs (ncols)              
    REAL,    INTENT(OUT  ) :: gloss (ncols)              
    REAL,    INTENT(OUT  ) :: thermk(ncols)              
    REAL,    INTENT(OUT  ) :: p1f   (ncols)              
    REAL,    INTENT(OUT  ) :: p2f   (ncols)              
    REAL,    INTENT(OUT  ) :: zlwup (ncols)              
    REAL,    INTENT(OUT  ) :: salb  (ncols,2,2)          
    REAL,    INTENT(OUT  ) :: tgeff (ncols)              
    REAL,    INTENT(IN   ) :: cosz  (ncols)              
    INTEGER, INTENT(IN   ) :: nsx                       
    REAL   , INTENT(in   ) :: zlt2    (ncols,icg)


    REAL :: f     (ncols)
    REAL :: deltg (ncols)
    REAL :: fmelt (ncols)
    REAL :: depcov(ncols)
    REAL :: scov  (ncols)
    REAL :: scov2 (ncols)
    REAL :: tc4   (ncols)
    REAL :: tg4   (ncols)
    REAL :: fac1  (ncols)
    REAL :: fac2  (ncols)
    REAL :: zkat  (ncols)

    INTEGER, PARAMETER :: nk=3
    REAL    :: temp(nmax,18)
    REAL    :: xmi1(12,nk)
    INTEGER :: i 
    INTEGER :: ntyp(ncols) 
    INTEGER :: monx(ncols) 
    INTEGER :: jj 
    INTEGER :: i1 
    INTEGER :: ml(nmax)
    INTEGER :: k1 
    INTEGER :: k2 
    INTEGER :: ik
    REAL    :: capaci 
    !    REAL    :: xf 
    !    REAL    :: xf2 
    !    REAL    :: sc1 
    !    REAL    :: sc2 
    REAL    :: xm1 
    !    REAL    :: xm2 
    REAL    :: xtm1 
    REAL    :: xtm2 
    REAL    :: stbi
    LOGICAL :: flagtyp(nmax)
    LOGICAL :: flagscov(nmax)

    DO i = 1, nmax
       f(i)= MAX ( cosz(i), 0.01746  )
    END DO
    !     
    !     maximum water storage values.
    !     
    DO i = 1, nmax
       deltg(i)=tf-tg(i)
       fmelt(i)=1.0
       IF (ABS(deltg(i)) < 0.5 .AND. deltg(i) > 0.0) THEN
          fmelt(i)=0.6
       END IF
    END DO
    ntyp=itype
    DO i = 1, nmax
       !ntyp=itype(i)
       satcap(i,1)=zlt2(i,1)*1.0e-4
       satcap(i,2)=zlt2(i,2)*1.0e-4
       depcov(i  )=MAX(0.0  ,capac(i,2)*5.0  -z1(ntyp(i),mon(i)))
       depcov(i  )=MIN(depcov(i),(z2(ntyp(i),mon(i))-z1(ntyp(i),mon(i)))*0.95  )
       satcap(i,1)=satcap(i,1) &
            *(1.0  -depcov(i)/(z2(ntyp(i),mon(i))-z1(ntyp(i),mon(i))))
    END DO

    DO i = 1, nmax
       scov(i)=0.0
       IF (tc(i) <= tf) THEN
          scov(i)= MIN( 0.5  , capac(i,1)/satcap(i,1))
       END IF
    END DO
    capaci=1.0  /0.004
    DO i = 1, nmax
       IF (tg(i) > tf) THEN
          scov2(i)=0.0
       ELSE
          scov2(i)=MIN( 1.0  , capac(i,2)*capaci)
       END IF
    END DO
    !     
    !     terms which multiply incoming short wave fluxes
    !     to give absorption of radiation by canopy and ground
    !     
    monx = mon
    DO i = 1, nmax
       IF (fmelt(i) == 1.0) THEN
          ml(i) = 1
       ELSE
          ml(i) = 2
       END IF
    END DO
    ntyp=itype
    !TO DO i = 1, nmax
    !TO    mon(i) = monx(i)
    !TO    !xf = f(i)
    !TO    !xf2 = xf * xf
    !TO    !ntyp=itype(i)
    !TO    IF (ntyp(i) == 13) ntyp(i) = 11
    !TO    DO jj = 1, nk
    !TO       xmi1(mon(i),jj) = xmiu(mon(i),jj)
    !TO    END DO
    !TO    IF (ntyp(i) == 12 .AND. nsx > 0) THEN
    !TO       ntyp(i) = 13
    !TO       mon(i) = nsx
    !TO       IF (nsx == 1 .AND. (monx(i) >= 9 .AND. monx(i) <= 11)) mon(i) = 7
    !TO       DO jj = 1, nk
    !TO          xmi1(mon(i),jj) = xmiw(mon(i),jj)
    !TO       END DO
    !TO    END IF
    !TO END DO
    DO i = 1, nmax
       mon(i) = monx(i)
       flagtyp(i) = .TRUE.
       IF (ntyp(i) == 13) ntyp(i) = 11
       IF (ntyp(i) == 12 .AND. nsx > 0) THEN
          ntyp(i) = 13
          mon(i) = nsx
          IF (nsx == 1 .AND. (monx(i) >= 9 .AND. monx(i) <= 11)) mon(i) = 7
        flagtyp(i) = .FALSE.
       END IF
    END DO
    DO jj = 1, nk
       DO i=1, nmax
          xmi1(mon(i),jj) = xmiu(mon(i),jj)
       END DO
    END DO
    DO jj = 1, nk
       DO i=1, nmax
          IF (.NOT.flagtyp(i))xmi1(mon(i),jj) = xmiw(mon(i),jj)
       END DO
    END DO
    !
    !        snow free case
    !
    DO i = 1, nmax
       flagscov(i) = scov(i) < 0.025 .AND. scov2(i) < 0.025 
    END DO

    DO i1 = 1, 9
       DO i = 1, nmax
          IF (flagscov(i)) THEN
             temp(i,i1) = cledir(ntyp(i),mon(i),i1,1) + cledir(ntyp(i),mon(i),i1,2) &
                  * f(i) + cledir(ntyp(i),mon(i),i1,3) * (f(i)*f(i))
             temp(i,i1+9) = cledfu(ntyp(i),mon(i),i1)
          END IF
       END DO
    END DO
    flagscov = .NOT. flagscov
    DO i1 = 1, 9
       DO i = 1, nmax
          IF (flagscov(i)) THEN
             !
             !     with snow cover
             !
             temp(i,i1) = cedir(ntyp(i),mon(i),i1,1) + f(i) * &
                  cedir(ntyp(i),mon(i),i1,2) + cedir(ntyp(i),mon(i),i1,3) * (f(i)*f(i))
             temp(i,i1+9) = cedfu(ntyp(i),mon(i),i1)
          END IF
       END DO
    END DO
    DO i1 = 1, 6
       DO i = 1, nmax
          IF (flagscov(i) .AND. ntyp(i) == 11) THEN
             !sc2 = scov2(i) * scov2(i)
             !sc1 = scov2(i)        
             temp(i,i1)=cedir2(ml(i),ntyp(i),mon(i),i1,nk,1)+ &
                  cedir2(ml(i),ntyp(i),mon(i),i1,nk,2) &
                  *scov2(i) + cedir2(ml(i),ntyp(i),mon(i),i1,nk,3) *(scov2(i) * scov2(i)) + temp(i,i1)
             temp(i,i1+9) = cedfu2(ml(i),ntyp(i),mon(i),i1,1) +  &
                  cedfu2(ml(i),ntyp(i),mon(i),i1,2) &
                  * scov2(i) + cedfu2(ml(i),ntyp(i),mon(i),i1,3) * (scov2(i) * scov2(i)) + temp(i,i1+9)
          END IF
       END DO
    END DO
    DO i = 1, nmax
       IF (flagscov(i) .AND. ntyp(i) /= 11) THEN
          k2 = 1
          k1 = 2
          DO ik = nk, 1, -1
             IF (f(i) >= xmi1(mon(i),ik)) THEN
                CONTINUE
             ELSE
                k1 = ik + 1
                k2 = ik
                EXIT
             END IF
          END DO
          !xm2 = xmi1(mon(i),k2)
          IF (k1 <= nk) xm1 = xmi1(mon(i),k1)
          !
          !     snow cover at 1st layer
          !
          IF (scov(i) > 0.025) THEN
             !sc2 = scov(i) * scov(i)
             !sc1 = scov(i)
             IF (k2 >= nk .OR. k2 <= 1) THEN
                DO i1 = 1, 6
                   temp(i,i1)=cedir1(ml(i),ntyp(i),mon(i),i1,k2,1)+ &
                        cedir1(ml(i),ntyp(i),mon(i),i1,k2,2)*scov(i) + &
                        cedir1(ml(i),ntyp(i),mon(i),i1,k2,3)*(scov(i) * scov(i)) + temp(i,i1)
                END DO
             ELSE
                DO i1 = 1, 6
                   xtm1=cedir1(ml(i),ntyp(i),mon(i),i1,k1,1)+ &
                        cedir1(ml(i),ntyp(i),mon(i),i1,k1,2)*scov(i) + &
                        cedir1(ml(i),ntyp(i),mon(i),i1,k1,3)*(scov(i) * scov(i))
                   xtm2=cedir1(ml(i),ntyp(i),mon(i),i1,k2,1)+ &
                        cedir1(ml(i),ntyp(i),mon(i),i1,k2,2)*scov(i)+ &
                        cedir1(ml(i),ntyp(i),mon(i),i1,k2,3) *(scov(i) * scov(i))
                   temp(i,i1) = (xtm1*((xmi1(mon(i),k2))-f(i))+xtm2*(f(i)-xm1))/((xmi1(mon(i),k2))-xm1) &
                        + temp(i,i1)
                END DO
             END IF
             DO i1 = 1, 6
                temp(i,i1+9) = cedfu1(ml(i),ntyp(i),mon(i),i1,1) +  &
                     cedfu1(ml(i),ntyp(i),mon(i),i1,2)*scov(i) + &
                     cedfu1(ml(i),ntyp(i),mon(i),i1,3) * (scov(i) * scov(i)) + temp(i,i1+9)
             END DO
          END IF
          !     
          !     snow cover on ground
          !     
          IF (scov2(i) > 0.025) THEN
             !sc2 = scov2(i) * scov2(i)
             !sc1 = scov2(i)
             IF (k2 >= nk .OR. k2 <= 1) THEN
                DO i1 = 1, 6
                   temp(i,i1)=cedir2(ml(i),ntyp(i),mon(i),i1,k2,1)+ &
                        cedir2(ml(i),ntyp(i),mon(i),i1,k2,2)*scov2(i) +  &
                        cedir2(ml(i),ntyp(i),mon(i),i1,k2,3)*(scov2(i) * scov2(i)) + temp(i,i1)
                END DO
             ELSE
                DO i1 = 1, 6
                   xtm1=cedir2(ml(i),ntyp(i),mon(i),i1,k1,1)+ &
                        cedir2(ml(i),ntyp(i),mon(i),i1,k1,2)*scov2(i) + &
                        cedir2(ml(i),ntyp(i),mon(i),i1,k1,3) *(scov2(i) * scov2(i))
                   xtm2=cedir2(ml(i),ntyp(i),mon(i),i1,k2,1)+ &
                        cedir2(ml(i),ntyp(i),mon(i),i1,k2,2)*scov2(i)+ &
                        cedir2(ml(i),ntyp(i),mon(i),i1,k2,3) *(scov2(i) * scov2(i))
                   temp(i,i1) = (xtm1*((xmi1(mon(i),k2))-f(i))+xtm2*(f(i)-xm1))/((xmi1(mon(i),k2))-xm1) &
                        + temp(i,i1)
                END DO
             END IF
             DO i1 = 1, 6
                temp(i,i1+9) = cedfu2(ml(i),ntyp(i),mon(i),i1,1) +  &
                     cedfu2(ml(i),ntyp(i),mon(i),i1,2)* scov2(i) +  &
                     cedfu2(ml(i),ntyp(i),mon(i),i1,3) * (scov2(i) * scov2(i)) + temp(i,i1+9)
             END DO
          END IF
       END IF
    END DO
    !500    CONTINUE
    DO i = 1, nmax
       radfac(i,1,1,2) = temp(i,10)
       radfac(i,1,2,2) = temp(i,11)
       radfac(i,2,1,2) = temp(i,12)
       radfac(i,2,2,2) = temp(i,13)
       salb(i,1,2) = temp(i,14)
       salb(i,2,2) = temp(i,15)
       p2f(i) =  temp(i,16)
       extk(i,1,1,2) = temp(i,17)
       extk(i,2,1,2) = temp(i,18)
       radfac(i,1,1,1) = temp(i,1)
       radfac(i,1,2,1) = temp(i,2)
       radfac(i,2,1,1) = temp(i,3)
       radfac(i,2,2,1) = temp(i,4)
       salb(i,1,1) = temp(i,5)
       salb(i,2,1) = temp(i,6)
       p1f(i) =  temp(i,7)
       extk(i,1,1,1) = temp(i,8) / f(i)
       extk(i,2,1,1) = temp(i,9) / f(i)
       extk(i,1,3,1) = cether(ntyp(i),mon(i),1)
       extk(i,1,3,2) = cether(ntyp(i),mon(i),2)
       extk(i,2,3,1) = cether(ntyp(i),mon(i),1)
       extk(i,2,3,2) = cether(ntyp(i),mon(i),2)
    END DO
    mon = monx
    !     
    !     long-wave flux terms from canopy and ground
    !     
    stbi=1.0  /stefan
    DO  i = 1, nmax
       tc4(i)=tc(i)*tc(i)*tc(i)*tc(i)
       tg4(i)=tg(i)*tg(i)*tg(i)*tg(i)
       !ntyp=itype(i)
       zkat(i)=extk(i,1,3,2)*zlt2(i,1)/vcover(i,1)
       zkat(i)=MAX(expcut  ,-zkat(i) )
       zkat(i)=MIN(-10.0e-5, zkat(i) )
       thermk(i)=EXP(zkat(i))
       fac1 (i)=vcover(i,1)*( 1.0  -thermk(i) )
       fac2 (i)=1.0
       closs(i)=2.0  *fac1(i)*stefan*tc4(i)
       closs(i)=closs(i)-fac2(i)*fac1(i)*stefan*tg4(i)
       gloss(i)= fac2(i)*stefan*tg4(i)
       gloss(i)= gloss(i)-fac1(i)*fac2(i)*stefan*tc4(i)
       !     
       !     effective surface radiative temperature ( tgeff )
       !     
       zlwup(i)= stefan*( fac1(i)*tc4(i)+ &
            (1.0  -vcover(i,1)*(1.0  -thermk(i)))*fac2(i)*tg4(i))
       tgeff(i)=SQRT ( SQRT (( zlwup(i)*stbi )))
    END DO
  END SUBROUTINE radalb



  ! vegin  :reads vegetation morphological and physiological data.




  SUBROUTINE vegin ( si1   , sl1)

    REAL,  INTENT(in   ) :: si1   
    REAL,  INTENT(in   ) :: sl1

    INTEGER :: jcg
    INTEGER :: jmon
    INTEGER :: jtyp
    INTEGER :: iv
    INTEGER :: im
    INTEGER :: i
    INTEGER :: it
    INTEGER :: m 
    REAL    :: f0001
    REAL    :: yhil (2)
    REAL    :: dz
    REAL    :: dzcut
    !
    ALLOCATE(cedfu (13,12, 9)           )
    ALLOCATE(cedir (13,12, 9,3)      )
    ALLOCATE(cedfu1( 2,13,12,6,3)    )
    ALLOCATE(cedir1( 2,13,12,6,3,3)  )
    ALLOCATE(cedfu2( 2,13,12,6,3)    )
    ALLOCATE(cedir2( 2,13,12,6,3,3)  )
    ALLOCATE(cledfu(13,12, 9)           )
    ALLOCATE(cledir(13,12, 9,3)      )
    ALLOCATE(xmiu  (12, 3)           )
    ALLOCATE(cether(13,12, 2)           )
    ALLOCATE(xmiw  (12, 3)           )
    !    
    ALLOCATE(ystpar(2,3)             ) 
    ALLOCATE(yopt  (2)               ) 
    ALLOCATE(yll   (2)               ) 
    ALLOCATE(yu    (2)               ) 
    ALLOCATE(yefac (2)               ) 
    ALLOCATE(yh1   (2)               )
    ALLOCATE(yh2   (2)               )
    ALLOCATE(yootd (2)               )
    ALLOCATE(yreen (12,2)            )
    ALLOCATE(ycover(12,2)            )
    ALLOCATE(ylt   (12,2)            )
    !
    !     vegetation and soil parameters
    !     
    ALLOCATE(rstpar_fixed(ityp,icg,iwv)   )
    ALLOCATE(chil_fixed  (ityp,icg)          )
    ALLOCATE(topt_fixed  (ityp,icg)          )
    ALLOCATE(tll_fixed   (ityp,icg)          ) 
    ALLOCATE(tu_fixed    (ityp,icg)          )
    ALLOCATE(defac_fixed (ityp,icg)          )
    ALLOCATE(ph1_fixed   (ityp,icg)          )
    ALLOCATE(ph2_fixed   (ityp,icg)          )
    ALLOCATE(rootd (ityp,icg)          )
    ALLOCATE(bee   (ityp)          )
    ALLOCATE(phsat (ityp)          )
    ALLOCATE(satco (ityp)          )
    ALLOCATE(poros (ityp)          )
    ALLOCATE(zdepth(ityp,idp)          )
    ALLOCATE(green_fixed (ityp,imon,icg)  )
    ALLOCATE(xcover_fixed(ityp,imon,icg)  )
    ALLOCATE(zlt_fixed   (ityp,imon,icg)  )
    ALLOCATE(x0x   (ityp,imon)      )
    ALLOCATE(xd    (ityp,imon)      )
    ALLOCATE(z2    (ityp,imon)      )
    ALLOCATE(z1    (ityp,imon)      )
    ALLOCATE(xdc   (ityp,imon)      )
    ALLOCATE(xbc   (ityp,imon)      )

    OPEN(nfsibd, file=TRIM(fNameSibVeg),ACTION="read",FORM="UNFORMATTED")
    OPEN(nalbyx, file=TRIM(fNameSibAlb),ACTION="read",FORM="UNFORMATTED")

    READ(nfsibd) rstpar_fixed,chil_fixed  ,topt_fixed  ,tll_fixed   , &
         tu_fixed    ,defac_fixed ,ph1_fixed   ,ph2_fixed   ,rootd , &
         bee   ,phsat ,satco ,poros , &
         zdepth

    READ(nfsibd) green_fixed ,xcover_fixed,zlt_fixed   ,x0x   ,xd    ,z2    , &
         z1    ,xdc   ,xbc


    READ(nalbyx) cedfu, cedir, cedfu1, cedir1, cedfu2, cedir2, &
         cledfu, cledir, xmiu, cether, xmiw

    REWIND nfsibd

    REWIND nalbyx

    f0001=0.0001

    DO jcg =1, 2
       DO jmon=1,12
          DO jtyp=1,ityp
             green_fixed(jtyp,jmon,jcg)=MAX(f0001,green_fixed(jtyp,jmon,jcg))
          END DO
       END DO
    END DO

    DO iv =1, 2
       jtyp = 12
       IF (iv.EQ.2) jtyp = 13

       DO  im = 1,3
          ystpar(iv,im)=rstpar_fixed(jtyp,1,im)
       END DO

       yhil  (iv)=chil_fixed  (jtyp,1)
       yopt  (iv)=topt_fixed  (jtyp,1)
       yll   (iv)=tll_fixed   (jtyp,1)
       yu    (iv)=tu_fixed    (jtyp,1)
       yefac (iv)=defac_fixed (jtyp,1)
       yootd (iv)=rootd (jtyp,1)
       yh1   (iv)=ph1_fixed   (jtyp,1)
       yh2   (iv)=ph2_fixed   (jtyp,1)

    END DO

    DO jmon=1,12
       DO iv = 1,2
          jtyp = 12
          IF (iv.EQ.2) jtyp = 13
          ylt   (jmon,iv)=zlt_fixed(jtyp,jmon,1)
          yreen (jmon,iv)=green_fixed (jtyp,jmon,1)
          ycover(jmon,iv)=xcover_fixed(jtyp,jmon,1)
       END DO
    END DO

    DO iv = 1,2
       DO im = 1,3
          rstpar_fixed(13,iv,im) = 1000.0
       END DO
       chil_fixed  (13,iv) = 0.01
       topt_fixed  (13,iv) = 310.
       tll_fixed   (13,iv) = 300.
       tu_fixed    (13,iv) = 320.
       defac_fixed (13,iv) = 0.0
       ph1_fixed   (13,iv) = 3.0
       ph2_fixed   (13,iv) = 6.0
       rootd (13,iv) = 2.1
    END DO

    bee(13) = 4.8
    phsat(13) = -0.167
    satco(13) = 0.762e-4
    poros(13) = 0.4352

    DO i = 1, imon
       zlt_fixed(13,i,1) = 0.0001
       zlt_fixed(13,i,2) = 0.0001
       z2(13,i) = 0.1
       z1(13,i) = 0.0001
       xcover_fixed(13,i,1) = 0.0001
       xcover_fixed(13,i,2) = 0.0001
       x0x(13,i) = 0.01
       xd(13,i) = 0.0004
       xbc(13,i) = 35461.0
       xdc(13,i) = 28.5
    END DO

    zdepth(13,1) = 1.0
    zdepth(13,2) = 1.0
    zdepth(13,3) = 1.0

    !Manzi Suggestion
    ! dz - mean height of the first model layer
    dz=(gasr*tf/grav)*LOG(si1/sl1)
    ! SiB calibration values 
    ! 45 m - height of the first tower level of measurements
    ! 27 m - maximum calibrated displacement height
    dzcut=MIN(45.0-27.0,dz)
    WRITE (nfprt, '(/,2(A,F10.5),/)') &
         ' From vegin: dz = ',dz,' dzcut = ', dzcut
    DO it=1,ityp
       DO m=1,imon
          ! fixed displacement height higher than mean height of the first model layer
          ! in this case there is no sense to consider displacement height
          IF (xd(it,m) > dz) THEN
             WRITE (nfprt, '(2(A,I5),1P,2(A,G12.5))') &
                  ' it= ',it,' m= ',m,' xd= ',xd(it,m),' xdnew = ',0.0
             xd(it,m)=0.0
             ! fixed displacement height too close top of canopy
             ! it is necessary to lower the displacement height
             ! it is used the difference dzcut obtained experimentally to lower d
          ELSE IF ((dz-xd(it,m)) < dzcut) THEN
             WRITE (nfprt, '(2(A,I5),1P,2(A,G12.5))') &
                  ' it= ',it,' m= ',m,' xd= ',xd(it,m),' xdnew = ',MAX(0.0,dz-dzcut)
             xd(it,m)=MAX(0.0,dz-dzcut)
          END IF
       END DO
    END DO
    WRITE (nfprt, '(/,A,/)') ' End vegin'

  END SUBROUTINE vegin



  ! wheat  :determine wheat phenology for latitude and julian day?.



  SUBROUTINE wheat (itype ,nmax  ,colrad ,month ,xday   ,yrl   , &
       idatec,monl  ,nsx    ,vcover ,zlt2  , &
       green2,chil2 ,rstpar2,topt2 ,tll2   ,tu2   , &
       defac2,ph12  ,ph22, xcover, zlt, green, ph2, ph1, defac, tu, tll, topt, rstpar, chil)
    !==========================================================================
    !==========================================================================
    !  ityp.......Numero das classes de solo vegetacao 13 
    !  imon.......Number max of month at year (12)
    !  icg........Parameters of vagetation (icg=1 top e icg=2 bottom)
    !  iwv........Comprimento de onda iwv=1=visivel, iwv=2=infravermelho
    !             proximo, iwv=3 infravermelho termal
    !  nmax
    !  itype......Classe de textura do solo
    !  jmax.......Number of grid points on a gaussian longitude circle
    !  colrad.....colatitude
    !  month......Number of month at year (1-12)
    !  xday.......is julian day - 1 with fraction of day
    !  pie........Constante Pi=3.1415926e0     
    !  yrl........length of year in days               
    !  idatec.....idatec(1)=current hour of
    !            idatec(2)=current day of month.
    !            idatec(3)=current month of year.
    !            idatec(4)=current year.
    !  monl.......length of each month in days       
    !  ystpar.....Coefficints related to par influence on
    !             stomatal resistance     
    !  yopt.......Temperatura ideal de funcionamento estomatico       
    !  yll........Temperatura minima de funcionamento estomatico        
    !  yu.........Temperatura maxima de funcionamento estomatico         
    !  yefac......Parametro de deficit de pressao de vapor d'agua      
    !  yh1........Coeficiente para o efeito da agua no solo        
    !  yh2........Potencial de agua no solo para ponto de Wilting           
    !  rstpar.....Coefficints related to par influence on
    !             stomatal resistance
    !  chil.......Leaf orientation parameter   
    !  topt.......Temperatura ideal de funcionamento estomatico         
    !  tll........Temperatura minima de funcionamento estomatico    
    !  tu.........Temperatura maxima de funcionamento estomatico     
    !  defac......Parametro de deficit de pressao de vapor d'agua 
    !  ph1........Coeficiente para o efeito da agua no solo        
    !  ph2........Potencial de agua no solo para ponto de Wilting   
    !  green......Fraction of grenn leaves 
    !  xcover(iv).Fracao de cobertura de vegetacao iv=1 Top 
    !  xcover(iv).Fracao de cobertura de vegetacao iv=2 Bottom   
    !  zlt(icg)...Indice de area foliar "LEAF AREA INDEX" icg=1 topo da copa 
    !  zlt(icg)...Indice de area foliar "LEAF AREA INDEX" icg=2 base da copa 
    !  nsx........phenology dates to fall within one year period              
    !==========================================================================

    INTEGER, INTENT(in ) :: nmax 
    INTEGER, INTENT(in ) :: itype (nmax)
    REAL,    INTENT(in ) :: colrad(nmax)
    INTEGER, INTENT(in ) :: month (nmax)
    REAL,    INTENT(in ) :: xday
    REAL,    INTENT(in ) :: yrl                  
    INTEGER, INTENT(in ) :: idatec(4)            
    INTEGER, INTENT(in ) :: monl  (12)           
    INTEGER, INTENT(out) :: nsx                  
    REAL   , INTENT(out) :: vcover  (nmax,icg)       
    REAL   , INTENT(out) :: zlt2    (nmax,icg)
    REAL   , INTENT(out) :: green2  (nmax,icg)
    REAL   , INTENT(out) :: chil2   (nmax,icg)
    REAL   , INTENT(out) :: rstpar2 (nmax,icg,iwv)
    REAL   , INTENT(out) :: topt2   (nmax,icg)
    REAL   , INTENT(out) :: tll2    (nmax,icg)
    REAL   , INTENT(out) :: tu2          (nmax,icg)
    REAL   , INTENT(out) :: defac2  (nmax,icg)
    REAL   , INTENT(out) :: ph12    (nmax,icg)
    REAL   , INTENT(out) :: ph22    (nmax,icg)    
    REAL   , INTENT(INOUT) :: xcover(ityp, imon, icg)
    REAL   , INTENT(INOUT) :: zlt(ityp, imon, icg)
    REAL   , INTENT(INOUT) :: green(ityp,imon,icg)
    REAL   , INTENT(INOUT) :: ph2(ityp,icg)
    REAL   , INTENT(INOUT) :: ph1(ityp,icg)
    REAL   , INTENT(INOUT) :: defac(ityp,icg)
    REAL   , INTENT(INOUT) :: tu(ityp,icg)
    REAL   , INTENT(INOUT) :: tll(ityp,icg)
    REAL   , INTENT(INOUT) :: topt(ityp,icg)
    REAL   , INTENT(INOUT) :: rstpar(ityp,icg,iwv)
    REAL   , INTENT(INOUT) :: chil(ityp,icg)
    REAL    :: rday 
    REAL    :: thrsh 
    REAL    :: phi(nmax) 
    REAL    :: flip 
    REAL    :: tfac 
    REAL    :: demg 
    REAL    :: rootgc 
    REAL    :: chilw 
    REAL    :: tlai
    REAL    :: grlf 
    REAL    :: top 
    REAL    :: bot 
    REAL    :: diff1 
    REAL    :: diff2 
    REAL    :: perc 
    REAL    :: x1 
    REAL    :: xdif1 
    REAL    :: xdif2 
    REAL    :: zltgmx
    INTEGER :: i 
    INTEGER :: iityp 
    INTEGER :: kold 
    INTEGER :: i1 
    INTEGER :: ns 
    INTEGER :: mind 
    INTEGER :: icond 
    INTEGER :: kk 
    INTEGER :: mnl 
    INTEGER :: nn
    INTEGER :: ntyp
    REAL    :: phenst(9)
    INTEGER, PARAMETER :: iimon=12

    REAL, PARAMETER :: wlai(9)=(/1.0, 2.0, 6.0, 4.0, 3.0, 1.0, 0.01, 0.01, 1.0/)

    REAL, PARAMETER :: xgren(iimon+1)=(/0.55,0.68,0.8,0.9,0.9,0.9,0.9,0.81,0.64,0.53, &
         0.49,0.48,0.55/)

    REAL, PARAMETER :: vlt(iimon+1)=(/1.,6.,6.,6.,6.,6.,6.,6.,6.,3.78,1.63,1.,1./)

    !    INTEGER, SAVE :: kmon(iimon+1)

    REAL :: xgreen(iimon+1)
    INTEGER :: kmon(imon+1)

    INTEGER, PARAMETER :: ihead = 3
    INTEGER, PARAMETER :: iwheat=12
    REAL,    PARAMETER :: syr   =365.25e0
    REAL,    PARAMETER :: vcv   =0.569

    !
    !     vlt and xgren are assumed to be correct at the beginning of the 
    !     month
    !     
    nsx = 0
    !
    !     xday is julian day - 1 with fraction of day
    !
    rday=xday
    !
    !     for standard length years, determine the offset for the year
    !     within the leap year period
    !
    thrsh=0.0e0
    IF (yrl == syr) thrsh=-MOD(idatec(4)+3,4)*0.25e0
    DO i = 1, nmax
       phi(i) = 90.0    -180.0e0/pie * colrad(i)
       !     
       !     constrain latitude range
       !  
       IF (ABS(phi(i)) > 55.) phi(i)=SIGN(55.,phi(i))
       IF (ABS(phi(i)) < 20.) phi(i)=SIGN(20.,phi(i))
    ENDDO
    !     
    !     search for any wheat vegetation points at this latitude
    !     if found, set sib parameters for latitude and time of year
    !     
    DO i = 1, nmax
       iityp=itype(i)
       IF (iityp /= iwheat) THEN 
          CYCLE
       ELSE
          kold=0
          DO i1 = 1, 13
             xgreen(i1)=xgren(i1)
             kmon(i1)=kold
             !     
             !     add extra day for leap years if using standard length year
             !     
             IF (yrl == syr .AND. MOD(idatec(4),4) == 0 .AND. i1 > 2) &
                  kmon(i1)=kmon(i1)+1
             IF (i1 < 13) kold=kold+monl(i1)
          END DO
          flip =   0.0
          IF (phi(i)< 0.0e0) flip = yrl/2.0
          tfac=yrl/syr
          !     
          !     determine julian day - 1 for each wheat phenology for this 
          !     latitude.  scale by length of year and adjust for south. hem.
          !     
          phenst(2) = (4.50    *ABS(phi(i)) - 65.0)*tfac+ flip
          phenst(3) = (4.74    *ABS(phi(i)) - 47.2)*tfac+ flip
          phenst(4) = (4.86    *ABS(phi(i)) - 31.8)*tfac+ flip
          phenst(5) = (4.55    *ABS(phi(i)) -  2.0)*tfac+ flip
          phenst(6) = (4.35    *ABS(phi(i)) + 10.5)*tfac+ flip
          phenst(7) = phenst(6) + 3.0*tfac
          demg      = ABS( 5.21    *ABS(phi(i)) - 0.3    )*tfac
          phenst(1) = phenst(2) - demg
          phenst(9) = phenst(1)
          phenst(8) = phenst(9) - 5.0*tfac
          !     
          !     constrain phenology dates to fall within one year period
          !     
          DO ns = 1, 9
             IF (phenst(ns) < 0.0e0) phenst(ns) = phenst(ns) + yrl
             IF (phenst(ns) > yrl  )phenst(ns) = phenst(ns) - yrl
          END DO
          !     
          !     find month of the head phenology stage for this latitude
          !     
          DO i1 = 1, 12
             IF (phenst(ihead) <= kmon(i1+1)) THEN
                mind = i1
                IF (i1 <= 4) THEN
                   xgreen(i1+1) = 0.9
                   xgreen(i1+2) = 0.9
                END IF
                EXIT
             END IF
          END DO

          rootgc = 1.0
          chilw  =-0.02
          tlai   = 0.5
          grlf   = 0.6
          !     
          !     find growth stage given latitude and day
          !     
          DO ns = 1,8
             top = phenst(ns+1)
             bot = phenst(ns)
             diff1 = top-bot
             diff2 = rday-bot

             IF ((rday < bot) .OR. (rday > top)) THEN
                IF (bot < top) CYCLE
                !     
                !     phenology stages overlap the end of year?
                !     
                icond = 0
                IF (rday >= bot   .AND. rday <= yrl) icond = 1
                IF (rday >= thrsh .AND. rday <= top) icond = 2
                IF (icond == 0) CYCLE
                IF (icond /= 2) THEN
                   diff1 = yrl    - bot + top
                   diff2 = rday     - bot
                ELSE
                   diff1 = yrl   - bot + top
                   diff2 = yrl   - bot + rday
                END IF
             END IF
             !     
             !     date found in phenology stage
             !     
             perc =  diff2/diff1
             !     
             !     kk is current month number
             !     
             kk=idatec(2)
             mnl=monl(kk)

             IF (yrl == syr .AND. MOD(idatec(4),4) == 0 .AND. kk == 2) &
                  mnl=mnl+1

             IF (rday > phenst(ihead)) THEN

                IF (kk /= mind) THEN
                   x1 = vlt(kk)
                   xdif1 = mnl
                   xdif2 = rday - kmon(kk)
                ELSE
                   x1 = wlai(ihead)
                   xdif1 = kmon(kk+1) - phenst(ihead)
                   xdif2 = rday - phenst(ihead)
                END IF

                tlai = x1 - (x1-vlt(kk+1)) / xdif1 * xdif2

             ELSE
                tlai =  perc*(wlai(ns+1)-wlai(ns)) + wlai(ns)
             END IF

             IF (rday > phenst(ihead+1)) THEN
                xcover(iwheat,month(i),1)=vcv+(0.9-vcv) &
                     *(yrl-rday)/(yrl-phenst(ihead+1))
             ELSE
                xcover(iwheat,month(i),1)=0.90*(1.0 - EXP(-tlai))
             END IF

             grlf = xgreen(kk)-(xgreen(kk)-xgreen(kk+1)) &
                  /mnl*(rday-kmon(kk))
             zltgmx = wlai(ihead)
             rootgc = 2910.0    * (0.5    +0.5    *tlai/zltgmx * grlf)

             IF (ns /= 1 .AND. ns /= 2) chilw=-0.2
             !EXIT
          END DO

          zlt   (iwheat,month(i),1) = tlai 
          green (iwheat,month(i),1) = grlf
          chil  (iwheat,1)          = chilw
          topt  (iwheat,1)          = yopt(2)
          tll   (iwheat,1)          = yll (2)
          tu    (iwheat,1)          = yu  (2)
          defac (iwheat,1)          = yefac(2)
          ph1   (iwheat,1)          = yh1(2)
          ph2   (iwheat,1)          = yh2(2)

          DO  nn = 1, 3
             rstpar(iwheat,1,nn) = ystpar(2,nn)
          END DO

          nsx = ns

          IF (nsx == 9) nsx = 1
          IF (nsx >  6) nsx = 6

       END IF
       ntyp=itype(i)
       topt2  (i,1) =  topt  (ntyp,1)
       tll2      (i,1) =  tll   (ntyp,1)
       tu2      (i,1) =  tu    (ntyp,1)
       defac2 (i,1) =  defac (ntyp,1)
       ph12      (i,1) =  ph1   (ntyp,1)
       ph22      (i,1) =  ph2   (ntyp,1)
       DO  nn = 1, 3
        rstpar2 (i,1,nn)= rstpar(ntyp,1,nn)
       END DO
       vcover (i,1) =  xcover(ntyp,month(i),1)
       zlt2      (i,1) =  zlt   (ntyp,month(i),1)
       green2 (i,1) =  green (ntyp,month(i),1)
       chil2  (i,1) =  chil  (ntyp,1)
    END DO
    RETURN
  END SUBROUTINE wheat



  ! sibwet :transform mintz-serafini and national meteorological center fields
  !         of soil moisture into sib compatible fields of soil moisture.




  SUBROUTINE sibwet &
       (ibmax,jbmax,sinp,sinmax,imask,wsib,ssib,mxiter,nfprt,ifprt,ibMaxPerJB)
    !
    !
    !     piers sellers : 29 april 1987
    !
    !     
    !   input  :   sinp     = mintz-serafini or national meteorological
    !                         center soil moisture (mm)
    !              sinmax   = maximum value of sinp (mm)
    !              wsinp    = m-s or nmc fractional wetness
    !              ms       = 1, mintz-serafini
    !              nmc      = 1, national meteorological center
    !              bee      = sib : soil moisture potential factor
    !              phsat    = sib : soil potential at saturation (m)
    !              zdepth(3)= sib : depth of 3 soil layers (m)
    !              poros    = sib : soil porosity
    !
    !   output :   wsibt    = sib : fractional wetness
    !              ssibt    = sib : soil moisture content (m)
    !              psit     = sib : soil moisture potential (m)
    !              factor   = sib : extraction factor
    !
    INTEGER, INTENT(in   ) :: ibmax
    INTEGER, INTENT(in   ) :: jbmax 
    INTEGER, INTENT(in   ) :: mxiter
    REAL   , INTENT(in   ) :: sinp(ibmax,jbmax)
    REAL   , INTENT(in   ) :: sinmax
    !

    INTEGER, INTENT(in   ) :: imask (ibmax,jbmax)
    REAL   , INTENT(out  ) :: wsib  (ibmax,jbmax)
    REAL   , INTENT(out  ) :: ssib  (ibmax,jbmax)
    INTEGER, INTENT(in   ) :: nfprt
    INTEGER, INTENT(in   ) :: ifprt(100)
    INTEGER, INTENT(in   ) :: ibMaxPerJB(:)

    REAL :: sm(ityp,mxiter)
    REAL :: time(ityp,mxiter)
    REAL :: fact(ityp,mxiter)

    REAL, PARAMETER :: xph1(13,2) = RESHAPE( &
         (/-100.,-190.,-200.,-200.,-200.,-120.,-120.,-120.,-200., &
         -200., -10.,-190., -10.,-100.,-190.,-200.,-200.,-200., &
         -120.,-120.,-120.,-200.,-200., -10.,-190., -10./), &
         (/13,2/))
    REAL, PARAMETER :: xph2(13,2) = RESHAPE( &
         (/-500.,-250.,-250.,-250.,-250.,-230.,-230.,-280.,-400., &
         -400.,-100.,-250.,-100.,-500.,-250.,-250.,-250.,-250., &
         -230.,-230.,-280.,-400.,-400.,-100.,-250.,-100./) , &
         (/13,2/))

    REAL    :: tzdep(3) 
    REAL    :: tzltm(2)
    REAL    :: sibmax(ityp)
    REAL    :: tphsat
    REAL    :: tbee
    REAL    :: tporos
    INTEGER :: imm1
    INTEGER :: imm2
    INTEGER :: is
    INTEGER :: im
    INTEGER :: imm
    INTEGER :: ivegm
    REAL    :: cover
    REAL    :: tph1
    REAL    :: tph2
    REAL    :: sref
    REAL    :: smin
    REAL    :: dssib
    REAL    :: dw
    REAL    :: times
    REAL    :: soilmo
    REAL    :: w
    REAL    :: rsoilm
    INTEGER :: iter
    INTEGER :: latmax 
    INTEGER :: lonmax 
    INTEGER :: lat 
    INTEGER :: lon
    REAL    :: tsinp 
    REAL    :: etp
    REAL    :: facmod
    REAL    :: ssibt
    REAL    :: psit
    REAL    :: factor
    REAL    :: dt
    INTEGER :: itsoil
    INTEGER :: itfac

    sm=0.0
    time=0.0
    fact=0.0
    ssib=0.0
    wsib=0.0

    lonmax=ibmax
    latmax=jbmax

    DO is = 1,ityp
       tzdep (1)= zdepth(is,1)
       tzdep (2)= zdepth(is,2)
       tzdep (3)= zdepth(is,3)
       tphsat   = phsat (is)
       tbee     = bee   (is)
       tporos   = poros (is)
       imm1=1
       imm2=1
       tzltm(1)=zlt_fixed(is,1,1)
       tzltm(2)=zlt_fixed(is,1,2)
       DO im=2,12
          IF(tzltm(1).LE.zlt_fixed(is,im,1) ) THEN
             imm1=im
             tzltm(1)=zlt_fixed(is,im,1)
          END IF
          IF(tzltm(2).LE.zlt_fixed(is,im,2) )THEN
             imm2=im
             tzltm(2)=zlt_fixed(is,im,2)
          END IF
       END DO
       imm=imm1
       ivegm=1
       IF(tzltm(1).LE.tzltm(2)) THEN
          imm=imm2
          ivegm=2
       END IF
       cover=xcover_fixed(is,imm,ivegm)
       tph1=xph1(is,ivegm)
       tph2=xph2(is,ivegm)
       sibmax(is) = ( tzdep(1) + tzdep(2) + tzdep(3) ) * tporos
       IF(ifprt(83).GE.1)WRITE(nfprt,999)is,sibmax(is),tzdep(1), &
            tzdep(2),tzdep(3),tporos
       sref = sibmax(is) * EXP( LOG(tphsat /(-1.0e0)) /tbee )
       smin = sibmax(is) * EXP( LOG(tphsat /(-1.0e10)) /tbee )
       dssib= (sref - smin) / float(mxiter)
       dw   = dssib / sibmax(is)
       times  = 0.0e0
       soilmo = sref
       w      = soilmo / sibmax(is)
       rsoilm = 101840. * (1.0 - w**0.0027)
       DO iter = 1, mxiter
          CALL extrak      ( w, dw, tbee, tphsat, rsoilm, cover, &
               tph1, tph2, psit, factor )
          dt            = dssib / factor
          soilmo        = soilmo - dssib
          w             = soilmo / sibmax(is)
          times         = times + dt
          sm  (is,iter) = soilmo
          time(is,iter) = times
          fact(is,iter) = factor
       END DO
    END DO
    !     
    !     input soil moisture map is now transformed to sib fields.
    !     
    DO lat = 1, latmax
       DO lon = 1, ibMaxPerJB(lat)
          is=imask(lon,lat)
          IF(is.NE.0)THEN
             tsinp = sinp(lon,lat)
             tsinp = MAX (sinmax/100.0e3 , tsinp )
             tsinp = MIN (sinmax,tsinp)
             IF (tsinp .GT. 0.75e0*sinmax ) etp = sinmax - tsinp
             facmod=MIN(1.0e0,tsinp/(0.75e0*sinmax) )
             IF (tsinp .LE. 0.75e0*sinmax ) THEN
                etp = 0.75e0*sinmax* LOG(0.75e0*sinmax/tsinp ) + &
                     0.25e0 *sinmax
             END IF
             etp = etp / 1000.0e0
             DO iter = 1, mxiter
                itsoil=iter
                IF ( time(is,iter) - etp .GT. 0.0e0  ) EXIT
             END DO
             DO iter=1,mxiter
                itfac=iter
                IF( fact(is,iter)-facmod-0.01e0.LT.0.0e0)EXIT
             END DO
             ssibt=MIN(sm(is,itsoil),sm(is,itfac))
             DO iter=1,mxiter
                IF(ssibt.GT.sm(is,iter))EXIT
             END DO
             ssib(lon,lat) = sm(is,iter)
             wsib(lon,lat) = sm(is,iter) / sibmax(is)
          END IF
       END DO
    END DO
999 FORMAT(' IS,MAX,D1,D2,D3,POROS=',I2,1X,5E12.5)
  END SUBROUTINE sibwet






  SUBROUTINE extrak( w, dw, tbee, tphsat, rsoilm, cover, tph1, tph2, &
       psit, factor )
    REAL, INTENT(in   ) :: w 
    REAL, INTENT(in   ) :: dw 
    REAL, INTENT(in   ) :: tbee 
    REAL, INTENT(in   ) :: tphsat 
    REAL, INTENT(in   ) :: rsoilm 
    REAL, INTENT(in   ) :: cover 
    REAL, INTENT(in   ) :: tph1 
    REAL, INTENT(in   ) :: tph2
    REAL, INTENT(out  ) :: psit 
    REAL, INTENT(out  ) :: factor
    REAL :: rsoil
    REAL :: argg
    REAL :: hr
    REAL :: rplant
    psit   = tphsat * ( w-dw/2.0e0 ) ** (-tbee)
    rsoil  = 101840. * (1.0-( w-dw/2.0) ** 0.0027)
    argg   = MAX ( -10.0e0 , (psit*9.81e0 / 461.5e0 / 310.e0) )
    hr     = EXP ( argg )
    rsoil  = rsoilm /rsoil * hr
    rplant = ( psit - tph2 -50.0) / ( tph1 - tph2 )
    rplant = MAX ( 0.0e0, MIN ( 1.0e0, rplant ) )
    factor = cover * rplant + ( 1.0e0 - cover ) * rsoil
    factor = MAX ( 1.e-6, factor )
  END SUBROUTINE extrak
END MODULE Surface
