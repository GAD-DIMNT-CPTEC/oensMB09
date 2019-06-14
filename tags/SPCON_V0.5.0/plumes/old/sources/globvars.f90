MODULE globvars
  IMPLICIT NONE
  
  LOGICAL :: Globvars_already_allocated=.FALSE.
  INTEGER,ALLOCATABLE,DIMENSION(:) :: lvar,lvari,lvarc,uvarc,uvar,iloc,jloc
  REAL,ALLOCATABLE ::    grh(:,:),del(:),topo(:,:),top(:)
  REAL ,ALLOCATABLE ::    psr(:),pcr(:),usr(:),vsr(:),tsr(:),hsr(:)
  REAL ,ALLOCATABLE ::    psm(:),pcm(:),usm(:),vsm(:),tsm(:),hsm(:)
  REAL ,ALLOCATABLE ::    plr(:),plm(:)
  REAL ,ALLOCATABLE ::    ccr(:),ccm(:)
  REAL ,ALLOCATABLE ::    sdr(:),sdm(:)
  REAL ,ALLOCATABLE ::    sfr(:),sfm(:)
  REAL ,ALLOCATABLE ::    tar(:),tam(:)
  
  REAL ,ALLOCATABLE ::    psp(:,:,:),pcp(:,:,:),usp(:,:,:),vsp(:,:,:) 
  REAL ,ALLOCATABLE ::    tsp(:,:,:),hsp(:,:,:),ccp(:,:,:),sdp(:,:,:)
  REAL ,ALLOCATABLE ::    sfp(:,:,:),tap(:,:,:),tpp(:,:,:),plp(:,:,:)
  REAL ,ALLOCATABLE ::    vst(:,:,:)
  
  REAL ,ALLOCATABLE ::    psp_p(:,:,:),pcp_p(:,:,:),usp_p(:,:,:),vsp_p(:,:,:) 
  REAL ,ALLOCATABLE ::    tsp_p(:,:,:),hsp_p(:,:,:),ccp_p(:,:,:),sdp_p(:,:,:)
  REAL ,ALLOCATABLE ::    sfp_p(:,:,:),tap_p(:,:,:),tpp_p(:,:,:),plp_p(:,:,:)
  REAL ,ALLOCATABLE ::    vst_p(:,:,:)
  
  !especiais
  REAL ,ALLOCATABLE ::    max_tsp(:),max_hsp(:),max_plp(:),max_vst(:)
  REAL ,ALLOCATABLE ::    min_tsp(:),min_hsp(:),min_plp(:),min_vst(:)

  CHARACTER (LEN=4) ,ALLOCATABLE ::  avar(:),bvar(:)
  CHARACTER (LEN=11) ,ALLOCATABLE :: prx(:)
  CHARACTER (LEN=16) ,ALLOCATABLE :: aunits(:)
  CHARACTER (LEN=40) ,ALLOCATABLE :: cvar(:),cloc(:),dvar(:)
  LOGICAL ,ALLOCATABLE ::            dopt(:)

  CONTAINS
  
  LOGICAL FUNCTION Alloc_Globvars(imax,jmax,kmax,nfmx,nfmxo,npmx, &
                                   nmb,mxghsl,numx,ndt,ndtout,ndiv)
    
    INTEGER,INTENT(IN) :: imax,jmax,kmax,nfmx,nfmxo,npmx,nmb,mxghsl,numx
    INTEGER,INTENT(IN) :: ndt,ndiv,ndtout
    
    IF (Globvars_already_allocated) THEN
       Alloc_Globvars=.FALSE.
    END IF
    ALLOCATE(lvar(nfmx),lvari(nfmx),lvarc(nfmxo))  
    ALLOCATE(uvarc(nfmxo),uvar(nfmx),iloc(npmx),jloc(npmx))
    ALLOCATE(grh(npmx,mxghsl),del(kmax),topo(imax,jmax),top(npmx))
    ALLOCATE(psr(npmx),pcr(npmx),usr(npmx),vsr(npmx),tsr(npmx),hsr(npmx))
    ALLOCATE(psm(npmx),pcm(npmx),usm(npmx),vsm(npmx),tsm(npmx),hsm(npmx))
    ALLOCATE(plr(npmx),plm(npmx))
    ALLOCATE(ccr(npmx),ccm(npmx))
    ALLOCATE(sdr(npmx),sdm(npmx))
    ALLOCATE(sfr(npmx),sfm(npmx))
    ALLOCATE(tar(npmx),tam(npmx))

    ALLOCATE(psp(npmx,nmb,ndt),pcp(npmx,nmb,ndt),usp(npmx,nmb,ndt))
    ALLOCATE(vsp(npmx,nmb,ndt),tsp(npmx,nmb,ndt),hsp(npmx,nmb,ndt))
    ALLOCATE(ccp(npmx,nmb,ndt),sdp(npmx,nmb,ndt),sfp(npmx,nmb,ndt))
    ALLOCATE(tap(npmx,nmb,ndt),tpp(npmx,nmb,ndt),plp(npmx,nmb,ndt))
    ALLOCATE(vst(npmx,nmb,ndt))

    !ALLOCATE(psp_p(ndt,npmx,ndiv),pcp_p(ndt,npmx,ndiv),usp_p(ndt,npmx,ndiv))
    !ALLOCATE(vsp_p(ndt,npmx,ndiv),tsp_p(ndt,npmx,ndiv),hsp_p(ndt,npmx,ndiv))
    !ALLOCATE(ccp_p(ndt,npmx,ndiv),sdp_p(ndt,npmx,ndiv),sfp_p(ndt,npmx,ndiv))
    !ALLOCATE(tap_p(ndt,npmx,ndiv),tpp_p(ndt,npmx,ndiv),plp_p(ndt,npmx,ndiv))

    ALLOCATE(psp_p(npmx,ndiv,ndtout),pcp_p(npmx,ndiv,ndtout),usp_p(npmx,ndiv,ndtout))
    ALLOCATE(vsp_p(npmx,ndiv,ndtout),tsp_p(npmx,ndiv,ndtout),hsp_p(npmx,ndiv,ndtout))
    ALLOCATE(ccp_p(npmx,ndiv,ndtout),sdp_p(npmx,ndiv,ndtout),sfp_p(npmx,ndiv,ndtout))
    ALLOCATE(tap_p(npmx,ndiv,ndtout),tpp_p(npmx,ndiv,ndtout),plp_p(npmx,ndiv,ndtout))
    ALLOCATE(vst_p(npmx,ndiv,ndtout))
    
    !Especiais
    ALLOCATE(max_tsp(npmx),max_hsp(npmx),max_plp(npmx),max_vst(npmx))
    ALLOCATE(min_tsp(npmx),min_hsp(npmx),min_plp(npmx),min_vst(npmx))
    
    ALLOCATE(avar(nfmx),bvar(nfmxo))
    ALLOCATE(prx(npmx))
    ALLOCATE(aunits(-1:numx))
    ALLOCATE(cvar(nfmx),cloc(npmx),dvar(nfmxo))
    ALLOCATE(dopt(npmx))
    
    Globvars_already_allocated=.TRUE.
    Alloc_Globvars=.TRUE.
    
  END FUNCTION Alloc_Globvars
  
END MODULE globvars
