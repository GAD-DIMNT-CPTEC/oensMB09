SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:44 2005
FILE NAME: globvars.f90
PROGRAM NAME: globvars
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            MODULE globvars
     2:              IMPLICIT NONE
     3:              
     4:              LOGICAL :: Globvars_already_allocated=.FALSE.
     5:              INTEGER,ALLOCATABLE,DIMENSION(:) :: lvar,lvari,lvarc,uvarc,uvar,iloc,jloc
     6:              REAL,ALLOCATABLE ::    grh(:,:),del(:),topo(:,:),top(:)
     7:              REAL ,ALLOCATABLE ::    psr(:),pcr(:),usr(:),vsr(:),tsr(:),hsr(:)
     8:              REAL ,ALLOCATABLE ::    psm(:),pcm(:),usm(:),vsm(:),tsm(:),hsm(:)
     9:              REAL ,ALLOCATABLE ::    plr(:),plm(:)
    10:              REAL ,ALLOCATABLE ::    ccr(:),ccm(:)
    11:              REAL ,ALLOCATABLE ::    sdr(:),sdm(:)
    12:              REAL ,ALLOCATABLE ::    sfr(:),sfm(:)
    13:              REAL ,ALLOCATABLE ::    tar(:),tam(:)
    14:              
    15:              REAL ,ALLOCATABLE ::    psp(:,:,:),pcp(:,:,:),usp(:,:,:),vsp(:,:,:) 
    16:              REAL ,ALLOCATABLE ::    tsp(:,:,:),hsp(:,:,:),ccp(:,:,:),sdp(:,:,:)
    17:              REAL ,ALLOCATABLE ::    sfp(:,:,:),tap(:,:,:),tpp(:,:,:),plp(:,:,:)
    18:              REAL ,ALLOCATABLE ::    vst(:,:,:)
    19:              
    20:              REAL ,ALLOCATABLE ::    psp_p(:,:,:),pcp_p(:,:,:),usp_p(:,:,:),vsp_p(:,:,:) 
    21:              REAL ,ALLOCATABLE ::    tsp_p(:,:,:),hsp_p(:,:,:),ccp_p(:,:,:),sdp_p(:,:,:)
    22:              REAL ,ALLOCATABLE ::    sfp_p(:,:,:),tap_p(:,:,:),tpp_p(:,:,:),plp_p(:,:,:)
    23:              REAL ,ALLOCATABLE ::    vst_p(:,:,:)
    24:              
    25:              !especiais
    26:              REAL ,ALLOCATABLE ::    max_tsp(:),max_hsp(:),max_plp(:),max_vst(:)
    27:              REAL ,ALLOCATABLE ::    min_tsp(:),min_hsp(:),min_plp(:),min_vst(:)
    28:            
    29:              CHARACTER (LEN=4) ,ALLOCATABLE ::  avar(:),bvar(:)
    30:              CHARACTER (LEN=11) ,ALLOCATABLE :: prx(:)
    31:              CHARACTER (LEN=16) ,ALLOCATABLE :: aunits(:)
    32:              CHARACTER (LEN=40) ,ALLOCATABLE :: cvar(:),cloc(:),dvar(:)
    33:              LOGICAL ,ALLOCATABLE ::            dopt(:)
    34:            
    35:              CONTAINS
    36:              
    37:              LOGICAL FUNCTION Alloc_Globvars(imax,jmax,kmax,nfmx,nfmxo,npmx, &
    38:                                               nmb,mxghsl,numx,ndt,ndtout,ndiv)
    39:                
    40:                INTEGER,INTENT(IN) :: imax,jmax,kmax,nfmx,nfmxo,npmx,nmb,mxghsl,numx
    41:                INTEGER,INTENT(IN) :: ndt,ndiv,ndtout
    42:                
    43:                IF (Globvars_already_allocated) THEN
    44:                   Alloc_Globvars=.FALSE.
    45:                END IF
    46:                ALLOCATE(lvar(nfmx),lvari(nfmx),lvarc(nfmxo))  
    47:                ALLOCATE(uvarc(nfmxo),uvar(nfmx),iloc(npmx),jloc(npmx))
    48:                ALLOCATE(grh(npmx,mxghsl),del(kmax),topo(imax,jmax),top(npmx))
    49:                ALLOCATE(psr(npmx),pcr(npmx),usr(npmx),vsr(npmx),tsr(npmx),hsr(npmx))
    50:                ALLOCATE(psm(npmx),pcm(npmx),usm(npmx),vsm(npmx),tsm(npmx),hsm(npmx))
    51:                ALLOCATE(plr(npmx),plm(npmx))
    52:                ALLOCATE(ccr(npmx),ccm(npmx))
    53:                ALLOCATE(sdr(npmx),sdm(npmx))
    54:                ALLOCATE(sfr(npmx),sfm(npmx))
    55:                ALLOCATE(tar(npmx),tam(npmx))
    56:            
    57:                ALLOCATE(psp(npmx,nmb,ndt),pcp(npmx,nmb,ndt),usp(npmx,nmb,ndt))
    58:                ALLOCATE(vsp(npmx,nmb,ndt),tsp(npmx,nmb,ndt),hsp(npmx,nmb,ndt))
    59:                ALLOCATE(ccp(npmx,nmb,ndt),sdp(npmx,nmb,ndt),sfp(npmx,nmb,ndt))
    60:                ALLOCATE(tap(npmx,nmb,ndt),tpp(npmx,nmb,ndt),plp(npmx,nmb,ndt))
    61:                ALLOCATE(vst(npmx,nmb,ndt))
    62:            
    63:                !ALLOCATE(psp_p(ndt,npmx,ndiv),pcp_p(ndt,npmx,ndiv),usp_p(ndt,npmx,ndiv))
    64:                !ALLOCATE(vsp_p(ndt,npmx,ndiv),tsp_p(ndt,npmx,ndiv),hsp_p(ndt,npmx,ndiv))
    65:                !ALLOCATE(ccp_p(ndt,npmx,ndiv),sdp_p(ndt,npmx,ndiv),sfp_p(ndt,npmx,ndiv))
    66:                !ALLOCATE(tap_p(ndt,npmx,ndiv),tpp_p(ndt,npmx,ndiv),plp_p(ndt,npmx,ndiv))
    67:            
    68:                ALLOCATE(psp_p(npmx,ndiv,ndtout),pcp_p(npmx,ndiv,ndtout),usp_p(npmx,ndiv,ndtout))
    69:                ALLOCATE(vsp_p(npmx,ndiv,ndtout),tsp_p(npmx,ndiv,ndtout),hsp_p(npmx,ndiv,ndtout))
    70:                ALLOCATE(ccp_p(npmx,ndiv,ndtout),sdp_p(npmx,ndiv,ndtout),sfp_p(npmx,ndiv,ndtout))
    71:                ALLOCATE(tap_p(npmx,ndiv,ndtout),tpp_p(npmx,ndiv,ndtout),plp_p(npmx,ndiv,ndtout))
    72:                ALLOCATE(vst_p(npmx,ndiv,ndtout))
    73:                
    74:                !Especiais
    75:                ALLOCATE(max_tsp(npmx),max_hsp(npmx),max_plp(npmx),max_vst(npmx))
    76:                ALLOCATE(min_tsp(npmx),min_hsp(npmx),min_plp(npmx),min_vst(npmx))
    77:                
    78:                ALLOCATE(avar(nfmx),bvar(nfmxo))
    79:                ALLOCATE(prx(npmx))
    80:                ALLOCATE(aunits(-1:numx))
    81:                ALLOCATE(cvar(nfmx),cloc(npmx),dvar(nfmxo))
    82:                ALLOCATE(dopt(npmx))
    83:                
    84:                Globvars_already_allocated=.TRUE.
    85:                Alloc_Globvars=.TRUE.
    86:                
    87:              END FUNCTION Alloc_Globvars
    88:              
    89:            END MODULE globvars
