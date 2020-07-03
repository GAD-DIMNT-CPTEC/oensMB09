PROGRAM rwgrhens
  USE config
  USE geral
  USE globvars
  
  INTEGER :: nfmx,mxghsl,nunits,numx,nfmxo
  PARAMETER (nunits=220,numx=nunits-1,nfmxo=13)
  INTEGER :: kmaxm,kmaxp
  INTEGER :: mt,i,j,k,kl,nc,maxtim,l,ihora,ihor,idia,imes,iano,ix,  &
      mtmstp,idim,jdim,kdim,kqdim,ngpts,ngfldr,la,lb,nfl,  &
      nghsl,np,npmp,nv,nt,ntmp,n,medtim,mintim,mutc,iic,nvar
  INTEGER :: nhours
      
  INTEGER :: tpp_div,plp_div,psp_div,pcp_div,ccp_div,usp_div,vsp_div
  INTEGER :: tsp_div,hsp_div,sdp_div,sfp_div,tap_div,vst_div  
  REAL :: tpp_max,tpp_min,plp_max,plp_min,psp_max,psp_min,pcp_max,pcp_min
  REAL :: ccp_max,ccp_min,usp_max,usp_min,vsp_max,vsp_min,tsp_max,tsp_min
  REAL :: hsp_max,hsp_min,sdp_max,sdp_min,sfp_max,sfp_min,tap_max,tap_min
  REAL :: vst_max,vst_min  
  
  REAL :: fmdtim,tp,es,ee,tvm,cte
  CHARACTER (LEN=128) :: prov
  CHARACTER (LEN=2) :: dia,mes,hor
  CHARACTER (LEN=4) :: ano,nexp,iacc,idev
  CHARACTER (LEN=6) :: grdbox
  CHARACTER (LEN=7) :: nameh
  CHARACTER (LEN=20) :: title
  CHARACTER (LEN=40) :: ngrh
  CHARACTER (LEN=40) :: cps,cpc,ccc,cus,cvs,cts,chs,csd,csf,cta,exper
  CHARACTER (LEN=512) :: nticn
  CHARACTER (LEN=512) :: ntgrh
  LOGICAL :: exicn
  INTEGER :: lmes(12),idate(4)
  REAL :: stmp(6)  
  CHARACTER (LEN=3) :: cmes(12)
  INTEGER :: ndt,ndtout,ntime
  
  DATA mintim /1/, mutc /0/
  DATA grdbox/'GRDBO '/

  nvar=13
  
  print *,'reading config file'
  CALL le_config()
  
  nmb=nmemb
  kmax=levels
  nfmx=nfsf+nfkm
  mxghsl=nfsf+nfkm*kmax
  kmaxm=kmax-1
  kmaxp=kmax+1
  ndt=(86400*dofprev)/delt
  ndtout=(86400*dofprev)/deltout
  PRINT *,'Deltout= ',deltout,'Ndt= ',ndt,'Ndtout= ',ndtout
  tpp_div=divbyvar+1
  plp_div=divbyvar+1
  psp_div=divbyvar+1
  pcp_div=divbyvar+1
  ccp_div=divbyvar+1
  usp_div=divbyvar+1
  vsp_div=divbyvar+1
  tsp_div=divbyvar+1
  hsp_div=divbyvar+1
  sdp_div=divbyvar+1
  sfp_div=divbyvar+1
  tap_div=divbyvar+1
  vst_div=divbyvar+1
  
  IF(.NOT.  &
  Alloc_Globvars(imax,jmax,kmax,nfmx,nfmxo,npmx,nmb,mxghsl, &
                 numx,ndt,ndtout,divbyvar+1)) &
              THEN
    PRINT *,Error('Globvars ja alocado',52,'rwgrhens',2)
  END IF  
  
  dopt=.FALSE.
  
  cps='SURFACE PRESSURE                        '
  cpc='TOTAL PRECIPITATION                     '
  ccc='CLOUD COVER                             '
  cus='SURFACE ZONAL WIND (U)                  '
  cvs='SURFACE MERIDIONAL WIND (V)             '
  cts='SURFACE VIRTUAL TEMPERATURE             '
  chs='SURFACE SPECIFIC HUMIDITY               '
  csd='SNOW DEPTH                              '
  csf='SNOWFALL                                '
  cta='TEMPERATURE OF CANOPY AIR SPACE         '
  bvar(1)='TOPO'
  bvar(2)='PSLC'
  bvar(3)='PSNM'
  bvar(4)='PREC'
  bvar(5)='CBNV'
  bvar(6)='UVES'
  bvar(7)='VVES'
  bvar(8)='TEMS'
  bvar(9)='UMRS'
  bvar(10)='PNEV'
  bvar(11)='NEVE'
  bvar(12)='TADL'
  bvar(13)='VSUT'

  dvar(1)='TOPOGRAPHY                              '
  dvar(2)='SURFACE PRESSURE                        '
  dvar(3)='SEA LEVEL PRESSURE                      '
  dvar(4)='TOTAL PRECIPITATION                     '
  dvar(5)='CLOUD COVER                             '
  dvar(6)='SURFACE ZONAL WIND (U)                  '
  dvar(7)='SURFACE MERIDIONAL WIND (V)             '
  dvar(8)='SURFACE ABSOLUTE TEMPERATURE            '
  dvar(9)='SURFACE RELATIVE HUMIDITY               '
  dvar(10)='SNOW DEPTH                              '
  dvar(11)='SNOWFALL                                '
  dvar(12)='TEMPERATURE OF CANOPY AIR SPACE         '
  dvar(13)='TOTAL SURFACE WIND                      '
  uvarc(1)=10
  uvarc(2)=131
  uvarc(3)=131
  uvarc(4)=121
  uvarc(5)=1
  uvarc(6)=60
  uvarc(7)=60
  uvarc(8)=41
  uvarc(9)=1
  uvarc(10)=110
  uvarc(11)=120
  uvarc(12)=41
  lvarc(1)=0
  lvarc(2)=0
  lvarc(3)=0
  lvarc(4)=0
  lvarc(5)=0
  lvarc(6)=1
  lvarc(7)=1
  lvarc(8)=0
  lvarc(9)=0
  lvarc(10)=0
  lvarc(11)=0
  lvarc(12)=0
  !*
  print *,'Defining filenames'
  CALL filename(bvar,nvar)

  !CALL getenv('NAMEH',nameh)
  !*
  !READ(*,grhnml)
  !*
  OPEN(27,FILE=TRIM(filen(27)),STATUS='UNKNOWN',FORM='FORMATTED')
  print *,'reading units:',TRIM(filen(27))
  READ(27,'(A16)')aunits
  INQUIRE(UNIT=27,NAME=prov)
  WRITE(*,*)'unit27=',prov
  CLOSE(27)
  !*
  DO i=1,npmx
    top(i)=0.0
    !tpp(i,kl)=0.0
  END DO
  !*
  OPEN(28,FILE=TRIM(filen(28)),STATUS='OLD',FORM='FORMATTED')
  print *,'reading title:',TRIM(filen(28))
  READ(28,'(A20)')title
  INQUIRE(UNIT=28,NAME=prov)
  WRITE(*,*)'unit28=',prov
  WRITE(*,'(1X,A20)')title
  READ(28,'(A4,1X,A4,11I5,1X,A4)') nexp,iacc,idim,jdim,kdim,kqdim,  &
      ngpts,ngfldr,nghsl,idate,idev
  WRITE(*,'(1X,A4,1X,A4,11I5,1X,A4)') nexp,iacc,idim,jdim,kdim,kqdim,  &
      ngpts,ngfldr,nghsl,idate,idev
  READ(28,'(A40)')exper
  WRITE(*,'(1X,A40)')exper
  READ(28,'(5E16.8)')del
  WRITE(*,'(5E16.8)')del
  DO j=1,nfmx
    READ(28,'(A40,I5,2X,I5,1X,A4)') cvar(j),lvar(j),uvar(j),avar(j)
    WRITE(*,'(I3,1X,A40,I5,2X,I5,1X,A4)') i,cvar(j),lvar(j),uvar(j),avar(j)
  END DO
  DO i=1,npmx
    READ(28,'(A40,2I5,1X,A11)') cloc(i),iloc(i),jloc(i),prx(i)
    WRITE(*,'(i4,1X,A40,2I5,1X,A11)') i,cloc(i),iloc(i),jloc(i),prx(i)
  END DO
  CLOSE(28)
  !*
  CALL getenv('FORT29',nticn)
  iic=INDEX(nticn//' ',' ')-1
  IF (iic <= 0)iic=1
  INQUIRE(FILE=nticn(1:iic),EXIST=exicn)
  IF (exicn) THEN
    !AMM      OPEN(29,FILE=NTICN(1:IIC),STATUS='OLD',READONLY,
    !AMM     *        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=IMAX*JMAX)
    OPEN(29,FILE=TRIM(filen(29)),STATUS='OLD',FORM='UNFORMATTED')
    !AMM      READ(29,REC=1)((TOPO(I,J),I=1,IMAX),J=1,JMAX)
    READ(29)topo
    INQUIRE(UNIT=29,NAME=prov)
    WRITE(*,*)'unit29=',prov
  ELSE
    WRITE(0,'(A)')' Inital Condition File Does Not Exist:'
    WRITE(0,'(A)')' Set Topography Null'
    DO j=1,jmax
      DO i=1,imax
        topo(i,j)=0.0
      END DO
    END DO
  END IF
  CLOSE(29)
  nt=0
  DO i=1,npmx
    ix=0
    DO l=1,imax
      DO k=1,jmax
        IF (iloc(i) == l .AND. jloc(i) == k) THEN
          ix=1
          nt=nt+1
          top(nt)=topo(l,k)
        END IF
      END DO
    END DO
    IF (ix == 0) WRITE(0,*)i,iloc(i),jloc(i)
  END DO
  ntmp=nt
  
  np=0
  DO i=1,npmx
    IF (cloc(i)(1:6) /= grdbox) THEN
      np=np+1
      dopt(i)=.true.
      !WRITE(*,'(A)')cloc(i)
    END IF
  END DO
  npmp=np

  WRITE(*,*)'nmb=',nmb
  nfl=30+nmb

  medtim=nint(REAL(deltout)/delt)
  fmdtim=1.0/FLOAT(medtim)

!$OMP PARALLEL DO
  DO i=1,npmx
    plm(i)=0.0
    psm(i)=0.0
    pcm(i)=0.0
    ccm(i)=0.0
    usm(i)=0.0
    vsm(i)=0.0
    tsm(i)=0.0
    hsm(i)=0.0
    sdm(i)=0.0
    sfm(i)=0.0
    tam(i)=0.0
  END DO
  nv=0
  DO j=1,nfmx
    nv=nv+lvar(j)
    lvari(j)=nv-lvar(j)+1
  END DO
  WRITE(*,'(A)')' LVAR:'
  WRITE(*,'(20I4)')lvar
  WRITE(*,'(A)')' LVARI:'
  WRITE(*,'(20I4)')lvari
  
  DO kl=1,nmb
    OPEN(29+kl,FILE=TRIM(filen(29+kl)),STATUS='UNKNOWN',FORM='UNFORMATTED')
    nc=32+nmb+2*kl-1
    !OPEN(nc,STATUS='UNKNOWN',FORM='UNFORMATTED', convert='BIG_ENDIAN')
    mt=0
    INQUIRE(UNIT=29+kl,NAME=prov)
    WRITE(*,*)'unit',29+kl,'=',prov
    INQUIRE(UNIT=nc,NAME=prov)
    WRITE(*,*)'unit',nc,'=',prov
    ntime=0
30  READ(29+kl,END=40)stmp
    READ(29+kl)grh
    mt=mt+1
    
    
    DO j=1,nfmx
      l=lvari(j)
      DO k=1,npmx
        IF (INDEX(cvar(j),cps) == 1) THEN
          plr(k)=grh(k,l)
          psr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),cpc) == 1) THEN
          pcr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),ccc) == 1) THEN
          ccr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),cus) == 1) THEN
          usr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),cvs) == 1) THEN
          vsr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),cts) == 1) THEN
          tsr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),chs) == 1) THEN
          hsr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),csd) == 1) THEN
          sdr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),csf) == 1) THEN
          sfr(k)=grh(k,l)
        ELSE IF (INDEX(cvar(j),cta) == 1) THEN
          tar(k)=grh(k,l)
        END IF
      END DO
    END DO
    
!$OMP PARALLEL DO
    DO k=1,npmx
      pcr(k)=pcr(k)*FLOAT(medtim)*delt
      sfr(k)=sfr(k)*FLOAT(medtim)*delt
      ccr(k)=ccr(k)*100.0
      IF (hsr(k) < 0.0) hsr(k)=1.0E-6
      tp=tsr(k)/(1.0+0.608*hsr(k))
      es=6.1078*EXP(17.2693882*(tp-273.16)/(tp-35.86))
      ee=psr(k)*hsr(k)/(0.622+0.378*hsr(k))
      hsr(k)=100.0*ee/es
      IF (hsr(k) > 100.0) hsr(k)=100.0
      tvm=tsr(k)+0.5*0.0065*top(k)
      cte=(9.80665*top(k))/(287.05*tvm)
      psr(k)=psr(k)*EXP(cte)
      tsr(k)=tp-273.16
      IF (ABS(tar(k)) > 100.0) THEN
        tar(k)=tar(k)-273.16
      ELSE
        tar(k)=tsr(k)
      END IF
    END DO
    
!$OMP PARALLEL DO
    DO i=1,npmx
      plm(i)=plm(i)+plr(i)
      psm(i)=psm(i)+psr(i)
      pcm(i)=pcm(i)+pcr(i)
      ccm(i)=ccm(i)+ccr(i)
      usm(i)=usm(i)+usr(i)
      vsm(i)=vsm(i)+vsr(i)
      tsm(i)=tsm(i)+tsr(i)
      hsm(i)=hsm(i)+hsr(i)
      sdm(i)=sdm(i)+sdr(i)
      sfm(i)=sfm(i)+sfr(i)
      tam(i)=tam(i)+tar(i)
    END DO

    IF (MOD(mt,medtim) == 0) THEN   !Step
      ntime=ntime+1                 !Incrementa o novo timestep
      np=0
!$OMP PARALLEL DO
      DO i=1,npmx
        IF (dopt(i)) THEN	    !Se a cidade eh valida
          np=np+1
          plp(np,kl,ntime)=fmdtim*plm(i)
          psp(np,kl,ntime)=fmdtim*psm(i)
          pcp(np,kl,ntime)=fmdtim*pcm(i)
          ccp(np,kl,ntime)=fmdtim*ccm(i)
          usp(np,kl,ntime)=fmdtim*usm(i)
          vsp(np,kl,ntime)=fmdtim*vsm(i)
          tsp(np,kl,ntime)=fmdtim*tsm(i)
          hsp(np,kl,ntime)=fmdtim*hsm(i)
          sdp(np,kl,ntime)=fmdtim*sdm(i)
          sfp(np,kl,ntime)=fmdtim*sfm(i)
          tap(np,kl,ntime)=fmdtim*tam(i)
          tpp(np,kl,ntime)=top(i)
	  !Calculando o vento a partir de U e V
	  vst(np,kl,ntime)=sqrt((vsp(np,kl,ntime))**2+usp(np,kl,ntime)**2)
        END IF
      END DO

!$OMP PARALLEL DO
      DO i=1,npmx
        plm(i)=0.0
        psm(i)=0.0
        pcm(i)=0.0
        ccm(i)=0.0
        usm(i)=0.0
        vsm(i)=0.0
        tsm(i)=0.0
        hsm(i)=0.0
        sdm(i)=0.0
        sfm(i)=0.0
        tam(i)=0.0
      END DO
    END IF

    GO TO 30
40  maxtim=mt
    CLOSE(29+kl)
  END DO

  nhours=dofprev*24 !numero de horas de integracao
  write(*,*)'nhours=',nhours
!$OMP PARALLEL DO  
  DO i=1,npmx
     max_tsp(i)=MAXVAL(tsp(i,:,1:nhours)) 
     max_hsp(i)=MAXVAL(hsp(i,:,1:nhours))
     max_plp(i)=MAXVAL(plp(i,:,1:nhours))
     max_vst(i)=MAXVAL(vst(i,:,1:nhours))
     min_tsp(i)=MINVAL(tsp(i,:,1:nhours))
     min_hsp(i)=MINVAL(hsp(i,:,1:nhours))
     min_plp(i)=MINVAL(plp(i,:,1:nhours))
     min_vst(i)=MINVAL(vst(i,:,1:nhours))
  END DO   
  
  PRINT *,'Imprimindo arquivo de controle'
  write(*,*)'unit95:',filen(95)
  OPEN(UNIT=95,FILE=filen(95))
  WRITE(95,FMT='(I5)') npmx
  WRITE(95,FMT='(A4,1X,A40,1X,A11,1X,8(A8,1X))') &
    'NUM','GRID POINT','POSITION','T_max','T_min', &
                                  'H_max','H_min', &
			  	  'P_max','P_min', &
				  'W_max','W_min'
  DO i=1,npmx
     WRITE(95,FMT='(I4,1X,A40,1X,A11,1X,8(F8.2,1X))') i,cloc(i),prx(i), &
              max_tsp(i),min_tsp(i),max_hsp(i),min_hsp(i), &
	      max_plp(i),min_plp(i),max_vst(i),min_vst(i)
  END DO
  CLOSE(UNIT=95)	 
  
  PRINT *,'Chamando plumas' !calculando probabilidades
  CALL plumas(tpp,nmb,npmx,tpp_p,tpp_div,tpp_max,tpp_min,ndt,ndtout)
  CALL plumas(plp,nmb,npmx,plp_p,plp_div,plp_max,plp_min,ndt,ndtout)
  CALL plumas(psp,nmb,npmx,psp_p,psp_div,psp_max,psp_min,ndt,ndtout)
  CALL plumas(pcp,nmb,npmx,pcp_p,pcp_div,pcp_max,pcp_min,ndt,ndtout)
  CALL plumas(ccp,nmb,npmx,ccp_p,ccp_div,ccp_max,ccp_min,ndt,ndtout)
  CALL plumas(usp,nmb,npmx,usp_p,usp_div,usp_max,usp_min,ndt,ndtout)
  CALL plumas(vsp,nmb,npmx,vsp_p,vsp_div,vsp_max,vsp_min,ndt,ndtout)
  CALL plumas(tsp,nmb,npmx,tsp_p,tsp_div,tsp_max,tsp_min,ndt,ndtout)
  CALL plumas(hsp,nmb,npmx,hsp_p,hsp_div,hsp_max,hsp_min,ndt,ndtout)
  CALL plumas(sdp,nmb,npmx,sdp_p,sdp_div,sdp_max,sdp_min,ndt,ndtout)
  CALL plumas(sfp,nmb,npmx,sfp_p,sfp_div,sfp_max,sfp_min,ndt,ndtout)
  CALL plumas(tap,nmb,npmx,tap_p,tap_div,tap_max,tap_min,ndt,ndtout)
  CALL plumas(vst,nmb,npmx,vst_p,vst_div,vst_max,vst_min,ndt,ndtout)

  !Abrindo arquivos
  DO i=1,nvar+1
    OPEN(UNIT=nfl+i-1,FILE=filen(nfl+i-1),STATUS='UNKNOWN',FORM='UNFORMATTED')
  END DO
  
  PRINT *,'Escrevendo ctls'
  !CALL writectl_out(bvar,nvar,dvar,1,tpp_div,tpp_max,tpp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,2,plp_div,plp_max,plp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,3,psp_div,psp_max,psp_min,ndtout,1)

  !Precipitation
  CALL writectl_out(bvar,nvar,dvar,4,nmb,nmb,1.0,ndtout,0)

  CALL writectl_out(bvar,nvar,dvar,5,ccp_div,ccp_max,ccp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,6,usp_div,usp_max,usp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,7,vsp_div,vsp_max,vsp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,8,tsp_div,tsp_max,tsp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,9, hsp_div,hsp_max,hsp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,10,sdp_div,sdp_max,sdp_min,ndtout,1)

  !Snowfall
!  CALL writectl_out(bvar,nvar,dvar,11,sfp_div,sfp_max,sfp_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,11,nmb,nmb,1.0,ndtout,2)

  CALL writectl_out(bvar,nvar,dvar,12,tap_div,tap_max,tap_min,ndtout,1)
  CALL writectl_out(bvar,nvar,dvar,13,vst_div,vst_max,vst_min,ndtout,1)

  CALL cont_ctl(npmx,bvar,nvar,dvar,lvarc,uvarc,ndtout,aunits)
  
  PRINT *,'Escrevendo binarios'
  DO t=1,ndtout
    WRITE(nfl)    ((tpp_p(i,j,t),i=1,npmx),j=1,tpp_div)
    WRITE(nfl+1)  ((plp_p(i,j,t),i=1,npmx),j=1,plp_div)
    WRITE(nfl+2)  ((psp_p(i,j,t),i=1,npmx),j=1,psp_div)
    WRITE(nfl+4)  ((ccp_p(i,j,t),i=1,npmx),j=1,ccp_div)
    WRITE(nfl+5)  ((usp_p(i,j,t),i=1,npmx),j=1,usp_div)
    WRITE(nfl+6)  ((vsp_p(i,j,t),i=1,npmx),j=1,vsp_div)
    WRITE(nfl+7)  ((tsp_p(i,j,t),i=1,npmx),j=1,tsp_div)
    WRITE(nfl+8)  ((hsp_p(i,j,t),i=1,npmx),j=1,hsp_div)
    WRITE(nfl+9)  ((sdp_p(i,j,t),i=1,npmx),j=1,sdp_div)
!    WRITE(nfl+10) ((sfp_p(i,j,t),i=1,npmx),j=1,sfp_div)
    WRITE(nfl+11) ((tap_p(i,j,t),i=1,npmx),j=1,tap_div)
    WRITE(nfl+12) ((vst_p(i,j,t),i=1,npmx),j=1,vst_div)
  END DO
  
  !Precipitation
  DO t=1,ndtout
    WRITE(nfl+3)  ((pcp(i,j,t),i=1,npmx),j=1,nmb)
  END DO
  !Snowfall
  DO t=1,ndtout
    WRITE(nfl+10)  ((sfp(i,j,t),i=1,npmx),j=1,nmb)
  END DO
  
  PRINT *,'Escrevendo controle'
  DO t=1,ndtout
    WRITE(nfl+13) (tpp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (plp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (psp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (pcp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (ccp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (usp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (vsp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (tsp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (hsp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (sdp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (sfp(i,nmb,t),i=1,npmx)
    WRITE(nfl+13) (tap(i,nmb,t),i=1,npmx)
!    WRITE(nfl+13) (vst(i,nmb,t),i=1,npmx)
  END DO
      
  DO i=1,nvar
    CLOSE(UNIT=nfl+i-1)
  END DO

  !Somente para testes
  !CALL Chek_Plumas(npmx,ndtout,nmemb,psp_div,psp,psp_p,cloc(85),85, &
  !                     psp_max,psp_min,'PSPM - Pressao ao Nivel do Mar')

END PROGRAM rwgrhens
  