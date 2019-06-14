SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:28:49 2005
FILE NAME: rwgrhens_luf.f90
PROGRAM NAME: rwgrhens
DIAGNOSTIC LIST

  LINE  LEVEL( NO.): DIAGNOSTIC MESSAGE

    76  vec  (   4): Vectorized array expression.
   154  vec  (   1): Vectorized loop.
   173  vec  (   2): Partially vectorized loop.
   177  vec  (   3): Unvectorized loop.
   199  vec  (   1): Vectorized loop.
   199  vec  (   1): Vectorized loop.
   209  vec  (   1): Vectorized loop.
   222  vec  (   2): Partially vectorized loop.
   238  mul  (  12): Parallelized by DO option.
   238  vec  (   1): Vectorized loop.
   251  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: main_$1
   252  vec  (   3): Unvectorized loop.
   278  vec  (   2): Partially vectorized loop.
   305  mul  (  12): Parallelized by DO option.
   305  vec  (   1): Vectorized loop.
   326  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: main_$2
   327  mul  (  12): Parallelized by DO option.
   327  vec  (   1): Vectorized loop.
   341  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: main_$3
   345  mul  (  12): Parallelized by DO option.
   345  vec  (   1): Vectorized loop.
   365  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: main_$4
   366  mul  (  12): Parallelized by DO option.
   366  vec  (   1): Vectorized loop.
   379  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: main_$5
   389  mul  (  12): Parallelized by DO option.
   389  vec  (   3): Unvectorized loop.
   390  vec  (   4): Vectorized array expression.
   391  vec  (   4): Vectorized array expression.
   392  vec  (   4): Vectorized array expression.
   393  vec  (   4): Vectorized array expression.
   394  vec  (   4): Vectorized array expression.
   395  vec  (   4): Vectorized array expression.
   396  vec  (   4): Vectorized array expression.
   397  vec  (   4): Vectorized array expression.
   400  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: main_$6
   409  vec  (   3): Unvectorized loop.
   432  vec  (   3): Unvectorized loop.
   442  warn (  62): Characteristics of arguments of procedure "writectl_out" are inconsistent with previous reference.
   453  warn (  62): Characteristics of arguments of procedure "writectl_out" are inconsistent with previous reference.
   461  vec  (   3): Unvectorized loop.
   477  vec  (   3): Unvectorized loop.
   481  vec  (   3): Unvectorized loop.
   486  vec  (   3): Unvectorized loop.
   502  vec  (   3): Unvectorized loop.
   510  warn (  82): Name "ano" is not used.
   510  warn (  82): Name "mtmstp" is not used.
   510  warn (  82): Name "idia" is not used.
   510  warn (  82): Name "ntgrh" is not used.
   510  warn (  82): Name "iano" is not used.
   510  warn (  82): Name "dia" is not used.
   510  warn (  82): Name "la" is not used.
   510  warn (  82): Name "lb" is not used.
   510  warn (  82): Name "n" is not used.
   510  warn (  82): Name "nameh" is not used.
   510  warn (  82): Name "imes" is not used.
   510  warn (  82): Name "ngrh" is not used.
   510  warn (  82): Name "hor" is not used.
   510  warn (  82): Name "lmes" is not used.
   510  warn (  82): Name "cmes" is not used.
   510  warn (  82): Name "mes" is not used.
   510  warn (  82): Name "ihor" is not used.
   510  warn (  82): Name "ihora" is not used.
   510  warn (  63): Data object having SAVE attribute "grdbox" is allocated as MACROTASK-SHARED OBJECT.
   510  warn (  63): Data object having SAVE attribute "mutc" is allocated as MACROTASK-SHARED OBJECT.
   510  warn (  63): Data object having SAVE attribute "mintim" is allocated as MACROTASK-SHARED OBJECT.
SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:28:49 2005
FILE NAME: rwgrhens_luf.f90
PROGRAM NAME: 
DIAGNOSTIC LIST

  LINE  LEVEL( NO.): DIAGNOSTIC MESSAGE

   511  warn (   6): Only comment lines in program.
SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:28:49 2005
FILE NAME: rwgrhens_luf.f90
PROGRAM NAME: rwgrhens
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            PROGRAM rwgrhens
     2:              USE config
     3:              USE geral
     4:              USE globvars
     5:              
     6:              INTEGER :: nfmx,mxghsl,nunits,numx,nfmxo
     7:              PARAMETER (nunits=220,numx=nunits-1,nfmxo=13)
     8:              INTEGER :: kmaxm,kmaxp
     9:              INTEGER :: mt,i,j,k,kl,nc,maxtim,l,ihora,ihor,idia,imes,iano,ix,  &
    10:                  mtmstp,idim,jdim,kdim,kqdim,ngpts,ngfldr,la,lb,nfl,  &
    11:                  nghsl,np,npmp,nv,nt,ntmp,n,medtim,mintim,mutc,iic,nvar
    12:              INTEGER :: nhours
    13:                  
    14:              INTEGER :: tpp_div,plp_div,psp_div,pcp_div,ccp_div,usp_div,vsp_div
    15:              INTEGER :: tsp_div,hsp_div,sdp_div,sfp_div,tap_div,vst_div  
    16:              REAL :: tpp_max,tpp_min,plp_max,plp_min,psp_max,psp_min,pcp_max,pcp_min
    17:              REAL :: ccp_max,ccp_min,usp_max,usp_min,vsp_max,vsp_min,tsp_max,tsp_min
    18:              REAL :: hsp_max,hsp_min,sdp_max,sdp_min,sfp_max,sfp_min,tap_max,tap_min
    19:              REAL :: vst_max,vst_min  
    20:              
    21:              REAL :: fmdtim,tp,es,ee,tvm,cte
    22:              CHARACTER (LEN=128) :: prov
    23:              CHARACTER (LEN=2) :: dia,mes,hor
    24:              CHARACTER (LEN=4) :: ano,nexp,iacc,idev
    25:              CHARACTER (LEN=6) :: grdbox
    26:              CHARACTER (LEN=7) :: nameh
    27:              CHARACTER (LEN=20) :: title
    28:              CHARACTER (LEN=40) :: ngrh
    29:              CHARACTER (LEN=40) :: cps,cpc,ccc,cus,cvs,cts,chs,csd,csf,cta,exper
    30:              CHARACTER (LEN=512) :: nticn
    31:              CHARACTER (LEN=512) :: ntgrh
    32:              LOGICAL :: exicn
    33:              INTEGER :: lmes(12),idate(4)
    34:              REAL :: stmp(6)  
    35:              CHARACTER (LEN=3) :: cmes(12)
    36:              INTEGER :: ndt,ndtout,ntime
    37:              
    38:              DATA mintim /1/, mutc /0/
    39:              DATA grdbox/'GRDBO '/
    40:            
    41:              nvar=13
    42:              
    43:              print *,'reading config file'
    44:              CALL le_config()
    45:              
    46:              nmb=nmemb
    47:              kmax=levels
    48:              nfmx=nfsf+nfkm
    49:              mxghsl=nfsf+nfkm*kmax
    50:              kmaxm=kmax-1
    51:              kmaxp=kmax+1
    52:              ndt=(86400*dofprev)/delt
    53:              ndtout=(86400*dofprev)/deltout
    54:              PRINT *,'Deltout= ',deltout,'Ndt= ',ndt,'Ndtout= ',ndtout
    55:              tpp_div=divbyvar+1
    56:              plp_div=divbyvar+1
    57:              psp_div=divbyvar+1
    58:              pcp_div=divbyvar+1
    59:              ccp_div=divbyvar+1
    60:              usp_div=divbyvar+1
    61:              vsp_div=divbyvar+1
    62:              tsp_div=divbyvar+1
    63:              hsp_div=divbyvar+1
    64:              sdp_div=divbyvar+1
    65:              sfp_div=divbyvar+1
    66:              tap_div=divbyvar+1
    67:              vst_div=divbyvar+1
    68:              
    69:              IF(.NOT.  &
    70:              Alloc_Globvars(imax,jmax,kmax,nfmx,nfmxo,npmx,nmb,mxghsl, &
    71:                             numx,ndt,ndtout,divbyvar+1)) &
    72:                          THEN
    73:                PRINT *,Error('Globvars ja alocado',52,'rwgrhens',2)
    74:              END IF  
    75:              
    76: V======      dopt=.FALSE.
    77:              
    78:              cps='SURFACE PRESSURE                        '
    79:              cpc='TOTAL PRECIPITATION                     '
    80:              ccc='CLOUD COVER                             '
    81:              cus='SURFACE ZONAL WIND (U)                  '
    82:              cvs='SURFACE MERIDIONAL WIND (V)             '
    83:              cts='SURFACE VIRTUAL TEMPERATURE             '
    84:              chs='SURFACE SPECIFIC HUMIDITY               '
    85:              csd='SNOW DEPTH                              '
    86:              csf='SNOWFALL                                '
    87:              cta='TEMPERATURE OF CANOPY AIR SPACE         '
    88:              bvar(1)='TOPO'
    89:              bvar(2)='PSLC'
    90:              bvar(3)='PSNM'
    91:              bvar(4)='PREC'
    92:              bvar(5)='CBNV'
    93:              bvar(6)='UVES'
    94:              bvar(7)='VVES'
    95:              bvar(8)='TEMS'
    96:              bvar(9)='UMRS'
    97:              bvar(10)='PNEV'
    98:              bvar(11)='NEVE'
    99:              bvar(12)='TADL'
   100:              bvar(13)='VSUT'
   101:            
   102:              dvar(1)='TOPOGRAPHY                              '
   103:              dvar(2)='SURFACE PRESSURE                        '
   104:              dvar(3)='SEA LEVEL PRESSURE                      '
   105:              dvar(4)='TOTAL PRECIPITATION                     '
   106:              dvar(5)='CLOUD COVER                             '
   107:              dvar(6)='SURFACE ZONAL WIND (U)                  '
   108:              dvar(7)='SURFACE MERIDIONAL WIND (V)             '
   109:              dvar(8)='SURFACE ABSOLUTE TEMPERATURE            '
   110:              dvar(9)='SURFACE RELATIVE HUMIDITY               '
   111:              dvar(10)='SNOW DEPTH                              '
   112:              dvar(11)='SNOWFALL                                '
   113:              dvar(12)='TEMPERATURE OF CANOPY AIR SPACE         '
   114:              dvar(13)='TOTAL SURFACE WIND                      '
   115:              uvarc(1)=10
   116:              uvarc(2)=131
   117:              uvarc(3)=131
   118:              uvarc(4)=121
   119:              uvarc(5)=1
   120:              uvarc(6)=60
   121:              uvarc(7)=60
   122:              uvarc(8)=41
   123:              uvarc(9)=1
   124:              uvarc(10)=110
   125:              uvarc(11)=120
   126:              uvarc(12)=41
   127:              lvarc(1)=0
   128:              lvarc(2)=0
   129:              lvarc(3)=0
   130:              lvarc(4)=0
   131:              lvarc(5)=0
   132:              lvarc(6)=1
   133:              lvarc(7)=1
   134:              lvarc(8)=0
   135:              lvarc(9)=0
   136:              lvarc(10)=0
   137:              lvarc(11)=0
   138:              lvarc(12)=0
   139:              !*
   140:              print *,'Defining filenames'
   141:              CALL filename(bvar,nvar)
   142:            
   143:              !CALL getenv('NAMEH',nameh)
   144:              !*
   145:              !READ(*,grhnml)
   146:              !*
   147:              OPEN(27,FILE=TRIM(filen(27)),STATUS='UNKNOWN',FORM='FORMATTED')
   148:              print *,'reading units:',TRIM(filen(27))
   149:              READ(27,'(A16)')aunits
   150:              INQUIRE(UNIT=27,NAME=prov)
   151:              WRITE(*,*)'unit27=',prov
   152:              CLOSE(27)
   153:              !*
   154: V------>     DO i=1,npmx
   155: |              top(i)=0.0
   156: |              !tpp(i,kl)=0.0
   157: V------      END DO
   158:              !*
   159:              OPEN(28,FILE=TRIM(filen(28)),STATUS='OLD',FORM='FORMATTED')
   160:              print *,'reading title:',TRIM(filen(28))
   161:              READ(28,'(A20)')title
   162:              INQUIRE(UNIT=28,NAME=prov)
   163:              WRITE(*,*)'unit28=',prov
   164:              WRITE(*,'(1X,A20)')title
   165:              READ(28,'(A4,1X,A4,11I5,1X,A4)') nexp,iacc,idim,jdim,kdim,kqdim,  &
   166:                  ngpts,ngfldr,nghsl,idate,idev
   167:              WRITE(*,'(1X,A4,1X,A4,11I5,1X,A4)') nexp,iacc,idim,jdim,kdim,kqdim,  &
   168:                  ngpts,ngfldr,nghsl,idate,idev
   169:              READ(28,'(A40)')exper
   170:              WRITE(*,'(1X,A40)')exper
   171:              READ(28,'(5E16.8)')del
   172:              WRITE(*,'(5E16.8)')del
   173: V------>     DO j=1,nfmx
   174: |       S      READ(28,'(A40,I5,2X,I5,1X,A4)') cvar(j),lvar(j),uvar(j),avar(j)
   175: |       S      WRITE(*,'(I3,1X,A40,I5,2X,I5,1X,A4)') i,cvar(j),lvar(j),uvar(j),avar(j)
   176: V------      END DO
   177: +------>     DO i=1,npmx
   178: |              READ(28,'(A40,2I5,1X,A11)') cloc(i),iloc(i),jloc(i),prx(i)
   179: |              WRITE(*,'(i4,1X,A40,2I5,1X,A11)') i,cloc(i),iloc(i),jloc(i),prx(i)
   180: +------      END DO
   181:              CLOSE(28)
   182:              !*
   183:              CALL getenv('FORT29',nticn)
   184:              iic=INDEX(nticn//' ',' ')-1
   185:              IF (iic <= 0)iic=1
   186:              INQUIRE(FILE=nticn(1:iic),EXIST=exicn)
   187:              IF (exicn) THEN
   188:                !AMM      OPEN(29,FILE=NTICN(1:IIC),STATUS='OLD',READONLY,
   189:                !AMM     *        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=IMAX*JMAX)
   190:                OPEN(29,FILE=TRIM(filen(29)),STATUS='OLD',FORM='UNFORMATTED')
   191:                !AMM      READ(29,REC=1)((TOPO(I,J),I=1,IMAX),J=1,JMAX)
   192:                READ(29)topo
   193:                INQUIRE(UNIT=29,NAME=prov)
   194:                WRITE(*,*)'unit29=',prov
   195:              ELSE
   196:                WRITE(0,'(A)')' Inital Condition File Does Not Exist:'
   197:                WRITE(0,'(A)')' Set Topography Null'
   198: +------>       DO j=1,jmax
   199: |V----->         DO i=1,imax
   200: ||                 topo(i,j)=0.0
   201: |V-----          END DO
   202: +------        END DO
   203:              END IF
   204:              CLOSE(29)
   205:              nt=0
   206: +------>     DO i=1,npmx
   207: |              ix=0
   208: |+----->       DO l=1,imax
   209: ||V---->         DO k=1,jmax
   210: |||                IF (iloc(i) == l .AND. jloc(i) == k) THEN
   211: |||                  ix=1
   212: |||                  nt=nt+1
   213: |||                  top(nt)=topo(l,k)
   214: |||                END IF
   215: ||V----          END DO
   216: |+-----        END DO
   217: |              IF (ix == 0) WRITE(0,*)i,iloc(i),jloc(i)
   218: +------      END DO
   219:              ntmp=nt
   220:              
   221:              np=0
   222: V------>     DO i=1,npmx
   223: |       S      IF (cloc(i)(1:6) /= grdbox) THEN
   224: |                np=np+1
   225: |       S        dopt(i)=.true.
   226: |                !WRITE(*,'(A)')cloc(i)
   227: |              END IF
   228: V------      END DO
   229:              npmp=np
   230:            
   231:              WRITE(*,*)'nmb=',nmb
   232:              nfl=30+nmb
   233:            
   234:              medtim=nint(REAL(deltout)/delt)
   235:              fmdtim=1.0/FLOAT(medtim)
   236:            
   237:            !$OMP PARALLEL DO
   238: P------>     DO i=1,npmx
   239: |              plm(i)=0.0
   240: |              psm(i)=0.0
   241: |              pcm(i)=0.0
   242: |              ccm(i)=0.0
   243: |              usm(i)=0.0
   244: |              vsm(i)=0.0
   245: |              tsm(i)=0.0
   246: |              hsm(i)=0.0
   247: |              sdm(i)=0.0
   248: |              sfm(i)=0.0
   249: |              tam(i)=0.0
   250: P------      END DO
   251:              nv=0
   252: +------>     DO j=1,nfmx
   253: |              nv=nv+lvar(j)
   254: |              lvari(j)=nv-lvar(j)+1
   255: +------      END DO
   256:              WRITE(*,'(A)')' LVAR:'
   257:              WRITE(*,'(20I4)')lvar
   258:              WRITE(*,'(A)')' LVARI:'
   259:              WRITE(*,'(20I4)')lvari
   260:              
   261: +------>     DO kl=1,nmb
   262: |              OPEN(29+kl,FILE=TRIM(filen(29+kl)),STATUS='UNKNOWN',FORM='UNFORMATTED')
   263: |              nc=32+nmb+2*kl-1
   264: |              !OPEN(nc,STATUS='UNKNOWN',FORM='UNFORMATTED', convert='BIG_ENDIAN')
   265: |              mt=0
   266: |              INQUIRE(UNIT=29+kl,NAME=prov)
   267: |              WRITE(*,*)'unit',29+kl,'=',prov
   268: |              INQUIRE(UNIT=nc,NAME=prov)
   269: |              WRITE(*,*)'unit',nc,'=',prov
   270: |              ntime=0
   271: |          30  READ(29+kl,END=40)stmp
   272: |              READ(29+kl)grh
   273: |              mt=mt+1
   274: |              
   275: |              
   276: |+----->       DO j=1,nfmx
   277: ||               l=lvari(j)
   278: ||V---->         DO k=1,npmx
   279: |||     S          IF (INDEX(cvar(j),cps) == 1) THEN
   280: |||     S            plr(k)=grh(k,l)
   281: |||     S            psr(k)=grh(k,l)
   282: |||     S          ELSE IF (INDEX(cvar(j),cpc) == 1) THEN
   283: |||     S            pcr(k)=grh(k,l)
   284: |||     S          ELSE IF (INDEX(cvar(j),ccc) == 1) THEN
   285: |||     S            ccr(k)=grh(k,l)
   286: |||     S          ELSE IF (INDEX(cvar(j),cus) == 1) THEN
   287: |||     S            usr(k)=grh(k,l)
   288: |||     S          ELSE IF (INDEX(cvar(j),cvs) == 1) THEN
   289: |||     S            vsr(k)=grh(k,l)
   290: |||     S          ELSE IF (INDEX(cvar(j),cts) == 1) THEN
   291: |||     S            tsr(k)=grh(k,l)
   292: |||     S          ELSE IF (INDEX(cvar(j),chs) == 1) THEN
   293: |||     S            hsr(k)=grh(k,l)
   294: |||     S          ELSE IF (INDEX(cvar(j),csd) == 1) THEN
   295: |||     S            sdr(k)=grh(k,l)
   296: |||     S          ELSE IF (INDEX(cvar(j),csf) == 1) THEN
   297: |||     S            sfr(k)=grh(k,l)
   298: |||     S          ELSE IF (INDEX(cvar(j),cta) == 1) THEN
   299: |||     S            tar(k)=grh(k,l)
   300: |||                END IF
   301: ||V----          END DO
   302: |+-----        END DO
   303: |              
   304: |          !$OMP PARALLEL DO
   305: |P----->       DO k=1,npmx
   306: ||               pcr(k)=pcr(k)*FLOAT(medtim)*delt
   307: ||               sfr(k)=sfr(k)*FLOAT(medtim)*delt
   308: ||               ccr(k)=ccr(k)*100.0
   309: ||               IF (hsr(k) < 0.0) hsr(k)=1.0E-6
   310: ||               tp=tsr(k)/(1.0+0.608*hsr(k))
   311: ||               es=6.1078*EXP(17.2693882*(tp-273.16)/(tp-35.86))
   312: ||               ee=psr(k)*hsr(k)/(0.622+0.378*hsr(k))
   313: ||               hsr(k)=100.0*ee/es
   314: ||               IF (hsr(k) > 100.0) hsr(k)=100.0
   315: ||               tvm=tsr(k)+0.5*0.0065*top(k)
   316: ||               cte=(9.80665*top(k))/(287.05*tvm)
   317: ||               psr(k)=psr(k)*EXP(cte)
   318: ||               tsr(k)=tp-273.16
   319: ||               IF (ABS(tar(k)) > 100.0) THEN
   320: ||                 tar(k)=tar(k)-273.16
   321: ||               ELSE
   322: ||                 tar(k)=tsr(k)
   323: ||               END IF
   324: |P-----        END DO
   325: |              
   326: |          !$OMP PARALLEL DO
   327: |P----->       DO i=1,npmx
   328: ||               plm(i)=plm(i)+plr(i)
   329: ||               psm(i)=psm(i)+psr(i)
   330: ||               pcm(i)=pcm(i)+pcr(i)
   331: ||               ccm(i)=ccm(i)+ccr(i)
   332: ||               usm(i)=usm(i)+usr(i)
   333: ||               vsm(i)=vsm(i)+vsr(i)
   334: ||               tsm(i)=tsm(i)+tsr(i)
   335: ||               hsm(i)=hsm(i)+hsr(i)
   336: ||               sdm(i)=sdm(i)+sdr(i)
   337: ||               sfm(i)=sfm(i)+sfr(i)
   338: ||               tam(i)=tam(i)+tar(i)
   339: |P-----        END DO
   340: |          
   341: |              IF (MOD(mt,medtim) == 0) THEN   !Step
   342: |                ntime=ntime+1                 !Incrementa o novo timestep
   343: |                np=0
   344: |          !$OMP PARALLEL DO
   345: |P----->         DO i=1,npmx
   346: ||                 IF (dopt(i)) THEN	    !Se a cidade eh valida
   347: ||                   np=np+1
   348: ||                   plp(np,kl,ntime)=fmdtim*plm(i)
   349: ||                   psp(np,kl,ntime)=fmdtim*psm(i)
   350: ||                   pcp(np,kl,ntime)=fmdtim*pcm(i)
   351: ||                   ccp(np,kl,ntime)=fmdtim*ccm(i)
   352: ||                   usp(np,kl,ntime)=fmdtim*usm(i)
   353: ||                   vsp(np,kl,ntime)=fmdtim*vsm(i)
   354: ||                   tsp(np,kl,ntime)=fmdtim*tsm(i)
   355: ||                   hsp(np,kl,ntime)=fmdtim*hsm(i)
   356: ||                   sdp(np,kl,ntime)=fmdtim*sdm(i)
   357: ||                   sfp(np,kl,ntime)=fmdtim*sfm(i)
   358: ||                   tap(np,kl,ntime)=fmdtim*tam(i)
   359: ||                   tpp(np,kl,ntime)=top(i)
   360: ||         	  !Calculando o vento a partir de U e V
   361: ||         	  vst(np,kl,ntime)=sqrt((vsp(np,kl,ntime))**2+usp(np,kl,ntime)**2)
   362: ||                 END IF
   363: |P-----          END DO
   364: |          
   365: |          !$OMP PARALLEL DO
   366: |P----->         DO i=1,npmx
   367: ||                 plm(i)=0.0
   368: ||                 psm(i)=0.0
   369: ||                 pcm(i)=0.0
   370: ||                 ccm(i)=0.0
   371: ||                 usm(i)=0.0
   372: ||                 vsm(i)=0.0
   373: ||                 tsm(i)=0.0
   374: ||                 hsm(i)=0.0
   375: ||                 sdm(i)=0.0
   376: ||                 sfm(i)=0.0
   377: ||                 tam(i)=0.0
   378: |P-----          END DO
   379: |              END IF
   380: |          
   381: |              GO TO 30
   382: |          40  maxtim=mt
   383: |              CLOSE(29+kl)
   384: +------      END DO
   385:            
   386:              nhours=dofprev*24 !numero de horas de integracao
   387:              write(*,*)'nhours=',nhours
   388:            !$OMP PARALLEL DO  
   389: P------>     DO i=1,npmx
   390: |WW====         max_tsp(i)=MAXVAL(tsp(i,:,1:nhours)) 
   391: |WW====         max_hsp(i)=MAXVAL(hsp(i,:,1:nhours))
   392: |WW====         max_plp(i)=MAXVAL(plp(i,:,1:nhours))
   393: |WW====         max_vst(i)=MAXVAL(vst(i,:,1:nhours))
   394: |**====         min_tsp(i)=MINVAL(tsp(i,:,1:nhours))
   395: |**====         min_hsp(i)=MINVAL(hsp(i,:,1:nhours))
   396: |**====         min_plp(i)=MINVAL(plp(i,:,1:nhours))
   397: |**====         min_vst(i)=MINVAL(vst(i,:,1:nhours))
   398: P------      END DO   
   399:              
   400:              PRINT *,'Imprimindo arquivo de controle'
   401:              write(*,*)'unit95:',filen(95)
   402:              OPEN(UNIT=95,FILE=filen(95))
   403:              WRITE(95,FMT='(I5)') npmx
   404:              WRITE(95,FMT='(A4,1X,A40,1X,A11,1X,8(A8,1X))') &
   405:                'NUM','GRID POINT','POSITION','T_max','T_min', &
   406:                                              'H_max','H_min', &
   407:            			  	  'P_max','P_min', &
   408:            				  'W_max','W_min'
   409: +------>     DO i=1,npmx
   410: |               WRITE(95,FMT='(I4,1X,A40,1X,A11,1X,8(F8.2,1X))') i,cloc(i),prx(i), &
   411: |                        max_tsp(i),min_tsp(i),max_hsp(i),min_hsp(i), &
   412: |          	      max_plp(i),min_plp(i),max_vst(i),min_vst(i)
   413: +------      END DO
   414:              CLOSE(UNIT=95)	 
   415:              
   416:              PRINT *,'Chamando plumas' !calculando probabilidades
   417:              CALL plumas(tpp,nmb,npmx,tpp_p,tpp_div,tpp_max,tpp_min,ndt,ndtout)
   418:              CALL plumas(plp,nmb,npmx,plp_p,plp_div,plp_max,plp_min,ndt,ndtout)
   419:              CALL plumas(psp,nmb,npmx,psp_p,psp_div,psp_max,psp_min,ndt,ndtout)
   420:              CALL plumas(pcp,nmb,npmx,pcp_p,pcp_div,pcp_max,pcp_min,ndt,ndtout)
   421:              CALL plumas(ccp,nmb,npmx,ccp_p,ccp_div,ccp_max,ccp_min,ndt,ndtout)
   422:              CALL plumas(usp,nmb,npmx,usp_p,usp_div,usp_max,usp_min,ndt,ndtout)
   423:              CALL plumas(vsp,nmb,npmx,vsp_p,vsp_div,vsp_max,vsp_min,ndt,ndtout)
   424:              CALL plumas(tsp,nmb,npmx,tsp_p,tsp_div,tsp_max,tsp_min,ndt,ndtout)
   425:              CALL plumas(hsp,nmb,npmx,hsp_p,hsp_div,hsp_max,hsp_min,ndt,ndtout)
   426:              CALL plumas(sdp,nmb,npmx,sdp_p,sdp_div,sdp_max,sdp_min,ndt,ndtout)
   427:              CALL plumas(sfp,nmb,npmx,sfp_p,sfp_div,sfp_max,sfp_min,ndt,ndtout)
   428:              CALL plumas(tap,nmb,npmx,tap_p,tap_div,tap_max,tap_min,ndt,ndtout)
   429:              CALL plumas(vst,nmb,npmx,vst_p,vst_div,vst_max,vst_min,ndt,ndtout)
   430:            
   431:              !Abrindo arquivos
   432: +------>     DO i=1,nvar+1
   433: |              OPEN(UNIT=nfl+i-1,FILE=filen(nfl+i-1),STATUS='UNKNOWN',FORM='UNFORMATTED')
   434: +------      END DO
   435:              
   436:              PRINT *,'Escrevendo ctls'
   437:              !CALL writectl_out(bvar,nvar,dvar,1,tpp_div,tpp_max,tpp_min,ndtout,1)
   438:              CALL writectl_out(bvar,nvar,dvar,2,plp_div,plp_max,plp_min,ndtout,1)
   439:              CALL writectl_out(bvar,nvar,dvar,3,psp_div,psp_max,psp_min,ndtout,1)
   440:            
   441:              !Precipitation
   442:              CALL writectl_out(bvar,nvar,dvar,4,nmb,nmb,1.0,ndtout,0)
   443:            
   444:              CALL writectl_out(bvar,nvar,dvar,5,ccp_div,ccp_max,ccp_min,ndtout,1)
   445:              CALL writectl_out(bvar,nvar,dvar,6,usp_div,usp_max,usp_min,ndtout,1)
   446:              CALL writectl_out(bvar,nvar,dvar,7,vsp_div,vsp_max,vsp_min,ndtout,1)
   447:              CALL writectl_out(bvar,nvar,dvar,8,tsp_div,tsp_max,tsp_min,ndtout,1)
   448:              CALL writectl_out(bvar,nvar,dvar,9, hsp_div,hsp_max,hsp_min,ndtout,1)
   449:              CALL writectl_out(bvar,nvar,dvar,10,sdp_div,sdp_max,sdp_min,ndtout,1)
   450:            
   451:              !Snowfall
   452:            !  CALL writectl_out(bvar,nvar,dvar,11,sfp_div,sfp_max,sfp_min,ndtout,1)
   453:              CALL writectl_out(bvar,nvar,dvar,11,nmb,nmb,1.0,ndtout,2)
   454:            
   455:              CALL writectl_out(bvar,nvar,dvar,12,tap_div,tap_max,tap_min,ndtout,1)
   456:              CALL writectl_out(bvar,nvar,dvar,13,vst_div,vst_max,vst_min,ndtout,1)
   457:            
   458:              CALL cont_ctl(npmx,bvar,nvar,dvar,lvarc,uvarc,ndtout,aunits)
   459:              
   460:              PRINT *,'Escrevendo binarios'
   461: +------>     DO t=1,ndtout
   462: |              WRITE(nfl)    ((tpp_p(i,j,t),i=1,npmx),j=1,tpp_div)
   463: |              WRITE(nfl+1)  ((plp_p(i,j,t),i=1,npmx),j=1,plp_div)
   464: |              WRITE(nfl+2)  ((psp_p(i,j,t),i=1,npmx),j=1,psp_div)
   465: |              WRITE(nfl+4)  ((ccp_p(i,j,t),i=1,npmx),j=1,ccp_div)
   466: |              WRITE(nfl+5)  ((usp_p(i,j,t),i=1,npmx),j=1,usp_div)
   467: |              WRITE(nfl+6)  ((vsp_p(i,j,t),i=1,npmx),j=1,vsp_div)
   468: |              WRITE(nfl+7)  ((tsp_p(i,j,t),i=1,npmx),j=1,tsp_div)
   469: |              WRITE(nfl+8)  ((hsp_p(i,j,t),i=1,npmx),j=1,hsp_div)
   470: |              WRITE(nfl+9)  ((sdp_p(i,j,t),i=1,npmx),j=1,sdp_div)
   471: |          !    WRITE(nfl+10) ((sfp_p(i,j,t),i=1,npmx),j=1,sfp_div)
   472: |              WRITE(nfl+11) ((tap_p(i,j,t),i=1,npmx),j=1,tap_div)
   473: |              WRITE(nfl+12) ((vst_p(i,j,t),i=1,npmx),j=1,vst_div)
   474: +------      END DO
   475:              
   476:              !Precipitation
   477: +------>     DO t=1,ndtout
   478: |              WRITE(nfl+3)  ((pcp(i,j,t),i=1,npmx),j=1,nmb)
   479: +------      END DO
   480:              !Snowfall
   481: +------>     DO t=1,ndtout
   482: |              WRITE(nfl+10)  ((sfp(i,j,t),i=1,npmx),j=1,nmb)
   483: +------      END DO
   484:              
   485:              PRINT *,'Escrevendo controle'
   486: +------>     DO t=1,ndtout
   487: |              WRITE(nfl+13) (tpp(i,nmb,t),i=1,npmx)
   488: |              WRITE(nfl+13) (plp(i,nmb,t),i=1,npmx)
   489: |              WRITE(nfl+13) (psp(i,nmb,t),i=1,npmx)
   490: |              WRITE(nfl+13) (pcp(i,nmb,t),i=1,npmx)
   491: |              WRITE(nfl+13) (ccp(i,nmb,t),i=1,npmx)
   492: |              WRITE(nfl+13) (usp(i,nmb,t),i=1,npmx)
   493: |              WRITE(nfl+13) (vsp(i,nmb,t),i=1,npmx)
   494: |              WRITE(nfl+13) (tsp(i,nmb,t),i=1,npmx)
   495: |              WRITE(nfl+13) (hsp(i,nmb,t),i=1,npmx)
   496: |              WRITE(nfl+13) (sdp(i,nmb,t),i=1,npmx)
   497: |              WRITE(nfl+13) (sfp(i,nmb,t),i=1,npmx)
   498: |              WRITE(nfl+13) (tap(i,nmb,t),i=1,npmx)
   499: |          !    WRITE(nfl+13) (vst(i,nmb,t),i=1,npmx)
   500: +------      END DO
   501:                  
   502: +------>     DO i=1,nvar
   503: |              CLOSE(UNIT=nfl+i-1)
   504: +------      END DO
   505:            
   506:              !Somente para testes
   507:              !CALL Chek_Plumas(npmx,ndtout,nmemb,psp_div,psp,psp_p,cloc(85),85, &
   508:              !                     psp_max,psp_min,'PSPM - Pressao ao Nivel do Mar')
   509:            
   510:            END PROGRAM rwgrhens
