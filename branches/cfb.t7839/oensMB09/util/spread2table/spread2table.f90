! Este programa lê uma lista com os arquivos de spread do ensemble do CPTEC,
! calcula a média para diferentes áreas do globo (gl, hn, tr, hs, as) e organiza 
! os resultados em tabelas semelhantes às do SCANTEC:
!
! %Previsao suvel:850 suvel:500 suvel:250 ... stemp:850 stemp:500 stemp:250
!       000
!       006
!       012
!       ...
!       360
!
! Arquivos a serem lidos: spreadYYYYiMMiDDiHHiYYYYfMMfDDfHHf.TQ0126L028.bin
! Binário não formatado com acesso sequencial
! Cada arquivo possui 7 variáveis, 1 tempo, 6 variáveis com 3 níveis e 1 variável com 1 nível
! suvel = nxpts * nypts * nzpts
! svvel = nxpts * nypts * nzpts
! sfcor = nxpts * nypts * nzpts
! spotv = nxpts * nypts * nzpts
! szgeo = nxpts * nypts * nzpts
! spsnm = nxpts * nypts 
! stemp = nxpts * nypts * nzpts
! Tamanho total do arquivo: (1*(384*192)*4 + 1*8 + 6*(384*192*3)*4 + 6*3*8) = 5603480 bytes
!
! Compilação:
! $ make
!
! Uso: 
!./spread_to_table-sub.x
!
! carlos.bastarz@inpe.br (04/11/2020)

program spread_to_table

  use m_gauss2lats

  implicit none

  integer :: i, j, v, ierr, cont, hour
  integer :: rnxpts, rnypts, xbeg, xend, ybeg, yend
  integer, parameter :: nxpts=384, nypts=192, nzpts=3, nvars=7

  ! Médias das variáveis nos níveis indicados
  real :: fmsuvel850, fmsuvel500, fmsuvel250 
  real :: fmsvvel850, fmsvvel500, fmsvvel250 
  real :: fmsfcor850, fmsfcor500, fmsfcor250 
  real :: fmspotv850, fmspotv500, fmspotv250 
  real :: fmszgeo850, fmszgeo500, fmszgeo250 
  real :: fmspsnm000 
  real :: fmstemp850, fmstemp500, fmstemp250 

  real, allocatable, dimension(:) :: wght, latweight, lats
  real, allocatable, dimension(:,:) :: aloc

  logical :: exarq, verif

  character(len=2) :: reg
  character(len=3) :: f3
  character(len=17) :: fmt                             ! usado na escrita do arquivo binário de verificação
  character(len=17) :: fname                           ! arquivo binário de saída
  character(len=24) :: namenml='inputparameters.nml'   ! nome do arquivo de namelist
  character(len=138) :: spreadfile                     ! arquivo binário de entrada
  character(len=100) :: dirin, filein, dirout, fileout
  character(len=200) :: fheader

  ! Parâmetros do namelist a serem lidos
!  namelist /InputParams/ reg, verif, nxpts, nypts, nzpts, nvars, dirin, filein, dirout, fileout
  namelist /InputParams/ reg, verif, dirin, filein, dirout, fileout

  fmt='(I3.3)'

  reg='ge'
  verif=.FALSE.
!  nxpts=192
!  nypts=96
!  nzpts=1
!  nvars=1
  dirin='./'
  filein='filein.txt'
  dirout='./'
  fileout='fileout.txt'

  allocate(latweight(nypts), stat=ierr)
  if (ierr.ne.0) stop 'latweight nao alocou corretamente'

  allocate(lats(nypts), stat=ierr)
  if (ierr.ne.0) stop 'lats nao alocou corretamente'
  
  ! Abre o arquivo de namelist para leitura
  inquire(file=trim(namenml), exist=exarq)
  if (exarq) then
    open (20, file=trim(namenml), form='formatted', access='sequential', action='read', status='old', iostat=ierr)
    if (ierr.ne.0) stop 'arquivo namelist nao abriu corretamente'
    read(20, nml=InputParams)
    close(20)
  else
    stop 'arquivo namelist nao encontrado'
  end if

  print *
  print *, 'reg= ', reg
  print *, 'verif= ', verif
  print *, 'nxpts= ', nxpts
  print *, 'nypts= ', nypts
  print *, 'nzpts= ', nzpts
  print *, 'nvars= ', nvars
  print *, 'dirin= ', dirin
  print *, 'filein= ', filein
  print *, 'dirout= ', dirout
  print *, 'fileout= ', fileout

  ! Verifica se o arquivo com a lista de arquivo existe e abre
  inquire(file=trim(dirin)//trim(filein), exist=exarq)
  if (exarq) then
    open(30, file=trim(dirin)//trim(filein), status='old', form='formatted', iostat=ierr)
    if (ierr.ne.0) stop 'lista de arquivos de entrada nao abriu corretamente'
  else
    stop 'lista de arquivos de entrada nao existe'
  end if

  ! Define o nome do arquivo de saída e abre a unidade para a escrita
  open(40, file=trim(dirout)//trim(fileout), status='replace', action='write', form='formatted', access='sequential', iostat=ierr)
  if (ierr.ne.0) stop 'arquivo de saida nao abriu corretamente para escrita'

  ! Cabeçalho do arquivo de saída
  fheader = trim('%Previsao SUVEL-850 SUVEL-500 SUVEL-250 SVVEL-850 SVVEL-500 SVVEL-250 ' // & 
                 'SFCOR-850 SFCOR-500 SFCOR-250 SPOTV-850 SPOTV-500 SPOTV-250 SZGEO-850 ' // &
                 'SZGEO-500 SZGEO-250 SPSNM-000 STEMP-850 STEMP-500 STEMP-250')
  write(40, '(A)') fheader

  ! Início do loop para a leitura da lista de arquivos
  cont=0
  do

    hour = cont * 6 ! tempo das previsões (primeira coluna do arquivo de saída)

    ! Lê os arquivos da lista
    read(30, '(A)', iostat=ierr) spreadfile
    if (ierr.ne.0) then
      if (ierr.eq.-1) then
        stop 'fim da lista de arquivos de entrada'
      else
        stop 'erro na leitura da lista de arquivos de entrada'
      end if
    end if

    ! Imprime na tela as horas de previão e o nome do arquivo a ser lido
    print *
    print *, hour, spreadfile

    ! Verifica se o arquivo de espalhamento existe e abre (unidade 50)
    if (exarq) then
      open(50, file=trim(spreadfile), status='old', action='read', form='unformatted', access='sequential', iostat=ierr)
      if (ierr.ne.0) stop 'arquivo de espalhamento nao abriu corretamente'
    else
      stop 'arquivo de espalhamento nao existe'
    end if

    ! Define o número de pontos e os limites da região escolhida (em coordenadas de grade, x e y)
    call set_reg_dims(reg, nxpts, nypts, xbeg, xend, ybeg, yend, rnxpts, rnypts)

    ! Se verif=.TRUE., abre a unidade 60 para escrita em disco do arquivo que está sendo lido 
    if (verif) then
      write(f3, fmt) hour
      fname = 'file_fct'//trim(f3)//'h.bin'
      open(60, file=trim(dirout)//trim(fname), status='unknown', form='unformatted', access='sequential')
    end if

    call latitudes(nypts, lats, latweight)

    print *, 'debug'

    print *, 'xend, xbeg, yend, ybeg, (xend-xbeg+1)*(yend-ybeg+1)=', xend, xbeg, yend, ybeg, (xend-xbeg+1)*(yend-ybeg+1)

    allocate(wght((xend-xbeg+1)*(yend-ybeg+1)), stat=ierr)
    if (ierr.ne.0) stop 'wght nao alocou corretamente'

    allocate(aloc(nxpts,nypts), stat=ierr)
    if (ierr.ne.0) stop 'aloc nao alocou corretamente'
  
    ! Aplica uma mascara (ver subrotina subdomain do modulo m_gauss2lats) no array dos pesos para a regiao escolhida 
    ! (os pesos a serem usados estarao no array wght)
    ! Obs: as latitudes sempre são calculadas entre ~ -90 e 90 graus. A mascara separa a regiao de interesse.
    do i=1, nxpts
      do j=1, nypts
        aloc(i,j) = latweight(j)
      enddo
    enddo

    call subdomain(nxpts, nypts, aloc, xbeg, xend, ybeg, yend, wght)
    print *, 'wght', shape(wght), minval(wght), maxval(wght)

    deallocate(aloc)

    ! Escreve os pesos no arquivo wght.grads para verificacao no GrADS
    open(70, file='wght.grads', form='unformatted', action='write', access='sequential', status='replace')
    write(70) wght
    close(70)

    ! Faz um loop entre as variáveis do arquivo de espalhamento (unidade 50), lê as informações, 
    ! calcula as médias das variáveis para a área e escreve o resultado no arquivo de saída (unidade 50)
    do v=1, nvars
      call readwrite_binrec(v, 50, 60, xbeg, xend, ybeg, yend,  &
                            rnxpts, rnypts, nzpts, nvars,        & 
                            fmsuvel850, fmsuvel500, fmsuvel250, & 
                            fmsvvel850, fmsvvel500, fmsvvel250, & 
                            fmsfcor850, fmsfcor500, fmsfcor250, & 
                            fmspotv850, fmspotv500, fmspotv250, & 
                            fmszgeo850, fmszgeo500, fmszgeo250, & 
                            fmspsnm000,                         & 
                            fmstemp850, fmstemp500, fmstemp250, &
                            reg, verif)
    end do

    deallocate(wght)

    ! Escreve no arquivo de saída (unidade 40) a linha com o tempo de previsão e as médias do espelhamento
    ! das variáveis sobre a região escolhida
    write(40, '(I9,19ES10.3)') hour, fmsuvel850, fmsuvel500, fmsuvel250, & 
                                     fmsvvel850, fmsvvel500, fmsvvel250, & 
                                     fmsfcor850, fmsfcor500, fmsfcor250, &
                                     fmspotv850, fmspotv500, fmspotv250, &
                                     fmszgeo850, fmszgeo500, fmszgeo250, &
                                     fmspsnm000,                         &
                                     fmstemp850, fmstemp500, fmstemp250

    if (verif) close(60)

    close(50)

    cont=cont+1

  end do

  close(40)
  close(30)

  deallocate(lats, latweight)

  contains
  
  subroutine readwrite_binrec(vindex, funit, funitout, fxbeg, fxend, fybeg, &
                              fyend, fnxpts, fnypts, fnzpts, fnvars,        &
                              msuvel850, msuvel500, msuvel250,              & 
                              msvvel850, msvvel500, msvvel250,              & 
                              msfcor850, msfcor500, msfcor250,              & 
                              mspotv850, mspotv500, mspotv250,              & 
                              mszgeo850, mszgeo500, mszgeo250,              & 
                              mspsnm000,                                    & 
                              mstemp850, mstemp500, mstemp250,              &
                              recreg, fverif)
  
    integer :: i, k, fierr, fnpts
  
    integer, intent(in) :: vindex, funit, funitout             ! índice da variável, unidade de leitura, unidade de escrita 
    integer, intent(in) :: fnxpts, fnypts, fnzpts              ! número de pontos em x, y, e z (lats, lons, levs)
    integer, intent(in) :: fnvars                              ! número de variáveis
    integer, intent(in) :: fxbeg, fxend, fybeg, fyend          ! xi, xf, yi, yf
  
    real :: wmean, wwmean                                      ! média da área
    real :: wsum, wwsum                        

    real, intent(out) :: msuvel850, msuvel500, msuvel250       ! array uvel 
    real, intent(out) :: msvvel850, msvvel500, msvvel250       ! array vvel
    real, intent(out) :: msfcor850, msfcor500, msfcor250       ! array fcor
    real, intent(out) :: mspotv850, mspotv500, mspotv250       ! array potv
    real, intent(out) :: mszgeo850, mszgeo500, mszgeo250       ! array zgeo
    real, intent(out) :: mspsnm000                             ! array psnm
    real, intent(out) :: mstemp850, mstemp500, mstemp250       ! array temp

    real, allocatable, dimension(:) :: work, tmp, outwork      ! array temporário (1d), array de saída (1d)
    real, allocatable, dimension(:,:,:) :: sp                  ! array das variáves sp(val,var,lev)
  
    character(len=5) :: vname                                  ! nome da variável
    character(len=2), intent(in) :: recreg                     ! nome da região

    logical, intent(in) :: fverif
  
    ! Número de pontos total da área escolhida
    fnpts = fnxpts * fnypts
  
    print *
    print *, 'reg=', recreg
    print *, 'fnxpts=' , fnxpts, ' fnypts=', fnypts, ' fnpts=', fnpts
    print *, 'fxbeg=' , fxbeg, ' fxend=', fxend
    print *, 'fybeg=' , fybeg, ' fyend=', fyend
  
    ! Alocação dos arrays
    allocate(outwork(fnpts), stat=fierr)
    if (fierr.ne.0) stop 'outwork nao alocou corretamente'
  
    allocate(work(fnpts), stat=fierr)
    if (fierr.ne.0) stop 'work nao alocou corretamente'
  
    allocate(sp(fnpts,fnzpts,fnvars), stat=fierr)
    if (fierr.ne.0) stop 'sp nao alocou corretamente'
  
    if (fverif) then
      allocate(tmp(fnpts), stat=fierr)
      if (fierr.ne.0) stop 'tmp nao alocou corretamente'
    end if
  
    ! Define os nomes das variáveis de acordo com o índice
    if (vindex.eq.1) vname='suvel'
    if (vindex.eq.2) vname='svvel'
    if (vindex.eq.3) vname='sfcor'
    if (vindex.eq.4) vname='spotv'
    if (vindex.eq.5) vname='szgeo'
    if (vindex.eq.6) vname='spsnm'
    if (vindex.eq.7) vname='stemp'
 
    print *
    print *, 'wght shape, min, max=', shape(wght), minval(wght), maxval(wght)

    ! Início da leitura dos records
    if (vindex.eq.6) then ! psnm
  
      print *    
      print *, vname
  
      read(funit) work
      sp(:,1,vindex) = work

      ! Se verif=.TRUE., escreve na unidade funitout (60) o record lido para verificação
      if (fverif) write(funitout) sp(:,1,vindex)

      ! Cálculo da média (soma todos os valores e divide pelo número de pontos)
      wmean = 0.
      wwmean = 0.
      wsum = 0.
      wwsum = 0.
      do i=1, fnpts
        wmean = wmean + sp(i,1,vindex) 
        wwmean = wwmean + sp(i,1,vindex) * wght(i)
        wsum = wsum + sp(i,1,vindex) 
        wwsum = wwsum + sp(i,1,vindex) * wght(i)
      end do
  
      wmean = wmean / fnpts
      wwmean = wwmean / sum(wght)
  
      print *, 'amean k=', 1, 'min=', minval(sp(:,1,vindex)), 'max=', maxval(sp(:,1,vindex)), '&
                wsum=', wsum, 'mean=', wmean, 'n=', fnpts
      print *, 'aave  k=', 1, 'min=', minval(sp(:,1,vindex)), 'max=', maxval(sp(:,1,vindex)), '&
                wsum=', wwsum, 'mean=', wwmean, 'n=', fnpts
  
      mspsnm000 = wwmean ! sera gravado no arquivo a media ponderada pelo cosseno das latitudes
  
    else ! demais variáveis
  
      print *
      print *, vname
  
      do k=1, fnzpts ! niveis
        read(funit) work
        sp(:,k,vindex) = work
  
        ! Se verif=.TRUE., escreve na unidade funitout (60) o record lido para verificação
        if (fverif) write(funitout) sp(:,k,vindex)
  
        ! Cálculo da média (soma todos os valores e divide pelo número de pontos)
        wwmean = 0.
        wwsum = 0.
        wmean = 0.
        wsum = 0.
        do i=1, fnpts
          wmean = wmean + sp(i,k,vindex)
          wwmean = wwmean + sp(i,k,vindex) * wght(i)
          wsum = wsum + sp(i,k,vindex)
          wwsum = wwsum + sp(i,k,vindex) * wght(i)
        end do
  
        wmean = wsum / fnpts
        wwmean = wwsum / sum(wght)
  
        print *, 'amean k=', k, 'min=', minval(sp(:,k,vindex)), 'max=', maxval(sp(:,k,vindex)), '&
                 wsum=', wsum, 'mean=', wmean, 'n=', fnpts
        print *, 'aave  k=', k, 'min=', minval(sp(:,k,vindex)), 'max=', maxval(sp(:,k,vindex)), '&
                 wsum=', wwsum, 'mean=', wwmean, 'n=', fnpts
  
        ! serao gravados no arquivo as medias ponderadas pelo cosseno das latitudes
        if (k.eq.1.and.vname.eq.'suvel') msuvel850 = wwmean
        if (k.eq.2.and.vname.eq.'suvel') msuvel500 = wwmean
        if (k.eq.3.and.vname.eq.'suvel') msuvel250 = wwmean
  
        if (k.eq.1.and.vname.eq.'svvel') msvvel850 = wwmean
        if (k.eq.2.and.vname.eq.'svvel') msvvel500 = wwmean
        if (k.eq.3.and.vname.eq.'svvel') msvvel250 = wwmean
  
        if (k.eq.1.and.vname.eq.'sfcor') msfcor850 = wwmean
        if (k.eq.2.and.vname.eq.'sfcor') msfcor500 = wwmean
        if (k.eq.3.and.vname.eq.'sfcor') msfcor250 = wwmean
  
        if (k.eq.1.and.vname.eq.'spotv') mspotv850 = wwmean
        if (k.eq.2.and.vname.eq.'spotv') mspotv500 = wwmean
        if (k.eq.3.and.vname.eq.'spotv') mspotv250 = wwmean
  
        if (k.eq.1.and.vname.eq.'szgeo') mszgeo850 = wwmean
        if (k.eq.2.and.vname.eq.'szgeo') mszgeo500 = wwmean
        if (k.eq.3.and.vname.eq.'szgeo') mszgeo250 = wwmean
  
        if (k.eq.1.and.vname.eq.'stemp') mstemp850 = wwmean
        if (k.eq.2.and.vname.eq.'stemp') mstemp500 = wwmean
        if (k.eq.3.and.vname.eq.'stemp') mstemp250 = wwmean
 
      end do 
  
    end if
  
    if (fverif) deallocate(tmp)
  
    deallocate(work, sp, outwork)
  
  end subroutine readwrite_binrec
  
  subroutine set_reg_dims(freg, fnxpts, fnypts, fxbeg, fxend, fybeg, fyend, frnxpts, frnypts)
  
    integer, intent(in) :: fnxpts, fnypts
    integer, intent(out) :: fxbeg, fxend, fybeg, fyend, frnxpts, frnypts 
  
    real :: rlat1, rlat2, rlon1, rlon2, fxbeg2, fxend2, fybeg2, fyend2

    character(len=2), intent(in) :: freg

    ! w2gr -> world corrdinates (lat/lon em graus) to grid coordinates (x/y em unidades de grade)
    ! ge: lon1=0,       lon2=360,     x1=1,   x2=384, nxpts=384
    !     lat1=-90,     lat2=90,      y1=1,   y2=192, nypts=192
    ! gl: lon1=0,       lon2=360,     x1=1,   x2=384, nxpts=384
    !     lat1=-80,     lat2=80,      y1=11,  y2=182, nypts=172
    ! hn: lon1=0,       lon2=360,     x1=1,   x2=384, nxpts=384
    !     lat1=20,      lat2=80,      y1=118, y2=182, nypts=65
    ! tr: lon1=0,       lon2=360,     x1=1,   x2=384, nxpts=384
    !     lat1=-20,     lat2=20,      y1=75,  y2=118, nypts=44
    ! hs: lon1=0,       lon2=360,     x1=1,   x2=384, nxpts=384
    !     lat1=-80,     lat2=-20,     y1=11,  y2=75,  nypts=65
    ! as: lon1=-82.625, lon2=-35.375, x1=104, x2=156, nxpts=53 
    !     lat1=-49.875, lat2=11.375,  y1=43,  y2=109, nypts=67
  
    ! A subrotina llij_latlon do modulo m_gauss2lats calcula os pontos i/j (na grade) a partir do lat/lon (em graus)

    if (freg.eq.'ge') then
      rlat1=-90.0;   rlat2=90.0
      rlon1=0.0  ;   rlon2=360.0
    else if (freg.eq.'gl') then
      rlat1=-80.0;   rlat2=80.0
      rlon1=0.0  ;   rlon2=360.0
    else if (freg.eq.'hn') then
      rlat1=20.0;    rlat2=80.0
      rlon1=0.0 ;    rlon2=360.0
    else if (freg.eq.'tr') then
      rlat1=-20.0;   rlat2=20.0
      rlon1=0.0  ;   rlon2=360.0
    else if (freg.eq.'hs') then
      rlat1=-80.0;   rlat2=-20.0
       rlon1=0.0  ;   rlon2=360.0
    else if (freg.eq.'as') then
      rlat1=-49.875; rlat2=11.375
      rlon1=-82.625; rlon2=-35.375
    else 
      rlat1=-90.0;   rlat2=90.0
      rlon1=0.0  ;   rlon2=360.0
    end if

    call llij_latlon(fnxpts, fnypts, rlat1, rlon1, fxbeg2, fybeg2)
    call llij_latlon(fnxpts, fnypts, rlat2, rlon2, fxend2, fyend2)

    if (fxend2.gt.fnxpts) fxend2=fxend2-1
    if (fyend2.gt.fnypts) fyend2=fyend2-1

!    print *, 'fxbeg2=', int(fxbeg2) 
!    print *, 'fxend2=', int(fxend2)
!    print *, 'fybeg2=', int(fybeg2)
!    print *, 'fyend2=', int(fyend2)
! 
!    print *
!
!    print *, 'fxbeg=', fxbeg 
!    print *, 'fxend=', fxend
!    print *, 'fybeg=', fybeg
!    print *, 'fyend=', fyend
! 
!    print *

    fxbeg=int(fxbeg2) 
    fxend=int(fxend2)
    fybeg=int(fybeg2)
    fyend=int(fyend2)

    frnxpts = (fxend - fxbeg) + 1
    frnypts = (fyend - fybeg) + 1
 
  end subroutine set_reg_dims

end program spread_to_table
