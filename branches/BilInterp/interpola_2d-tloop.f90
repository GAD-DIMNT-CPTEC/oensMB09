! Programa para interpolar um campo meteorológico 2d.
!
! Depende dos modulos bilinear_interpolation.f90 e map_utils.f90 do SCAMTEC 
! (originais da biblioteca iplib do NCEP).
!
! Informações sobre os métodos de interpolação disponíveis:
! http://www.nco.ncep.noaa.gov/pmb/docs/libs/iplib/iplib_doc.html
!
! Compilação:
! source /usr/bin/development_config <6>
! make
!
! Uso:
! $ ./interpola_2d.x filein fileout 
!
! Observação: revisar os parâmetros das grades de entrada/saída nos vetores gridDesci e gridDesco
!
! Histórico:
! 20/03/2017 - versão inicial
! 22/03/2017 - mudança para a versão da bilinear_interpolation.f90 do João Gerd \
!              (resolve problemas com o gridDesc)
! 03/04/2017 - alterado numero de pontos de latitude da grade alvo de 96 para 97 \
!              (permite calculo de latitudes de -90 a 90 no CRPS)
! 15/01/2019 - revisão dos comentários \
!              inclusão de informações sobre as grades correspondentes às resoluções \
!              TQ0062L028 (28x192x96), TQ0126L028 (28x384x192), TQ0213L042 (42x640x320)
!
! carlos.frederico@cptec.inpe.br (15/01/2019) 

program interpola_2d

  use BilinInterp

  implicit none

  ! ìndices utilizados nos loops
  integer :: i    
  integer :: j    
  integer :: k   
  integer :: t

  integer, parameter :: tlength=23 !?

  ! Informações das grades de entrada/saída
  integer :: nptsi ! Número de pontos da grade entrada
  integer :: nptso ! Número de pontos da grade saida
  integer :: nxi   ! Número de pontos em x na grade de entrada
  integer :: nyi   ! Número de pontos em y na grade de entrada
  integer :: nxo   ! Número de pontos em x na grade de saída
  integer :: nyo   ! Número de pontos em y na grade de saída

  real, allocatable, dimension(:) :: gridDesci ! Descrição dos parametros da grade de entrada
  real, allocatable, dimension(:) :: gridDesco ! Descrição dos parametros da grade de saída
  real, allocatable, dimension(:) :: rlat      ! Latitudes em graus
  real, allocatable, dimension(:) :: rlon      ! Longitudes em graus

  ! Códigos do status de leitura/escrita
  integer :: iret  

  ! Parâmetros do método de interpolação
  integer :: ibi   ! Flag (logico) do campo a ser interpolado
  integer :: ibo   ! Flags do campo de saida

  logical*1, dimension(:,:), allocatable :: lb ! 
  logical*1, dimension(:,:), allocatable :: lo ! 

  ! Informações sobre as variáveis
  integer, parameter :: nvar=1 ! Número de variáveis

  real, parameter :: udef=-9.99e+08 ! Valor indefinido a ser utilizado

  ! Coeficientes e índices do polinômio de interpolação
  integer, allocatable, dimension(:) :: n11     ! Indices dos pontos vizinhos
  integer, allocatable, dimension(:) :: n12     ! Indices dos pontos vizinhos
  integer, allocatable, dimension(:) :: n21     ! Indices dos pontos vizinhos
  integer, allocatable, dimension(:) :: n22     ! Indices dos pontos vizinhos

  real, allocatable, dimension(:) :: w12        ! Pesos a serem usados na interpolacao
  real, allocatable, dimension(:) :: w22        ! Pesos a serem usados na interpolacao
  real, allocatable, dimension(:) :: w11        ! Pesos a serem usados na interpolacao
  real, allocatable, dimension(:) :: w21        ! Pesos a serem usados na interpolacao

  ! Arrays intermediários de entrada/saída
  real, allocatable, dimension(:) :: field1di   ! Array 1d intermediário com o campo de entrada
                                                ! (usado para a interpolação)
  real, allocatable, dimension(:) :: field1do   ! Array 1d intermediário com o campo de saída interpolado
                                                ! (contém o resultado da interpolação)

  logical, allocatable, dimension(:) :: ibitmap ! 
  logical, allocatable, dimension(:) :: obitmap ! 

  real, allocatable, dimension(:,:,:) :: worki    ! Array 2d temporário com o campo de entrada
                                                  ! (contém o campo original lido)
  real, allocatable, dimension(:,:,:) :: worko    ! Array 2d temporário de saída com o campo interpolado
                                                  ! (contém o campo interpolado final)

  ! Nomes dos arquivos de entrada/saída
  integer :: ninput, iargc   ! Número de argumentos passados pela linha de comando

  character(1024) :: filein  ! Nome do arquivo de leitura
  character(1024) :: fileout ! Nome do arquivo de escrita

  logical :: existfile

  ! Recebe os nomes dos arquivos de leitura/escrita
  ninput=iargc()

  if(ninput.ne.2)then

    print *
    print *, 'São necessários dois argumentos: <filein> <fileout>'
    stop 'Erro na leitura dos argumentos'

  else

    call getarg(1,filein)
    call getarg(2,fileout)

    ! Verifica se o arquivo de entrada existe
    inquire(file=filein,exist=existfile)

    if(.not.existfile)then
      stop 'Arquivo filein não existe!'
    end if

    ! Aloca os array com os parâmetros das grades de entrada e saída
    allocate(gridDesci(50))
    allocate(gridDesco(50))
 
    ! LATITUDE/LONGITUDE GRIDS
    ! (2)  - N(I) NR POINTS ON LATITUDE CIRCLE
    ! (3)  - N(J) NR POINTS ON LONGITUDE MERIDIAN
    ! (4)  - LA(1) LATITUDE OF ORIGIN
    ! (5)  - LO(1) LONGITUDE OF ORIGIN
    ! (6)  - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
    ! (7)  - LA(2) LATITUDE OF EXTREME POINT
    ! (8)  - LO(2) LONGITUDE OF EXTREME POINT
    ! (9)  - DI LONGITUDINAL DIRECTION OF INCREMENT
    ! (10) - DJ LATITUDINAL DIRECTION INCREMENT
    ! (11) - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)

    ! Parâmetros da grade de entrada
    gridDesci     =0 
    gridDesci( 1) =0         
    gridDesci( 2) =240       
    gridDesci( 3) =121       
    gridDesci( 4) =-90.0000  
    gridDesci( 5) =0.0000    
    gridDesci( 7) =90.0000  
    gridDesci( 8) =360.0000  
    gridDesci( 9) =1.500     
    gridDesci(10) =1.500     
  
    ! Parâmetros da grade de saída (TQ0062L028 - 1 ponto de grade extra nas latitudes)
!    gridDesco    =0 
!    gridDesco( 1)=0          
!    gridDesco( 2)=192        
!    gridDesco( 3)=97         
!    gridDesco( 4)=-90.00000  
!    gridDesco( 5)=0.0000     
!    gridDesco( 7)=90.0000    
!    gridDesco( 8)=360.0000   
!    gridDesco( 9)=1.8750     
!    gridDesco(10)=1.8750     
  
    ! Parâmetros da grade de saída (TQ0062L028)
!    gridDesco    =0 
!    gridDesco( 1)=0          
!    gridDesco( 2)=192        
!    gridDesco( 3)=96
!    gridDesco( 4)=-88.57217
!    gridDesco( 5)=0.0000     
!    gridDesco( 7)=88.57217  
!    gridDesco( 8)=360.0000   
!    gridDesco( 9)=1.8750     
!    gridDesco(10)=1.8750     
!  

    ! Parâmetros da grade de saída (TQ0126L028)
!    gridDesco    =0 
!    gridDesco( 1)=0          
!    gridDesco( 2)=384        
!    gridDesco( 3)=192
!    gridDesco( 4)=-89.28423
!    gridDesco( 5)=0.0000     
!    gridDesco( 7)=89.28423 
!    gridDesco( 8)=360.0000   
!    gridDesco( 9)=0.9375
!    gridDesco(10)=0.9375     
!  

    ! Parâmetros da grade de saída (TQ0213L042 - 1 ponto de grade extra nas latitudes)
    gridDesco    =0 
    gridDesco( 1)=0          
    gridDesco( 2)=640        
    gridDesco( 3)=321
    gridDesco( 4)=-89.57009
    gridDesco( 5)=0.0000     
    gridDesco( 7)=89.57009
    gridDesco( 8)=360.0000   
    gridDesco( 9)=0.5625  
    gridDesco(10)=0.5625     
  
    ! Determina as dimensões das grades de entrada/saída
    nxi=int(gridDesci(2))
    nyi=int(gridDesci(3))
  
    nxo=int(gridDesco(2))
    nyo=int(gridDesco(3))
  
    nptsi=nxi*nyi
    nptso=nxo*nyo
  
    print *
    print *, 'Tamanho das grades de entrada e saida:'
    print *, 'nptsi =', nptsi
    print *, 'nptso =', nptso
  
    ! Aloca os demais arrays
    allocate(worki(nxi,nyi,tlength))
    allocate(worko(nxo,nyo,tlength))
    allocate(field1di(nptsi))
    allocate(ibitmap(nptsi))
    allocate(field1do(nptso))
    allocate(obitmap(nptso))
    allocate(lb(nptso,nvar))
    allocate(lo(nxi,nyi))
    allocate(n11(nptso))
    allocate(n12(nptso))
    allocate(n21(nptso))
    allocate(n22(nptso))
    allocate(rlat(nptso))
    allocate(rlon(nptso))
    allocate(w11(nptso))
    allocate(w21(nptso))
    allocate(w12(nptso))
    allocate(w22(nptso))
 
    ! Abre o arquivo na grade de entrada original para leitura
    open(300,file=trim(filein),access='direct',status='old',form='unformatted', &
         recL=nxi*nyi*4)
  
    open(500,file='worki.grads',access='direct',status='unknown',form='unformatted', &
         action='write',recL=nxi*nyi*4)
  
    ! Le da unidade 300 o array worki com o campo original 
    do t=1, tlength
      read(300,rec=t) ((worki(i,j,t),i=1,nxi),j=1,nyi)
      print *, 't =',t
      print *, 'max worki =', maxval(worki(:,:,t))
      print *, 'min worki =', minval(worki(:,:,t))
      write(500,rec=t) ((worki(i,j,t),i=1,nxi),j=1,nyi)
    end do

    ! Transfere o array 2d worki para o array 1d field1di
    do t=1, tlength

    print *
    print *, 'Interpolação para tempo t =',t      
 
    k=1
    do j=1, nyi
      do i=1, nxi
        field1di(k)=worki(i,j,t)
        k=k+1
      end do
    end do

    !
    ibitmap = .true.
    where(field1di.eq.udef)ibitmap=.false.

    print *
    print *, 'Valores grade de entrada (array 1d):'
    print *, 'maxval = ', maxval(field1di)
    print *, 'minval = ', minval(field1di)
    print *, 'size   =', size(field1di)
  
    ! Determinação dos índices e coeficientes do polinômio interpolador a ser utilizado
  
    call bilinear_interp_init(gridDesci,gridDesco,rlat,rlon, &
                               w11,w12,w21,w22,              &
                               n11,n12,n21,n22)

    print *
    print *, 'Índices calculados para a interpolação:'
    print *, 'size n11 =',   size(n11)
    print *, 'max  n11 =', maxval(n11)
    print *, 'min  n11 =', minval(n11)
  
    print *, 'size n21 =',   size(n21)
    print *, 'max  n21 =', maxval(n21)
    print *, 'min  n21 =', minval(n21)
  
    print *, 'size n12 =',   size(n12)
    print *, 'max  n12 =', maxval(n12)
    print *, 'min  n12 =', minval(n12)
  
    print *, 'size n22 =',   size(n22)
    print *, 'max  n22 =', maxval(n22)
    print *, 'min  n22 =', minval(n22)
  
    print *
    print *, 'Pesos calculados para a interpolação:'
    print *, 'size w11 =',   size(w11)
    print *, 'max  w11 =', maxval(w11)
    print *, 'min  w11 =', minval(w11)
  
    print *, 'size w21 =',   size(w21)
    print *, 'max  w21 =', maxval(w21)
    print *, 'min  w21 =', minval(w21)
  
    print *, 'size w12 =',   size(w12)
    print *, 'max  w12 =', maxval(w12)
    print *, 'min  w12 =', minval(w12)
  
    print *, 'size w22 =',   size(w22)
    print *, 'max  w22 =', maxval(w22)
    print *, 'min  w22 =', minval(w22)
  
    ! Cálculo da interpolação utilizando os índices e pesos calculados anteriormente
  
    ibi  =1      ! integer input bitmap flags (considera (1) ou não (0) valores indefinidos na interpolação)
    lb   =.true. ! logical input bitmaps (?)

    ibo  =1      ! integer output bitmap flags (?)
    lo   =.true. ! logical output bitmaps (?)

    call bilinear_interp(gridDesco,rlat,rlon,ibitmap,field1di,udef, & 
                         w11,w12,w21,w22,                           &
                         n11,n12,n21,n22,                           &
                         obitmap,field1do,iret)
  
    ! Transfere o array 1d field1do para o array 2d worko
    k=1
    do j=1, nyo
      do i=1, nxo
        worko(i,j,t)=field1do(k)
        k=k+1
      end do
    end do
  
  end do ! loop do tempo

    ! Abre o arquivo onde será escrito o campo com a grade de saída interpolada
    open(400,file=trim(fileout),access='direct',status='unknown',form='unformatted', &
         action='write',recL=nxo*nyo*4)

    ! Escreve na unidade 400 o array worko com o campo interpolado
    do t=1, tlength
      write(400,rec=t) ((worko(i,j,t),i=1,nxo),j=1,nyo)
      print *, 't =',t
      print *, 'max worko =', maxval(worko(:,:,t))
      print *, 'min worko =', minval(worko(:,:,t))
    end do
  
    ! Fecha as unidades de leitura/escrita
    close(300)
    close(400)
  
    ! Desaloca os arrays
    deallocate(gridDesci)
    deallocate(gridDesco)
  
    deallocate(worki)
    deallocate(worko)
    deallocate(field1di)
    deallocate(field1do)
    deallocate(lb)
    deallocate(lo)
    deallocate(n11)
    deallocate(n12)
    deallocate(n21)
    deallocate(n22)
    deallocate(rlat)
    deallocate(rlon)
    deallocate(w11)
    deallocate(w21)
    deallocate(w12)
    deallocate(w22)

  end if

end program interpola_2d
