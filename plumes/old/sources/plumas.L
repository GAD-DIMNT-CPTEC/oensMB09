SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:57 2005
FILE NAME: plumas.f90
PROGRAM NAME: plumas
DIAGNOSTIC LIST

  LINE  LEVEL( NO.): DIAGNOSTIC MESSAGE

    16  vec  (   4): Vectorized array expression.
    16  vec  (   4): Vectorized array expression.
    17  vec  (   4): Vectorized array expression.
    17  vec  (   4): Vectorized array expression.
    20  vec  (   4): Vectorized array expression.
    23  mul  (  12): Parallelized by DO option.
    23  vec  (   1): Vectorized loop.
    26  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: plumas$1
    29  mul  (  12): Parallelized by DO option.
    31  vec  (   1): Vectorized loop.
    31  vec  (   1): Vectorized loop.
    41  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: plumas$2
    42  mul  (  12): Parallelized by DO option.
    43  vec  (   1): Vectorized loop.
    50  warn (  82): Name "tent" is not used.
    50  warn (  83): Dummy argument "dt" is not used.
    50  mul  (  11): Parallelized by PARALLEL, and parallel routine generated: plumas$3
SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:57 2005
FILE NAME: plumas.f90
PROGRAM NAME: plumas
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            SUBROUTINE plumas(var,memb,nloc,prob,ndiv,vmax,vmin,dt,dtout)
     2:              IMPLICIT NONE
     3:              
     4:              INTEGER,INTENT(IN) :: ndiv               ! Numero de divisoes entre max e min
     5:              INTEGER,INTENT(IN) :: memb               ! Numero de membros
     6:              INTEGER,INTENT(IN) :: nloc               ! Numero de localidades
     7:              INTEGER,INTENT(IN) :: dt                 ! Numero de timesteps  
     8:              INTEGER,INTENT(IN) :: dtout              ! Numero de timesteps de saida  
     9:              REAL,INTENT(IN)    :: var(nloc,memb,dtout)  ! Variavel para avaliar
    10:              REAL,INTENT(OUT)   :: prob(nloc,ndiv,dtout) ! array de probabilidade 
    11:              REAL,INTENT(OUT)   :: vmax,vmin          ! Valores maximos e minimos
    12:              REAL               :: delta
    13:              REAL               :: faixa(ndiv+1)
    14:              INTEGER            :: i,j,k,t,tent
    15:              
    16: ++V====      vmax=MAXVAL(var) !Maior valor da variavel em todas as localidades
    17: ++V====      vmin=MINVAL(var) !Menor valor da variavel em todas as localidades
    18:            #AMM  delta=(vmax-vmin)/ndiv !delta variavel 
    19:              delta=(vmax-vmin)/(ndiv-1) !delta variavel -modificado para resolver problema na umidade relativa
    20: W**====      prob=0.
    21:              
    22:            !$OMP PARALLEL DO
    23: P------>     DO i=1,ndiv+1
    24: |              faixa(i)=vmin+(i-1)*delta !distribui por faixas
    25: P------      END DO
    26:              faixa(ndiv+1)=2*faixa(ndiv+1) !extrapola faixa maxima
    27:              
    28:            !$OMP PARALLEL DO
    29: P------>     DO i=1,nloc            !Localizacoes
    30: |+----->       DO j=1,memb          ! membros
    31: ||X---->         DO k=1,ndiv        !Procura nas faixas
    32: |||+--->           DO t=1,dtout
    33: ||||                 IF(var(i,j,t)>=faixa(k) .AND. var(i,j,t)<faixa(k+1)) &
    34: ||||       	                           prob(i,k,t)=prob(i,k,t)+1
    35: |||+---            END DO
    36: ||X----          END DO
    37: |+-----        END DO
    38: P------      END DO
    39:              
    40:              !Determina a probabilidade
    41:            !$OMP PARALLEL DO
    42: P------>     DO i=1,nloc !Localizacoes
    43: |W----->       DO k=1,ndiv ! membros
    44: ||*---->         DO t=1,dtout
    45: |||                prob(i,k,t)=prob(i,k,t)/memb
    46: ||*----          END DO
    47: |W-----        END DO
    48: P------      END DO
    49:            
    50:            END SUBROUTINE plumas
