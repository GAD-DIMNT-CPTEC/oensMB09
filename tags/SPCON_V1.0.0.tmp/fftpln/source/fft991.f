      SUBROUTINE FFT991(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
C*
C*-----------------------------------------------------------------------
C*        I=SQRT(-1)
C*        0<X<2*PAI
C*        MAX=N/2
C*     F(X)=F(0)+F(  1)*EXP(-I*  1*X)+F(-  1)*EXP(+I*  1*X)
C*              +F(  2)*EXP(-I*  2*X)+F(-  2)*EXP(+I*  2*X)
C*              +........................
C*              +F(  M)*EXP(-I*  M*X)+F(-  M)*EXP(+I*  M*X)
C*              +........................
C*              +F(MAX)*EXP(-I*MAX*X)+F(-MAX)*EXP(+I*MAX*X)
C*-----------------------------------------------------------------------
C*..  A    (JUMP,LOT)   ISIGN=+1
C*                 INPUT ARRAY  FOR M=0,1,2,....,N/2
C*                      A(2*M+1)=REAL(F(M))
C*                      A(2*M+2)=IMAG(F(M))
C*                      A(  1)=COS(M= 0)   ,A(2)=0.
C*                      A(  3)=COS(M= 1)   ,A(4)=SIN(M=1)
C*                      A(  5)=COS(M= 2)   ,A(6)=SIN(M=2)
C*                      .................................................
C*                      A(N-1)=COS(M=N/2-1),A(N)=SIN(M=N/2-1)
C*                      A(N+1)=COS(M=N/2)
C*                 OUTPUT ARRAY
C*                      GRID POINT VALUES
C*                      A(N+1)=0.,......A(JUMP)=0.
C*..  A    (JUMP,LOT)  ISIGN=-1
C*                 INPUT ARRAY
C*                      GRID POINT VALUES
C*                      A(N+1)=0.,......A(JUMP)=0.
C*                 OUTPUT ARRAY  FOR M=0,1,2,....,N/2
C*                      A(2*M+1)=REAL(F(M))
C*                      A(2*M+2)=IMAG(F(M))
C*                      A(  1)=COS(M= 0)   ,A(2)=0.
C*                      A(  3)=COS(M= 1)   ,A(4)=SIN(M= 1)
C*                      A(  5)=COS(M= 2)   ,A(6)=SIN(M= 2)
C*                      .................................................
C*                      A(N-1)=COS(M=N/2-1),A(N)=SIN(M=N/2-1)
C*                      A(N+1)=COS(M=N/2)
C*..  WORK (JUMP,LOT)     WORK  ARRAY
C*..  TRIGS(.GT.1.5*IMAX) SIN,COS COEFFICIENT
C*..  IFAX (*)            FACTORS OF IMAX
C*..  INC                 INCREMENT OF DATA SPACING
C*..  JUMP                JUMP.GE.N+1
C*..  N                   NUMBER OF GRIDS TO BE TRASFORMED
C*..  LOT                 NUMBER OF LINES TRANSFORMED SIMULTANEOUSLY
C*..  ISIGN=+1            WAVE TO GRID TRANSFORM
C*          -1            GRID TO WAVE TRANSFORM
C*-----------------------------------------------------------------------
C*
      INTEGER INC,JUMP,N,LOT,ISIGN
      INTEGER IFAX(*)
      REAL A(*),WORK(*),TRIGS(*)
C*
      INTEGER NFAX,NX,NH,INK,IGO,IBASE,JBASE,L,I,J,M,IA,LA,K,IB
C*
      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      INK=INC+INC
      IF(ISIGN.EQ.1) GO TO 30
C*
      IGO=50
      IF(MOD(NFAX,2).EQ.1) GO TO 40
      IBASE=1
      JBASE=1
      DO 20 L=1,LOT
      I=IBASE
      J=JBASE
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO 10 M=1,N
      WORK(J)=A(I)
      I=I+INC
      J=J+1
   10 CONTINUE
      IBASE=IBASE+JUMP
      JBASE=JBASE+NX
   20 CONTINUE
C*
      IGO=60
      GO TO 40
C*
   30 CONTINUE
      CALL FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
      IGO=60
C*
   40 CONTINUE
      IA=1
      LA=1
      DO 80 K=1,NFAX
      IF(IGO.EQ.60) GO TO 60
   50 CONTINUE
      CALL VPASSM(A(IA),A(IA+INC),WORK(1),WORK(2),TRIGS,INK,2,JUMP,NX,
     *            LOT,NH,IFAX(K+1),LA)
      IGO=60
      GO TO 70
   60 CONTINUE
      CALL VPASSM(WORK(1),WORK(2),A(IA),A(IA+INC),TRIGS,2,INK,NX,JUMP,
     *            LOT,NH,IFAX(K+1),LA)
      IGO=50
   70 CONTINUE
      LA=LA*IFAX(K+1)
   80 CONTINUE
C*
      IF(ISIGN.EQ.-1) GO TO 130
C*
      IF(MOD(NFAX,2).EQ.1) GO TO 110
      IBASE=1
      JBASE=1
      DO 100 L=1,LOT
      I=IBASE
      J=JBASE
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO 90 M=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
   90 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  100 CONTINUE
C*
  110 CONTINUE
      IB=N*INC+1
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO 120 L=1,LOT
      A(IB)=0.0
      A(IB+INC)=0.0
      IB=IB+JUMP
  120 CONTINUE
      RETURN
C*
  130 CONTINUE
      CALL FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
C*
  140 CONTINUE
      RETURN
C*
      END
