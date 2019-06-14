      SUBROUTINE LINEAR(X,Y,M,XI,YI,N,IER)
C*
C*    Abstract:
C*
C*    LINEAR: performs linear interpolation
C*
C*    Argument(dimension):             Description:
C*
C*    X(M)                     Input:  abscissa of input data:
C*                                     descendent order (INV=1)
C*                                     X(1) > X(2) >...> X(M-1) > X(M);
C*                                     or ascendent order (INV=-1)
C*                                     X(1) < X(2) <...< X(M-1) < X(M).
C*    Y(M)                     Input:  ordinate of input data.
C*    M                        Input:  number of input data;
C*                                     must be greater than 3.
C*
C*    XI(N)                    Input:  abscissa of interpolated data.
C*    YI(N)                    Output: ordinate of interpolated data.
C*    N                        Input:  number of interpolated data.
C*
C*    IER                      Output: error indicator:
C*                                     0 - no errors,
C*                                     1 - number of input data too low,
C*                                     2 - X(I) not in ascendent order,
C*
C*    INV                      Local:  indicator of the order of the
C*                                     input field:
C*                                     descendent order =  1;
C*                                     ascendent  order = -1.
C*
C*
C*    References:
C*
C*    LINEAR is called by the following routines:
C*           ?, ..., and ?.
C*
C*    LINEAR calls no subroutines.
C*
C*    LINEAR does not refer to any COMMON blocks.
C*
C*    LINEAR works on double precision if:
C*           REAL declarations are replaced by DOUBLE PRECISION.
C*
C*    Created at CPTEC in 27 July 1992, by Dr. J. P. Bonatti.
C*
C*    LINEAR is coded in FORTRAN77 computing language:
C*
      INTEGER M,N,IER
      REAL X(M),Y(M),XI(N),YI(N)
C*
      INTEGER M1,I,J,K,INV
      REAL CX,CY,A,B
C*
      M1=M-1
C*
C*    Check for minimum M (at least 4)
C*
      IF (M1.LT.3) THEN
      IER=1
      RETURN
      ENDIF
C*
C*    Changing the order of X(I) and Y(I) for descendent input order
C*
      IER=0
      INV=1
      DO I=1,M1
      IF (X(I).GE.X(I+1)) THEN
      IER=2
      GOTO 10
      ENDIF
      ENDDO
   10 IF (IER.EQ.2) THEN
      IER=0
      INV=-1
      ENDIF
C*
      IF (INV.EQ.-1) THEN
      DO I=1,M/2
      J=M+1-I
      CX=X(I)
      CY=Y(I)
      X(I)=X(J)
      Y(I)=Y(J)
      X(J)=CX
      Y(J)=CY
      ENDDO
      ENDIF
C*
C*    Recheck for X(I) in descendent order
C*
      DO I=1,M1
      IF (X(I).GE.X(I+1)) THEN
      IER=2
      RETURN
      ENDIF
      ENDDO
C*
C*    Begin of interpolation do-loop
C*
      DO J=1,N
C*
C*    Checking X-intervals
C*
      I=M
      IF (XI(J) .LE. X(1)) THEN
      I=2
      GOTO 20
      ENDIF
      DO K=2,M
      IF (XI(J).GT.X(K-1) .AND. XI(J).LE. X(K)) THEN
      I=K
      GOTO 20
      ENDIF
      ENDDO
C*
C*    Computation of Linear interpolation coefficients
C*
   20 A=(Y(I)-Y(I-1))/(X(I)-X(I-1))
      B=Y(I-1)-A*X(I-1)
C*
C*    Linear interpolation
C*
      YI(J)=B+A*XI(J)
C*
C*    End of interpolation do-loop
C*
      ENDDO
C*
      IER=0
C*
C*    Changing the order of X(I) and Y(I) for descendent input order
C*
      IF (INV.EQ.-1) THEN
      DO I=1,M/2
      J=M+1-I
      CX=X(I)
      CY=Y(I)
      X(I)=X(J)
      Y(I)=Y(J)
      X(J)=CX
      Y(J)=CY
      ENDDO
      ENDIF
C*
      RETURN
      END
