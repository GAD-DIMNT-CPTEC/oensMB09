      SUBROUTINE GLATS(KHALF,COLRAD,WGT,WGTCS,RCS2)
C @(#)glats.v1	1.7
C***********************************************************************
C
C     GLATS : CALCULATES GAUSSIAN LATITUDES AND GAUSSIAN WEIGHTS
C             FOR USE IN GRID-SPECTRAL AND SPECTRAL-GRID TRANSFORMS.
C
C***********************************************************************
C
C     GLATS IS CALLED BY THE MAIN ROUTINE SMF.
C
C     GLATS CALLS THE FOLLOWING SUBROUTINE :  POLY
C
C***********************************************************************
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C           KHALF                 INPUT : NUMBER OF GAUSSIAN LATITUDES
C                                         IN ONE HEMISPHERE. SET IN MAIN
C                                         ROUTINE "SMF".
C         COLRAD(KHALF)          OUTPUT : CO-LATITUDES FOR GAUSSIAN
C                                         LATITUDES IN ONE HEMISPHERE.
C          WGT(KHALF)            OUTPUT : GAUSSIAN WEIGHTS.
C         WGTCS(KHALF)           OUTPUT : GAUSSIAN WEIGHTS DIVIDED BY
C                                         COS(LATITUDE)**2.
C          RCS2(KHALF)           OUTPUT : 1.0/COS(LATITUDE)**2 AT
C                                         GAUSSIAN LATITUDES.
C
C***********************************************************************
C
C     GLATS DOES NOT REFER TO ANY COMMONS.
C
C***********************************************************************
C

      DIMENSION COLRAD( KHALF ),WGT( KHALF ),WGTCS( KHALF )
      DIMENSION RCS2  ( KHALF )
C*    PRINT 101
 101  FORMAT ('0 I   COLAT   COLRAD     WGT', 12X, 'WGTCS',
     1 10X, 'ITER  RES')
      EPS=1.0E-12
      SI = 1.0E0
      K2=2*KHALF
      RK2=K2
      SCALE = 2.0E0/(RK2**2)
      K1=K2-1
      PI =  ATAN(SI)* 4.0E0
      DRADZ = PI / 360.E0
      RAD = 0.0
      DO 1000 K=1,KHALF
      ITER=0
      DRAD=DRADZ
1     CALL POLY(K2,RAD,P2)
2     P1 =P2
      ITER=ITER+1
      RAD=RAD+DRAD
      CALL POLY(K2,RAD,P2)
      IF( SIGN(SI,P1).EQ. SIGN(SI,P2)) GO TO 2
C     write(*,*)' k=',k,'  iter=',iter,' eps=',eps,'  drad=',drad
      IF(DRAD.LT.EPS)GO TO 3
      RAD=RAD-DRAD
      DRAD = DRAD * 0.25E0
      GO TO 1
3     CONTINUE
      COLRAD(K)=RAD
      PHI = RAD * 180.E0/ PI
      CALL POLY(K1,RAD,P1)
      X =  COS(RAD)
      W = SCALE * (1.0E0 - X*X)/ (P1*P1)
      WGT(K) = W
      SN =  SIN(RAD)
      W=W/(SN*SN)
      WGTCS(K) = W
      RC=1.0E0/(SN*SN)
      RCS2(K) = RC

      CALL POLY(K2,RAD,P1)
C*    PRINT 102,K,PHI,COLRAD(K),WGT(K),WGTCS(K),ITER,P1
 102  FORMAT(1H ,I2,2X,F6.2,2X,F10.7,2X,E13.7,2X,E13.7,2X,I4,2X,D13.7)
1000  CONTINUE
C*    WRITE (6, 100) KHALF
100   FORMAT(1H ,'SHALOM FROM 0.0 E 0 GLATS FOR ',I3)
      RETURN
      END
