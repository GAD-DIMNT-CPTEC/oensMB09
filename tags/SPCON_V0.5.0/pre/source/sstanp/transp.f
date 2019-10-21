      SUBROUTINE TRANSP(A,MNWV2,MEND1,NEND1,JEND1,KMAX,LA0,W,ISIGN)
C @(#)transp.v1	1.7
C***********************************************************************
C
C     TRANSP : AFTER INPUT, TRANSPOSES ARRAYS OF SPECTRAL COEFFICIENTS
C              BY SWAPPING THE ORDER OF THE SUBSCRIPTS REPRESENTING THE
C              DEGREE AND ORDER OF THE ASSOCIATED LEGENDRE FUNCTIONS.
C
C***********************************************************************
C
C     TRANSP IS CALLED BY THE SUBROUTINES GREAD
C
C     TRANSP CALLS NO SUBROUTINES.
C
C***********************************************************************
C
C    ARGUMENT(DIMENSIONS)                       DESCRIPTION
C
C        A(MNWV2,KMAX)            INPUT : SPECTRAL REPRESENTATION OF A
C                                         GLOBAL FIELD AT "N" LEVELS.
C                                ISIGN=+1 DIAGONALWISE STORAGE
C                                ISIGN=-1 COLUMNWISE   STORAGE
C                                OUTPUT : SPECTRAL REPRESENTATION OF A
C                                         GLOBAL FIELD AT "N" LEVELS.
C                                ISIGN=+1 COLUMNWISE   STORAGE
C                                ISIGN=-1 DIAGONALWISE STORAGE
C        W(MNWV2)                         WORK SPACE
C        LA0(MEND1,NEND1)         INPUT : ORDERING ARRAY IN DIAGONAL
C                                         DIRECTION
C             MEND1               INPUT :
C             NEND1               INPUT : } TRUNCATION WAVE NUMBERS
C             JEND1               INPUT :
C
C                          !
C                     JEND1!_      ____________
C                          !      /            !
C                          !    /              !
C                          !  /               /
C                     NEND1!/               /
C                          !              /
C                          !            /
C                          !          /
C                          !        /
C                          !      /
C                          !    /
C                          !  /
C                          !/___________________!_____
C                          1                  MEND1
C
C             MNWV2               INPUT : NUMBER OF ELEMENTS
C             KMAX                INPUT : NUMBER OF LEVELS.
C***********************************************************************
C
      DIMENSION A(MNWV2,KMAX)
      DIMENSION W(MNWV2)
      DIMENSION LA0(MEND1,NEND1)
C
CVD$R NOVECTOR
*vdir novector
      IF(ISIGN.EQ.1) THEN
      DO 3 K=1,KMAX
      L=0
      DO 1 MM=1,MEND1
      IF(MM.LE.JEND1-NEND1+1) THEN
      NMAX=NEND1
      ELSE
      NMAX=JEND1+1-MM
      END IF
      DO 1 NN=1,NMAX
      L=L+1
      LX=LA0(MM,NN)
      W(2*L-1)=A(2*LX-1,K)
      W(2*L  )=A(2*LX  ,K)
 1    CONTINUE
      DO 2 MN=1,MNWV2
      A(MN,K)=W(MN)
 2    CONTINUE
 3    CONTINUE
      ELSE
      DO 4 K=1,KMAX
      L=0
      DO 5 MM=1,MEND1
      IF(MM.LE.JEND1-NEND1+1) THEN
      NMAX=NEND1
      ELSE
      NMAX=JEND1+1-MM
      END IF
      DO 5 NN=1,NMAX
      L=L+1
      LX=LA0(MM,NN)
      W(2*LX-1)=A(2*L-1,K)
      W(2*LX  )=A(2*L  ,K)
 5    CONTINUE
      DO 6 MN=1,MNWV2
      A(MN,K)=W(MN)
 6    CONTINUE
 4    CONTINUE
      END IF
      RETURN
      END