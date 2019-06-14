      SUBROUTINE TRANSS(A,KDIM,ISIGN)
C*
C*    TRANSP: AFTER INPUT, TRANSPOSES SCALAR ARRAYS
C*            OF SPECTRAL COEFFICIENTS BY SWAPPING
C*            THE ORDER OF THE SUBSCRIPTS
C*            REPRESENTING THE DEGREE AND ORDER
C*            OF THE ASSOCIATED LEGENDRE FUNCTIONS.
C*
C*    ARGUMENT(DIMENSIONS)        DESCRIPTION
C*
C*    A(MNWV2,KDIM)        INPUT: SPECTRAL REPRESENTATION OF A
C*                                GLOBAL FIELD AT "N" LEVELS.
C*                                ISIGN=+1 DIAGONALWISE STORAGE
C*                                ISIGN=-1 COLUMNWISE   STORAGE
C*                        OUTPUT: SPECTRAL REPRESENTATION OF A
C*                                GLOBAL FIELD AT "N" LEVELS.
C*                                ISIGN=+1 COLUMNWISE   STORAGE
C*                                ISIGN=-1 DIAGONALWISE STORAGE
C*    KDIM                 INPUT: NUMBER OF LAYERS.
C*
      include 'fftpln.h'
C*
      INTEGER MEND1,MEND2,MNWV2
      PARAMETER(MEND1=MEND+1,MEND2=MEND+2,
     *          MNWV2=MEND1*MEND2)
C*
      INTEGER KDIM,ISIGN
      INTEGER LA0(MEND1,MEND1),LA1(MEND1,MEND2)
      REAL W(MNWV2),A(MNWV2,KDIM)
C*
      COMMON /LA0LA1/ LA0,LA1
C*
      INTEGER K,L,LX,MN,MM,NMAX,NN
C*
      IF(ISIGN.EQ.1) THEN
C*
      DO K=1,KDIM
C*
      L=0
      DO MM=1,MEND1
      NMAX=MEND2-MM
      DO NN=1,NMAX
      L=L+1
      LX=LA0(MM,NN)
      W(2*L-1)=A(2*LX-1,K)
      W(2*L  )=A(2*LX  ,K)
      ENDDO
      ENDDO
C*
      DO MN=1,MNWV2
      A(MN,K)=W(MN)
      ENDDO
C*
      ENDDO
C*
      ELSE
C*
      DO K=1,KDIM
C*
      L=0
      DO MM=1,MEND1
      NMAX=MEND2-MM
      DO NN=1,NMAX
      L=L+1
      LX=LA0(MM,NN)
      W(2*LX-1)=A(2*L-1,K)
      W(2*LX  )=A(2*L  ,K)
      ENDDO
      ENDDO
C*
      DO MN=1,MNWV2
      A(MN,K)=W(MN)
      ENDDO
C*
      ENDDO
C*
      ENDIF
C*
      RETURN
      END
