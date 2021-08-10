      SUBROUTINE PLNDER(PLN,DER,PLNWCS,RCS2L,WGTL,EPS,LA1)
C*
C*    CALCULATES ZONAL AND MERIDIONAL PSEUDO-DERIVATIVES AS
C*    WELL AS LAPLACIANS OF THE ASSOCIATED LEGENDRE FUNCTIONS.
C*
C*    ARGUMENT(DIMENSIONS)             DESCRIPTION
C*
C*    PLN   (MNWV1)            INPUT : ASSOCIATED LEGENDRE FUNCTION
C*                                     VALUES AT GAUSSIAN LATITUDE
C*                            OUTPUT : PLN(L,N)=
C*                                     PLN(L,N)*(L+N-2)*(L+N-1)/A**2.
C*                                     ("A" DENOTES EARTH'S RADIUS)
C*                                     USED IN CALCULATING THE
C*                                     LAPLACIAN OF GLOBAL FIELDS
C*                                     IN SPECTRAL FORM.
C*    DER   (MNWV0)           OUTPUT : COSINE-WEIGHTED DERIVATIVES OF
C*                                     ASSOCIATED LEGENDRE FUNCTIONS
C*                                     AT GAUSSIAN LATITUDE
C*    PLNWCS(MNWV0)           OUTPUT : PLNWCS(L,N)=
C*                                     PLN(L,N)*(L-1)/SIN(LATITUDE)**2.
C*    RCS2L                    INPUT : 1.0/SIN(LATITUDE)**2 AT
C*                                     GAUSSIAN LATITUDE
C*    WGTL                     INPUT : GAUSSIAN WEIGHT, AT GAUSSIAN
C*                                     LATITUDE
C*    EPS   (MNWV1)            INPUT : ARRAY OF CONSTANTS USED TO
C*                                     CALCULATE "DER" FROM "PLN".
C*                                     COMPUTED IN ROUTINE "EPSLON".
C*    LA1(MEND1,MEND2)         INPUT : NUMBERING ARRAY FOR PLN
C*
      include 'fftpln.h' 
C*
      INTEGER MEND1,MEND2,MEND3,MNWV2,MNWV0,MNWV3,MNWV1
      PARAMETER (MEND1=MEND+1,MEND2=MEND+2,MEND3=MEND+3,
     *           MNWV2=MEND1*MEND2,MNWV0=MNWV2/2,
     *           MNWV3=MNWV2+2*MEND1,MNWV1=MNWV3/2)
      INTEGER IFP,N,L,NN,MMAX,MM,MN,LM,L0,LP
      REAL RCS2L,WGTL,ERIV,ERSQIV,RAA,WCSA
      INTEGER LA1(MEND1,MEND2)
      REAL PLN(MNWV1),DER(MNWV0),PLNWCS(MNWV0),
     *     EPS(MNWV1),X(MNWV1),AN(MEND2)
C*
      SAVE IFP,ERIV,ERSQIV,AN
      DATA IFP/1/
C*
C*    COMPUTE PLN DERIVATIVES
C*
      IF(IFP.EQ.1) THEN
      ERIV=1.0/FLOAT(6370000)
      ERSQIV=ERIV*ERIV
      DO N=1,MEND2
      AN(N)=FLOAT(N-1)
      ENDDO
      IFP=0
      ENDIF
C*
      RAA=WGTL*ERSQIV
      WCSA=RCS2L*WGTL*ERIV
      L=0
      DO MM=1,MEND1
      L=L+1
      X(L)=AN(MM)
      ENDDO
      DO NN=2,MEND2
      MMAX=MEND3-NN
      DO MM=1,MMAX
      L=L+1
      X(L)=AN(MM+NN-1)
      ENDDO
      ENDDO
C*
      L=MEND1
      DO NN=2,MEND1
      MMAX=MEND2-NN
      DO MM=1,MMAX
      L=L+1
      LM=LA1(MM,NN-1)
      L0=LA1(MM,NN)
      LP=LA1(MM,NN+1)
      DER(L)=X(LP)*EPS(L0)*PLN(LM)-X(L0)*EPS(LP)*PLN(LP)
      ENDDO
      ENDDO
      DO MM=1,MEND1
      DER(MM)=-X(MM)*EPS(MM+MEND1)*PLN(MM+MEND1)
      ENDDO
      DO MN=1,MNWV0
      DER(MN)=WCSA*DER(MN)
      ENDDO
C*
      L=0
      DO NN=1,MEND1
      MMAX=MEND2-NN
      DO MM=1,MMAX
      L=L+1
      L0=LA1(MM,NN)
      PLNWCS(L)=AN(MM)*PLN(L0)
      ENDDO
      ENDDO
      DO MN=1,MNWV0
      PLNWCS(MN)=WCSA*PLNWCS(MN)
      ENDDO
C*
      DO NN=1,MEND1
      MMAX=MEND2-NN
      DO MM=1,MMAX
      L0=LA1(MM,NN)
      LP=LA1(MM,NN+1)
      PLN(L0)=X(L0)*X(LP)*RAA*PLN(L0)
      ENDDO
      ENDDO
C*
      RETURN
      END
