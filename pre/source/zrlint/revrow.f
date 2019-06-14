      SUBROUTINE REVROW (H, LONF, LATG, HS)
      DIMENSION H(LONF,LATG)
      LATGD2 = LATG/2
      DO 20 J1=1,LATGD2
      J2 = LATG + 1 - J1
      DO 10 I=1,LONF
      HT = H(I,J1)
      H(I,J1) = H(I,J2)
      H(I,J2) = HT
10    CONTINUE
20    CONTINUE
      RETURN
      END
