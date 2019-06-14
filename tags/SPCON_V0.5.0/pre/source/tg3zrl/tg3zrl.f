      PROGRAM ZRLTG3
C
      INCLUDE "reshtgz.h"
C
      PARAMETER (LATOUT=JMAX,LONOUT=IMAX)              
      DIMENSION TG3(LONOUT,LATOUT),ZORL(LONOUT,LATOUT)
C
C     GENERATES TG1,TG2,TG3,ZORL INPUT FILE FOR THE AGCM MODEL
C
      OPEN(18,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(19,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(10,FORM='UNFORMATTED',STATUS='UNKNOWN')
C                                                                       
C     READ IN INPUT FIELD                                               
C                                                                       
      READ(18)TG3
      READ(19)ZORL
C                                                                       
C     WRITE OUTPUT FILE
C                                                                       
      WRITE(10)TG3,TG3,TG3,ZORL
C
      STOP                                                              
      END                                                               
