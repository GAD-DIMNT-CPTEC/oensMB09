      PROGRAM INTSLM
C
      INCLUDE "reshslm.h"
C
      PARAMETER (LATIN=181,LONIN=360,LATG=JMAX,LONG=IMAX,               
     *LATINP=LATIN+1,LONINP=LONIN+1,
     *LONS=LONG+LONIN+2,LATS=LATG+LATIN+2)
      DIMENSION WTLON(LONS),WTLAT(LATS),MPLON(LONS,2),MPLAT(LATS,2),
     B     WORK(LONG,LATG),
     C     DRAG(LONG,LATG),SLMO(LONG,LATG),MSKIN(LONIN,LATIN),
     D     FIELDA(LONIN, LATIN),TOPOG(LONG,LATG),RFLAG(LONIN,LATIN)
      LOGICAL FLGIN(5),FLGOUT(5)
      INTEGER*2 MASK(LONG,LATG)                                       
      DOUBLE PRECISION DWORK(2*LONS)
      CHARACTER*7 MSKFMT/'(   I1)'/ 
C                                    
      REAL*4 SMCGRD(LONIN,LATIN),SMIGRD(LONG,LATG)
C                                                                       
C     SET UNDEFINED VALUE FOR INPUT DATA FOUND AT LOCATIONS WHICH       
C     ARE NOT TO BE INCLUDED IN INTERPOLATION AND WHICH WILL BE         
C     USE ON OUTPUT DATA IF NO VALID DATA LOCATIONS ARE FOUND WITHIN    
C     OUTPUT LOCATION                                                   
      UNDEF=-999.0                                                      
C
C                                                                        
C
      OPEN(10,FORM='FORMATTED',STATUS='UNKNOWN')
      OPEN(18,FORM='FORMATTED',STATUS='UNKNOWN')
      OPEN(19,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(20,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(21,FORM='UNFORMATTED',STATUS='UNKNOWN')
C                                                                        
      FLGIN(1)=.TRUE.
      FLGIN(2)=.FALSE.
      FLGIN(3)=.TRUE.
      FLGIN(4)=.TRUE.
      FLGIN(5)=.FALSE.
      FLGOUT(1)=.TRUE.
      FLGOUT(2)=.TRUE.
      FLGOUT(3)=.FALSE.
      FLGOUT(4)=.TRUE.
      FLGOUT(5)=.TRUE.
C
      CALL WTERP(LONIN,LATIN,LONG,LATG,FLGIN,FLGOUT,WTLON,WTLAT,
     *MPLON,MPLAT,LOND,LATD,DWORK)
C                                                                       
C    READ IN OUTPUT MASK IF NEEDED                                      
C                                                                       
      WRITE(MSKFMT(2:4),50)LONG                                       
   50 FORMAT(I3)                                                        
      READ(10,MSKFMT)MASK                                               
C                                                                       
C     READ IN INPUT FIELD                                               
C                                                                       
      DO 60 J=1,LATIN
      DO 55 I=1,LONIN
      MSKIN(I,J)=1
   55 CONTINUE
   60 CONTINUE
      DO 100 M=1,12                                                     
      WRITE(6,20)M                                                      
   20 FORMAT(' M=',I2)                                                  
      READ(18,'(5E15.8)')FIELDA                                                     
C          
      LN=LONIN/2                                                             
      DO J=1,LATIN
      DO I=1,LN
      SMCGRD(I,J)=FIELDA(LN+I,J)
      SMCGRD(LN+I,J)=FIELDA(I,J)
      ENDDO
      ENDDO
      WRITE(21) SMCGRD
C                                                                       
C     INTERPOLATE INPUT FIELD TO PUTPUT FIELD                           
C                                                                       
      CALL NTERP(FIELDA,LONIN,LATIN,SLMO,LONG,LATG,WTLON,WTLAT,
     *MPLON,MPLAT,LOND,LATD,MSKIN,UNDEF,WORK)
C                                                                       
C     PERFORM OUTPUT DATA ADJUSTMENT IF NECESSARY (USING MASK AS NEEDED)
C                                                                       
      DO 40 J=1,LATG                                                  
      DO 30 I=1,LONG                                                  
      IF(MASK(I,J).EQ.0)SLMO(I,J)=150.0                               
   30 CONTINUE                                                          
   40 CONTINUE                                                          
C                                                                       
C                                                                       
C     WRITE OUT ADJUSTED INTERPOLATED FIELD                             
C                                                                       
      WRITE(19)SLMO
C
C     WRITE GRADS FIELDS
C                                                    
      DO J=1,LATG
      DO I=1,LONG
      SMIGRD(I,J)=SLMO(I,J)
      ENDDO
      ENDDO
      WRITE(20) SMIGRD
  100 CONTINUE                                                          
      STOP                                                              
      END                                                               
