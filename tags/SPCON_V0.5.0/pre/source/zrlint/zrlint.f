      PROGRAM INTZRL
C
      INCLUDE "reshtgz.h"
C
      PARAMETER (LATIN=73,LONIN=145,LATOUT=JMAX,LONOUT=IMAX,
     *           LONS=LONOUT+LONIN+2,LATS=LATOUT+LATIN+2)
      DIMENSION FIELDA(LONIN,LATIN),FIELDB(LONOUT,LATOUT),
     *          WTLON(LONS),WTLAT(LATS),WORK(LONOUT,LATOUT)
      DIMENSION MPLON(LONS,2),MPLAT(LATS,2),MSKIN(LONIN,LATIN)
      LOGICAL FLGIN(5),FLGOUT(5)
      DOUBLE PRECISION DWORK(2*LONS)
CJPB  PARAMETER (IWD=2*MAX(LONS,LATS))
CJPB  REAL*8 DWORK(IWD)
C
      REAL*4 RLIGRD(LONOUT,LATOUT),RLCGRD(LONIN,LATIN)
C
C    GENERAL PURPOSE HORIZONTAL INTERPOLATOR                            
C    CAN INTERPOLATE REGULAR TO REGULAR (COARSE OR FINE)                
C                    REGULAR TO GAUSSIAN                                
C                    GAUSSIAN TO REGULAR                                
C                    GAUSSIAN TO GAUSSIAN                               
C    WILL REORIENT REGULAR INPUT DATA AS NEEDED AND PUT DATA OUT ON     
C    GRID ORIENTED WITH THE NORTH POLE AND GREENWICH AS THE FIRST POINT 
C    DATA CAN BE SUBSEQUENTLY MASKED FOR LAND-SEA DEPENDENCE OR         
C    OTHER RELATIONSHIPS                                                
C                                                                       
C
C     SET UNDEFINED VALUE FOR INPUT DATA FOUND AT LOCATIONS WHICH
C     ARE NOT TO BE INCLUDED IN INTERPOLATION AND WHICH WILL BE
C     USE ON OUTPUT DATA IF NO VALID DATA LOCATIONS ARE FOUND WITHIN
C     OUTPUT LOCATION
      UNDEF=-999.0
      DO J=1,LATIN
      DO I=1,LONIN
      MSKIN(I,J)=1
      ENDDO
      ENDDO
C
C     FLAGS: (IN OR OUT)
C     1     START AT NORTH POLE (TRUE) START AT SOUTH POLE (FALSE)
C     2     START AT PRIME MERIDIAN (TRUE) START AT I.D.L. (FALSE)
C     3     LATITUDES ARE AT CENTER OF BOX (TRUE)
C           LATITUDES ARE AT EDGE (FALSE) NORTH EDGE IF 1=TRUE
C                                         SOUTH EDGE IF 1=FALSE 
C     4     LONGITUDES ARE AT CENTER OF BOX (TRUE)
C           LONGITUDES ARE AT WESTERN EDGE OF BOX (FALSE) 
C     5     GAUSSIAN (TRUE) REGULAR (FALSE)
C                                                                       
      FLGIN(1)=.TRUE.
      FLGIN(2)=.TRUE.
      FLGIN(3)=.TRUE.
      FLGIN(4)=.TRUE.
      FLGIN(5)=.FALSE.
      FLGOUT(1)=.TRUE.
      FLGOUT(2)=.TRUE.
      FLGOUT(3)=.FALSE.
      FLGOUT(4)=.TRUE.
      FLGOUT(5)=.TRUE.
C                                                                       
C
C     CALL WTERP ONCE TO SETUP WEIGHTING FACTORS AND INDICES FOR
C     GRID INTERPOLATION
C
      CALL WTERP(LONIN,LATIN,LONOUT,LATOUT,FLGIN,FLGOUT,WTLON,WTLAT,
     *           MPLON,MPLAT,LOND,LATD,DWORK)
C                                                                       
      OPEN(18,FORM='FORMATTED',STATUS='UNKNOWN')
      OPEN(19,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(20,FORM='UNFORMATTED',STATUS='UNKNOWN')
      OPEN(21,FORM='UNFORMATTED',STATUS='UNKNOWN')
C                                                                       
C     READ IN INPUT FIELD                                               
C                                                                       
      READ(18,'(5E15.8)')FIELDA                                                     
C  
      DO J=1,LATIN
      DO I=1,LONIN
      RLCGRD(I,J)=FIELDA(I,J)
      ENDDO
      ENDDO
      WRITE(20) RLCGRD
C                                                                       
C                                                                       
C     INTERPOLATE INPUT FIELD TO OUTPUT FIELD                           
C                                                                       
      CALL NTERP(FIELDA,LONIN,LATIN,FIELDB,LONOUT,LATOUT,WTLON,WTLAT,
     *           MPLON,MPLAT,LOND,LATD,MSKIN,UNDEF,WORK)
C                                                                       
C     WRITE OUT ADJUSTED INTERPOLATED FIELD                             
C                                                                       
      WRITE(19)FIELDB
C                                                    
      DO J=1,LATOUT
      DO I=1,LONOUT
      RLIGRD(I,J)=FIELDB(I,J)
      ENDDO
      ENDDO
      WRITE(21) RLIGRD
C
      STOP                                                              
      END                                                               
