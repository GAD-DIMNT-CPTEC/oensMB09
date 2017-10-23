!
!  $Author: bonatti $
!  $Date: 2005/06/10 15:00:00 $
!  $Revision: 1.0 $
!
MODULE Units

  USE InputParameters, ONLY : i4

  IMPLICIT NONE

  PRIVATE

  INTEGER (KIND=i4), PARAMETER :: nUnits=260
  INTEGER (KIND=i4), PARAMETER :: nUmx=nUnits-1

  CHARACTER (LEN=16), PUBLIC :: aunits(-1:nUmx)

  PUBLIC :: SetUnits


CONTAINS


  SUBROUTINE SetUnits ()

    IMPLICIT NONE

    aunits( -1: -1) = 'Unknown         '
    aunits(  0:  9) = (/ &
      'No Dim          ','%               ','Gm/Kg           ','Ppm             ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 10: 19) = (/ &
      'M               ','Cm              ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 20: 29) = (/ &
      'Kg              ','Gm              ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 30: 39) = (/ &
      'Sec             ','Days            ','Yrs             ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 40: 49) = (/ &
      'K               ','C               ','F               ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 50: 59) = (/ &
      '1/Sec           ','1/Day           ','Gm/Kg/Day       ','10**-5 1/Sec    ','10**-6 1/Sec    ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 60: 69) = (/ &
      'M/Sec           ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 70: 79) = (/ &
      'K/Sec           ','K/Day           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 80: 89) = (/ &
      'Sec**-2         ','1/Sec/Day       ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 90: 99) = (/ &
      'M**2/Sec        ','10**6 M**2/Sec  ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(100:109) = (/ &
      'M Sec**-2       ','M/Sec/Day       ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(110:119) = (/ &
      'Kg M**-2        ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(120:129) = (/ &
      'Kg M**-2 Sec**-1','Kg M**-2 Day**-1','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(130:139) = (/ &
      'Pa              ','Mb              ','Cb              ','Dynes Cm**-2    ','Mb-1000         ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(140:149) = (/ &
      'Ln(Pa)          ','Ln(Mb)          ','Ln(Cb)          ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(150:159) = (/ &
      'Pa/Sec          ','Mb/Sec          ','Mb/Day          ','Cb/Sec          ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(160:169) = (/ &
      'Log(Pa)/Sec     ','Log(Mb)/Sec     ','Log(Cb)/Sec     ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(170:179) = (/ &
      'W M**-2         ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(180:189) = (/ &
      'M**2 Sec**-2    ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(190:199) = (/ &
      'Sec/M           ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(200:209) = (/ &
      'Kg M**-3        ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(210:219) = (/ &
      'Kg M**-1 Sec**-1','Kg M**-1 Day**-1','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(220:229) = (/ &
      'Kg Sec**-1      ','10**9 Kg Sec**-1','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(230:239) = (/ &
      'K M Sec**-1     ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(240:249) = (/ &
      'K Pa Sec**-1    ','K Mb Sec**-1    ','K Cb Sec**-1    ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(250:259) = (/ &
      'M Pa Sec**-2    ','M Mb Sec**-2    ','M Cb Sec**-2    ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)

  END SUBROUTINE SetUnits


END MODULE Units
