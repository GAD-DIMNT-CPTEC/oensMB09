!
!  $Author: bonatti $
!  $Date: 2005/06/10 15:00:00 $
!  $Revision: 1.0 $
!
MODULE Units

  USE InputParameters, ONLY : i4

  IMPLICIT NONE

  PRIVATE

  INTEGER (KIND=i4), PARAMETER :: nUnits=260_i4
  INTEGER (KIND=i4), PARAMETER :: nUmx=nUnits-1_i4

  CHARACTER (LEN=16_i4), PUBLIC :: aunits(-1_i4:nUmx)

  PUBLIC :: SetUnits


CONTAINS


  SUBROUTINE SetUnits ()

    IMPLICIT NONE

    aunits( -1_i4: -1_i4) = 'Unknown         '
    aunits(  0_i4:  9_i4) = (/ &
      'No Dim          ','%               ','Gm/Kg           ','Ppm             ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 10_i4: 19_i4) = (/ &
      'M               ','Cm              ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 20_i4: 29_i4) = (/ &
      'Kg              ','Gm              ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 30_i4: 39_i4) = (/ &
      'Sec             ','Days            ','Yrs             ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 40_i4: 49_i4) = (/ &
      'K               ','C               ','F               ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 50_i4: 59_i4) = (/ &
      '1/Sec           ','1/Day           ','Gm/Kg/Day       ','10**-5 1/Sec    ','10**-6 1/Sec    ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 60_i4: 69_i4) = (/ &
      'M/Sec           ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 70_i4: 79_i4) = (/ &
      'K/Sec           ','K/Day           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 80_i4: 89_i4) = (/ &
      'Sec**-2         ','1/Sec/Day       ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits( 90_i4: 99_i4) = (/ &
      'M**2/Sec        ','10**6 M**2/Sec  ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(100_i4:109_i4) = (/ &
      'M Sec**-2       ','M/Sec/Day       ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(110_i4:119_i4) = (/ &
      'Kg M**-2        ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(120_i4:129_i4) = (/ &
      'Kg M**-2 Sec**-1','Kg M**-2 Day**-1','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(130_i4:139_i4) = (/ &
      'Pa              ','Mb              ','Cb              ','Dynes Cm**-2    ','Mb-1000         ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(140_i4:149_i4) = (/ &
      'Ln(Pa)          ','Ln(Mb)          ','Ln(Cb)          ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(150_i4:159_i4) = (/ &
      'Pa/Sec          ','Mb/Sec          ','Mb/Day          ','Cb/Sec          ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(160_i4:169_i4) = (/ &
      'Log(Pa)/Sec     ','Log(Mb)/Sec     ','Log(Cb)/Sec     ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(170_i4:179_i4) = (/ &
      'W M**-2         ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(180_i4:189_i4) = (/ &
      'M**2 Sec**-2    ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(190_i4:199_i4) = (/ &
      'Sec/M           ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(200_i4:209_i4) = (/ &
      'Kg M**-3        ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(210_i4:219_i4) = (/ &
      'Kg M**-1 Sec**-1','Kg M**-1 Day**-1','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(220_i4:229_i4) = (/ &
      'Kg Sec**-1      ','10**9 Kg Sec**-1','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(230_i4:239_i4) = (/ &
      'K M Sec**-1     ','Unset           ','Unset           ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(240_i4:249_i4) = (/ &
      'K Pa Sec**-1    ','K Mb Sec**-1    ','K Cb Sec**-1    ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)
    aunits(250_i4:259_i4) = (/ &
      'M Pa Sec**-2    ','M Mb Sec**-2    ','M Cb Sec**-2    ','Unset           ','Unset           ', &
      'Unset           ','Unset           ','Unset           ','Unset           ','Unset           ' /)

  END SUBROUTINE SetUnits


END MODULE Units
