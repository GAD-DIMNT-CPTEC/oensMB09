C>    @file
C>    @brief Reads and unpacks one report into the unpacked office note
C>    29/124 format
C>    @author Dennis Keyser @date 2013-03-20

C>    This routine has not been tested reading input data from any dump
C>    type in ON29/124 format on WCOSS. It likely will not work when
C>    attempting to read ON29/124 format dumps on WCOSS. It has also
C>    not been tested reading any dump file other than ADPUPA (BUFR
C>    input only) on WCOSS. It does work reading BUFR ADPUPA dump files
C>    on WCOSS. It will hopefully working reading other BUFR (only)
C>    dump files on WCOSS. Also, this routine is only known to work correctly
C>    when compiled using 8 byte machine words (real and integer).
C>
C>    Reads and unpacks one report into the unpacked office note
C>    29/124 format.  The input data may be packed into either bufr or
C>    true on29/124 format with a y2k compliant pseudo-on85 header label.
C>    (Note: as a temporary measure, this code will still operate on a
C>    true on29/124 format file with a non-y2k compliant on85 header
C>    label.  The code will use the "windowing" technique to obtain a
C>    4-digit year.) This routine will determine the format of the
C>    input data and take the appropriate action. It returns the
C>    unpacked report to the calling program in the array 'obs'.
C>    Various contingencies are covered by return value of the function
C>    and parameter 'ier' - function and ier have same value. Repeated
C>    calls of function will return a sequence of unpacked on29/124
C>    reports.  The calling program may switch to a new 'nunit' at any
C>    time, that dataset will then be read in sequence. If user
C>    switches back to a previous 'nunit', that data set will be read
C>    from the beginning, not from where the user left off (this is a
C>    'software tool', not an entire i/o system).
C>
C>    Program history log:
C>    - Jack Woollen 1996-12-13 (gsc) Note this new
C>    version of iw3gad incorporates the earlier version which
C>    was written by j. stackpole and dealt only with true
C>    on29/124 data as input - this option is still available
C>    but is a small part of the new routine which was written
C>    from scratch to read in bufr data.
C>    - Dennis Keyser 1997-01-27 Changes to more closely duplicate format
C>    obtained when reading from true on29/124 data sets.
C>    - Dennis Keyser 1997-02-04 Drops with missing stnid get stnid set to
C>    "drp88a"; satwnds with zero pressure are tossed.
C>    - Dennis Keyser 1997-02-12 To get around the 3-bit limitation to
C>    the on29 pressure q.m. mnemonic "qmpr", an sdmedit/quips
C>    purge or reject flag on pressure is changed from 12 or 14
C>    to 6 in order to fit into 3-bits, see function e35o29;
C>    interprets sdmedit and quips purge/keep/change flags
C>    properly for all data types; can now process cat. 6 and
C>    cat. 2/3 type flight-level reccos (before skipped these);
C>    tests for missing lat, lon, obtime decoded from bufr and
C>    retains missing value on these in unpacked on29/124
C>    format (before no missing check, led to possible non-
C>    missing but incorrect values for these); the check for
C>    drops with missing stnid removed since decoder fixed for
C>    this.
C>    - Dennis Keyser 1997-05-01 Looks for duplicate levels when
C>    processing on29 cat. 2, 3, and 4 (in all data on level)
C>    and removes duplicate level; in processing on29 cat. 3
C>    levels, removes all levels where wind is missing; fixed
C>    bug in aircraft (airep/pirep/amdar) quality mark
C>    assignment (was not assigning keep flag to report if
C>    pressure had a keep q.m. but temperature q.m. was
C>    missing).
C>    - Dennis Keyser 1997-05-30 For aircft: (only acars right now) -
C>    seconds are decoded (if avail.) and used to obtain
C>    report time; only asdar/amdar - new cat. 8 code figs.
C>    o-put 917 (char. 1 & 2 of actual stnid), 918 (char. 3 &
C>    4 of actual stnid), 919 (char. 5 & 6 of actual stnid);
C>    asdar/amdar and acars - new cat. 8 code fig. o-put 920
C>    (char. 7 & 8 of actual stnid); only acars - new cat. 8
C>    code fig. o-put 921 (report time to nearest 1000'th of
C>    an hour); only some acars - new mnemonic "ialt" now
C>    exists and can (if line not commented out) be used to
C>    obtain unpacked on29 cat. 6.
C>    - Dennis Keyser 1997-07-02 Removed filtering of aircraft data as
C>    follows: air france amdars no longer filtered, amdar/
C>    asdar below 7500 ft. no longer filtered, airep/pirep
C>    below 100 meters no longer filtered, all aircraft with
C>    missing wind but valid temperature are no longer
C>    filtered; reprocesses u.s. satwnd stn. ids to conform
C>    with previous on29 appearance except now 8-char (tag
C>    char. 1 & 6 not changed from bufr stn. id) - never any
C>    dupl. ids now for u.s. satwnds decoded from a single
C>    bufr file; streamlined/eliminated some do loops to
C>    speed up a bit.
C>    - Dennis Keyser 1997-09-18 Corrected errors in reformatting surface
C>    data into unpacked on124, specifically-header: inst. type
C>    (synoptic fmt flg, auto stn. type, converted hrly flg),
C>    indicators (precip., wind speed, wx/auto stn), cat51:
C>    p-tend, horiz. viz., present/past wx, cloud info, max/
C>    min temp, cat52: precip., snow dpth, wave info, ship
C>    course/speed, cat8: code figs. 81-85,98; corrected
C>    problem which coded upper-air mandatory level winds
C>    as cat. 3 instead of cat. 1 when mass data (only) was
C>    reported on same mandatory level in a separate reported
C>    level in the raw bulletin.
C>    - Dennis Keyser 1997-10-06 Updated logic to read and process nesdis
C>    hi-density satellite winds properly.
C>    - Dennis Keyser 1997-10-30 Added gross check on u-air pressure, all
C>    levels with reported pressure .le. zero now tossed; sfc
C>    cat. 52 sea-sfc temperature now read from hierarchy of
C>    sst in bufr {1st choice - hi-res sst ('sst2'), 2nd
C>    choice - lo-res sst ('sst1'), 3rd choice - sea temp
C>    ('stmp')}, before only read 'sst1'.
C>    - Dennis Keyser 1998-01-26 Changed pqm processing for adpupa types
C>    such that sdmedit flags are now honored (before, pqm
C>    was always hardwired to 2 for adpupa types); bumped
C>    limit for number of levels that can be processed from
C>    100 to 150 and added diagnostic print when the limit
C>    is exceeded.
C>    - Dennis Keyser 1998-05-19 Y2k compliant version of iw3gad routine
C>    accomplished by redefining original 32-character on85
C>    header label to be a 40-character label that contains a
C>    full 4-digit year, can still read "true" on29/124 data
C>    sets provided their header label is in this modified
C>    form.
C>    - Dennis Keyser 1998-07-22 Minor modifications to account for
C>    corrections in y2k/f90 bufrlib (mainly related to
C>    bufrlib routine dumpbf).
C>    - Dennis Keyser 1998-08-04 Fixed a bug that resulted in code being
C>    clobbered in certain situations for recco reports; minor
C>    modifications to give same answers on cray as on sgi;
C>    allowed code to read true on29/124 files with non-y2k
C>    compliant on85 label (a temporary measure during
C>    transition of main programs to y2k); added call to "aea"
C>    which converts ebcdic characters to ascii for input
C>    true on29/124 data set processing of sgi (which does
C>    not support "-cebcdic" in assign statement).
C>    - Dennis Keyser 1999-02-25 Added ability to read reprocessed ssm/i
C>    bufr data set (spssmi); added ability to read mean
C>    sea-level pressure bogus (paobs) data set (sfcbog).
C>    - Dennis Keyser 1999-05-14 Made changes necessary to port this
C>    routine to the ibm sp.
C>    - Dennis Keyser 1999-06-18 Can now process water vapor satwnds
C>    from foreign producers; stn. id for foreign satwnds
C>    now reprocessed in same way as for nesdis/goes satwnds,
C>    character 1 of stn. id now defines even vs. odd
C>    satellite while character 6 of stn. id now defines
C>    ir cloud-drft vs. visible cloud drft vs. water vapor.
C>    - Dennis Keyser 2002-03-05 Removed entry "e02o29", now performs
C>    height to press. conversion directly in code for cat. 7;
C>    test for missing "rpid" corrected for adpupa data (now
C>    checks ufbint return code rather than value=bmiss);
C>    accounts for changes in input adpupa, adpsfc, aircft
C>    and aircar bufr dump files after 3/2002: cat. 7 and cat.
C>    51 use mnemonic "hblcs" to get height of cloud base if
C>    mnemonic "hocb" not available (and it will not be for all
C>    cat. 7 and some cat. 51 reports); mnemonic "tiwm"
C>    replaces "suws" in header for surface data; mnemonic
C>    "borg" replaces "icli" in cat. 8 for aircraft data (will
C>    still work properly for input adpupa, adpsfc, aircft and
C>    aircar dump files prior to 3/2002).
C>    - Dennis Keyser 2013-03-20 Changes to run on wcoss, obtain value of
C>    bmiss set in calling program via call to bufrlib routine
C>    getbmiss rather than hardwiring it to 10e08 (or 10e10);
C>    use formatted print statements where previously
C>    unformatted print was used (wcoss splits unformatted
C>    print at 80 characters).
C>
C>    @param[in] lunit fortran unit number for sequential data set containing
C>    packed bufr reports or packed and blocked office note 29/124 reports
C>    @param[out] obs array containing one report in unpacked office note
C>    29/124 format. Format is mixed, user must equivalence
C>    integer and character arrays to this array (see
C>    docblock for w3fi64 in /nwprod/lib/sorc/w3nco
C>    or writeups on w3fi64, on29, on124 for help)
C>    the length of the array should be at least 1608.
C>    @param[out] ier return flag (equal to function value)
C>
C>    Input files:
C>    - unit aa sequential bufr or office note 29/124 data set ("aa"
C>    is unit number specified by input argument "nunit")
C>
C>    Output files:
C>    - unit 06 printout
C>
C>    @note
C>    - if input data set is on29/124, it should be assigned in this way:
C>     - cray:
C>      - assign -a adpupa -fcos -cebcdic fort.xx
C>     - sgi:
C>      - assign -a adpupa -fcos fort.xx
C>      (note: -cebcdic is not possible on sgi, so call to w3nco
C>      routine "aea" takes care of the conversion as each
C>      on29 record is read in)
C>    - if input data set is bufr, it should be assigned in this way:
C>     - cray:
C>      - assign -a adpupa fort.xx
C>     - sgi:
C>      - assign -a adpupa -f cos fort.xx
C>
C>    For input on29/124 data sets, a contingency has been built
C>    into this subroutine to perform the conversion from ebcdic to
C>    ascii in the event the assign does not perform the conversion
C>    the return flags in ier (and function iw3unp29 itself) are:
C>    - 0 Observation read and unpacked into location 'obs'.
C>    see writeup of w3fi64 for contents. (all character
C>    words are left-justified.) Next call to iw3unp29
C>    will return next observation in data set.
C>    - 1 A 40 byte header in the format described here
C>    (y2k compliant pseudo-office note 85) is returned
C>    in the first 10 words of 'obs' on a 4-byte machine
C>    (ibm) and in the first 5 words of 'obs' on an
C>    8-byte machine (cray). Next call to
C>    iw3unp29 will return first obs. in this data set.
C>    (note: if input data set is a true on29/124 file
C>    with the y2k compliant pseudo-on85 header record,
C>    then the pseudo-on85 header record is actually
C>    read in and returned; if input data set is a true
C>    on29/124 file with a non-y2k compliant on85 header
C>    record, then a y2k compliant pseudo-on85 header
C>    record is constructed from it using the "windowing"
C>    technique to obtain a 4-digit year from a 2-digit
C>    year.)
C>    format for y2k compliant pseudo-on85 header record
C>    returned (40 bytes in character):
C>     - bytes 1- 8 -- data set name (as defined in on85 except up to
C>       eight ascii char., left justified with blank fill)
C>     - bytes 9-10 -- set type (as defined in on85)
C>     - bytes 11-20 -- center (analysis) date for data
C>       set (ten ascii characters in form "yyyymmddhh")
C>     - bytes 21-24 -- set initialize (dump) time, as dedined in on85)
C>     - bytes 25-34 -- always "washington" (as in on85)
C>     - bytes 35-36 -- source machine (as defined in on85)
C>     - bytes 37-40 -- blank fill characters
C>    - 2 end-of-file (never an empty or null file):
C>     - input on29/124 data set: the "endof file" record is
C>     encountered - no useful information in 'obs' array.
C>     next call to iw3unp29 will return physical end of
C>     file for data set in 'nunit' (see ier=3 below).
C>     - input bufr data set: the physical end of file is
C>     encountered.
C>    -3 end-of-file:
C>    Physical end of file encountered on data set -
C>    this can only happen for an empty (null) data set
C>    or for a true on29/124 data set. There are no
C>    more reports (or never were any if null) associated
C>    with data set in this unit number - no useful
C>    information in 'obs' array. Either all done (if
C>    no more unit numbers are to be read in), or reset
C>    'nunit' to point to a new data set (in which case
C>    next call to iw3unp29 should return with ier=1).
C>    - 4 only valid for input on29/124 data set - i/o error
C>    reading the next record of reports - no useful
C>    information in 'obs' array. Calling program can
C>    choose to stop or again call iw3unp29 which will
C>    attempt to unpack the first observation in the next
C>    record of reports.
C>    - 999 applies only to non-empty data sets:
C>     - input on29/124 data set: first choice y2k compliant
C>     pseudo-on85 file header label not encountered where
C>     expected, and second choice non-y2k compliant on85
C>     file header label also not encountered.
C>     - input bufr data set either header label in
C>     format of pseudo-on85 could not be returned, or an
C>     abnormal error occurred in the attempt to decode an
C>     observation. For either input data set type, no
C>     useful information in 'obs' array. Calling program
C>     can choose to stop with non-zero condition code or
C>     reset 'nunit' to point to a new data set (in which
C>     case next call to iw3unp29 should return with
C>     ier=1).
C>     - input data set neither on29/124 nor bufr speaks for
C>     itself.
C>
C>    @author Dennis Keyser @date 2013-03-20
C>

      FUNCTION IW3UNP29(LUNIT,OBS,IER)

      COMMON/IO29AA/JWFILE(100),LASTF
      COMMON/IO29BB/KNDX,KSKACF(8),KSKUPA,KSKSFC,KSKSAT,KSKSMI
      COMMON/IO29CC/SUBSET,IDAT10
      COMMON/IO29DD/HDR(12),RCATS(50,150,11),IKAT(11),MCAT(11),NCAT(11)
      COMMON/IO29EE/ROBS(255,11)
      COMMON/IO29FF/QMS(255,9)
      COMMON/IO29GG/SFO(34)
      COMMON/IO29HH/SFQ(5)
      COMMON/IO29II/PWMIN
      COMMON/IO29JJ/ISET,MANLIN(1001)
      COMMON/IO29KK/KOUNT(499,18)
      COMMON/IO29LL/BMISS

      DIMENSION    OBS(*)
      REAL(8)      BMISS,GETBMISS

      SAVE

      DATA ITIMES/0/

      IF(ITIMES.EQ.0)  THEN

C  THE FIRST TIME IN, INITIALIZE SOME DATA
C  (NOTE: FORTRAN 77/90 STANDARD DOES NOT ALLOW COMMON BLOCK VARIABLES
C         TO BE INITIALIZED VIA DATA STATEMENTS, AND, FOR SOME REASON,
C         THE BLOCK DATA DOES NOT INITIALIZE DATA IN THE W3NCO LIBRARY
C         AVOID BLOCK DATA IN W3NCO/W3EMC)
C  --------------------------------------------------------------------

         ITIMES = 1
         JWFILE = 0
         LASTF = 0
         KNDX = 0
         KSKACF = 0
         KSKUPA = 0
         KSKSFC = 0
         KSKSAT = 0
         KSKSMI = 0
         KOUNT = 0
         IKAT(1)  =  1
         IKAT(2)  =  2
         IKAT(3)  =  3
         IKAT(4)  =  4
         IKAT(5)  =  5
         IKAT(6)  =  6
         IKAT(7)  =  7
         IKAT(8)  =  8
         IKAT(9)  = 51
         IKAT(10) = 52
         IKAT(11) =  9
         MCAT(1)  =  6
         MCAT(2)  =  4
         MCAT(3)  =  4
         MCAT(4)  =  4
         MCAT(5)  =  6
         MCAT(6)  =  6
         MCAT(7)  =  3
         MCAT(8)  =  3
         MCAT(9)  = 21
         MCAT(10) = 15
         MCAT(11) =  3
         ISET  = 0
      END IF

C  UNIT NUMBER OUT OF RANGE RETURNS A 999
C  --------------------------------------

      IF(LUNIT.LT.1 .OR. LUNIT.GT.100)  THEN
         PRINT'(" ##IW3UNP29 - UNIT NUMBER ",I0," OUT OF RANGE -- ",
     $    "IER = 999")', LUNIT
         GO TO 9999
      END IF
      IF(LASTF.NE.LUNIT .AND. LASTF.GT.0) THEN
         CALL CLOSBF(LASTF)
         JWFILE(LASTF) = 0
      END IF
      LASTF = LUNIT

C  THE JWFILE INDICATOR: =0 IF UNOPENED; =1 IF ON29; =2 IF BUFR
C  ------------------------------------------------------------

      IF(JWFILE(LUNIT).EQ.0) THEN
         PRINT'(" ===> IW3UNP29 - WCOSS VERSION: 03-20-2013")'

         BMISS = GETBMISS()
         print'(1X)'
         print'(" BUFRLIB value for missing passed into IW3UNP29 is: ",
     $    G0)', bmiss
         print'(1X)'

         IF(I03O29(LUNIT,OBS,IER).EQ.1) THEN
            PRINT'(" IW3UNP29 - OPENED A TRUE OFFICE NOTE 29 FILE IN ",
     $       "UNIT ",I0)', LUNIT
            JWFILE(LUNIT) = 1
            IER = 1
            IW3UNP29 = 1
         ELSEIF(I03O29(LUNIT,OBS,IER).EQ.3) THEN
            PRINT 107, LUNIT
  107 FORMAT(/,' ##IW3UNP29 - FILE IN UNIT',I3,' IS EMPTY OR NULL -- ',
     $ 'IER = 3'/)
            IER = 3
            IW3UNP29 = 3
         ELSEIF(I02O29(LUNIT,OBS,IER).EQ.1) THEN
            PRINT'(" IW3UNP29 - OPENED A BUFR FILE IN UNIT ",I0)', LUNIT

            JWFILE(LUNIT) = 2
            KNDX = 0
            KSKACF = 0
            KSKUPA = 0
            KSKSFC = 0
            KSKSAT = 0
            KSKSMI = 0
            IER = 1
            IW3UNP29 = 1
         ELSEIF(I03O29(LUNIT,OBS,IER).EQ.999) THEN
            PRINT'(" IW3UNP29 - OPENED A TRUE OFFICE NOTE 29 FILE IN ",
     $       "UNIT ",I0)', LUNIT
            PRINT 88
   88 FORMAT(/' ##IW3UNP29/I03O29 - NEITHER EXPECTED Y2K COMPLIANT ',
     $ 'PSEUDO-ON85 LABEL NOR SECOND CHOICE NON-Y2K COMPLIANT ON85 ',
     $ 'LABEL FOUND IN'/21X,'FIRST RECORD OF FILE  -- IER = 999'/)
            GO TO 9999
         ELSE
            PRINT 108, LUNIT
  108 FORMAT(/,' ##IW3UNP29 - FILE IN UNIT',I3,' IS NEITHER BUFR NOR ',
     $ 'TRUE OFFICE NOTE 29 -- IER = 999'/)
            GO TO 9999
         END IF
      ELSEIF(JWFILE(LUNIT).EQ.1) THEN
         IF(I03O29(LUNIT,OBS,IER).NE.0) JWFILE(LUNIT) = 0
         IF(IER.GT.0) CLOSE (LUNIT)
         IW3UNP29 = IER
      ELSEIF(JWFILE(LUNIT).EQ.2) THEN
         IF(I02O29(LUNIT,OBS,IER).NE.0) JWFILE(LUNIT) = 0
         IF(IER.GT.0) CALL CLOSBF(LUNIT)
         IF(IER.EQ.2.OR.IER.EQ.3)  THEN
            IF(KSKACF(1).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT/",
     $      "AIRCAR REPORTS TOSSED DUE TO ZERO CAT. 6 LVLS = ",I0)',
     $      KSKACF(1)
            IF(KSKACF(2).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT ",
     $       "REPORTS TOSSED DUE TO BEING ""LFPW"" AMDAR = ",I0)',
     $       KSKACF(2)
            IF(KSKACF(8).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT ",
     $       "REPORTS TOSSED DUE TO BEING ""PHWR"" AIREP = ",I0)',
     $       KSKACF(8)
            IF(KSKACF(3).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT ",
     $       "REPORTS TOSSED DUE TO BEING CARSWELL AMDAR = ",I0)',
     $       KSKACF(3)
            IF(KSKACF(4).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT ",
     $       "REPORTS TOSSED DUE TO BEING CARSWELL ACARS = ",I0)',
     $       KSKACF(4)
            IF(KSKACF(5).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT/",
     $       "AIRCAR REPORTS TOSSED DUE TO HAVING MISSING WIND = ",I0)',
     $       KSKACF(5)
            IF(KSKACF(6).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT ",
     $       "REPORTS TOSSED DUE TO BEING AMDAR < 2286 M = ",I0)',
     $       KSKACF(6)
            IF(KSKACF(7).GT.0)  PRINT'(" IW3UNP29 - NO. OF AIRCFT ",
     $       "REPORTS TOSSED DUE TO BEING AIREP <  100 M = ",I0)',
     $       KSKACF(7)
            IF(KSKACF(1)+KSKACF(2)+KSKACF(3)+KSKACF(4)+KSKACF(5)+
     $       KSKACF(6)+KSKACF(7)+KSKACF(8).GT.0)
     $       PRINT'(" IW3UNP29 - TOTAL NO. OF AIRCFT/AIRCAR REPORTS ",
     $        "TOSSED = ",I0)',
     $        KSKACF(1)+KSKACF(2)+KSKACF(3)+KSKACF(4)+
     $        KSKACF(5)+KSKACF(6)+KSKACF(7)+KSKACF(8)
            IF(KSKUPA.GT.0)  PRINT'(" IW3UNP29 - TOTAL NO. OF ADPUPA ",
     $       "REPORTS TOSSED = ",I0)', KSKUPA
            IF(KSKSFC.GT.0)  PRINT'(" IW3UNP29 - TOTAL NO. OF ADPSFC/",
     $       "SFCSHP/SFCBOG REPORTS TOSSED = ",I0)', KSKSFC
            IF(KSKSAT.GT.0)  PRINT'(" IW3UNP29 - TOTAL NO. OF SATWND ",
     $       "REPORTS TOSSED = ",I0)', KSKSAT
            IF(KSKSMI.GT.0)  PRINT'(" IW3UNP29 - TOTAL NO. OF SPSSMI ",
     $       "REPORTS TOSSED = ",I0)', KSKSMI
            KNDX = 0
            KSKACF = 0
            KSKUPA = 0
            KSKSFC = 0
            KSKSAT = 0
            KSKSMI = 0
         END IF
         IW3UNP29 = IER
      END IF

      RETURN

 9999 CONTINUE
      IER = 999
      IW3UNP29 = 999
      RETURN

      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
C>    This function read obs files and returns error message.
C>    @param LUNIT full path of file
C>    @param HDR header of file
C>    @param IER  missing or invalid data indicator
C>    @return Y2K COMPLIANT
C>
C>    @author Dennis Keyser @date 2013-03-20
C>
C-----------------------------------------------------------------------
      FUNCTION I01O29(LUNIT,HDR,IER)
C     ---> formerly FUNCTION IW3HDR

      COMMON/IO29AA/JWFILE(100),LASTF

      DIMENSION HDR(*)

      SAVE

C  UNIT NUMBER OUT OF RANGE RETURNS A 999
C  --------------------------------------

      IF(LUNIT.LT.1 .OR. LUNIT.GT.100)  THEN
         PRINT'(" ##IW3UNP29/I01O29 - UNIT NUMBER ",I0," OUT OF RANGE ",
     $    "-- IER = 999")', LUNIT
         GO TO 9999
      END IF

C  THE JWFILE INDICATOR: =0 IF UNOPENED; =1 IF ON29; =2 IF BUFR
C  ------------------------------------------------------------

      IF(JWFILE(LUNIT).EQ.0) THEN
         IF(I03O29(LUNIT,HDR,IER).EQ.1) THEN
            I01O29 = I03O29(0,HDR,IER)
            I01O29 = 1
            RETURN
         ELSEIF(I02O29(LUNIT,HDR,IER).EQ.1) THEN
            CALL CLOSBF(LUNIT)
            I01O29 = 1
            RETURN
         ELSE

C  CAN'T READ FILE HEADER RETURNS A 999
C  ------------------------------------

            PRINT'(" ##IW3UNP29/I01O29 - CAN""T READ FILE HEADER -- ",
     $       "IER = 999")'
            GO TO 9999
         END IF
      ELSE

C  FILE ALREADY OPEN RETURNS A 999
C  -------------------------------

         PRINT'(" ##IW3UNP29/I01O29 - FILE ALREADY OPEN -- IER = 999")'
         GO TO 9999
      END IF

      RETURN

 9999 CONTINUE
      IER = 999
      I01O29 = 999
      RETURN

      END
C***********************************************************************
C***********************************************************************
C***********************************************************************

C>    This function read obs files and returns error message.
C>    @param LUNIT full path of file
C>    @param OBS data output
C>    @param IER  missing or invalid data indicator
C>    @return Y2K COMPLIANT
C>
C>    @author Dennis Keyser @date 2013-03-20
C>

      FUNCTION I02O29(LUNIT,OBS,IER)
C     ---> formerly FUNCTION JW3O29

      COMMON/IO29CC/SUBSET,IDAT10

      CHARACTER*40 ON85
      CHARACTER*10 CDATE
      CHARACTER*8  SUBSET,CBUFR
      CHARACTER*6  C01O29
      CHARACTER*4  CDUMP
      DIMENSION    OBS(1608),RON85(16),JDATE(5),JDUMP(5)
      EQUIVALENCE  (RON85(1),ON85)

      SAVE

      DATA ON85/'                                        '/

      JDATE = -1
      JDUMP = -1

C  IF FILE IS CLOSED TRY TO OPEN IT AND RETURN A Y2K COMPLIANT
C    PSEUDO-ON85 LABEL
C  -----------------------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)

      IF(IL.EQ.0) THEN
         IRET = -1
         I02O29 = 2
         REWIND LUNIT
         READ(LUNIT,END=10,ERR=10,FMT='(A8)') CBUFR
         IF(CBUFR(1:4).EQ.'BUFR') THEN
            PRINT'(" IW3UNP29/I02O29 - INPUT FILE ON UNIT ",I0, " IS",
     $      " UNBLOCKED NCEP BUFR"/)', LUNIT
         ELSE  IF(CBUFR(5:8).EQ.'BUFR') THEN
            PRINT'(" IW3UNP29/I02O29 - INPUT FILE ON UNIT ",I0, " IS",
     $      " BLOCKED NCEP BUFR"/)', LUNIT
         ELSE
            REWIND LUNIT
            GO TO 10
         END IF
         call datelen(10)
         CALL DUMPBF(LUNIT,JDATE,JDUMP)
cppppp
         print'(" CENTER DATE (JDATE) = ",I4,4I3.2/" DUMP DATE (JDUMP)",
     $    " (year not used anywhere) = "I4,4I3.2)',jdate,jdump
cppppp
         IF(JDATE(1).GT.999)  THEN
            WRITE(CDATE,'(I4.4,3I2.2)') (JDATE(I),I=1,4)
         ELSE  IF(JDATE(1).GT.0)  THEN

C If 2-digit year returned in JDATE(1), must use "windowing" technique
C  2 create a 4-digit year

            PRINT'(" ##IW3UNP29/I02O29 - 2-DIGIT YEAR IN JDATE(1) ",
     $       "RETURNED FROM DUMPBF (JDATE IS: ",I4.4,3I2.2,") - USE ",
     $       "WINDOWING TECHNIQUE TO OBTAIN 4-DIGIT YEAR")', JDATE
            IF(JDATE(1).GT.20)  THEN
               WRITE(CDATE,'("19",4I2.2)') (JDATE(I),I=1,4)
            ELSE
               WRITE(CDATE,'("20",4I2.2)') (JDATE(I),I=1,4)
            ENDIF
            PRINT'(" ##IW3UNP29/I02O29 - CORRECTED JDATE(1) WITH ",
     $       "4-DIGIT YEAR, JDATE NOW IS: ",I4.4,3I2.2)', JDATE
         ELSE
            GO TO 10
         ENDIF

         CALL OPENBF(LUNIT,'IN',LUNIT)

C This next call, I believe, is needed only because SUBSET is not
C  returned in DUMPBF ...
         call readmg(lunit,subset,idat10,iret)

         WRITE(CDUMP,'(2I2.2)') JDUMP(4),100*JDUMP(5)/60
         IF(JDUMP(1).LT.0) CDUMP = '9999'
         ON85=C01O29(SUBSET)//'  C2'//CDATE//CDUMP//'WASHINGTONCR    '
         OBS(1:16) = RON85
         I02O29 = 1
   10    CONTINUE
         IER = I02O29
         RETURN
      END IF

C  IF THE FILE IS ALREADY OPENED FOR INPUT TRY TO READ THE NEXT SUBSET
C  -------------------------------------------------------------------

      IF(IL.LT.0) THEN
 7822    CONTINUE
         CALL READNS(LUNIT,SUBSET,IDAT10,IRET)
         IF(IRET.EQ.0) I02O29 = R01O29(SUBSET,LUNIT,OBS)
         IF(IRET.NE.0) I02O29 = 2
         IF(I02O29.EQ.-9999)  GO TO 7822
         IER = I02O29
         RETURN
      END IF

C  FILE MUST BE OPEN FOR INPUT!
C  ----------------------------

      PRINT'(" ##IW3UNP29/I02O29 - FILE ON UNIT ",I0," IS OPENED FOR ",
     $ "OUTPUT -- IER = 999")', LUNIT
      I02O29 = 999
      IER = 999
      RETURN

      END

C>    This function reads a true (see *) on29/124 data set and unpacks one
C>    report into the unpacked office note 29/124 format. the input and
C>    output arguments here have the same meaning as for iw3unp29.
C>    repeated calls of function will return a sequence of unpacked
C>    on29/124 reports. * - unlike original "true" on29/124 data sets,
C>    the "expected" file header label is a y2k compliant 40-byte
C>    pseudo-on85 version - if this is not encountered this code, as a
C>    temporary measure during the y2k transition period, will look for
C>    the original non-y2k compliant 32-byte on85 header label and use
C>    the "windowing" technique to convert the 2-digit year to a 4-digit
C>    year in preparation for returning a 40-byte pseudo-on85 label in
C>    the first C call. (see iw3unp29 docblock for format of 40-byte
C>    pseudo-on85 header label.)
C>
C>    Program History Log:
C>    -1991-07-23 Dennis Keyser w3fi64 (f77) internal read error
C>    no longer causes calling program to fail but will move
C>    to next record if can't recover to next report
C>    -1993-10-07 Dennis Keyser -- adapted for use on cray (added save
C>    statement, removed ibm-specific code, etc.)
C>    -1993-10-15 R. E. Jones added code so if file is ebcdic it converts
C>    it to ascii
C>    -1996-10-04 Jack Woollen changed name to i03gad and incorporated
C>    into new w3lib routine iw3gad
C>    -2013-03-20 Dennis Keyser changes to run on wcoss
C>
C>    @param[in] nunit fortran unit number for sequential data set containing
C>    packed and blocked office note 29/124 reports
C>    @param[out] obs array containing one report in unpacked office note
C>    - 29/124 format is mixed, user must equivalence
C>    - integer and character arrays to this array (see
C>    - docblock for w3fi64 in /nwprod/lib/sorc/w3nco
C>    - or writeups on w3fi64, on29, on124 for help)
C>    - the length of the array should be at least 1608
C>    @param[out] ier return flag (equal to function value) in iw3unp29 docblock
C>    @return Y2K COMPLIANT
C>
C>    @note aa unit number specified by input argument "nunit")
C>    called by subprogram iw3unp29.
C>
C>    @author keyser @date 2013-03-20
C>
      FUNCTION I03O29(NUNIT, OBS, IER)
C     ---> formerly FUNCTION KW3O29

      CHARACTER*1  CBUFF(6432),CON85L(32)
      CHARACTER*2  CBF910
      CHARACTER*4  CYR4D
      CHARACTER*8  CBUFR
      INTEGER  IBUFF(5),OBS(*)

      EQUIVALENCE (IBUFF,CBUFF)

      SAVE

      DATA IOLDUN/0/

C TEST FOR NEW (OR PREVIOUSLY USED) NUNIT AND ADJUST 'NEXT'
C  (THIS ALLOWS USER TO SWITCH TO NEW NUNIT PRIOR TO READING TO
C  THE 'END OF FILE' ON AN OLD UNIT.  ANY SWITCH TO A NEW UNIT WILL
C  START THE READ AT THE BEGINNING)
C  ----------------------------------------------------------------

      if(nunit.eq.0) then
         if(ioldun.gt.0) rewind ioldun
         I03O29 = 0
         ioldun = 0
         return
      end if

      IF(NUNIT.NE.IOLDUN)  THEN

C THIS IS A NEW UNIT NUMBER, SET 'NEXT' TO 0 AND REWIND THIS UNIT
C ---------------------------------------------------------------

CDAKCDAK PRINT 87, NUNIT    NOW REDUNDANT TO PRINT THIS
   87 FORMAT(//' IW3UNP29/I03O29 - PREPARING TO READ ON29 DATA SET IN ',
     $ 'UNIT ',I3/)
         IOLDUN = NUNIT
         NEXT = 0
         NFILE = 0
         REWIND NUNIT
         ISWT = 0
      END IF

   10 CONTINUE

      IF(NEXT.NE.0)  GO TO 70

C COME HERE TO READ IN A NEW RECORD (EITHER REPORTS, Y2K COMPLIANT 40-
C  BYTE PSEUDO-ON85 LBL, NON-Y2K 32-BYTE COMPLIANT ON85 LBL, OR E-O-F)
C --------------------------------------------------------------------

      READ(NUNIT,END=9997,ERR=9998,FMT='(A8)') CBUFR
      IF(CBUFR(1:4).EQ.'BUFR' .OR. CBUFR(5:8).EQ.'BUFR') THEN

C  INPUT DATASET IS BUFR - EXIT IMMEDIATELY
C  ----------------------------------------

         IOLDUN = 0
         NEXT = 0
         IER = 999
         GO TO 90
      END IF

      REWIND NUNIT

      READ(NUNIT,ERR=9998,END=9997,FMT='(6432A1)')  CBUFF

C  IF ISWT=1, CHARACTER DATA IN RECORD ARE EBCDIC - CONVERT TO ASCII
C  -----------------------------------------------------------------

      IF(ISWT.EQ.1)  CALL AEA(CBUFF,CBUFF,6432)

      IF(NFILE.EQ.0)  THEN

C  TEST FOR EXPECTED HEADER LABEL
C  ------------------------------

         NFILE = 1

         IF(CBUFF(25)//CBUFF(26)//CBUFF(27)//CBUFF(28).EQ.'WASH')  THEN
        ELSEIF(CBUFF(21)//CBUFF(22)//CBUFF(23)//CBUFF(24).EQ.'WASH')THEN
         ELSE

C  QUICK CHECK SHOWS SOMETHING OTHER THAN EITHER Y2K COMPLIANT PSEUDO-
C  ON85 LBL OR NON-Y2K COMPLIANT ON85 LBL FOUND -- COULD MEAN CHARACTER
C  DATA ARE IN EBCDIC, SO SEE IF CONVERSION TO ASCII RECTIFIES THIS
C ---------------------------------------------------------------------

            PRINT 78
   78 FORMAT(/' ##IW3UNP29 - NEITHER EXPECTED Y2K COMPLIANT PSEUDO-',
     $ 'ON85 LABEL NOR SECOND CHOICE NON-Y2K COMPLIANT ON85 LABEL ',
     $ 'FOUND IN'/14X,'FIRST RECORD OF FILE  -- TRY EBCDIC TO ASCII ',
     $ 'CONVERSION'/)
            CALL AEA(CBUFF,CBUFF,6432)
            ISWT = 1
         END IF

         IF(CBUFF(25)//CBUFF(26)//CBUFF(27)//CBUFF(28).EQ.'WASH')  THEN

C  THIS IS Y2K COMPLIANT 40-BYTE PSEUDO-ON85 LBL; RESET 'NEXT', SET
C   'IER', FILL 'OBS(1)-(4)', AND QUIT
C  ---------------------------------------------------------------
            NEXT = 0
            IER = 1
            OBS(1:5) = IBUFF(1:5)
            GO TO 90
         ELSE  IF(CBUFF(21)//CBUFF(22)//CBUFF(23)//CBUFF(24).EQ.'WASH')
     $    THEN

C  THIS IS NON-Y2K COMPLIANT 32-BYTE ON85 LBL; RESET 'NEXT', SET
C   'IER', USE "WINDOWING" TECHNIQUE TO CONTRUCT 4-DIGIT YEAR,
C   CONSTRUCT A 40-BYTE PSEUDO-ON85 LABE, FILL 'OBS(1)-(4)', AND QUIT
C  ------------------------------------------------------------------
            PRINT'(" ==> THIS IS A TRUE OFFICE NOTE 29 FILE!! <==")'
            PRINT 88
   88 FORMAT(/' ##IW3UNP29/I03O29 - WARNING: ORIGINAL NON-Y2K ',
     $ 'COMPLIANT ON85 LABEL FOUND IN FIRST RECORD OF FILE INSTEAD OF ',
     $ 'EXPECTED'/30X,'Y2K COMPLIANT PSEUDO-ON85 LABEL -- THIS ',
     $ 'ROUTINE IS FORCED TO USE "WINDOWING" TECHNIQUE TO CONTRUCT'/30X,
     $'A Y2K COMPLIANT PSEUDO-ON85 LABEL TO RETURN TO CALLING PROGRAM'/)

            NEXT = 0
            IER = 1

            CBF910 = CBUFF(9)//CBUFF(10)
            READ(CBF910,'(I2)')  IYR2D
            PRINT'(" ##IW3UNP29/I03O29 - 2-DIGIT YEAR FOUND IN ON85 ",
     $       "LBL (",A,") IS: ",I0/19X," USE WINDOWING TECHNIQUE TO ",
     $       "OBTAIN 4-DIGIT YEAR")', CBUFF(1:32),IYR2D
            IF(IYR2D.GT.20)  THEN
               IYR4D = 1900 + IYR2D
            ELSE
               IYR4D = 2000 + IYR2D
            ENDIF
            PRINT'(" ##IW3UNP29/I03O29 - 4-DIGIT YEAR OBTAINED VIA ",
     $       "WINDOWING TECHNIQUE IS: ",I0/)', IYR4D
            CON85L = CBUFF(1:32)
            CBUFF(7:40) = ' '
            CBUFF(9:10) = CON85L(7:8)
            WRITE(CYR4D,'(I4.4)') IYR4D
            DO I=1,4
              CBUFF(10+I) = CYR4D(I:I)
            ENDDO
            CBUFF(15:36) = CON85L(11:32)
            OBS(1:5) = IBUFF(1:5)
            GO TO 90
         ELSE

C  SOMETHING OTHER THAN EITHER Y2K COMPLIANT PSEUDO-ON85 LBL OR
C  NON-Y2K COMPLIANT ON85 LBL FOUND; RESET 'NEXT', SET 'IER' AND QUIT
C  ------------------------------------------------------------------
CDAKCDAK    PRINT 88  CAN'T PRINT THIS ANYMORE
CDA88 FORMAT(/' ##IW3UNP29/I03O29 - EXPECTED ON85 LABEL NOT FOUND IN ',
CDAK $ 'FIRST RECORD OF NEW LOGICAL FILE  -- IER = 999'/)
            IOLDUN = 0
            NEXT = 0
            IER = 999
            GO TO 90
         END IF

      END IF

      IF(CBUFF(1)//CBUFF(2)//CBUFF(3)//CBUFF(4).EQ.'ENDO')  THEN

C  LOGICAL "ENDOF FILE" READ; RESET NEXT, SET IER, AND QUIT
C  --------------------------------------------------------

         NEXT = 0
         IER = 2
         NFILE = 0
         GO TO 90
      END IF
      GO TO 70

 9997 CONTINUE

C  PHYSICAL END OF FILE; RESET 'NEXT', SET 'IER' AND QUIT
C  ------------------------------------------------------

      NEXT = 0
      IER = 3
      GO TO 90

 9998 CONTINUE

C  I/O ERROR; RESET 'NEXT', SET 'IER' AND QUIT
C  -------------------------------------------

cppppp
      print'(" ##IW3UNP29/I03O29 - ERROR READING DATA RECORD")'
cppppp
      NEXT = 0
      IER = 4
      GO TO 90

   70 CONTINUE

C  WORKING WITHIN ACTUAL DATA REC. READ, CALL W3FI64 TO READ IN NEXT RPT
C  ---------------------------------------------------------------------

      CALL W3FI64(CBUFF,OBS,NEXT)

      IF(NEXT.GE.0)  THEN

C  REPORT SUCCESSFULLY RETURNED IN ARRAY 'OBS'
C  -------------------------------------------

         IER = 0

      ELSE

C HIT END-OF-RECORD, OR INTERNAL READ ERROR ENCOUNTERED & CAN'T RECOVER
C  -- READ IN NEXT RECORD OF REPORTS
C ---------------------------------------------------------------------

         NEXT = 0
         GO TO 10
      END IF

   90 CONTINUE

      I03O29 = IER

      RETURN

      END
C***********************************************************************
C>    This function read subset and returns group name.
C>    @param SUBSET subset
C>    @return group name
C>
C>    @author Dennis Keyser @date 2013-03-20
C>
C***********************************************************************
      FUNCTION C01O29(SUBSET)
C     ---> formerly FUNCTION ADP

      CHARACTER*(*) SUBSET
      CHARACTER*6   C01O29

      SAVE

      C01O29 = 'NONE'

      IF(SUBSET(1:5).EQ.'NC000')  C01O29 = 'ADPSFC'
      IF(SUBSET(1:5).EQ.'NC001')  THEN
         IF(SUBSET(6:8).NE.'006')  THEN
            C01O29 = 'SFCSHP'
         ELSE
            C01O29 = 'SFCBOG'
         END IF
      END IF
      IF(SUBSET(1:5).EQ.'NC002')  C01O29 = 'ADPUPA'
      IF(SUBSET(1:5).EQ.'NC004')  C01O29 = 'AIRCFT'
      IF(SUBSET(1:5).EQ.'NC005')  C01O29 = 'SATWND'
      IF(SUBSET(1:5).EQ.'NC012')  C01O29 = 'SPSSMI'

      IF(SUBSET .EQ. 'NC003101')  C01O29 = 'SATEMP'
      IF(SUBSET .EQ. 'NC004004')  C01O29 = 'AIRCAR'
      IF(SUBSET .EQ. 'NC004005')  C01O29 = 'ADPUPA'

      IF(SUBSET .EQ. 'ADPSFC')  C01O29 = 'ADPSFC'
      IF(SUBSET .EQ. 'SFCSHP')  C01O29 = 'SFCSHP'
      IF(SUBSET .EQ. 'SFCBOG')  C01O29 = 'SFCBOG'
      IF(SUBSET .EQ. 'ADPUPA')  C01O29 = 'ADPUPA'
      IF(SUBSET .EQ. 'AIRCFT')  C01O29 = 'AIRCFT'
      IF(SUBSET .EQ. 'SATWND')  C01O29 = 'SATWND'
      IF(SUBSET .EQ. 'SATEMP')  C01O29 = 'SATEMP'
      IF(SUBSET .EQ. 'AIRCAR')  C01O29 = 'AIRCAR'
      IF(SUBSET .EQ. 'SPSSMI')  C01O29 = 'SPSSMI'

      IF(C01O29.EQ.'NONE') PRINT'(" ##IW3UNP29/C01O29 - UNKNOWN SUBSET",
     $ " (=",A,") -- CONTINUE~~")', SUBSET

      RETURN
      END
C***********************************************************************
C>    This function read subset and returns corresponding file data.
C>    @param SUBSET subset
C>    @param LUNIT full path of file
C>    @param OBS data output
C>    @return file data
C>
C>    @author Dennis Keyser @date 2013-03-20
C>
C***********************************************************************
      FUNCTION R01O29(SUBSET,LUNIT,OBS)
C     ---> formerly FUNCTION ADC

      CHARACTER*(*) SUBSET
      CHARACTER*6   C01O29,ADPSUB
      DIMENSION     OBS(*)

      SAVE

C  FIND AN ON29/124 DATA TYPE AND CALL A TRANSLATOR
C  ------------------------------------------------

      R01O29 = 4
      ADPSUB = C01O29(SUBSET)
      IF(ADPSUB .EQ. 'ADPSFC')  R01O29 = R04O29(LUNIT,OBS)
      IF(ADPSUB .EQ. 'SFCSHP')  R01O29 = R04O29(LUNIT,OBS)
      IF(ADPSUB .EQ. 'SFCBOG')  R01O29 = R04O29(LUNIT,OBS)
      IF(ADPSUB .EQ. 'ADPUPA')  R01O29 = R03O29(LUNIT,OBS)
      IF(ADPSUB .EQ. 'AIRCFT')  R01O29 = R05O29(LUNIT,OBS)
      IF(ADPSUB .EQ. 'AIRCAR')  R01O29 = R05O29(LUNIT,OBS)
      IF(ADPSUB .EQ. 'SATWND')  R01O29 = R06O29(LUNIT,OBS)
      IF(ADPSUB .EQ. 'SPSSMI')  R01O29 = R07O29(LUNIT,OBS)
      RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      SUBROUTINE S01O29(SID,XOB,YOB,RHR,RCH,RSV,RSV2,ELV,ITP,RTP)
C     ---> Formerly SUBROUTINE O29HDR

      COMMON/IO29DD/HDR(12),RCATS(50,150,11),IKAT(11),MCAT(11),NCAT(11)
      COMMON/IO29LL/BMISS

      CHARACTER*(*) RSV,RSV2
      CHARACTER*8   COB,SID,RCT
      DIMENSION     IHDR(12),RHDR(12),ICATS(50,150,11)
      REAL(8)       BMISS
      EQUIVALENCE   (IHDR(1),RHDR(1)),(COB,IOB),(ICATS,RCATS)

      SAVE

      DATA OMISS/99999/

C  INITIALIZE THE UNPACK ARRAY TO MISSINGS
C  ---------------------------------------

      NCAT = 0
      RCATS = OMISS
      COB = '        '
      ICATS(6,1:149,1) = IOB
      ICATS(4,1:149,2) = IOB
      ICATS(4,1:149,3) = IOB
      ICATS(4,1:149,4) = IOB
      ICATS(6,1:149,5) = IOB
      ICATS(6,1:149,6) = IOB
      ICATS(3,1:149,7) = IOB
      ICATS(3,1:149,8) = IOB

C  WRITE THE RECEIPT TIME IN CHARACTERS
C  ------------------------------------

      RCT = '9999    '
      IF(RCH*100.LT.2401.AND.RCH*100.GT.-1)
     $ WRITE(RCT,'(I4.4)') NINT(RCH*100.)

C  STORE THE ON29 HEADER INFORMATION INTO UNP FORMAT
C  -------------------------------------------------

      RHDR( 1) = OMISS
      IF(YOB.LT.BMISS)  RHDR( 1) = NINT(100.*YOB)
cppppp
      IF(YOB.GE.BMISS)  print'(" ~~IW3UNP29/S01O29: ID ",A," has a ",
     $ "missing LATITUDE - on29 hdr, word 1 is set to ",G0)',
     $ sid,RHDR(1)
cppppp
      RHDR( 2) = OMISS
      IF(XOB.LT.BMISS)  RHDR( 2) = NINT(100.*MOD(720.-XOB,360.))
cppppp
      IF(XOB.GE.BMISS)  print'(" ~~IW3UNP29/S01O29: ID ",A," has a ",
     $ "missing LONGITUDE - on29 hdr, word 2 is set to ",G0)',
     $ sid,RHDR(2)
cppppp
      RHDR( 3) = OMISS
      RHDR( 4) = OMISS
      IF(RHR.LT.BMISS)  RHDR( 4) = NINT((100.*RHR)+0.0001)
cppppp
      IF(RHR.GE.BMISS)  print'(" ~~IW3UNP29/S01O29: ID ",A," has a ",
     $ "missing OB TIME - on29 hdr, word 4 is set to ",G0)', sid,RHDR(4)
cppppp
      IF(RSV2.EQ.'        ')  THEN
         COB = '        '
         COB(1:4) = RCT(3:4)//RSV(1:2)
         IHDR(5) = IOB
         COB = '        '
         COB(1:3) = RCT(1:2)//RSV(3:3)
         IHDR(6) = IOB
      ELSE
         COB = '        '
         COB(1:4) = RSV2(3:4)//RSV(1:2)
         IHDR(5) = IOB
         COB = '        '
         COB(1:3) = RSV2(1:2)//RSV(3:3)
         IHDR(6) = IOB
      END IF
      RHDR( 7) = NINT(ELV)
      IHDR( 8) = ITP
      IHDR( 9) = RTP
      RHDR(10) = OMISS
      COB = '        '
      COB(1:4) = SID(1:4)
      IHDR(11) = IOB
      COB = '        '
      COB(1:4) = SID(5:6)//'  '
      IHDR(12) = IOB

C  STORE THE HEADER INTO A HOLDING ARRAY
C  -------------------------------------

      HDR = RHDR

      RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      SUBROUTINE S02O29(ICAT,N,*)
C     ---> Formerly SUBROUTINE O29CAT

      COMMON/IO29DD/HDR(12),RCATS(50,150,11),IKAT(11),MCAT(11),NCAT(11)
      COMMON/IO29EE/POB(255),QOB(255),TOB(255),ZOB(255),DOB(255),
     $              SOB(255),VSG(255),CLP(255),CLA(255),OB8(255),
     $              CF8(255)
      COMMON/IO29FF/PQM(255),QQM(255),TQM(255),ZQM(255),WQM(255),
     $              QCP(255),QCA(255),Q81(255),Q82(255)
      COMMON/IO29GG/PSL,STP,SDR,SSP,STM,DPD,TMX,TMI,HVZ,PRW,PW1,CCN,CHN,
     $              CTL,CTM,CTH,HCB,CPT,APT,PC6,SND,P24,DOP,POW,HOW,SWD,
     $              SWP,SWH,SST,SPG,SPD,SHC,SAS,WES
      COMMON/IO29HH/PSQ,SPQ,SWQ,STQ,DDQ
      COMMON/IO29II/PWMIN
      COMMON/IO29LL/BMISS

      CHARACTER*8 COB,C11,C12
      CHARACTER*1 PQM,QQM,TQM,ZQM,WQM,QCP,QCA,Q81,Q82,PSQ,SPQ,SWQ,STQ,
     $ DDQ
      DIMENSION   RCAT(50),JCAT(50)
      REAL(8)     BMISS
      EQUIVALENCE (RCAT(1),JCAT(1)),(C11,HDR(11)),(C12,HDR(12)),
     $ (COB,IOB)
      LOGICAL     SURF

      SAVE

cppppp-ID
      iprint = 0
c     if(C11(1:4)//C12(1:2).eq.'59758 ')  iprint = 1
c     if(C11(1:4)//C12(1:2).eq.'59362 ')  iprint = 1
c     if(C11(1:4)//C12(1:2).eq.'57957 ')  iprint = 1
c     if(C11(1:4)//C12(1:2).eq.'74794 ')  iprint = 1
c     if(C11(1:4)//C12(1:2).eq.'74389 ')  iprint = 1
c     if(C11(1:4)//C12(1:2).eq.'96801A')  iprint = 1
cppppp-ID

      SURF = .FALSE.
      GOTO 1

C  ENTRY POINT SE01O29 FORCES DATA INTO THE SURFACE (FIRST) LEVEL
C  --------------------------------------------------------------

      ENTRY SE01O29(ICAT,N)
C     ---> formerly ENTRY O29SFC
      SURF = .TRUE.

C  CHECK THE PARAMETERS COMING IN
C  ------------------------------

1     KCAT = 0
      DO I = 1,11
         IF(ICAT.EQ.IKAT(I))  THEN
            KCAT = I
            GO TO 991
         END IF
      ENDDO

  991 CONTINUE

C  PARAMETER ICAT (ON29 CATEGORY) OUT OF BOUNDS RETURNS A 999
C  ----------------------------------------------------------

      IF(KCAT.EQ.0)  THEN
         PRINT'(" ##IW3UNP29/S02O29 - ON29 CATEGORY ",I0," OUT OF ",
     $    "BOUNDS -- IER = 999")', ICAT
         RETURN 1
      END IF

C  PARAMETER N (LEVEL INDEX) OUT OF BOUNDS RETURNS A 999
C  -----------------------------------------------------

      IF(N.GT.255)  THEN
         PRINT'(" ##IW3UNP29/S02O29 - LEVEL INDEX ",I0," EXCEEDS 255 ",
     $    "-- IER = 999")', N
         RETURN 1
      END IF

C  MAKE A MISSING LEVEL AND RETURN WHEN N=0 (NOT ALLOWED FOR CAT 01)
C  -----------------------------------------------------------------

      IF(N.EQ.0) THEN
         IF(KCAT.EQ.1) RETURN
         NCAT(KCAT) = MIN(149,NCAT(KCAT)+1)
cppppp
         if(iprint.eq.1)
     $    print'(" To prepare for sfc. data, write all missings on ",
     $     "lvl ",I0," for cat ",I0)', ncat(kcat),kcat
cppppp
         RETURN
      END IF

C  FIGURE OUT WHICH LEVEL TO UPDATE AND RESET THE LEVEL COUNTER
C  ------------------------------------------------------------

      IF(KCAT.EQ.1) THEN
         L = I04O29(POB(N)*.1)
         IF(L.EQ.999999)  GO TO 9999

C  BAD MANDATORY LEVEL RETURNS A 999
C  ---------------------------------

         IF(L.LE.0)  THEN
            PRINT'(" ##IW3UNP29/S02O29 - BAD MANDATORY LEVEL (P = ",
     $       G0,") -- IER = 999")', POB(N)
            RETURN 1
         END IF
         NCAT(KCAT) = MAX(NCAT(KCAT),L)
cppppp
         if(iprint.eq.1)
     $    print'(" Will write cat. 1 data on lvl ",I0," for cat ",I0,
     $    ", - total no. cat. 1 lvls processed so far = ",I0)',
     $    L,kcat,ncat(kcat)
cppppp
      ELSEIF(SURF) THEN
         L = 1
         NCAT(KCAT) = MAX(NCAT(KCAT),1)
cppppp
         if(iprint.eq.1)
     $    print'(" Will write cat. ",I0," SURFACE data on lvl ",I0,
     $    ", - total no. cat. ",I0," lvls processed so far = ",I0)',
     $    kcat,L,kcat,ncat(kcat)
cppppp
      ELSE
         L = MIN(149,NCAT(KCAT)+1)
         IF(L.EQ.149) THEN
cppppp
            print'(" ~~IW3UNP29/S02O29: ID ",A," - This cat. ",I0,
     $       " level cannot be processed because the limit has already",
     $       " been reached")', c11(1:4)//c12(1:2),kcat
cppppp
            RETURN
         END IF
         NCAT(KCAT) = L
cppppp
         if(iprint.eq.1)
     $    print'(" Will write cat. ",I0," NON-SFC data on lvl ",I0,
     $    ", - total no. cat. ",I0," lvls processed so far = ",I0)',
     $    kcat,L,kcat,ncat(kcat)
cppppp
      END IF

C  EACH CATEGORY NEEDS A SPECIFIC DATA ARRANGEMENT
C  -----------------------------------------------

      COB = '        '
      IF(ICAT.EQ.1) THEN
         RCAT(1) = MIN(NINT(ZOB(N)),NINT(RCATS(1,L,KCAT)))
         RCAT(2) = MIN(NINT(TOB(N)),NINT(RCATS(2,L,KCAT)))
         RCAT(3) = MIN(NINT(QOB(N)),NINT(RCATS(3,L,KCAT)))
         RCAT(4) = MIN(NINT(DOB(N)),NINT(RCATS(4,L,KCAT)))
         RCAT(5) = MIN(NINT(SOB(N)),NINT(RCATS(5,L,KCAT)))
         COB(1:4) = ZQM(N)//TQM(N)//QQM(N)//WQM(N)
         JCAT(6) = IOB
      ELSEIF(ICAT.EQ.2) THEN
         RCAT(1) = MIN(NINT(POB(N)),99999)
         RCAT(2) = MIN(NINT(TOB(N)),99999)
         RCAT(3) = MIN(NINT(QOB(N)),99999)
         COB(1:3) = PQM(N)//TQM(N)//QQM(N)
         JCAT(4) = IOB
      ELSEIF(ICAT.EQ.3) THEN
         RCAT(1) = MIN(NINT(POB(N)),99999)
         RCAT(2) = MIN(NINT(DOB(N)),99999)
         RCAT(3) = MIN(NINT(SOB(N)),99999)

C  MARK THE TROPOPAUSE LEVEL IN CAT. 3

         IF(NINT(VSG(N)).EQ.16)  PQM(N) = 'T'

C  MARK THE MAXIMUM WIND LEVEL IN CAT. 3

         IF(NINT(VSG(N)).EQ. 8)  THEN
            PQM(N) = 'W'
            IF(POB(N).EQ.PWMIN)  PQM(N) = 'X'
         END IF
         COB(1:2) = PQM(N)//WQM(N)
         JCAT(4) = IOB
      ELSEIF(ICAT.EQ.4) THEN
         RCAT(1) = MIN(NINT(ZOB(N)),99999)
         RCAT(2) = MIN(NINT(DOB(N)),99999)
         RCAT(3) = MIN(NINT(SOB(N)),99999)
         COB(1:2) = ZQM(N)//WQM(N)
         JCAT(4) = IOB
      ELSEIF(ICAT.EQ.5) THEN
         RCAT(1) = MIN(NINT(POB(N)),99999)
         RCAT(2) = MIN(NINT(TOB(N)),99999)
         RCAT(3) = MIN(NINT(QOB(N)),99999)
         RCAT(4) = MIN(NINT(DOB(N)),99999)
         RCAT(5) = MIN(NINT(SOB(N)),99999)
         COB(1:4) = PQM(N)//TQM(N)//QQM(N)//WQM(N)
         JCAT(6) = IOB
      ELSEIF(ICAT.EQ.6) THEN
         RCAT(1) = MIN(NINT(ZOB(N)),99999)
         RCAT(2) = MIN(NINT(TOB(N)),99999)
         RCAT(3) = MIN(NINT(QOB(N)),99999)
         RCAT(4) = MIN(NINT(DOB(N)),99999)
         RCAT(5) = MIN(NINT(SOB(N)),99999)
         COB(1:4) = ZQM(N)//TQM(N)//QQM(N)//WQM(N)
         JCAT(6) = IOB
      ELSEIF(ICAT.EQ.7) THEN
         RCAT(1) = MIN(NINT(CLP(N)),99999)
         RCAT(2) = MIN(NINT(CLA(N)),99999)
         COB(1:2) = QCP(N)//QCA(N)
         JCAT(3) = IOB
      ELSEIF(ICAT.EQ.8) THEN
         RCAT(1) = MIN(NINT(OB8(N)),99999)
         RCAT(2) = MIN(NINT(CF8(N)),99999)
         COB(1:2) = Q81(N)//Q82(N)
         JCAT(3) = IOB
      ELSEIF(ICAT.EQ.51) THEN
         RCAT( 1) = MIN(NINT(PSL),99999)
         RCAT( 2) = MIN(NINT(STP),99999)
         RCAT( 3) = MIN(NINT(SDR),99999)
         RCAT( 4) = MIN(NINT(SSP),99999)
         RCAT( 5) = MIN(NINT(STM),99999)
         RCAT( 6) = MIN(NINT(DPD),99999)
         RCAT( 7) = MIN(NINT(TMX),99999)
         RCAT( 8) = MIN(NINT(TMI),99999)
         COB(1:4) = PSQ//SPQ//SWQ//STQ
         JCAT(9) = IOB
         COB = '        '
         COB(1:1) = DDQ
         JCAT(10) = IOB
         JCAT(11) = MIN(NINT(HVZ),99999)
         JCAT(12) = MIN(NINT(PRW),99999)
         JCAT(13) = MIN(NINT(PW1),99999)
         JCAT(14) = MIN(NINT(CCN),99999)
         JCAT(15) = MIN(NINT(CHN),99999)
         JCAT(16) = MIN(NINT(CTL),99999)
         JCAT(17) = MIN(NINT(HCB),99999)
         JCAT(18) = MIN(NINT(CTM),99999)
         JCAT(19) = MIN(NINT(CTH),99999)
         JCAT(20) = MIN(NINT(CPT),99999)
         RCAT(21) = MIN(ABS(NINT(APT)),99999)
         IF(CPT.GE.BMISS.AND.APT.LT.0.)
     $    RCAT(21) = MIN(ABS(NINT(APT))+500,99999)
      ELSEIF(ICAT.EQ.52) THEN
         JCAT( 1) = MIN(NINT(PC6),99999)
         JCAT( 2) = MIN(NINT(SND),99999)
         JCAT( 3) = MIN(NINT(P24),99999)
         JCAT( 4) = MIN(NINT(DOP),99999)
         JCAT( 5) = MIN(NINT(POW),99999)
         JCAT( 6) = MIN(NINT(HOW),99999)
         JCAT( 7) = MIN(NINT(SWD),99999)
         JCAT( 8) = MIN(NINT(SWP),99999)
         JCAT( 9) = MIN(NINT(SWH),99999)
         JCAT(10) = MIN(NINT(SST),99999)
         JCAT(11) = MIN(NINT(SPG),99999)
         JCAT(12) = MIN(NINT(SPD),99999)
         JCAT(13) = MIN(NINT(SHC),99999)
         JCAT(14) = MIN(NINT(SAS),99999)
         JCAT(15) = MIN(NINT(WES),99999)
      ELSE

C  UNSUPPORTED CATEGORY RETURNS A 999
C  ----------------------------------

         PRINT'(" ##IW3UNP29/S02O29 - CATEGORY ",I0," NOT SUPPORTED ",
     $    "-- IER = 999")', ICAT
         RETURN 1
      END IF

C  TRANSFER THE LEVEL DATA INTO THE HOLDING ARRAY AND EXIT
C  -------------------------------------------------------

      DO I = 1,MCAT(KCAT)
         RCATS(I,L,KCAT) = RCAT(I)
      ENDDO

      RETURN
 9999 CONTINUE
      RETURN 1
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      SUBROUTINE S03O29(UNP,SUBSET,*,*)
C     ---> Formerly SUBROUTINE O29UNP

      COMMON/IO29DD/HDR(12),RCATS(50,150,11),IKAT(11),MCAT(11),NCAT(11)

      DIMENSION   RCAT(50),JCAT(50),UNP(*)
      CHARACTER*8  SUBSET
      EQUIVALENCE (RCAT(1),JCAT(1))

      SAVE

C  CALL TO SORT CATEGORIES 02, 03, 04, AND 08 LEVELS
C  -------------------------------------------------

      CALL S04O29

C  TRANSFER DATA FROM ALL CATEGORIES INTO UNP ARRAY & SET POINTERS
C  ---------------------------------------------------------------

      INDX = 43
      JCAT = 0
      NLEVTO = 0
      NLEVC8 = 0

      DO K = 1,11
         JCAT(2*K+11) = NCAT(K)
         IF(K.NE.7.AND.K.NE.8.AND.K.NE.11)  THEN
            NLEVTO = NLEVTO + NCAT(K)
         ELSE  IF(K.EQ.8)  THEN
            NLEVC8 = NLEVC8 + NCAT(K)
         END IF
         IF(NCAT(K).GT.0) JCAT(2*K+12) = INDX
         IF(NCAT(K).EQ.0) JCAT(2*K+12) = 0
         DO J = 1,NCAT(K)
         DO I = 1,MCAT(K)

C  UNPACKED ON29 REPORT CONTAINS MORE THAN 1608 WORDS - RETURNS A 999
C  ------------------------------------------------------------------

            IF(INDX.GT.1608)  THEN
               PRINT'(" ##IW3UNP29/S03O29 - UNPKED ON29 RPT CONTAINS ",
     $          I0," WORDS, > LIMIT OF 1608 -- IER = 999")', INDX
               RETURN 1
            END IF
            UNP(INDX) = RCATS(I,J,K)
            INDX = INDX+1
         ENDDO
         ENDDO
      ENDDO

C  RETURN WITHOUT PROCESSING THIS REPORT IF NO DATA IN CAT. 1-6, 51, 52
C   (UNLESS SSM/I REPORT, THEN DO NOT RETURN UNLESS ALSO NO CAT. 8 DATA)
C  --------------------------------------------------------------------

      IF(NLEVTO.EQ.0)  THEN
         IF(SUBSET(1:5).NE.'NC012')  THEN
            RETURN 2
         ELSE
            IF(NLEVC8.EQ.0)  RETURN 2
         END IF
      END IF

C  TRANSFER THE HEADER AND POINTER ARRAYS INTO UNP
C  -----------------------------------------------

      UNP(1:12) =  HDR
      UNP(13:42) = RCAT(13:42)

      RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      SUBROUTINE S04O29
C     ---> Formerly SUBROUTINE O29SRT

      COMMON/IO29DD/HDR(12),RCATS(50,150,11),IKAT(11),MCAT(11),NCAT(11)
cppppp
      character*8 c11,c12,sid
cppppp

      DIMENSION RCAT(50,150),IORD(150),IWORK(65536),SCAT(50,150),RCTL(3)
cppppp
      EQUIVALENCE  (C11,HDR(11)),(C12,HDR(12))
cppppp

      SAVE

cppppp
      sid = c11(1:4)//c12(1:4)
cppppp

C  SORT CATEGORIES 2, 3, AND 4 - LEAVE THE FIRST LEVEL IN EACH INTACT
C  ------------------------------------------------------------------

      DO K=2,4
         IF(NCAT(K).GT.1) THEN
            DO J=1,NCAT(K)-1
            DO I=1,MCAT(K)
               SCAT(I,J) = RCATS(I,J+1,K)
            ENDDO
            ENDDO
            CALL ORDERS(2,IWORK,SCAT(1,1),IORD,NCAT(K)-1,50,8,2)
            RCTL = 10E9
            DO J=1,NCAT(K)-1
               IF(K.LT.4) JJ = IORD((NCAT(K)-1)-J+1)
               IF(K.EQ.4) JJ = IORD(J)
               DO I=1,MCAT(K)
                  RCAT(I,J) = SCAT(I,JJ)
               ENDDO
               IDUP = 0
               IF(NINT(RCAT(1,J)).EQ.NINT(RCTL(1)))  THEN
                  IF(NINT(RCAT(2,J)).EQ.NINT(RCTL(2)).AND.
     $               NINT(RCAT(3,J)).EQ.NINT(RCTL(3)))  THEN
cppppp
                     if(k.ne.4)  then
                        print'(" ~~@@IW3UNP29/S04O29: ID ",A," has a ",
     $ "dupl. cat. ",I0," lvl (all data) at ",G0," mb -- lvl will be ",
     $ "excluded from processing")', sid,k,rcat(1,j)*.1
                     else
                        print'(" ~~@@IW3UNP29/S04O29: ID ",A," has a ",
     $ "dupl. cat. ",I0," lvl (all data) at ",G0," m -- lvl will be ",
     $ "excluded from processing")', sid,k,rcat(1,j)
                     end if
cppppp
                     IDUP = 1
                  ELSE
cppppp
                     if(k.ne.4)  then
                        print'(" ~~@@#IW3UNP29/S04O29: ID ",A," has a ",
     $ "dupl. cat. ",I0," press. lvl (data differ) at ",G0," mb -- lvl",
     $ " will NOT be excluded")', sid,k,rcat(1,j)*.1
                     else
                        print'(" ~~@@#IW3UNP29/S04O29: ID ",A," has a ",
     $ "dupl. cat. ",I0," height lvl (data differ) at ",G0," m -- lvl ",
     $ "will NOT be excluded")', sid,k,rcat(1,j)
                     end if
cppppp
                  END IF
               END IF
               RCTL = RCAT(1:3,J)
               IF(IDUP.EQ.1)  RCAT(1,J) = 10E8
            ENDDO
            JJJ = 1
            DO J=2,NCAT(K)
               IF(RCAT(1,J-1).GE.10E8)  GO TO 887
               JJJ = JJJ + 1
               DO I=1,MCAT(K)
                  RCATS(I,JJJ,K) = RCAT(I,J-1)
               ENDDO
  887          CONTINUE
            ENDDO
cppppp
            if(jjj.ne.NCAT(K))
     $       print'(" ~~@@IW3UNP29/S04O29: ID ",A," has had ",I0,
     $        " lvls removed due to their being duplicates")',
     $        sid,NCAT(K)-jjj
cppppp
            ncat(k) = jjj
         end if
         IF(NCAT(K).EQ.1)  THEN
            IF(MIN(RCATS(1,1,K),RCATS(2,1,K),RCATS(3,1,K)).GT.99998.8)
     $       NCAT(K) = 0
         END IF
      ENDDO

C  SORT CATEGORY 08 BY CODE FIGURE
C  -------------------------------

      DO K=8,8
      IF(NCAT(K).GT.1) THEN
         CALL ORDERS(2,IWORK,RCATS(2,1,K),IORD,NCAT(K),50,8,2)
         DO J=1,NCAT(K)
         DO I=1,MCAT(K)
         RCAT(I,J) = RCATS(I,IORD(J),K)
         ENDDO
         ENDDO
         DO J=1,NCAT(K)
         DO I=1,MCAT(K)
         RCATS(I,J,K) = RCAT(I,J)
         ENDDO
         ENDDO
      END IF
      ENDDO

C  NORMAL EXIT
C  -----------

      RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      SUBROUTINE S05O29
C     ---> Formerly SUBROUTINE O29INX

      COMMON/IO29EE/OBS(255,11)
      COMMON/IO29FF/QMS(255,9)
      COMMON/IO29GG/SFO(34)
      COMMON/IO29HH/SFQ(5)
      COMMON/IO29LL/BMISS

      CHARACTER*1 QMS,SFQ

      REAL(8)     BMISS

      SAVE

C  SET THE INPUT DATA ARRAYS TO MISSING OR BLANK
C  ---------------------------------------------

      OBS = BMISS
      QMS = ' '
      SFO = BMISS
      SFQ = ' '

      RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION I04O29(P)
C     ---> formerly FUNCTION MANO29

      COMMON/IO29JJ/ISET,MANLIN(1001)

      SAVE

      IF(ISET.EQ.0) THEN
         MANLIN = 0

         MANLIN(1000) =  1
         MANLIN(850)  =  2
         MANLIN(700)  =  3
         MANLIN(500)  =  4
         MANLIN(400)  =  5
         MANLIN(300)  =  6
         MANLIN(250)  =  7
         MANLIN(200)  =  8
         MANLIN(150)  =  9
         MANLIN(100)  = 10
         MANLIN(70)   = 11
         MANLIN(50)   = 12
         MANLIN(30)   = 13
         MANLIN(20)   = 14
         MANLIN(10)   = 15
         MANLIN(7)    = 16
         MANLIN(5)    = 17
         MANLIN(3)    = 18
         MANLIN(2)    = 19
         MANLIN(1)    = 20

         ISET = 1
      END IF

      IP = NINT(P*10.)

      IF(IP.GT.10000 .OR. IP.LT.10 .OR. MOD(IP,10).NE.0) THEN
         I04O29 = 0
      ELSE
         I04O29 = MANLIN(IP/10)
      END IF

      RETURN

      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION R02O29()
C     ---> formerly FUNCTION ONFUN

      COMMON/IO29LL/BMISS

      CHARACTER*8 SUBSET,RPID
      LOGICAL     L02O29,L03O29
      INTEGER KKK(0:99),KKKK(49)
      REAL(8) BMISS

      SAVE

      DATA GRAV/9.8/,CM2K/1.94/,TZRO/273.15/
      DATA KKK /5*90,16*91,30*92,49*93/
      DATA KKKK/94,2*95,6*96,10*97,30*98/

      PRS1(Z) = 1013.25 * (((288.15 - (.0065 * Z))/288.15)**5.256)
      PRS2(Z) = 226.3 * EXP(1.576106E-4 * (11000. - Z))
      PRS3(PMND,TEMP,Z,ZMND)
     $ = PMND * (((TEMP - (.0065 * (Z - ZMND)))/TEMP)**5.256)
      ES(T) = 6.1078 * EXP((17.269 * (T-273.16))/((T-273.16)+237.3))
      QFRMTP(T,PPPP) = (0.622 * ES(T))/(PPPP-(0.378 * ES(T)))
      HGTF(P) = (1.-(P/1013.25)**(1./5.256))*(288.15/.0065)

      R02O29 = 0

      RETURN

      ENTRY E01O29(PRS)
C     ---> formerly ENTRY ONPRS
         IF(PRS.LT.BMISS) E01O29 =  NINT(PRS*.1)
         IF(PRS.GE.BMISS) E01O29 =  BMISS
         RETURN
      ENTRY E37O29(PMND,TEMP,HGT,ZMND,TQM)
C     ---> formerly ENTRY ONPFHT
         IF(HGT.GE.BMISS)  THEN
            E37O29 =  BMISS
         ELSE
            IF(HGT.LE.11000)  THEN
               P =  PRS1(HGT)
            ELSE
               P =  PRS2(HGT)
            END IF
            IF(MAX(PMND,ZMND).GE.BMISS)  THEN
               E37O29 = P
               RETURN
            END IF
            IF(TEMP.GE.9999.) TEMP = BMISS
            IF(TQM.GE.BMISS)  TQM = 2
            IF(TEMP.GE.BMISS.OR.TQM.GE.4)  CALL W3FA03(P,D1,TEMP,D2)
            Q = QFRMTP(TEMP,P)
            TVIRT = TEMP * (1.0 + (0.61 * Q))
            E37O29 = PRS3(PMND,TVIRT,HGT,ZMND)
         END IF
         RETURN
      ENTRY E03O29(PRS)
C     ---> formerly ENTRY ONHFP
         IF(PRS.LT.BMISS) E03O29 =  HGTF(PRS)
         IF(PRS.GE.BMISS) E03O29 =  BMISS
         RETURN
      ENTRY E04O29(WDR,WSP)
C     ---> formerly ENTRY ONWDR
         E04O29 = WDR
         RETURN
      ENTRY E05O29(WDR,WSP)
C     ---> formerly ENTRY ONWSP
         IF(WSP.LT.BMISS) THEN
            E05O29 = (WSP*CM2K)
            E05O29 = E05O29 + 0.0000001
         ELSE
            E05O29 = BMISS
         END IF
         RETURN
      ENTRY E06O29(TMP)
C     ---> formerly ENTRY ONTMP
         ITMP = NINT(TMP*100.)
         ITZRO = NINT(TZRO*100.)
         IF(TMP.LT.BMISS) E06O29 = NINT((ITMP - ITZRO)*0.1)
         IF(TMP.GE.BMISS) E06O29 = BMISS
         RETURN
      ENTRY E07O29(DPD,TMP)
C     ---> formerly ENTRY ONDPD
         IF(DPD.LT.BMISS .AND. TMP.LT.BMISS) E07O29 = (TMP-DPD)*10.
         IF(DPD.GE.BMISS .OR.  TMP.GE.BMISS) E07O29 = BMISS
         RETURN
      ENTRY E08O29(HGT)
C     ---> formerly ENTRY ONHGT
         E08O29 = HGT
         IF(HGT.LT.BMISS) E08O29 = (HGT/GRAV)
         RETURN
      ENTRY E09O29(HVZ)
C     ---> formerly ENTRY ONHVZ
         IF(HVZ.GE.BMISS.OR.HVZ.LT.0.) THEN
            E09O29 = BMISS
         ELSE  IF(NINT(HVZ).LT.6000)  THEN
            E09O29 = MIN(INT(NINT(HVZ)/100),50)
         ELSE  IF(NINT(HVZ).LT.30000)  THEN
            E09O29 = INT(NINT(HVZ)/1000) + 50
         ELSE  IF(NINT(HVZ).LE.70000)  THEN
            E09O29 = INT(NINT(HVZ)/5000) + 74
         ELSE
            E09O29 = 89
         END IF
         RETURN
      ENTRY E10O29(PRW)
C     ---> formerly ENTRY ONPRW
         E10O29 = BMISS
         IF(PRW.LT.BMISS)  E10O29 = NINT(MOD(PRW,100.))
         RETURN
      ENTRY E11O29(PAW)
C     ---> formerly ENTRY ONPAW
         E11O29 = BMISS
         IF(PAW.LT.BMISS)  E11O29 = NINT(MOD(PAW,10.))
         RETURN
      ENTRY E12O29(CCN)
C     ---> formerly ENTRY ONCCN
         IF(NINT(CCN).EQ.0)  THEN
            E12O29 = 0
         ELSE  IF(CCN.LT. 15)  THEN
            E12O29 = 1
         ELSE  IF(CCN.LT. 35)  THEN
            E12O29 = 2
         ELSE  IF(CCN.LT. 45)  THEN
            E12O29 = 3
         ELSE  IF(CCN.LT. 55)  THEN
            E12O29 = 4
         ELSE  IF(CCN.LT. 65)  THEN
            E12O29 = 5
         ELSE  IF(CCN.LT. 85)  THEN
            E12O29 = 6
         ELSE  IF(CCN.LT.100)  THEN
            E12O29 = 7
         ELSE  IF(NINT(CCN).EQ.100)  THEN
            E12O29 = 8
         ELSE
            E12O29 = BMISS
         END IF
         RETURN
      ENTRY E13O29(CLA)
C     ---> formerly ENTRY ONCLA
         E13O29 = BMISS
         IF(CLA.EQ.0) E13O29 = 0
         IF(CLA.EQ.1) E13O29 = 5
         IF(CLA.EQ.2) E13O29 = 25
         IF(CLA.EQ.3) E13O29 = 40
         IF(CLA.EQ.4) E13O29 = 50
         IF(CLA.EQ.5) E13O29 = 60
         IF(CLA.EQ.6) E13O29 = 75
         IF(CLA.EQ.7) E13O29 = 95
         IF(CLA.EQ.8) E13O29 = 100
         RETURN
      ENTRY E14O29(CCL,CCM)
C     ---> formerly ENTRY ONCHN
         E14O29 = CCL
         IF(NINT(E14O29).EQ.0)  E14O29 = CCM
         IF(NINT(E14O29).LT.10)  RETURN
         IF(NINT(E14O29).EQ.10)  THEN
            E14O29 = 9.
         ELSE  IF(NINT(E14O29).EQ.15)  THEN
            E14O29 = 10.
         ELSE
            E14O29 = BMISS
         END IF
         RETURN
      ENTRY E15O29(CTLMH)
C     ---> formerly ENTRY ONCTL, ONCTM, ONCTH
         E15O29 = CTLMH
         RETURN
      ENTRY E18O29(CHL,CHM,CHH,CTL,CTM,CTH)
C     ---> formerly ENTRY ONHCB
         IF(NINT(MAX(CTL,CTM,CTH)).EQ.0)  THEN
            E18O29 = 9
            RETURN
         END IF
         E18O29 = BMISS
         IF(CHH.LT.BMISS) E18O29 = CHH
         IF(CHM.LT.BMISS) E18O29 = CHM
         IF(CHL.LT.BMISS) E18O29 = CHL
         IF(E18O29.GE.BMISS.OR.E18O29.LT.0)  RETURN
         IF(E18O29.LT. 150)  THEN
            E18O29 = 0
         ELSE  IF(E18O29.LT. 350)  THEN
            E18O29 = 1
         ELSE  IF(E18O29.LT. 650)  THEN
            E18O29 = 2
         ELSE  IF(E18O29.LT. 950)  THEN
            E18O29 = 3
         ELSE  IF(E18O29.LT.1950)  THEN
            E18O29 = 4
         ELSE  IF(E18O29.LT.3250)  THEN
            E18O29 = 5
         ELSE  IF(E18O29.LT.4950)  THEN
            E18O29 = 6
         ELSE  IF(E18O29.LT.6750)  THEN
            E18O29 = 7
         ELSE  IF(E18O29.LT.8250)  THEN
            E18O29 = 8
         ELSE
            E18O29 = 9
         END IF
         RETURN
      ENTRY E19O29(CPT)
C     ---> formerly ENTRY ONCPT
         E19O29 = BMISS
         IF(NINT(CPT).GT.-1.AND.NINT(CPT).LT.9)  E19O29 = CPT
         RETURN
      ENTRY E20O29(PRC)
C     ---> formerly ENTRY ONPRC
         E20O29 = PRC
         IF(PRC.LT.0.) THEN
            E20O29 = 9998
         ELSE  IF(PRC.LT.BMISS) THEN
            E20O29 = NINT(PRC*3.937)
         END IF
         RETURN
      ENTRY E21O29(SND)
C     ---> formerly ENTRY ONSND
         E21O29 = SND
         IF(SND.LT.0.) THEN
            E21O29 = 998
         ELSE  IF(SND.LT.BMISS) THEN
            E21O29 = NINT(SND*39.37)
         END IF
         RETURN
      ENTRY E22O29(PC6)
C     ---> formerly ENTRY ONDOP
         E22O29 = BMISS
         IF(PC6.LT.BMISS)  E22O29 = 1
         RETURN
      ENTRY E23O29(PER)
C     ---> formerly ENTRY ONPOW, ONSWP
         E23O29 = NINT(PER)
         RETURN
      ENTRY E24O29(HGT)
C     ---> formerly ENTRY ONHOW, ONSWH
         E24O29 = HGT
         IF(HGT.LT.BMISS) E24O29 = NINT(2.*HGT)
         RETURN
      ENTRY E25O29(SWD)
C     ---> formerly ENTRY ONSWD
         E25O29 = SWD
         IF(SWD.EQ.0)  THEN
            E25O29 = 0
         ELSE  IF(SWD.LT.5)  THEN
            E25O29 = 36
         ELSE  IF(SWD.LT.BMISS)  THEN
            E25O29 = NINT((SWD+.001)*.1)
         END IF
         RETURN
      ENTRY E28O29(SPG)
C     ---> formerly ENTRY ONSPG
         E28O29 = SPG
         RETURN
      ENTRY E29O29(SPD)
C     ---> formerly ENTRY ONSPD
         E29O29 = SPD
         RETURN
      ENTRY E30O29(SHC)
C     ---> formerly ENTRY ONSHC
         E30O29 = BMISS
         IF(NINT(SHC).GT.-1.AND.NINT(SHC).LT.9)  E30O29 = NINT(SHC)
         RETURN
      ENTRY E31O29(SAS)
C     ---> formerly ENTRY ONSAS
         E31O29 = BMISS
         IF(NINT(SAS).GT.-1.AND.NINT(SAS).LT.10)  E31O29 = NINT(SAS)
         RETURN
      ENTRY E32O29(WES)
C     ---> formerly ENTRY ONWES
         E32O29 = WES
         RETURN
      ENTRY E33O29(SUBSET,RPID)
C     ---> formerly ENTRY ONRTP
         E33O29 = BMISS
         IF(SUBSET(1:5).EQ.'NC000'.AND.L02O29(RPID) ) E33O29 = 511
         IF(SUBSET(1:5).EQ.'NC000'.AND.L03O29(RPID) ) E33O29 = 512
         IF(SUBSET.EQ.'NC001001'.AND.RPID.NE.'SHIP') E33O29 = 522
         IF(SUBSET.EQ.'NC001001'.AND.RPID.EQ.'SHIP') E33O29 = 523
         IF(SUBSET.EQ.'NC001002') E33O29 = 562
         IF(SUBSET.EQ.'NC001003') E33O29 = 561
         IF(SUBSET.EQ.'NC001004') E33O29 = 531
         IF(SUBSET.EQ.'NC001006') E33O29 = 551
         IF(SUBSET.EQ.'NC002001')  THEN

C  LAND RADIOSONDE - FIXED
C  -----------------------

            E33O29 = 011
            IF(L03O29(RPID)) E33O29 = 012
            IF(RPID(1:4).EQ.'CLAS') E33O29 = 013
         END IF
         IF(SUBSET.EQ.'NC002002')  THEN

C  LAND RADIOSONDE - MOBILE
C  ------------------------

            E33O29 = 013
         END IF
         IF(SUBSET.EQ.'NC002003')  THEN

C  SHIP RADIOSONDE
C  ---------------

            E33O29 = 022
            IF(RPID(1:4).EQ.'SHIP') E33O29 = 023
         END IF
         IF(SUBSET.EQ.'NC002004')  THEN

C  DROPWINSONDE
C  -------------

            E33O29 = 031
         END IF
         IF(SUBSET.EQ.'NC002005')  THEN

C  PIBAL
C  -----

            E33O29 = 011
            IF(L03O29(RPID)) E33O29 = 012
         END IF

         IF(SUBSET.EQ.'NC004001') E33O29 = 041
         IF(SUBSET.EQ.'NC004002') E33O29 = 041
         IF(SUBSET.EQ.'NC004003') E33O29 = 041
         IF(SUBSET.EQ.'NC004004') E33O29 = 041
         IF(SUBSET.EQ.'NC004005') E33O29 = 031
         IF(SUBSET(1:5).EQ.'NC005') E33O29 = 063
         RETURN
      ENTRY E34O29(HGT,Z100)
C     ---> formerly ENTRY ONFIX
C - With Jeff Ator's fix on 1/30/97, don't need this anymore
cdak     HGT0 = HGT
cdak     IF(MOD(NINT(HGT),300).EQ.0.OR.MOD(NINT(HGT),500).EQ.0)
cdak $    HGT = HGT * 1.016

C  ALL WINDS-BY-HEIGHT HEIGHTS ARE TRUNCATED DOWN TO THE NEXT
C   10 METER LEVEL IF PART DD (ABOVE 100 MB LEVEL) (ON29 CONVENTION)
C  -----------------------------------------------------------------

         IF(HGT.GT.Z100)  THEN
            IF(MOD(NINT(HGT),10).NE.0)  HGT = INT(HGT/10.) * 10
            E34O29 = NINT(HGT)
         ELSE
C - With Jeff Ator's fix on 1/30/97, don't need this anymore
cdak        IF(HGT.NE.HGT0)  THEN
cdak           IF(MOD(NINT(HGT0),1500).EQ.0)  HGT = HGT - 1.0
cdak        ELSE
               IF(MOD(NINT(HGT/1.016),1500).EQ.0) HGT = NINT(HGT - 1.0)
cdak        END IF
            E34O29 = INT(HGT)
         END IF
         RETURN
      ENTRY E38O29(HVZ)
         IF(HVZ.GE.BMISS.OR.HVZ.LT.0.) THEN
            E38O29 = BMISS
         ELSE  IF(NINT(HVZ).LT.1000)  THEN
            KK = MIN(INT(NINT(HVZ)/10),99)
            E38O29 = KKK(KK)
         ELSE  IF(NINT(HVZ).LT.50000)  THEN
            KK = MIN(INT(NINT(HVZ)/1000),49)
            E38O29 = KKKK(KK)
         ELSE
            E38O29 = 99
         END IF
         RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION C02O29()
C     ---> formerly FUNCTION ONCHR
      CHARACTER*8 C02O29,E35O29,E36O29
      CHARACTER*1  CPRT(0:11),CMR29(0:15)

      SAVE

C  (NOTE: Prior to mid-March 1999, a purge or reject flag on pressure
C         was set to 6 (instead of 14 or 12, resp.) to get around the
C         3-bit limit to ON29 pressure q.m. mnemonic "QMPR".  The 3-bit
C         limit on "QMPR" was changed to 4-bits with a decoder change
C         in February 1999.  However, the codes that write the q.m.'s
C         out (EDTBUFR and QUIPC) were not changed to write out 14 or
C         12 for purge or reject until mid-March 1999.  In order to
C         allow old runs to work properly, a q.m. of 6 will continue
C         to be interpreted as a "P".  This would have to change if
C         q.m.=6 ever has a defined meaning.)

C  Code Table Value:  0   1   2   3   4   5   6   7

      DATA CMR29    /'H','A',' ','Q','C','F','P','F',

C  Code Table Value:  8   9  10  11  12  13  14  15

     .               'F','F','O','B','R','F','P','F'/

      DATA CPRT  /' ',' ',' ',' ','A','B','C','D','I','J','K','L'/

      C02O29 = ' '
      RETURN
      ENTRY E35O29(QMK)
C     ---> formerly ENTRY ONQMK
         IF(QMK.GE.0 .AND. QMK.LE.15) E35O29 = CMR29(NINT(QMK))
         IF(QMK.LT.0 .OR.  QMK.GT.15) E35O29 = ' '
         RETURN
      ENTRY E36O29(NPRT)
C     ---> formerly ENTRY ONPRT
         E36O29 = '        '
         IF(NPRT.LT.12)  E36O29 = CPRT(NPRT)//'       '
         RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION L01O29()
C     ---> formerly FUNCTION ONLOG
      CHARACTER*8 RPID
      LOGICAL L01O29,L02O29,L03O29

      SAVE

      L01O29 = .TRUE.

      RETURN

      ENTRY L02O29(RPID)
C     ---> formerly ENTRY ONBKS
         L02O29 = .FALSE.
         READ(RPID,'(I5)',ERR=1) IBKS
         L02O29 = .TRUE.
1        RETURN
      ENTRY L03O29(RPID)
C     ---> formerly ENTRY ONCAL
         L03O29 = .TRUE.
         READ(RPID,'(I5)',ERR=2) IBKS
         L03O29 = .FALSE.
2        RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION R03O29(LUNIT,OBS)
C     ---> formerly FUNCTION ADPUPA

      COMMON/IO29DD/HDR(12),RCATS(50,150,11),IKAT(11),MCAT(11),NCAT(11)
      COMMON/IO29EE/POB(255),QOB(255),TOB(255),ZOB(255),DOB(255),
     $               SOB(255),VSG(255),CLP(255),CLA(255),OB8(255),
     $               CF8(255)
      COMMON/IO29FF/PQM(255),QQM(255),TQM(255),ZQM(255),WQM(255),
     $               QCP(255),QCA(255),Q81(255),Q82(255)
      COMMON/IO29CC/SUBSET,IDAT10
      COMMON/IO29BB/KNDX,KSKACF(8),KSKUPA,KSKSFC,KSKSAT,KSKSMI
      COMMON/IO29II/PWMIN
      COMMON/IO29LL/BMISS

      CHARACTER*80 HDSTR,LVSTR,QMSTR,RCSTR
      CHARACTER*8  SUBSET,SID,E35O29,E36O29,RSV,RSV2
      CHARACTER*1  PQM,QQM,TQM,ZQM,WQM,QCP,QCA,Q81,Q82,PQML
      REAL(8)  RID_8,HDR_8(12),VSG_8(255)
      REAL(8)  RCT_8(5,255),ARR_8(10,255)
      REAL(8)  RAT_8(255),RMORE_8(4),RGP10_8(255),RPMSL_8,RPSAL_8
      REAL(8)  BMISS
      INTEGER    IHBLCS(0:9)
      DIMENSION    OBS(*),RCT(5,255),ARR(10,255)
      DIMENSION    RAT(255),RMORE(4),RGP10(255)
      DIMENSION  P2(255),P8(255),P16(255)

      EQUIVALENCE  (RID_8,SID)
      LOGICAL      L02O29

      SAVE

      DATA HDSTR/'NULL CLON CLAT HOUR MINU SELV               '/
      DATA LVSTR/'PRLC TMDP TMDB GP07 GP10 WDIR WSPD          '/
      DATA QMSTR/'QMPR QMAT QMDD QMGP QMWN                    '/
      DATA RCSTR/'RCHR RCMI RCTS                              '/

      DATA IHBLCS/25,75,150,250,450,800,1250,1750,2250,2500/

      PRS1(Z) = 1013.25 * (((288.15 - (.0065 * Z))/288.15)**5.256)
      PRS2(Z) = 226.3 * EXP(1.576106E-4 * (11000. - Z))

C  CHECK IF THIS IS A PREPBUFR FILE
C  --------------------------------

      R03O29 = 99
c#V#V#dak - future
cdak  IF(SUBSET.EQ.'ADPUPA') R03O29 = PRPUPA(LUNIT,OBS)
caaaaadak - future
      IF(R03O29.NE.99) RETURN
      R03O29 = 0

      CALL S05O29

C  VERTICAL SIGNIFICANCE DESCRIPTOR TO ASSIGN ON29 CATEGORY
C  --------------------------------------------------------

C NOTE: MNEMONIC "VSIG" 008001 IS DEFINED AS VERTICAL SOUNDING
C       SIGNIFICANCE -- CODE TABLE FOLLOWS:
C        64   Surface
C              processed as ON29 category 2 and/or 3 and/or 4
C        32   Standard (mandatory) level
C              processed as ON29 category 1
C        16   Tropopause level
C              processed as ON29 category 5
C         8   Maximum wind level
C              processed as ON29 category 3 or 4
C         4   Significant level, temperature
C              processed as ON29 category 2
C         2   Significant level, wind
C              processed as ON29 category 3 or 4
C         1   ???????????????????????
C              processed as ON29 category 6
C
C  anything else - the level is not processed

      CALL UFBINT(LUNIT,VSG_8,1,255,NLEV,'VSIG');VSG=VSG_8

C  PUT THE HEADER INFORMATION INTO ON29 FORMAT
C  -------------------------------------------

      CALL UFBINT(LUNIT,HDR_8,12,  1,IRET,HDSTR);HDR(2:)=HDR_8(2:)
      IF(HDR(5).GE.BMISS) HDR(5) = 0
      CALL UFBINT(LUNIT,RID_8,1,1,IRET,'RPID')
      IF(IRET.NE.1) SID = 'MISSING '
cppppp-ID
      iprint = 0
c     if(sid.eq.'59758   ')  iprint = 1
c     if(sid.eq.'61094   ')  iprint = 1
c     if(sid.eq.'62414   ')  iprint = 1
c     if(sid.eq.'59362   ')  iprint = 1
c     if(sid.eq.'57957   ')  iprint = 1
c     if(sid.eq.'74794   ')  iprint = 1
c     if(sid.eq.'74389   ')  iprint = 1
c     if(sid.eq.'96801A  ')  iprint = 1
      if(iprint.eq.1)
     $ print'(" @@@ START DIAGNOSTIC PRINTOUT FOR ID ",A)', sid
cppppp-ID

      IRECCO = 0
      CALL UFBINT(LUNIT,RPMSL_8,1,  1,IRET,'PMSL');RPMSL=RPMSL_8
      IF(SUBSET.EQ.'NC004005')  THEN
         CALL UFBINT(LUNIT,RGP10_8,1,255,NLEV,'GP10');RGP10=RGP10_8
         CALL UFBINT(LUNIT,RPSAL_8,1,1,IRET,'PSAL');RPSAL=RPSAL_8
         IF(NINT(VSG(1)).EQ.32.AND.RPMSL.GE.BMISS.AND.
     $    MAX(RGP10(1),RPSAL).LT.BMISS)  THEN
cppppp
cdak        print'(" ~~IW3UNP29/R03O29: ID ",A," is a Cat. 1 type ",
cdak $       "Flight-level RECCO")', sid
cppppp
            IRECCO = 1
         ELSE  IF(MIN(VSG(1),RPMSL,RGP10(1)).GE.BMISS.AND.RPSAL.LT.
     $    BMISS)
     $    THEN
cppppp
cdak        print'(" ~~IW3UNP29/R03O29: ID ",A," is a Cat. 6 type ",
cdak $       "Flight-level RECCO (but reformatted into cat. 2/3)")', sid
cppppp
            IRECCO = 6
         ELSE  IF(MIN(VSG(1),RGP10(1)).GE.BMISS.AND.MAX(RPMSL,RPSAL)
     $    .LT.BMISS)  THEN
cppppp
cdak        print'(" ~~IW3UNP29/R03O29: ID ",A," is a Cat. 2/3 type ",
cdak $       "Flight-level RECCO with valid PMSL")', sid
cppppp
            IRECCO = 23
         ELSE
cppppp
            print'(" ~~IW3UNP29/R03O29: ID ",A," is currently an ",
     $       "unknown type of Flight-level RECCO - VSIG =",G0,
     $       "; PMSL =",G0,"; GP10 =",G0," -- SKIP IT for now")',
     $       sid,VSG(1),RPMSL,RGP10(1)
            R03O29 = -9999
            KSKUPA =KSKUPA + 1
            RETURN
cppppp
         END IF
      END IF

      XOB = HDR(2)
      YOB = HDR(3)
      RHR = BMISS
      IF(HDR(4).LT.BMISS)  RHR = NINT(HDR(4))+NINT(HDR(5))/60.
      RCH = BMISS
      RSV = '999     '
      ELV = HDR(6)
      IF(IRECCO.GT.0)  THEN
         RPSAL = RPSAL + SIGN(0.0000001,RPSAL)
         ELV = RPSAL
      END IF

      CALL UFBINT(LUNIT,RAT_8, 1,255,NLEV,'RATP');RAT=RAT_8
      ITP = MIN(99,NINT(RAT(1)))
      RTP = E33O29(SUBSET,SID)
      IF(ELV.GE.BMISS)  THEN
cppppp
         print'(" IW3UNP29/R03O29: ID ",A," has a missing elev, so ",
     $    "elevation set to ZERO")', sid
cppppp
         IF((RTP.GT.20.AND.RTP.LT.24).OR.SUBSET.EQ.'NC002004')  ELV = 0
      END IF
cdak  if(sid(5:5).eq.' ') print'(A)', sid
      IF(L02O29(SID).AND.SID(5:5).EQ.' ') SID = '0'//SID
      RSV2 = '        '
      CALL S01O29(SID,XOB,YOB,RHR,RCH,RSV,RSV2,ELV,ITP,RTP)

C  PUT THE LEVEL DATA INTO ON29 UNITS
C  ----------------------------------

      CALL UFBINT(LUNIT,ARR_8,10,255,NLEV,LVSTR);ARR=ARR_8

      PWMIN = 999999.
      JLV = 2
      IF(IRECCO.EQ.6)  JLV = 1
      IF(IRECCO.GT.0.AND.NLEV.EQ.1)  THEN
         VSG(JLV) = 4
         VSG(JLV+1) = 2
         QOB(JLV) = E07O29(ARR(2,1),ARR(3,1))
         TOB(JLV) = E06O29(ARR(3,1))
         ARR(2,1) = BMISS
         ARR(3,1) = BMISS
         DOB(JLV+1) = E04O29(ARR(6,1),ARR(7,1))
         SOB(JLV+1) = E05O29(ARR(6,1),ARR(7,1))
         IF(NINT(DOB(JLV+1)).EQ.0.AND.NINT(SOB(JLV+1)).GT.0)
     $    DOB(JLV+1) = 360.
         IF(NINT(DOB(JLV+1)).EQ.360.AND.NINT(SOB(JLV+1)).EQ.0)
     $    DOB(JLV+1) = 0.
         ARR(6,1) = BMISS
         ARR(7,1) = BMISS
         IF(IRECCO.EQ.23)  THEN
            VSG(1) = 64
            ARR(1,1) = RPMSL
         END IF
      END IF

      IF(IRECCO.EQ.6)  GO TO 4523

      DO L=1,NLEV
         POB(L) = E01O29(ARR(1,L))
         IF(NINT(ARR(1,L)).LE.0) THEN
            POB(L) =  BMISS
cppppp
            print'(" ~~@@IW3UNP29/R03O29: ID ",A," has a ZERO or ",
     $       "negative reported pressure that is reset to missing")',
     $       sid
cppppp
         END IF
         QOB(L) = E07O29(ARR(2,L),ARR(3,L))
         TOB(L) = E06O29(ARR(3,L))
         ZOB(L) = MIN(E08O29(ARR(4,L)),E08O29(ARR(5,L)))
cppppp
      if(iprint.eq.1)  then
         if(irecco.gt.0)  print'(" At lvl=",I0,"; orig. ZOB = ",G0)',
     $     L,zob(L)
      end if
cppppp
         IF(IRECCO.EQ.1)  THEN
            IF(MOD(NINT(ZOB(L)),10).NE.0)  ZOB(L) = INT(ZOB(L)/10.) * 10
            ZOB(L) = NINT(ZOB(L))
         ELSEIF(IRECCO.EQ.23)  THEN
            ZOB(L) = 0
         END IF
         DOB(L) = E04O29(ARR(6,L),ARR(7,L))
         SOB(L) = E05O29(ARR(6,L),ARR(7,L))
         IF(NINT(DOB(L)).EQ.0.AND.NINT(SOB(L)).GT.0)    DOB(L) = 360.
         IF(NINT(DOB(L)).EQ.360.AND.NINT(SOB(L)).EQ.0)  DOB(L) = 0.
cppppp
      if(iprint.eq.1)  then
         print'(" At lvl=",I0,"; VSG=",G0,"; POB = ",G0,"; QOB = ",G0,
     $    "; TOB = ",G0,"; ZOB = ",G0,"; DOB = ",G0,"; final SOB ",
     $    "(kts) = ",G0,"; origl SOB (mps) = ",G0)',
     $    L,vsg(L),pob(L),qob(L),tob(L),zob(L),dob(L),sob(L),arr(7,L)
      end if
cppppp
         IF(IRECCO.EQ.0.AND.MAX(POB(L),DOB(L),SOB(L)).LT.BMISS)
     $    PWMIN=MIN(PWMIN,POB(L))
      ENDDO

 4523 CONTINUE

      MLEV = NLEV

      CALL UFBINT(LUNIT,ARR_8,10,255,NLEV,QMSTR);ARR=ARR_8

      IF(IRECCO.GT.0.AND.MLEV.EQ.1)  THEN
         POB1 = BMISS
         IF(POB(1).LT.BMISS)  POB1 = POB(1) * 0.1
         TOB1 = BMISS
         IF(TOB(JLV).LT.BMISS)  TOB1 = (TOB(JLV) * 0.1) + 273.15
         RPS1 = RPSAL
         ZOB1 = ZOB(1)
         TQM1 = ARR(3,1)
         POB(JLV)=NINT(E37O29(POB1,TOB1,RPS1,ZOB1,TQM1)) * 10
         POB(JLV+1) = POB(JLV)
cppppp
         if(iprint.eq.1)  then
         do L=JLV,JLV+1
            print'(" At lvl=",I0,"; VSG=",G0,"; POB = ",G0,"; QOB = ",
     $       G0,"; TOB = ",G0,"; ZOB = ",G0,"; DOB = ",G0,"; SOB = ",
     $       G0)', L,vsg(L),pob(L),qob(L),tob(L),zob(L),dob(L),sob(L)
         enddo
         end if
cppppp
      END IF

      IF(IRECCO.GT.0.AND.NLEV.EQ.1)  THEN
         PQM(JLV) = 'E'
         PQM(JLV+1) = 'E'
         TQM(JLV) = E35O29(ARR(2,1))
         ARR(2,1) = BMISS
         QQM(JLV) = E35O29(ARR(3,1))
         ARR(3,1) = BMISS
         ARR(4,1) = 3
         WQM(JLV+1) = E35O29(ARR(5,1))
         ARR(5,1) = BMISS
      END IF

      IF(IRECCO.EQ.6)  GO TO 4524

      DO L=1,NLEV
      PQM(L) = E35O29(ARR(1,L))
      TQM(L) = E35O29(ARR(2,L))
      QQM(L) = E35O29(ARR(3,L))
      ZQM(L) = E35O29(ARR(4,L))
      WQM(L) = E35O29(ARR(5,L))
      ENDDO

 4524 CONTINUE

      IF(IRECCO.GT.0.AND.NLEV.EQ.1)  NLEV = JLV + 1

C  SURFACE DATA MUST GO FIRST
C  --------------------------

      CALL S02O29(2,0,*9999)
      CALL S02O29(3,0,*9999)
      CALL S02O29(4,0,*9999)

      INDX2  = 0
      INDX8  = 0
      INDX16 = 0
      P2  = BMISS
      P8  = BMISS
      P16 = BMISS

      DO L=1,NLEV
      IF(NINT(VSG(L)).EQ.64) THEN
cppppp
      if(iprint.eq.1)  then
         print'(" Lvl=",L," is a surface level")'
      end if
      if(iprint.eq.1.and.POB(L).LT.BMISS.AND.(TOB(L).LT.BMISS.OR.IRECCO
     $ .EQ.23))  then
         print'("  --> valid cat. 2 sfc. lvl ")'
      end if
cppppp
         IF(POB(L).LT.BMISS.AND.(TOB(L).LT.BMISS.OR.IRECCO.EQ.23))
     $    CALL SE01O29(2,L)
cppppp
      if(iprint.eq.1.and.POB(L).LT.BMISS.AND.(DOB(L).LT.BMISS.OR.IRECCO
     $ .EQ.23))  then
         print'("  --> valid cat. 3 sfc. lvl ")'
      end if
cppppp
         IF(POB(L).LT.BMISS.AND.(DOB(L).LT.BMISS.OR.IRECCO.EQ.23))
     $    CALL SE01O29(3,L)
         IF(ZOB(L).LT.BMISS.AND.DOB(L).LT.BMISS) THEN
cppppp
            if(iprint.eq.1)  print'("  --> valid cat. 4 sfc. lvl ")'
cppppp

C  CAT. 4 HEIGHT DOES NOT PASS ON A KEEP, PURGE, OR REJECT LIST Q.M.
C  -----------------------------------------------------------------

            ZQM(L) = ' '
            CALL SE01O29(4,L)
         END IF
         VSG(L) = 0
      ELSE  IF(NINT(VSG(L)).EQ.2)  THEN
         P2(L) = POB(L)
         INDX2 = L
         IF(INDX8.GT.0)  THEN
            DO II = 1,INDX8
               IF(POB(L).EQ.P8(II).AND.POB(L).LT.BMISS)  THEN
cppppp
                  if(iprint.eq.1)  then
                     print'("  ## This cat. 3 level, on lvl ",I0,
     $                " will have already been processed as a cat. 3 ",
     $                "MAX wind lvl (on lvl ",I0,") - skip this Cat. ",
     $                "3 lvl")', L,II
                  end if
cppppp
                  IF(MAX(SOB(II),DOB(II)).GE.BMISS)  THEN
                     SOB(II) = SOB(L)
                     DOB(II) = DOB(L)
cppppp
                     if(iprint.eq.1)  then
                        print'("  ...... also on lvl ",I0," - transfer",
     $                  " wind data to dupl. MAX wind lvl because its ",
     $                  "missing there")', L
                     end if
cppppp
                  END IF
                  VSG(L) = 0
                  GO TO 7732
               END IF
            ENDDO
         END IF
      ELSE  IF(NINT(VSG(L)).EQ.8)  THEN
         P8(L) = POB(L)
         INDX8 = L
         IF(INDX2.GT.0)  THEN
            DO II = 1,INDX2
               IF(POB(L).EQ.P2(II).AND.POB(L).LT.BMISS)  THEN
cppppp
                  if(iprint.eq.1)  then
                     print'("  ## This MAX wind level, on lvl ",I0,
     $                " will have already been processed as a cat. 3 ",
     $                "lvl (on lvl ",I0,") - skip this MAX wind lvl ",
     $                "but set"/6X,"cat. 3 lvl PQM to ""W""")', L,II
                  end if
cppppp
                  PQM(II) = 'W'
                  IF(POB(L).EQ.PWMIN)  PQM(II) = 'X'
                  IF(MAX(SOB(II),DOB(II)).GE.BMISS)  THEN
                     SOB(II) = SOB(L)
                     DOB(II) = DOB(L)
cppppp
                     if(iprint.eq.1)  then
                        print'("  ...... also on lvl ",I0," - transfer",
     $                  " wind data to dupl. cat. 3 lvl because its ",
     $                  "missing there")', L
                     end if
cppppp
                  END IF
                  VSG(L) = 0
                  GO TO 7732
               END IF
            ENDDO
         END IF
         IF(INDX8-1.GT.0)  THEN
            DO II = 1,INDX8-1
               IF(POB(L).EQ.P8(II).AND.POB(L).LT.BMISS)  THEN
cppppp
                  if(iprint.eq.1)  then
                     print'("  ## This cat. 3 MAX wind lvl, on lvl ",I0,
     $                " will have already been processed as a cat. 3 ",
     $                "MAX wind lvl (on lvl ",I0,") - skip this Cat. ",
     $                "3 MAX wind lvl")', L,II
                  end if
cppppp
                  IF(MAX(SOB(II),DOB(II)).GE.BMISS)  THEN
                     SOB(II) = SOB(L)
                     DOB(II) = DOB(L)
cppppp
                     if(iprint.eq.1)  then
                        print'("  ...... also on lvl ",I0," - transfer",
     $                  " wind data to dupl. MAX wind lvl because its ",
     $                  "missing there")', L
                     end if
cppppp
                  END IF
                  VSG(L) = 0
                  GO TO 7732
               END IF
            ENDDO
         END IF
      ELSE  IF(NINT(VSG(L)).EQ.16)  THEN
         INDX16 = INDX16 + 1
         P16(INDX16) = POB(L)
      END IF
 7732 CONTINUE
      ENDDO

C  TAKE CARE OF 925 MB NEXT
C  ------------------------

      DO L=1,NLEV
      IF(NINT(VSG(L)).EQ.32 .AND. NINT(POB(L)).EQ.9250) THEN
         CF8(L) = 925
         OB8(L) = ZOB(L)
         Q81(L) = ' '
         Q82(L) = ' '
         IF(TOB(L).LT.BMISS) CALL S02O29(2,L,*9999)
         IF(DOB(L).LT.BMISS) CALL S02O29(3,L,*9999)
         IF(OB8(L).LT.BMISS) CALL S02O29(8,L,*9999)
         VSG(L) = 0
      END IF
      ENDDO

C  REST OF THE DATA
C  ----------------

      Z100 = 16000
      DO L=1,NLEV
      IF(NINT(VSG(L)).EQ.32) THEN
         IF(MIN(DOB(L),ZOB(L),TOB(L)).GE.BMISS)  THEN
cppppp
            if(iprint.eq.1)  then
               print'("  ==> For lvl ",I0,"; VSG=32 & DOB,ZOB,TOB all ",
     $          "missing --> this level not processed")', L
            end if
            VSG(L) = 0
         ELSE  IF(MIN(ZOB(L),TOB(L)).LT.BMISS) THEN
cppppp
            if(iprint.eq.1)  then
               print'("  ==> For lvl ",I0,"; VSG=32 & one or both of ",
     $          "ZOB,TOB non-missing --> valid cat. 1 lvl")', L
            end if
cppppp
            CALL S02O29(1,L,*9999)
            IF(NINT(POB(L)).EQ.1000.AND.ZOB(L).LT.BMISS)  Z100 = ZOB(L)
            VSG(L) = 0
         END IF
      END IF
      ENDDO
      DO L=1,NLEV
      IF(NINT(VSG(L)).EQ.32) THEN
         IF(DOB(L).LT.BMISS.AND.MIN(ZOB(L),TOB(L)).GE.BMISS) THEN
            LL = I04O29(POB(L)*.1)
            IF(LL.EQ.999999)  THEN
cppppp
               print'(" ~~IW3UNP29/R03O29: ID ",A," has VSG=32 for ",
     $          "lvl ",I0," but pressure not mand.!! --> this level ",
     $          "not processed")', sid,L
cppppp
            ELSE  IF(MIN(RCATS(1,LL,1),RCATS(2,LL,1)).LT.99999.)  THEN
               IF(RCATS(4,LL,1).GE.99998.)  THEN
cppppp
                  if(iprint.eq.1)  then
                     print'("  ==> For lvl ",I0,"; VSG=32 & ZOB,TOB ",
     $                "both missing while DOB non-missing BUT one or ",
     $                "both of Z, T non-missing while wind missing ",
     $                "in"/7X,"earlier cat. 1 processing of this ",G0,
     $                "mb level --> valid cat. 1 lvl")', L,POB(L)*.1
                  end if
cppppp
                  CALL S02O29(1,L,*9999)
               ELSE
cppppp
                  if(iprint.eq.1)  then
                     print'("  ==> For lvl ",I0,"; VSG=32 & ZOB,TOB ",
     $                "both missing while DOB non-missing BUT one or ",
     $                "both of Z, T non-missing while wind non-missing",
     $                " in"/6X,"earlier cat. 1 processing of this ",G0,
     $                "mb level --> valid cat. 3 lvl")', L,POB(L)*.1
                  end if
cppppp
                  CALL S02O29(3,L,*9999)
               END IF
            ELSE
cppppp
               if(iprint.eq.1)  then
                  print'("  ==> For lvl ",I0,"; VSG=32 & ZOB,TOB both ",
     $             "missing while DOB non-missing AND both Z, T ",
     $             "missing on"/7X,"this ",G0,"mb level in cat. 1 --> ",
     $             "valid cat. 3 lvl")', L,POB(L)*.1
               end if
cppppp
               CALL S02O29(3,L,*9999)
            END IF
         ELSE
cppppp
            print'("  ~~IW3UNP29/R03O29: ID ",A," has VSG=32 for lvl ",
     $       I0," & should never come here!! - by default output",
     $       " as cat. 1 lvl")', sid,L
cppppp
            CALL S02O29(1,L,*9999)
         END IF
         VSG(L) = 0
      END IF
      ENDDO

      DO L=1,NLEV
      IF(NINT(VSG(L)).EQ. 4) THEN
cppppp
         if(iprint.eq.1)  then
            print'("   ==> For lvl ",I0,"; VSG= 4 --> valid cat. 2 ",
     $       "lvl")', L
         end if
cppppp
         IF(INDX16.GT.0)  THEN
            DO II = 1,INDX16
               IF(POB(L).EQ.P16(II).AND.POB(L).LT.BMISS)  THEN
cppppp
                  if(iprint.eq.1)  then
                     print'("  ## This cat. 2 level, on lvl ",I0," is",
     $                " also the tropopause level, as its pressure ",
     $                "matches that of trop. lvl no. ",I0," - ",
     $                "set this cat. 2"/5X,"lvl PQM to ""T""")', L,II
                  end if
cppppp
                  PQM(L) = 'T'
                  GO TO 7738
               END IF
            ENDDO
         END IF
 7738    CONTINUE
         CALL S02O29(2,L,*9999)
         VSG(L) = 0
      ELSEIF(NINT(VSG(L)).EQ.16) THEN
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG=16 --> valid cat. 3/5 ",
     $       "lvl")', L
         end if
cppppp
         PQML = PQM(L)
         IF(MIN(SOB(L),DOB(L)).LT.BMISS)  CALL S02O29(3,L,*9999)
         PQM(L) = PQML
         CALL S02O29(5,L,*9999)
         VSG(L) = 0
      ELSEIF(NINT(VSG(L)).EQ. 1) THEN
cppppp
         print'(" ~~IW3UNP29/R03O29: HERE IS A VSG =1, SET TO CAT.6, ",
     $    "AT ID ",A,"; SHOULD NEVER HAPPEN!!")', SID
cppppp
         CALL S02O29(6,L,*9999)
         VSG(L) = 0
      ELSEIF(NINT(VSG(L)).EQ. 2 .AND. POB(L).LT.BMISS) THEN
         IF(MAX(SOB(L),DOB(L)).LT.BMISS)  THEN
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG= 2 & POB .ne. missing ",
     $       "--> valid cat. 3 lvl (expect that ZOB is missing)")', L
         end if
cppppp
         CALL S02O29(3,L,*9999)
         ELSE
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG= 2 & POB .ne. missing ",
     $       "--> Cat. 3 level not processed - wind is missing")', L
         end if
cppppp
         END IF
         VSG(L) = 0
      ELSEIF(NINT(VSG(L)).EQ. 2 .AND. ZOB(L).LT.BMISS) THEN
         IF(MAX(SOB(L),DOB(L)).LT.BMISS)  THEN

C  CERTAIN U.S. WINDS-BY-HEIGHT ARE CORRECTED TO ON29 CONVENTION
C  -------------------------------------------------------------

         IF(SID(1:2).EQ.'70'.OR.SID(1:2).EQ.'71'.OR.SID(1:2).EQ.'72'
     $    .OR.SID(1:2).EQ.'74')  ZOB(L) = E34O29(ZOB(L),Z100)
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG= 2 & ZOB .ne. missing ",
     $       "--> valid cat. 4 lvl (POB must always be missing)")', L
            if(sid(1:2).eq.'70'.or.sid(1:2).eq.'71'.or.sid(1:2).eq.'72'
     $       .or.sid(1:2).eq.'74')  print'("    .... ZOB at this ",
     $       "U.S. site adjusted to ",G0)', zob(L)
         end if
cppppp

C  CAT. 4 HEIGHT DOES NOT PASS ON A KEEP, PURGE, OR REJECT LIST Q.M.
C  -----------------------------------------------------------------

         ZQM(L) = ' '

         CALL S02O29(4,L,*9999)
         ELSE
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG= 2 & ZOB .ne. missing ",
     $       "--> Cat. 4 level not processed - wind is missing")', L
         end if
cppppp
         END IF
         VSG(L) = 0
      ELSEIF(NINT(VSG(L)).EQ. 8 .AND. POB(L).LT.BMISS) THEN
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG= 8 & POB .ne. missing ",
     $       "--> valid cat. 3 lvl (expect that ZOB is missing)")', L
         end if
cppppp
         CALL S02O29(3,L,*9999)
         VSG(L) = 0
      ELSEIF(NINT(VSG(L)).EQ. 8 .AND. ZOB(L).LT.BMISS) THEN
         IF(MAX(SOB(L),DOB(L)).LT.BMISS)  THEN

C  CERTAIN U.S. WINDS-BY-HEIGHT ARE CORRECTED TO ON29 CONVENTION
C  -------------------------------------------------------------

         IF(SID(1:2).EQ.'70'.OR.SID(1:2).EQ.'71'.OR.SID(1:2).EQ.'72'
     $    .OR.SID(1:2).EQ.'74')  ZOB(L) = E34O29(ZOB(L),Z100)
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG= 8 & ZOB .ne. missing ",
     $       "--> valid cat. 4 lvl (POB must always be missing)")', L
            if(sid(1:2).eq.'70'.or.sid(1:2).eq.'71'.or.sid(1:2).eq.'72'
     $       .or.sid(1:2).eq.'74')  print'("    .... ZOB at this ",
     $       "U.S. site adjusted to ",G0)', zob(L)
         end if
cppppp

C  CAT. 4 HEIGHT DOES NOT PASS ON A KEEP, PURGE, OR REJECT LIST Q.M.
C  -----------------------------------------------------------------

         ZQM(L) = ' '

         CALL S02O29(4,L,*9999)
         ELSE
cppppp
         if(iprint.eq.1)  then
            print'("  ==> For lvl ",I0,"; VSG= 8 & ZOB .ne. missing ",
     $       "--> Cat. 4 level not processed - wind is missing")', L
         end if
cppppp
         END IF
         VSG(L) = 0
      END IF
      ENDDO

C  CHECK FOR LEVELS WHICH GOT LEFT OUT
C  -----------------------------------

      DO L=1,NLEV
      IF(NINT(VSG(L)).GT.0)  THEN
         PRINT 887, L,SID,NINT(VSG(L))
  887 FORMAT(' ##IW3UNP29/R03O29 - ~~ON LVL',I4,' OF ID ',A8,', A ',
     $    'VERTICAL SIGNIFICANCE OF',I3,' WAS NOT SUPPORTED - LEAVE ',
     $    'THIS LEVEL OUT OF THE PROCESSING')
         print'("  ..... at lvl=",I0,"; POB = ",G0,"; QOB = ",G0,
     $    "; TOB = ",G0,"; ZOB = ",G0,"; DOB = ",G0,";"/19X,"SOB = ",
     $    G0)', pob(L),qob(L),tob(L),zob(L),dob(L),sob(L)
      END IF
      ENDDO

C  CLOUD DATA GOES INTO CATEGORY 07
C  --------------------------------

      CALL UFBINT(LUNIT,ARR_8,10,255,NLEV,'HOCB CLAM QMCA HBLCS')
      ARR=ARR_8
      DO L=1,NLEV
         IF(ARR(1,L).LT.BMISS/2.)  THEN
               ! Prior to 3/2002 HBLCS was not available, this will
               !  always be tested first because it is more precise
               !  in theory but will now be missing after 3/2002
            IF(ELV+ARR(1,L).GE.BMISS/2.)  THEN
               CLP(L) =  BMISS
            ELSE  IF(ELV+ARR(1,L).LE.11000)  THEN
               CLP(L) = (PRS1(ELV+ARR(1,L))*10.)  + 0.001
            ELSE
               CLP(L) = (PRS2(ELV+ARR(1,L))*10.)  + 0.001
            END IF
         ELSE
               ! Effective 3/2002 only this will be available
            IF(NINT(ARR(4,L)).GE.10)  THEN
               CLP(L) =  BMISS
            ELSE
               IF(ELV+IHBLCS(NINT(ARR(4,L))).GE.BMISS/2.)  THEN
                  CLP(L) =  BMISS
               ELSE  IF(ELV+IHBLCS(NINT(ARR(4,L))).LE.11000)  THEN
                  CLP(L) = (PRS1(ELV+IHBLCS(NINT(ARR(4,L))))*10.) +0.001
               ELSE
                  CLP(L) = (PRS2(ELV+IHBLCS(NINT(ARR(4,L))))*10.) +0.001
               END IF
            END IF
         END IF
         CLA(L) = E13O29(ARR(2,L))
         QCP(L) = ' '
         QCA(L) = E35O29(ARR(3,L))
         IF(CLP(L).LT.BMISS .OR. CLA(L).LT.BMISS) CALL S02O29(7,L,*9999)
      ENDDO

C  -----------------------------------------------------
C  MISC DATA GOES INTO CATEGORY 08
C  -----------------------------------------------------
C  CODE FIGURE 104 - RELEASE TIME IN .01*HR
C  CODE FIGURE 105 - RECEIPT TIME IN .01*HR
C  CODE FIGURE 106 - RADIOSONDE INSTR. TYPE,
C                    SOLAR/IR CORRECTION INDICATOR,
C                    TRACKING TECH/STATUS OF SYSTEM USED
C  CODE FIGURE 925 - HEIGHT OF 925 LEVEL
C  -----------------------------------------------------

      CALL UFBINT(LUNIT,RCT_8, 5,255,NRCT,RCSTR);RCT=RCT_8

C NOTE: MNEMONIC "RCTS" 008202 IS A LOCAL DESCRIPTOR DEFINED AS
C       RECEIPT TIME SIGNIFICANCE -- CODE TABLE FOLLOWS:
C         0   General decoder receipt time
C         1   NCEP receipt time
C         2   OSO  receipt time
C         3   ARINC ground station receipt time
C         4   Radiosonde TEMP AA part receipt time
C         5   Radiosonde TEMP BB part receipt time
C         6   Radiosonde TEMP CC part receipt time
C         7   Radiosonde TEMP DD part receipt time
C         8   Radiosonde PILOT AA part receipt time
C         9   Radiosonde PILOT BB part receipt time
C        10   Radiosonde PILOT CC part receipt time
C        11   Radiosonde PILOT DD part receipt time
C      12-62  Reserved for future use
C        63   Missing

      DO L=1,NRCT
      CF8(L) = 105
      OB8(L) = NINT((NINT(RCT(1,L))+NINT(RCT(2,L))/60.) * 100.)
      IF(IRECCO.GT.0.AND.NINT(RCT(3,L)).EQ.0)  RCT(3,L) = 9
      Q81(L) = E36O29(NINT(RCT(3,L)))
      Q82(L) = ' '
      CALL S02O29(8,L,*9999)
      ENDDO

      CALL UFBINT(LUNIT,RMORE_8,4,1,NRMORE,'SIRC TTSS UALNHR UALNMN')
      RMORE=RMORE_8
      IF(MAX(RMORE(3),RMORE(4)).LT.BMISS)  THEN
         CF8(1) = 104
         OB8(1) = NINT((RMORE(3)+RMORE(4)/60.) * 100.)
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
      END IF
      IF(NINT(RAT(1)).LT.100)  THEN
         CF8(1) = 106
         ISIR = 9
         IF(NINT(RMORE(1)).LT.9)  ISIR = NINT(RMORE(1))
         ITEC = 99
         IF(NINT(RMORE(2)).LT.99)  ITEC = NINT(RMORE(2))
         OB8(1) = (ISIR * 10000) +  (NINT(RAT(1)) * 100) + ITEC
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
      END IF

C  PUT THE UNPACKED ON29 REPORT INTO OBS
C  -------------------------------------

      CALL S03O29(OBS,SUBSET,*9999,*9998)

      RETURN
 9999 CONTINUE
      R03O29 = 999
      RETURN
 9998 CONTINUE
      print'(" IW3UNP29/R03O29: RPT with ID= ",A," TOSSED - ZERO ",
     $ "CAT.1-6,51,52 LVLS")', SID
      R03O29 = -9999
      KSKUPA =KSKUPA + 1
      RETURN
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION R04O29(LUNIT,OBS)
C     ---> formerly FUNCTION SURFCE

      COMMON/IO29EE/POB(255),QOB(255),TOB(255),ZOB(255),DOB(255),
     $              SOB(255),VSG(255),CLP(255),CLA(255),OB8(255),
     $              CF8(255)
      COMMON/IO29FF/PQM(255),QQM(255),TQM(255),ZQM(255),WQM(255),
     $              QCP(255),QCA(255),Q81(255),Q82(255)
      COMMON/IO29GG/PSL,STP,SDR,SSP,STM,DPD,TMX,TMI,HVZ,PRW,PW1,CCN,CHN,
     $              CTL,CTM,CTH,HCB,CPT,APT,PC6,SND,P24,DOP,POW,HOW,SWD,
     $              SWP,SWH,SST,SPG,SPD,SHC,SAS,WES
      COMMON/IO29HH/PSQ,SPQ,SWQ,STQ,DDQ
      COMMON/IO29CC/SUBSET,IDAT10
      COMMON/IO29BB/KNDX,KSKACF(8),KSKUPA,KSKSFC,KSKSAT,KSKSMI
      COMMON/IO29LL/BMISS

      CHARACTER*80 HDSTR,RCSTR
      CHARACTER*8  SUBSET,SID,E35O29,RSV,RSV2
      CHARACTER*1  PQM,QQM,TQM,ZQM,WQM,QCP,QCA,Q81,Q82,PSQ,SPQ,SWQ,STQ,
     $ DDQ
      REAL(8) RID_8,UFBINT_8,BMISS
      REAL(8) HDR_8(20),RCT_8(5,255),RRSV_8(3),CLDS_8(4,255),
     $ TMXMNM_8(4,255)
      INTEGER ITIWM(0:15),IHBLCS(0:9)
      DIMENSION  OBS(*),HDR(20),RCT(5,255),RRSV(3),CLDS(4,255),JTH(0:9),
     $ JTL(0:9),LTL(0:9),TMXMNM(4,255)
      EQUIVALENCE  (RID_8,SID)

      SAVE

      DATA HDSTR/'RPID CLON CLAT HOUR MINU SELV AUTO          '/
      DATA RCSTR/'RCHR RCMI RCTS                              '/

      DATA JTH/0,1,2,3,4,5,6,8,7,9/,JTL/0,1,5,8,7,2,3,4,6,9/
      DATA LTL/0,1,5,6,7,2,8,4,3,9/
      DATA ITIWM/0,3*7,3,3*7,1,3*7,4,3*7/
      DATA IHBLCS/25,75,150,250,450,800,1250,1750,2250,2500/

C  CHECK IF THIS IS A PREPBUFR FILE
C  --------------------------------

      R04O29 = 99
c#V#V#dak - future
cdak  IF(SUBSET.EQ.'ADPSFC') R04O29 = PRPSFC(LUNIT,OBS)
cdak  IF(SUBSET.EQ.'SFCSHP') R04O29 = PRPSFC(LUNIT,OBS)
cdak  IF(SUBSET.EQ.'SFCBOG') R04O29 = PRPSFC(LUNIT,OBS)
caaaaadak - future
      IF(R04O29.NE.99) RETURN
      R04O29 = 0

      CALL S05O29

C  PUT THE HEADER INFORMATION INTO ON29 FORMAT
C  -------------------------------------------

      CALL UFBINT(LUNIT,HDR_8,20,  1,IRET,HDSTR);HDR(2:)=HDR_8(2:)
      CALL UFBINT(LUNIT,RCT_8, 5,255,NRCT,RCSTR);RCT=RCT_8
      IF(HDR(5).GE.BMISS) HDR(5) = 0
      RCTIM = NINT(RCT(1,1))+NINT(RCT(2,1))/60.
      RID_8 = HDR_8(1)
      XOB = HDR(2)
      YOB = HDR(3)
      RHR = BMISS
      IF(HDR(4).LT.BMISS)  RHR = NINT(HDR(4))+NINT(HDR(5))/60.
      RCH = RCTIM
      ELV = HDR(6)

C  I1 DEFINES SYNOPTIC FORMAT FLAG (SUBSET NC000001, NC000009)
C  I1 DEFINES AUTOMATED STATION TYPE (SUBSET NC000003-NC000008,NC000010)
C  I2 DEFINES CONVERTED HOURLY FLAG (SUBSET NC000xxx)
C  I2 DEFINES SHIP LOCATION FLAG (SUBSET NC001xxx) (WHERE xxx != 006)

      I1  = 9
      I2  = 9
      IF(SUBSET(1:5).EQ.'NC000')  THEN
         IF(SUBSET(6:8).EQ.'001'.OR.SUBSET(6:8).EQ.'009')  THEN
            I1 = 1
            IF(SUBSET(6:8).EQ.'009')  I2 = 1
         ELSE  IF(SUBSET(6:8).NE.'002')  THEN
            IF(HDR(7).LT.15)  THEN
               IF(HDR(7).GT.0.AND.HDR(7).LT.5) THEN
                  I1 = 2
               ELSE  IF(HDR(7).EQ.8) THEN
                  I1 = 3
               ELSE
                  I1 = 4
               END IF
            END IF
         END IF
      END IF
      ITP = (10 * I1) + I2
      RTP = E33O29(SUBSET,SID)

C  THE 25'TH (RESERVE) CHARACTER IS INDICATOR FOR PRECIP. (INCL./EXCL.)
C  THE 26'TH (RESERVE) CHARACTER IS INDICATOR FOR W SPEED (SOURCE/UNITS)
C         '0' - Wind speed estimated in m/s (uncertified instrument)
C         '1' - Wind speed obtained from anemometer in m/s (certified
C               instrument)
C         '3' - Wind speed estimated in knots (uncertified instrument)
C         '4' - Wind speed obtained from anemometer in knots (certified
C               instrument)
C         '7' - Missing
C  THE 27'TH (RESERVE) CHARACTER IS INDICATOR FOR STN OPER./PAST WX DATA

      CALL UFBINT(LUNIT,UFBINT_8,1,1,NRSV,'INPC');RRSV(1)=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,NRSV,'TIWM');TIWM=UFBINT_8
      IF(TIWM.LT.BMISS)  THEN   ! Effective 3/2002
         RRSV(2) = 7
         IF(NINT(TIWM).LE.15)  RRSV(2) = ITIWM(NINT(TIWM))
      ELSE                      ! Prior to 3/2002
         CALL UFBINT(LUNIT,UFBINT_8,1,1,NRSV,'SUWS');RRSV(2)=UFBINT_8
      END IF
      CALL UFBINT(LUNIT,UFBINT_8,1,1,NRSV,'ITSO');RRSV(3)=UFBINT_8
      RSV = '999     '
      DO  I=1,3
         IF(RRSV(I).LT.BMISS)  WRITE(RSV(I:I),'(I1)') NINT(RRSV(I))
      ENDDO

C  READ THE CATEGORY 51 SURFACE DATA FROM BUFR
C  -------------------------------------------

      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'PMSL');PSL=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'PRES');STP=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'WDIR');SDR=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'WSPD');SSP=UFBINT_8
      WSPD1 = SSP
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TMDB');STM=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TMDP');DPD=UFBINT_8
      IF(SUBSET.NE.'NC000007')  THEN
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'MXTM');TMX=UFBINT_8
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'MITM');TMI=UFBINT_8
      ELSE
         TMX = BMISS
         TMI = BMISS
      END IF
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'QMPR');QSL=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'QMPR');QSP=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'QMWN');QMW=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'QMAT');QMT=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'QMDD');QMD=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'HOVI');HVZ=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'PRWE');PRW=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'PSW1');PW1=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'PSW2');PW2=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TOCC');CCN=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'CHPT');CPT=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'3HPC');APT=UFBINT_8
      IF(MAX(APT,CPT).GE.BMISS) THEN
         APT = BMISS
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'24PC');APT24=UFBINT_8
         IF(APT24.LT.BMISS)  THEN
            APT = APT24
            CPT = BMISS
         END IF
      END IF


C  READ THE CATEGORY 52 SURFACE DATA FROM BUFR
C  -------------------------------------------

      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TP06');PC6=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TOSD');SND=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TP24');P24=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TOPC');PTO=UFBINT_8
      IF(PTO.LT.BMISS)  THEN
         IF(PC6.GE.BMISS.AND.NINT(DOP).EQ. 6)  PC6 = PTO
cppppp
         IF(PC6.GE.BMISS.AND.NINT(DOP).EQ. 6)
     $    print'(" ~~IW3UNP29/R04O29: PTO used for PC6 since latter ",
     $    "missing &  6-hr DOP")'
cppppp
         IF(P24.GE.BMISS.AND.NINT(DOP).EQ.24)  P24 = PTO
cppppp
         IF(P24.GE.BMISS.AND.NINT(DOP).EQ.24)
     $    print'(" ~~IW3UNP29/R04O29: PTO used for P24 since latter ",
     $    "missing & 24-hr DOP")'
cppppp
      END IF
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'POWW');POW=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'HOWW');HOW=UFBINT_8
      IF(SUBSET(1:5).EQ.'NC001')  THEN
         IF(SUBSET(6:8).NE.'006')  THEN
            IF(MIN(POW,HOW).GE.BMISS)  THEN
               CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'POWV');POW=UFBINT_8
               CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'HOWV');HOW=UFBINT_8
            END IF
         ELSE
C  PAOBS always have a missing elev, but we know they are at sea level
            ELV = 0
         END IF
      END IF
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'DOSW');SWD=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'POSW');SWP=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'HOSW');SWH=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'SST1');SST=UFBINT_8
      IF(SST.GE.BMISS) THEN
          CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'STMP');SST=UFBINT_8
      ENDIF
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'????');SPG=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'????');SPD=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TDMP');SHC=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'ASMP');SAS=UFBINT_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'????');WES=UFBINT_8
      I52FLG = 0
      IF(MIN(SND,P24,POW,HOW,SWD,SWP,SWH,SST,SPG,SPD,SHC,SAS,WES)
     $ .GE.BMISS.AND.(PC6.EQ.0..OR.PC6.GE.BMISS))  I52FLG= 1

C  SOME CLOUD DATA IS NEEDED FOR LOW, MIDDLE, AND HIGH CLOUDS IN CAT. 51
C  ---------------------------------------------------------------------

      CALL UFBINT(LUNIT,CLDS_8,4,255,NCLD,'VSSO CLAM CLTP HOCB')
      CLDS=CLDS_8
      CTH = -9999.
      CTM = -9999.
      CTL = -9999.
      CHH = BMISS
      CHM = BMISS
      CHL = BMISS
      IF(NCLD.EQ.0)  THEN
         CCM = BMISS
         CCL = BMISS
      ELSE
         CCM = 0.
         CCL = 0.
         DO L=1,NCLD
            VSS = CLDS(1,L)
            CAM = CLDS(2,L)
            CTP = CLDS(3,L)
            CHT = BMISS
            IF(CLDS(4,L).LT.BMISS)  THEN
                 ! Prior to 3/2002 HBLCS was not available, this will
                 !  always be tested first because it is more precise
                 !  and may still be available for some types after
                 !  3/2002
               CHT = CLDS(4,L)
            ELSE
                 ! Effective 3/2002 this will be available and can be
                 !  used for types where HOCB is not available - less
                 !  precise and only available on 1 level
               CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'HBLCS')
               HBLCS=UFBINT_8
               IF(NINT(HBLCS).LT.10) CHT = IHBLCS(NINT(HBLCS))
            END IF
            IF(CHT.LT.BMISS)  CHT = CHT * 3.2808
            IF(NINT(VSS).EQ.0)  THEN
               IF(NINT(CTP).GT.9.AND.NINT(CTP).LT.20)  THEN
                  ITH = MOD(NINT(CTP),10)
                  KTH = JTH(ITH)
                  CTH = MAX(KTH,NINT(CTH))
                  CHH = MIN(CHT,CHH)
               ELSE  IF(NINT(CTP).LT.30)  THEN
                  ITM = MOD(NINT(CTP),10)
                  CTM = MAX(ITM,NINT(CTM))
                  IF(ITM.EQ.0)  CAM = 0.
                  CCM = MAX(CAM,CCM)
                  CHM = MIN(CHT,CHM)
               ELSE  IF(NINT(CTP).LT.40)  THEN
                  ITL = MOD(NINT(CTP),10)
                  KTL = JTL(ITL)
                  CTL = MAX(KTL,NINT(CTL))
                  IF(ITL.EQ.0)  CAM = 0.
                  CCL = MAX(CAM,CCL)
                  CHL = MIN(CHT,CHL)
               ELSE  IF(NINT(CTP).EQ.59)  THEN
                  CTH = 10.
                  CTM = 10.
                  IF(CCM.EQ.0.)  CCM = 15.
                  CTL = 10.
                  IF(CCL.EQ.0.)  CCL = 15.
               ELSE  IF(NINT(CTP).EQ.60)  THEN
                  CTH = 10.
               ELSE  IF(NINT(CTP).EQ.61)  THEN
                  CTM = 10.
                  IF(CCM.EQ.0.)  CCM = 15.
               ELSE  IF(NINT(CTP).EQ.62)  THEN
                  CTL = 10.
                  IF(CCL.EQ.0.)  CCL = 15.
               END IF
            END IF
         ENDDO
      END IF
      IF(NINT(CTH).GT.-1.AND.NINT(CTH).LT.10)  THEN
         CTH = JTH(NINT(CTH))
      ELSE  IF(NINT(CTH).NE.10)  THEN
         CTH = BMISS
      END IF
      IF(NINT(CTM).LT.0.OR.NINT(CTM).GT.10)  THEN
         CTM = BMISS
         CCM = BMISS
      END IF
      IF(NINT(CTL).GT.-1.AND.NINT(CTL).LT.10)  THEN
         CTL = LTL(NINT(CTL))
      ELSE  IF(NINT(CTL).NE.10)  THEN
         CTL = BMISS
         CCL = BMISS
      END IF

C  CALL FUNCTIONS TO TRANSFORM TO ON29/124 UNITS
C  ---------------------------------------------

      PSL = E01O29(PSL)
      STP = E01O29(STP)
      SDR = E04O29(SDR,SSP)
      SSP = E05O29(SDR,SSP)
      IF(NINT(SDR).EQ.0)   SDR = 360.
      IF(SDR.GE.BMISS.AND.NINT(SSP).EQ.0)   SDR = 360.
      DPD = E07O29(DPD,STM)
      STM = E06O29(STM)
      TMX = E06O29(TMX)
      TMI = E06O29(TMI)

      PSQ = E35O29(QSL)
      SPQ = E35O29(QSP)
      SWQ = E35O29(QMW)
      STQ = E35O29(QMT)
      DDQ = E35O29(QMD)

C  ADJUST QUIPS QUALITY MARKERS TO REFLECT UNPACKED ON29 CONVENTION

      IF(SUBSET(1:5).EQ.'NC001'.AND.PSQ.EQ.'C')  STP = BMISS
      IF(PSL.GE.BMISS)  PSQ = ' '
      IF(STP.GE.BMISS)  SPQ = ' '
      IF(MAX(SDR,SSP).GE.BMISS)  SWQ = ' '
      IF(STM.GE.BMISS)  STQ = ' '

      IF(SUBSET(1:5).EQ.'NC000'.OR.SUBSET.EQ.'NC001004')  THEN
         HVZ = E09O29(HVZ)
      ELSE
         HVZ = E38O29(HVZ)
      END IF
      PRW = E10O29(PRW)
      PW1 = E11O29(PW1)
      PW2 = E11O29(PW2)
      IF(DDQ.NE.'P'.AND.DDQ.NE.'H'.AND.DDQ.NE.'C')  THEN
         DDQ = ' '
         IPW2 = NINT(PW2)
         IF(IPW2.GT.-1.AND.IPW2.LT.10)  WRITE(DDQ,'(I1)') IPW2
      END IF
      CCN = E12O29(CCN)
      CHN = E14O29(CCL,CCM)
      CTL = E15O29(CTL)
      CTM = E15O29(CTM)
      CTH = E15O29(CTH)
      HCB = E18O29(CHL,CHM,CHH,CTL,CTM,CTH)
      CPT = E19O29(CPT)
      APT = E01O29(APT)

      PC6 = E20O29(PC6)
      SND = E21O29(SND)
      P24 = E20O29(P24)
      DOP = E22O29(PC6)
      POW = E23O29(POW)
      HOW = E24O29(HOW)
      SWD = E25O29(SWD)
      SWP = E23O29(SWP)
      SWH = E24O29(SWH)
      SST = E06O29(SST)
      SPG = E28O29(SPG)
      SPD = E29O29(SPD)
      SHC = E30O29(SHC)
      SAS = E31O29(SAS)
      WES = E32O29(WES)

C  MAKE THE UNPACKED ON29/124 REPORT INTO OBS
C  ------------------------------------------

      RSV2 = '        '
      CALL S01O29(SID,XOB,YOB,RHR,RCH,RSV,RSV2,ELV,ITP,RTP)
      CALL S02O29(51,1,*9999)
      IF(I52FLG.EQ.0)  CALL S02O29(52,1,*9999)

C  ------------------------------------------------------------------
C  MISC DATA GOES INTO CATEGORY 08
C  ------------------------------------------------------------------
C  CODE FIGURE 020 - ALTIMETER SETTING IN 0.1*MB
C  CODE FIGURE 081 - CALENDAR DAY MAXIMUM TEMPERATURE
C  CODE FIGURE 082 - CALENDAR DAY MINIMUM TEMPERATURE
C  CODE FIGURE 083 - SIX HOUR MAXIMUM TEMPERATURE
C  CODE FIGURE 084 - SIX HOUR MINIMUM TEMPERATURE
C  CODE FIGURE 085 - PRECIPITATION OVER PAST HOUR IN 0.01*INCHES
C  CODE FIGURE 098 - DURATION OF SUNSHINE FOR CALENDAR DAY IN MINUTES
C  CODE FIGURE 924 - WIND SPEED IN 0.01*M/S
C  ------------------------------------------------------------------

      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'ALSE');ALS=UFBINT_8
      IF(ALS.LT.BMISS) THEN
         OB8(1) = E01O29(ALS)
         CF8(1) = 20
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
      END IF
      IF(SUBSET.EQ.'NC000007')  THEN
         CALL UFBINT(LUNIT,TMXMNM_8,4,255,NTXM,
     $    '.DTHMXTM MXTM .DTHMITM MITM');TMXMNM=TMXMNM_8
         IF(NTXM.GT.0)  THEN
            DO I = 1,NTXM
               DO J = 1,3,2
                  IF(NINT(TMXMNM(J,I)).EQ.24) THEN
                     IF(TMXMNM(J+1,I).LT.BMISS)  THEN
                        TMX = E06O29(TMXMNM(J+1,I))
                        IF(TMX.LT.0)  THEN
                           OB8(1) = 1000 + ABS(NINT(TMX))
                        ELSE
                           OB8(1) = NINT(TMX)
                        END IF
                        CF8(1) = 81 + INT(J/2)
                        Q81(1) = ' '
                        Q82(1) = ' '
                        CALL S02O29(8,1,*9999)
                     END IF
                  ELSE  IF(NINT(TMXMNM(J,I)).EQ.6) THEN
                     IF(TMXMNM(J+1,I).LT.BMISS)  THEN
                        TMX = E06O29(TMXMNM(J+1,I))
                        IF(TMX.LT.0)  THEN
                           OB8(1) = 1000 + ABS(NINT(TMX))
                        ELSE
                           OB8(1) = NINT(TMX)
                        END IF
                        CF8(1) = 83 + INT(J/2)
                        Q81(1) = ' '
                        Q82(1) = ' '
                        CALL S02O29(8,1,*9999)
                     END IF
                  END IF
               ENDDO
            ENDDO
         END IF
      END IF
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TP01');PC1=UFBINT_8
      IF(PC1.LT.10000) THEN
         OB8(1) = E20O29(PC1)
         CF8(1) = 85
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
      END IF
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TOSS');DUS=UFBINT_8
      IF(NINT(DUS).LT.1000) THEN
         OB8(1) = NINT(98000. + DUS)
         CF8(1) = 98
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
      END IF
      IF(WSPD1.LT.BMISS) THEN
         OB8(1) = NINT(WSPD1*10.)
         CF8(1) = 924
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
      END IF

      CALL S03O29(OBS,SUBSET,*9999,*9998)

      RETURN

 9999 CONTINUE
      R04O29 = 999
      RETURN

 9998 CONTINUE
      print'(" IW3UNP29/R04O29: RPT with ID= ",A," TOSSED - ZERO ",
     $ "CAT.1-6,51,52 LVLS")', SID
      R04O29 = -9999
      KSKSFC =KSKSFC + 1
      RETURN

      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION R05O29(LUNIT,OBS)
C     ---> formerly FUNCTION AIRCFT

      COMMON/IO29EE/POB(255),QOB(255),TOB(255),ZOB(255),DOB(255),
     $               SOB(255),VSG(255),CLP(255),CLA(255),OB8(255),
     $               CF8(255)
      COMMON/IO29FF/PQM(255),QQM(255),TQM(255),ZQM(255),WQM(255),
     $               QCP(255),QCA(255),Q81(255),Q82(255)
      COMMON/IO29CC/SUBSET,IDAT10
      COMMON/IO29BB/KNDX,KSKACF(8),KSKUPA,KSKSFC,KSKSAT,KSKSMI
      COMMON/IO29LL/BMISS

      CHARACTER*80 HDSTR,LVSTR,QMSTR,RCSTR,CRAWR
      CHARACTER*8 SUBSET,SID,SIDO,SIDMOD,E35O29,RSV,RSV2,CCL,CRAW(1,255)
      CHARACTER*1  PQM,QQM,TQM,ZQM,WQM,QCP,QCA,Q81,Q82,CTURB(0:14)
      REAL(8) RID_8,RCL_8,UFBINT_8,RNS_8,BMISS
      REAL(8) HDR_8(20),RCT_8(5,255),ARR_8(10,255),RAW_8(1,255)
      DIMENSION    OBS(*),HDR(20),RCT(5,255),ARR(10,255),RAW(1,255)
      EQUIVALENCE  (RID_8,SID),(RCL_8,CCL),(RAW_8,CRAW)

      SAVE

      DATA HDSTR/'RPID CLON CLAT HOUR MINU SECO               '/
      DATA LVSTR/'PRLC TMDP TMDB WDIR WSPD                    '/
      DATA QMSTR/'QMPR QMAT QMDD QMGP QMWN                    '/
      DATA RCSTR/'RCHR RCMI RCTS                              '/

      DATA CTURB/'0','1','2','3','0','1','2','3','0','1','2',4*'3'/

C  CHECK IF THIS IS A PREPBUFR FILE
C  --------------------------------

      R05O29 = 99
c#V#V#dak - future
cdak  IF(SUBSET.EQ.'AIRCFT') R05O29 = PRPCFT(LUNIT,OBS)
cdak  IF(SUBSET.EQ.'AIRCAR') R05O29 = PRPCFT(LUNIT,OBS)
caaaaadak - future
      IF(R05O29.NE.99) RETURN
      R05O29 = 0

      CALL S05O29

C  PUT THE HEADER INFORMATION INTO ON29 FORMAT
C  -------------------------------------------

      CALL UFBINT(LUNIT,HDR_8,20,  1,IRET,HDSTR);HDR(2:)=HDR_8(2:)
      IF(IRET.EQ.0)  SID = '        '
      CALL UFBINT(LUNIT,RCT_8, 5,255,NRCT,RCSTR);RCT=RCT_8
      IF(HDR(5).GE.BMISS) HDR(5) = 0
      IF(HDR(6).GE.BMISS) HDR(6) = 0
      RCTIM = NINT(RCT(1,1))+NINT(RCT(2,1))/60.
      RID_8 = HDR_8(1)
      XOB = HDR(2)
      YOB = HDR(3)
      RHR = BMISS
      IF(HDR(4).LT.BMISS) RHR = NINT(HDR(4)) + ((NINT(HDR(5)) * 60.) +
     $ NINT(HDR(6)))/3600.
      RCH = RCTIM

C  TRY TO FIND FIND THE FLIGHT LEVEL HEIGHT
C  ----------------------------------------

      CALL UFBINT(LUNIT,HDR_8,20,1,IRET,'PSAL FLVL IALT HMSL PRLC')
      HDR=HDR_8
      ELEV = BMISS
      IF(HDR(5).LT.BMISS) ELEV = E03O29(HDR(5)*.01)
      IF(HDR(4).LT.BMISS) ELEV = HDR(4)
C FOR MDCARS ACARS DATA ONLY:
C  UNCOMMENTING NEXT LINE WILL SET P-ALT TO REPORTED "IALT" VALUE --
C    IN THIS CASE, PREPDATA WILL LATER GET PRESS. VIA STD. ATMOS. FCN.
C  COMMENTING NEXT LINE WILL USE REPORTED PRESSURE "PRLC" TO GET
C    P-ALT VIA INVERSE STD. ATMOS. FCN. -- IN THIS CASE, PREPDATA WILL
C    LATER RETURN THIS SAME PRESS. VIA STD. ATMOS. FCN.
cdak  IF(HDR(3).LT.BMISS) ELEV = HDR(3)
      IF(HDR(2).LT.BMISS)  ELEV = HDR(2) + SIGN(0.0000001,HDR(2))
      IF(HDR(1).LT.BMISS)  ELEV = HDR(1) + SIGN(0.0000001,HDR(1))
      ELV = ELEV

C  ACFT NAVIGATION SYSTEM STORED IN INSTR. TYPE LOCATION (AS WITH ON29)
C  --------------------------------------------------------------------

      ITP = 99
      CALL UFBINT(LUNIT,RNS_8,1,1,IRET,'ACNS');RNS=RNS_8
      IF(RNS.LT.BMISS)  THEN
         IF(NINT(RNS).EQ.0)  THEN
            ITP = 97
         ELSE  IF(NINT(RNS).EQ.1)  THEN
            ITP = 98
         END IF
      END IF

      RTP = E33O29(SUBSET,SID)

      CALL UFBINT(LUNIT,RCL_8,1,1,IRET,'BORG')    ! Effective 3/2002
      IF(IRET.EQ.0) THEN
         CCL = '        '
         CALL UFBINT(LUNIT,RCL_8,1,1,IRET,'ICLI') ! Prior to  3/2002
         IF(IRET.EQ.0)  CCL = '        '
      END IF
cvvvvv temporary?
      IF(CCL(1:4).EQ.'KAWN')  THEN

C This will toss all Carswell/Tinker Aircraft reports - until Jack
C  fixes the dup-check to properly remove the duplicate Carswell
C  reports, we are better off removing them all since they are
C  often of less quality than the non-Carswell AIREP reports
C  RIGHT NOW WE ARE HAPPY WITH DUP-CHECKER'S HANDLING OF THESE,
C   SO COMMENT THIS OUT

cdak     R05O29 = -9999
cdak     KSKACF(?) = KSKACF(?) + 1
cdak     RETURN
      END IF
caaaaa temporary?
      IF(SUBSET.EQ.'NC004003')  THEN

C  ------------------------------------
C  ASDAR/AMDAR AIRCRAFT TYPE COME HERE
C  ------------------------------------

cvvvvv temporary?
C  Currently, we throw out any ASDAR/AMDAR reports with header "LFPW" -
C   simply because they never appeared in NAS9000 ON29 AIRCFT data set
C   (NOTE: These should all have ACID's that begin with "IT")
C   (NOTE: These will not be removed from the new decoders, because
C          they are apparently unique reports of reasonable
C          quality.  EMC just needs to test them in a parallel run
C          to make sure prepacqc and the analysis handle them okay.)

C NOTE: NO, NO DON'T THROW THEM OUT ANY MORE !!!!!!
C        Keyser -- 6/13/97

CDAKCDAK if(ccl(1:4).eq.'LFPW')  then
cppppp
cdak  print'(" IW3UNP29/R05O29: TOSS ""LFPW"" AMDAR with ID = ",A,
cdak $ "; CCL = ",A)', SID,CCL(1:4)
cppppp
CDAKCDAK    R05O29 = -9999
CDAKCDAK    kskacf(2) = kskacf(2) + 1
CDAKCDAK    return
CDAKCDAK end if
caaaaa temporary?

C  MODIFY REPORT ID AS WAS DONE IN OLD ON29 AIRCRAFT PACKER
C  --------------------------------------------------------

         CALL S06O29(SID,SIDMOD)
         SIDO = SID
         SID = SIDMOD

C  THE 25'TH (RESERVE) CHARACTER INDICATES PHASE OF FLIGHT
C  THE 26'TH (RESERVE) CHARACTER INDICATES TEMPERATURE PRECISION
C  THE 27'TH (RESERVE) CHARACTER INDICATES CARSWELL (NEVER HAPPENS)
C   (NOTE: NAS9000 ONLY ASSIGNED HEADER "KAWN" AS CARSWELL, ALTHOUGH
C          "PHWR" AND "EGWR" ARE ALSO APPARENTLY ALSO CARSWELL)

         RSV = '71      '
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'POAF');POF=UFBINT_8
         IF(POF.LT.BMISS)  WRITE(RSV(1:1),'(I1)') NINT(POF)
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'PCAT');PCT=UFBINT_8
         IF(NINT(PCT).GT.1)  RSV(2:2) = '0'
         IF(CCL(1:4).EQ.'KAWN')  RSV(3:3) = 'C'

      ELSE IF(SUBSET.EQ.'NC004004') THEN

C  ------------------------------
C  ACARS AIRCRAFT TYPE COME HERE
C  ------------------------------

         CALL UFBINT(LUNIT,RID_8,1,1,IRET,'ACRN')
         IF(IRET.EQ.0)  SID = 'ACARS   '
         KNDX = KNDX + 1
         RSV = '999     '

      ELSE IF(SUBSET.EQ.'NC004001'.OR.SUBSET.EQ.'NC004002') THEN

C  -----------------------------------------
C  AIREP AND PIREP AIRCRAFT TYPES COME HERE
C  -----------------------------------------

C  MAY POSSIBLY NEED TO MODIFY THE RPID HERE
C  -----------------------------------------

         IF(SID(6:6).EQ.'Z')  SID(6:6) = 'X'
         IF(SID.EQ.'A       '.OR.SID.EQ.'        '.OR.SID(1:3).EQ.'ARP'
     $    .OR.SID(1:3).EQ.'ARS')  SID = 'AIRCFT  '

cvvvvv temporary?
C  Determined that Hickum AFB reports are much like Carswell - they have
C   problems!  They also are usually duplicates of either Carswell or
C   non-Carswell reports.  Apparently the front-end processing filters
C   them out (according to B. Ballish).  So, to make things match,
C   we will do the same here.
C   ACTUALLY, JEFF ATOR HAS REMOVED THESE FROM THE DECODER, SO WE
C    SHOULD NEVER EVEN SEE THEM IN THE DATABASE, but it won't hurt
C    anything to keep this in here.
C   (NOTE: These all have headers of "PHWR")

         if(ccl(1:4).eq.'PHWR')  then
cppppp
cdak  print'(" IW3UNP29/R05O29: TOSS ""PHWR"" AIREP with ID = ",A,
cdak $ "; CCL = ",A)', SID,CCL(1:4)
cppppp
            R05O29 = -9999
            kskacf(8) = kskacf(8) + 1
            return
         end if
caaaaa temporary?

cvvvvv temporary?
C        1) Carswell/Tinker AMDARS are processed as AIREP subtypes.
C      Nearly all of them are duplicated as true non-Carswell AMDARS in
C      the AMDAR subtype.  The earlier version of the aircraft dup-
C      checker could not remove such duplicates; the new verison now
C      in operations can remove these. SO, WE HAVE COMMENTED THIS OUT.
C
C      The Carswell AMDARS can be identified by the string " Sxyz" in
C      the raw report (beyond byte 40), where y is 0,1, or 2.
C      (NOTE: Apparently Carswell here applies to more headers than
C             just "KAWN", so report header is not even checked.)

C        2) Carswell/Tinker ACARS are processed as AIREP subtypes.
C      These MAY duplicate true non-Carswell ACARS in the ACARS
C      subtype.  The NAS9000 decoder always excluded this type (no
C      dup-checking was done).  All of these will be removed here.
C      The Carswell ACARS can be identified by the string " Sxyz" in
C      the raw report (beyond byte 40), where y is 3 or greater.
C      (NOTE: Apparently Carswell here applies to more headers than
C             just "KAWN", so report header is not even checked.)

         call ufbint(lunit,raw_8,1,255,nlev,'RRSTG');raw=raw_8
         if(nlev.gt.5)  then
            ni = -7
            do mm = 6,nlev
               ni = ni + 8
               crawr(ni:ni+7) = craw(1,mm)
               if(ni+8.gt.80)  go to 556
            enddo
  556       continue
            do mm = 1,ni+7
               if(crawr(mm:mm+1).eq.' S')  then
                  if((crawr(mm+2:mm+2).ge.'0'.and.crawr(mm+2:mm+2).le.
     $             '9').or.crawr(mm+2:mm+2).eq.'/')  then
                     if((crawr(mm+3:mm+3).ge.'0'.and.crawr(mm+3:mm+3)
     $                .le.'9').or.crawr(mm+3:mm+3).eq.'/')  then
                        if((crawr(mm+4:mm+4).ge.'0'.and.
     $                   crawr(mm+4:mm+4).le.'9').or.crawr(mm+4:mm+4)
     $                   .eq.'/')  then
cppppp
cdak  print'(" IW3UNP29/R05O29: For ",A,", raw_8(",I0,") = ",A)',
cdak $ SID,ni+7,crawr(1:ni+7)
cppppp
                           if(crawr(mm+3:mm+3).lt.'3')  then

C  THIS IS A CARSWELL/TINKER AMDAR REPORT --> THROW OUT
C   (NOT ANYMORE, DUP-CHECKER IS HANDLING THESE OKAY NOW)
C  ----------------------------------------------------

cppppp
cdak  print'(" IW3UNP29/R05O29: Found a Carswell AMDAR for ",A,
cdak $ "; CCL = ",A)', SID,CCL(1:4)
cppppp
cdak  R05O29 = -9999
cdak  KSKACF(3) = KSKACF(3) + 1
cdak  RETURN
                           else

C  THIS IS A CARSWELL/TINKER ACARS REPORT --> THROW OUT
C  ----------------------------------------------------

cppppp
cdak  print'(" IW3UNP29/R05O29: Found a Carswell ACARS for ",A,
cdak $ "; CCL = ",A)', SID,CCL(1:4)
cppppp
      R05O29 = -9999
      KSKACF(4) = KSKACF(4) + 1
      RETURN

                           end if
                        end if
                     end if
                  end iF
               end if
               if(mm+5.gt.ni+7)  go to 557
            enddo
  557       continue
         END IF
caaaaa temporary?

C  THE 25'TH (RESERVE) CHARACTER INDICATES 8'TH CHARACTER OF STATION ID
C  THE 26'TH (RESERVE) CHARACTER INDICATES 7'TH CHARACTER OF STATION ID
C  THE 27'TH (RESERVE) CHARACTER INDICATES CARSWELL
C   (NOTE: NAS9000 ONLY ASSIGNED HEADER "KAWN" AS CARSWELL, ALTHOUGH
C          "PHWR" AND "EGWR" ARE ALSO APPARENTLY ALSO CARSWELL)

         RSV = SID(8:8)//SID(7:7)//'      '
         IF(CCL(1:4).EQ.'KAWN')  RSV(3:3) = 'C'

      END IF

C  -----------------------------
C  ALL AIRCRAFT TYPES COME HERE
C  -----------------------------

      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'DGOT');DGT=UFBINT_8

C  PUT THE LEVEL DATA INTO ON29 UNITS
C  ----------------------------------

      CALL UFBINT(LUNIT,ARR_8,10,255,NLEV,LVSTR);ARR=ARR_8
      DO L=1,NLEV

Cvvvvv temporary?
C  Even though PREPDATA filters out any aircraft reports with a missing
C   wind, or AIREP/PIREP and AMDAR reports below 100 and 2286 meters,
C   respectively, it will be done here for now in order to help in
C   the comparison between counts coming from the Cray dumps and the
C   NAS9000 ON29 dumps (the NAS9000 ON29 maker filters these out).

C  NO, NO LET'S NOT FILTER HERE ANY MORE - LEAVE IT UP TO PREPDATA
C   SINCE WE AREN'T COMPARING NAS9000 AND CRAY COUNTS ANY MORE
C     Keyser -- 6/13/97

CDAKCDAK if(arr(4,1).ge.bmiss.or.arr(5,1).ge.bmiss)  then
CDAKCDAK    R05O29 = -9999
CDAKCDAK    kskacf(5) = kskacf(5) + 1
CDAKCDAK    return
CDAKCDAK end if
CDAKCDAK if(subset.eq.'NC004003'.and.elev.lt.2286.)  then
CDAKCDAK    R05O29 = -9999
CDAKCDAK    kskacf(6) = kskacf(6) + 1
CDAKCDAK    return
CDAKCDAK else if(subset.ne.'NC004004'.and.elev.lt.100.)  then
CDAKCDAK    R05O29 = -9999
CDAKCDAK    kskacf(7) = kskacf(7) + 1
CDAKCDAK    return
CDAKCDAK end if
caaaaa temporary?

      POB(L) = E01O29(ARR(1,L))
      QOB(L) = E07O29(ARR(2,L),ARR(3,L))
      TOB(L) = E06O29(ARR(3,L))
      ZOB(L) = ELEV
      DOB(L) = E04O29(ARR(4,L),ARR(5,L))
      SOB(L) = E05O29(ARR(4,L),ARR(5,L))
      ENDDO
      WSPD1 = ARR(5,1)

      CALL UFBINT(LUNIT,ARR_8,10,255,NLEV,QMSTR);ARR=ARR_8

      IF(SUBSET.EQ.'NC004004') THEN

C  ---------------------------------------------------------
C  ACARS AIRCRAFT TYPE COME HERE FOR QUALITY MARK ASSIGNMENT
C  ---------------------------------------------------------

         DO L=1,NLEV
            PQM(L) = E35O29(ARR(1,L))
            TQM(L) = E35O29(ARR(2,L))
            QQM(L) = E35O29(ARR(3,L))
            ZQM(L) = E35O29(ARR(4,L))
            WQM(L) = E35O29(ARR(5,L))
         ENDDO

C  DEFAULT Q.MARK FOR WIND: "A"
C  ----------------------------

         IF(NLEV.EQ.0.OR.ARR(5,1).GE.BMISS)  WQM(1) = 'A'

      ELSE

C  --------------------------------------------------------------
C  ALL OTHER AIRCRAFT TYPES COME HERE FOR QUALITY MARK ASSIGNMENT
C  --------------------------------------------------------------

         DO L=1,NLEV
            ARR(4,L) =  2

C  IF KEEP  FLAG ON WIND, ENTIRE REPORT GETS KEEP  FLAG ('H' IN ZQM)
C   -- unless....
C  IF PURGE FLAG ON WIND, ENTIRE REPORT GETS PURGE FLAG ('P' IN ZQM)
C  IF PURGE FLAG ON TEMP, ENTIRE REPORT GETS PURGE FLAG ('P' IN ZQM)
C  IF FAIL  FLAG ON WIND, ENTIRE REPORT GETS FAIL  FLAG ('F' IN ZQM)
C  IF FAIL  FLAG ON TEMP, ENTIRE REPORT GETS FAIL  FLAG ('F' IN ZQM)
C  -----------------------------------------------------------------

            IF(ARR(5,L).EQ.0.AND.(ARR(2,L).LT.10.OR.ARR(2,L).GT.15))THEN
               ARR(4,L) =  0
            ELSE  IF(ARR(5,L).EQ.14.OR.ARR(2,L).EQ.14)  THEN
               ARR(4,L) = 14
            ELSE  IF(ARR(5,L).EQ.13.OR.ARR(2,L).EQ.13)  THEN
               ARR(4,L) = 13
            END IF
            PQM(L) = ' '
            TQM(L) = ' '
            QQM(L) = ' '
            ZQM(L) = E35O29(ARR(4,L))

C  DEGREE OF TURBULENCE IS STORED IN MOISTURE Q.M. SLOT
C  ----------------------------------------------------

            IF(NINT(DGT).LT.15)  QQM(L) = CTURB(NINT(DGT))
         ENDDO

C  DEFAULT Q.MARK FOR WIND: "C"
C  ----------------------------

         WQM(1) = 'C'
      END IF

C  PUT THE UNPACKED ON29 REPORT INTO OBS
C  -------------------------------------

      RSV2 = '        '
      CALL S01O29(SID,XOB,YOB,RHR,RCH,RSV,RSV2,ELV,ITP,RTP)
      CALL S02O29(6,1,*9999)

C  ------------------------------------------------------------------
C  MISC DATA GOES INTO CATEGORY 08
C  ------------------------------------------------------------------
C  CODE FIGURE 021 - REPORT SEQUENCE NUMBER
C  CODE FIGURE 917 - CHARACTERS 1 AND 2 OF ACTUAL STATION IDENTIFICATION
C                     (CURRENTLY ONLY FOR ASDAR/AMDAR)
C  CODE FIGURE 918 - CHARACTERS 3 AND 4 OF ACTUAL STATION IDENTIFICATION
C                     (CURRENTLY ONLY FOR ASDAR/AMDAR)
C  CODE FIGURE 919 - CHARACTERS 5 AND 6 OF ACTUAL STATION IDENTIFICATION
C                     (CURRENTLY ONLY FOR ASDAR/AMDAR)
C  CODE FIGURE 920 - CHARACTERS 7 AND 8 OF ACTUAL STATION IDENTIFICATION
C                     (CURRENTLY ONLY FOR ASDAR/AMDAR AND ACARS)
C  CODE FIGURE 921 - OBSERVATION TIME TO NEAREST 1000'TH OF AN HOUR
C                     (CURRENTLY ONLY FOR ACARS)
C  CODE FIGURE 922 - FIRST TWO CHARACTERS OF BULLETIN BEING MONITORED
C  CODE FIGURE 923 - LAST  TWO CHARACTERS OF BULLETIN BEING MONITORED
C  CODE FIGURE 924 - WIND SPEED IN 0.01*M/S
C  ------------------------------------------------------------------

      IF(SUBSET.EQ.'NC004004') THEN
         OB8(1) = KNDX
         CF8(1) = 21
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
         OB8(1) = 99999.
         Q81(1) = SID(7:7)
         Q82(1) = SID(8:8)
         CF8(1) = 920
         CALL S02O29(8,1,*9999)
         IF(RHR.LT.BMISS)  THEN
            OB8(1) = NINT((RHR*1000.)+0.0000001)
            CF8(1) = 921
            Q81(1) = ' '
            Q82(1) = ' '
            CALL S02O29(8,1,*9999)
         END IF
      ELSE  IF(SUBSET.EQ.'NC004003')  THEN
         DO  KKK = 1,4
            OB8(KKK) = 99999.
            Q81(KKK) = SIDO(2*KKK-1:2*KKK-1)
            Q82(KKK) = SIDO(2*KKK:2*KKK)
            CF8(KKK) = 916 + KKK
            CALL S02O29(8,KKK,*9999)
         ENDDO
      END IF
      IF(CCL.NE.'        ') THEN
         OB8(2) = 99999.
         Q81(2) = CCL(1:1)
         Q82(2) = CCL(2:2)
         CF8(2) = 922
         CALL S02O29(8,2,*9999)
         OB8(3) = 99999.
         Q81(3) = CCL(3:3)
         Q82(3) = CCL(4:4)
         CF8(3) = 923
         CALL S02O29(8,3,*9999)
      END IF
      IF(WSPD1.LT.BMISS) THEN
         OB8(4) = NINT(WSPD1*10.)
         CF8(4) = 924
         Q81(4) = ' '
         Q82(4) = ' '
         CALL S02O29(8,4,*9999)
      END IF

      CALL S03O29(OBS,SUBSET,*9999,*9998)

      RETURN

 9999 CONTINUE
      R05O29 = 999
      RETURN

 9998 CONTINUE
      print'(" IW3UNP29/R05O29: RPT with ID= ",A," TOSSED - ZERO ",
     $ "CAT.1-6,51,52 LVLS")', SID
      R05O29 = -9999
      KSKACF(1) = KSKACF(1) + 1
      RETURN

      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION R06O29(LUNIT,OBS)
C     ---> formerly FUNCTION SATWND

      COMMON/IO29EE/POB(255),QOB(255),TOB(255),ZOB(255),DOB(255),
     $               SOB(255),VSG(255),CLP(255),CLA(255),OB8(255),
     $               CF8(255)
      COMMON/IO29FF/PQM(255),QQM(255),TQM(255),ZQM(255),WQM(255),
     $               QCP(255),QCA(255),Q81(255),Q82(255)
      COMMON/IO29CC/SUBSET,IDAT10
      COMMON/IO29BB/KNDX,KSKACF(8),KSKUPA,KSKSFC,KSKSAT,KSKSMI
      COMMON/IO29KK/KOUNT(499,18)
      COMMON/IO29LL/BMISS

      CHARACTER*80 HDSTR,LVSTR,QMSTR,RCSTR
      CHARACTER*8  SUBSET,SID,E35O29,RSV,RSV2
      CHARACTER*3  CINDX3
      CHARACTER*1  PQM,QQM,TQM,ZQM,WQM,QCP,QCA,Q81,Q82,CSAT(499),
     $ CPRD(9),CINDX7,C7(26),CPROD(0:4),CPRDF(3)
      INTEGER      IPRDF(3)
      REAL(8) RID_8,UFBINT_8,BMISS
      REAL(8) HDR_8(20),RCT_8(5,255),ARR_8(10,255)
      DIMENSION    OBS(*),HDR(20),RCT(5,255),ARR(10,255)
      EQUIVALENCE  (RID_8,SID)

      SAVE

      DATA HDSTR/'RPID CLON CLAT HOUR MINU SAID               '/
      DATA LVSTR/'PRLC TMDP TMDB WDIR WSPD                    '/
      DATA QMSTR/'QMPR QMAT QMDD QMGP SWQM                    '/
      DATA RCSTR/'RCHR RCMI RCTS                              '/

      DATA CSAT  /'A','B','C','D',45*'?','Z','W','X','Y','Z','W','X',
     $ 'Y','Z','W',90*'?','R','O','P','Q','R','O','P','Q','R','O',
     $ 339*'?','V'/
      DATA CPROD /'C','D','?','?','E'/
      DATA CPRDF /'C','B','V'/
      DATA IPRDF / 1 , 6 , 4 /
      DATA CPRD  /'C','V','I','W','P','T','L','Z','G'/
      DATA C7    /'A','B','C','D','E','F','G','H','I','J','K','L','M',
     $            'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/

C  CHECK IF THIS IS A PREPBUFR FILE
C  --------------------------------

      R06O29 = 99
c#V#V#dak - future
cdak  IF(SUBSET.EQ.'SATWND') R06O29 = PRPWND(LUNIT,OBS)
caaaaadak - future
      IF(R06O29.NE.99) RETURN
      R06O29 = 0

      CALL S05O29

C  TRY TO FIND FIND THE HEIGHT ASSIGNMENT
C  --------------------------------------

      CALL UFBINT(LUNIT,HDR_8,20,1,IRET,'HGHT PRLC');HDR=HDR_8
      ELEV = BMISS
      IF(HDR(2).LT.BMISS) ELEV = E03O29(HDR(2)*.01)
      IF(HDR(1).LT.BMISS) ELEV = HDR(1)

C  PUT THE HEADER INFORMATION INTO ON29 FORMAT
C  -------------------------------------------

      CALL UFBINT(LUNIT,HDR_8,20,  1,IRET,HDSTR);HDR(2:)=HDR_8(2:)
      CALL UFBINT(LUNIT,RCT_8, 5,255,NRCT,RCSTR);RCT=RCT_8
      IF(HDR(5).GE.BMISS) HDR(5) = 0
      RCTIM = NINT(RCT(1,1))+NINT(RCT(2,1))/60.
      RID_8 = HDR_8(1)
      XOB = HDR(2)
      YOB = HDR(3)
      RHR = BMISS
      IF(HDR(4).LT.BMISS)  RHR = NINT(HDR(4))+NINT(HDR(5))/60.
      RCH = RCTIM
      RSV = '990     '

C  THE 25'TH (RESERVE) CHARACTER IS THE CLOUD MASK/DEEP LAYER INDICATOR
C   {=2 - CLOUD TOP (NORMAL CLOUD DRIFT), =1 - DEEP LAYER,
C    =9 - INDICATOR MISSING, THUS REVERTS TO DEFAULT CLOUD TOP}
C   (=9 FOR ALL BUT U.S. HIGH-DENSITY SATWND TYPES)
C  --------------------------------------------------------------------

C  THE 27'TH (RESERVE) CHARACTER INDICATES THE PRODUCER OF THE SATWND
C  ------------------------------------------------------------------

C  THE INSTRUMENT TYPE INDICATES THE PRODUCT TYPE
C  ----------------------------------------------

      ITP = 99

C  REPROCESS THE STN. ID
C  ---------------------

C    REPROCESSED CHAR 1 -----> GOES: BUFR CHAR 1
C                       -----> METEOSAT: SAT. NO.  52, 56     GET 'X'
C                                        SAT. NO.  53, 57     GET 'Y'
C                                        SAT. NO.  50, 54, 58 GET 'Z'
C                                        SAT. NO.  51, 55, 59 GET 'W'
C                       -----> GMS(JA):  SAT. NO. 152,156     GET 'P'
C                                        SAT. NO. 153,157     GET 'Q'
C                                        SAT. NO. 150,154,158 GET 'R'
C                                        SAT. NO. 151,155,159 GET 'O'
C                       -----> INSAT:    SAT. NO. 499         GET 'V'
C    REPROCESSED CHAR 2 -----> GOES: RETURNED VALUE IN BUFR FOR 'SWPR'
C                               (PRODUCER)
C                       -----> OTHERS: SAT. PRODUCER -- ESA   GET 'C'
C                                                    -- GMS   GET 'D'
C                                                    -- INSAT GET 'E'
C    REPROCESSED CHAR 6 -----> GOES: BUFR CHAR 6
C                       -----> OTHERS -- INFRA-RED CLOUD DRIFT GET 'C'
C                                     -- VISIBLE   CLOUD DRIFT GET 'B'
C                                     -- WATER VAPOR           GET 'V'
C    REPROCESSED CHAR 3-5 ---> SEQUENTIAL SERIAL INDEX (001 - 999)
C                              (UNIQUE FOR EACH BUFR CHAR 1/6 COMB.)
C    REPROCESSED CHAR 7 -----> GROUP NUMBER FOR SERIAL INDEX IN
C                              REPROCESSED CHAR 3-5 (0 - 9, A - Z)
C    REPROCESSED CHAR 8 -----> ALWAYS BLANK (' ') FOR NOW

      READ(SUBSET(8:8),'(I1)') INUM
      IF(SID(1:1).GE.'A'.AND.SID(1:1).LE.'D')  THEN
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'SWPR');SWPR=UFBINT_8
         IF(NINT(SWPR).GT.0.AND.NINT(SWPR).LT.10)
     $    WRITE(RSV(3:3),'(I1)') NINT(SWPR)
         SID(2:2) = RSV(3:3)
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'SWTP');SWTP=UFBINT_8
         IF(SWTP.LT.BMISS) ITP = NINT(SWTP)
         CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'SWDL');SWDL=UFBINT_8
         IF(NINT(SWDL).GT.-1.AND.NINT(SWDL).LT.10)
     $    WRITE(RSV(1:1),'(I1)') NINT(SWDL)
      ELSE
         SID = '????????'
         IF(NINT(HDR(6)).LT.500)  THEN
            SID(1:1) = CSAT(NINT(HDR(6)))
            SID(2:2) = CPROD(NINT(HDR(6))/100)
            RSV(3:3) = SID(2:2)
         END IF
         IF(INUM.LT.4)  THEN
            SID(6:6) = CPRDF(INUM)
            ITP = IPRDF(INUM)
         END IF
      END IF
      CINDX3 = '???'
      CINDX7 = '?'
      IF(NINT(HDR(6)).LT.500.AND.ITP.LT.19)  THEN
         KOUNT(NINT(HDR(6)),ITP) = MIN(KOUNT(NINT(HDR(6)),ITP)+1,35999)
         KOUNT3 = MOD(KOUNT(NINT(HDR(6)),ITP),1000)
         KOUNT7 = INT(KOUNT(NINT(HDR(6)),ITP)/1000)
         WRITE(CINDX3,'(I3.3)')  KOUNT3
         IF(KOUNT7.LT.10)  THEN
            WRITE(CINDX7,'(I1.1)')  KOUNT7
         ELSE
            CINDX7 = C7(KOUNT7-9)
         END IF
      END IF
      SID = SID(1:2)//CINDX3//SID(6:6)//CINDX7//' '

      ELV = ELEV
      RTP = E33O29(SUBSET,SID)

C  PUT THE LEVEL DATA INTO ON29 UNITS
C  ----------------------------------

      CALL UFBINT(LUNIT,ARR_8,10,255,NLEV,LVSTR);ARR=ARR_8
      DO L=1,NLEV
         POB(L) = E01O29(ARR(1,L))

C  GROSS CHECK ON PRESSURE
C  -----------------------

         IF(NINT(POB(L)).EQ.0)  THEN
            print'(" ~~IW3UNP29/R06O29: RPT with ID= ",A," TOSSED - ",
     $       "PRES. IS ZERO MB")', SID
            R06O29 = -9999
            KSKSAT = KSKSAT + 1
            RETURN
         END IF

         QOB(L) = E07O29(ARR(2,L),ARR(3,L))
         TOB(L) = E06O29(ARR(3,L))
         ZOB(L) = ELEV
         DOB(L) = E04O29(ARR(4,L),ARR(5,L))
         SOB(L) = E05O29(ARR(4,L),ARR(5,L))
      ENDDO
      WSPD1 = ARR(5,1)

C  DETERMINE QUALITY MARKERS
C  -------------------------

      CALL UFBINT(LUNIT,ARR_8,10,255,NLEV,QMSTR);ARR=ARR_8
      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'RFFL');RFFL=UFBINT_8
      IF(RFFL.LT.BMISS.AND.(NINT(ARR(5,1)).EQ.2.OR.NINT(ARR(5,1)).GE.
     $ BMISS))  THEN
         IF(NINT(RFFL).GT.84)  THEN
            ARR(5,1) = 1
         ELSE  IF(NINT(RFFL).GT.55)  THEN
            ARR(5,1) = 2
         ELSE  IF(NINT(RFFL).GT.49)  THEN
            ARR(5,1) = 3
         ELSE
            ARR(5,1) = 13
         END IF
      END IF

      DO L=1,NLEV
         WQM(L) = E35O29(ARR(5,L))

         IF(WQM(L).EQ.'R'.OR.WQM(L).EQ.'P'.OR.WQM(L).EQ.'F')  THEN

C  A REJECT, PURGE, OR FAIL FLAG ON WIND IS TRANSFERRED TO ALL VARIABLES
C  ---------------------------------------------------------------------

            PQM(L) = WQM(L)
            TQM(L) = WQM(L)
            QQM(L) = WQM(L)
            ZQM(L) = WQM(L)

         ELSE

            PQM(L) = E35O29(ARR(1,L))
            TQM(L) = E35O29(ARR(2,L))
            QQM(L) = E35O29(ARR(3,L))
            ZQM(L) = E35O29(ARR(4,L))

         END IF

      ENDDO

C  PUT THE UNPACKED ON29 REPORT INTO OBS
C  -------------------------------------

      RSV2 = '        '
      CALL S01O29(SID,XOB,YOB,RHR,RCH,RSV,RSV2,ELV,ITP,RTP)
      CALL S02O29(6,1,*9999)

C  ---------------------------------------------------------------------
C  MISC DATA GOES INTO CATEGORY 08
C  ---------------------------------------------------------------------
C  CODE FIGURE 013 - PRESSURE
C  CODE FIGURE 920 - CHARACTERS 7 AND 8 OF ACTUAL STATION IDENTIFICATION
C                     (CURRENTLY ONLY APPLIES TO U.S. SATWND TYPES)
C  CODE FIGURE 924 - WIND SPEED IN 0.01*M/S
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------

      IF(POB(1).LT.BMISS) THEN
         OB8(1) = NINT(POB(1)*0.1)
         CF8(1) = 13
         Q81(1) = ' '
         Q82(1) = ' '
         CALL S02O29(8,1,*9999)
      END IF
      IF(SID(1:1).GE.'A'.AND.SID(1:1).LE.'D')  THEN
         OB8(1) = 99999.
         Q81(1) = SID(7:7)
         Q82(1) = SID(8:8)
         CF8(1) = 920
         CALL S02O29(8,1,*9999)
      END IF
      IF(WSPD1.LT.BMISS) THEN
         OB8(2) = NINT(WSPD1*10.)
         CF8(2) = 924
         Q81(2) = ' '
         Q82(2) = ' '
         CALL S02O29(8,2,*9999)
      END IF

      CALL S03O29(OBS,SUBSET,*9999,*9998)

      RETURN

 9999 CONTINUE
      R06O29 = 999
      RETURN

 9998 CONTINUE
      print'(" IW3UNP29/R06O29: RPT with ID= ",A," TOSSED - ZERO ",
     $ "CAT.1-6,51,52 LVLS")', SID
      R06O29 = -9999
      KSKSAT =KSKSAT + 1
      RETURN

      END
C***********************************************************************
C***********************************************************************
C***********************************************************************
      FUNCTION R07O29(LUNIT,OBS)
C     ---> formerly FUNCTION SPSSMI

      COMMON/IO29EE/POB(255),QOB(255),TOB(255),ZOB(255),DOB(255),
     $              SOB(255),VSG(255),CLP(255),CLA(255),OB8(255),
     $              CF8(255)
      COMMON/IO29FF/PQM(255),QQM(255),TQM(255),ZQM(255),WQM(255),
     $              QCP(255),QCA(255),Q81(255),Q82(255)
      COMMON/IO29CC/SUBSET,IDAT10
      COMMON/IO29BB/KNDX,KSKACF(8),KSKUPA,KSKSFC,KSKSAT,KSKSMI
      COMMON/IO29LL/BMISS

      CHARACTER*80 HDSTR
      CHARACTER*8  SUBSET,SID,RSV,RSV2
      CHARACTER*4  CSTDV
      CHARACTER*1  PQM,QQM,TQM,ZQM,WQM,QCP,QCA,Q81,Q82,CRF
      REAL(8) RID_8,UFBINT_8,HDR_8(20),TMBR_8(7),ADDP_8(5),PROD_8(2,2)
      REAL(8) BMISS
      DIMENSION    OBS(*),HDR(20),ADDP(5),PROD(2,2),TMBR(7)

      EQUIVALENCE  (RID_8,SID)

      SAVE

      DATA HDSTR/'RPID CLON CLAT HOUR MINU SECO NMCT SAID     '/

C  CHECK IF THIS IS A PREPBUFR FILE
C  --------------------------------

      R07O29 = 99
c#V#V#dak - future
cdak  IF(SUBSET.EQ.'SPSSMI') R07O29 = PRPSMI(LUNIT,OBS)
caaaaadak - future
      IF(R07O29.NE.99) RETURN
      R07O29 = 0

      CALL S05O29

C  PUT THE HEADER INFORMATION INTO ON29 FORMAT
C  -------------------------------------------

      CALL UFBINT(LUNIT,HDR_8,20,  1,IRET,HDSTR);HDR(2:)=HDR_8(2:)
      IF(HDR(5).GE.BMISS) HDR(5) = 0
      IF(HDR(6).GE.BMISS) HDR(6) = 0
      RID_8 = HDR_8(1)
      XOB = HDR(2)
      YOB = HDR(3)
      RHR = BMISS
      IF(HDR(4).LT.BMISS) RHR = NINT(HDR(4)) + ((NINT(HDR(5)) * 60.) +
     $ NINT(HDR(6)))/3600.
      RCH = 99999.
      ELV = 99999.
      ITP = 99
      RTP = HDR(7)

C  CHECK ON VALUE FOR SATELLITE ID TO DETERMINE IF THIS IS A SUPEROB
C   (SATELLITE ID IS MISSING FOR SUPEROBS)
C  -----------------------------------------------------------------

      ISUPOB = 1
      IF(HDR(8).LT.BMISS)  ISUPOB = 0

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      STDV = BMISS

C  PUT THE SSM/I DATA INTO ON29 UNITS (WILL RETURN TO HEADER DATA LATER)
C    ALL PROCESSING GOES INTO CATEGORY 08
C  ---------------------------------------------------------------------

      IF(RTP.EQ.68)  THEN
C  ---------------------------------------------------------------------
C          ** 7-CHANNEL BRIGHTNESS TEMPERATURES -- REPORT TYPE 68 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 189 - 19 GHZ V BRIGHTNESS TEMPERATURE (DEG. K X 100)
C  CODE FIGURE 190 - 19 GHZ H BRIGHTNESS TEMPERATURE (DEG. K X 100)
C  CODE FIGURE 191 - 22 GHZ V BRIGHTNESS TEMPERATURE (DEG. K X 100)
C  CODE FIGURE 192 - 37 GHZ V BRIGHTNESS TEMPERATURE (DEG. K X 100)
C  CODE FIGURE 193 - 37 GHZ H BRIGHTNESS TEMPERATURE (DEG. K X 100)
C  CODE FIGURE 194 - 85 GHZ V BRIGHTNESS TEMPERATURE (DEG. K X 100)
C  CODE FIGURE 195 - 85 GHZ H BRIGHTNESS TEMPERATURE (DEG. K X 100)
C  ---------------------------------------------------------------------
         NLCAT8 = 7
         CALL UFBINT(LUNIT,TMBR_8,1,7,NLEV,'TMBR');TMBR=TMBR_8
         DO  NCHN = 1,7
            OB8(NCHN) = MIN(NINT(TMBR(NCHN)*100.),99999)
            CF8(NCHN) = 188 + NCHN
         ENDDO
      ELSE  IF(RTP.EQ.575)  THEN
C  ---------------------------------------------------------------------
C          ** ADDITIONAL PRODUCTS -- REPORT TYPE 575 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 210 - SURFACE TAG (RANGE: 0,1,3-6)
C  CODE FIGURE 211 - ICE CONCENTRATION (PERCENT)
C  CODE FIGURE 212 - ICE AGE (RANGE: 0,1)
C  CODE FIGURE 213 - ICE EDGE (RANGE: 0,1)
C  CODE FIGURE 214 - CALCULATED SURFACE TYPE (RANGE: 1-20)
C  ---------------------------------------------------------------------
         NLCAT8 = 5
         CALL UFBINT(LUNIT,ADDP_8,5,1,IRET,'SFTG ICON ICAG ICED SFTP')
         ADDP=ADDP_8
         DO NADD = 1,5
            IF(ADDP(NADD).LT.BMISS) THEN
               OB8(NADD) = NINT(ADDP(NADD))
               CF8(NADD) = 209 + NADD
            END IF
         ENDDO
      ELSE  IF(RTP.EQ.571)  THEN
C  ---------------------------------------------------------------------
C          ** OCEAN SURFACE WIND SPEED PRODUCT -- REPORT TYPE 571 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 196 - OCEANIC WIND SPEED (M/S * 10)
C                    (RAIN FLAG IN Q.M. BYTE 2)
C  ---------------------------------------------------------------------
         CF8(1) = 196
         ELV = 0
         NLCAT8 = 1
         IF(ISUPOB.EQ.1)  THEN
            CALL UFBREP(LUNIT,PROD_8,2,2,IRET,'FOST WSOS');PROD=PROD_8
            DO  JJ = 1,2
               IF(PROD(1,JJ).EQ.4)  THEN
                  OB8(1) = NINT(PROD(2,JJ)*10.)
               ELSE  IF(PROD(1,JJ).EQ.10)  THEN
                  STDV = NINT(PROD(2,JJ)*100.)
               END IF
            ENDDO
         ELSE
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'WSOS');PRODN=UFBINT_8
            OB8(1) = NINT(PRODN*10.)
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'RFLG');RFLG=UFBINT_8
            IF(RFLG.LT.BMISS) THEN
               WRITE(CRF,'(I1.1)')  NINT(RFLG)
               Q82(1) = CRF
            END IF
         END IF
      ELSE  IF(RTP.EQ.65)  THEN
C  ---------------------------------------------------------------------
C      ** OCEAN TOTAL PRECIPITABLE WATER PRODUCT -- REPORT TYPE 65 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 197 - TOTAL PRECIPITABLE WATER (MM * 10)
C                    (RAIN FLAG IN Q.M. BYTE 2)
C  ---------------------------------------------------------------------
         CF8(1) = 197
         ELV = 0
         NLCAT8 = 1
         IF(ISUPOB.EQ.1)  THEN
            CALL UFBREP(LUNIT,PROD_8,2,2,IRET,'FOST PH2O');PROD=PROD_8
            DO  JJ = 1,2
               IF(PROD(1,JJ).EQ.4)  THEN
                  OB8(1) = NINT(PROD(2,JJ)*10.)
               ELSE  IF(PROD(1,JJ).EQ.10)  THEN
                  STDV = NINT(PROD(2,JJ)*100.)
               END IF
            ENDDO
         ELSE
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'PH2O');PRODN=UFBINT_8
            OB8(1) = NINT(PRODN*10.)
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'RFLG');RFLG=UFBINT_8
            IF(RFLG.LT.BMISS) THEN
               WRITE(CRF,'(I1)')  NINT(RFLG)
               Q82(1) = CRF
            END IF
         END IF
      ELSE  IF(RTP.EQ.66)  THEN
C  ---------------------------------------------------------------------
C      ** LAND/OCEAN RAINFALL RATE -- REPORT TYPE 66 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 198 - RAINFALL RATE (MM/HR)
C  ---------------------------------------------------------------------
         CF8(1) = 198
         NLCAT8 = 1
         IF(ISUPOB.EQ.1)  THEN
            CALL UFBREP(LUNIT,PROD_8,2,2,IRET,'FOST REQV');PROD=PROD_8
            DO  JJ = 1,2
               IF(PROD(1,JJ).EQ.4)  THEN
                  OB8(1) = NINT(PROD(2,JJ)*3600.)
               ELSE  IF(PROD(1,JJ).EQ.10)  THEN
                  STDV = NINT(PROD(2,JJ)*36000.)
               END IF
            ENDDO
         ELSE
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'REQV');PRODN=UFBINT_8
            OB8(1) = NINT(PRODN*3600.)
         END IF
      ELSE  IF(RTP.EQ.576)  THEN
C  ---------------------------------------------------------------------
C      ** SURFACE TEMPERATURE -- REPORT TYPE 576 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 199 - SURFACE TEMPERATURE (DEGREES KELVIN)
C  ---------------------------------------------------------------------
         CF8(1) = 199
         NLCAT8 = 1
         IF(ISUPOB.EQ.1)  THEN
            CALL UFBREP(LUNIT,PROD_8,2,2,IRET,'FOST TMSK');PROD=PROD_8
            DO  JJ = 1,2
               IF(PROD(1,JJ).EQ.4)  THEN
                  OB8(1) = NINT(PROD(2,JJ))
               ELSE  IF(PROD(1,JJ).EQ.10)  THEN
                  STDV = NINT(PROD(2,JJ)*10.)
               END IF
            ENDDO
         ELSE
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'TMSK');PRODN=UFBINT_8
            OB8(1) = NINT(PRODN)
         END IF
      ELSE  IF(RTP.EQ.69)  THEN
C  ---------------------------------------------------------------------
C      ** OCEAN CLOUD WATER -- REPORT TYPE 69 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 200 - CLOUD WATER (MM * 100)
C  ---------------------------------------------------------------------
         CF8(1) = 200
         ELV = 0
         NLCAT8 = 1
         IF(ISUPOB.EQ.1)  THEN
            CALL UFBREP(LUNIT,PROD_8,2,2,IRET,'FOST CH2O');PROD=PROD_8
            DO  JJ = 1,2
               IF(PROD(1,JJ).EQ.4)  THEN
                  OB8(1) = NINT(PROD(2,JJ)*100.)
               ELSE  IF(PROD(1,JJ).EQ.10)  THEN
                  STDV = NINT(PROD(2,JJ)*1000.)
               END IF
            ENDDO
         ELSE
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'CH2O');PRODN=UFBINT_8
            OB8(1) = NINT(PRODN*100.)
         END IF
      ELSE  IF(RTP.EQ.573)  THEN
C  ---------------------------------------------------------------------
C      ** SOIL MOISTURE -- REPORT TYPE 573 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 201 - SOIL MOISTURE (MM)
C  ---------------------------------------------------------------------
         CF8(1) = 201
         NLCAT8 = 1
         IF(ISUPOB.EQ.1)  THEN
            CALL UFBREP(LUNIT,PROD_8,2,2,IRET,'FOST SMOI');PROD=PROD_8
            DO  JJ = 1,2
               IF(PROD(1,JJ).EQ.4)  THEN
                  OB8(1) = NINT(PROD(2,JJ)*1000.)
               ELSE  IF(PROD(1,JJ).EQ.10)  THEN
                  STDV = NINT(PROD(2,JJ)*10000.)
               END IF
            ENDDO
         ELSE
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'SMOI');PRODN=UFBINT_8
            OB8(1) = NINT(PRODN*1000.)
         END IF
      ELSE  IF(RTP.EQ.574)  THEN
C  ---------------------------------------------------------------------
C      ** SNOW DEPTH -- REPORT TYPE 574 **
C  ---------------------------------------------------------------------
C  CODE FIGURE 202 - SNOW DEPTH (MM)
C  ---------------------------------------------------------------------
         CF8(1) = 202
         NLCAT8 = 1
         IF(ISUPOB.EQ.1)  THEN
            CALL UFBREP(LUNIT,PROD_8,2,2,IRET,'FOST SNDP');PROD=PROD_8
            DO  JJ = 1,2
               IF(PROD(1,JJ).EQ.4)  THEN
                  OB8(1) = NINT(PROD(2,JJ)*1000.)
               ELSE  IF(PROD(1,JJ).EQ.10)  THEN
                  STDV = NINT(PROD(2,JJ)*10000.)
               END IF
            ENDDO
         ELSE
            CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'SNDP');PRODN=UFBINT_8
            OB8(1) = NINT(PRODN*1000.)
         END IF
      END IF

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C  FINISH PUTTING THE HEADER INFORMATION INTO ON29 FORMAT
C  ------------------------------------------------------

      RSV  = '999     '
      RSV2 = '        '

      IF(STDV.LT.BMISS)  THEN
         WRITE(CSTDV,'(I4.4)')  NINT(STDV)
      ELSE
         CSTDV = '9999'
      END IF
      RSV2(3:4) = CSTDV(1:2)
      RSV(1:2)  = CSTDV(3:4)

      CALL UFBINT(LUNIT,UFBINT_8,1,1,IRET,'ACAV');ACAV=UFBINT_8
      IF(ACAV.LT.BMISS)  THEN
         WRITE(CSTDV(1:2),'(I2.2)')  NINT(ACAV)
      ELSE
         CSTDV = '9999'
      END IF
      RSV2(1:2) = CSTDV(1:2)

      CALL S01O29(SID,XOB,YOB,RHR,RCH,RSV,RSV2,ELV,ITP,RTP)

      DO II = 1,NLCAT8
         IF(CF8(II).LT.BMISS)  CALL S02O29(8,II,*9999)
      ENDDO

C  PUT THE UNPACKED ON29 REPORT INTO OBS
C  -------------------------------------

      CALL S03O29(OBS,SUBSET,*9999,*9998)

      RETURN
 9999 CONTINUE
      R07O29 = 999
      RETURN
 9998 CONTINUE
      print'(" IW3UNP29/R07O29: RPT with ID= ",A," TOSSED - ZERO ",
     $ "CAT.1-6,8,51,52 LVLS")', SID
      R07O29 = -9999
      KSKSMI = KSKSMI + 1
      RETURN
      END

C>    This subrountine modifies amdar reports so that last character ends
C>    with 'Z'.
C>    @param[in] IDEN Acft id
C>    @param[out] ID Modified aircraft id.
C>
C>    @author RAY CRAYTON @date 1992-02-16

      SUBROUTINE S06O29(IDEN,ID)
C     ---> formerly SUBROUTINE IDP

      CHARACTER*8  IDEN,ID
      CHARACTER*6  ZEROES
      CHARACTER*1  JCHAR

      SAVE

      DATA  ZEROES/'000000'/

      ID = '        '

      L = INDEX(IDEN(1:8),' ')
      IF(L.EQ.0)  THEN
         N = 8
      ELSE
         N = L - 1
         IF(N.LT.1)  THEN
            ID = 'AMDARZ'
         END IF
      END IF

      IF(N.EQ.8)  THEN
         IF(IDEN(8:8).EQ.'Z')  THEN

C  THE ID INDICATES IT IS AN 8-CHARACTER ASDAR REPORT. COMPRESS IT BY
C  DELETING THE 6TH AND 7TH CHARACTER
C  ------------------------------------------------------------------

            ID = IDEN(1:5)//'Z'
            GO TO 500
         END IF
      END IF

      L = I05O29(IDEN(1:1),7,JCHAR)

      IF(L.EQ.0.OR.L.GT.6.OR.N.GT.6)  THEN

C  UP THROUGH 6 CHARACTERS ARE LETTERS. CHANGE 6TH CHARACTER TO 'Z'
C  ---------------------------------------------------------------

         IF(N.GE.5)  THEN
            ID = IDEN
            ID(6:6) = 'Z'
         ELSE

C  ZERO FILL AND ADD 'Z' TO MAKE 6 CHARAACTERS
C  -------------------------------------------

            ID = IDEN(1:N)//ZEROES(N+1:5)//'Z'
         END IF

      ELSE IF(N.EQ.6)  THEN

C  THE IDEN HAS 6 NUMERIC OR ALPHANUMERIC CHARACTERS
C  -------------------------------------------------

         IF(IDEN(6:6).EQ.'Z')  THEN
            ID = IDEN(1:6)
         ELSE  IF(L.GT.3)  THEN
            ID = IDEN(1:3)//IDEN(5:6)//'Z'
         ELSE  IF(L.EQ.1)  THEN
            ID = IDEN(2:6)//'Z'
         ELSE
            ID = IDEN(1:L-1)//IDEN(L+1:6)//'Z'
         END IF

      ELSE IF(N.EQ.5)  THEN

C  THE IDEN HAS 5 NUMERIC OR ALPHANUMERIC CHARACTERS
C  -------------------------------------------------

         ID = IDEN(1:5)//'Z'
      ELSE

C  THE IDEN HAS 1-4 NUMERIC OR ALPHANUMERIC CHARACTERS
C  ---------------------------------------------------

         IF(L.EQ.1)  THEN
            ID = ZEROES(1:5-N)//IDEN(1:N)//'Z'
         ELSE
            IF(N.LT.L)  THEN
               IDEN(1:6) = 'AMDARZ'
            ELSE
               ID = IDEN(1:L-1)// ZEROES(1:5-N)//IDEN(L:N)//'Z'
            END IF
         END IF
      END IF

 500  CONTINUE
      RETURN
      END

C>    This function finds the location of the next numeric character
C>    in a string of characters.
C>
C>    @param[in] STRING Character array.
C>    @param[in] NUM Number of characters to search in string.
C>    @param[out] CHAR Character found.
C>    @return I05O29 Integer*4 location of alphanumeric character, = 0 if not found.
C>    @author Ray Crayton @date 1989-07-07
C>
      FUNCTION I05O29(STRING,NUM,CHAR)
C     ---> formerly FUNCTION IFIG
      CHARACTER*1  STRING(1),CHAR

      SAVE

      DO I = 1,NUM
         IF(STRING(I).GE.'0'.AND.STRING(I).LE.'9')  THEN
            I05O29 = I
            CHAR = STRING(I)
            GO TO 200
         END IF
      ENDDO
      I05O29 = 0
      CHAR = '?'
 200  CONTINUE
      RETURN
      END
