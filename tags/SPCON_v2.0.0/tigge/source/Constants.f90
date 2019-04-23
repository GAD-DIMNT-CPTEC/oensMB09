MODULE Constants

  IMPLICIT NONE

  INTEGER ji
  INTEGER jpds(200),jgds(200),jens(200)
  INTEGER kpds(200),kgds(200),kens(200)
  !PARAMETER(ji=128*64)             ! T042L28
  PARAMETER(ji=384*192)            ! T126L28

  REAL dummy(ji)
  LOGICAL*1 lisum(ji),li(ji),aok
  INTEGER lengh, lengh1,lengh2
  INTEGER*4 nwords
  CHARACTER(LEN=256) :: fname
  CHARACTER(LEN=256) :: name
!  CHARACTER(LEN=256) :: fname2(200)
  CHARACTER(LEN=4)   :: nivel
  !
  ! Single level fields
  !
  ! instantaneous variables
  !
  INTEGER,PARAMETER:: nsurf=24
  INTEGER,PARAMETER:: isuns_topo_sfc =1
  INTEGER,PARAMETER:: isuns_lsmk_sfc =2
  INTEGER,PARAMETER:: isuns_pslc_sfc =3
  INTEGER,PARAMETER:: isuns_psnm_msl =4
  INTEGER,PARAMETER:: isuns_agpl_sfc =5
  INTEGER,PARAMETER:: isuns_tsfc_sfc =6
  INTEGER,PARAMETER:: isuns_ussl_sfc =7
  INTEGER,PARAMETER:: isuns_tp2m_sfc =8
  INTEGER,PARAMETER:: isuns_u10m_sfc =9
  INTEGER,PARAMETER:: isuns_v10m_sfc =10
  INTEGER,PARAMETER:: isuns_cbnv_sfc =11
  INTEGER,PARAMETER:: isuns_tgsc_sfc =12
  INTEGER jpdssuns(1:24,nsurf)
  INTEGER jpdssuns2(1:24,nsurf)

  DATA jpdssuns(24,:) /nsurf*-1/ 
 data jpdssuns(5:7,isuns_topo_sfc    ) /132,001,000/
 data jpdssuns(5:7,isuns_lsmk_sfc    ) /081,001,000/
 data jpdssuns(5:7,isuns_pslc_sfc    ) /135,001,000/
 data jpdssuns(5:7,isuns_psnm_msl    ) /002,102,000/
 data jpdssuns(5:7,isuns_agpl_sfc    ) /054,200,000/
 data jpdssuns(5:7,isuns_tsfc_sfc    ) /187,001,000/
 data jpdssuns(5:7,isuns_ussl_sfc    ) /182,001,000/
 data jpdssuns(5:7,isuns_tp2m_sfc    ) /128,105,002/
 data jpdssuns(5:7,isuns_u10m_sfc    ) /130,105,010/
 data jpdssuns(5:7,isuns_v10m_sfc    ) /131,105,010/
 data jpdssuns(5:7,isuns_cbnv_sfc    ) /071,003,000/ 
 data jpdssuns(5:7,isuns_tgsc_sfc    ) /191,001,000/

!../../../../../grib_api/lib_grib_api/grib_api-1.9.0/data/tigge

  DATA jpdssuns2(24,:) /nsurf*-1/ 
  !                                          1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
  DATA jpdssuns2(1:24,isuns_topo_sfc    ) /046,000,000,000,007,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_lsmk_sfc    ) /046,000,000,000,081,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_pslc_sfc    ) /046,000,000,000,001,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_psnm_msl    ) /046,000,000,000,002,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_agpl_sfc    ) /046,000,000,000,054,200,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_tsfc_sfc    ) /046,000,000,000,187,001,002,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_ussl_sfc    ) /046,000,000,000,086,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_tp2m_sfc    ) /046,000,000,000,011,105,002,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_u10m_sfc    ) /046,000,000,000,033,105,010,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_v10m_sfc    ) /046,000,000,000,034,105,010,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_cbnv_sfc    ) /046,000,000,000,071,003,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_tgsc_sfc    ) /046,000,000,000,085,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/

  ! 
  ! accum variables
  !
  INTEGER,PARAMETER:: isuns_prec_sfc=13
  INTEGER,PARAMETER:: isuns_neve_sfc=14
  INTEGER,PARAMETER:: isuns_cssf_sfc=15
  INTEGER,PARAMETER:: isuns_clsf_sfc=16
  INTEGER,PARAMETER:: isuns_role_sfc=17
  INTEGER,PARAMETER:: isuns_ocas_sfc=18
  INTEGER,PARAMETER:: isuns_slds_sfc=19

  DATA jpdssuns(5:7,isuns_prec_sfc    ) /061,001,000/
  DATA jpdssuns(5:7,isuns_neve_sfc    ) /064,001,000/
  DATA jpdssuns(5:7,isuns_cssf_sfc    ) /122,001,000/ 
  DATA jpdssuns(5:7,isuns_clsf_sfc    ) /121,001,000/
  DATA jpdssuns(5:7,isuns_role_sfc    ) /114,008,000/
  DATA jpdssuns(5:7,isuns_ocas_sfc    ) /111,001,000/
  DATA jpdssuns(5:7,isuns_slds_sfc    ) /112,001,000/

                                   !          1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
  DATA jpdssuns2(1:24,isuns_prec_sfc     ) /046,000,000,000,061,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_neve_sfc     ) /046,000,000,000,065,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_cssf_sfc     ) /046,000,000,000,122,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_clsf_sfc     ) /046,000,000,000,121,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_role_sfc     ) /046,000,000,000,114,008,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_ocas_sfc     ) /046,000,000,000,111,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_slds_sfc     ) /046,000,000,000,121,001,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/

  !
  !
  ! Upper air data
  !
  INTEGER,PARAMETER:: isuns_t_up =20
  INTEGER,PARAMETER:: isuns_g_up =21
  INTEGER,PARAMETER:: isuns_u_up =22
  INTEGER,PARAMETER:: isuns_v_up =23
  INTEGER,PARAMETER:: isuns_q_up =24

  INTEGER ilev(9)
  DATA ilev /1000,0925,0850,0700,0500,0300,0250,0200,0050/

  DATA jpdssuns(5:6,isuns_t_up    ) /011,100/
  DATA jpdssuns(5:6,isuns_g_up    ) /007,100/
  DATA jpdssuns(5:6,isuns_u_up    ) /033,100/
  DATA jpdssuns(5:6,isuns_v_up    ) /034,100/
  DATA jpdssuns(5:6,isuns_q_up    ) /051,100/ 
                                 !      1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
  DATA jpdssuns2(1:24,isuns_t_up   ) /046,000,000,000,011,100,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_g_up   ) /046,000,000,000,007,100,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_u_up   ) /046,000,000,000,033,100,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_v_up   ) /046,000,000,000,034,100,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/
  DATA jpdssuns2(1:24,isuns_q_up   ) /046,000,000,000,051,100,000,000,000,&
  000,000,000,000,000,000,000,000,001,002,000,000,000,000,000/

  !
  ! Upper air data
  ! 
  ! character*4 tiggename(24)
  CHARACTER(LEN=256),PARAMETER :: tiggename(1:24) = &
       (/'orog','lsm ','sp  ','msl ','tcw ','skt ',&
       'sm  ','2t  ','10u ','10v ','tcc ','st  ',&
       'tp  ','sf  ','sshf','slhf','ttr ','ssr ',&
       'str ','t   ','g   ','u   ','v   ','q   '/)

END MODULE Constants
