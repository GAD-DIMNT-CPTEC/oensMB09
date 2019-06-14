MODULE Constants

 IMPLICIT NONE

 integer ji
 integer jpds(200),jgds(200),jens(200)
 integer kpds(200),kgds(200),kens(200)
 parameter(ji=384*192)             ! T126L28
 real dummy(ji)
 logical*1 lisum(ji),li(ji),aok
 integer lengh, lengh1,lengh2

 character*512 fname,name,tiggename
 character*4 nivel
!
! Single level fields
!
! instantaneous variables
!
 integer,parameter:: nsurf=24
 integer,parameter:: isuns_topo_sfc =1
 integer,parameter:: isuns_lsmk_sfc =2
 integer,parameter:: isuns_pslc_sfc =3
 integer,parameter:: isuns_psnm_msl =4
 integer,parameter:: isuns_agpl_sfc =5
 integer,parameter:: isuns_tsfc_sfc =6
 integer,parameter:: isuns_ussl_sfc =7
 integer,parameter:: isuns_tp2m_sfc =8
 integer,parameter:: isuns_u10m_sfc =9
 integer,parameter:: isuns_v10m_sfc =10
 integer,parameter:: isuns_cbnv_sfc =11
 integer,parameter:: isuns_tgsc_sfc =12
 integer jpdssuns(1:24,nsurf)

 data jpdssuns(24,:) /nsurf*-1/ 
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
! 
! accum variables
!
 integer,parameter:: isuns_prec_sfc=13
 integer,parameter:: isuns_neve_sfc=14
 integer,parameter:: isuns_cssf_sfc=15
 integer,parameter:: isuns_clsf_sfc=16
 integer,parameter:: isuns_role_sfc=17
 integer,parameter:: isuns_ocas_sfc=18
 integer,parameter:: isuns_slds_sfc=19

 data jpdssuns(5:7,isuns_prec_sfc    ) /061,001,000/
 data jpdssuns(5:7,isuns_neve_sfc    ) /064,001,000/
 data jpdssuns(5:7,isuns_cssf_sfc    ) /122,001,000/ 
 data jpdssuns(5:7,isuns_clsf_sfc    ) /121,001,000/
 data jpdssuns(5:7,isuns_role_sfc    ) /114,008,000/
 data jpdssuns(5:7,isuns_ocas_sfc    ) /111,001,000/
 data jpdssuns(5:7,isuns_slds_sfc    ) /112,001,000/
!
!
! Upper air data
!
 integer,parameter:: isuns_t_up =20
 integer,parameter:: isuns_g_up =21
 integer,parameter:: isuns_u_up =22
 integer,parameter:: isuns_v_up =23
 integer,parameter:: isuns_q_up =24

 integer ilev(9)
 data ilev /1000,0925,0850,0700,0500,0300,0250,0200,0050/

 data jpdssuns(5:6,isuns_t_up    ) /011,100/
 data jpdssuns(5:6,isuns_g_up    ) /007,100/
 data jpdssuns(5:6,isuns_u_up    ) /033,100/
 data jpdssuns(5:6,isuns_v_up    ) /034,100/
 data jpdssuns(5:6,isuns_q_up    ) /051,100/
!
! Upper air data
! 
 character*4 tiggename(24)

 data tiggename /'orog','lsm','sp','msl','tcw','skt', &
 'sm','2t','10u','10v','tcc','st','tp','sf','sshf',   &
 'slhf','ttr','ssr','str','t','g','u','v','q'/

END MODULE Constants
