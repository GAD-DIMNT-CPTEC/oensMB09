#!/bin/ksh
cd /gfs/home3/modoper/tempo/global/oens/run
runtemplate.ksh ${1} AVN ; runtemplate.ksh ${1} 01N ; runtemplate.ksh ${1} 01P ; runtemplate.ksh ${1} 02N ; runtemplate.ksh ${1} 02P ; runtemplate.ksh ${1} 03N ; runtemplate.ksh ${1} 03P ; runtemplate.ksh ${1} 04N ; runtemplate.ksh ${1} 04P ; runtemplate.ksh ${1} 05N ; runtemplate.ksh ${1} 05P ; runtemplate.ksh ${1} 06N ; runtemplate.ksh ${1} 06P ; runtemplate.ksh ${1} 07N ; runtemplate.ksh ${1} 07P ; runtemplate.ksh ${1} ENM
