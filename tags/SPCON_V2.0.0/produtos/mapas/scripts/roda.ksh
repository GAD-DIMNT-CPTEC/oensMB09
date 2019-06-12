#!/bin/ksh
#help#
#************************************************************************************#
#                                                                                    #
#                                                                                    #
#      Name:           runMap                                                        #
#                                                                                    #
#      Function:       Generation of latlon figures                                  #
#                                                                                    #
#      Date:           April 03th, 2000.                                             #
#      Last change:    Abril 03th, 2000.                                             #
#                                                                                    #
#      Valid Arguments for runMap                                                    #
#      usage:                                                                        #
#      runMap cpu_mpi  cpu_node name TRC LV LABELI LABELF hold                       #
#      First : TRC    : three-digit triangular truncation                            #
#      Second: LV     : two-digit number of vertical sigma-layers                    #
#      Third : LABELI : initial forecasting label                                    #
#      Fourth: LABELF : final forecasting label                                      #
#                                                                                    #
#              LABELx : yyyymmddhh                                                   #
#                       yyyy = two digit year                                        #
#                       mm   = two digit month                                       #
#                       dd   = two digit day                                         #
#                       hh   = two digit hour                                        #
#                                                                                    #
#************************************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
fi

export CASE=$4
if [ ${#CASE} -ne 10 ]; then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
else
#
# SETTING THE APPROPRIATED ENVIRONMENT
#
  PATHA=`pwd`
  export FILEENV=`find ${PATHA} -name EnvironmentalVariablesMCGA -print`
  export PATHENV=`dirname ${FILEENV}`
. ${FILEENV} ${CASE}  
  cd ${HOME_suite}/run
fi

echo "`echo ${0}`: Start execution at `date`"
#       Test of Valid Arguments
#
if [ -z "$5" ]; then
      print "LABELI is not set (yyyymmddhh)"
      exit 1
else
      LABELI=$5 ; export LABELI
fi

if [ -z "$6" ]; then
      print "LABELF is not set (yyyymmddhh)"
      exit 2
else
      LABELF=$6 ; export LABELF
fi

cd ${HOME_suite}/mapas/scripts

/usr/local/grads/bin/gradsnc -bp << E
run Mapa1.sx6.gs
${LABELI} ${LABELP} 1 7 ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
E
mv *png ${DK_suite}/mapas/${TRCLV}/${LABELI}/



exit 0
/usr/local/grads/bin/gradsnc -bp  << E
run Mapa1.sx6.gs
${LABELI} ${LABELP} 2 7 ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
E
mv *png ${DK_suite}/mapas/${TRCLV}/${LABELI}/

/usr/local/grads/bin/gradsnc -bl  << E
run Mapa1.sx6.gs
${LABELI} ${LABELP} 3 7 ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
E
mv *png ${DK_suite}/mapas/${TRCLV}/${LABELI}/

/usr/local/grads/bin/gradsnc -bp  << E
run Mapa1.sx6.gs
${LABELI} ${LABELP} 4 7 ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${DK_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
E
mv *png ${DK_suite}/mapas/${TRCLV}/${LABELI}/

/usr/local/grads/bin/gradsnc -bp << E
run Mapa2.sx6.gs
${LABELI} 4 7  ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl  ${DK_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl
quit
E
mv *png ${DK_suite}/mapas/${TRCLV}/${LABELI}/

exit 0
