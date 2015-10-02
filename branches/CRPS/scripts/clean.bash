#!/bin/bash
CURRENTDIR=`pwd`
cd ../output
rm -f *
cd ../datain
rm -f epsfilesin*
rm -f CPTEC*
rm -f CRPS.nml
cd ../dataout
rm -f *
cd ${CURRENTDIR}
