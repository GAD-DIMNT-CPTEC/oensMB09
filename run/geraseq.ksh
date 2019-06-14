#!/bin/ksh
typeset -ZR4 a

a=0
while [ ! $a -gt 360 ]; do
      echo $a
      let a=$a+6
done
      


exit 0
