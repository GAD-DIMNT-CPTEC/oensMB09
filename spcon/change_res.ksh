#! /bin/ksh

#set -o xtrace

if [ $# -ne 1 ]
then
  echo "Uso: ./change_res.ksh TQXXXXLXXX"
  exit 1
else
  res=${1}
fi

home_spcon=${PWD}
spcon_include=${home_spcon}/include

set -A Procs decanl deceof rdpert recanl recfct eofhumi eofpres eoftemp eofwind fftpln

for proc in ${Procs[@]}
do

  dir_proc=${home_spcon}/${proc}

  cd ${dir_proc}

  ln -sfn ${spcon_include}/${res} include

done

exit 0
