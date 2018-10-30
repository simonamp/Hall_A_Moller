#! /bin/sh
#
#===  Starts the temperature calculation task
#--- Input: TIM,pulse,fpulse,rb,zsiz,fradiat,dtv
#
  if [ $# -lt 7 ]; then
    echo Too few parameters, the list should be TIM,pulse,fpulse,rb,zsiz,fradiat,dtv
    exit
  fi

  tim=$1
  pulse=$2
  fpulse=$3
  rb=$4
  zsiz=$5
  fradiat=$6
  dtv=$7

  echo $@ 

  dir=t_$tim"_beam-"$pulse"x"$fpulse"-"$rb"_foil-"$zsiz"_rad-"$fradiat"_dtv-"$dtv
  echo $dir
  if test -d $dir ; then
    echo "---" Directory exists: $dir
    echo "---" Erase it by hand
    exit
  fi

  mkdir $dir
  cd $dir
  echo $@ > temp_tar.input
  
  ../temp_tar.exe &> log

  
