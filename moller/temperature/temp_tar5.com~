#! /bin/sh
#
#===  Starts the temperature calculation task
#--- Input: TIM,bcurr,rb,ang,zsiz,fradiat,dtv
#
  if [ $# -lt 7 ]; then
    echo Too few parameters, the list should be TIM,bcurr,rb,ang,zsiz,fradiat,dtv
    exit
  fi

  tim=$1
  bcurr=$2
  rb=$3
  ang=$4
  zsiz=$5
  fradiat=$6
  dtv=$7

  echo $@ 

  dir=t_$tim"_beam-"$bcurr"-ua-"$rb"_foil-"$ang"-deg-z-"$zsiz"_rad-"$fradiat"_dtv-"$dtv
  echo $dir
  if test -d $dir ; then
    echo "---" Directory exists: $dir
    echo "---" Erase it by hand
    exit
  fi

  mkdir $dir
  cd $dir
  echo $@ > temp_tar.input
   
  ../temp_tar4.exe &> log

  
