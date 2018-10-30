#! /bin/sh
#
#===  Starts the temperature calculation task
#--- Input: TIM,bcurr,rb,ang,zsiz,fradiat,dtv,vers
#
  if [ $# -lt 10 ]; then
    echo Too few parameters, the list should be TIM,bcurr,rb,ang,zsiz,fradiat,dtv,xrast,yrast,vers
    exit
  fi

  tim=$1
  bcurr=$2
  rb=$3
  ang=$4
  zsiz=$5
  fradiat=$6
  dtv=$7
  xrast=$8
  yrast=$9
  vers=${10}

  echo $@ 
  dir=f5_t_$tim"_beam-"$bcurr"-ua-"$rb"_foil-"$ang"-deg-z-"$zsiz"_rad-"$fradiat"_dtv-"$dtv"_rast-"$xrast"-"$yrast"_v-"$vers
  echo $dir
  if test -d $dir ; then
    echo "---" Directory exists: $dir
    echo "---" Erase it by hand
    exit
  fi

  mkdir $dir
  cd $dir
  echo $@ > temp_tar.input
  cp ../temp_tar5.f .
   
  ../temp_tar5.exe &> log

  
