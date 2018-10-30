#! /bin/bash
#
# -- Starts tasks for m_optp simulation
#    Paremeters: e0 e1 de
#                e0 - initial energy in MeV
#                e1 - final energy in MeV (may be not reached exactly)
#                de - energy increment (in MeV)
#                dir - directory (extension) for histograms
#
 PROG=`basename $0`
 echo Start $PROG


 dr=`pwd`
 cpu=`hostname -s | sed "s/haplix//"`

  if [ $# -lt 4 ]; then
    echo "Error: 3 parameters needed (e0 e1 de in MeV)"
    exit
  fi

  e0=$1
  e1=$2
  de=$3
  ds=$4

  suf="$e0"-"$e1"
  log=m_opt."$suf".log
  if test -f $log ; then
    rm $log
  fi

  e=`expr $e0 - $de`
  echo $e0 $e1 $de $e
  while e=`expr $e + $de`; [ $e -le $e1 ] ; do
    eg=`echo $e | awk '{a=$1/1000. ; printf("\%5.3f\n",a)}'`
    id=1000
    echo "$eg"GeV
#    echo $eg $id 0 96 -112 -2.4 2.4 -2.8 2.8 -2.8 2.8 0.0 0.0 0.0 > m_opt.in
#    echo $eg $id 0 96 -112 -2.4 2.4 -2.8 2.8 -2.8 2.8 0.2 0.3 0.3 > m_opt.in
    echo $eg $id 0 -1 -1 -2.4 2.4 -0.2 2.8 -2.8 2.8 0.2 0.3 0.3 > m_opt.in
#    echo $eg $id 0 96 -1 -2.4 2.4 -0.2 2.8 -2.8 2.8 0.2 0.3 0.3 > m_opt.in
#    echo $eg $id 0 96 -60 -2.4 2.4 -0.2 2.8 -2.8 2.8 0.2 0.3 0.3 > m_opt.in
    nice -n 4 ./m_optp.exe | tee log
    cat log >> $log
    ee=$e
    if [ $ee -lt 1000 ]; then
      ee=0"$ee"
    fi 
    his=m_opt_"$ee".his
    cp m_opt.his /data"$cpu"/user/gen/moller/simul/"$ds"/$his
  done 




