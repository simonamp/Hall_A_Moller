#! /bin/bash
#
#  Gets some data form the moller monitor files, store them in a file cur.tmp (one line per run)
#  the run numbers are taken from runs.tab.cur
#  the 1-st call parameter is the directory name for the monitoring files
  
  rm -f cur.tmp
  dirm=$1
  for r in `cat runs.tab.cur | awk '{print $1}'` ; do
    z=0
    fn=mollerrun_"$r".set
#    echo $r " " $fn
    ok=0
    nam=`find $dirm -name $fn | head -1` ; echo $nam | grep $fn > /dev/null ; if [ $? -eq 0 ]; then ok=1 ; fi
    if [ $ok -eq 0 ] ; then
      nam=`find $dirm -name $fn.gz | head -1` ; echo $nam | grep $fn > /dev/null ; if [ $? -eq 0 ]; then ok=1 ; fi
      if [ $ok -eq 1 ] ; then
        echo Run $r gzipped file $fn
        z=1
        gunzip $nam
        nam=`find $dirm -name $fn | head -1`
      fi
    fi
    if [ $ok -ne 0 ] ; then
#        echo $nam
       d=`cat $nam | grep '^Date' | cut -d: -f2-10`
       day=`date +%j -d "$d"`
       tim=`date '+%Y %m %d %H %M' -d "$d"`
       a1=`cat $nam | grep  MBSY1C_energy | head -1 | awk '{print $NF}' | sed s/:/0000/ | awk '{printf("%5d",$1)}'`
       a2=`cat $nam | grep  'Passes Hall A'  | head -1 | awk '{print $NF}' | sed s/:/0/ | awk '{printf("%1d",$1)}'`
       q1=`cat $nam | grep  MMSLIN1EGAIN | head -1 | awk '{print $NF}' | sed s/:/0/`
       q2=`cat $nam | grep  MMSLIN2EGAIN | head -1 | awk '{print $NF}' | sed s/:/0/`
       a3=`echo $q1 $q2 | awk '{printf("%7.2f",$1*0.5+$2*0.5)}'` 
       a4=`cat $nam | grep hac_bcm_average | head -1 | awk '{print $NF}' | sed s/:/0.000/`
       a5=`cat $nam | grep  'Wien filter angle, deg'  | head -1 | awk '{print $NF}' | sed s/:/0.0/ | awk '{printf("%6.1f",$1)}'`
       a6=`cat $nam | grep 'Slit Position Hall A'     | head -1 | awk '{print $NF}' | sed s/:/0.0/ | awk '{printf("%5.2f",$1)}'`
       a7=`cat $nam | grep 'Laser attenuation Hall A' | head -1 | awk '{print $NF}' | sed s/:/0/   | awk '{printf("%3d",$1)}'`
       a8=`cat $nam | grep 'Laser power Hall A'       | head -1 | awk '{print $NF}' | sed s/:/0/   | awk '{printf("%3d",$1)}'`
       q1=`cat $nam | grep 'Laser mode  Hall B'       | head -1 | awk '{print $NF}' | sed s/:/MODE/`
       a9=0
       if [ "$q1" = "MODE" ]; then
         a9=1
       fi
       q1=`cat $nam | grep 'Laser mode  Hall C'       | head -1 | awk '{print $NF}' | sed s/:/MODE/`
       a10=0
       if [ "$q1" = "MODE" ]; then
         a10=1
       fi
       q1=`cat $nam | grep 'Laser 1/2 wave plate'     | head -1 | awk '{print $NF}' | sed s/:/OFF/` 
       a11=0
       if [ "$q1" = "ON" ]; then
         a11=1
       fi
       a12=`cat $nam | grep 'Beam Position BPM01  X, mm' | head -1 | awk '{print $NF}' | sed s/:/0.0/ | awk '{printf("%6.3f",$1)}'`
       a13=`cat $nam | grep 'Beam Position BPM01  Y, mm' | head -1 | awk '{print $NF}' | sed s/:/0.0/ | awk '{printf("%6.3f",$1)}'`

       coil=`cat $nam | grep 'Hcoils current (Amps)' | cut -d: -f2 | awk '{printf("%5.1f",$1)}'` 
       y=`echo $coil | awk '{print NF}'`
       if [ "$y" -eq 0 ]; then
         coil=0.0
       fi
       q1=`cat $nam | grep 'Target linear status'    | head -1 | cut -d: -f2 | awk '{print $1}'`
       tl=0
       if [ "$q1" = "Bottom" ]; then
         tl=1
       elif [ "$q1" = "Top" ]; then
         tl=2
       fi
       tr=`cat $nam | grep 'Target rotary position' | head -1 | awk '{print $NF}' | sed s/:/0.0/ | awk '{printf("%6.2f",$1)}'`
       thr=`cat $nam | grep -A 6 Ortec | grep thresh | head -1 | awk '{printf("%3d %3d", $2,$3)}'`
       plu3=`cat $nam | grep -A 12 'PLU Module Lecroy2365' | grep '^  3' | head -1 | awk '{printf("%5d %5d %1d", $2,$3,$4)}'`
#       plu8=`cat $nam | grep -A 12 'PLU Module Lecroy2365' | grep '^  8' | head -1 | awk '{printf("%5d %5d %1d", $2,$3,$4)}'`
       
       echo $r $tim $day "$a1" "$a2" "$a3" "$a4" "$a5" "$a6" "$a7" "$a8" "$a9" "$a10" "$a11" "$a12" "$a13" "$thr" "$plu3" "$tl" "$tr" "$coil" >> cur.tmp
       echo $r $tim $day "$a1" "$a2" "$a3" "$a4" "$a5" "$a6" "$a7" "$a8" "$a9" "$a10" "$a11" "$a12" "$a13" "$thr" "$plu3" "$tl" "$tr" "$coil" 
    else
      echo No file for run $r
      echo $r " " 0 0 0 0 0 0 0 0 0. 0. 0. 0. 0 0 0 0 0 0. 0. 0 0 0 0 0 0 0. 0. >> cur.tmp
      echo $r " " 0 0 0 0 0 0 0 0 0. 0. 0. 0. 0 0 0 0 0 0. 0. 0 0 0 0 0 0 0. 0. 
    fi
    if [ $z -ne 0 ]; then
      gzip $nam
    fi
  done








