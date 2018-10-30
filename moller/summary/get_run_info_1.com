#! /bin/bash
#
# ---  Reads a raw table for polarization results, makes the averages  and adds the run info
#      (for HAPPEX)
# parameters: the filename 

 fil1=$1

# ntot=`cat $fil1 | wc | awk '{print $1}' `
# n=0
# echo $ntot
# while n=`expr $n + 1` ; [ $n -le $ntot ]; do
#   lin=`cat $fil1 | head -$n | tail -1`
  lin=0
#   echo "$lin"
#   r1=`echo $lin | awk '{print $4}'`
   r1=$1
#   r2=`echo $lin | awk '{print $5}'`
   fset=~/paw/moller/Info/mollerrun_$r1.set
   ad=
#   echo $fset
   latta=x
   lslia=x
   bcura=x
   hwave=x
   lmodb=x
   lmodc=x
   if test -f $fset ; then
      lpowa=`cat $fset | grep IGL1I00DAC0 | cut -d: -f2 | awk '{print $1}'`
      y=`echo $lpowa | awk '{print NF}'`
      if [ "$y" -eq 0 ]; then
        lpowa=-1
      fi
      latta=`cat $fset | grep comm11      | cut -d: -f2 | awk '{print $1}'`
      y=`echo $latta | awk '{print NF}'`
      if [ "$y" -eq 0 ]; then
        latta=-1
      fi
      lslia=`cat $fset | grep SMRPOSA     | cut -d: -f2 | awk '{print $1}'`
      y=`echo $lslia | awk '{print NF}'`
      if [ "$y" -eq 0 ]; then
        lslia=-1
      fi
      bcura=`cat $fset | grep hac_bcm_average | head -1 | cut -d: -f2 | awk '{print $1}'`
      y=`echo $bcura | awk '{print NF}'`
      if [ "$y" -eq 0 ]; then
        bcura=-1
      fi
      hwave=`cat $fset | grep IGL1I00OD16_16   | cut -d: -f2 | awk '{print $1}'`
      if [ "$hwave" = "ON" ]; then
         hwave="1"
      else
         hwave="0"
      fi
      lmodb=`cat $fset | grep IGL1I00HALLBMODE | cut -d: -f2 | awk '{print $1}'`
      if [ "$lmodb" = "CW" ]; then
         lmodb="1"
      else
         lmodb="0"
      fi
      lmodc=`cat $fset | grep IGL1I00HALLCMODE | cut -d: -f2 | awk '{print $1}'`
      if [ "$lmodc" = "CW" ]; then
         lmodc="1"
      else
         lmodc="0"
      fi
     
      coil=`cat $fset | grep 'Hcoils current (Amps)' | cut -d: -f2 | awk '{printf("%3d",$1)}'` 
      y=`echo $coil | awk '{print NF}'`
      if [ "$y" -eq 0 ]; then
        coil=0
      fi
#      echo "$coil"
   fi
   pol=`grep "^ $r1" ../tab/run.tab.* | sed s"#+/-# #"g | gawk '{printf("%7.1f \n", $11*100)}'`
   pols=`echo $pol | gawk -v c=$coil '{printf("%7.0f \n", $1*c/9*(-1))}'`
#   echo $pol $pols
   if [ "$pols" -lt 0 ]; then
     c=-1
   elif [ "$pols" -gt 0 ]; then
     c=1
   else
     c=0
   fi
#   echo $pol $c 
  
   echo $bcura $lpowa
   ad=`echo "$bcura" " " "$lpowa" " " "$latta" " " "$lslia" " " "$hwave" " " "$lmodb" " " "$lmodc" " " "$coil" " " "$pol"`
#   echo "$lin" " " "$ad"
   echo "$lin" " " "$ad" | gawk -v c=$c '{printf("%2d %2d %2d %5d %5d %6.1f %6.1f %5.2f %6.1f %6.1f %5.2f %4.1f %6.3f %5.1f %5.0f %6.2f %2d %2d %2d %3d %6.1f \n",$1,$2,$3,$4,$5,$6,$7*c,$8,$9,$10*c,$11,$12,$13,$14,$15,$16,$17,$18,$19,$20,$21)}'
#   if [ "$n" -eq "$ntot" -a "$r0" -gt 0 ]; then
#     echo $namf0
#   fi
# done





