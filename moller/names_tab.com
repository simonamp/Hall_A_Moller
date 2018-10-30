#! /bin/bash
#
# ---  prints a table with filesnames of the *.tab files within a given range 
#        parameters:  1  - 1-st run of the first file
#                     2  - 1-st run of the last file
#                     3  - directory name with the tab files

 rr1=$1
 rr2=$2
 dnam=$3
 r0=0

 
 ntot=`ls -1 $dnam/run.tab.* | wc | awk '{print $1}' `
 n=0

 for namf in `ls -1 $dnam/run.tab.*`; do
   n=`expr $n + 1 `
#   echo $n $namf
   nam=`basename $namf`
   r=`echo $nam | cut -c 9-13`
   rr=`echo $r | grep '-'`
   if [ "$?" -eq 0 ]; then
     r=`echo $r | cut -c 1-4`
   fi
   if [ "$r0" -gt 0 -a "$r" -ne "$r0" ]; then
      echo $namf0
      r0=0
   fi
#      echo $n $r $rr1 $rr2
   if [ "$r" -ge "$rr1" -a "$r" -le "$rr2" ] ; then 
      ok=1
# Ignore a "corrected" file
      rr=`echo $nam | cut -c 9-50 | grep 'corrected'`
      if [ "$?" -eq 0 ]; then
        ok=0
      fi
# Ignore a "ignore" file
      rr=`echo $nam | cut -c 9-50 | grep 'ignore'`
      if [ "$?" -eq 0 ]; then
        ok=0
      fi
# Prefer a ".a" file if any
      namf1=$namf".a"
      if test -f $namf1 ; then
        ok=0
      fi
      if [ "$ok" -eq 1 ]; then
# Prefer the last file with the given "1-st" run
        r0=$r
        namf0=$namf
      fi
   fi
   if [ "$n" -eq "$ntot" -a "$r0" -gt 0 ]; then
     echo $namf0
   fi
 done



