#! /bin/bash

#
#  --  Reads the target angle from run.tab and includes it into *.set files  
#
#      Parameters: nametab
#                  nametab - the tab filename (run.tab.xxxx-xxxx)
#      Output set file is created with a suffux ".upd"

   if [ "$#" -lt 1 ]; then
     echo Error: 1 parameters needed but only "$#" are provided 
     exit
   fi

   fnam=$1

   if ! test -f $fnam ; then
      echo Error: no file $fnam
      exitm
   fi

   for ra in `cat $fnam | grep -v '^  ' | awk 'NF > 12 {print $1"_"$12}'`; do
     r=`echo $ra | cut -d_ -f1`
     a=`echo $ra | cut -d_ -f2`
#     echo $r $a
     ./add_set.com  $r $r Target_dial_add $a
   done









