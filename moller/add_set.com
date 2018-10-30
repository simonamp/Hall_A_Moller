#! /bin/bash

#
#  --  adds information to the set files
#
#      Parameters: r1 r2 name value
#                  r1 - 1-st run
#                  r2 - last run
#                  name - the word to identify the variable
#                  value - value to write
#      Output file is created with a suffux ".upd"

   if [ "$#" -lt 4 ]; then
     echo Error: 4 parameters needed but only "$#" are provided 
     exit
   fi

   dir=Info

   r1=$1
   r2=$2
   vname=$3
   value=$4

   r=`expr $r1 - 1`

   while r=`expr $r + 1` ; [ $r -le $r2 ]; do

      fnam="$dir"/mollerrun_"$r".set

      if test -f $fnam ; then
#         File exists

        fnam1="$fnam"_upd
        nw=`grep $vname $fnam | wc -l `
        ok=0

        if [ $nw -eq 0 ] ; then
#         New parameter
          cp $fnam $fnam1
          echo $vname"                                           ":" "$value >> $fnam1
          echo "Added   : "$fnam1
          ok=1
        elif [ $nw -eq 1 ]; then
#         Modify the parameter
	  
          nl=`grep -n $vname $fnam | cut -d: -f1`
          nl1=`expr $nl - 1 `
          head -n $nl1 $fnam > $fnam1
          grep $vname $fnam | sed s/:/:%/ | cut -d% -f1 | sed s/:/:" "$value/ >> $fnam1
          nt=`cat $fnam | wc -l `
          nl2=`expr $nt - $nl `
          tail -n $nl2 $fnam >> $fnam1
          echo "Modified: "$fnam
          ok=1
        else
          echo Error: in $fnam too many lines with $vname
          grep $vname $fnam 
        fi
      else
        echo No file $fnam 
      fi
   done









