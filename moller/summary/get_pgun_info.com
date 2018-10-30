#! /bin/bash
#
# ---  Get the interesting info from PLOG files
# parameters: the filename for the PLOG extract

  fnam=$1
  nl=`wc -l $fnam | awk '{print $1}'`
  
  i=0
  while i=`expr $i + 1`; [ $i -le $nl ] ; do

     line=`head -$i $fnam | tail -1`
     ok=0

#     echo $line | grep -i -q spot ; if [ $? -eq 0 ]; then ok=1 ; fi
#     echo $line | grep -i -q cesia ; if [ $? -eq 0 ]; then ok=1 ; fi
#     echo $line | grep -q Cs ; if [ $? -eq 0 ]; then ok=1 ; fi
#     echo $line | grep -q -i switch ; if [ $? -eq 0 ]; then ok=1 ; fi
#     echo $line | grep -q -i mott ; if [ $? -eq 0 ]; then ok=1 ; fi
     echo $line | grep -q -i polari ; if [ $? -eq 0 ]; then ok=1 ; fi

     if [ $ok -gt 0 ]; then
        dat=`echo $line | awk '{print $4}'`
        dat1=`date +%j -d $dat`
        tim=`echo $line | awk '{print $5}'`
        h=`date +%H -d $tim`
        m=`date +%M -d $tim`
        dat2=`echo $dat1 $h $m | awk '{printf("%6.2f \n", $1+($2*60+$3)/1440.)}'`
        echo $dat2 " " $line
#        echo $ok " " $line
     fi
 
  done





