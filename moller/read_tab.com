#! /bin/bash

 n=$1

 if [ $n -eq 0 ]; then
   cat $2 | grep '+/-' | grep -v 'run' | sed "s^+/-^ ^g"  
 else
   cat $2 | gawk -v t=$n '{print $t}'
 fi


