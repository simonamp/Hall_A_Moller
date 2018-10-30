#! /bin/bash

#
#  --  Updates the *set files 
#      Should run after add_set.com
#      moves files XXX_upd to XXX
#
  n=0
  dir=Info
  for i in `ls -1 $dir/mollerrun*_upd`; do
    j=`echo $i | sed s/_upd//`
    mv $i $j
    n=`expr $n + 1`
  done
  echo $n files are modified

