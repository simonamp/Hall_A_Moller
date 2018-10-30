#!/usr/bin/ksh

if [ $# -lt 1 ] ; then
  infile=runs.tab
else 
  infile=$1
fi

rm -f runs.ps

a2ps -r --columns=1 --rows=1 -l154 --output=runs.ps $infile && rlpr -Hprtsrv -Pcha2hp runs.ps

exit
