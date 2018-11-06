#! /bin/bash
#
# ---  reading of raw data 

 PROG=`basename $0`
 echo Start $PROG
 os=`uname`

 dr=`pwd`

 dirwork=~/analysis/raw-ntup
 echo Running in $dirwork
 cd $dirwork

 fexe=rawread_"$os".exe
 diraw=/adaqs3/data4
 dext=dat
 fdr=0
 fdn=0
 run=0

 hostn=`hostname`

 hostn=`hostname`
 os=`uname | cut -f1`

# ==========   Help routine start
 help_dis () {
  cat <<help_doc
 $PROG - a script to convert a Moeller RAW data file to an NTUPLE

  Call parameters:
 $PROG -R RunNumber [-d raw_dir_name] [-n ntup_dir_name] [-e ext]
    where:
           -R the run number should be set              default:none
           -d raw directory name                        default: $diraw
           -n ntuple directory name                     default: = raw dir name
           -e data file extension                       default: $dext
    In the raw directory there should be only one raw *_NN.log file with the given number
    The output file will have the same name, but ended with .nt, not with .log
    Also, a link from the current directory to the NTUPLE file will be left
 Example: $PROG -R 9999 -n /tmp
help_doc
 }
# ==========   Help routine end

  if [ "$1" = "?" -o "$1" = "-?" ]; then
    help_dis
    exit
  fi
 
  set -- `getopt R:d:n:e: $*`
  if [ $? != 0 ] ; then
    echo *** Error in getopt . Usage:
    help_dis
    exit 2
  fi
  for i in $* ; do
     case $i in
      -R)  run=$2 ; shift 2;;
      -d)  fdr=1 ; diraw=$2 ; shift 2;;
      -n)  fdn=1 ; dirnt=$2 ; shift 2;;
      -e)  dext=$2 ; shift 2;;
      --)  shift ; break;;
     esac
  done

  if [ $run -eq 0 ]; then
    echo "***" Error: no run number is defined
    help_dis
    exit 2
  fi

  if [ $fdn -eq 0 ]; then
    dirnt=$diraw
  fi

  if ! test -d $diraw ; then
    echo "***" Error: missing raw data directory $diraw
    exit 2
  fi

  if ! test -d $dirnt ; then
    echo "***" Error: missing ntuple directory $dirnt
    exit 2
  fi

  if ! test -w $dirnt ; then
    echo "***" Error: no write permission to the ntuple directory $dirnt
    exit 2
  fi

  filer=$diraw/"*_"$run."$dext"
  echo $filer
  nf=0
  for i in `ls -1 $diraw/*_"$run"."$dext"` ; do nf=`expr $nf + 1` ; done
  echo $nf
  
  if [ $nf -eq 0 ]; then
    echo "***" Error: no raw data file "$filer"
    exit 2
  fi
 
  if [ $nf -gt 1 ]; then
    echo "***" Error: more than one raw data file
    ls -alF $filer
    exit 2
  fi
  filer=`ls -1 $diraw/*_"$run"."$dext"` 

  if ! test -r $filer ; then
    echo "***" Error: raw data file has no read permission  
    exit 2
  fi

  if ! test -s $filer ; then
    echo "***" Error: raw data file has zero length  
    exit 2
  fi

  rm raw.dat
  ln -s $filer raw.dat
  
  nam=`basename $filer`
  filnt=`echo $nam | sed s^\.$dext^\.nt^2 `
  echo $nam
  echo $filnt
  filnt=$dirnt/$filnt

  
  rm adctdc.nt
  ln -s $filnt adctdc.nt
  filntl=`basename $filnt `

#  ln -s $filnt $filntl

  echo Start: $fexe

  $fexe
