#! /bin/sh

### This script should be put in the exec-path of pvmd (via ep='' option
### in ~/.xpvmhosts file.

## This script needs to know 
##
## 1. the name of the R script: it is specified as the first argument (with or
##    without .R postfix).
## 2. the path to the R script (could vary from host to host).
## 3. the path to the R output (could vary from host to host).
## 
## There are three methods to specify the latter (the method with higher order
## override lower ones).
##
## 1. as the 2nd and 3rd arguments when spawning task (only can be used if the
##    hosts can share the same file system).
## 2. defined as environmental variables on each host
## 3. otherwise, this script provides default position.

logfile=/tmp/rpvm.$$.log

if [ $# -lt 3 ]; then
    echo "There has to be three arguments!" >> $logfile
    exit 1
fi

infile=$1
base=`basename $infile .R`
outfile=$base.$$.Rout

## Directory of input file (relative to rpvm installation) supplied as the
## second argument
indir=$2
echo "input file is $indir/$infile" >> $logfile
OUT=$3/$outfile
echo "output file is $OUT" >> $logfile

### It is assumed that R is in the search path
${RPROG:-R} --vanilla <<EOF > $OUT 2>> $logfile

library(rpvm)

infile <- file.path (system.file (package = "rpvm"), "$indir", "$infile")
if (!file.exists (infile)) {
    cat ("Cannot find ", infile, "\n", file = "$OUT")
} else {
    source (infile)
}
.PVM.exit()
EOF
