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

echo $1 $2 $3

if [ $# -lt 1 ]; then
    echo "There has to be at least one argument!"
    exit 1
fi

# base=${1%.R}
base=`basename $1`
infile=$base.R
outfile=$base.$$.Rout

## Full path to input file
if [ -n "$2" ]; then    
    RSLAVEDIR=$2                         # Specified as the second argument
fi

## Full path to output file
if [ -n "$3" ]; then   
    RSLAVEOUT=$3                         # Specified as the third argument
fi

JOB=${RSLAVEDIR:=$R_LIBS/rpvm}/$infile
OUT=${RSLAVEOUT:=$TMPDIR}/$outfile

## Test if the infile exists and readable
if [ ! -r $JOB ];  then
    echo "$JOB does not exist or is not readable!"
    exit 1
fi

## Test if the outdir is a directory and writable
if [ ! -d $RSLAVEOUT -o ! -w $RSLAVEOUT ]; then
    echo "$RSLAVEOUT is not a diretory or is not writable!"
    exit 1
fi

echo $JOB
echo $OUT

### It is assumed that R is in the search path
R BATCH $JOB $OUT
