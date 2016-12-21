# !/bin/sh

# 'testall.sh'
# Regression Testing script for ART
# Step through a list of files
#   Compile, run, and check the output of each expected-to-work test
#   Compile and check the error of each expected-to-fail test


# llvm static compiler
LLC="llc"

# c compiler for linking in glut
CC="gcc"

# does the compilation
COMPILE="compile"
COMP="bash $COMPILE"

# Path to ART compiler
if [ -e "./art.native" ]
then
    ART="./art.native"
elif [ -e "./art" ]
then
    ART="./art"
else
    echo "No art compiler found. Attempting build..."
    echo ""
    make clean
    if make 
    then
        ART="./art.native"
    else
        echo -e "\nBuild Failed!!!" && exit 2
    fi
fi

# Time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

# Usage instructions
Usage() {
    echo "Usage: testall.sh [options] [.art files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
    echo "FAILED"
    error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <diffile>
# Compare <outfile> with <reffile>
# Differences, if any, are written to <difffile>
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -bu $1 $2 ">" $3 1>&2
    diff -bu "$1" "$2" > "$3" 2>&1 || {
    SignalError "$1 differs"
    echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
# Used for tests that are supposed to run without any errors
Run() {
    echo $* 1>&2
    eval $* || {
    SignalError "$1 failed on $*"
    return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
# Used for tests that are supposed to give an error
RunFail() {
    echo $* 1>&2
    eval $* && {
    SignalError "failed: $* did not report an error"
    return 1
    }
    return 0
}


# For tests that are supposed to run without any erros
Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.art//'`
    reffile=`echo $1 | sed 's/.art$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    resultname="results/"${basename}

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${resultname}.ll ${resultname}.out ${resultname}.s ${resultname}" &&

    Run "$COMP" "$1" "$resultname" "results/" &&
    Run "$resultname"  ">" "${resultname}.out" &&
    Compare "${resultname}.out" ${reffile}.out "${resultname}.diff"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    #rm -f $generatedfiles
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}

# For tests that are supposed to give an error
CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.art//'`
    reffile=`echo $1 | sed 's/.art$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    resultname="results/"${basename}

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${resultname}.err ${resultname}.diff" &&
    RunFail "$ART" "<" $1 "2>" "${resultname}.err" ">>" $globallog &&
    Compare "${resultname}.err" ${reffile}.err "${resultname}.diff"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    #rm -f $generatedfiles
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}


# Options
while getopts kdpsh c; do
    case $c in
    k) # Keep intermediate files
        keep=1
        ;;
    h) # Help
        Usage
        ;;
    esac
done

shift `expr $OPTIND - 1`

# Error finding CC
CCFail()
{
    echo "Could not find c compiler \"$CC\"."
    echo "Check your installation and/or modify the CC variable in testall.sh"
    exit 1
}

# Error finding LLC
LLCFail()
{
    echo "Could not find the LLVM static compiler \"$LLC\"."
    echo "Check your LLVM installation and/or modify the LLC variable in testall.sh"
    exit 1
}

# Error finding COMPILE
COMPILEFail()
{
    echo "Could not find compile \"$COMPILE\"."
    exit 1
}

which "$CC" >> $globallog || CCFail
which "$LLC" >> $globallog || LLCFail
ls "$COMPILE" >> $globallog || COMPILEFail

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.art tests/fail-*.art"
fi

# Make the results directory
if [ ! -e results ]
then
    mkdir results
fi

for file in $files
do
    case $file in
    *test-*)
        Check $file 2>> $globallog
        ;;
    *fail-*)
        CheckFail $file 2>> $globallog
        ;;
    *)
        echo "unknown file type $file"
        globalerror=1
        ;;
    esac
done

exit $globalerror
