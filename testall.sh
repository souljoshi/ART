# !/bin/sh

# 'testall.sh'
# Regression Testing script for ART
# Step through a list of files
#   Compile, run, and check the output of each expected-to-work test
#   Compile and check the error of each expected-to-fail test

# Path to LLVM interpreter
LLI="lli"

# Path to ART compiler
ART="./art.native"

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
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
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

# Syntax error runs with RunFail now
#
# RunSyntax <args>
# Report the command, run it, and expect a syntax error
# Used for tests that are supposed to give a syntax error
# RunSyntax() {
#    echo $* 1>&2
#    eval $* || {
#    SignalError "$1 failed on $*"
#    return 1
#    }
# }

# For tests that are supposed to run without any erros
Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.art//'`
    reffile=`echo $1 | sed 's/.art$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.out" &&
    Run "$ART" "<" $1 ">" "${basename}.ll" &&
    Run "$LLI" "${basename}.ll" ">" "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff

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

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    RunFail "$ART" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
    Compare ${basename}.err ${reffile}.err ${basename}.diff

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

# Syntax errors runs with CheckFail() now
#
# For tests that are supposed to give a syntax error rather than an exception
# CheckSyntax() {
#    error=0
#    basename=`echo $1 | sed 's/.*\\///
#                             s/.art//'`
#    reffile=`echo $1 | sed 's/.art$//'`
#    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
#
#    echo -n "$basename..."
#
#    echo 1>&2
#    echo "###### Testing $basename" 1>&2
#
#    generatedfiles=""
#
#    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
#    RunSyntax "$ART" "<" $1 "| tee" "${basename}.err" ">>" $globallog &&
#    Compare ${basename}.err ${reffile}.err ${basename}.diff
#
#    # Report the status and clean up the generated files
#
#    if [ $error -eq 0 ] ; then
#    if [ $keep -eq 0 ] ; then
#        rm -f $generatedfiles
#    fi
#    echo "OK"
#    echo "###### SUCCESS" 1>&2
#    else
#    #rm -f $generatedfiles
#    echo "###### FAILED" 1>&2
#    globalerror=$error
#    fi
# }

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

# Error finding LLI
LLIFail()
{
    echo "Could not find the LLVM interpreter \"$LLI\"."
    echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
    exit 1
}

which "$LLI" >> $globallog || LLIFail


if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.art tests/fail-*.art" #tests/syntax-*.art
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
    #*syntax-*)
    #    CheckSyntax $file 2>> $globallog
    #    ;;
    *)
        echo "unknown file type $file"
        globalerror=1
        ;;
    esac
done

exit $globalerror
