set -euax

bufrin=$1
bufrout=$2

compiler=g95
    ## e.g. set compiler=xlf for ibm-sp

$compiler -o grabbufr grabbufr.f spbufr.f

wc -c $bufrin | grabbufr $bufrin $bufrout

