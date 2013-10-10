#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrupprair.x which to extract data from ADP BUFR
#  input files, and place the data into a basic text file.  It is used to
#  extract data from these kinds of files:
#      gdas.adpupa.tHHz.YYYYMMDD.bufr 
#      gdas.aircft.tHHz.YYYYMMDD.bufr
#      gdas.satwnd.tHHz.YYYYMMDD.bufr 
#      gdas.aircar.tHHz.YYYYMMDD.bufr
#
#  dumpbufr.x:        used to dump all contents of a BUFR file.
#  ** Make sure the "ar" command location has been set in your path
#  environment variable.  Type "which ar" to check if this is done. **
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=../lib
EXE=../exe

#  different platforms use different link name protocols
#  -----------------------------------------------------

# if using linux, BUFR files must be run through the "grabbufr/grabbufr.sh" script
# with the resulting output used as input for the decoders.  Set appropriate compiler
# in grabbufr.sh, and exe/convert.csh
 
cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   ff=g95

   fflag="-fno-second-underscore -fsloppy-char -w"
#   fflag="-fno-second-underscore -fsloppy-char -ftrace=frame -w"
#   fflag="-fno-second-underscore -fbounds-check -w"
#   fflag="-fno-second-underscore -w"
#
   cc=gcc
   cflag="-DUNDERSCORE -w"
elif [ $CPLAT = sgi ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   ff=f77
   cc=cc
   cflag=-DUNDERSCORE
elif [ $CPLAT = aix ]
then
   openrb=openrb
   openwb=openwb
   crdbfr=crdbufr
   cwrbfr=cwrbufr
   lenmsg=lenm
   ff=f77
   cc=cc
elif [ $CPLAT = sun ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   ff=f77
   cc=cc
   cflag=-DUNDERSCORE 
fi

#  Compile and archive the Bufr Library
#  ------------------------------------

$cc $cflag -c $cflag $LIB/*.c
$ff $fflag -c $LIB/*.f

ar crv $LIB/bufrlib.a *.o

#  Compile the decode programs
#  ---------------------------------------
 
$ff $fflag -c $SRC/bufrupprair.f
$ff $fflag -c $SRC/dumpbufr.f
 
#  link and load the executables
#  -----------------------------


$ff $fflag -o $EXE/bufrupprair.x bufrupprair.o $LIB/bufrlib.a
$ff $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/bufrlib.a

#  clean up
#  --------

rm -f *.o
