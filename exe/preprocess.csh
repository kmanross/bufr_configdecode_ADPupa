#!/bin/csh

# Script to process all BUFR tar files located in "bufr_configdecode/bufrobs"
# If a file is gzipped, it will gunzip it before untarring.
#

# !! Edit procdir directory definition to match the location where you
# !! put the software tar file before untarring it

set procdir=$HOME/bufr_configdecode_ADPupa
set CPLAT=other

# !!! Uncomment the following for linux !!!
set CPLAT=linux

if($CPLAT =~ "linux") then 
 set compile = gfortran    ## set compile=compiler_name, ie for ibm-sp compile = xlf
 cd $procdir/grabbufr
 $compile -w -o grabbufr grabbufr.f spbufr.f
endif

cd $procdir/bufrobs

set z=0

foreach file (gdasupaobs.*????.tar*)

# gdasupaobs.yyyymmdd.tar.gz
  set fntar = `echo "$file" | cut -c1-23`
  if ("$file" =~ *.gz)  then
    if (! -e $fntar)  then
      gunzip $file   # gunzip before untarring  # gunzip removes the .gz file
    endif
  endif
  tar -xvf $fntar    # tar does not remove the .tar file
end

foreach dir (upaobs.*????)

  set date=`echo $dir | awk -F. '{print $2}'`

  foreach hh ("00" "06" "12" "18") 

    set datehh=$date$hh
    set hour=$hh"z"

    echo $datehh
    echo $hour
    cd $dir
    cp $procdir/grabbufr/grabbufr grabbufr 
    foreach type (adpupa aircft satwnd aircar)
      if ($CPLAT =~ "linux") then
#
#       we want to save the original data file to avoid not knowing whether it 
#       is flipped, because the flipped version has the same name
#         
        if (-e gdas.$type.t$hour.$date.bufr)  then
          if (-e gdas.$type.t$hour.$date.bufr_save)  then
            cp -p gdas.$type.t$hour.$date.bufr_save gdas.$type.t$hour.$date.bufr
          else
            cp -p gdas.$type.t$hour.$date.bufr gdas.$type.t$hour.$date.bufr_save
          endif
          wc -c gdas.$type.t$hour.$date.bufr | grabbufr gdas.$type.t$hour.$date.bufr $type.t$hour.le
          mv $type.t$hour.le ../gdas.$type.t$hour.$date.bufr
        endif
      else
        if (! -e ../gdas.$type.t$hour.$date.bufr)  then
          cp -p gdas.$type.t$hour.$date.bufr ..
        endif
      endif
    end
    rm grabbufr
    cd ..
  end
end
