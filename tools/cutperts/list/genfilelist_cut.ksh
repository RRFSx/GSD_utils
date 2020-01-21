#!/bin/ksh 
set -x

path=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/run/results

YYYY=2019
fcst="12 15 18 21 24 27 30 33 36"
#months="08 09 10 11 12"
months="01 02 03"

for mm in ${months}
do
  for ff in ${fcst}
  do
    filename=Y${YYYY}M${mm}F${ff}g400_*.bin
    echo  'file=' ${filename}
    ls ${path}/${filename} > flcut_Y${YYYY}M${mm}F${ff}
  done
done

