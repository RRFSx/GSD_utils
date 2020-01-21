#!/bin/ksh 
set -x

path=/mnt/lfs1/projects/rtwbl/mhu/test/gsi/hrrre/data/

YYYY=2019
fcst="12 15 18 21 24 27 30 33 36"
#months="08 09 10 11 12"
months="01 02 03 04"

for mm in ${months}
do
  for ff in ${fcst}
  do
    filename=enspert_${YYYY}${mm}??00f0${ff}_mem*
    echo  'file=' ${filename}
    ls ${path}${filename} > fl_Y${YYYY}M${mm}F${ff}
  done
done

