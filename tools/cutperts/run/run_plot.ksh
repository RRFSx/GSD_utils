#!/bin/ksh --login

set -x
WORKPATH=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/run
cd ${WORKPATH}
cp /mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/build/bin/plot_spread.exe plot_spread.exe

fields="ps rh tv u v"
#season="0809 120102"
season="120102"
fcst="12 15 18 21 24 27 30 33 36"
for ss in ${season}
do
for ff in ${fcst}
  do
    for fld in ${fields}
    do
       rm plot/spread_${fld}.bin
       ln -s /mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/genSpread/run/results/M${ss}F${ff}_spread_${fld}.bin plot/spread_${fld}.bin
    done
    ./plot_spread.exe
    mv plot/spread_X0001-0399Y0001-0399Z0001-0050.bin plot/M${ss}F${ff}_spread_X0001-0399Y0001-0399Z0001-0050.bin
done
done

exit

