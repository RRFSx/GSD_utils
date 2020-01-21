#!/bin/ksh 
set -x

path=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/list
cd ${path}

fcst="12 15 18 21 24 27 30 33 36"
for ff in ${fcst}
  do
    filename1=flcut_Y2018M12F${ff}
    filename2=flcut_Y2019M01F${ff}
    filename3=flcut_Y2019M02F${ff}
    cat ${filename1} ${filename2} ${filename3} > filelist03_M120102F${ff}
done

