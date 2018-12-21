#!/bin/ksh 
set -x

rwfilepath="/public/data/radar/nssl/AliasedVelocityDPQC"
rwfilepath="/gpfs/fs1/scratch/chunhua/HRRR/2018/data/obs_062018/DPQC/201806260000"
radarname="KLTX"

YYYYMMDD=20180625
HH=21
MIN=0

radarlist=${YYYYMMDD}-${HH}${MIN}???.${radarname}.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/${radarname}/${radarlist} > rwfilelist


