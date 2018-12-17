#!/bin/ksh 
set -x

rwfilepath="/public/data/radar/nssl/AliasedVelocityDPQC"
radarname="KLTX"

YYYYMMDD=20181217
HH=05
MIN=26

radarlist=${YYYYMMDD}-${HH}????.${radarname}.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/${radarname}/${radarlist} > rwfilelist


