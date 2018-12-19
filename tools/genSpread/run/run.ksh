#!/bin/ksh --login

# Set the queueing options 
#PBS -l procs=5
#PBS -l walltime=4:25:00
#PBS -A rtwbl
## #PBS -A wrfruc
## #PBS -q debug
#PBS -N wrf_gsi
#PBS -l partition=tjet
#PBS -j oe

set -x
np=$PBS_NP

rundir=/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/GSD_UTL/tools/genSpread/run
runexe=/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/GSD_UTL/tools/buildsprd/bin/cal_spread_mpi.exe

cd ${rundir}
cp ${runexe} cal_spread_mpi.exe
mpirun -envall -np ${np} ./cal_spread_mpi.exe < namelist.sprd > stdout 2>&1

exit

# ls /mnt/lfs1/projects/rtwbl/mhu/test/gsi/hrrre/data/enspert_201808*f021_mem000? > filelist08f21
# cut_spread.exe < namelist.cut
