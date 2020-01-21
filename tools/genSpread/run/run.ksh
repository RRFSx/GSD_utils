#!/bin/ksh --login

# Set the queueing options 
#SBATCH --nodes=1 --ntasks-per-node=12
#SBATCH -t 0:30:00
## #SBATCH -A wrfruc
#SBATCH -A rtrr
#SBATCH -q debug
#SBATCH -J wrf_gsi
#SBATCH --partition=kjet

set -x

export OMP_NUM_THREADS=1
export OMP_STACKSIZE=300M

# Load modules
module load slurm
module load intel/18.0.5.274
module load impi/2018.4.274
module load szip
module load hdf5
module load netcdf/4.2.1.1
module load nco
module load contrib wrap-mpi

rundir=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/genSpread/run
runexe=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/genSpread/build/bin/cal_spread_mpi.exe
cd ${rundir}
cp ${runexe} cal_spread_mpi.exe

    mpirun -np 5 ./cal_spread_mpi.exe < namelist.sprd
exit
listpath=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/list

fields="ps rh tv u v"
season="120102"
#season="0809 120102"
fcst="24 27 30 33 36"
#fcst="12 15 18 21 24 27 30 33 36"
for ss in ${season}
do
for ff in ${fcst}
  do
    cp ${listpath}/filelist03_M${ss}F${ff} filelist03
    mpirun -np 5 ./cal_spread_mpi.exe < namelist.sprd

    for fld in ${fields}
    do
       mv results/spread_${fld}.bin  results/M${ss}F${ff}_spread_${fld}.bin
    done
    
done
done

exit
