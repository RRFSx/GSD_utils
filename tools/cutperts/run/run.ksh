#!/bin/ksh --login

# Set the queueing options 
#SBATCH --nodes=1 --ntasks-per-node=24
#SBATCH -t 4:25:00
## #SBATCH -A wrfruc
#SBATCH -A rtrr
# #SBATCH -q debug
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

WORKPATH=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/run
GSI_EXE=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/build/bin/cut_pert.exe

cd ${WORKPATH}
cp ${GSI_EXE} gsi.exe

path=/mnt/lfs1/projects/rtwbl/mhu/test/gsi/hrrre/data/
pathlist=/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/list

YYYY=2019
fcst="12 15 18 21 24 27 30 33 36"
#months="08 09 10 11 12"
months="01 02 03"

for mm in ${months}
do
  for ff in ${fcst}
  do

    cp ${pathlist}/fl_Y${YYYY}M${mm}F${ff} filelist
    runname=Y${YYYY}M${mm}F${ff}g400

. ${WORKPATH}/namelist.cutpert_tmpl.sh
cat << EOF > namelist.cutpert
$namelist_cutpert
EOF

    mpirun -np 24 ./gsi.exe 
  done
done

exit

