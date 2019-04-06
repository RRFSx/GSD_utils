#!/bin/ksh
#####################################################
# machine set up (users should change this part)
#####################################################

### in PBS, cannnot put comments after job decription commands
#PBS -A P48503002
#PBS -l walltime=00:29:00
#PBS -N nexrad
#PBS -l select=1:ncpus=36:mpiprocs=36
#PBS -q premium
#PBS -o out.dpqc
#PBS -j oe

source /etc/profile.d/modules.sh
module purge
module load intel/17.0.1
module load mpt/2.19
module load netcdf/4.6.1

set -x
#
 cd /gpfs/fs1/p/ral/jntp/mhu/temp/GSD_UTL/tools/bufrDPQCRW/run
 cp /gpfs/fs1/p/ral/jntp/mhu/temp/GSD_UTL/tools/bufrDPQCRW/build/bin/encoderw.exe .

#------------------------------------------------
#  run 
###################################################
 mpiexec_mpt  ./encoderw.exe  > stdout_test 2>&1

##################################################################
#  run time error check
##################################################################
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi

cat sub_l2rwbufr_nssl_??? > l2rwbufr_nssl
grep 'processing finished successfully' sub_l2rwbufr_nssl_???.log
rm sub_l2rwbufr_nssl_???
rm sub_l2rwbufr_nssl_???.log
#
exit 0

