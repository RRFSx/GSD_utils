#!/bin/ksh --login

set -x

GSI_ROOT=/mnt/lfs3/projects/rtwbl/mhu/rapcode/GSI_r1326/data_process/encode_tamdar
FIX_ROOT=/mnt/lfs3/projects/rtwbl/mhu/rapcode/GSI_r1326/fix
DATAHOME=/mnt/lfs3/projects/rtwbl/mhu/rapcode/GSI_r1326/data_process/test
DATAROOT=/mnt/lfs3/projects/rtwbl/mhu/rapcode/GSI_r1326/data_process/test
TAMDAR_ROOT=/misc/public/data/acars/raw/netcdf
START_TIME=2016012515

# np=`cat $PBS_NODEFILE | wc -l`

#module load intel
#module load netcdf

# Vars used for testing.  Should be commented out for production mode

# Set up paths to unix commands
RM=/bin/rm
CP=/bin/cp
MV=/bin/mv
LS=/bin/ls
MKDIR=/bin/mkdir
CAT=/bin/cat
ECHO=/bin/echo
CUT=/bin/cut
WC=/usr/bin/wc
DATE=/bin/date
AWK="/bin/awk --posix"
SED=/bin/sed
LN=/bin/ln
MPIRUN=mpiexec

# Set the path to the gsi executable
ENCODE_TAMDAR=${GSI_ROOT}/process_tamdar_netcdf.exe

# Make sure DATAHOME is defined and exists
if [ ! "${DATAHOME}" ]; then
  ${ECHO} "ERROR: \$DATAHOME is not defined!"
  exit 1
fi
if [ ! -d "${DATAHOME}" ]; then
  ${ECHO} "Warning: DATAHOME directory '${DATAHOME}' does not exist!"
fi

if [ ! "${DATAROOT}" ]; then
  ${ECHO} "ERROR: \$DATAROOT is not defined!"
  exit 1
fi
if [ ! -d "${DATAROOT}" ]; then
  ${ECHO} "ERROR: DATAROOT directory '${DATAROOT}' does not exist!"
  exit 1
fi
if [ ! -d "${TAMDAR_ROOT}" ]; then
  ${ECHO} "ERROR: TAMDAR_ROOT directory '${TAMDAR_ROOT}' does not exist!"
  exit 1
fi

# Make sure GSI_ROOT is defined and exists
if [ ! "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: \$GSI_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${GSI_ROOT}" ]; then
  ${ECHO} "ERROR: GSI_ROOT directory '${GSI_ROOT}' does not exist!"
  exit 1
fi

# Make sure START_TIME is defined and in the correct format
if [ ! "${START_TIME}" ]; then
  ${ECHO} "ERROR: \$START_TIME is not defined!"
  exit 1
else
  if [ `${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`${ECHO} "${START_TIME}" | ${SED} 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`${ECHO} "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    ${ECHO} "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
  fi
  START_TIME=`${DATE} -d "${START_TIME}"`
fi


# Make sure the GSI executable exists
if [ ! -x "${ENCODE_TAMDAR}" ]; then
  ${ECHO} "ERROR: ${ENCODE_TAMDAR} does not exist!"
  exit 1
fi

# Create the obsprd directory if necessary and cd into it
if [ ! -d "${DATAHOME}" ]; then
  ${MKDIR} -p ${DATAHOME}
fi
cd ${DATAHOME}

if [ -x "process_tamdar.exe" ]; then
  ${RM} process_tamdar.exe
fi

# Compute date & time components for the analysis time
YYYYJJJHH=`${DATE} +"%Y%j%H" -d "${START_TIME}"`
PREYYJJJHH=`${DATE} +"%y%j%H" -d "${START_TIME} 1 hour ago"`
YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${START_TIME}"`
YYJJJHH=`${DATE} +"%y%j%H" -d "${START_TIME}"`
YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%H" -d "${START_TIME}"`

# Save a copy of the GSI executable in the workdir
${CP} ${ENCODE_TAMDAR} .

#
# Link to the TAMDAR data
#
filenum=0
${LS} ${TAMDAR_ROOT}/${PREYYJJJHH}*5r > tamdar_filelist
${LS} ${TAMDAR_ROOT}/${YYJJJHH}*5r >> tamdar_filelist
filenum=`more tamdar_filelist | wc -l`
# filenum=$(( filenum -3 ))

echo "found tamdar files: ${filenum}"

# Build the namelist on-the-fly
${CAT} << EOF > tamdar.namelist
 &SETUP
   analysis_time = ${YYYYMMDDHH},
   time_window = 0.5,
   TAMDAR_filenum  = ${filenum},
 /
EOF

# cp ${FIX_ROOT}/prepobs_prep_RAP.bufrtable ./prepobs_prep.bufrtable

exit 0
# Run obs pre-processor
${MPIRUN} -envall -np ${np} ${ENCODE_TAMDAR} < lightning.namelist > stdout_lighting 2>&1
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${ENCODE_TAMDAR} crashed  Exit status=${error}"
  exit ${error}
fi

exit 0
