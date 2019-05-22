cmake_minimum_required(VERSION 2.8)

set(ENV{CC} "icc")
set(ENV{CXX} "icpc")
set(ENV{FC} "mpif90 -f90=ifort")

project(data_process)
enable_language (Fortran)
enable_language (C)

site_name(HOSTNAME)
message("The hostname is  ${HOSTNAME}" )
string(REGEX MATCH "cheyenne" HOST-Cheyenne ${HOSTNAME}  )
string(REGEX MATCH "tfe[0-9]" HOST-Theia ${HOSTNAME} )
if(EXISTS /jetmon)
  set(HOST-Jet "True" )
endif()

### Do the following to load required compiling modules on Jet
#     source /home/rtrr/PARM_EXEC/modulefiles/modulefile.jet.GSI_UPP_WRF 
###

if(HOST-Cheyenne)
  ###SET(GSIBUILD  "/glade/work/geguo/MPMC/b_release_v3.7/ci06.intel_17.0.1.mpt_2.15f")
  SET(GSIBUILD  "/glade/p/ral/jntp/DAtask/chunhua/HRRR-2018/code/GSD_GSI/build")
  set(COMUPP "/gpfs/fs1/p/ral/jntp/mhu/rapcode/GSD_UPP/comupp")
  set(wrfframe "/glade/p/ral/jntp/DAtask/chunhua/HRRR-2018/code/rapid-refresh/WRFV3.9/frame/pack_utils.o")
  set(WPSBUILD "/gpfs/fs1/p/ral/jntp/mhu/wrf/WPSV3.9.0.01")
  ### MPI_FORTRAN_LIBRARIES
  SET(MPILIB  "/gpfs/u/apps/ch/opt/mpt/2.19/opt/hpe/hpc/mpt/mpt-2.19/lib/libxmpi.so")
  ### BLAS_iomp5_LIBRARY
  SET(LIBIOMP5  "/glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/lib/intel64_lin/libiomp5.so")
elseif(HOST-Jet)
    ###set(COMUPP "/gpfs/fs1/p/ral/jntp/mhu/rapcode/GSD_UPP/comupp")
    ###SET(GSIBUILD  "/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/gsicode/build")
  SET(GSIBUILD  "/lfs1/projects/wrfruc/gge/MPMC/b_new_Jet_OS/ji01.intel_18.0.5.274.impi_2018.4.274")
  set(COMUPP "/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/upp/GSD_UPP_20190217_bc55f77/comupp")
  set(wrfframe "/mnt/lfs1/projects/rtwbl/mhu/code/wrf/WRFV3.9.1.1/frame/pack_utils.o")
  set(WPSBUILD "/mnt/lfs3/projects/rtwbl/mhu/rapcode/git_local/work/WPSV3.9")
#  SET(MPILIB  "/apps/intel/impi/5.1.1.109/intel64/lib/libmpifort.so;/apps/intel/impi/5.1.1.109/intel64/lib/release_mt/libmpi.so;/apps/intel/impi/5.1.1.109/intel64/lib/libmpigi.a;/usr/lib64/libdl.so;/usr/lib64/librt.so;/usr/lib64/libpthread.so")
  SET(LIBIOMP5  "-qopenmp")
elseif(HOST-Theia)
  SET(GSIBUILD  "/scratch3/BMC/wrfruc/mhu/rapcode/git_local/work/buildgsi_master")
  set(COMUPP "/scratch3/BMC/wrfruc/mhu/rapcode/git_local/work/upp/GSD_UPP/comupp")
  set(wrfframe "/scratch3/BMC/wrfruc/mhu/code/WRF/WRFV3.9.1.1/frame/pack_utils.o")
  set(WPSBUILD "/scratch3/BMC/wrfruc/mhu/rapcode/git_local/work/WPSV3.9.0.1")
  SET(MPILIB  "/apps/intel/impi/5.1.1.109/intel64/lib/libmpifort.so;/apps/intel/impi/5.1.1.109/intel64/lib/release_mt/libmpi.so;/apps/intel/impi/5.1.1.109/intel64/lib/libmpigi.a;/usr/lib64/libdl.so;/usr/lib64/librt.so;/usr/lib64/libpthread.so")
  SET(LIBIOMP5  "-openmp")
else()
  message(FATAL_ERROR "
       Unknow host ${HOSTNAME}

        ")
endif()

#----------------------------Normally no changes needed after this line ------------------------
SET(NETCDF  $ENV{NETCDF})

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_INCLUDE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/include")
set(Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

SET(GSIINC  "${GSIBUILD}/include")
SET(NETCDFINC  "${NETCDF}/include")
SET(G2INC  "${COMUPP}/include")

SET(GSILIB  "${GSIBUILD}/lib/libgsilib_shrd.a")
SET(wrflib  "${GSIBUILD}/lib/libWRFLIB.a")
###NETCDF_LIBRARIES
####SET(NETCDFLIB  "${NETCDF}/lib/libnetcdf.a;${NETCDF}/lib/libnetcdff.a") #Cheyenne
SET(NETCDFLIB  "${NETCDF}/lib/libnetcdf.so;${NETCDF}/lib/libnetcdff.so")

SET(LIBZ      "/usr/lib64/libz.so")
SET(LIBPNG    "/usr/lib64/libpng.so")
SET(LIBJASPER "/usr/lib64/libjasper.so")
SET(GSILIBS "${GSIBUILD}/lib")
#SET(LIBBUFR "${GSILIBS}/libbufr_v10.2.5.a")
#SET(LIBW3EMC "${GSILIBS}/libw3emc_v2.2.0.a")
#SET(LIBW3NCO "${GSILIBS}/libw3nco_v2.0.6.a")
#SET(LIBBACIO "${GSILIBS}/libbacio_v2.0.1.a")
SET(LIBBUFR "${GSILIBS}/libbufr_v.a")
SET(LIBW3EMC "${GSILIBS}/libw3emc_v.a")
SET(LIBW3NCO "${GSILIBS}/libw3nco_v.a")
SET(LIBBACIO "${GSILIBS}/libbacio_v.a")

SET(LIBW3EMC_UPP "${COMUPP}/lib/libw3emc.a")
SET(LIBW3NCO_UPP "${COMUPP}/lib/libw3nco.a")
SET(LIBBACIO_UPP "${COMUPP}/lib/libbacio.a")
SET(LIBG2     "${COMUPP}/lib/libg2.a")
SET(LIBG2TMPL "${COMUPP}/lib/libg2tmpl.a")
SET(LIBSP     "${COMUPP}/lib/libsp.a")
SET(LIBIP     "${COMUPP}/lib/libip.a")

set(LIBG2_WPS "${WPSBUILD}/ungrib/src/ngl/g2/libg2_4.a")
SET(CONSTANTS "${GSIBUILD}/src/gsi/CMakeFiles/gsilib_shrd.dir/constants.f90.o")

SET(Fortran_FLAGS "-O3 -fp-model precise -assume byterecl -fpe0 -DLINUX -g -traceback")
SET(Fortran_FLAGS_bigEndian "-convert big_endian")
SET(C_FLAGS "-D_UNDERSCORE -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'")

SET(EXESUFFIX "exe")
SET(EXEref2tten "ref2tten.${EXESUFFIX}")
SET(EXEtamdar   "process_tamdar_netcdf.${EXESUFFIX}")
SET(EXEsatcast  "process_satcast_netcdf.${EXESUFFIX}")
SET(EXEsnow     "process_NESDIS_imssnow.${EXESUFFIX}")
SET(EXEcloud    "rap_process_cloud.${EXESUFFIX}")
SET(EXEsst      "process_SST.${EXESUFFIX}")
SET(EXEfvcom    "process_FVCOM.${EXESUFFIX}")

add_subdirectory(mosaic/all)
add_subdirectory(updateGVF)
add_subdirectory(updatebk/netcdf)
add_subdirectory(imssnow/netcdf)
add_subdirectory(lightning/merge/src/baselib)
add_subdirectory(lightning/merge/src/getobsltng)
add_subdirectory(lightning/netcdf_bufr)
add_subdirectory(lightning/netcdf)
add_subdirectory(fvcom)

add_subdirectory(diag_obscount)
add_subdirectory(ref2tten)
add_subdirectory(encode_tamdar)
add_subdirectory(satcast)
add_subdirectory(nasalarc/bufr)
#add_subdirectory(ssthires/netcdf)
add_subdirectory(ssthires/grib2)

