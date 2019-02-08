cmake_minimum_required(VERSION 2.8)

project(data_process)
enable_language (Fortran)
enable_language (C)

SET(GSIBUILD  "/glade/work/geguo/MPMC/b_release_v3.7/ci06.intel_17.0.1.mpt_2.15f")
set(COMUPP "/gpfs/fs1/p/ral/jntp/mhu/rapcode/GSD_UPP/comupp")
set(wrfframe "/glade/p/ral/jntp/DAtask/chunhua/HRRR-2018/code/rapid-refresh/WRFV3.9/frame/pack_utils.o")
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
SET(NETCDFLIB  "${NETCDF}lib/libnetcdf.a;${NETCDF}lib/libnetcdff.a")
SET(MPILIB  "/glade/u/apps/ch/os/usr/lib64/libpthread.so;/opt/sgi/mpt/mpt-2.15/lib/libmpi.so")
SET(LIBIOMP5  "/glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/lib/intel64_lin/libiomp5.so")

SET(LIBZ      "/usr/lib64/libz.so")
SET(LIBPNG    "/usr/lib64/libpng.so")
SET(LIBJASPER "/usr/lib64/libjasper.so")
SET(GSILIBS "${GSIBUILD}/lib")
SET(LIBBUFR "${GSILIBS}/libbufr_v10.2.5.a")
SET(LIBW3EMC "${GSILIBS}/libw3emc_v2.2.0.a")
SET(LIBW3NCO "${GSILIBS}/libw3nco_v2.0.6.a")
SET(LIBBACIO "${GSILIBS}/libbacio_v2.0.1.a")
SET(LIBG2     "${COMUPP}/lib/libg2.a")
SET(LIBG2TMPL "${COMUPP}/lib/libg2tmpl.a")
SET(LIBSP     "${COMUPP}/lib/libsp.a")
SET(LIBIP     "${COMUPP}/lib/libip.a")
SET(CONSTANTS "${GSIBUILD}/src/CMakeFiles/gsilib_shrd.dir/constants.f90.o")

SET(Fortran_FLAGS "-O3 -fp-model precise -assume byterecl -fpe0 -DLINUX")
SET(C_FLAGS "-D_UNDERSCORE -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'")

SET(EXESUFFIX "exe")
SET(EXEref2tten "ref2tten.${EXESUFFIX}")
SET(EXEtamdar   "process_tamdar_netcdf.${EXESUFFIX}")
SET(EXEsatcast  "process_satcast_netcdf.${EXESUFFIX}")
SET(EXEsnow     "process_NESDIS_imssnow.${EXESUFFIX}")
SET(EXEcloud    "rap_process_cloud")
SET(EXEsst      "process_SST.${EXESUFFIX}")


#add_subdirectory(diag_obscount)
#add_subdirectory(ref2tten)
#add_subdirectory(encode_tamdar)
#add_subdirectory(lightning/merge/src/baselib)
#add_subdirectory(lightning/merge/src/getobsltng)
#add_subdirectory(mosaic/all)
#add_subdirectory(satcast)
#add_subdirectory(imssnow/netcdf)
#add_subdirectory(nasalarc/bufr)
#add_subdirectory(ssthires/netcdf)
#add_subdirectory(updatebk/netcdf)
add_subdirectory(updateGVF)



