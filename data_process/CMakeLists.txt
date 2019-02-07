cmake_minimum_required(VERSION 2.8)

project(data_process)
enable_language (Fortran)
enable_language (C)

SET(GSIBUILD  "/glade/work/geguo/MPMC/b_release_v3.7/ci06.intel_17.0.1.mpt_2.15f")
set(COMUPP "/gpfs/fs1/p/ral/jntp/mhu/rapcode/GSD_UPP/comupp")
SET(NETCDF  $ENV{NETCDF})

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

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
SET(LIBSP     "${COMUPP}/lib/libsp.a")
SET(LIBG2TMPL "${COMUPP}/lib/libg2tmpl.a")
SET(CONSTANTS "${GSIBUILD}/src/CMakeFiles/gsilib_shrd.dir/constants.f90.o")

SET(Fortran_FLAGS "-O3 -fp-model precise -assume byterecl -fpe0 -DLINUX")
SET(C_FLAGS "-D_UNDERSCORE -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'")

SET(EXESUFFIX "exe")
SET(EXEref2tten "ref2tten.${EXESUFFIX}")
SET(EXEtamdar   "process_tamdar_netcdf.${EXESUFFIX}")
SET(EXEsatcast  "process_satcast_netcdf.${EXESUFFIX}")


add_subdirectory(diag_obscount)
add_subdirectory(ref2tten)
add_subdirectory(encode_tamdar)
add_subdirectory(lightning/merge/src/baselib)
add_subdirectory(lightning/merge/src/getobsltng)
add_subdirectory(mosaic/all)
add_subdirectory(satcast)



