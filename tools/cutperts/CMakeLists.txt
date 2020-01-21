set(ENV{CC} "icc")
set(ENV{CXX} "icpc")
set(ENV{FC} "mpif90 -f90=ifort")
#set(ENV{FC} "ifort")

cmake_minimum_required (VERSION 2.8)

project (cutPert)

enable_language (Fortran)

MESSAGE( STATUS "Binary directory " ${PROJECT_BINARY_DIR})
MESSAGE( STATUS "SOURCE directory " ${PROJECT_SOURCE_DIR})

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(BASELIBPATH "/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/util/GSD_UTL/tools/baselib/build")
SET(NETCDF_LIB "-L$ENV{NETCDF}/lib -lnetcdf -lnetcdff")

SET(Fortran_FLAGS "-O0 -fp-model source -check bounds -g -traceback")

add_subdirectory (${PROJECT_SOURCE_DIR}/src)

