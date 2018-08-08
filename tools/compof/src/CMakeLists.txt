
set(ENV{CC} "icc")
set(ENV{CXX} "icpc")
set(ENV{FC} "ifort")

cmake_minimum_required (VERSION 2.8)

project (GetPairs)

enable_language (Fortran)

MESSAGE( STATUS "Binary directory " ${PROJECT_BINARY_DIR})
MESSAGE( STATUS "SOURCE directory " ${PROJECT_SOURCE_DIR})

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(BUFRPATH "/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/buildgsi/lib")
SET(BASEPATH "/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/temp/GSD_UTL/tools/buildbase")
SET(NETCDF_LIB "-L$ENV{NETCDF}/lib/ -lnetcdf -lnetcdff")
# SET(NETCDF_LIB "-L$ENV{NETCDF}/lib/ -lnetcdf")

SET(Fortran_FLAGS "-O0 -fp-model source -check bounds -g -traceback")

#add_subdirectory (${PROJECT_SOURCE_DIR}/baselib)
add_subdirectory (${PROJECT_SOURCE_DIR}/getobslist)
add_subdirectory (${PROJECT_SOURCE_DIR}/getobsarr)
add_subdirectory (${PROJECT_SOURCE_DIR}/getfcstobs)
add_subdirectory (${PROJECT_SOURCE_DIR}/getpairs)
add_subdirectory (${PROJECT_SOURCE_DIR}/plot)
add_subdirectory (${PROJECT_SOURCE_DIR}/location)
