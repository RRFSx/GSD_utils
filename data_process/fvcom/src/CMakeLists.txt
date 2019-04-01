
set(ENV{CC} "icc")
set(ENV{CXX} "icpc")
set(ENV{FC} "ifort")

cmake_minimum_required (VERSION 2.8)

project (Lightning)

enable_language (Fortran)

MESSAGE( STATUS "Binary directory " ${PROJECT_BINARY_DIR})
MESSAGE( STATUS "SOURCE directory " ${PROJECT_SOURCE_DIR})

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(BUFRPATH "/lfs3/projects/rtwbl/ejames/wrf_code_svn/GSI_74387abe/lib")
SET(NETCDF_LIB "-L$ENV{NETCDF4}/lib/ -lnetcdf -lnetcdff")
# SET(NETCDF_LIB "-L$ENV{NETCDF}/lib/ -lnetcdf")

SET(Fortran_FLAGS "-O0 -fp-model source -check bounds -g -traceback")

add_subdirectory (${PROJECT_SOURCE_DIR}/baselib)
add_subdirectory (${PROJECT_SOURCE_DIR}/getfvcom)
