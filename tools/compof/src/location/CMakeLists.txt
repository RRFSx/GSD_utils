
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
include_directories ("${BASEPATH}/src")
SET(SRC_LIST module_obs_location.f90 find_location.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY(obslocation module_obs_location.f90)
TARGET_LINK_LIBRARIES(obslocation obsbase)
TARGET_LINK_LIBRARIES(obslocation ${NETCDF_LIB})

ADD_EXECUTABLE(get_obs_location.exe find_location.f90)
TARGET_LINK_LIBRARIES(get_obs_location.exe obslocation)
