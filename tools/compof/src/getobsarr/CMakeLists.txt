
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
include_directories ("${BASEPATH}/src")
include_directories ("${PROJECT_BINARY_DIR}/getobslist")
SET(SRC_LIST module_obs_conv_array.f90 get_obs_array.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY(obsarray module_obs_conv_array.f90)
TARGET_LINK_LIBRARIES(obsarray obsbase)
TARGET_LINK_LIBRARIES(obsarray ${NETCDF_LIB})

ADD_EXECUTABLE(get_obs_array.exe get_obs_array.f90)
TARGET_LINK_LIBRARIES(get_obs_array.exe obsarray)
#TARGET_LINK_LIBRARIES(get_obs_array.exe ${NETCDF_LIB})
