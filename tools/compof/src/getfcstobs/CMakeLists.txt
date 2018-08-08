
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
include_directories ("${BASEPATH}/src")
include_directories ("${PROJECT_BINARY_DIR}/getobslist")
include_directories ("${PROJECT_BINARY_DIR}/getobsarr")
SET(SRC_LIST gen_fcstobs.f90  module_fcstobs.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY(fcstobs  module_fcstobs.f90)
TARGET_LINK_LIBRARIES(fcstobs obsbase obsarray)
TARGET_LINK_LIBRARIES(fcstobs ${NETCDF_LIB})

ADD_EXECUTABLE(get_fcstobs.exe gen_fcstobs.f90)
TARGET_LINK_LIBRARIES(get_fcstobs.exe fcstobs)
