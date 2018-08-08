
include_directories ("${BASEPATH}/src")
#include_directories ("${PROJECT_BINARY_DIR}/getobslist")
#include_directories ("${PROJECT_BINARY_DIR}/getobsarr")
#include_directories ("${PROJECT_BINARY_DIR}/getfcstobs")
include_directories ("${PROJECT_BINARY_DIR}/getpairs")

SET(SRC_LIST read_statistic_results.f90 module_readwrt_stats_results.f90)
set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY(readwrt_statistic module_readwrt_stats_results.f90)
TARGET_LINK_LIBRARIES(readwrt_statistic statistic stationarray)

ADD_EXECUTABLE(read_results.exe read_statistic_results.f90)
TARGET_LINK_LIBRARIES(read_results.exe readwrt_statistic)
