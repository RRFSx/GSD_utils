
include_directories ("${BASEPATH}/src")
include_directories ("${PROJECT_BINARY_DIR}/getobslist")
include_directories ("${PROJECT_BINARY_DIR}/getobsarr")
include_directories ("${PROJECT_BINARY_DIR}/getfcstobs")
SET(SRC_LIST get_pairs.f90 module_hashsearch.f90  module_pairs.f90  module_stationarray.f90  module_statistic.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY(hashsearch module_hashsearch.f90)
TARGET_LINK_LIBRARIES(hashsearch obsbase)

ADD_LIBRARY(fcstobs_pairs  module_pairs.f90)
TARGET_LINK_LIBRARIES(fcstobs_pairs obsbase obsarray fcstobs)

ADD_LIBRARY(stationarray module_stationarray.f90)
TARGET_LINK_LIBRARIES(stationarray obsbase obsarray hashsearch)

ADD_LIBRARY(statistic  module_statistic.f90)
TARGET_LINK_LIBRARIES(statistic fcstobs_pairs stationarray)

ADD_EXECUTABLE(get_pairs.exe get_pairs.f90)
TARGET_LINK_LIBRARIES(get_pairs.exe statistic stationarray)

