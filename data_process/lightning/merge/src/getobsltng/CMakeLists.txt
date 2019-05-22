
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
include_directories ("${PROJECT_BINARY_DIR}/lightning/merge/src/baselib")
# SET(BUFRPATH "/scratch3/BMC/wrfruc/mhu/rapcode/git_local/work/GSI_201707b_5c5800c/lib/")
SET(SRC_LIST module_lightning_base.f90 module_lightning.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})
set_source_files_properties(process_Lightning.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY (lightningbase ${SRC_LIST})
add_dependencies(lightningbase base)
TARGET_LINK_LIBRARIES(lightningbase ${LIBBACIO} ${NETCDFLIB})

ADD_EXECUTABLE(process_Lightning.exe process_Lightning.f90)
add_dependencies(process_Lightning.exe base)
TARGET_LINK_LIBRARIES(process_Lightning.exe lightningbase base ncio ${LIBBUFR} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP} ${LIBBACIO} )
