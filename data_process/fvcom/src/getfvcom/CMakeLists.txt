
INCLUDE_DIRECTORIES("$ENV{NETCDF4}/include")
include_directories ("${PROJECT_BINARY_DIR}/baselib")
# SET(BUFRPATH "/scratch3/BMC/wrfruc/mhu/rapcode/git_local/work/GSI_201707b_5c5800c/lib/")
SET(SRC_LIST module_nwp_base.f90 module_nwp.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})
set_source_files_properties(process_FVCOM.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY (nwpbase ${SRC_LIST})
TARGET_LINK_LIBRARIES(nwpbase base ncio)
TARGET_LINK_LIBRARIES(nwpbase ${NETCDF_LIB})

ADD_EXECUTABLE(process_FVCOM.exe process_FVCOM.f90)
TARGET_LINK_LIBRARIES(process_FVCOM.exe nwpbase base ncio)
TARGET_LINK_LIBRARIES(process_FVCOM.exe ${BUFRPATH}/libbufr_i4r8.a)
TARGET_LINK_LIBRARIES(process_FVCOM.exe ${BUFRPATH}/libw3emc_i4r8.a)
TARGET_LINK_LIBRARIES(process_FVCOM.exe ${BUFRPATH}/libw3nco_i4r8.a)
TARGET_LINK_LIBRARIES(process_FVCOM.exe ${BUFRPATH}/libbacio.a)
