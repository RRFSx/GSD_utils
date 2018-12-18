 
MESSAGE(STATUS "NETCDF dirctory: $ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("${BASELIBPATH}/src")

set(SRC_CODE cal_spread.f90 module_cal_spread.f90)
set(SRC_CODEMPI cal_spread_mpi.f90 module_cal_spread.f90)
set_source_files_properties(${SRC_CODE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
set_source_files_properties(${SRC_CODEMPI} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

ADD_EXECUTABLE (cal_spread.exe ${SRC_CODE})
ADD_EXECUTABLE (cal_spread_mpi.exe ${SRC_CODEMPI})
TARGET_LINK_LIBRARIES(cal_spread.exe ${BASELIBPATH}/lib/libbase.a)
TARGET_LINK_LIBRARIES(cal_spread_mpi.exe ${BASELIBPATH}/lib/libbase.a)
#set_target_properties(testlib.exe PROPERTIES LINK_FLAGS "-O3")

ADD_EXECUTABLE (cut_spread.exe cut_spread.f90)
TARGET_LINK_LIBRARIES(cut_spread.exe ${BASELIBPATH}/lib/libncio.a ${NETCDF_LIB})
