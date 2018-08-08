MESSAGE(STATUS "NETCDF dirctory: $ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")

set(SRC_BASE kinds.f90 module_map_utils.f90 module_time.f90)
set_source_files_properties(module_ncio.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
set_source_files_properties(${SRC_BASE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

ADD_LIBRARY (ncio STATIC module_ncio.f90)
ADD_LIBRARY (base STATIC ${SRC_BASE})

ADD_EXECUTABLE (testlib.exe testlib.f90)
TARGET_LINK_LIBRARIES(testlib.exe base ncio)
set_target_properties(testlib.exe PROPERTIES LINK_FLAGS "-O3")
TARGET_LINK_LIBRARIES(testlib.exe ${NETCDF_LIB})

