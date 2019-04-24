MESSAGE(STATUS "NETCDF dirctory: $ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("${BASELIBPATH}/src")

set(SRC_ENCODERW calrefl.f90 refl_thompson.f90)
set_source_files_properties(${SRC_ENCODERW} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_EXECUTABLE (calculate_refl.exe ${SRC_ENCODERW})
TARGET_LINK_LIBRARIES(calculate_refl.exe ${BASELIBPATH}/lib/libncio.a)
TARGET_LINK_LIBRARIES(calculate_refl.exe ${NETCDF_LIB})
