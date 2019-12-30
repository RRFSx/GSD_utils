 
MESSAGE(STATUS "NETCDF dirctory: $ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("${BASELIBPATH}/src")

set(SRC_CODE cut_pert.f90 module_interp_pert.f90)
set_source_files_properties(${SRC_CODE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
set_source_files_properties(${SRC_CODEMPI} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

ADD_EXECUTABLE (cut_pert.exe ${SRC_CODE})
TARGET_LINK_LIBRARIES(cut_pert.exe ${BASELIBPATH}/lib/libbase.a ${BASELIBPATH}/lib/libncio.a ${NETCDF_LIB})

ADD_EXECUTABLE (plot_spread.exe plot_spread.f90)
TARGET_LINK_LIBRARIES(plot_spread.exe ${BASELIBPATH}/lib/libncio.a ${NETCDF_LIB})
