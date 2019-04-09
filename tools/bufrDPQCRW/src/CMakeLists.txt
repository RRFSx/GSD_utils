 
MESSAGE(STATUS "NETCDF dirctory: $ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("${BASELIBPATH}/src")

set(SRC_ENCODERW EncodeRW_DPQC.f90 module_RW_DPQC.f90 module_radar_station_config.f90 module_RW_dealiasing.f90 module_background.f90)
set_source_files_properties(${SRC_ENCODERW} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

ADD_EXECUTABLE (encoderw_dealiasing.exe ${SRC_ENCODERW})
TARGET_LINK_LIBRARIES(encoderw_dealiasing.exe ${BASELIBPATH}/lib/libbase.a ${BASELIBPATH}/lib/libncio.a)
#set_target_properties(testlib.exe PROPERTIES LINK_FLAGS "-O3")
TARGET_LINK_LIBRARIES(encoderw_dealiasing.exe ${NETCDF_LIB} ${BUFRPATH})

