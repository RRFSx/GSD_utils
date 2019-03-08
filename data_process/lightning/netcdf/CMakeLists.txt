
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("${GSIBUILD}/include")

SET(SRC_LIST Check_Alaska.f90 Check_NLDN.f90 netCDFsub_geo.f90 netCDFsub_lightning.f90 misc_definitions_module.f90 constants_module.f90 module_map_utils.f90 gridmod_gsi_map_utils.f90 write_bufr_lght.f90 module_debug.F90)
SET(CSRC_LIST cio.c)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})
set_source_files_properties(${CSRC_LIST} PROPERTIES COMPILE_FLAGS ${C_FLAGS})
set_source_files_properties(process_Lightning.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_EXECUTABLE(process_Lightning_nc.exe process_Lightning.f90 ${SRC_LIST} ${CSRC_LIST})
TARGET_LINK_LIBRARIES(process_Lightning_nc.exe ${NETCDFLIB} ${LIBBUFR} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP} ${LIBBACIO} ${GSIBUILD}/src/CMakeFiles/gsilib_shrd.dir/constants.f90.o )

