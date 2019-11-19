cmake_minimum_required(VERSION 2.8)

file(GLOB src_all  netCDFsub_geo.f90 surface_consistent_check_netcdf_mass.f90 )
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} )

set(EXE ${EXEsnow_check})
add_executable(${EXE} ${src_all})
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${wrflib} ${NETCDFLIB} ) 

