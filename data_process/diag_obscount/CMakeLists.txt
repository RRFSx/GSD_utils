cmake_minimum_required(VERSION 2.8)

set_source_files_properties(read_diag_rad.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
set_source_files_properties(read_diag_conv.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} )

add_executable(read_diag_rad.exe read_diag_rad.f90)
set_target_properties( read_diag_rad.exe PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

add_executable(read_diag_conv.exe read_diag_conv.f90)
set_target_properties( read_diag_conv.exe PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
