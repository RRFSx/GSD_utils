cmake_minimum_required(VERSION 2.8)

set(Fortran_FLAGS_ALL "${Fortran_FLAGS} ${Fortran_FLAGS_bigEndian}")

set_source_files_properties(read_diag_rad.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS_ALL})
set_source_files_properties(read_diag_conv.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS_ALL})

include_directories( ${GSIINC} ${NETCDFINC} )

add_executable(count_obs.exe count_obs.f90)
set_target_properties( count_obs.exe  PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS_ALL})

add_executable(read_diag_rad.exe read_diag_rad.f90)
set_target_properties( read_diag_rad.exe PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS_ALL})

add_executable(read_diag_conv.exe read_diag_conv.f90)
set_target_properties( read_diag_conv.exe PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS_ALL})
