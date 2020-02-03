cmake_minimum_required(VERSION 2.8)

file(GLOB src_all  merge_mosaic_aqpi.f90 readwrite_grib2_mod.f90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} ${G2INC} )

set(EXE "merge_mosaic_aqpi.exe")
add_executable(${EXE} ${src_all})
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${GSILIB} ${LIBG2_WPS} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${MPILIB} ${LIBBACIO} ${LIBSP} ${LIBIOMP5} )
