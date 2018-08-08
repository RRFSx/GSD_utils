#! /bin/sh -v

# Define the fortran compiler and options
 FC  =  ifort
 CC  =  icc
 FLAGS=  -g -traceback -ftz -O3
#
 INCLD=   -I${NETCDF}/include # -I${UNIPOSTPATH}/include
 LIBS =   -L${NETCDF}/lib -lnetcdf -lnetcdff
#         -L${UNIPOSTPATH}/lib -lw3emc -lw3nco -lbacio -lg2 -lg2tmpl \
#                 -L/usr/lib64 -ljasper -lpng -lz
#
#
OBJS = kinds.o module_map_utils.o module_ncio.o module_time.o

#constants.o gridmod_gsi_map_utils.o get_gridcord.o gen_fcstobs_grib2.o gen_sfc_obs.o \
       read_config_mod.o gen_nat_obs.o gen_prs_obs.o read_grib2_mod.o
## OBJS = read_grib1.o

all: baselib

baselib: ${OBJS} testlib.o
	${FC} -o testlib.exe ${FLAGS} testlib.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.mod *.exe


kind.o : kind.f90
