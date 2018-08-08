#! /bin/sh -v

 GSILIB=/scratch3/BMC/wrfruc/mhu/rapcode/git_local/work/GSI_201707b_5c5800c/lib
# Define the fortran compiler and options
 FC  =  ifort
 CC  =  icc
 FLAGS=  -g -traceback -ftz -O3
#
 INCLD=   -I${NETCDF}/include -I../baselib -I../getobslist
 LIBS =   -L${NETCDF}/lib -lnetcdf -lnetcdff 
#
OBJSBASE = ../baselib/kinds.o ../baselib/module_map_utils.o ../baselib/module_ncio.o \
           ../getobslist/module_obs_base.o ../getobslist/module_obs_conv_pt.o

OBJS = module_obs_conv_array.o get_obs_array.o

all: get_obs_array

get_obs_array: ${OBJS}
	${FC} -o get_obs_array.exe ${FLAGS} ${OBJSBASE} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	rm -f *.o *.mod *.exe
