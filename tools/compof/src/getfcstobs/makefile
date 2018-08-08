#! /bin/sh -v

# Define the fortran compiler and options
 FC  =  ifort
 CC  =  icc
 FLAGS=  -g -traceback -ftz -O3
#
 INCLD=   -I${NETCDF}/include -I../baselib -I../getobsarr -I../getobslist
 LIBS =   -L${NETCDF}/lib -lnetcdf -lnetcdff
#
#
OBJSBASE = ../baselib/kinds.o ../baselib/module_time.o \
           ../baselib/module_map_utils.o ../baselib/module_ncio.o \
           ../getobslist/module_obs_base.o ../getobsarr/module_obs_conv_array.o
           

OBJS = module_fcstobs.o gen_fcstobs.o

all: gen_fcstobs

gen_fcstobs: ${OBJS}
	${FC} -o gen_fcstobs.exe ${FLAGS} ${OBJSBASE} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.mod *.exe


