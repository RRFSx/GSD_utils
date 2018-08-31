# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../configure.process

INCLD=  -I${NETCDF}/include -I../../include
LIBS =  ${WRFDIR}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf  -lnetcdff ## -L../../../lib -lw3emc_i4r8 -lw3nco_i4r8

OBJS = read_netcdf_mass.o

all: check_netcdf_field 

check_netcdf_field: check_NetCDF_fields.o ${OBJS}
	${FC} -o check_netcdf_field.exe ${FLAGS} check_NetCDF_fields.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
