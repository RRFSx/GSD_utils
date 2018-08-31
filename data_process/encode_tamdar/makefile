# makefile for data process nasalarc global
# use netcdf/4.1.3-intel
# # data : /misc/public/data/acars/raw/netcdf
#
#! /bin/sh -v

# Define the fortran compiler and options


WRFDIR=/pan2/projects/nrtrr/alexander/code/rap/primary/RR-svn/trunk/WRFV3.6

FC   =  ifort -free
CC  = cc
FLAGS= -O3 -fp-model precise -g -traceback $(LDFLAGS) -DLINUX ## -convert big_endian
GSILIBS =../../lib -lw3emc_i4r8 -lw3nco_i4r8 -lbacio -lbufr_i4r8

INCLD=  -I${NETCDF4}/include -I../../include
LIBS =  -L${NETCDF4}/lib -lnetcdf -lnetcdff -L${GSILIBS} \
	../../src/main/constants.o

OBJS = read_tamdar_netcdf.o process_tamdar_netcdf.o Atmosphere.o ruc_saturation.o append_tamdar.o

all: process_tamdar_netcdf

process_tamdar_netcdf: ${OBJS}
	${FC} -o process_tamdar_netcdf.exe ${FLAGS} ${OBJS} ${LIBS} 


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
