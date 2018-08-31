# makefile for process_satcast
#! /bin/sh -v

# Define the fortran compiler and options

GSIDIR=/pan2/projects/nrtrr/alexander/code/rapidrefresh/rapid-refresh/GSI
GSILIBS = ${GSIDIR}/lib -lw3emc_i4r8 -lw3nco_i4r8 -lbacio -lbufr_i4r8
FC   =  mpiifort -free
CC   = mpiicc
FLAGS= -O3 -fp-model precise -assume byterecl -fpe0 -g -traceback $(LDFLAGS) -DLINUX 
CPPFLAGS =  -D_UNDERSCORE -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'

INCLD=  -I${NETCDF}/include -I ${GSIDIR}/include
LIBS =  -L${NETCDF}/lib -lnetcdf -L${GSILIBS} \
        ${GSIDIR}/src/main/constants.o

OBJS = netCDFsub.o netCDFsub_geo.o \
       cio.o module_debug.o misc_definitions_module.o constants_module.o \
       module_map_utils.o gridmod_gsi_map_utils.o outqv.o \
       write_bufr_satcast.o process_satcast.o

all: process_satcast

process_satcast: ${OBJS}
	${FC} -o process_satcast.exe ${FLAGS} ${OBJS} ${LIBS} 


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
