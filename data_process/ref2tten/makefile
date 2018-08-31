# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

WRFDIR=/pan2/projects/nrtrr/alexander/code/hrrr/primary/RR-svn/trunk/WRFV3.6_2015
GSIDIR=/pan2/projects/nrtrr/alexander/code/rapidrefresh/rapid-refresh/GSI
FC   =  mpiifort -free
CC  = mpiicc
FLAGS= -O3 -fp-model precise -assume byterecl -fpe0 -DLINUX #-g -traceback # -convert big_endian
CPPFLAGS =  -D_UNDERSCORE -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'
GSILIBS =  ${GSIDIR}/src/main -lgsi #../../../lib -lw3emc_i4r8 -lw3nco_i4r8 -lbacio -lbufr_i4r8

INCLD=  -I${NETCDF}/include -I${GSIDIR}/include
LIBS =  ${WRFDIR}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf -L${GSILIBS}

OBJS = read_netcdf_mass.o BackgroundCld.o pbl_height.o vinterp_radar_ref.o \
       build_missing_REFcone.o radar_ref2tten.o smooth.o convert_stcst2ref.o convert_lghtn2ref.o read_Lightning_cld.o

all: ref2tten

ref2tten: ${OBJS} gsdcloudanalysis_ref2tten.o
	${FC} -o ref2tten.exe ${FLAGS} gsdcloudanalysis_ref2tten.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
