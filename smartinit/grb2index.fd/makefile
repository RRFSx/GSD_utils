################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	grb2index
FOPTS=  -g -O3 -convert big_endian   -axCORE-AVX2
LOPTS=	
INCS=	
OBJS=	grb2index.o
#LIBS=	-L/nwprod/lib -lg2_4 -lw3nco_4 -lbacio_4
UPPDIR=/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/UPP_20180521_745c5c/comupp
G2_LIB4=/mnt/lfs1/projects/rtwbl/mhu/rapcode/g2/lib/libg2_4.a
W3NCO_LIB4=${UPPDIR}/lib/libw3nco.a
BACIO_LIB4=${UPPDIR}/lib/libbacio.a

LIBS=   ${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}
################################################################################
$(CMD):	$(OBJS)
	ifort $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	makefile $(INCS)
grb2index.o:	grb2index.f
	ifort -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
