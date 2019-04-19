! process_FVCOM.f90
! Eric James
! NOAA/OAR/ESRL/GSD/ADB
! 12 Dec 2018
!
! This is the code to grab lake surface temperature and aerial ice
! concentration from GLERL-provided FVCOM forecast files (which have
! already been mapped to HRRR grid).  The code uses new object-oriented
! coding to handle the data, performing some minimal QC.
!
! The executable is now created via cmake.

program process_FVCOM

   use module_map_utils, only: map_util
   use kinds, only: r_kind, i_kind, r_single
   use module_ncio, only: ncio
   use module_nwp, only: fcst_nwp

   implicit none

!  New object-oriented declarations

   type(ncio) :: geo
   type(fcst_nwp) :: fcst
   type(map_util) :: map

!  Grid variables

   character*180 :: geofile
   character*2 :: workPath
   character*1 :: char1

   integer :: MAP_PROJ, NLON, NLAT, NLEV
   integer :: hrrrlon, hrrrlat, hrrrtimes, hrrrlevs
   integer :: fvlon, fvlat, fvtimes
   integer :: i, j, k, t1, t2

   real :: rad2deg = 180.0/3.1415926
   real :: userDX, userDY, CEN_LAT, CEN_LON
   real :: userTRUELAT1, userTRUELAT2, MOAD_CEN_LAT, STAND_LON
   real :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
   real :: knowni, knownj, dx
   real :: one, pi, deg2rad

!  For FVCOM and HRRR data

   character*180 :: hrrrfile
   character*180 :: fvcomfile

   real(r_single), allocatable :: hrrrice(:,:), hrrrsst(:,:)
   real(r_single), allocatable :: hrrrsfcT(:,:), hrrrmask(:,:)
   real(r_single), allocatable :: hrrrland(:,:), hrrrlu(:,:)
   real(r_single), allocatable :: hrrrsnow(:,:), hrrrsnowh(:,:), hrrrsnowc(:,:)
   integer, allocatable :: hrrrsltyp(:,:), hrrrvgtyp(:,:)
   real(r_single), allocatable :: soilm(:,:,:)
   real(r_single), allocatable :: fvice(:,:), fvsst(:,:)
   real(r_single), allocatable :: fvsfcT(:,:), fvmask(:,:)
   real(r_single), allocatable :: fvland(:,:), fvlu(:,:)
   real(r_single), allocatable :: fvsnow(:,:), fvsnowh(:,:), fvsnowc(:,:)
   integer, allocatable :: fvsltyp(:,:), fvvgtyp(:,:)
   real(r_single) :: xice_threshold
   integer :: fractional_seaice

!  Declare namelists
!  SETUP (general control namelist) :

   integer :: update_type

   namelist/setup/update_type, t2

   read(5,setup)
   write(*,*) 'From namelist: update type is ', update_type
   write(*,*) 'From namelist: t2 is ', t2

!  Set up sea ice threshold

   fractional_seaice = 1
   if (fractional_seaice == 0) then
      xice_threshold = 0.5
      write(*,*) 'Do not use fractional sea ice'
   else if (fractional_seaice == 1) then
      xice_threshold = 0.02
      write(*,*) 'Use fraction sea ice'
   endif

!  Set geogrid fle name and obtain grid parameters

   workPath='./'
   write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'
   write(*,*) 'geofile', trim(geofile)
   call geo%open(trim(geofile),'r',200)
   call geo%get_dim("west_east",NLON)
   call geo%get_dim("south_north",NLAT)
   write(*,*) 'NLON,NLAT:', NLON, NLAT
   call geo%get_att("DX",userDX)
   call geo%get_att("DY",userDY)
   call geo%get_att("CEN_LAT",CEN_LAT)
   call geo%get_att("CEN_LON",CEN_LON)
   call geo%get_att("TRUELAT1",userTRUELAT1)
   call geo%get_att("TRUELAT2",userTRUELAT2)
   call geo%get_att("MOAD_CEN_LAT",MOAD_CEN_LAT)
   call geo%get_att("STAND_LON",STAND_LON)
   call geo%get_att("MAP_PROJ",MAP_PROJ)
   call geo%close
   write(*,*) userDX, userDY, CEN_LAT, CEN_LON
   write(*,*) userTRUELAT1, userTRUELAT2, MOAD_CEN_LAT, STAND_LON, MAP_PROJ

   write(*,*) 'Finished reading geofile.'
   write(*,*) ' '

   nlev = 9

!  Allocate variables for I/O

   allocate(hrrrice(nlon,nlat))
   allocate(hrrrsfcT(nlon,nlat))
   allocate(hrrrsst(nlon,nlat))
   allocate(hrrrmask(nlon,nlat))
   allocate(hrrrsltyp(nlon,nlat))
   allocate(hrrrvgtyp(nlon,nlat))
   allocate(hrrrland(nlon,nlat))
   allocate(hrrrlu(nlon,nlat))
   allocate(hrrrsnow(nlon,nlat))
   allocate(hrrrsnowc(nlon,nlat))
   allocate(hrrrsnowh(nlon,nlat))
   allocate(soilm(nlon,nlat,nlev))

   allocate(fvice(nlon,nlat))
   allocate(fvsfcT(nlon,nlat))
   allocate(fvsst(nlon,nlat))
   allocate(fvmask(nlon,nlat))
   allocate(fvsltyp(nlon,nlat))
   allocate(fvvgtyp(nlon,nlat))
   allocate(fvland(nlon,nlat))
   allocate(fvlu(nlon,nlat))
   allocate(fvsnow(nlon,nlat))
   allocate(fvsnowc(nlon,nlat))
   allocate(fvsnowh(nlon,nlat))

!  Read HRRR input datasets

   hrrrfile='wrf_inout'
   t1=1

   call fcst%initial(' HRRR')
   call fcst%list_initial
   call fcst%read_n(trim(hrrrfile),'HRRR',hrrrlon,hrrrlat,hrrrtimes,t1,hrrrmask,hrrrsst,hrrrice,hrrrsfcT,hrrrsltyp,hrrrvgtyp,hrrrland,hrrrlu,hrrrsnow,hrrrsnowh,hrrrsnowc)
   call fcst%read_n3(trim(hrrrfile),'HRRR',hrrrlon,hrrrlat,hrrrlevs,hrrrtimes,t1,soilm)
   call fcst%finish

!  Check that the dimensions match

   if (hrrrlon .ne. nlon .or. hrrrlat .ne. nlat ) then
      write(*,*) 'ERROR: HRRR/geofile dimensions do not match:'
      write(*,*) 'hrrrlon: ', hrrrlon
      write(*,*) 'nlon: ', nlon
      write(*,*) 'hrrrlat: ', hrrrlat
      write(*,*) 'nlat: ', nlat
      stop 123
   endif

   write(*,*) 'hrrrlevs: ', hrrrlevs
   write(*,*) 'hrrrtimes: ', hrrrtimes
   write(*,*) 'time to use: ', t1

!  Read FVCOM input datasets

   fvcomfile='fvcom.nc'

   call fcst%initial('FVCOM')
   call fcst%list_initial
   call fcst%read_n(trim(fvcomfile),'FVCOM',fvlon,fvlat,fvtimes,t2,fvmask,fvsst,fvice,fvsfcT,fvsltyp,fvvgtyp,fvland,fvlu,fvsnow,fvsnowh,fvsnowc)
   call fcst%finish

!  Check that the dimensions match

   if (fvlon .ne. nlon .or. fvlat .ne. nlat) then
      write(*,*) 'ERROR: FVCOM/geofile dimensions do not match:'
     write(*,*) 'fvlon: ', fvlon
     write(*,*) 'nlon: ', nlon
     write(*,*) 'fvlat: ', fvlat
     write(*,*) 'nlat: ', nlat
     stop 135
   endif

   write(*,*) 'fvtimes: ', fvtimes
   write(*,*) 'time to use: ', t2

!  Update with FVCOM fields
!  Use FVCOM values for SST, TSK, and SEAICE
!  Ensure consistency for LU_INDEX, XLAND, IVGTYP, ISLTYP, LANDMASK

   do j=1,nlat
      do i=1,nlon
         if (fvmask(i,j) .gt. 0.) then
            if (fvsst(i,j) .ge. -80.0) then
               hrrrice(i,j) = fvice(i,j)
               hrrrsst(i,j) = fvsst(i,j) + 273.15
               hrrrsfcT(i,j) = fvsst(i,j) + 273.15
            endif
            do k=1,hrrrlevs
               soilm(i,j,k) = 1.0
            enddo
            if (fvice(i,j) .lt. xice_threshold) then
               hrrrsltyp(i,j) = 14
               hrrrvgtyp(i,j) = 17
               hrrrmask(i,j) = 0.0
               hrrrlu(i,j) = 17.0
               hrrrland(i,j) = 2.0
            else
               hrrrsltyp(i,j) = 16
               hrrrvgtyp(i,j) = 15
               hrrrmask(i,j) = 1.0
               hrrrlu(i,j) = 15.0
               hrrrland(i,j) = 1.0
            endif
            if (fvice(i,j) < 0.001 .and. hrrrsnow(i,j) > 0.0) then
               hrrrsnow(i,j) = 0.0
               hrrrsnowh(i,j) = 0.0
               hrrrsnowc(i,j) = 0.0
            endif
         endif
      enddo
   enddo

! Write out HRRR file again

   call geo%open(trim(hrrrfile),'w',300)
   if (update_type .eq. 1) then
      call geo%replace_var("SST",NLON,NLAT,hrrrsst)
      call geo%replace_var("TSK",NLON,NLAT,hrrrsfcT)
      call geo%replace_var("LANDMASK",NLON,NLAT,hrrrmask)
   else
      call geo%replace_var("SEAICE",NLON,NLAT,hrrrice)
      call geo%replace_var("ISLTYP",NLON,NLAT,hrrrsltyp)
      call geo%replace_var("IVGTYP",NLON,NLAT,hrrrvgtyp)
      call geo%replace_var("LANDMASK",NLON,NLAT,hrrrmask)
      call geo%replace_var("XLAND",NLON,NLAT,hrrrland)
      call geo%replace_var("LU_INDEX",NLON,NLAT,hrrrlu)
      call geo%replace_var("SNOW",NLON,NLAT,hrrrsnow)
      call geo%replace_var("SNOWH",NLON,NLAT,hrrrsnowh)
      call geo%replace_var("SNOWC",NLON,NLAT,hrrrsnowc)
      call geo%replace_var("SMOIS",NLON,NLAT,hrrrlevs,soilm)
   endif
   call geo%close

end program process_FVCOM
