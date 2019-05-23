program  process_NASALaRC_global_cloud
!
!   PRGMMR: Terra Ladwig          ORG: GSD        DATE: 2015-04-20
!
! ABSTRACT: 
!     This code is based on process_NASALaRC_cloud
!     This routine reads in NASA LaRC global cloud products (netcdf)
!     and write them to a bufr file. 
!     Mapping to the analysis grid is now done within gsi.
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind,r_single

  implicit none

  INCLUDE 'netcdf.inc'

  character*256 output_file


!  For NASA LaRC 
  CHARACTER*80   satfile
  INTEGER ::   nxp, nyp  ! dimension
  
!     ****VARIABLES FOR THIS NETCDF FILE****
  CHARACTER*40 :: cbase_time
  REAL*8      time_offset
  REAL(r_single), allocatable ::   lat(:,:)
  REAL(r_single), allocatable ::   lon(:,:)
  REAL(r_single), allocatable ::   lwp(:,:)
  REAL(r_single), allocatable ::   teff(:,:)
  REAL(r_single), allocatable ::   ptop(:,:)
  REAL(r_single), allocatable ::   htop(:,:)
  integer(i_kind), allocatable ::   phase(:,:)


! Working
  integer,allocatable  ::  index(:,:)
  integer  jj

!  ** misc
  integer i,j
  Integer nf_status,nf_fid,nf_vid

  integer :: NCID
  integer :: isat
  LOGICAL :: first
  LOGICAL :: file_exists

  integer :: status,mype

!**********************************************************************
!
!            END OF DECLARATIONS....start of program


  ! read in the NASA LaRC cloud data
  ! there are 8 gloabl files:
  ! northern hemisphere and southern hemisphere for 
  ! goes-east, goes-west, mt10 and mt2
  first = .true.
  DO isat=1,8
     if(isat==1) then
       satfile='nasaLaRC_gesoWestNH.nc'
     elseif(isat==2) then
       satfile='nasaLaRC_gesoWestSH.nc'
     elseif(isat==3) then
       satfile='nasaLaRC_gesoEastNH.nc'
     elseif(isat==4) then
       satfile='nasaLaRC_gesoEastSH.nc'
     elseif(isat==7) then
       satfile='nasaLaRC_mt10NH.nc'
     elseif(isat==8) then
       satfile='nasaLaRC_mt10SH.nc'
     elseif(isat==5) then
       satfile='nasaLaRC_mt2NH.nc'
     elseif(isat==6) then
       satfile='nasaLaRC_mt2SH.nc'
     else
       write(6,*) 'No satellite information '
       stop 123
     endif

     ! if a file is missing skip it
     INQUIRE(FILE=satfile,EXIST=file_exists)
     if (file_exists == .false.) CYCLE

     ! get the dimensions for this file
     call read_NASALaRC_global_cloud_dim(satfile,nxp,nyp)
     write(6,*)'LaRC isat/nxl/nyl =',isat,nxp, nyp
     allocate(lat(nxp,nyp))
     allocate(lon(nxp,nyp))
     allocate(ptop(nxp,nyp))
     allocate(htop(nxp,nyp))
     allocate(teff(nxp,nyp))
     allocate(phase(nxp,nyp))
     allocate(lwp(nxp,nyp))
     ptop=-9.
     htop=-9.
     teff=-9.
     lat =-9.
     lon =-9.
     lwp =-9.
     phase=-9

     ! read in data
     call read_NASALaRC_global_cloud(satfile,nxp,nyp,ptop,htop,teff,phase,lwp,lat,lon,cbase_time,time_offset)

     ! write out results
     call write_NASALaRC_global_cloud(nxp,nyp,lat,lon,ptop,htop,teff,phase,lwp,cbase_time,time_offset,first)
     ! after one file is written, append data
     first = .false.

     deallocate(lat,lon,ptop,htop,teff,phase,lwp)

  ENDDO  ! isat 

  write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="
!
end program process_NASALaRC_global_cloud

