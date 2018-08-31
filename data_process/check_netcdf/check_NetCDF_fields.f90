program check_netcdf_field
!$$$  documentation block
!                .      .    .                                       .
!   full_cycle_surface_netcdf_mass: read surface variables from latest wrf mass
!            netcdf 0 h forecast 
!           and update them in wrf mass background file
!  List of variables updated:
!	MF_VX_INV
!
!   prgmmr: Ming Hu                 date: 2008-11-21
!
! program history log:
!
!    2009-11-09 Ming Hu : add snowc
!
!   input argument list:
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind
  implicit none

  INCLUDE 'netcdf.inc'

! Declare local parameters

  character(len=120) :: flnm1
  character(len=19)  :: DateStr1
  integer(i_kind)    :: dh1

  integer(i_kind) :: Status, Status_next_time
  integer(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff

  character (len=80) :: SysDepInfo
  character (len=31) :: rmse_var

  logical :: if_integer
  logical :: if_lake_physics
  integer :: NCID,in_SF_LAKE_PHYSICS,out_SF_LAKE_PHYSICS

  if_lake_physics=.true.
  in_SF_LAKE_PHYSICS=0
  out_SF_LAKE_PHYSICS=0
!
  flnm1='wrfout_d01'
!
  STATUS=NF_OPEN(trim(flnm1),0,NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS = NF_GET_ATT_INT (NCID, NF_GLOBAL, 'SF_LAKE_PHYSICS',in_SF_LAKE_PHYSICS)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS=NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  write(*,*) 'in_SF_LAKE_PHYSICS=',in_SF_LAKE_PHYSICS
!
  call ext_ncd_ioinit(sysdepinfo,status)
!
!           open netcdf file to read
!
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74
  endif
!-------------  get date info  from file read in

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                        ,iyear,imonth,iday,ihour,iminute,isecond
  JDATE=0
  JDATE(1)=iyear
  JDATE(2)=imonth
  JDATE(3)=iday
  JDATE(5)=ihour
  JDATE(6)=iminute
!
!
!  ------ read in the field
  if_integer=.false.
  rmse_var='MF_VX_INV'
  call read_netcdf_mass(dh1,DateStr1,rmse_var,if_integer)

!-------------  close files ----------------
  call ext_ncd_ioclose(dh1, Status)

end program check_netcdf_field


!
SUBROUTINE HANDLE_ERR_M(STATUS)
     INCLUDE 'netcdf.inc'
     INTEGER STATUS
     IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
     ENDIF
END SUBROUTINE HANDLE_ERR_M

