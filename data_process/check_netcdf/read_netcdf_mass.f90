
subroutine read_netcdf_mass(dh1,DateStr1,rmse_var,if_integer)
!$$$  documentation block
!                .      .    .                                       .
!   read_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file
!
!   prgmmr: Ming Hu                 date: 2009-01-16
!
! program history log:
!
!   input argument list:
!      dh1 :    handle of file read in
!      DateStr1 : time string of file read in 
!      rmse_var :  variable updated
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind, r_kind
  implicit none

!
  integer(i_kind), intent(in)      :: dh1
  character (len=31), intent(in)   :: rmse_var
  character(len=19),intent(in)  :: DateStr1
  logical,intent(in)  :: if_integer
  
! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering
  
  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
  
! Declare local parameters
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field3(:,:,:)
  integer,allocatable::ifield3(:,:,:)
  
  integer(i_kind) :: k
  integer(i_kind) :: ierr
!
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(rmse_var)
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!-------------  get grid info
  
  end_index=0
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1       
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE
  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  if(ndim1 == 2) nsig_regional=1
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  if( ndim1 == 2 .or. ndim1 == 3) then
      if(if_integer) then
         allocate(ifield3(nlon_regional,nlat_regional,nsig_regional))
      else
         allocate(field3(nlon_regional,nlat_regional,nsig_regional))
      endif
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  if(if_integer) then
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield3,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  else
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  endif

  DO k=1,nsig_regional
    if(if_integer) then
      write(6,*)' max,min =',maxval(ifield3(:,:,k)),minval(ifield3(:,:,k))
    else
      write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
    endif
  enddo

  DO k=1,nsig_regional
    if(if_integer) then
      write(6,*)' max,min =',maxval(ifield3(:,:,k)),minval(ifield3(:,:,k))
    else
      if(maxval(field3(:,:,k)) > 1.0_r_kind) write(6,*)' Error max =',maxval(field3(:,:,k))
    endif
  enddo

  if(if_integer) then
     deallocate(ifield3)
  else
     deallocate(field3)
  endif
  
end subroutine read_netcdf_mass

SUBROUTINE wrf_debug( level , str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
!  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    ! old behavior
!      CALL wrf_message( str )
  ENDIF
  write(*,*) 'wrf_debug called !'
  RETURN
END SUBROUTINE wrf_debug

