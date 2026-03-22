program test
  use kinds, only: r_single,i_kind
  use netcdf

  implicit none
  !include 'netcdf.inc'
  character (len=80) :: SysDepInfo,flnm1, VarName, flnm2
  character(len=19)  :: DateStr1, myname_='test'
  integer(i_kind) :: ierr,ierr2, ncid,ndim1,WrfType,varid,dimid,get_dim_value
  integer(i_kind) :: west_east_stag,south_north_stag,ilen
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering
  logical :: print_verbose=.true.

  flnm1="wrf_src"
  call gcheck(nf90_open(trim(adjustl(flnm1)),nf90_write,ncid))
  west_east_stag=get_dim_value("west_east_stag",ncid)
  south_north_stag=get_dim_value("south_north_stag",ncid)
  call gcheck(nf90_close(ncid))
  write(6,'(a,2i6)') 'west_east_stag,south_north_stag=',west_east_stag,south_north_stag

  call ext_ncd_ioinit(sysdepinfo,ierr)
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", ncid, ierr)

  call ext_ncd_get_next_time(ncid, DateStr1, ierr) !ierr=0 indicates good
  !read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  !write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

  do while (ierr==0)
     call ext_ncd_get_next_var(ncid, VarName, ierr)
     call ext_ncd_get_var_info (ncid,VarName,ndim1,ordering,staggering,start_index,end_index, WrfType, ierr2)
     if(print_verbose)then
       write(6,'(a25,I3,2a4,I5,4(i2,a1,i4))') trim(VarName),ndim1, &
         trim(ordering),trim(staggering), WrfType,  &
         start_index(1),',', end_index(1),start_index(2),',', end_index(2), &
         start_index(3),',', end_index(3),start_index(4),',', end_index(4)
     end if
  enddo

  call ext_ncd_ioclose(ncid, ierr)


! call nc_check( nf90_put_att(dh1,nf90_global,'START_DATE',trim(DateStr1)),!myname_,'put_att:  START_DATE '//trim(adjustl(flnm1)) )
! call nc_check( nf90_put_att(dh1,nf90_global,'SIMULATION_START_DATE',trim(DateStr1)),&
!     myname_,'put_att:  SIMULATION_START_DATE '//trim(adjustl(flnm1)) )
! call nc_check( nf90_put_att(dh1,nf90_global,'GMT',float(ihour)),&
!     myname_,'put_att: GMT '//trim(adjustl(flnm1)) )
! call nc_check( nf90_put_att(dh1,nf90_global,'JULYR',iyear),&
!     myname_,'put_att: JULYR'//trim(adjustl(flnm1)) )
! call nc_check( nf90_put_att(dh1,nf90_global,'JULDAY',iw3jdn(iyear,imonth,iday)-iw3jdn(iyear,1,1)+1),&
!     myname_,'put_att: JULDAY'//trim(adjustl(flnm1)) )

end program test



SUBROUTINE wrf_debug( level , str )
  !a dummy subroutine to skip compiling error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  RETURN
END SUBROUTINE wrf_debug

subroutine gcheck(ierr)
  use kinds, only: i_kind
  use netcdf, only: nf90_noerr
  implicit none
  integer(i_kind), intent (in) :: ierr
  if (ierr/=nf90_noerr) then
    print *, "Stopped! NetCDF I/O error #: ",ierr
    stop
  endif
end subroutine gcheck

function get_dim_value(varname,ncid)
  use netcdf, only: nf90_inq_dimid, nf90_inquire_dimension
  IMPLICIT NONE
  CHARACTER*(*) varname
  integer::ncid,dimid,get_dim_value, varvalue
  call gcheck(nf90_inq_dimid(ncid, trim(varname), dimid))
  call gcheck(nf90_inquire_dimension(ncid, dimid, len=get_dim_value))
!!!extra notes
  !!!call gcheck(nf90_get_att(ncid, NF90_GLOBAL,"west_east_stag", west_east_stag))
end function get_dim_value
