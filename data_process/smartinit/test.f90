program test
  use kinds, only: r_single,i_kind
  character (len=80) :: SysDepInfo,flnm1, VarName
  character(len=19)  :: DateStr1
  integer(i_kind) :: ierr,ierr2, dhandle,ndim1,WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering

  logical :: print_verbose=.true.

  flnm1="wrf_inout"
  call ext_ncd_ioinit(sysdepinfo,ierr)
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dhandle, ierr)

  call ext_ncd_get_next_time(dhandle, DateStr1, ierr) !ierr=0 indicates good
  !read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  !write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

  do while (ierr==0)
     call ext_ncd_get_next_var(dhandle, VarName, ierr)
 !!!call ext_ncd_get_var_info(DataHandle,Name,NDim,MemoryOrder,Stagger,DomainStart,DomainEnd,WrfType,ierr)
     call ext_ncd_get_var_info (dhandle,VarName,ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr2)
     if(print_verbose)then
       write(6,'(a30,I3,2a4,I5,8I5)') trim(VarName),ndim1, &
         trim(ordering),trim(staggering), WrfType, start_index, end_index
     end if
  enddo


  call ext_ncd_ioclose(dhandle, ierr)

end program test

SUBROUTINE wrf_debug( level , str )
  !a dummy subroutine to skip compiling error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  RETURN
END SUBROUTINE wrf_debug
