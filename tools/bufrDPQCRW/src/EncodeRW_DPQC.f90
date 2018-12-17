Program EncodeRW_DPQC
!
! read in radial wind from DPQC netcdf file and
!     write out as BUFR
!
  use module_RW_DPQC, only : rw_dpqc

  implicit none

  type(rw_dpqc) :: rwdpqc

  integer :: nfile
  character(len=180),allocatable :: filelist(:)
  character(len=180) :: crwfile
  character(len=180) :: wrtfile

  integer :: i
!
!
  wrtfile='l2rwbufr_nssl'
!
  nfile=0 
  open(12,file='rwfilelist')
     do
        read(12,*,end=100)
        nfile=nfile+1
        if(nfile > 1000) exit
     enddo
100  continue
     if(nfile > 0) then
        allocate(filelist(nfile))
        rewind(12)
        do i=1,nfile
           read(12,'(a)') filelist(i)
        enddo
     endif
  close(12)
  write(*,*) 'total file will Process  =',nfile

  do i=1,nfile

     crwfile=trim(filelist(i))
     write(*,'(a,I3,a)') "processing ",i,trim(crwfile)

     call rwdpqc%readnc(trim(crwfile))
     call rwdpqc%wrtbufr(trim(wrtfile))
     call rwdpqc%destroy()

   enddo

   deallocate(filelist)

End program EncodeRW_DPQC
