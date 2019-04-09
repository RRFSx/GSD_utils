Program EncodeRW_DPQC
!
! read in radial wind from DPQC netcdf file and
!     write out as BUFR
!
  use mpi
  use module_RW_DPQC, only : rw_dpqc
  use module_RW_dealiasing, only : rw_dealiasing
  use module_background, only : background
  use module_radar_station_config, only : rdrsta_config
  use module_map_utils, only : map_util

  implicit none

!  MPI variables
  integer :: npe, mype, mypeLocal,ierror

  type(rw_dpqc)       :: rwdpqc
  type(rw_dealiasing) :: rwdealis
  type(background)    :: bkgd
  type(map_util)      :: map
  type(rdrsta_config) :: rscf

  integer :: nfile
  character(len=180),allocatable :: filelist(:)
  character(len=180) :: crwfile
  character(len=180) :: wrtfile
  character(len=180) :: logfile
  character(len=180) :: bkfile

  integer, allocatable :: filelistbin(:)
  integer :: i,n, fileliststart,logout
!
  logical :: if_dealiasing
!

!  MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  if_dealiasing=.false.
!
!  prepare background
  bkfile='wrfinput_d01'

  if(if_dealiasing) then
     call bkgd%initialmap(trim(bkfile),map)
     call bkgd%initial(trim(bkfile))
  endif
!
!
  call rscf%readcf("radar_station_list.txt")
!
  write(wrtfile,'(a,I3.3)') 'sub_l2rwbufr_nssl_',mype+1
  write(logfile,'(a,I3.3,a)') 'sub_l2rwbufr_nssl_',mype+1,'.log'
  logout=13

  open(logout,file=trim(logfile))
!
  nfile=0 
  open(12,file='rwfilelist')
     do
        read(12,*,end=100)
        nfile=nfile+1
        if(nfile > 1000000) then
          write(*,*) 'Error, too many files, double check!!!'
          stop 345
        endif
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
  if(mype==0) then
     write(*,*) 'total file will Process  =',nfile
  endif
!
!  figure out file list processed by each core
  allocate(filelistbin(npe))
  filelistbin=0
  if(nfile < npe) then
     filelistbin(1)=nfile
  else
     n=max(1,nfile/npe)
     filelistbin(1)=n
     do i=1,npe-1
        filelistbin(i+1)=filelistbin(i)+n
     enddo
     filelistbin(npe)=nfile
  endif
  if(mype==0) then
     write(*,*) "file list bin=",filelistbin
  endif
  if(mype==0) then
     fileliststart=1
  else
     fileliststart=filelistbin(mype)+1
  endif
!
!
  if(filelistbin(mype+1) > 0) then
     write(logout,*) 'This core ',mype,' process file from',fileliststart,' to ',&
                filelistbin(mype+1)
     do i=fileliststart,filelistbin(mype+1)

        crwfile=trim(filelist(i))
        write(logout,'(a,I5,2x,a)') "processing ",i,trim(crwfile)

        call rwdpqc%readnc(trim(crwfile))
        if(if_dealiasing) then
           call rwdealis%initial(rwdpqc)
           call rwdealis%cal_bkrw(rwdpqc,bkgd,map)
           call rwdealis%destroy()
        endif
        call rwdpqc%wrtbufr(trim(wrtfile),rscf)
        call rwdpqc%destroy()

     enddo
     write(logout,*) 'processing finished successfully:',mype
  endif
  close(logout)

  deallocate(filelist)
  deallocate(filelistbin)

  call rscf%destroy()
  if(if_dealiasing) then
     call bkgd%destroy()
     call map%destory_general_transform()
  endif

  call MPI_FINALIZE(ierror)

End program EncodeRW_DPQC
