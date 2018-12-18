program calculate_spread
!
!  read in regional ensemble perturbations from 
!  HRRRE and calculate ensemble spread
!
   use mpi
   use module_cal_spread , only : cal_spread 

   implicit none
   type(cal_spread) :: calspd
   
   integer :: nfile
   character(len=180),allocatable :: filelist(:)
   character(len=180) :: filename
   character(len=180) :: spreadfile
   character(len=10),allocatable :: varnamelist(:)
   integer :: numvar
   character(len=10) :: varname

   character(len=180) :: filelistname
   character(len=180) :: resultspath
   namelist/setup/ filelistname,resultspath

   integer :: i,iv, id, worldid,worldsize
   integer :: ierr
!
   call MPI_Init (ierr)

   call MPI_Comm_size (MPI_COMM_WORLD, worldsize, ierr)
   call MPI_Comm_rank (MPI_COMM_WORLD, worldid, ierr)    
   id=worldid+1
   if(id==1) write(*,*) 'id=',worldid, ' msize=',worldsize
   open(12,file='namelist')
      read(12,setup)
   close(12)
   if(id==1) write(*,setup)

   numvar=5
   allocate(varnamelist(numvar))
   varnamelist(1)='ps'
   varnamelist(2)='tv'
   varnamelist(3)='u'
   varnamelist(4)='v'
   varnamelist(5)='rh'
!
   if( worldsize < numvar) then
      if(id==1) then
         write(*,*) 'MPI ERROR: no enough core for variables'
      endif
      call MPI_Finalize (ierr)
      stop
   endif
!
   nfile=0
   open(12,file=trim(filelistname))
      do
        read(12,*,end=100)
        nfile=nfile+1
        if(nfile > 1000) exit
      enddo
100 continue
     if(nfile > 0) then
        allocate(filelist(nfile))
        rewind(12)
        do i=1,nfile
           read(12,'(a)') filelist(i)
        enddo
     else
        write(*,*) 'no file in filelist'
        call MPI_Finalize (ierr)
        stop 123
     endif
   close(12)
   if(id==1) write(*,*) 'total file will Process  =',nfile

!   do iv=1,numvar
   if(id <= numvar) then
!      varname=trim(varnamelist(iv)) 
      varname=trim(varnamelist(id)) 
      write(spreadfile,'(a,a,a,a)') trim(resultspath),'/spread_',trim(varname),'.bin'
      filename=trim(filelist(1))
      call calspd%initial(trim(filename),trim(varname))
      do i=1,nfile
         filename=trim(filelist(i))
         write(*,'(I3,a,I3,a)') id,":processing ",i,trim(filename)
         call calspd%readadd(trim(filename))
      enddo
      call calspd%calsprd()
      call calspd%wrtprd(trim(spreadfile))
      call calspd%destroy()
    endif
!   enddo ! iv

   deallocate(filelist)
   deallocate(varnamelist)

   call MPI_Finalize (ierr)

end program calculate_spread
