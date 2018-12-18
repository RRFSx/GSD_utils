program calculate_spread
!
!  read in regional ensemble perturbations from 
!  HRRRE and calculate ensemble spread
!
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

   integer :: i,iv
!
   numvar=5
   allocate(varnamelist(numvar))
   varnamelist(1)='ps'
   varnamelist(2)='tv'
   varnamelist(3)='u'
   varnamelist(4)='v'
   varnamelist(5)='rh'
!
   nfile=0
   open(12,file='filelist')
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
        stop 123
     endif
   close(12)
   write(*,*) 'total file will Process  =',nfile

   do iv=1,numvar
      varname=trim(varnamelist(iv)) 
      write(spreadfile,'(a,a,a)') './results/spread_',trim(varname),'.bin'
      filename=trim(filelist(1))
      call calspd%initial(trim(filename),trim(varname))
      do i=1,nfile
         filename=trim(filelist(i))
         write(*,'(a,I3,a)') "processing ",i,trim(filename)
         call calspd%readadd(trim(filename))
      enddo
      call calspd%calsprd()
      call calspd%wrtprd(trim(spreadfile))
      call calspd%destroy()
   enddo ! iv

   deallocate(filelist)
   deallocate(varnamelist)

end program calculate_spread
