module module_statistic

   use kinds, only: r_kind,r_single,rmissing
   use module_pairs,        only : fcstobspairs
   use module_hashsearch,   only : hashtable
   use module_stationarray, only : stationarray

   implicit none

   public :: statisticbase
   private
   type :: statisticbase
      integer            ::  nlevel     ! number of levels
      character          ::  cleveltype ! ilevel type (P,H,...)
      real,allocatable   ::  rlevel(:)  ! level
      integer            ::  nfcstmm    ! number of forecast time in minutes
      integer,allocatable::  ifcstmm(:) ! forecast time in minutes
      integer            ::  numvar     ! number of variables 
      character(len=20),allocatable :: varnames(:)
      integer,allocatable::  indexfcstvar(:)   ! index of variable in fcst observation 
      integer            ::  numstation     ! number of obs stations
      integer            ::  validdate,validmm
      logical            ::  ifsavefcst! save forecast value in rmse
      logical            ::  if_save_4sfcpoints ! save surrounding sfc points

      real(r_kind), allocatable :: rmse(:,:,:,:)   !  numvar,nlevel,nfcstmm,numstation  
      real(r_kind), allocatable :: bias(:,:,:,:)
      integer ,     allocatable :: numb(:,:,:,:)
      real(r_single),allocatable :: value4(:,:,:,:)! numvar,4,nfcstmm,numstation 
      contains
         procedure :: initial => initial_statisticbase
         procedure :: find_varid => find_statisticbase_varid
         procedure :: destroy    => destroy_statisticbase
         procedure :: reset      => reset_statisticbase
         procedure :: list       => list_statisticbase
         procedure :: writes     => write_statisticbase
         procedure :: writes_sta => write_statisticbase_station
         procedure :: reads      => read_statisticbase
         procedure :: accumulate => accumulate_statisticbase
         procedure :: calculate  => calculate_statisticbase
         procedure :: save_sta   => save_stations_statisticbase
   end type

   contains
      
      subroutine initial_statisticbase(this,datatype,filename,numstation)
! initial the space
         implicit none
         class(statisticbase) :: this
         integer, intent(in) :: datatype
         integer, intent(in),optional :: numstation
         character(len=*),intent(in) :: filename

         integer :: nlevel,nfcstmm,numvar
         integer :: unitin
         logical :: ifexist
         character(len=50) :: cline
         integer :: itype,i
         character(len=10) ctemp

         unitin=12
!         filename='statisticset.txt'

         this%ifsavefcst=.false.
         this%if_save_4sfcpoints=.false.
         this%nlevel=0
         this%nfcstmm=0
         this%numvar=0
         this%numstation=0
         inquire( file=trim(filename), EXIST=ifexist )
         if(ifexist) then
            open(unitin,file=trim(filename),status='old')
               do while(1==1)
                  read(unitin,'(a)',end=100) cline
                  if(cline(1:5)=='TYPE:') then
                     read(cline,'(5x,I10)') itype
                     if(itype==datatype) then
! read in level
                       read(unitin,*) ctemp,this%nlevel,this%cleveltype
                       if(allocated(this%rlevel)) deallocate(this%rlevel)
                       if(this%cleveltype=="A") then
                          allocate(this%rlevel(this%nlevel))
                          read(unitin,*) this%rlevel(:)
                       else
                          allocate(this%rlevel(this%nlevel+1))
                          read(unitin,*) this%rlevel(:)
                       endif
! read in forecast  time
                       read(unitin,*) ctemp,this%nfcstmm
                       if(allocated(this%ifcstmm)) deallocate(this%ifcstmm)
                       allocate(this%ifcstmm(this%nfcstmm))
                       read(unitin,*) this%ifcstmm(:)
! convert from hhmm to mm
                       do i=1,this%nfcstmm
                           this%ifcstmm(i)=(this%ifcstmm(i)/100)*60+mod(this%ifcstmm(i),100)
                       enddo
! read in verify variables
                       read(unitin,*) ctemp,this%numvar
                       if(allocated(this%varnames)) deallocate(this%varnames)
                       allocate(this%varnames(this%numvar))
                       read(unitin,*) this%varnames(:)
! set obs numb as 1
                       this%numstation=1
                       if(present(numstation)) this%numstation=numstation
                     endif
                  endif
               enddo
100         continue
            close(unitin)
         else
            write(*,*) ' ERRROR, statistic set file does not exist!'
            stop 123
         endif

         if(this%nlevel > 0 .and. this%nfcstmm > 0 .and. &
            this%numvar > 0 .and. this%numstation>0) then

            write(*,*) 'initial statistic type success',datatype
            write(*,*) 'level=',this%nlevel,this%cleveltype
            write(*,*) this%rlevel(:)
            write(*,*) 'forecast time=',this%nfcstmm
            write(*,*) this%ifcstmm
            write(*,*) 'variable=',this%numvar
            write(*,*) this%varnames
            write(*,*) 'number of obs station=',this%numstation

            if(allocated(this%indexfcstvar)) deallocate(this%indexfcstvar)
            allocate(this%indexfcstvar(this%numvar))
            this%indexfcstvar=0

            if(allocated(this%rmse)) deallocate(this%rmse)
            allocate(this%rmse(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
            this%rmse=0.0_r_kind
            if(allocated(this%bias)) deallocate(this%bias)
            allocate(this%bias(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
            this%bias=0.0_r_kind
            if(allocated(this%numb)) deallocate(this%numb)
            allocate(this%numb(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
            this%numb=0
            if(allocated(this%value4)) deallocate(this%value4)
            allocate(this%value4(this%numvar,4,this%nfcstmm,this%numstation))
            this%value4=0.0_r_single
         else
            write(*,*) 'initial statistic type failed=',datatype,&
            this%nlevel, this%nfcstmm,this%numvar,this%numstation
            stop 123
         endif

      end subroutine initial_statisticbase

      subroutine find_statisticbase_varid(this,pairs)
! find index of the varaible used for verification in fcst obs list
         implicit none
         class(statisticbase) :: this
         type(fcstobspairs), intent(in) :: pairs

         integer :: i,j

         if(this%numvar > 0 )then
            this%indexfcstvar=0
            do i=1,this%numvar
               if(trim(this%varnames(i))=="U") this%indexfcstvar(i)=pairs%iu
               if(trim(this%varnames(i))=="V") this%indexfcstvar(i)=pairs%iv
               if(trim(this%varnames(i))=="T") this%indexfcstvar(i)=pairs%it
               if(trim(this%varnames(i))=="Q") this%indexfcstvar(i)=pairs%iq
               if(trim(this%varnames(i))=="H") this%indexfcstvar(i)=pairs%ih
               if(trim(this%varnames(i))=="P") this%indexfcstvar(i)=pairs%ip
            enddo
!            write(*,*) 'forecast variable index='
!            write(*,*) 'h,p,t,q,u,v=',pairs%ih,pairs%ip,pairs%it,pairs%iq,pairs%iu,pairs%iv
!            write(*,*) 'indexfcstvar=',this%indexfcstvar
         else
            write(*,*) 'No varaible name in statistic initialization!'
            write(*,*) 'statistic variable:',this%varnames(:)
            write(*,*) 'fcstobs variable:',pairs%varname(:)
            stop 123
         endif
      end subroutine find_statisticbase_varid

      subroutine destroy_statisticbase(this)
! release the space
         implicit none
         class(statisticbase) :: this

         this%nlevel=0
         this%nfcstmm=0
         this%numvar=0
         this%numstation=0

         if(allocated(this%rlevel)) deallocate(this%rlevel)
         if(allocated(this%ifcstmm)) deallocate(this%ifcstmm)
         if(allocated(this%varnames)) deallocate(this%varnames)
         if(allocated(this%indexfcstvar)) deallocate(this%indexfcstvar)

         if(allocated(this%rmse)) deallocate(this%rmse)
         if(allocated(this%bias)) deallocate(this%bias)
         if(allocated(this%numb)) deallocate(this%numb)
         if(allocated(this%value4)) deallocate(this%value4)

      end subroutine destroy_statisticbase

      subroutine reset_statisticbase(this)
! reset arrary to 0
         implicit none
         class(statisticbase) :: this

         if(allocated(this%rmse)) this%rmse=0.0_r_kind 
         if(allocated(this%bias)) this%bias=0.0_r_kind
         if(allocated(this%numb)) this%numb=0
         if(allocated(this%value4)) this%value4=0.0_r_single

      end subroutine reset_statisticbase

      subroutine accumulate_statisticbase(this,pairs,fcsttime,hashobs)
! calculate statistics
         implicit none
         class(statisticbase) :: this
         type(fcstobspairs), intent(in) :: pairs
         integer, intent(in) :: fcsttime
         type(hashtable),intent(in),optional  :: hashobs

         integer :: i,j,k
         real :: rlvl
         integer :: itime,ilvl,idx,iobs
         real :: bminuso,fcstobs
         real :: xc,yc
!
         if(pairs%num_obs > 0) then

            this%validdate=pairs%inidate
            this%validmm=pairs%inimm
            this%if_save_4sfcpoints=pairs%if_save_4sfcpoints
            iobs=1
! find forecast time
            itime=0
            do i=1,this%nfcstmm
               if(this%ifcstmm(i)==fcsttime) itime=i
            enddo
            if(itime==0) then
               return
            endif
            if(this%numvar>0 .and. pairs%num_var>0) then

               iobsloop:do i=1,pairs%num_obs
!
! find the statistic level where forecast obs is
                  klevel:do k=1,pairs%fcstobs(i)%numlvl
                     idx=(k-1)*pairs%num_var
                     ilvl=0
                     if(this%cleveltype =='A') then
                        ilvl=1
                        rlvl=pairs%fcstobs(i)%obs(idx+pairs%ih)
                     endif
                     if(this%cleveltype =='H') then
                        rlvl=pairs%fcstobs(i)%obs(idx+pairs%ih)
                        if(rlvl < this%rlevel(1) )then
                           ilvl=-1
                        elseif(rlvl >= this%rlevel(this%nlevel+1)) then
                           ilvl=-2
                        else
                           findlvlh:do j=1,this%nlevel
                              if( rlvl >= this%rlevel(j) .and. &
                                  rlvl < this%rlevel(j+1) ) then
                                 ilvl=j
                                 exit findlvlh
                              endif
                           enddo findlvlh
                        endif
                     endif
                     if(this%cleveltype =='P') then
                        rlvl=pairs%fcstobs(i)%obs(idx+pairs%ip)
                        if(rlvl > this%rlevel(1) )then
                           ilvl=-1
                        elseif(rlvl <= this%rlevel(this%nlevel+1)) then
                           ilvl=-2
                        else
                           findlvlp: do j=1,this%nlevel
                              if( rlvl <= this%rlevel(j) .and. &
                                  rlvl > this%rlevel(j+1) ) then
                                 ilvl=j
                                 exit findlvlp
                              endif
                           enddo findlvlp
                        endif
                    endif
                    if(ilvl<=0) then
                       cycle
                    endif
!
!  find station index
                    if(this%numstation>1)  then
                       if(present(hashobs)) then
                          xc=pairs%fcstobs(i)%xc
                          yc=pairs%fcstobs(i)%yc
                          call hashobs%search(pairs%fcstobs(i)%name,int(xc),int(yc),iobs)
                          if(iobs > this%numstation.or. iobs < 1) then
                             write(*,*) 'obs station index is out of range', &
                                     iobs,this%numstation
                             stop 345
                          endif
                       else
                          write(*,*) 'ERROR: mush has hash table when numstation>1',this%numstation              
                       endif
                    endif
!
! this%numvar,this%nlevel,this%nfcstmm,this%numstation
                    do j=1,this%numvar
                       bminuso=pairs%fcstminusobs(i)%obs(idx+this%indexfcstvar(j))
                       fcstobs=pairs%fcstobs(i)%obs(idx+this%indexfcstvar(j))
                       if(abs(bminuso)<99998.0) then
                          if(this%ifsavefcst) then
                             this%rmse(j,ilvl,itime,iobs)=this%rmse(j,ilvl,itime,iobs)+fcstobs
                          else
                             this%rmse(j,ilvl,itime,iobs)=this%rmse(j,ilvl,itime,iobs)+bminuso*bminuso
                          endif
                          this%bias(j,ilvl,itime,iobs)=this%bias(j,ilvl,itime,iobs)+bminuso
                          this%numb(j,ilvl,itime,iobs)=this%numb(j,ilvl,itime,iobs)+1
                       endif
                       if(this%numstation>1 .and. pairs%if_save_4sfcpoints) then
                          this%value4(j,:,itime,iobs)=pairs%value4(i,this%indexfcstvar(j),:)
                       endif
                    enddo
                  enddo  klevel
               enddo iobsloop
            else
               write(*,*) 'Warning: number of fcst obs or statistic variables is 0!'
            endif
         else
            write(*,*) 'No observations available'
         endif
      end subroutine  accumulate_statisticbase

      subroutine save_stations_statisticbase(this)
! calculate the statistic results
         implicit none
         class(statisticbase) :: this
        
         integer :: i,j,k,n
         integer :: listnum

         do j=1,this%numstation
            do i=1,this%nfcstmm
               do k=1,this%nlevel
                  do n=1,this%numvar
                     if(this%numb(n,k,i,j) >0) then
                        this%rmse(n,k,i,j)=this%rmse(n,k,i,j)/float(this%numb(n,k,i,j))
                        this%bias(n,k,i,j)=this%bias(n,k,i,j)/float(this%numb(n,k,i,j))
                     else
                        this%rmse(n,k,i,j)=rmissing
                        this%bias(n,k,i,j)=rmissing
                     endif
                  enddo
               enddo
            enddo
         enddo

      end subroutine save_stations_statisticbase 

      subroutine calculate_statisticbase(this)
! calculate the statistic results
         implicit none
         class(statisticbase) :: this
        
         integer :: i,j,k,n
         integer :: listnum

         do j=1,this%numstation
            do i=1,this%nfcstmm
               do k=1,this%nlevel
                  do n=1,this%numvar
                     if(this%numb(n,k,i,j) >0) then
                        this%rmse(n,k,i,j)=this%rmse(n,k,i,j)/float(this%numb(n,k,i,j))
                        if(this%rmse(n,k,i,j)>0) then
                           this%rmse(n,k,i,j)=sqrt(this%rmse(n,k,i,j))
                        else
                           this%rmse(n,k,i,j)=0.0_r_kind
                        endif
                        this%bias(n,k,i,j)=this%bias(n,k,i,j)/float(this%numb(n,k,i,j))
                     else
                        this%rmse(n,k,i,j)=rmissing
                        this%bias(n,k,i,j)=rmissing
                     endif
                  enddo
               enddo
            enddo
         enddo

      end subroutine calculate_statisticbase 

      subroutine list_statisticbase(this)
! list the statistic results
         implicit none
         class(statisticbase) :: this
        
         integer :: i,j,k,n
         integer :: listnum

         write(*,*) 'level for statistic=',this%nlevel,this%cleveltype
!         write(*,'(100f10.2)') (this%rlevel(i),i=1,this%nlevel+1)
         write(*,*) 'forecast time for statistic=',this%nfcstmm
         write(*,*) 'forecast variable for statistic=',this%numvar
         write(*,*) 'stations for statistic=',this%numstation
         do j=1,min(10,this%numstation)
            do n=1,this%numvar
               write(*,*) 'statistic for ',this%varnames(n)
               write(*,'(a15,100I10)') 'fcst time',(this%ifcstmm(i),i=1,this%nfcstmm)
               do k=1,this%nlevel
                  write(*,'(A5,f10.1,100f10.2)') 'rmse=',this%rlevel(k),(this%rmse(n,k,i,j),i=1,this%nfcstmm)
                  write(*,'(A5,f10.1,100f10.2)') 'bias=',this%rlevel(k),(this%bias(n,k,i,j),i=1,this%nfcstmm)
                  write(*,'(A5,f10.1,100I10)') 'numb=',this%rlevel(k),(this%numb(n,k,i,j),i=1,this%nfcstmm)
               enddo
            enddo

            if(this%numstation >1 .and. this%if_save_4sfcpoints) then
               do n=1,this%numvar
                  write(*,*) 'list suround points for ',this%varnames(n)
                  do k=1,this%nlevel
                     write(*,'(I10,100f10.2)') k,(this%value4(n,k,i,j),i=1,this%nfcstmm)
                  enddo
               enddo
            endif
         enddo

      end subroutine list_statisticbase 

      subroutine write_statisticbase(this,action,filename,timetag)
! write the statistic results
         implicit none
         class(statisticbase) :: this
         character(len=3),intent(in) :: action
         character(len=*), intent(in) :: filename
         character(len=12),intent(in),optional :: timetag
        
         integer :: iunit
         integer :: i,j,k,n
         integer :: listnum
         logical :: exist

         if(present(timetag)) read(timetag,'(I10,I2)') this%validdate, this%validmm
         iunit=13
         if(action=='new') then
            open(iunit,file=trim(filename),form='unformatted')
               write(iunit) this%nlevel,this%nfcstmm,this%numvar,this%numstation,this%cleveltype,&
                            .false.
               write(iunit) this%rlevel
               write(iunit) this%ifcstmm
               write(iunit) this%varnames
            
               write(iunit) this%validdate, this%validmm
               write(iunit) real(this%rmse)
               write(iunit) real(this%bias)
               write(iunit) this%numb
            close(iunit)
         elseif(action=='app') then
            inquire(file=trim(filename), exist=exist)
            if(exist) then
               open(iunit,file=trim(filename),form='unformatted',position='append')
                 write(iunit) this%validdate, this%validmm
                 write(iunit) real(this%rmse)
                 write(iunit) real(this%bias)
                 write(iunit) this%numb
               close(iunit)
            else
               write(*,*) 'Error, this file has to exist=',trim(filename)
               stop 345
            endif
         else
            write(*,*) 'unknow action for writing, has to one of new, app'
            stop 234
         endif

      end subroutine write_statisticbase

      subroutine read_statisticbase(this,filename)
! write the statistic results
         implicit none
         class(statisticbase) :: this
         character(len=*), intent(in) :: filename
         type(stationarray) :: statarr
        
         integer :: iunit
         integer :: i,j,k,n
         logical :: exist
         real,allocatable :: rmse(:,:,:,:),bias(:,:,:,:)

         iunit=13
         inquire(file=trim(filename), exist=exist)
         if(exist) then
            open(iunit,file=trim(filename),form='unformatted')
               read(iunit) this%nlevel,this%nfcstmm,this%numvar,this%numstation,this%cleveltype,&
                           this%if_save_4sfcpoints

               if(allocated(this%rlevel)) deallocate(this%rlevel)
               if(this%cleveltype=="A") then
                  allocate(this%rlevel(this%nlevel))
               else
                  allocate(this%rlevel(this%nlevel+1))
               endif
               if(allocated(this%ifcstmm)) deallocate(this%ifcstmm)
               allocate(this%ifcstmm(this%nfcstmm))
               if(allocated(this%varnames)) deallocate(this%varnames)
               allocate(this%varnames(this%numvar))

               read(iunit) this%rlevel
               read(iunit) this%ifcstmm
               read(iunit) this%varnames
            
               if(this%numstation > 1) then
                  read(iunit) statarr%numsta
                  if(statarr%numsta >=1) then
                     if(allocated(statarr%stations)) deallocate(statarr%stations)
                     allocate(statarr%stations(statarr%numsta))

                     read(iunit) statarr%stations(1:statarr%numsta)%name
                     read(iunit) statarr%stations(1:statarr%numsta)%lon
                     read(iunit) statarr%stations(1:statarr%numsta)%lat
                     read(iunit) statarr%stations(1:statarr%numsta)%ele
                     read(iunit) statarr%stations(1:statarr%numsta)%xc
                     read(iunit) statarr%stations(1:statarr%numsta)%yc
                  endif
               endif

               if(allocated(this%rmse)) deallocate(this%rmse)
               allocate(this%rmse(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               allocate(rmse(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               if(allocated(this%bias)) deallocate(this%bias)
               allocate(this%bias(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               allocate(bias(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               if(allocated(this%numb)) deallocate(this%numb)
               allocate(this%numb(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               if(this%numstation>1 .and. this%if_save_4sfcpoints) then
                  if(allocated(this%value4)) deallocate(this%value4)
                  allocate(this%value4(this%numvar,4,this%nfcstmm,this%numstation))
               endif

               do while (.true.)
                  read(iunit,end=100) this%validdate, this%validmm
                  read(iunit) rmse
                  read(iunit) bias
                  read(iunit) this%numb
                  if(this%numstation>1 .and. this%if_save_4sfcpoints) read(iunit) this%value4
                  this%rmse=rmse
                  this%bias=bias
                  write(*,*) this%validdate, this%validmm
               enddo
100            continue
            close(iunit)
          
         else
            write(*,*) 'No file=',trim(filename)
         endif

      end subroutine read_statisticbase

      subroutine write_statisticbase_station(this,action,filename,statarr,timetag)
! write the statistic results
         implicit none
         class(statisticbase) :: this
         character(len=3),intent(in) :: action
         character(len=*), intent(in) :: filename
         type(stationarray), intent(in) :: statarr
         character(len=12),intent(in),optional :: timetag
        
         integer :: iunit
         integer :: i,j,k,n
         integer :: listnum
         logical :: exist

         if(present(timetag)) read(timetag,'(I10,I2)') this%validdate, this%validmm
         iunit=13
         if(action=='new') then
            open(iunit,file=trim(filename),form='unformatted')
               write(iunit) this%nlevel,this%nfcstmm,this%numvar,this%numstation,this%cleveltype,&
                            this%if_save_4sfcpoints
               write(iunit) this%rlevel
               write(iunit) this%ifcstmm
               write(iunit) this%varnames
            
               write(iunit) statarr%numsta
               write(iunit) statarr%stations(1:statarr%numsta)%name
               write(iunit) statarr%stations(1:statarr%numsta)%lon
               write(iunit) statarr%stations(1:statarr%numsta)%lat
               write(iunit) statarr%stations(1:statarr%numsta)%ele
               write(iunit) statarr%stations(1:statarr%numsta)%xc
               write(iunit) statarr%stations(1:statarr%numsta)%yc

               write(iunit) this%validdate, this%validmm
               write(iunit) real(this%rmse)
               write(iunit) real(this%bias)
               write(iunit) this%numb
               if(this%numstation>1 .and. this%if_save_4sfcpoints) write(iunit) this%value4
            close(iunit)
         elseif(action=='app') then
            inquire(file=trim(filename), exist=exist)
            if(exist) then
               open(iunit,file=trim(filename),form='unformatted',position='append')
                 write(iunit) this%validdate, this%validmm
                 write(iunit) real(this%rmse)
                 write(iunit) real(this%bias)
                 write(iunit) this%numb
                 if(this%numstation>1 .and. this%if_save_4sfcpoints) write(iunit) this%value4
               close(iunit)
            else
               write(*,*) 'Error, this file has to exist=',trim(filename)
               stop 345
            endif
         else
            write(*,*) 'unknow action for writing, has to one of new, app'
            stop 234
         endif

      end subroutine write_statisticbase_station

end module module_statistic
