module module_readwrt_stats_results

   use kinds, only: r_kind,r_single,rmissing
   use module_statistic,    only : statisticbase
   use module_stationarray, only : stationarray

   implicit none

   public :: readwrt_stats_results
   private
   type,extends(statisticbase) :: readwrt_stats_results
      integer :: num_timelevels
      contains
         procedure :: readwrt    => readwrt_statisticbase
         procedure :: reads       => read_statisticbase
   end type

   contains
      
      subroutine readwrt_statisticbase(this,filename,filehead,filedata,if_get_valid,filestations)
! write the statistic results
         implicit none
         class(readwrt_stats_results) :: this
         character(len=*), intent(in) :: filename
         character(len=*), intent(in) :: filehead
         character(len=*), intent(in) :: filedata
         logical ,         intent(in) :: if_get_valid
         character(len=*), intent(in),optional :: filestations
         type(stationarray) :: statarr
        
         integer :: iunit,iunit_head,iunit_data,iunit_sta,iunit_sta_valid
         integer :: i,j,k,n
         logical :: exist
         real,allocatable :: rmse(:,:,:,:),bias(:,:,:,:)
!
         real,allocatable :: rmse_valid(:,:,:,:,:),bias_valid(:,:,:,:,:)
         integer,allocatable :: numb_valid(:,:,:,:,:)
!
         integer :: i_save_4sfcpoints
         integer :: itime_valid,m
!
!
         this%num_timelevels=0
         iunit=13
         iunit_head=14
         iunit_data=15
         iunit_sta=16
         iunit_sta_valid=17
         inquire(file=trim(filename), exist=exist)
         if(exist) then
            open(iunit,file=trim(filename),form='unformatted')
            open(iunit_head,file=trim(filehead),form='unformatted')
            open(iunit_data,file=trim(filedata),form='unformatted')
               read(iunit) this%nlevel,this%nfcstmm,this%numvar,this%numstation,this%cleveltype,&
                           this%if_save_4sfcpoints
               write(iunit_head) this%nlevel,this%nfcstmm,this%numvar,this%numstation
               write(iunit_head) this%cleveltype 
               i_save_4sfcpoints=0
               if(this%if_save_4sfcpoints) i_save_4sfcpoints=1
               write(iunit_head) i_save_4sfcpoints

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
               write(iunit_head) this%rlevel
               write(iunit_head) this%ifcstmm
               write(iunit_head) this%varnames
            
               if(this%numstation > 1) then
                  read(iunit) statarr%numsta
                  write(iunit_head) statarr%numsta
                  if(statarr%numsta >=1) then
                     if(allocated(statarr%stations)) deallocate(statarr%stations)
                     allocate(statarr%stations(statarr%numsta))

                     read(iunit) statarr%stations(1:statarr%numsta)%name
                     read(iunit) statarr%stations(1:statarr%numsta)%lon
                     read(iunit) statarr%stations(1:statarr%numsta)%lat
                     read(iunit) statarr%stations(1:statarr%numsta)%ele
                     read(iunit) statarr%stations(1:statarr%numsta)%xc
                     read(iunit) statarr%stations(1:statarr%numsta)%yc
                     write(iunit_head) statarr%stations(1:statarr%numsta)%name
                     write(iunit_head) statarr%stations(1:statarr%numsta)%lon
                     write(iunit_head) statarr%stations(1:statarr%numsta)%lat
                     write(iunit_head) statarr%stations(1:statarr%numsta)%ele
                     write(iunit_head) statarr%stations(1:statarr%numsta)%xc
                     write(iunit_head) statarr%stations(1:statarr%numsta)%yc
                   
                     if(present(filestations)) then
                        open(iunit_sta,file=trim(filestations))
                           do i=1,statarr%numsta
                              write(iunit_sta,'(I5,1x,a8,1x,5f10.2)') i,statarr%stations(i)%name, &
                                                    statarr%stations(i)%lon,  &
                                                    statarr%stations(i)%lat,  &
                                                    statarr%stations(i)%ele,  &
                                                    statarr%stations(i)%xc,  &
                                                    statarr%stations(i)%yc
                           enddo
                        close(iunit_sta)
                     endif
                  endif
               endif

               if(allocated(this%rmse)) deallocate(this%rmse)
               allocate(rmse(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               if(allocated(this%bias)) deallocate(this%bias)
               allocate(bias(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               if(allocated(this%numb)) deallocate(this%numb)
               allocate(this%numb(this%numvar,this%nlevel,this%nfcstmm,this%numstation))
               if(this%numstation>1 .and. this%if_save_4sfcpoints) then
                  if(allocated(this%value4)) deallocate(this%value4)
                  allocate(this%value4(this%numvar,4,this%nfcstmm,this%numstation))
               endif
               if(this%numstation>1 .and. if_get_valid) then
                  allocate(rmse_valid(this%numvar,this%nlevel,this%nfcstmm,this%numstation,24))
                  rmse_valid=0.0
                  allocate(bias_valid(this%numvar,this%nlevel,this%nfcstmm,this%numstation,24))
                  bias_valid=0.0
                  allocate(numb_valid(this%numvar,this%nlevel,this%nfcstmm,this%numstation,24))
                  numb_valid=0
               endif

               do while (.true.)
                  read(iunit,end=100) this%validdate, this%validmm
                  write(iunit_data) this%validdate, this%validmm
                  this%num_timelevels=this%num_timelevels+1
                  read(iunit) rmse
                  read(iunit) bias
                  read(iunit) this%numb
                  if(this%numstation>1 .and. this%if_save_4sfcpoints) read(iunit) this%value4
                  write(iunit_data) rmse
                  write(iunit_data) bias
                  write(iunit_data) this%numb
                  if(this%numstation>1 .and. this%if_save_4sfcpoints) write(iunit_data) this%value4
                  write(*,*) 'validdate, validmm=',this%validdate, this%validmm
                  if(this%numstation>1 .and. if_get_valid) then
                     itime_valid=this%validdate-(this%validdate/100)*100 + 1
                      write(*,*) 'valid hour=',itime_valid
                     do i=1,this%numvar
                     do j=1,this%nlevel
                     do n=1,this%nfcstmm
                     do m=1,this%numstation
                        if(this%numb(i,j,n,m)==1) then
                           rmse_valid(i,j,n,m,itime_valid)=rmse_valid(i,j,n,m,itime_valid) + &
                                bias(i,j,n,m)*bias(i,j,n,m) 
                           bias_valid(i,j,n,m,itime_valid)=bias_valid(i,j,n,m,itime_valid) + &  
                                bias(i,j,n,m)
                           numb_valid(i,j,n,m,itime_valid)=numb_valid(i,j,n,m,itime_valid) + &
                                this%numb(i,j,n,m)
                        endif
                     enddo
                     enddo
                     enddo
                     enddo
                  endif
               enddo
100            continue

               write(iunit_head) this%num_timelevels
            
            deallocate(rmse)
            deallocate(bias)
            close(iunit)
            close(iunit_head)
            close(iunit_data)
!
            if(this%numstation>1 .and. if_get_valid) then
               do i=1,this%numvar
               do j=1,this%nlevel
               do n=1,this%nfcstmm
               do m=1,this%numstation
               do itime_valid=1,24
                  if(numb_valid(i,j,n,m,itime_valid) > 0) then
                     rmse_valid(i,j,n,m,itime_valid)=rmse_valid(i,j,n,m,itime_valid)/ &
                          float(numb_valid(i,j,n,m,itime_valid)) 
                     bias_valid(i,j,n,m,itime_valid)=bias_valid(i,j,n,m,itime_valid)/ &  
                          float(numb_valid(i,j,n,m,itime_valid)) 
                  else
                     rmse_valid(i,j,n,m,itime_valid)=-99999.0
                     bias_valid(i,j,n,m,itime_valid)=-99999.0  
                  endif
               enddo
               enddo
               enddo
               enddo
               enddo
               open(iunit_sta_valid,file=trim(filedata)//"_valid",form='unformatted')
                  do k=1,24
                     write(iunit_sta_valid) k,0
                     write(iunit_sta_valid) rmse_valid(:,:,:,:,k)
                     write(iunit_sta_valid) bias_valid(:,:,:,:,k)
                     write(iunit_sta_valid) numb_valid(:,:,:,:,k)
                  enddo
               close(iunit_sta_valid)
               deallocate(rmse_valid)
               deallocate(bias_valid)
               deallocate(numb_valid)
            endif
          
         else
            write(*,*) 'No file=',trim(filename)
         endif

      end subroutine readwrt_statisticbase

      subroutine read_statisticbase(this,filename)
! write the statistic results
         implicit none
         class(readwrt_stats_results) :: this
         character(len=*), intent(in) :: filename
         type(stationarray) :: statarr
        
         integer :: iunit
         integer :: i,j,k,n
         logical :: exist
         real,allocatable :: rmse(:,:,:,:),bias(:,:,:,:)

         this%num_timelevels=0
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
                  write(*,*) 'validdate, validmm=',this%validdate, this%validmm
               enddo
100            continue
            close(iunit)
          
         else
            write(*,*) 'No file=',trim(filename)
         endif

      end subroutine read_statisticbase

end module module_readwrt_stats_results
