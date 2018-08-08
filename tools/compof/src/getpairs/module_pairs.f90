module module_pairs

   use kinds, only: r_kind,r_single,rmissing
   use module_obs_conv_array, only : obs_conv_array
   use module_fcstobs,        only : fcstobs_array
   use module_fcstobs,        only : fcstobsbase

   implicit none

   public :: fcstobspairs
   private
   type, extends(fcstobs_array) :: fcstobspairs
      type(fcstobsbase), allocatable :: fcstminusobs(:) ! fsctobs - obs
      contains
         procedure :: destroypair => destroy_fcstobs_pairs
         procedure :: listpair => list_fcstobs_pairs
         procedure :: match => match_fcstobs_pairs
   end type

   contains
      
      subroutine destroy_fcstobs_pairs(this)
! read in station info
         implicit none
         class(fcstobspairs) :: this

         integer :: i

         if(this%num_obs > 0 ) then
            call this%destroy()

            do i=1,this%num_obs
               if(allocated(this%fcstminusobs(i)%obs)) deallocate(this%fcstminusobs(i)%obs)
            enddo

            if(allocated(this%fcstminusobs)) deallocate(this%fcstminusobs)
         endif

      end subroutine destroy_fcstobs_pairs

      subroutine match_fcstobs_pairs(this,obsarr)
! read in station info
         implicit none
         class(fcstobspairs) :: this
         type(obs_conv_array), intent(in) :: obsarr

         integer :: i,j
!
         if(this%num_obs > 0) then

            if(this%num_obs==obsarr%n_alloc) then
               if(this%num_var> 0) then

                  if(allocated(this%fcstminusobs)) deallocate(this%fcstminusobs)
                  allocate(this%fcstminusobs(this%num_obs))

                  do i=1,this%num_obs
                     if( (this%fcstobs(i)%lon==obsarr%obsarxy(i)%lon) .and. &
                         (this%fcstobs(i)%lat==obsarr%obsarxy(i)%lat) ) then
                        if(allocated(this%fcstminusobs(i)%obs)) deallocate(this%fcstminusobs(i)%obs)
                        allocate(this%fcstminusobs(i)%obs(this%num_var*this%fcstobs(i)%numlvl))

                        do j=1,this%num_var*this%fcstobs(i)%numlvl
                           if(abs(obsarr%obsarxy(i)%obs(j)) < 99999.0) then
                              this%fcstminusobs(i)%obs(j)=this%fcstobs(i)%obs(j)-obsarr%obsarxy(i)%obs(j)
                           else
                              this%fcstminusobs(i)%obs(j)=rmissing
                           endif
                        enddo
                     endif
                  enddo
               else
                  write(*,*) 'fcst obs array is empty',this%num_var,this%num_obs
               endif
            else
               this%num_obs=0
               write(*,*) 'Warning: number of fcst obs is not equal to obs number!'
            endif
         else
            write(*,*) 'No observations available'
         endif
      end subroutine match_fcstobs_pairs

      subroutine list_fcstobs_pairs(this,listnumin)
! read in station info
         implicit none
         class(fcstobspairs) :: this
         integer,optional,intent(in) :: listnumin
        
         integer :: i,j,k,idx
         integer :: listnum

         listnum=this%num_obs
         if(present(listnumin)) listnum=min(listnum,listnumin)
         if(this%num_var> 0 .and. listnum >0) then
            
            write(*,'(5I12,L)') this%num_var,this%num_obs,this%numlvl_model, &
                          this%inidate, this%inimm,this%if_save_4sfcpoints
            write(*,'(a,6I4)') 'ip,it,iq,ih,iu,iv=',this%ip,this%it,this%iq,this%ih,this%iu,this%iv

            write(*,'(10x,10a20)') (this%varname(j),j=1,this%num_var)
            do i=1,listnum
               write(*,'(I5,a,4F10.3,I10,2F10.3)') i,this%fcstobs(i)%name,this%fcstobs(i)%lon, &
                                this%fcstobs(i)%lat,this%fcstobs(i)%ele,  &
                                this%fcstobs(i)%time,this%fcstobs(i)%numlvl, &
                                this%fcstobs(i)%xc,this%fcstobs(i)%yc    
               do k=1,this%fcstobs(i)%numlvl
                  idx=(k-1)*this%num_var
                  write(*,'(I10,10f12.3)') k,(this%fcstobs(i)%obs(j+idx),j=1,this%num_var)
                  write(*,'(I10,10f12.3)') k,(this%fcstminusobs(i)%obs(j+idx),j=1,this%num_var)
               enddo
            enddo
            if(this%if_save_4sfcpoints) then
               write(*,*) 'list 4 surronding points==='
               do i=1,listnum
                  do k=1,4
                     write(*,'(I10,10f12.3)') k,(this%value4(i,j,k),j=1,this%num_var)
                  enddo
               enddo
            endif
         else
            write(*,*) 'fcst obs array is empty',this%num_var,this%num_obs
         endif

      end subroutine list_fcstobs_pairs

end module module_pairs
