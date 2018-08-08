module module_obs_location
!
!  this module defines observation data structure and the method to 
!           to find observation grid location.
!
!
   use kinds, only: r_kind,r_single,len_sta_name,rmissing
   use module_map_utils, only : map_util

   implicit none

   public :: obs_array

   private
   type :: obs_location
      character(len=8) :: name
      real(r_single) :: lat
      real(r_single) :: lon
      real(r_single) :: xc
      real(r_single) :: yc
   endtype obs_location

   type :: obs_array
      integer :: num_obs=0
      integer :: datatype=0
      type(obs_location),allocatable :: obsarxy(:) 
      contains
          procedure :: write    => write_obs_array
          procedure :: read     => read_obs_array
          procedure :: list     => list_obs_array
          procedure :: destroy  => destroy_obs_array
          procedure :: initial  => initial_obs_array
          procedure :: calculatexy => calculatexy_obs
   end type obs_array

   contains

      subroutine list_obs_array(this,minlist)
! display the content in a conventional obs list
         class(obs_array) :: this
         integer, intent(in), optional :: minlist
         integer :: i,k,ii
         integer :: listnum
      
         write(*,*) '====list this observation====',this%datatype
      
         if(this%num_obs <=0) then
            write(*,*) 'No obs available!'
            return
         endif
         listnum=this%num_obs
         if(present(minlist)) listnum=min(this%num_obs,minlist)

         write(*,'(a,4I10)') 'num_obs,datatype=',this%num_obs,this%datatype

         write(*,'(a16,4x,6a10)') "name","lon","lat","xc","yc"
         do i=1,listnum
            write(*,'(I10,2x,a,6f10.2)') i,this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%xc,this%obsarxy(i)%yc
         enddo

      end subroutine list_obs_array
!
      subroutine read_obs_array(this,filename)
! read in obsservations from obs_conv_xy
         implicit none
         class(obs_array) :: this
         character(len=*),intent(in) :: filename
        
         integer :: i,j,k,numobs
         integer :: iunit
         logical :: ifexist

         iunit=10
         inquire( file=trim(filename), EXIST=ifexist )
         if(ifexist) then
            write(*,'(a,a)') 'read_obs_array:',trim(filename)
            open(iunit,file=trim(filename),form='unformatted',status='old')
               read(iunit) this%num_obs,this%datatype
               write(*,*) 'read in=',this%num_obs,this%datatype
               allocate(this%obsarxy(this%num_obs))
               do i=1,this%num_obs
                  read(iunit) this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%xc,this%obsarxy(i)%yc
               enddo
            close(iunit)
         else
            this%num_obs=0
            write(*,'(a,a,a)') 'read_obs_array:',trim(filename),' does not exist!'
         endif

      end subroutine read_obs_array
!
      subroutine write_obs_array(this,filename)
! write obs in binary file
         implicit none
         class(obs_array) :: this
         character(len=*),intent(in) :: filename
        
         integer :: i, iunit
         character(len=180) :: filenameall

         iunit=10

         write(filenameall,'(a)') trim(filename)  

         if(this%num_obs>0) then
            write(*,'(a,I8,a,I8)') '>>>write_obs_array: Write',this%num_obs,& 
                                 ' stations for data type:',this%datatype
         else
            write(*,*) '>>>write_obs_array: NO ',this%datatype,' station'
            return
         endif

         write(*,'(a,a)') 'write data to:',trim(filenameall)
         open(iunit,file=trim(filenameall),form='unformatted')
            write(iunit) this%num_obs,this%datatype

            do i=1,this%num_obs
               write(iunit) this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%xc,this%obsarxy(i)%yc
            enddo

         close(iunit)

      end subroutine write_obs_array

      subroutine initial_obs_array(this,numobs)
! release memory 
         implicit none
         class(obs_array) :: this
         integer, intent(in) :: numobs
        
         this%num_obs=numobs
         if(this%num_obs > 0) then
            if(allocated(this%obsarxy)) deallocate(this%obsarxy)
            allocate(this%obsarxy(this%num_obs))
         endif

      end subroutine initial_obs_array

      subroutine destroy_obs_array(this)
! release memory 
         implicit none
         class(obs_array) :: this
        
         if(this%num_obs > 0) then
            if(allocated(this%obsarxy)) deallocate(this%obsarxy)
         else
            write(*,*) 'No data in this data target'
         endif
         this%num_obs=0

      end subroutine destroy_obs_array

      subroutine calculatexy_obs(this,mapin)
! calculate grid  coordinate
         implicit none
         class(obs_array) :: this
         type(map_util) ,intent(in) :: mapin
         integer :: i
         real(r_single) :: xc,yc,rlon,rlat

         if(this%num_obs>0) then
            write(*,*) "fill in the grid coordinate info xc,yc"
            do i=1,this%num_obs
                call mapin%tll2xy(this%obsarxy(i)%lon, this%obsarxy(i)%lat, &
                                xc,yc)
                if( (xc >=1 .and. xc <=mapin%nlon) .and. &
                    (yc >=1 .and. yc <=mapin%nlat) ) then
                   this%obsarxy(i)%xc=xc
                   this%obsarxy(i)%yc=yc
                else
                   this%obsarxy(i)%xc=-1
                   this%obsarxy(i)%yc=-1
                endif
            enddo
         else
            write(*,*) 'calculatxy_conv: No observations in this array'
         endif

      end subroutine calculatexy_obs

end module module_obs_location
