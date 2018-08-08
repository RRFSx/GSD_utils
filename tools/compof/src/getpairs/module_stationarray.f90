module module_stationarray

   use kinds, only: r_kind,r_single,len_sta_name,rmissing
   use module_obs_conv_array, only : obs_conv_array
   use module_hashsearch, only : hashtable

   implicit none

   public :: stationarray
   private

   type :: station
      character(len=len_sta_name) :: name
      real :: lat
      real :: lon
      real :: ele
      real :: xc
      real :: yc
   end type station
   
   type :: stationarray
      integer :: numsta
      integer :: maxnum
      type(station), allocatable :: stations(:)
      contains
         procedure :: initial => initial_stationarray
         procedure :: destroy => destroy_stationarray
         procedure :: list    => list_stationarray
         procedure :: make    => make_stationarray
   end type

   contains
      
      subroutine initial_stationarray(this,maxnum)
! initial station info
         implicit none
         class(stationarray) :: this
         integer, intent(in) :: maxnum

         if(maxnum > 0 ) then
            this%maxnum=maxnum
            this%numsta=0
            if(allocated(this%stations)) deallocate(this%stations)
            allocate(this%stations(this%maxnum))
         endif

      end subroutine initial_stationarray

      subroutine destroy_stationarray(this)
! destroy station info
         implicit none
         class(stationarray) :: this

         if(this%maxnum > 0 ) then
            this%maxnum=0
            this%numsta=0
            if(allocated(this%stations)) deallocate(this%stations)
         endif

      end subroutine destroy_stationarray

      subroutine make_stationarray(this,orgobs,hashobs)
! find station location
         implicit none
         class(stationarray) :: this
         type(obs_conv_array),intent(in) :: orgobs
         type(hashtable), intent(in) :: hashobs
        
         integer :: i,j
         real :: xc,yc
         integer :: hin
         character(len=len_sta_name) :: name

         j=this%numsta
         if(orgobs%n_alloc>0) then
            do i=1,orgobs%n_alloc
              
               j=this%numsta+1
               name=orgobs%obsarxy(i)%name
               xc=orgobs%obsarxy(i)%xc
               yc=orgobs%obsarxy(i)%yc

               call hashobs%make(name,int(xc),int(yc),j)
               if(j == 0) cycle
               if(j > this%maxnum) then
                  write(*,*) 'Too many stations, increase maxnum',this%maxnum
                  stop  456
               endif
               this%stations(j)%name=orgobs%obsarxy(i)%name
               this%stations(j)%lon=orgobs%obsarxy(i)%lon
               this%stations(j)%lat=orgobs%obsarxy(i)%lat
               this%stations(j)%ele=orgobs%obsarxy(i)%ele
               this%stations(j)%xc=orgobs%obsarxy(i)%xc
               this%stations(j)%yc=orgobs%obsarxy(i)%yc
               this%numsta=j
            enddo

         endif
      end subroutine make_stationarray
!
      subroutine list_stationarray(this,listnumin)
! list station info
         implicit none
         class(stationarray) :: this
         integer,optional,intent(in) :: listnumin
        
         integer :: i,j,k,idx
         integer :: listnum

         listnum=this%numsta
         if(present(listnumin)) listnum=min(listnum,listnumin)
         if(listnum >0) then
            
            write(*,*) 'The station number is ',this%numsta
            do i=1,listnum
               write(*,'(I5,1x,a,3F10.3,2F10.1)') i,this%stations(i)%name,this%stations(i)%lon, &
                     this%stations(i)%lat,this%stations(i)%ele,this%stations(i)%xc,&
                     this%stations(i)%yc
            enddo
         else
            write(*,*) 'station array is empty'
         endif

      end subroutine list_stationarray

end module module_stationarray
