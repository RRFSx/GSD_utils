module module_radar_station_config
!
! read in radar station latlon, height of above MSL and ground
! 
!

  implicit none

  public :: rdrsta_config
! set default to private
  private

  type :: rdrsta_config

     character(len=180) :: crsfile
     integer :: numsta
     character(len=4),allocatable :: radarvName(:)
     real,allocatable  :: rlat(:)
     real, allocatable :: rlon(:)
     real, allocatable :: hsmsl(:)
     real, allocatable :: hsalg(:)

  contains
     procedure ::  readcf  => read_rdrsta_config
     procedure ::  findid  => get_rdrsta_id
     procedure ::  destroy => destroy_rdrsta_config
  end type rdrsta_config


  contains

     function get_rdrsta_id(this,radarvName) result(id)
! from station name to find station id (number)
        implicit none
        class(rdrsta_config),intent(in) :: this
        character(len=4),    intent(in) :: radarvName
        integer :: id

        integer :: i
!
        id=0
        if(this%numsta > 0) then
           do i=1,this%numsta
              if(radarvName==this%radarvName(i)) id=i
           enddo
        endif
!
        if(id <=0 .and. id > this%numsta) id=0
     end function 
!
     subroutine read_rdrsta_config(this,crsfile)
!
!  read in radar station files
!
        implicit none

        class(rdrsta_config),intent(inout) :: this
        character(len=*),intent(in)  :: crsfile
    
        integer :: i,k
        logical :: ifexist
        character(len=8)  :: cstation

        !
        this%crsfile=trim(crsfile)
        inquire(file=trim(crsfile),exist=ifexist)
        if(ifexist) then
           open(12,file=trim(crsfile))
              read(12,*) this%numsta
              
              if(this%numsta <=0) then
                 write(*,*) 'station number is wrong', this%numsta
                 stop 123
              endif
              allocate(this%radarvName(this%numsta))
              allocate(this%rlat(this%numsta))
              allocate(this%rlon(this%numsta))
              allocate(this%hsmsl(this%numsta))
              allocate(this%hsalg(this%numsta))
              do k=1,this%numsta
                 read(12,'(a14,10f12.4)') cstation,this%rlat(k), &
                       this%rlon(k),this%hsmsl(k),this%hsalg(k)
                 this%radarvName(k)=trim(cstation)
              enddo
              close(12)
        else
           this%numsta=0
           write(*,*) 'radar station file is not existing'
        endif

!              do k=1,this%numsta
!                 write(*,'(a14,10f12.4)') this%radarvName(k),this%rlat(k), &
!                       this%rlon(k),this%hsmsl(k),this%hsalg(k)
!              enddo

     end subroutine read_rdrsta_config

     subroutine destroy_rdrsta_config(this)
!
!  release memory
!
        implicit none

        class(rdrsta_config),intent(inout) :: this

        this%numsta=0
        if(allocated(this%radarvName)) deallocate(this%radarvName)
        if(allocated(this%rlat)) deallocate(this%rlat)
        if(allocated(this%rlon)) deallocate(this%rlon)
        if(allocated(this%hsmsl)) deallocate(this%hsmsl)
        if(allocated(this%hsalg)) deallocate(this%hsalg)

     end subroutine destroy_rdrsta_config

End module module_radar_station_config
