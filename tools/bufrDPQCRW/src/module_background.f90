module module_background
!
! prepare background
!
  use module_ncio, only: ncio
  use module_map_utils, only : map_util

  implicit none
  integer, parameter :: r_kind=8
  integer, parameter :: r_single=4

  public :: background
! set default to private
  private

  type :: background

     character(len=180) :: bkfile
     integer :: nx,ny,nz
     real(r_single), allocatable :: bku(:,:,:)
     real(r_single), allocatable :: bkv(:,:,:)
     real(r_single), allocatable :: bkw(:,:,:)
     real(r_single), allocatable :: bkh(:,:,:)
     real(r_single), allocatable :: bkterrain(:,:)

  contains
     procedure ::  initial   => initial_background
     procedure ::  initialmap=> initial_map
     procedure ::  destroy   => destroy_background
  end type background


  contains

     subroutine initial_map(this,bkfile,map)
!
!  set up dimension and allocate memory
!
        implicit none

        class(background),intent(inout) :: this
        type(map_util),intent(inout)    :: map
        character(len=*),intent(in)     :: bkfile

        type(ncio)    :: bkio
        integer       :: nx,ny
        real(r_single),allocatable :: rlon2d(:,:),rlat2d(:,:)

        call bkio%open(trim(bkfile),'r',200)
        call bkio%get_dim("west_east",nx)
        call bkio%get_dim("south_north",ny)
        write(*,*) 'nx,ny,nz=',nx,ny

        allocate(rlon2d(nx,ny))
        allocate(rlat2d(nx,ny))
        call bkio%get_var("XLONG",nx,ny,rlon2d)
        call bkio%get_var("XLAT",nx,ny,rlat2d)
!
        call map%init_general_transform(nx,ny,rlat2d,rlon2d)
!
        deallocate(rlon2d,rlat2d)

        call bkio%close()

     end subroutine initial_map

     subroutine initial_background(this,bkfile)
!
!  set up dimension and allocate memory
!
        implicit none

        class(background),intent(inout) :: this
        character(len=*),intent(in)      :: bkfile

        type(ncio) :: bkio
        real(r_single),allocatable :: tmp3d(:,:,:)
        integer :: nx,ny,nz
        integer :: i,j,k

        this%bkfile=trim(bkfile)
        this%nx=0
        this%ny=0
        this%nz=0
 
        
        call bkio%open(trim(bkfile),'r',0)
        call bkio%get_dim("west_east",this%nx)
        call bkio%get_dim("south_north",this%ny)
        call bkio%get_dim("bottom_top",this%nz)
        write(*,*) 'nx,ny,nz=',this%nx,this%ny,this%nz
        nx=this%nx
        ny=this%ny
        nz=this%nz

! get terrain
        if(allocated(this%bkterrain)) deallocate(this%bkterrain)
        allocate(this%bkterrain(this%nx,this%ny))
        call bkio%get_var("HGT",nx,ny,this%bkterrain)

! get 3D height
        if(allocated(this%bkh)) deallocate(this%bkh)
        allocate(this%bkh(this%nx,this%ny,this%nz))
        allocate(tmp3d(this%nx,this%ny,this%nz+1))
        call bkio%get_var("PH",nx,ny,nz+1,tmp3d)
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 this%bkh(i,j,k)=(tmp3d(i,j,k)+tmp3d(i,j,k+1))*0.5_r_single
              enddo
           enddo
        enddo
        call bkio%get_var("PHB",nx,ny,nz+1,tmp3d)
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 this%bkh(i,j,k)=(this%bkh(i,j,k)+(tmp3d(i,j,k)+tmp3d(i,j,k+1))*0.5_r_single)
              enddo
           enddo
        enddo
        do k=1,nz
           this%bkh(:,:,k)=this%bkh(:,:,k)/9.8_r_single-this%bkterrain(:,:)
           write(*,*) 'height =',k,maxval(this%bkh(:,:,k)),minval(this%bkh(:,:,k))
        enddo
        
! get U
        if(allocated(this%bku)) deallocate(this%bku)
        allocate(this%bku(this%nx,this%ny,this%nz))
        deallocate(tmp3d)
        allocate(tmp3d(this%nx+1,this%ny,this%nz))
        call bkio%get_var("U",nx+1,ny,nz,tmp3d)
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 this%bku(i,j,k)=(tmp3d(i,j,k)+tmp3d(i+1,j,k))*0.5_r_single
              enddo
           enddo
        enddo
! get V
        if(allocated(this%bkv)) deallocate(this%bkv)
        allocate(this%bkv(this%nx,this%ny,this%nz))
        deallocate(tmp3d)
        allocate(tmp3d(this%nx,this%ny+1,this%nz))
        call bkio%get_var("V",nx,ny+1,nz,tmp3d)
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 this%bkv(i,j,k)=(tmp3d(i,j,k)+tmp3d(i,j+1,k))*0.5_r_single
              enddo
           enddo
        enddo
! get W
        if(allocated(this%bkw)) deallocate(this%bkw)
        allocate(this%bkw(this%nx,this%ny,this%nz))
        deallocate(tmp3d)
        allocate(tmp3d(this%nx,this%ny,this%nz+1))
        call bkio%get_var("W",nx,ny,nz+1,tmp3d)
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 this%bkv(i,j,k)=(tmp3d(i,j,k)+tmp3d(i,j,k+1))*0.5_r_single
              enddo
           enddo
        enddo

        deallocate(tmp3d)
!
        call bkio%close()

     end subroutine initial_background

     subroutine destroy_background(this)
!
!  release memory
!
        implicit none

        class(background),intent(inout) :: this

        this%bkfile=""
        this%nx=0
        this%ny=0
        this%nz=0
        if(allocated(this%bku)) deallocate(this%bku)
        if(allocated(this%bkv)) deallocate(this%bkv)
        if(allocated(this%bkw)) deallocate(this%bkw)
        if(allocated(this%bkh)) deallocate(this%bkh)
        if(allocated(this%bkterrain)) deallocate(this%bkterrain)

     end subroutine destroy_background

End module module_background
