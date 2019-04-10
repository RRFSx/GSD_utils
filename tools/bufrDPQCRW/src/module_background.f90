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
     real(r_single), pointer  :: bku(:,:,:)
     real(r_single), pointer  :: bkv(:,:,:)
     real(r_single), pointer  :: bkw(:,:,:)
     real(r_single), pointer  :: bkh(:,:,:)
     real(r_single), pointer  :: bkterrain(:,:)
     real(r_single), pointer  :: pt3d(:,:,:)
     real(r_single), pointer  :: pt2d(:,:)

  contains
     procedure ::  initial   => initial_background
     procedure ::  initialmap=> initial_map
     procedure ::  destroy   => destroy_background
     procedure ::  listflds  => list_fields
     procedure ::  listfld   => list_one_field
     procedure ::  interp2d  => interp_horizen_2d
     procedure ::  interp2dv => interp_horizen_2d_vertical
  end type background


  contains

     subroutine interp_horizen_2d_vertical(this,varname,nlvl,xc,yc,rval)
!
!  list content in this objects
!
        implicit none

        class(background),intent(inout) :: this
        character(*),intent(in)      :: varname
        integer, intent(in)          :: nlvl
        real(r_single), intent(in)   :: xc,yc
        real(r_single), intent(out)  :: rval(nlvl)
!
        integer :: ii,jj,k
        real(r_single), pointer  :: pt(:,:,:)
        real(r_single) :: dx,dy
!
        pt=>NULL()
        ii=int(xc)
        jj=int(yc)
        if(nlvl == this%nz ) then
           if( (ii >=1 .and. ii <this%nx) .and. &
               (jj >=1 .and. jj <this%ny) ) then
              if(trim(varname)=='U') pt=>this%bku(:,:,:)
              if(trim(varname)=='V') pt=>this%bkv(:,:,:)
              if(trim(varname)=='W') pt=>this%bkw(:,:,:)
              if(trim(varname)=='H') pt=>this%bkh(:,:,:)
           else
              write(*,*) 'input xc,yc is out of domain:',xc,yc,this%nx,this%ny
              rval=-999.0
           endif
        else
           write(*,*) 'input level is out of limit:',nlvl,this%nz
           rval=-999.0
        endif

        if(associated(pt)) then
           dx=xc-ii
           dy=yc-jj
           do k=1,nlvl
              if(abs(pt(ii,jj,k))   < 90000.0 .and. abs(pt(ii+1,jj,k))   < 90000.0 .and. &
                 abs(pt(ii,jj+1,k)) < 90000.0 .and. abs(pt(ii+1,jj+1,k)) < 90000.0 ) then
                 rval(k) = pt(ii,jj,k)*(1.0-dx)*(1.0-dy) + &
                           pt(ii+1,jj,k)*dx*(1.0-dy) + &
                           pt(ii,jj+1,k)*(1.0-dx)*dy + &
                           pt(ii+1,jj+1,k)*dx*dy
              else
                 write(*,*) 'invalid grid values:'
                 write(*,*) pt(ii,jj,k),pt(ii+1,jj,k),pt(ii,jj+1,k),pt(ii+1,jj+1,k)
                 rval=-999.0
              endif
           enddo
        endif

     end subroutine interp_horizen_2d_vertical

     subroutine interp_horizen_2d(this,varname,klvl,xc,yc,rval)
!
!  list content in this objects
!
        implicit none

        class(background),intent(inout) :: this
        character(*),intent(in)      :: varname
        integer, intent(in)          :: klvl
        real(r_single), intent(in)   :: xc,yc
        real(r_single), intent(out)  :: rval
!
        integer :: ii,jj,k
        real(r_single), pointer  :: pt2d(:,:)
        real(r_single) :: dx,dy
!
        pt2d=>NULL()
        ii=int(xc)
        jj=int(yc)
        if(klvl <=this%nz .and. klvl >=1) then
           if( (ii >=1 .and. ii <this%nx) .and. &
               (jj >=1 .and. jj <this%ny) ) then
              if(trim(varname)=='U') pt2d=>this%bku(:,:,klvl)
              if(trim(varname)=='V') pt2d=>this%bkv(:,:,klvl)
              if(trim(varname)=='W') pt2d=>this%bkw(:,:,klvl)
              if(trim(varname)=='H') pt2d=>this%bkh(:,:,klvl)
              if(trim(varname)=='terrain') pt2d=>this%bkterrain
           else
              write(*,*) 'input xc,yc is out of domain:',xc,yc,this%nx,this%ny
              rval=-999.0
           endif
        else
           write(*,*) 'input level is out of limit:',klvl,this%nz
           rval=-999.0
        endif

        if(associated(pt2d)) then
           dx=xc-ii
           dy=yc-jj
           if(abs(pt2d(ii,jj))   < 500.0 .and. abs(pt2d(ii+1,jj))   < 500.0 .and. &
              abs(pt2d(ii,jj+1)) < 500.0 .and. abs(pt2d(ii+1,jj+1)) < 500.0 ) then
              rval = pt2d(ii,jj)*(1.0-dx)*(1.0-dy) + &
                     pt2d(ii+1,jj)*dx*(1.0-dy) + &
                     pt2d(ii,jj+1)*(1.0-dx)*dy + &
                     pt2d(ii+1,jj+1)*dx*dy
           else
              write(*,*) 'invalid grid values:'
              write(*,*) pt2d(ii,jj),pt2d(ii+1,jj),pt2d(ii,jj+1),pt2d(ii+1,jj+1)
              rval=-999.0
           endif
        endif

     end subroutine interp_horizen_2d

     subroutine list_one_field(this,varname)
!
!  list content in this objects
!
        implicit none

        class(background),intent(inout) :: this
        character(*),intent(in)      :: varname
!
        integer :: i,j,k
        real(r_single), pointer  :: pt3d(:,:,:)
        real(r_single), pointer  :: pt2d(:,:)
!
        pt3d=>NULL()
        pt2d=>NULL()
        write(*,*) '===Check background field for ',trim(varname),'====='
        write(*,*) 'nx,ny,nz=',this%nx,this%ny,this%nz
        if(trim(varname)=='U') pt3d=>this%bku
        if(trim(varname)=='V') pt3d=>this%bkv
        if(trim(varname)=='W') pt3d=>this%bkw
        if(trim(varname)=='H') pt3d=>this%bkh
        if(trim(varname)=='terrain') pt2d=>this%bkterrain

        if(associated(pt3d)) then
           do k=1,this%nz
              write(*,*) trim(varname),k,maxval(pt3d(:,:,k)),minval(pt3d(:,:,k))
           enddo
        endif
        if(associated(pt2d)) then
           write(*,*) trim(varname),maxval(pt2d(:,:)),minval(pt2d(:,:))
        endif
!
     end subroutine list_one_field

     subroutine list_fields(this)
!
!  list content in this objects
!
        implicit none

        class(background),intent(in) :: this
!
        integer :: i,j,k
!
        write(*,*) '===Check background fields================='
        write(*,*) 'nx,ny,nz=',this%nx,this%ny,this%nz

        do k=1,this%nz
           write(*,*) 'U ',k,maxval(this%bku(:,:,k)),minval(this%bku(:,:,k))
        enddo
        do k=1,this%nz
           write(*,*) 'V ',k,maxval(this%bkv(:,:,k)),minval(this%bkv(:,:,k))
        enddo
        do k=1,this%nz
           write(*,*) 'W ',k,maxval(this%bkw(:,:,k)),minval(this%bkw(:,:,k))
        enddo
        do k=1,this%nz
           write(*,*) 'H ',k,maxval(this%bkh(:,:,k)),minval(this%bkh(:,:,k))
        enddo
        write(*,*) 'Terrian ',maxval(this%bkterrain(:,:)),minval(this%bkterrain(:,:))
!
     end subroutine list_fields

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
        if(associated(this%bkterrain)) deallocate(this%bkterrain)
        allocate(this%bkterrain(this%nx,this%ny))
        call bkio%get_var("HGT",nx,ny,this%bkterrain)

! get 3D height
        if(associated(this%bkh)) deallocate(this%bkh)
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
!           write(*,*) 'height =',k,maxval(this%bkh(:,:,k)),minval(this%bkh(:,:,k))
        enddo
        
! get U
        if(associated(this%bku)) deallocate(this%bku)
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
        if(associated(this%bkv)) deallocate(this%bkv)
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
        if(associated(this%bkw)) deallocate(this%bkw)
        allocate(this%bkw(this%nx,this%ny,this%nz))
        deallocate(tmp3d)
        allocate(tmp3d(this%nx,this%ny,this%nz+1))
        call bkio%get_var("W",nx,ny,nz+1,tmp3d)
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 this%bkw(i,j,k)=(tmp3d(i,j,k)+tmp3d(i,j,k+1))*0.5_r_single
              enddo
           enddo
        enddo

        deallocate(tmp3d)
!
        call bkio%close()
!
        this%pt3d=>NULL()
        this%pt2d=>NULL()

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
        if(associated(this%bku)) deallocate(this%bku)
        if(associated(this%bkv)) deallocate(this%bkv)
        if(associated(this%bkw)) deallocate(this%bkw)
        if(associated(this%bkh)) deallocate(this%bkh)
        if(associated(this%bkterrain)) deallocate(this%bkterrain)

     end subroutine destroy_background

End module module_background
