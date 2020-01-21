module module_cal_spread
!
!  read in regional ensemble perturbations from 
!  HRRRE and calculate ensemble spread
!
   use kinds, only: r_kind,r_single

   implicit none
   
   public :: cal_spread
   
   private

   type :: cal_spread

      integer :: numens
      character(len=10) :: varname
      integer :: nx,ny,nz
      real(r_kind), allocatable :: sprd(:,:,:)

   contains
      procedure :: initial => initialSpread
      procedure :: readadd => readAddPerturbation
      procedure :: calsprd => calSpread
      procedure :: wrtprd =>  writeSpread
      procedure :: destroy => destroy_sprd

   end type cal_spread

   contains

   subroutine initialSpread(this,filein,varname)
!
! initialize the type, mainly set the dimension 
! and allocate array
!
      class(cal_spread), intent(inout) :: this
      character(len=*), intent(in) :: filein
      character(len=*), intent(in) :: varname

      integer :: iunit
      logical :: file_exists 
  
      this%nx=0
      this%ny=0
      this%nz=0

      iunit=12
      inquire(file=trim(filein), exist=file_exists)
      if(file_exists) then
         open(iunit,file=trim(filein),form='unformatted',convert='BIG_ENDIAN')
            read(iunit) this%nx,this%ny,this%nz
            if(allocated(this%sprd)) deallocate(this%sprd)
            if(trim(varname)=='ps') this%nz=1
            allocate(this%sprd(this%nx,this%ny,this%nz))
            this%sprd=0.0_r_kind
         close(iunit)
      else
         write(*,*) 'initialSpread Error, cannot open file ',trim(filein)
         stop 123
      endif

      this%numens=0
      this%varname=trim(varname)

   end subroutine initialSpread
!
   subroutine readAddPerturbation(this,filein)
!
!  read one field
!
      class(cal_spread), intent(inout) :: this
      character(len=*), intent(in) :: filein

      integer :: iunit
      logical :: file_exists 
      integer :: nx,ny,nz
      real(r_single),allocatable :: fld3d(:,:,:)

      integer :: i,j,k

      iunit=12
      inquire(file=trim(filein), exist=file_exists)
      if(file_exists) then
         open(iunit,file=trim(filein),form='unformatted',convert='BIG_ENDIAN')
            read(iunit) nx,ny,nz
! read and add  ENSEMBLE perturbations 
            if(trim(this%varname)=='ps') nz=1
            if(nx==this%nx .and. ny==this%ny .and. nz==this%nz) then
               if(trim(this%varname)=='ps') then
                  allocate(fld3d(ny,nx,nz))
                  read(iunit) fld3d      ! ps
               elseif(trim(this%varname)=='tv') then
                  allocate(fld3d(ny,nx,nz))
                  read(iunit) 
                  do k=1,nz
                  read(iunit) fld3d(:,:,k)      ! tv
                  enddo
               elseif(trim(this%varname)=='u') then
                  allocate(fld3d(ny,nx,nz))
                  read(iunit) 
                  read(iunit) 
                  do k=1,nz
                  read(iunit) fld3d(:,:,k)
                  enddo
               elseif(trim(this%varname)=='v') then
                  allocate(fld3d(ny,nx,nz))
                  read(iunit)
                  read(iunit)
                  read(iunit)
                  do k=1,nz
                  read(iunit) fld3d(:,:,k)
                  enddo
               elseif(trim(this%varname)=='rh') then
                  allocate(fld3d(ny,nx,nz))
                  read(iunit)
                  read(iunit)
                  read(iunit)
                  read(iunit)
                  do k=1,nz
                  read(iunit) fld3d(:,:,k)    
                  enddo
               else
                  write(*,*) 'readAddPerturbation Error, unknow variable ',trim(this%varname)
                  stop 123
               endif
               write(*,*) 'read in ',trim(this%varname),maxval(fld3d),minval(fld3d)
               do k=1,nz
               do j=1,nx
               do i=1,ny
                  this%sprd(j,i,k)=this%sprd(j,i,k) + fld3d(i,j,k)*fld3d(i,j,k)
               enddo
               enddo
               enddo
               write(*,*) 'sum ',this%numens,maxval(this%sprd),minval(this%sprd)
               this%numens=this%numens+1
               deallocate(fld3d)
            else
               write(*,*) 'readAddPerturbation Error, dimension mismatch'
               write(*,*) 'dimension read in=',nx,ny,nz
               write(*,*) 'saved dimension=',this%nx,this%ny,this%nz
               stop 123
            endif
         close(iunit)

      else
         write(*,*) 'initialSpread Error, cannot open file ',trim(filein)
         stop 123
      endif

   end subroutine readAddPerturbation

   subroutine writeSpread(this,fileout)
!
! accumulate the perturbations
!
      class(cal_spread), intent(inout) :: this
      character(len=*) , intent(in)    :: fileout

      integer :: iunit

      iunit=12
      write(*,*) 'write ',trim(this%varname),' spread to',trim(fileout)
      open(iunit,file=trim(fileout),form='unformatted',convert='BIG_ENDIAN')
         write(iunit) this%nx,this%ny,this%nz,this%numens
         write(iunit) this%sprd
      close(iunit)

   end subroutine writeSpread 
!
   subroutine calSpread(this)
!
! accumulate the perturbations
!
      class(cal_spread), intent(inout) :: this

      real(r_kind) :: rnumens

      rnumens=this%numens-1
      rnumens=1.0_r_kind/rnumens
      this%sprd=this%sprd*rnumens
      this%sprd=sqrt(this%sprd)
      write(*,*) 'spread ',this%numens,trim(this%varname),maxval(this%sprd),minval(this%sprd)

   end subroutine calSpread
!
   subroutine destroy_sprd(this)
! release aray
      class(cal_spread), intent(inout) :: this

      this%numens=0
      this%nx=0
      this%ny=0
      this%nz=0
      this%varname=''
      if(allocated(this%sprd)) deallocate(this%sprd)

   end subroutine destroy_sprd

end module module_cal_spread
