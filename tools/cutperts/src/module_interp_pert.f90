module module_interp_perturbations
!
!  read in regional ensemble perturbations from 
!  HRRRE and interpolate them to another grid
!
   implicit none
   
   public :: interp_perts
   
   private

   type :: interp_perts

      integer :: numens
      integer :: nxorg,nyorg
      integer :: nxnew,nynew
      real, allocatable :: weights(:,:,:)   ! i,j,w
      integer, allocatable :: ijloc(:,:,:)  ! i,j,2

   contains
      procedure :: initial  => initialperts
      procedure :: rwinterp => readinterpwrite
      procedure :: genw     => gen_weights
      procedure :: destroy  => destroyperts

   end type interp_perts

   contains

   subroutine initialperts(this,nxorg,nyorg,nxnew,nynew)
!
! initialize the type, mainly set the dimension 
! and allocate array
!
      class(interp_perts), intent(inout) :: this
      integer,intent(in) :: nxorg,nyorg,nxnew,nynew

      this%nxorg=nxorg
      this%nyorg=nyorg
      this%nxnew=nxnew
      this%nynew=nynew

      if(allocated(this%weights)) deallocate(this%weights)
      allocate(this%weights(this%nxnew,this%nynew,4))
      this%weights=0.0

      if(allocated(this%ijloc)) deallocate(this%ijloc)
      allocate(this%ijloc(this%nxnew,this%nynew,2))
      this%ijloc=0

   end subroutine initialperts
!
   subroutine readinterpwrite(this,filein,fileout)
!
!  read one field
!
      class(interp_perts), intent(inout) :: this
      character(len=*), intent(in) :: filein
      character(len=*), intent(in) :: fileout

      integer :: iunit,outunit
      logical :: file_exists 
      integer :: nx,ny,nz
      real,allocatable :: fld3d(:,:,:)
      real,allocatable :: fld3dout(:,:,:)

      integer :: i,j,k,n,nzz
      integer :: ii,jj

      iunit=12
      inquire(file=trim(filein), exist=file_exists)
      if(file_exists) then
         open(iunit,file=trim(filein),form='unformatted',convert='BIG_ENDIAN')
         open(outunit,file=trim(fileout),form='unformatted',convert='BIG_ENDIAN')
            read(iunit) nx,ny,nz
            write(outunit) this%nxnew,this%nynew,nz
! read and add  ENSEMBLE perturbations 
            if(nx==this%nxorg .and. ny==this%nyorg) then
               do n=1,5    ! ps tv u v rh
                  nzz=nz
                  if(n==1) nzz=1
                  allocate(fld3d(ny,nx,nzz))
                  allocate(fld3dout(this%nynew,this%nxnew,nzz))
                  read(iunit) fld3d
!
                  do k=1,nzz
                    do j=1,this%nynew             
                      do i=1,this%nxnew             
                         ii=this%ijloc(i,j,1)
                         jj=this%ijloc(i,j,2)
                         fld3dout(j,i,k)=fld3d(jj,ii,k)*this%weights(i,j,1)+ &
                                         fld3d(jj+1,ii,k)*this%weights(i,j,2)+ &
                                         fld3d(jj,ii+1,k)*this%weights(i,j,3)+ &
                                         fld3d(jj+1,ii+1,k)*this%weights(i,j,4)
!if(k==1 .and. i==1 .and. j==1) then
!   write(*,*) n,fld3dout(j,i,k)
!   write(*,*) this%weights(i,j,1:4)
!   write(*,*) this%ijloc(i,j,1:2)
!   write(*,*) fld3d(jj,ii,k),fld3d(jj+1,ii,k),fld3d(jj,ii+1,k),&
!              fld3d(jj+1,ii+1,k)
!endif
                      enddo
                    enddo
                  enddo
                  do k=1,nzz
                     write(outunit) fld3dout(:,:,k)
!                     write(*,*) 'max/min',k,maxval(fld3dout(:,:,k)),&
!                                  minval(fld3dout(:,:,k))
                  enddo
!
                  deallocate(fld3d)
                  deallocate(fld3dout)
               enddo
            else
               write(*,*) 'readinterpwriteError, dimension mismatch'
               write(*,*) 'dimension read in=',nx,ny,nz
               write(*,*) 'saved dimension=',this%nxorg,this%nyorg
               stop 123
            endif
         close(iunit)
         close(outunit)

      else
         write(*,*) 'readinterpwriteError Error, cannot open file ',trim(filein)
         stop 123
      endif

   end subroutine readinterpwrite

   subroutine gen_weights(this,ii,jj,xc,yc)
!
! gen weight, calculated outside
!
      class(interp_perts), intent(inout) :: this
      integer, intent(in) :: ii,jj
      real,    intent(in) :: xc,yc

      real :: w1,w2

      
     !write(*,*) ii,jj,xc,yc
     if ( (ii >=1 .and. ii <=this%nxnew) .and. &
          (jj >=1 .and. jj <=this%nynew) ) then 
       this%ijloc(ii,jj,1)=min(max(int(xc),1),this%nxorg)
       this%ijloc(ii,jj,2)=min(max(int(yc),1),this%nyorg)

       w1=min(max(xc-this%ijloc(ii,jj,1),0.0),1.0)
       w2=min(max(yc-this%ijloc(ii,jj,2),0.0),1.0)
       this%weights(ii,jj,1)=(1.0-w1)*(1.0-w2)
       this%weights(ii,jj,2)=(1.0-w1)*w2
       this%weights(ii,jj,3)=w1*(1.0-w2)
       this%weights(ii,jj,4)=w1*w2
!   weight    ijloc
!   2 4       i(j+1)  (i+1)(j+1)
!   1 3       ij      (i+1)j
!
       !write(*,*) this%ijloc(ii,jj,1:2)
       !write(*,*) this%weights(ii,jj,1:4)
     else
       write(*,*) 'gen_weights Error, ii,jj out of range ',ii,jj, &
                   this%nxnew,this%nynew
       stop 234
     endif

   end subroutine gen_weights
!
   subroutine destroyperts(this)
! release aray
      class(interp_perts), intent(inout) :: this

      this%nxorg=0
      this%nyorg=0
      this%nxnew=0
      this%nynew=0

      if(allocated(this%weights)) deallocate(this%weights)
      if(allocated(this%ijloc)) deallocate(this%ijloc)

   end subroutine destroyperts

end module module_interp_perturbations
