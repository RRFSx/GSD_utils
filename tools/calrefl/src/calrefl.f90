Program calrefl
!
!  read in hydrometers and temperature, moisture to 
!  calculate the reflectivity and update background
!  reflectivity
!
  use mpi
  use module_ncio, only: ncio
  use module_mp_thompson

  implicit none
!
!
  integer, parameter :: r_single=4
  real, parameter    :: cpr=1.0_r_single/0.286_r_single
!
!  MPI variables
  integer :: npe, mype, mypeLocal,ierror
!
  type(ncio)    :: bkio
  integer       :: nx,ny,nz
!
  character(len=180) :: bkfile
!
  real(r_single),allocatable :: t3d(:,:,:)
  real(r_single),allocatable :: p3d(:,:,:)
  real(r_single),allocatable :: refl(:,:,:)
  real(r_single),allocatable :: qv(:,:,:)
  real(r_single),allocatable :: qc(:,:,:)
  real(r_single),allocatable :: qr(:,:,:)
  real(r_single),allocatable :: qs(:,:,:)
  real(r_single),allocatable :: qg(:,:,:)
  real(r_single),allocatable :: qnr(:,:,:)

!
! misc
  integer :: i,j,k
!
!
!  MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)
!
  if(mype==0) then

     bkfile='wrf_inout'
     call bkio%open(trim(bkfile),'w',200)
     call bkio%get_dim("west_east",nx)
     call bkio%get_dim("south_north",ny)
     call bkio%get_dim("bottom_top",nz)
     write(*,*) 'nx,ny,nz=',nx,ny,nz

     allocate(t3d(nx,ny,nz))
     allocate(p3d(nx,ny,nz))
     call bkio%get_var("PB",nx,ny,nz,t3d)
     call bkio%get_var("P",nx,ny,nz,p3d)
     p3d=p3d+t3d
     p3d=p3d/100.0_r_single
     call bkio%get_var("T",nx,ny,nz,t3d)
     t3d=t3d+300._r_single
     do k=1,nz
        do j=1,ny
          do i=1,nx
             t3d(i,j,k) = t3d(i,j,k)*(p3d(i,j,k)/1000.0_r_single)
!             t3d(i,j,k) = t3d(i,j,k)*(p3d(i,j,k)/1000.0_r_single) - 273.15_r_single
          enddo
        enddo
        write(*,*) 't ', k,maxval(t3d(:,:,k)),minval(t3d(:,:,k))
        write(*,*) 'p ', k,maxval(p3d(:,:,k)),minval(p3d(:,:,k))
     enddo
     p3d=p3d*100.0_r_single

     allocate(qv(nx,ny,nz))
     allocate(qc(nx,ny,nz))
     allocate(qr(nx,ny,nz))
     allocate(qs(nx,ny,nz))
     allocate(qg(nx,ny,nz))
     allocate(qnr(nx,ny,nz))
     call bkio%get_var("QVAPOR",nx,ny,nz,qv)
     call bkio%get_var("QCLOUD",nx,ny,nz,qc)
     call bkio%get_var("QRAIN",nx,ny,nz,qr)
     call bkio%get_var("QSNOW",nx,ny,nz,qs)
     call bkio%get_var("QGRAUP",nx,ny,nz,qg)
     call bkio%get_var("QNRAIN",nx,ny,nz,qnr)

     allocate(refl(nx,ny,nz))
     call refl_thompson(nx,ny,nz,qv,qc,qr,qnr,qs,qg,t3d,p3d,refl)

     do k=1,nz
        write(*,*) 'reflectivty max,min',maxval(refl(:,:,k)),minval(refl(:,:,k))
     enddo

     deallocate(t3d)
     deallocate(p3d)
     deallocate(qv)
     deallocate(qc)
     deallocate(qr)
     deallocate(qs)
     deallocate(qg)
     deallocate(qnr)

     call bkio%replace_var("QNICE",nx,ny,nz,refl) 
     deallocate(refl)

     call bkio%close()
  endif
!
  call MPI_FINALIZE(ierror)
!

end program
