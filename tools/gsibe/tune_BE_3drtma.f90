PROGRAM tune_be_3drtma
!
!  tune GSI be
!
  implicit none
  integer,parameter :: r_single=4,i_kind=4,r_kind=8
!
  integer(i_kind) :: msig,mlat
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn
!
  integer(i_kind):: inerr=12,istat
  integer(i_kind):: outerr=24
  integer(i_kind):: isig,k,i,j
  character*5 var
!
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn
  real(r_single),dimension(:,:),allocatable::  corqq_avn
!
  real(r_kind) fctr_agv,fctr_bv,fctr_wgv
!
  open(outerr,file='berror_stats_3drtma',form='unformatted',status='new',convert='big_endian')
  open(inerr,file='berror_stats_RAP',form='unformatted',status='old',convert='big_endian')
  
! Read header.
  rewind inerr
  read(inerr) msig,mlat
  write(outerr) msig,mlat
!
! Read background error file to get balance variables
  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  do k=1,msig
     write(*,*) "singma=",k,sigma_avn(k)
  enddo
  read(inerr)agv_avn,bv_avn,wgv_avn
  fctr_agv=1.0_r_kind
  fctr_bv =1.0_r_kind
  fctr_wgv=0.5_r_kind
!  agv_avn=agv_avn*fctr_agv
!  bv_avn=bv_avn*fctr_bv
!  wgv_avn=wgv_avn*fctr_wgv
  write(outerr)clat_avn,(sigma_avn(k),k=1,msig)
  write(outerr)agv_avn,bv_avn,wgv_avn

! Read amplitudes
  read: do
     read(inerr,iostat=istat) var, isig
     if (istat /= 0) exit
     write(outerr,iostat=istat) var, isig
     write(*,*) 'var, isig= ',var, isig
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )

     if (var/='q') then
        read(inerr) corz_avn
!        call tune_corz(isig,mlat,corz_avn,var)
        write(outerr) corz_avn
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        read(inerr) corz_avn,corqq_avn
!        call tune_corz(isig,mlat,corz_avn,var)
!        call tune_corz(isig,mlat,corqq_avn,var)
        write(outerr) corz_avn,corqq_avn
     end if

     read(inerr) hwll_avn
     call tune_hwll(isig,mlat,hwll_avn,var)
     write(outerr) hwll_avn
     if (isig>1) then
        read(inerr) vztdq_avn
        call tune_vz(isig,mlat,vztdq_avn,var)
        write(outerr) vztdq_avn
     end if

     deallocate ( corz_avn )
     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     if (var=='q') deallocate ( corqq_avn )

  enddo read
  close(inerr)
  close(outerr)

END

subroutine tune_vz(msig,mlat,vztdq_avn,var)
!
  implicit none
  integer,parameter :: r_single=4,i_kind=4,r_kind=8
!
  integer(i_kind),intent(in) :: msig,mlat
  real(r_single),dimension(1:msig,0:mlat+1),intent(inout) :: vztdq_avn
  character*5, intent(in) ::  var
!
  real(r_kind),dimension(:),allocatable ::  fctr_t
  real(r_kind),dimension(:),allocatable ::  fctr_q
  real(r_kind),dimension(:),allocatable ::  fctr_sf
  real(r_kind),dimension(:),allocatable ::  fctr_vp
  real(r_kind),dimension(:),allocatable ::  fctr
!
  integer :: k

!
! initial factor
!
  allocate( fctr_t(msig))
  allocate( fctr_q(msig))
  allocate( fctr_sf(msig))
  allocate( fctr_vp(msig))
  allocate( fctr(msig))
  fctr_t =1.0_r_kind
  fctr_q =1.0_r_kind
  fctr_vp=1.0_r_kind
  fctr_sf=1.0_r_kind
  fctr=1.0_r_kind
  if (var=='ps') then
     fctr(1) = 1.0_r_kind*8.0_r_kind
  else
     fctr(1) = 1.0_r_kind*8.0_r_kind
     fctr(2) = 1.0_r_kind*8.0_r_kind
     fctr(3) = 1.0_r_kind*7.0_r_kind
     fctr(4) = 1.0_r_kind*7.0_r_kind
     fctr(5) = 1.0_r_kind*6.0_r_kind
     fctr(6) = 1.0_r_kind*6.0_r_kind
     fctr(7) = 1.0_r_kind*5.0_r_kind
     fctr(8) = 1.0_r_kind*5.0_r_kind
     fctr(9) = 1.0_r_kind*4.0_r_kind
     fctr(10) = 1.0_r_kind*4.0_r_kind
     fctr(11) = 1.0_r_kind*3.0_r_kind
     fctr(12) = 1.0_r_kind*3.0_r_kind
     fctr(13:23) = 1.0_r_kind*2.0_r_kind
     fctr(24:34) = 1.0_r_kind*1.5_r_kind
  endif
!
!  if (var=='sf') ! fctr=fctr_sf
!  if (var=='vp') ! fctr=fctr_vp
!  if (var=='t') then
!  !   fctr=fctr_t
!  endif
!  if (var=='q')  then
!  !  fctr=fctr_q
!  endif
!  if (var=='ps') fctr(1)=1.0_r_kind/8.0_r_kind
  do k=1,msig
     vztdq_avn(k,:)=vztdq_avn(k,:)*fctr(k)
  end do

end subroutine

subroutine tune_hwll(msig,mlat,hwll_avn,var)
!
  implicit none
  integer,parameter :: r_single=4,i_kind=4,r_kind=8
!
  integer(i_kind),intent(in) :: msig,mlat
  real(r_single),dimension(0:mlat+1,1:msig),intent(inout) :: hwll_avn
  character*5, intent(in) ::  var
!
  real(r_kind),dimension(:),allocatable ::  fctr_t
  real(r_kind),dimension(:),allocatable ::  fctr_q
  real(r_kind),dimension(:),allocatable ::  fctr_uv
  real(r_kind),dimension(:),allocatable ::  fctr
!
  integer :: k

  write(*,*) 'msig,mlat=',msig,mlat
!
! initial factor
!
  allocate( fctr_t(msig))
  allocate( fctr_q(msig))
  allocate( fctr_uv(msig))
  allocate( fctr(msig))
  fctr_t =1.0_r_kind
  fctr_q =1.0_r_kind
  fctr_uv=1.0_r_kind
  fctr=1.0_r_kind/8.0_r_kind
  if (var=='ps') then
     fctr(1) = 1.0_r_kind/8.0_r_kind
  else
     fctr(1) = 1.0_r_kind/8.0_r_kind
     fctr(2) = 1.0_r_kind/8.0_r_kind
     fctr(3) = 1.0_r_kind/7.0_r_kind
     fctr(4) = 1.0_r_kind/7.0_r_kind
     fctr(5) = 1.0_r_kind/6.0_r_kind
     fctr(6) = 1.0_r_kind/6.0_r_kind
     fctr(7) = 1.0_r_kind/5.0_r_kind
     fctr(8) = 1.0_r_kind/5.0_r_kind
     fctr(9) = 1.0_r_kind/4.0_r_kind
     fctr(10) = 1.0_r_kind/4.0_r_kind
     fctr(11) = 1.0_r_kind/3.0_r_kind
     fctr(12) = 1.0_r_kind/3.0_r_kind
     fctr(13:23) = 1.0_r_kind/2.0_r_kind
     fctr(24:34) = 1.0_r_kind/1.5_r_kind
  endif
!  if(var=='t') then
!      call get_fctr_1(msig,fctr_t)
!      write(*,*) fctr_t
!  endif
!
!  if (var=='sf') !fctr=fctr_uv
!  if (var=='vp') !fctr=fctr_uv
!  if (var=='t')  !fctr=fctr_t
!  if (var=='q')  !fctr=fctr_q
!  if (var=='ps') fctr=1.0_r_kind/8.0_r_kind
  do k=1,msig
     hwll_avn(:,k)=hwll_avn(:,k)*fctr(k)
  end do

end subroutine

subroutine tune_corz(msig,mlat,corz_avn,var)
!
  implicit none
  integer,parameter :: r_single=4,i_kind=4,r_kind=8
!
  integer(i_kind),intent(in) :: msig,mlat
  real(r_single),dimension(1:mlat,1:msig),intent(inout) :: corz_avn
  character*5, intent(in) ::  var
!
  real(r_kind),dimension(:),allocatable ::  fctr_t
  real(r_kind),dimension(:),allocatable ::  fctr_q
  real(r_kind),dimension(:),allocatable ::  fctr_sf
  real(r_kind),dimension(:),allocatable ::  fctr_vp
  real(r_kind),dimension(:),allocatable ::  fctr
!
  integer :: k

!
! initial factor
!
  allocate( fctr_t(msig))
  allocate( fctr_q(msig))
  allocate( fctr_sf(msig))
  allocate( fctr_vp(msig))
  allocate( fctr(msig))
  fctr_t =1.0_r_kind
  fctr_q =1.0_r_kind
  fctr_vp=0.35_r_kind
  fctr_sf=0.40_r_kind
  fctr=1.0_r_kind
!
  if (var=='sf') fctr=fctr_sf
  if (var=='vp') fctr=fctr_vp
  if (var=='t')  fctr=fctr_t
  if (var=='q')  fctr=fctr_q
  if (var=='ps') fctr=1.0_r_kind
  do k=1,msig
     corz_avn(:,k)=corz_avn(:,k)*fctr(k)
  end do

end subroutine

subroutine get_fctr_1(msig,fctr_t)
!
  implicit none
  integer,parameter :: r_single=4,i_kind=4,r_kind=8
!
  integer(i_kind),intent(in) :: msig
  real(r_kind),dimension(msig),intent(inout) ::  fctr_t

  fctr_t(1)=0.8
  fctr_t(2)=0.8
  fctr_t(3)=0.8
  fctr_t(4)=0.8
  fctr_t(5)=0.8
  fctr_t(6)=0.81
  fctr_t(7)=0.82
  fctr_t(8)=0.84
  fctr_t(9)=0.86
  fctr_t(10)=0.88
  fctr_t(11)=0.90
  fctr_t(12)=0.92
  fctr_t(13)=0.94
  fctr_t(14)=0.96
  fctr_t(15)=0.98
  fctr_t(16)=0.99
  fctr_t(17)=1.0
  fctr_t(18)=1.0
  fctr_t(19)=1.0
  fctr_t(21)=1.02
  fctr_t(22)=1.04
  fctr_t(23)=1.06
  fctr_t(24)=1.08
  fctr_t(25)=1.10
  fctr_t(26)=1.13
  fctr_t(27)=1.16
  fctr_t(28)=1.18
  fctr_t(29)=1.2
  fctr_t(30)=1.2
  fctr_t(31)=1.2
  fctr_t(32)=1.18
  fctr_t(33)=1.14
  fctr_t(34)=1.10
  fctr_t(35)=1.08
  fctr_t(36)=1.06
  fctr_t(37)=1.04
  fctr_t(38)=1.02

end subroutine
