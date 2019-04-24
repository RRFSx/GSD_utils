program test
  use module_mp_thompson
  IMPLICIT NONE
  REAL, DIMENSION(:,:,:),allocatable:: qv, qc, qr, qs, qg,nr,t,p, refl
  integer::nx,ny,nz
  nx=1800
  ny=1600
  nz=51
  allocate(qv(nx,ny,nz),qc(nx,ny,nz),qr(nx,ny,nz))
  allocate(qs(nx,ny,nz),qg(nx,ny,nz),nr(nx,ny,nz))
  allocate(t(nx,ny,nz),p(nx,ny,nz),refl(nx,ny,nz))

  !!fill the 3D arrays here

  call refl_thompson(nx,ny,nz,qv,qc,qr,nr,qs,qg,t,p,refl)
end program test
