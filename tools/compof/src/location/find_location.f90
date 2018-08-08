program get_location
!
  use kinds, only: r_kind,r_single,len_sta_name
  use module_obs_location, only : obs_array
  use module_ncio, only: ncio
  use module_map_utils, only : map_util
!
  implicit none

  type(obs_array) :: obs_snd
  type(ncio) :: bk
  type(map_util) :: map
!
!
  real(r_single),allocatable :: rlon2d(:,:),rlat2d(:,:)
  real(r_single) :: rlon,rlat
  real(r_single) :: xc,yc
  integer :: nx,ny,nz

!
!  namelist

   character(len=180) :: obsfile
   character(len=180) :: savefile
   character(len=180) :: bkfile
!
!
  bkfile="/mnt/lfs1/projects/nrtrr/HRRR_AK/run/surface/wrfout_sfc_00"
  write(*,'(a,a)')  'read bk=',trim(bkfile)
  call bk%open(trim(bkfile),'r',200)
  call bk%get_dim("west_east",nx)
  call bk%get_dim("south_north",ny)
  write(*,*) 'nx,ny,nz=',nx,ny

  allocate(rlon2d(nx,ny))
  allocate(rlat2d(nx,ny))
  call bk%get_var("XLONG",nx,ny,rlon2d)
  call bk%get_var("XLAT",nx,ny,rlat2d)
!
  call map%init_general_transform(nx,ny,rlat2d,rlon2d)
  call bk%close()
!

  savefile="test_location.bin"
  write(*,*) 'save obs to file=',trim(savefile)
!
     call obs_snd%initial(4)
     obs_snd%obsarxy(1)%name="PANC"
     obs_snd%obsarxy(1)%lon=-150.0278
     obs_snd%obsarxy(1)%lat=61.1690
     obs_snd%obsarxy(2)%name="PAFA"
     obs_snd%obsarxy(2)%lon=-147.876
     obs_snd%obsarxy(2)%lat=64.8039
     obs_snd%obsarxy(3)%name="A#3"
     obs_snd%obsarxy(3)%lon=-140.00
     obs_snd%obsarxy(3)%lat=75.00
     obs_snd%obsarxy(4)%name="A#2"
     obs_snd%obsarxy(4)%lon=-156.60 
     obs_snd%obsarxy(4)%lat=74.30
     call obs_snd%list(10)

     call obs_snd%calculatexy(map)
     call obs_snd%list(10)
     call obs_snd%write(savefile)
     call obs_snd%destroy()
     call obs_snd%read(savefile)
     call obs_snd%list(10)
!
end program get_location
