program gen_fcstobs
!
  use kinds, only: r_kind,r_single,len_sta_name
  use module_obs_conv_array, only : obs_conv_array
  use module_ncio , only : ncio 
  use module_map_utils, only : map_util
  use module_fcstobs, only : fcstobs_array
  use module_time, only : mtime
!
  implicit none

  type(obs_conv_array) :: obs120,obs187,obs181,obs133,obs227
  type(ncio)  :: fcst
  type(mtime) :: mt
  type(map_util) :: map
  type(fcstobs_array) :: fcstobs120
  type(fcstobs_array) :: fcstobs187
  type(fcstobs_array) :: fcstobs181
  type(fcstobs_array) :: fcstobs133
  type(fcstobs_array) :: fcstobs227
  
  integer,parameter :: numvar_sfc=6
  character(len=10) :: varname_sfc(numvar_sfc)
  data varname_sfc /'T','U','V','P','H','Q'/
  integer,parameter :: numvar_up=6
  character(len=10) :: varname_up(numvar_up)
  data varname_up /'H','P','T','U','V','Q'/
  integer,parameter :: numvar_upwind=4
  character(len=10) :: varname_upwind(numvar_upwind)
  data varname_upwind /'H','P','U','V'/

!
!  namelist
  integer :: obsdate,obsmin
  character(len=180) :: obsPath,obsfilename
  character(len=180) :: savePath,prefixobssavename
  character(len=180) :: fcstPath,fcstfilename
  character(len=180) :: prefixfcstobssavename
  namelist/setup/ obsdate,obsmin,obsPath,obsfilename,&
                  savePath,prefixobssavename,prefixfcstobssavename,&
                  fcstPath,fcstfilename

  integer :: numobstype
  integer :: obstype(100)
  real(r_single) :: timewindow(100)
  namelist/obsset/ numobstype,obstype,timewindow

  character(len=180) :: obsfile
  character(len=180) :: obssavefile
  character(len=180) :: fcstobssavefile
  character(len=180) :: fcstfile
  character(len=12)  :: timetag
  character(len=32)  :: timetagout
  character(len=19)  :: START_DATE
  character(len=1)   :: FCST_TIMES(19)
  integer :: n
  integer :: vy,vm,vd,vh,vf   ! valid time
  integer :: sy,sm,sd,sh,sf   ! start time
  integer :: smins,vmins,fcsthh,fcstmin
!
  logical :: if_save_4sfcpoints
  real, allocatable :: fld(:,:,:),fldb(:,:,:)
  real, allocatable :: fld2d(:,:),fld2db(:,:)
  integer :: bkends(4)
  real(r_single),allocatable,target :: rlon2d(:,:),rlat2d(:,:)
  real(r_single) :: rlon,rlat
  integer :: nx,ny,nz
  integer :: i,k
!
! 
! 
  START_DATE='0000'
  obsdate=2017010103
  obsmin=0
  obsPath='../'
  obsfilename='prepbufr'
  savePath='./'
  prefixobssavename='prepbufr'
  prefixfcstobssavename='prepbufr'
  fcstPath='./'
  fcstfilename='wrfout_d01'
  numobstype=1
  obstype=120
  timewindow=1.0

  open(15,file='namelist.input')
      read(15,setup)
      read(15,obsset)
  close(15)
  write(timetag,'(I10,I2.2)') obsdate,obsmin
!
  obssavefile=trim(savePath)//"/"//trim(prefixobssavename)
  fcstobssavefile=trim(savePath)//"/"//trim(prefixfcstobssavename)
!
  do n=1,numobstype
     write(obsfile,'(a,a,I4.4,3a)') trim(obssavefile),'_arrayXY_type',obstype(n),&
                          '_',timetag,'.bin'
     write(*,*)
     write(*,*) '================================='
     write(*,'(a,a)') 'read obs file =',trim(obsfile)

     if(obstype(n)==120) then
        call obs120%readaxy(trim(obsfile))
!  call obs120%listpstn()
     elseif(obstype(n)==187) then
        call obs187%readaxy(trim(obsfile))
!  call obs187%listpstn()
     elseif(obstype(n)==181) then
        call obs181%readaxy(trim(obsfile))
!  call obs181%listpstn()
     elseif(obstype(n)==133) then
        call obs133%readaxy(trim(obsfile))
!  call obs133%listpstn()
     elseif(obstype(n)==227) then
        call obs227%readaxy(trim(obsfile))
!  call obs227%listpstn()
      endif
  enddo

! find dimension of forecast file 
  fcstfile=trim(fcstPath)//"/"//trim(fcstfilename)
  call fcst%open(trim(fcstfile),"r",200)
  call fcst%get_dim("west_east",nx)
  call fcst%get_dim("south_north",ny)
  call fcst%get_dim("bottom_top",nz)
  write(*,*) 'nx,ny,nz=',nx,ny,nz
  call fcst%get_att("START_DATE",START_DATE)
  call fcst%get_var("Times",19,FCST_TIMES)
  write(*,*) 'model start time=',START_DATE
  write(*,*) 'model valid time=',FCST_TIMES
  read(START_DATE,'(I4,1x,I2,1x,I2,1x,I2,1x,I2)') sy,sm,sd,sh,sf
  write(*,*) 'model start time=',sy,sm,sd,sh,sf
  write(START_DATE,'(19a1)') FCST_TIMES(1:19)
  read(START_DATE,'(I4,1x,I2,1x,I2,1x,I2,1x,I2)') vy,vm,vd,vh,vf
  write(*,*) 'model valid time=',vy,vm,vd,vh,vf
  smins=mt%date2mins(sy,sm,sd,sh,sf)
  vmins=mt%date2mins(vy,vm,vd,vh,vf)
  fcstmin=vmins-smins
  fcsthh=fcstmin/60
  fcstmin=fcstmin-fcsthh*60
  write(*,*) 'foreast hour and minutes=',fcsthh,fcstmin
  write(timetagout,'(a1,I4,4I2.2,a1,I3.3,I2.2,a1,I4,4I2.2)') "s",sy,sm,sd,sh,sf,&
                              "f",fcsthh,fcstmin,"v",vy,vm,vd,vh,vf
  write(*,*) 'Time tag for foreast obs=',timetagout
!
! initial forecast obs type
  call fcstobs120%initial(obs120,nz)
!  call obs120%destroy()
  if_save_4sfcpoints=.true.
  call fcstobs187%initial(obs187,nz,if_save_4sfcpoints)
!  call obs187%destroy()
  call fcstobs181%initial(obs181,nz)
!  call obs181%destroy()
  call fcstobs133%initial(obs133,nz)
!  call obs133%destroy()
  call fcstobs227%initial(obs227,nz)
!  call obs227%destroy()
!
!
  allocate(rlon2d(nx,ny))
  allocate(rlat2d(nx,ny))
!
! initialize map transfer parameters
!
  call fcst%get_var("XLONG",nx,ny,rlon2d)
  call fcst%get_var("XLAT",nx,ny,rlat2d)
  call map%init_general_transform(nx,ny,rlat2d,rlon2d)
!
! get surface pressure
!
  allocate(fld2d(nx,ny))
  allocate(fld2db(nx,ny))
  call fcst%get_var("MUB",nx,ny,fld2db)
  call fcst%get_var("MU",nx,ny,fld2d)
  fld2d=(fld2d+fld2db)/100.0 !pa to hPa
  write(*,*) 'surface pressure=',maxval(fld2d),minval(fld2d)
  deallocate(fld2db)
!
! get pressure and do interpolation
  allocate(fld(nx,ny,nz))
  allocate(fldb(nx,ny,nz))
  call fcst%get_var("PB",nx,ny,nz,fldb)
  call fcst%get_var("P",nx,ny,nz,fld)
  fld=(fld+fldb)/100.0 !pa to hPa
  call fcstobs120%interp(fld,nx,ny,nz,'P')
  call fcstobs187%interp(fld,nx,ny,nz,'P')
  call fcstobs181%interp(fld,nx,ny,nz,'P')
  call fcstobs133%interp(fld,nx,ny,nz,'P')
  call fcstobs227%interp(fld,nx,ny,nz,'P')

! get height and do interpolation
  deallocate(fld,fldb)
  allocate(fld(nx,ny,nz+1))
  allocate(fldb(nx,ny,nz+1))
  call fcst%get_var("PHB",nx,ny,nz+1,fldb)
  call fcst%get_var("PH",nx,ny,nz+1,fld)
  fldb=(fld+fldb)/9.8   ! potential height to m 
  deallocate(fld)
  allocate(fld(nx,ny,nz))
  do k=1,nz
     fld(:,:,k)=(fldb(:,:,k)+fldb(:,:,k+1))*0.5_r_single
  enddo

  call fcstobs120%interp(fld,nx,ny,nz,'H')
  call fcstobs187%interp(fld,nx,ny,nz,'H')
  call fcstobs181%interp(fld,nx,ny,nz,'H')
  call fcstobs133%interp(fld,nx,ny,nz,'H')
  call fcstobs227%interp(fld,nx,ny,nz,'H')

  deallocate(fldb)
  allocate(fldb(nx+1,ny,nz))
! get U and V and do interpolation
  call fcst%get_var("U",nx+1,ny,nz,fldb)
  do i=1,nx
     fld(i,:,:)=(fldb(i,:,:)+fldb(i+1,:,:))*0.5_r_single
  enddo
  call fcstobs120%interp(fld,nx,ny,nz,'U')
  call fcstobs187%interp(fld,nx,ny,nz,'U')
  call fcstobs181%interp(fld,nx,ny,nz,'U')
  call fcstobs227%interp(fld,nx,ny,nz,'U')
  call fcstobs133%interp(fld,nx,ny,nz,'U')

  deallocate(fldb)
  allocate(fldb(nx,ny+1,nz))
  call fcst%get_var("V",nx,ny+1,nz,fldb)
  do i=1,ny
     fld(:,i,:)=(fldb(:,i,:)+fldb(:,i+1,:))*0.5_r_single
  enddo
  call fcstobs120%interp(fld,nx,ny,nz,'V')
  call fcstobs187%interp(fld,nx,ny,nz,'V')
  call fcstobs181%interp(fld,nx,ny,nz,'V')
  call fcstobs133%interp(fld,nx,ny,nz,'V')
  call fcstobs227%interp(fld,nx,ny,nz,'V')
  call fcstobs187%listall(10)
  call fcstobs181%listall(10)
  call fcstobs133%listall(10)
  call fcstobs227%listall(10)

  deallocate(fldb)
! get mositure and do interpolation
  call fcst%get_var("QVAPOR",nx,ny,nz,fld)
  call fcstobs120%interp(fld,nx,ny,nz,'Q')
  call fcstobs133%interp(fld,nx,ny,nz,'Q')
  call fcst%get_var("Q2",nx,ny,fld(:,:,1))
  call fcstobs187%interp(fld(:,:,1),nx,ny,1,'Q2')
  call fcstobs181%interp(fld(:,:,1),nx,ny,1,'Q2')

! get temperature and do interpolation
  call fcst%get_var("T",nx,ny,nz,fld)
  fld=fld+300.0
  call fcstobs120%interp(fld,nx,ny,nz,'T')
!  call fcstobs187%interp(fld,nx,ny,nz,'T')
!  call fcstobs181%interp(fld,nx,ny,nz,'T')
  call fcstobs133%interp(fld,nx,ny,nz,'T')
  call fcst%get_var("TH2",nx,ny,fld(:,:,1))
  call fcst%convert_theta2t_2dgrid(nx,ny,fld2d,fld(:,:,1))
  call fcstobs187%interp(fld(:,:,1),nx,ny,1,'T2')
  call fcstobs181%interp(fld(:,:,1),nx,ny,1,'T2')
!
  deallocate(fld2d)
  deallocate(fld)
!
! convert potentional temperature to sensible T
  call fcstobs120%convert_theta2t()
!  call fcstobs187%convert_theta2t()
!  call fcstobs181%convert_theta2t()
  call fcstobs133%convert_theta2t()

! rotate wind to earth wind
  call fcstobs120%rotate_uv(map)
  call fcstobs187%rotate_uv(map)
  call fcstobs181%rotate_uv(map)
  call fcstobs133%rotate_uv(map)
  call fcstobs227%rotate_uv(map)
  call fcstobs120%listall(1)
  call fcstobs187%listall(10)
  call fcstobs181%listall(10)
  call fcstobs133%listall(10)
  call fcstobs227%listall(2)
!
!
  do n=1,numobstype
     write(fcstfile,'(a,a,I4.4,3a)') trim(fcstobssavefile),'_fcstobs_type',obstype(n),&
                          '_',timetagout,'.bin'
     write(*,*)
     write(*,*) '================================='
     write(*,'(a,a)') 'read fcst obs file =',trim(fcstfile)

     if(obstype(n)==120) then
        call fcstobs120%writeall(trim(fcstfile))
        call fcstobs120%destroy()
        call fcstobs120%readall(trim(fcstfile))
        call fcstobs120%listall(3)
        call obs120%lista(3)
     elseif(obstype(n)==187) then
        call fcstobs187%writeall(trim(fcstfile))
        call fcstobs187%destroy()
        call fcstobs187%readall(trim(fcstfile))
        call fcstobs187%listall(10)
        call obs187%lista(3)
     elseif(obstype(n)==181) then
        call fcstobs181%writeall(trim(fcstfile))
        call fcstobs181%destroy()
        call fcstobs181%readall(trim(fcstfile))
        call fcstobs181%listall(3)
        call obs181%lista(3)
     elseif(obstype(n)==133) then
        call fcstobs133%writeall(trim(fcstfile))
        call fcstobs133%destroy()
        call fcstobs133%readall(trim(fcstfile))
        call fcstobs133%listall(3)
        call obs133%lista(3)
     elseif(obstype(n)==227) then
        call fcstobs227%writeall(trim(fcstfile))
        call fcstobs227%destroy()
        call fcstobs227%readall(trim(fcstfile))
        call fcstobs227%listall(3)
        call obs227%lista(3)
      endif
  enddo
!
end program gen_fcstobs
