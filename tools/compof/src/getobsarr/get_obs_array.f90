program gen_fcstobs
!
  use kinds, only: r_kind,r_single,len_sta_name
  use module_obs_conv_array, only : obs_conv_array
  use module_ncio, only: ncio
  use module_map_utils, only : map_util
!
  implicit none

  type(obs_conv_array) :: obsall,obs_snd,obs_sfc,obs_prf
  type(ncio) :: bk
  type(map_util) :: map
!
!
  real(r_single),allocatable :: rlon2d(:,:),rlat2d(:,:)
  real(r_single) :: rlon,rlat
  real(r_single) :: xc,yc
  real(r_single) :: u,v,u0,v0
  integer :: nx,ny,nz

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
   character(len=180) :: savefile
   character(len=180) :: bkfile
   character(len=12)  :: timetag
   integer :: n
!
!
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

!   write(*,setup)
!   write(*,obsset)


  bkfile=trim(fcstPath)//"/"//trim(fcstfilename)
  write(*,'(a,a)')  'read bk=',trim(bkfile)
  call bk%open(trim(bkfile),"r",200)
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

  savefile=trim(savePath)//"/"//trim(prefixobssavename)
  write(*,*) 'save obs to file=',trim(savefile)
!
  write(timetag,'(I10,I2.2)') obsdate,obsmin
!
  do n=1,numobstype
     write(obsfile,'(a,a,I4.4,3a)') trim(savefile),'_type',obstype(n),&
                          '_',timetag,'.bin'
     write(*,*)
     write(*,*) '================================='
     write(*,'(a,a)') 'process obs file =',trim(obsfile)

     call obsall%readapt(trim(obsfile))
!     call obsall%lista(2)
!     write(obsfile,'(a,a,I4.4,3a)') trim(savefile),'_array_type',obstype(n),&
!                          '_',timetag,'.bin'
!     call obsall%writea(trim(obsfile))
     call obsall%calculatexy(map)
     write(obsfile,'(a,a,I4.4,3a)') trim(savefile),'_arrayXY_type',obstype(n),&
                          '_',timetag,'.bin'
     call obsall%writeaxy(trim(obsfile))
     call obsall%lista(10)
     call obsall%destroy()
!     call obsall%readaxy(trim(obsfile))
!     call obsall%lista(2)
!     call obsall%destroy()
  enddo
!
end program gen_fcstobs
