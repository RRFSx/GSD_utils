program  process_tamdar_netcdf
!
!   PRGMMR: Ming Hu               ORG: GSD        DATE: 2015-11-16
!
! ABSTRACT: 
!     This routine reads in Tamdar observations in netcdf
!     and write them to a bufr file. 
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind,r_single

  implicit none

  INCLUDE 'netcdf.inc'

  character*256 output_file

!! Declare namelists 
!
! SETUP (general control namelist) :
!
  character*10 :: analysis_time
  real         :: time_window
  integer      :: TAMDAR_filenum
  namelist/setup/analysis_time, time_window,TAMDAR_filenum
!
!

!  For NASA LaRC 
  CHARACTER*180,allocatable ::   aircraftfile(:)
  INTEGER ::   recNum  ! dimension
  
!     ****VARIABLES FOR THIS NETCDF FILE****
  character(9),   allocatable :: formatACARS(:)
  character(9),   allocatable :: tailNumber(:)
  real(r_single), allocatable :: latitude(:)
  real(r_single), allocatable :: longitude(:)
  real(r_single), allocatable :: T(:)
  real(r_single), allocatable :: prs(:)
  real(r_single), allocatable :: wd(:)
  real(r_single), allocatable :: spd(:)
  real(r_single), allocatable :: rawRH(:)
  real(r_single), allocatable :: wvmr(:)
  real(r_single), allocatable :: alt(:)
  real(r_kind),   allocatable :: time(:)
  real(r_single), allocatable :: rhUnct(:)
  character(1),   allocatable :: wvmrqc(:)
  integer(i_kind),allocatable :: TQCin(:)
  integer(i_kind),allocatable :: WDQCin(:)
  integer(i_kind),allocatable :: WSQCin(:)

!  ** misc
  integer i,j
  Integer nf_status,nf_fid,nf_vid

  integer :: NCID
  integer :: ifile
  LOGICAL :: first
  LOGICAL :: file_exists
  integer :: iyear,month,iday,ihh
  integer :: iiday,iisec,IW3JDN
  real    :: timedhr
  integer :: numtotal,numtotalTamdar,numt,numwind,numq
  real    :: altitude, sigma, delta, theta
  real    :: rad,uu,vv,qsat,qmix
  real    :: ruc_saturation

  integer :: status,mype

!**********************************************************************
!
!            END OF DECLARATIONS....start of program

  open(11,file='tamdar.namelist')
   read(11,setup)
  close(11)
  write(*,*) analysis_time, time_window,TAMDAR_filenum

  read(analysis_time(1:4),'(i4)') iyear
  read(analysis_time(5:6),'(i2)') month
  read(analysis_time(7:8),'(i2)') iday
  read(analysis_time(9:10),'(i2)') ihh

  write(*,*) iyear,month,iday,ihh

  IIday = IW3JDN(IYEAR,MONTH,IDAY) - IW3JDN(1970,1,1)
  IIsec=IIday * 3600*24 + ihh * 3600

  if(TAMDAR_filenum>0) then
     allocate(aircraftfile(TAMDAR_filenum))
     open(12,file='tamdar_filelist')
        do i=1,TAMDAR_filenum
          read(12,'(a)') aircraftfile(i)
        enddo
     close(12)
  endif
!
  numtotal=0
  numtotalTamdar=0
  numt=0
  numwind=0
  numq=0
  rad = 4.0*atan(1.0)/180.

  open(20, file='tamdarobs.bin',form='unformatted')
!
  DO ifile=1,TAMDAR_filenum
     
     write(*,'(a,a)') 'process file ',trim(aircraftfile(ifile))
     ! if a file is missing skip it
     INQUIRE(FILE=trim(aircraftfile(ifile)),EXIST=file_exists)
     if (file_exists == .false.) then
        write(*,'(a,a)') 'file does not exist ',trim(aircraftfile(ifile))
        CYCLE
     endif

     ! get the dimensions for this file
     call read_tamdar_netcdf_dim(aircraftfile(ifile),recNum)
     write(6,*)'Aircraft data number  =', ifile, recNum
     numtotal=numtotal+recNum
     allocate(formatACARS(recNum))
     allocate(tailNumber(recNum))
     allocate(t(recNum),rawrh(recNum),prs(recNum),wvmr(recNum))
     allocate(wd(recNum),spd(recNum))
     allocate(latitude(recNum),longitude(recNum))
     allocate(time(recNum),alt(recNum))
     allocate(rhUnct(recNum),TQCin(recNum),WDQCin(recNum),WSQCin(recNum))
     allocate(wvmrqc(recNum))
     wvmr=0.0

! read in data
     call read_tamdar_netcdf(aircraftfile(ifile),recNum,formatACARS,latitude,longitude,&
                             t,prs,wd,spd,rawrh,time,alt,tailNumber,rhUnct,TQCin,WDQCin,WSQCin)

     do i=1,recNum
        if(formatACARS(i)(1:6)=='TAMDAR') then
           numtotalTamdar=numtotalTamdar+1
           timedhr=(time(i)-iisec)/3600.0
           call Atmosphere(prs(i)/1000.0, sigma, delta, theta)
           prs(i)=delta
           uu= -spd(i)*sin(rad*wd(i))
           vv= -spd(i)*cos(rad*wd(i)) 
           qsat=ruc_saturation(t(i),prs(i))
           qmix=qsat*rawrh(i)*0.01

!           write(*,'(I5,1x,a9,5f10.2,1x,a9,2f10.2)') i,formatACARS(i),alt(i),t(i),wd(i),spd(i),rawrh(i),& 
!                                                tailNumber(i),prs(i),wvmr(i)*1000.0
!           write(*,'(I5,1x,a9,5f10.2,1x,a9,2f10.2)') i,formatACARS(i),alt(i),t(i),uu,vv,qmix*1000.0
!           write(*,'(I5,f10.2,5x,3I10,f10.2)') i,timedhr,TQCin(i),WDQCin(i),WSQCin(i),rhUnct(i)

           write(20) i,formatACARS(i),latitude(i),longitude(i),alt(i),t(i),wd(i),spd(i),rawrh(i),&
                       tailNumber(i),prs(i),uu,vv,qmix*1000.0,&
                       timedhr,TQCin(i),WDQCin(i),WSQCin(i),rhUnct(i)
        endif
     enddo

!     ! write out results
!     call write_NASALaRC_global_cloud(nxp,nyp,lat,lon,ptop,htop,teff,phase,lwp,cbase_time,time_offset,first)
!     ! after one file is written, append data
!     first = .false.

     deallocate(rawrh,t,prs)
     deallocate(wd,spd)
     deallocate(latitude,longitude)
     deallocate(formatACARS,tailNumber)
     deallocate(time,alt)
     deallocate(rhUnct,TQCin,WDQCin,WSQCin)
     deallocate(wvmrqc,wvmr)

  ENDDO  ! ifile 
  close(20)
!
  write(*,*)
  write(*,*) 'total Aircraft observations =',numtotal
  write(*,*) 'total TAMDAR observations =',numtotalTamdar
!
  call append_tamdar(numtotalTamdar,iyear,month,iday,ihh,time_window)
  
  write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="
!
end program process_tamdar_netcdf

