subroutine read_tamdar_netcdf(infile,recNum,formatACARS,lat,lon,t,prs,wd,spd,rawrh,&
                              time,alt,tailNumber,rhUnct,TQCin,WDQCin,WSQCin)
!
!   PRGMMR: Terra Ladwig          ORG: GSD        DATE: 2015-04-20
!
! ABSTRACT: 
!     This code is based on read_NASALaRC_cloud 
!     This routine reads in NASA LaRC cloud products (netcdf)
!
! PROGRAM HISTORY LOG:
!
!
!   input argument list:
!         infile  - file to read
!         recNum  - dimensions of data  
!
!   output argument list:
!         lat - latitude of ob
!         lon - longitude of ob
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
!
  INCLUDE 'netcdf.inc'
!
!  For NASA LaRC 
!
  character*180   infile

  integer,        intent(in)  :: recNum  ! dimension
  character(9),   intent(out) :: formatACARS(recNum)
  character(9),   intent(out) :: tailNumber(recNum)
  real(r_single), intent(out) :: lat(recNum)
  real(r_single), intent(out) :: lon(recNum)
  real(r_single), intent(out) :: t(recNum)
  real(r_single), intent(out) :: prs(recNum)
  real(r_single), intent(out) :: wd(recNum)
  real(r_single), intent(out) :: spd(recNum)
  real(r_single), intent(out) :: rawrh(recNum)
  real(r_single), intent(out) :: alt(recNum)
  real(r_kind),   intent(out) :: time(recNum)
  real(r_single), intent(out) :: rhUnct(recNum)
  integer(i_kind),intent(out) :: TQCin(recNum)
  integer(i_kind),intent(out) :: WDQCin(recNum)
  integer(i_kind),intent(out) :: WSQCin(recNum)
   
  real(r_single)  :: wvmr(recNum)
  character(1)    :: wvmrqc(recNum)
  
  INTEGER     FAILURE_P 
  PARAMETER ( FAILURE_P  =  1       )
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
!
!  ** misc
      
  integer i,j,k
  Integer nf_status,nf_fid,nf_vid

  integer :: NCID

  integer :: status

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! set geogrid fle name
!
  status = failure_p
  write(6,*) 'read in aircraft data from: ',trim(infile)
  nf_status = NF_OPEN(trim(infile), NF_NOWRITE,nf_fid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_OPEN'
        status=failure_p
        return
  endif
! -------------------------------------------------
!    statements to fill base_time
!
!  nf_status = NF_INQ_VARID(nf_fid,'base_time',nf_vid)
!  if(nf_status.ne.NF_NOERR) then
!        write(6,*)  NF_STRERROR(nf_status)
!        write(6,*) 'in var base_time'
!  endif
!  nf_status = NF_GET_VAR_INT(nf_fid,nf_vid,base_time)
!  if(nf_status.ne.NF_NOERR) then
!        write(6,*)  NF_STRERROR(nf_status)
!        write(6,*) 'in NF_GET_VAR_ base_time '
!  end if
! -------------------------------------------------
!    statements to fill formatACARS 
!
  nf_status = NF_INQ_VARID(nf_fid,'formatACARS',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var formatACARS'
  endif
  nf_status = NF_GET_VAR_TEXT(nf_fid,nf_vid,formatACARS)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ formatACARS'
  end if
!
! -------------------------------------------------
!    statements to fill tailNumber
!
  nf_status = NF_INQ_VARID(nf_fid,'tailNumber',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var tailNumber'
  endif
  nf_status = NF_GET_VAR_TEXT(nf_fid,nf_vid,tailNumber)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ tailNumber'
  end if
!
! -------------------------------------------------
!    statements to fill RH
!
  nf_status = NF_INQ_VARID(nf_fid,'rawRelativeHumidity',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var RH'
  endif
  nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,rawrh)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ RH '
  end if
!  print*, 'lat900',lat(900,350)
! -------------------------------------------------
!    statements to fill RH
!
  nf_status = NF_INQ_VARID(nf_fid,'waterVaporMR',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var waterVaporMR'
  endif
  nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,wvmr)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ waterVaporMR '
  end if

  wvmrqc="0"
! -------------------------------------------------
!    statements to fill lat
!
  nf_status = NF_INQ_VARID(nf_fid,'latitude',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var lat'
  endif
  nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lat)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ Lat '
  end if
! -------------------------------------------------
!    statements to fill lon
!
   nf_status = NF_INQ_VARID(nf_fid,'longitude',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var lon'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lon)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ Lon '
   end if
! -------------------------------------------------
!    statements to fill t 
   nf_status = NF_INQ_VARID(nf_fid,'temperature',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var temperature'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,t)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ Temperature '
   end if

! -------------------------------------------------
!    statements to fill pressure (this altitude is
!  calculated based on the standard atmosphere)
   nf_status = NF_INQ_VARID(nf_fid,'altitude',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var pressure'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,prs)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ pressure'
   end if
!
! -------------------------------------------------
!    statements to fill wind direction 
   nf_status = NF_INQ_VARID(nf_fid,'windDir',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var windDir'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,wd)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ wind direction'
   end if
!
! -------------------------------------------------
!    statements to fill speed 
   nf_status = NF_INQ_VARID(nf_fid,'windSpeed',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var windSpeed'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,spd)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ windSpeed'
   end if
!
! -------------------------------------------------
!    statements to fill time 
   nf_status = NF_INQ_VARID(nf_fid,'timeObs',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var timeAcarsHdr'
   endif
   nf_status = NF_GET_VAR_DOUBLE(nf_fid,nf_vid,time)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ Time'
   end if
!
! -------------------------------------------------
!    statements to fill GPSaltitude
   nf_status = NF_INQ_VARID(nf_fid,'GPSaltitude',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var GPSaltitude'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,alt)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ GPSaltitude'
   end if
!
! -------------------------------------------------
!    statements to fill rhUncertainty
   nf_status = NF_INQ_VARID(nf_fid,'rhUncertainty',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var rhUncertainty'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,rhUnct)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ rhUncertainty'
   end if
!
! -------------------------------------------------
!    statements to fill temperatureQCIn
   nf_status = NF_INQ_VARID(nf_fid,'temperatureQCIn',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var temperatureQCIn'
   endif
   nf_status = NF_GET_VAR_INT(nf_fid,nf_vid,TQCin)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ temperatureQCIn'
   end if
!
! -------------------------------------------------
!    statements to fill windDirQCIn 
   nf_status = NF_INQ_VARID(nf_fid,'windDirQCIn',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var windDirQCIn'
   endif
   nf_status = NF_GET_VAR_INT(nf_fid,nf_vid,WDQCin)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ windDirQCIn'
   end if
!
! -------------------------------------------------
!    statements to fill windSpdQCIn
   nf_status = NF_INQ_VARID(nf_fid,'windSpdQCIn',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var windSpdQCIn'
   endif
   nf_status = NF_GET_VAR_INT(nf_fid,nf_vid,WSQCin)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ windSpdQCIn'
   end if
!
! -------------------------------------------------
  nf_status = NF_CLOSE(nf_fid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_CLOSE '
        status=failure_p
        return
  endif  

!
end subroutine read_tamdar_netcdf


subroutine read_tamdar_netcdf_dim(infile,nyp)
!
!   PRGMMR: Terra Ladwig      ORG: GSD        DATE: 2015-03-19
!
! ABSTRACT: 
!     This routine reads in NASA LaRC global cloud dimensions  
!
! PROGRAM HISTORY LOG:
!
!   input argument list:
!         infile - file to read
!
!   output argument list:
!         nxp - x dimension
!         nyp - y dimension 
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind
!  use netcdf

  implicit none
!
  INCLUDE 'netcdf.inc'
!
!  For NASA LaRC 
!
  CHARACTER*180   infile

  INTEGER ::   nf_status,nf_fid
  INTEGER ::   dimid_y, dimid_x
  INTEGER ::   nxp, nyp  ! dimension
  INTEGER     FAILURE_P
  PARAMETER ( FAILURE_P  =  1       )
  INTEGER ::   status

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!

  ! open file
  status = failure_p
  write(6,*) 'read in aircraft data dim from: ',trim(infile)
  nf_status = NF_OPEN(trim(infile), NF_NOWRITE,nf_fid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_OPEN '
        status=failure_p
        return
  endif  
  !nf_status = NF90_OPEN(trim(infile), NF90_NOWRITE,nf_fid)

  ! get dimension id 
  nf_status = NF_INQ_DIMID(nf_fid, 'recNum', dimid_y)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_INQ_DIMID'
        status=failure_p
        return
  endif

  ! get dimension length
  nf_status = NF_INQ_DIMLEN(nf_fid, dimid_y, nyp)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_INQ_DIMLEN'
        status=failure_p
        return
  endif

  nf_status = NF_CLOSE(nf_fid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_CLOSE '
        status=failure_p
        return
  endif  
!
end subroutine read_tamdar_netcdf_dim
