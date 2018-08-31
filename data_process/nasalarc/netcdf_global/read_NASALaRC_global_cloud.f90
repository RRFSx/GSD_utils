subroutine read_NASALaRC_global_cloud(satfile,nxp,nyp,ptop,htop,teff,phase,lwp_iwp,lat,lon,cbase_time,time_offset)
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
!         satfile - file to read
!         nxp, nyp - dimensions of data  
!
!   output argument list:
!         ptop - cloud top pressure
!         htop - cloud top height
!         teff - cloud effective temperature
!         phase - cloud phase
!         lwp_iwp - liquid or ice water path
!         lat - latitude of ob
!         lon - longitude of ob
!         cbase_time - units text from time_offset, indicates the date
!         time_offset - seconds since start of this day
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
  use kinds, only: r_kind,i_kind

  implicit none
!
  INCLUDE 'netcdf.inc'
!
!  For NASA LaRC 
!
  CHARACTER*40   satfile

  INTEGER ::   nxp, nyp  ! dimension
  INTEGER     FAILURE_P 
  PARAMETER ( FAILURE_P  =  1       )
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
  CHARACTER*40 :: cbase_time
  INTEGER :: ulen
  INTEGER,PARAMETER :: maxlen = 40 
  INTEGER(i_kind) ::  base_time
  REAL*8      time_offset
  REAL*4      lat                            (  nxp,  nyp)
  REAL*4      lon                            (  nxp,  nyp)
  integer     phase                          (  nxp,  nyp)
  REAL*4      lwp_iwp                        (  nxp,  nyp)
  REAL*4      teff                           (  nxp,  nyp)
  REAL*4      ptop                           (  nxp,  nyp)
  REAL*4      htop                           (  nxp,  nyp)
!
!
!  ** misc
      
  integer i,j,k
  Integer nf_status,nf_fid,nx,ny,nf_vid

  integer :: NCID

  integer :: status

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! set geogrid fle name
!
  status = failure_p
  write(6,*) 'read in satellite data from: ',trim(satfile)
  nf_status = NF_OPEN(trim(satfile), NF_NOWRITE,nf_fid)
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
!    statements to fill time_offset
!
  nf_status = NF_INQ_VARID(nf_fid,'time_offset',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var time_offset'
  endif
  nf_status = NF_GET_VAR_DOUBLE(nf_fid,nf_vid,time_offset)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ time_offset '
  end if
  nf_status = NF_INQ_ATTLEN(nf_fid,nf_vid,'units',ulen)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'len time_offset units'
  end if
  if (ulen .gt. maxlen) then
        write(6,*) 'units string is too long'
        status=failure_p
        return 
  else
    nf_status = NF_GET_ATT_TEXT(nf_fid,nf_vid,'units',cbase_time)
    if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'get time_offset units'
    end if
  end if

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
!  print*, 'lat900',lat(900,350)
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
!   print*, 'lon900',lon(900,350)
! -------------------------------------------------
!    statements to fill ptop (cloud top pressure)
!
   nf_status = NF_INQ_VARID(nf_fid,'cloud_top_pressure',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var ptop'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,ptop)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ ptop '
   end if
!   print*, 'ptop900',ptop(900,350)
! -------------------------------------------------
!    statements to fill htop (cloud top height)
!
   nf_status = NF_INQ_VARID(nf_fid,'cloud_top_height',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var htop'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,htop)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ htop '
   end if

! -------------------------------------------------
!    statements to fill teff (cloud effective temperature)
!
   nf_status = NF_INQ_VARID(nf_fid,'cloud_effective_temperature',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var cloud_effective_temperature'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,teff)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ teff '
   end if
!   print*, 'teff900',teff(900,350)
! -------------------------------------------------
!    statements to fill phase (cloud phase)
!
   nf_status = NF_INQ_VARID(nf_fid,'cloud_phase',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var cloud_phase'
   endif
   nf_status = NF_GET_VAR_int(nf_fid,nf_vid,phase)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ cloud_phase '
   end if
!   print*, 'cloud_phase900',phase(900,350)
! -------------------------------------------------
!    statements to fill LWP-IWP (liquid water path)
!
   !nf_status = NF_INQ_VARID(nf_fid,'liquid_water_path',nf_vid)
   nf_status = NF_INQ_VARID(nf_fid,'cloud_lwp_iwp',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var liquid_water_path'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lwp_iwp)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ lwp_iwp '
   end if
!   print*, 'lwp_iwp900',lwp_iwp(900,350)
! -------------------------------------------------

!
end subroutine read_NASALaRC_global_cloud


subroutine read_NASALaRC_global_cloud_dim(satfile,nxp,nyp)
!
!   PRGMMR: Terra Ladwig      ORG: GSD        DATE: 2015-03-19
!
! ABSTRACT: 
!     This routine reads in NASA LaRC global cloud dimensions  
!
! PROGRAM HISTORY LOG:
!
!   input argument list:
!         satfile - file to read
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
  CHARACTER*40   satfile

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
  write(6,*) 'read in satellite dim from: ',trim(satfile)
  nf_status = NF_OPEN(trim(satfile), NF_NOWRITE,nf_fid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_OPEN '
        status=failure_p
        return
  endif  
  !nf_status = NF90_OPEN(trim(satfile), NF90_NOWRITE,nf_fid)

  ! get dimension id 
  nf_status = NF_INQ_DIMID(nf_fid, 'image_y', dimid_y)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_INQ_DIMID'
        status=failure_p
        return
  endif
  nf_status = NF_INQ_DIMID(nf_fid, 'image_x', dimid_x)
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
  nf_status = NF_INQ_DIMLEN(nf_fid, dimid_x, nxp)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_INQ_DIMLEN'
        status=failure_p
        return
  endif


!
end subroutine read_NASALaRC_global_cloud_dim
