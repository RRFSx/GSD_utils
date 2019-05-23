program  process_SATCAST
!
!   PRGMMR: Tracy Lorraine Smith     ORG: GSD        DATE: 2012-09-04
!
! ABSTRACT: 
!     This routine reads in UAH SATCAST products and 
!     interpolates them into the GSI mass grid
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
!   MACHINE:  Zeus
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind,r_single
  use map_utils
  use misc_definitions_module , only : PROJ_LC, PROJ_ROTLL
  use constants_module ,only : EARTH_RADIUS_M
  use constants, only: init_constants_derived, deg2rad
  use gridmod_gsimap ,only : nlon,nlat,init_general_transform,tll2xy,txy2ll

  implicit none
!
  INCLUDE 'netcdf.inc'
!
  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid
!  integer(i_kind) :: nlon,nlat
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  real(r_kind),allocatable:: rxlon(:,:)    !
  real(r_kind),allocatable:: rylat(:,:)    !

  real ::  userDX, userDY, CEN_LAT, CEN_LON
  real ::  userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON
  integer :: MAP_PROJ

  type (proj_info) :: proj_stack
  REAL :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
  REAL :: knowni, knownj, dx
  REAL :: user_known_x,user_known_y

  CHARACTER*180   geofile
!
!  For SATCAST 
!
  CHARACTER*180   workPath
  CHARACTER*80   satfile
  INTEGER ::   nxp, nyp  ! dimension
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
  REAL(r_single), allocatable ::   lat_l(:,:)
  REAL(r_single), allocatable ::   lon_l(:,:)
  REAL(r_single), allocatable ::   ir_cr_l(:,:)
  REAL(r_single), allocatable ::   sos_l(:,:)
!
!  array for RR
!
  REAL(r_single), allocatable ::   w_ir_cr (:,:)
  integer(i_kind),allocatable ::   nlev_cld(:,:)

!
! Working
  integer  nfov
! ***parameter (nfov=160)  ! mhu change for updated LARC data
! ***  parameter (nfov=1500)  !tls for satcast datay
  parameter (nfov=3000)
  real, allocatable ::     CRxx(:,:,:)
  real,allocatable  ::     xdist(:,:,:), xxxdist(:)
  real     fr,sqrt, qc, type
  integer,allocatable  ::  PHxx(:,:,:),index(:,:), jndex(:)
  integer  ioption
  integer  ixx,ii,jj,med_pt,igrid,jgrid  &
               ,ncount,ncount1,ncount2,ii1,jj1,nobs,n

!
!
!  ** misc
      
  real(r_kind)        :: xc  ! x-grid coordinate (grid units)
  real(r_kind)        :: yc  ! y-grid coordinate (grid units)
  real(r_kind)        :: rlon  ! earth longitude (radians)
  real(r_kind)        :: rlat  ! earth latitude  (radians)

  logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain

  integer i,j,k,ipt,jpt,cfov
  Integer nf_status,nf_fid,nf_vid

  integer :: NCID
  integer :: isat

  integer :: status,mype

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! set geogrid fle name
!
  call init_constants_derived

  workPath='./'
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT

  call GET_MAP_ATT_geo(geofile, userDX, userDY, CEN_LAT, CEN_LON, &
                userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ)
  write(*,*) userDX, userDY, CEN_LAT, CEN_LON
  write(*,*) userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat),rxlon(nlon,nlat))
  allocate(ylat(nlon,nlat),rylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)
!
!   setup  map
!
!  if (MAP_PROJ == PROJ_LC) then
!     user_known_x = (NLON+1)/2.0
!     user_known_y = (NLAT+1)/2.0
!     call map_init(proj_stack)
!
!     call map_set(MAP_PROJ, proj_stack, &
!                  truelat1=userTRUELAT1, &
!                  truelat2=userTRUELAT2, &
!                  stdlon=STAND_LON, &
!                  lat1=CEN_LAT, &
!                  lon1=CEN_LON, &
!                  knowni=user_known_x, &
!                  knownj=user_known_y, &
!                  dx=userDX, &
!                  r_earth=earth_radius_m)
!  else
     mype=0
     rylat=ylat*deg2rad
     rxlon=xlon*deg2rad
     call init_general_transform(rylat,rxlon,mype)
!  endif
!
  allocate (xdist(nlon,nlat,nfov), xxxdist(nfov))
  allocate (CRxx(nlon,nlat,nfov),index(nlon,nlat), jndex(nfov))
  index=0
!
!  read in the UAH SATCAST data
       satfile='UAH_SATCAST.nc'
       nxp=6480
       nyp=2200
       allocate(lat_l(nxp,nyp))
       allocate(lon_l(nxp,nyp))
       allocate(ir_cr_l(nxp,nyp))
       allocate(sos_l(nxp,nyp))
     lat_l =-999.
     lon_l =-999.
     ir_cr_l =i-999.
     sos_l =-999.
!
     call read_satcast(satfile,nxp,nyp,ir_cr_l,sos_l,lat_l,lon_l)

     write(6,*)'SATCAST isat/nxl/nyl =',isat,nxp, nyp

!     write(6,*)'SATCAST lat  =', (lat_l(500,j),j=1,nyp,50)
!     write(6,*)'SATCAST lon  =', (lon_l(500,j),j=1,nyp,50)
!     write(6,*)'SATCAST IR_CR  =', (IR_CR(500,j),j=1,nyp,50)

! -----------------------------------------------------------
! -----------------------------------------------------------
!     Map each FOV onto RR grid points 
! -----------------------------------------------------------
! -----------------------------------------------------------
     do jpt=1,nyp
     do ipt=1,nxp
!       if (ir_cr_l(ipt,jpt).ne.-999.) then
       if (ir_cr_l(ipt,jpt).ne.-999..and.sos_l(ipt,jpt).ge.60.) then
!  Indicates there is some data (not missing)
!         if (MAP_PROJ == PROJ_LC) then
!           call latlon_to_ij(proj_stack, 90.0-lat_l(ipt,jpt), lon_l(ipt,jpt), xc, yc)
!         else
           rlon=lon_l(ipt,jpt)*deg2rad
!mhu           rlat=(90.0-lat_l(ipt,jpt))*deg2rad
           rlat=lat_l(ipt,jpt)*deg2rad
           call tll2xy(rlon,rlat,xc,yc)
!           call txy2ll(xc,yc,rlon,rlat)
!         endif
! * Compute RR grid x/y at lat/lon of cloud data
! -----------------------------------------------------------
! * XC,YC should be within RR boundary, i.e., XC,YC >0
!         print*, 'rlon,rlat,xc,yc',rlon,rlat,xc,yc

         ii1 = int(xc+0.5)
         jj1 = int(yc+0.5)
         do jj = max(1,jj1-1), min(nlat,jj1+1)
         if (jj1-1.ge.1 .and. jj1+1.le.nlat) then
         do ii = max(1,ii1-1), min(nlon,ii1+1)
         if (ii1-1.ge.1 .and. ii1+1.le.nlon) then
!         if(XC .ge. 1. .and. XC .lt. nlon .and.        &
!            YC .ge. 1. .and. YC .lt. nlat) then
!             ii1 = int(xc+0.5)
!             jj1 = int(yc+0.5)
!             ii=ii1
!             jj=jj1

! * We check multiple data within gridbox
!                 print*, 'index(ii,jj)',index(ii,jj),ii,jj,nfov
                 if (index(ii,jj).lt.nfov) then
                   index(ii,jj) = index(ii,jj) + 1

                   CRxx(ii,jj,index(ii,jj)) = ir_cr_l(ipt,jpt)
                   xdist(ii,jj,index(ii,jj)) = sqrt(      &
                      (XC+1-ii)**2 + (YC+1-jj)**2)
                 else
                   write(6,*) ' too many data in one grid, increase nfov'
                   write(6,*) nfov, index(ii,jj),ii,jj
                   stop 1234
                 end if
         endif
         enddo ! ii
         endif
         enddo  ! jj
!         endif   ! observation is in the domain

       endif   ! cr_ir_l >= 0
     enddo   ! ipt
     enddo   ! jpt

     deallocate(lat_l,lon_l,ir_cr_l)

!
  write(6,*) 'The max index number is: ', maxval(index)

! * ioption = 1 is nearest neighrhood
! * ioption = 2 is median of cloudy fov
  ioption = 2
  allocate(w_ir_cr(nlon,nlat))
  w_ir_cr=99999.

  do jj = 1,nlat
  do ii = 1,nlon
    if (index(ii,jj) .lt. 3) then
!           w_pcld(ii,jj) = Pxx(ii,jj,1)
!           w_tcld(ii,jj) = Txx(ii,jj,1)
!           w_frac(ii,jj) = float(Nxx(ii,jj,1))/100.
!           w_eca(ii,jj) =  float(Nxx(ii,jj,1))/100.
    elseif(index(ii,jj) .ge. 3) then

! * We decided to use nearest neighborhood for ECA values,
! *     a kind of convective signal from GOES platform...
!
! * Sort to find median value 
      if(ioption .eq. 2) then    !pick median 
          do i=1,index(ii,jj)
              jndex(i) = i
              xxxdist(i) = CRxx(ii,jj,i)
          enddo
          call sortmed(xxxdist,index(ii,jj),jndex,fr)
          med_pt = index(ii,jj)/2  + 1
          w_ir_cr(ii,jj) = CRxx(ii,jj,jndex(med_pt)) !  g/m^2
      endif   ! pick median
!
    endif   ! index > 3
  enddo  !ii
  enddo  !jj

      write (6,*) 'index'
      write (6,'(10i10)') (index(300,jj),jj=1,nlat,10)
      write (6,*) 'w_ir_cr '
      write (6,'(10f10.2)') (w_ir_cr (300,jj),jj=1,nlat,10)
  OPEN(15, file='ScstInGSI.dat',form='unformatted')
    write(15)  nlon,nlat
    write(15)  w_ir_cr
  CLOSE(15)
!
!  write out results
!
  call write_bufr_SATCAST(nlon,nlat,index,w_ir_cr)
!

!  DO j=1,nlat
!  DO I=1,nlon
!    if( w_ir_cr(i,j) > 99998.0 ) w_ir_cr(i,j) = 0.0
!  ENDDO
!  ENDDO
!
  write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="

end program process_SATCAST

subroutine read_satcast(satfile, nxp, nyp, IR_CR, sos_in, lat, lon)
!
!   PRGMMR: Tracy Lorraine Smith          ORG: GSD        DATE: 2012-09-04
!
! ABSTRACT: 
!     This routine reads in UAH SATCAST products and 
!     interpolates them into the GSI mass grid
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
!   MACHINE:  Zeus
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
!  For UAH SATCAST data 
!
  CHARACTER*40   satfile

  INTEGER ::   nxp, nyp  ! dimension
  INTEGER     FAILURE_P 
  PARAMETER ( FAILURE_P  =  1       )
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
  REAL*4      lat                            (  nxp,  nyp)
  REAL*4      lon                            (  nxp,  nyp)
  REAL*4      IR_CR                          (  nxp,  nyp)
  REAL*4      SOS_in                         (  nxp,  nyp)
  REAL*4      SOS_test                         (  nxp,  nyp)
!
!
!  ** misc
      
  integer i,j,k,xdim,ydim,x,y
  Integer nf_status,nf_fid,nx,ny,nf_vid

  integer :: NCID

  integer :: status,numpts,numsos

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! open netcdf file for reading
!
  status = failure_p
  write(6,*) 'read in satellite data from: ',trim(satfile)
  nf_status = NF_OPEN(trim(satfile), NF_NOWRITE,nf_fid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'NF_OPEN '//trim(satfile)
        status=failure_p
        return
  endif

      print *,'nf_fid inside read_satcast',nf_fid

!
!  Fill all dimension values
!
!
! Get size of xdim
!
      nf_status = NF_INQ_DIMID(nf_fid,'x',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim x'
      endif
      nf_status = NF_INQ_DIMLEN(nf_fid,nf_vid,xdim)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim x2'
      endif
!
! Get size of ydim
!
      nf_status = NF_INQ_DIMID(nf_fid,'y',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim y'
      endif
      nf_status = NF_INQ_DIMLEN(nf_fid,nf_vid,ydim)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim y2'
      endif

      if(xdim.ne.nxp.or.ydim.ne.nyp) then
        print*, 'Dimension mismatch! IR_CR Stop!',xdim,nxp,ydim,nyp
        stop
      endif

! -------------------------------------------------
!    statements to fill Temp_trend_107, aka IR_Cooling_Rate
!
      nf_status = NF_INQ_VARID(nf_fid,'Temp_trend_107',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var IR_CR'
      endif
      nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,IR_CR)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ IR_CR '
      end if
      print*, 'min,max of ctcr'
      call outqv(IR_CR,nxp,nyp,1)

! -------------------------------------------------
!    statements to fill Strength of Signal (SOS)
!
      nf_status = NF_INQ_VARID(nf_fid,'SOS',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var SOS'
      endif
      nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,SOS_in)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_SOS '
      end if
      do i=1,nxp
      do j=1,nyp
        sos_test(i,j)=0.
        if(sos_in(i,j).lt.100.) sos_test(i,j)=sos_in(i,j)
      enddo
      enddo

      print*, 'min,max of sos'
      call outqv(SOS_test,nxp,nyp,1)

!*** Close file
      nf_status = nf_close(nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'nf_close'
      endif

!*** Read latitude and longitude of grid points
!***   First, open latitude file
!
!  Open netcdf File for reading
!
      nf_status = NF_OPEN('SATCASTLAT',NF_NOWRITE,nf_fid)

      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'NF_OPEN SATCASTLAT'
        status=failure_p
!        return
      endif

!
!  Fill all dimension values
!
!
! Get size of xdim
!
      nf_status = NF_INQ_DIMID(nf_fid,'xdim',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim xdim'
      endif
      nf_status = NF_INQ_DIMLEN(nf_fid,nf_vid,xdim)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim xdim2'
      endif
!
! Get size of ydim
!
      nf_status = NF_INQ_DIMID(nf_fid,'ydim',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim ydim'
      endif
      nf_status = NF_INQ_DIMLEN(nf_fid,nf_vid,ydim)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim ydim2'
      endif

      if(xdim.ne.nxp.or.ydim.ne.nyp) then
        print*, 'Dimension mismatch! Lat Stop!',xdim,nxp,ydim,nyp
        stop
      endif

! -------------------------------------------------
! -------------------------------------------------
!    statements to fill Latitude
!
      nf_status = NF_INQ_VARID(nf_fid,'Latitude',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var Latitude'
      endif
      nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lat)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ latitude'
      end if
      print*, 'latitude',lat(3000,1100)

!*** Close file
      nf_status = nf_close(nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'nf_close'
      endif
!
!
!*** Now the longitudes
!  Open netcdf File for reading
!
      nf_status = NF_OPEN('SATCASTLON',NF_NOWRITE,nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'NF_OPEN SATCASTLON'
        status=failure_p
!        return
      endif
!
!  Fill all dimension values
!
!
! Get size of xdim
!
      nf_status = NF_INQ_DIMID(nf_fid,'xdim',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim xdim'
      endif
      nf_status = NF_INQ_DIMLEN(nf_fid,nf_vid,xdim)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim xdim2'
      endif
!
! Get size of ydim
!
      nf_status = NF_INQ_DIMID(nf_fid,'ydim',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim ydim'
      endif
      nf_status = NF_INQ_DIMLEN(nf_fid,nf_vid,ydim)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim ydim2'
      endif

      if(xdim.ne.nxp.or.ydim.ne.nyp) then
        print*, 'Dimension mismatch! Lon Stop!',xdim,nxp,ydim,nyp
        stop
      endif

! -------------------------------------------------
! -------------------------------------------------
!    statements to fill Longitude
!
      nf_status = NF_INQ_VARID(nf_fid,'Longitude',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var Longitude'
      endif
        nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lon)
      if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ longitude '
      end if
      print*, 'longitude',lon(3000,1100)

!*** Close file
      nf_status = nf_close(nf_fid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'nf_close'
      endif

!*** check corners
       print*, 'SW corner',lat(1,1),lon(1,1)
       print*, 'NW corner',lat(1,2200),lon(1,2200)
       print*, 'SE corner',lat(6480,1),lon(6480,1)
       print*, 'NE corner',lat(6480,2200),lon(6480,2200)
       numsos=0
       numpts=0
       Do j=1,nyp
       Do i=1,nxp       
         if(IR_CR(i,j).ne.0.0) then
!            print*, 'ir_cr,lat,lon',ir_cr(i,j),lat(i,j),lon(i,j)
             if(IR_CR(i,j).gt.100.) print*, 'bignum',ir_cr(i,j),i,j
            numpts=numpts+1
         endif
         if(sos_in(i,j).ge.60.) then
           numsos=numsos+1
           if(sos_in(i,j).ge.60.) print*, 'bigsos',sos_in(i,j), &
              ir_cr(i,j),i,j, &
              lat(i,j),lon(i,j)
         endif
       Enddo
       Enddo
       print*, 'numpts ir_cr',numpts,'numpts sos',numsos
!
end subroutine read_satcast

subroutine sortmed(p,n,is)
      real p(n)
      integer is(n)
! * count cloudy fov
      real    f
      integer cfov
      cfov = 0
      do i=1,n
! - changed for NASA LaRC, p set = -9 for clear FOVs
         if(p(i) .gt. 0.) cfov = cfov + 1
      enddo
      f = float(cfov)/(max(1,n))
! cloud-top pressure is sorted high cld to clear
      nm1 = n-1 
      do 10 i=1,nm1
      ip1 = i+1 
        do 10 j=ip1,n
        if(p(i).le.p(j)) goto 10
          temp = p(i) 
          p(i) = p(j)
          p(j) = temp
          iold  = is(i)
          is(i) = is(j)
          is(j) = iold
   10 continue
      return
end subroutine sortmed
                                                                                 
