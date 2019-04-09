module module_RW_dealiasing
!
! dealiasing radar radial wind 
!
  use module_RW_DPQC, only : rw_dpqc
  use module_background, only : background
  use module_map_utils, only : map_util

  implicit none
  integer, parameter :: r_kind=8

!  parameters
  real(r_kind), parameter :: one=1.0_r_kind
  real(r_kind), parameter :: two=2.0_r_kind
  real(r_kind), parameter :: r8=8.0_r_kind
  real(r_kind), parameter :: half=one/two
  real(r_kind), parameter :: r89_5=89.5_r_kind
  real(r_kind), parameter :: rearth=6370000.00_r_kind
  real(r_kind), parameter :: four_thirds=4.0_r_kind/3.0_r_kind
  real(r_kind), parameter :: pi      = acos(-one)
  real(r_kind), parameter :: deg2rad=pi/180.0_r_kind
  real(r_kind), parameter :: rad2deg = one/deg2rad

  public :: rw_dealiasing
! set default to private
  private

  type :: rw_dealiasing

     integer :: Azimuth,Gate
     real :: rwAzimuth,rwtilt,rwheight
     real :: rwlon,rwlat
     real, allocatable :: rw2d(:,:)

     real(r_kind) :: stalon
     real(r_kind) :: stalat
     real(r_kind) :: stahgt 

  contains
     procedure ::  cal_bkrw  => calculate_background_rw
     procedure ::  location  => get_rw_location
     procedure ::  initial   => initial_rw_dealiasing
     procedure ::  destroy   => destroy_rw_dealiasing
  end type rw_dealiasing


  contains

  subroutine calculate_background_rw(this,rwdpqc,bkgd,map)
!
!  calculate background RW
!
        class(rw_dealiasing),intent(inout) :: this
        type(rw_dpqc), intent(in)          :: rwdpqc
        type(background), intent(in)       :: bkgd
        type(map_util),intent(inout)       :: map

        real(r_kind) :: thisrange,thisazimuth,thistilt
        integer :: i,iaz
!
!
        write(*,*) rwdpqc%Elevation,rwdpqc%RangeToFirstGate
        write(*,*) rwdpqc%Latitude,rwdpqc%Longitude,rwdpqc%Height
        write(*,*) rwdpqc%rUnambiguous_Range
        write(*,*) rwdpqc%GateWidth
        write(*,*) rwdpqc%radarName
        this%stalat=rwdpqc%Latitude
        this%stalon=rwdpqc%Longitude
        this%stahgt=rwdpqc%Height
        thistilt=rwdpqc%Elevation
!
        do iaz=1,this%Azimuth
!           write(*,*) iaz, rwdpqc%rwAzimuth(iaz),rwdpqc%NyquistV(iaz)
           thisazimuth=rwdpqc%rwAzimuth(iaz)
           do i=1,this%Gate
              if(rwdpqc%rw2d(i,iaz) > -500.0 .and. rwdpqc%rw2d(i,iaz) < 500.0) then  ! missing value -99900
                 thisrange=rwdpqc%RangeToFirstGate+rwdpqc%GateWidth*(i-1)
!                 write(*,*) '=======>',i,iaz,this%rw2d(i,iaz),rwdpqc%rw2d(i,iaz)

                 call this%location(thisrange,thistilt,thisazimuth)

!                 write(*,'(a,5f12.5)') 'observation height=',this%rwheight,this%stahgt,thisrange
!                 write(*,'(a,5f12.5)') 'corrected_tilt=',this%rwtilt,thistilt
!                 write(*,'(a,5f12.5)') 'rw obs latlon =',this%rwlat,this%rwlon,this%stalat,this%stalon
!                 write(*,'(a,5f12.5)') 'corrected_azimuth=',this%rwAzimuth,thisazimuth

              endif
           enddo
        enddo

  end subroutine calculate_background_rw
!
  subroutine get_rw_location(this,thisrange,thistilt,thisazimuth)
!
!  find earch location of one RW observation
!
        class(rw_dealiasing),intent(inout) :: this

        real(r_kind),intent(in) :: thisrange,thisazimuth,thistilt

!        
!  local 
        real(r_kind) :: this_stalat,this_stalon,this_stahgt
        real(r_kind) thislat,thislon
        real(r_kind) rad_per_meter,thishgt
        real(r_kind) rlonloc,rlatloc
        real(r_kind) a43,aactual,b,c,selev0,celev0,epsh,erad,h,ha
        real(r_kind) celev,selev,gamma
        real(r_kind) this_stalatr,thisazimuthr,thistiltr
        real(r_kind) corrected_tilt,corrected_azimuth
!
        real(r_kind) rlon0,clat0,slat0,rlonglob,rlatglob
        real(r_kind) clat1,caz0,saz0,cdlon,sdlon,caz1,saz1
        real(r_kind) deldistmax,deltiltmax
        real(r_kind) deldistmin,deltiltmin
!   
!
        this_stalat=this%stalat
        this_stalon=this%stalon
        this_stahgt=this%stahgt

!
        rad_per_meter= one/rearth
        erad = rearth
!
        rlon0=deg2rad*this_stalon
        this_stalatr=this_stalat*deg2rad
        clat0=cos(this_stalatr)
        slat0=sin(this_stalatr)

        aactual=erad+this_stahgt
        a43=four_thirds*aactual
        thistiltr=thistilt*deg2rad
        selev0=sin(thistiltr)
        celev0=cos(thistiltr)
        b=thisrange*(thisrange+two*aactual*selev0)
        c=sqrt(aactual*aactual+b)
        ha=b/(aactual+c)
        epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
        h=ha-epsh
        thishgt=this_stahgt+h
        
!            Get corrected tilt angle
        celev=celev0
        selev=selev0
        if(thisrange>=one) then
           celev=a43*celev0/(a43+h)
           selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
        end if
        corrected_tilt=atan2(selev,celev)*rad2deg
        deltiltmax=max(corrected_tilt-thistilt,deltiltmax)
        deltiltmin=min(corrected_tilt-thistilt,deltiltmin)
        gamma=half*thisrange*(celev0+celev)
        deldistmax=max(gamma-thisrange,deldistmax)
        deldistmin=min(gamma-thisrange,deldistmin)

!            Get earth lat lon of superob
        thisazimuthr=thisazimuth*deg2rad
        rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
        rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
        call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
        thislat=rlatglob*rad2deg
        thislon=rlonglob*rad2deg

!            Keep away from poles, rather than properly deal with polar singularity
        if(abs(thislat)>r89_5) stop 123456

!            Get corrected azimuth
        clat1=cos(rlatglob)
        caz0=cos(thisazimuthr)
        saz0=sin(thisazimuthr)
        cdlon=cos(rlonglob-rlon0)
        sdlon=sin(rlonglob-rlon0)
        caz1=clat0*caz0/clat1
        saz1=saz0*cdlon-caz0*sdlon*slat0
        corrected_azimuth=atan2(saz1,caz1)*rad2deg

        this%rwAzimuth=corrected_azimuth
        this%rwtilt=corrected_tilt
        this%rwheight=thishgt
        this%rwlon=thislon
        this%rwlat=thislat

     end subroutine get_rw_location

     subroutine initial_rw_dealiasing(this,rwdpqc)
!
!  set up dimension and allocate memory
!
        implicit none

        class(rw_dealiasing),intent(inout) :: this
        type(rw_dpqc), intent(in)         :: rwdpqc

        this%Azimuth=rwdpqc%Azimuth
        this%Gate=rwdpqc%Gate
        this%rwAzimuth=0.0
        this%rwlon=0.0
        this%rwlat=0.0
        this%rwheight=0.0
        this%rwtilt=0
        if(allocated(this%rw2d)) deallocate(this%rw2d)
        allocate(this%rw2d(this%Gate,this%Azimuth))
        this%rw2d=99999.0

     end subroutine initial_rw_dealiasing

     subroutine destroy_rw_dealiasing(this)
!
!  release memory
!
        implicit none

        class(rw_dealiasing),intent(inout) :: this

        this%Azimuth=0
        this%Gate=0
        this%rwAzimuth=0.0
        this%rwlon=0.0
        this%rwlat=0.0
        this%rwheight=0.0
        this%rwtilt=0
        if(allocated(this%rw2d)) deallocate(this%rw2d)

     end subroutine destroy_rw_dealiasing

SUBROUTINE invtllv(ALM,APH,TLMO,CTPH0,STPH0,TLM,TPH)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invtllv
!
!   prgrmmr:
!
! abstract:  inverse of tllv:  input ALM,APH is rotated lon,lat
!                   output is earth lon,lat, TLM,TPH
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     alm   -- input earth longitude
!     aph   -- input earth latitude
!     tlmo  -- input earth longitude of rotated grid origin (radrees)
!     ctph0 -- cos(earth lat of rotated grid origin)
!     stph0 -- sin(earth lat of rotated grid origin)
!
!   output argument list:
!     tlm   -- rotated grid longitude
!     tph   -- rotated grid latitude
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  real(r_kind),intent(in   ) :: alm,aph,tlmo,ctph0,stph0
  real(r_kind),intent(  out) :: tlm,tph

  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom

  RELM=ALM
  SRLM=SIN(RELM)
  CRLM=COS(RELM)
  SPH=SIN(APH)
  CPH=COS(APH)
  CC=CPH*CRLM
  ANUM=CPH*SRLM
  DENOM=CTPH0*CC-STPH0*SPH
  TLM=tlmo+ATAN2(ANUM,DENOM)
  TPH=ASIN(CTPH0*SPH+STPH0*CC)

END SUBROUTINE invtllv

End module module_RW_dealiasing
