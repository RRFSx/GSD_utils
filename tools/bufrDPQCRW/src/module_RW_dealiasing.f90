module module_RW_dealiasing
!
! dealiasing radar radial wind 
!
  use module_RW_DPQC, only : rw_dpqc
  use module_background, only : background
  use module_map_utils, only : map_util

  implicit none
  integer, parameter :: r_kind=8,r_single=4

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
     logical :: include_w
     real, allocatable :: rw2d(:,:)
     real, allocatable :: rw2d_hgt(:,:)
     real, allocatable :: vaduu(:),vadvv(:)

     real(r_kind) :: stalon
     real(r_kind) :: stalat
     real(r_kind) :: stahgt 

  contains
     procedure ::  cal_bkrw  => calculate_background_rw
     procedure ::  cal_vad   => calculate_VAD
     procedure ::  dealiasing_bkrw => dealiasing_with_bkrw
     procedure ::  dealiasing_vad  => dealiasing_with_vad
     procedure ::  dealiasing_localrw  => dealiasing_with_localrw
     procedure ::  location  => get_rw_location
     procedure ::  initial   => initial_rw_dealiasing
     procedure ::  destroy   => destroy_rw_dealiasing
  end type rw_dealiasing


  contains

     function binarySearch(nlvl,rlist,key) result(rlocation)
! binary search
        implicit none 

        integer,        intent(in) :: nlvl
        real(r_single), intent(in) :: rlist(nlvl)
        real(r_single), intent(in) :: key

        real(r_single) :: rlocation

        integer :: low, high, mid  
        integer :: i
        
        rlocation=-1.0

        low=1
        high=nlvl

        if(key <= rlist(low)) then 
           rlocation=1.0
        elseif(key >= rlist(high)) then
           rlocation=nlvl
        else
           i=0
           do while (high >= low)

              mid = (low + high) / 2

              i=i+1
              if(i > nlvl) then
                 write(*,*) 'something wrong in binarySearch=',i
                 stop 23456
              endif
   
              if (key < rlist(mid)) then       
                 high = mid
              else if (key >= rlist(mid+1)) then
                 low = mid+1
              else
                 if(key >= rlist(mid) .and. key < rlist(mid+1) ) then               
                    rlocation=mid+(key-rlist(mid))/(rlist(mid+1)-rlist(mid))
!                    write(*,*) rlocation,mid,key,rlist(mid),rlist(mid+1)
                    exit
                 else
                    write(*,*) 'something wrong in binarySearch',mid,key
                    stop 12345
                 endif
              endif
           enddo
        endif

  end function 

  subroutine calculate_VAD(this,rwdpqc)
!
!  dealiasinng based on VAD
!
   implicit none
        class(rw_dealiasing),intent(inout) :: this
        type(rw_dpqc), intent(inout)          :: rwdpqc

!
! variables for VAD wind calculation
        integer :: irad, nrad
        integer :: num_rng, num_ang
        real(r_single),allocatable :: vel(:,:),aza(:)
        real(r_single),allocatable :: rng(:)
        real el, cntmn, tgap, rmsmx, rmissing
! output
        real(r_single),allocatable :: uvad(:,:),vvad(:,:)
        real(r_single),allocatable :: err(:)
        real(r_single),allocatable :: con(:)
!misc
        integer :: i
        real(r_single) :: aaa
!
!
        nrad=1
        irad=1
        num_rng=rwdpqc%Gate
        num_ang=rwdpqc%Azimuth
        allocate(vel(num_rng,num_ang),aza(num_ang))
        allocate(rng(num_rng))
        allocate(uvad(num_rng,nrad),vvad(num_rng,nrad))
        allocate(err(num_rng),con(num_rng))
!
        do i=1,num_ang
           aza(i) = 450.0_r_single - rwdpqc%rwAzimuth(i)
           if(aza(i) > 360.0) aza(i) = aza(i) - 360.0_r_single
        enddo

        aaa=1.0_r_single/1000.0_r_single
        do i=1,num_rng
           rng(i)=(rwdpqc%RangeToFirstGate+rwdpqc%GateWidth*(i-1))*aaa
        enddo

        el=rwdpqc%Elevation
        cntmn=60
        tgap=270
        rmsmx=50.0_r_single
        rmissing=-8888.0
!        write(*,*) aza

        vel=rwdpqc%rw2d

        call vad(vel,aza,el,num_rng,num_ang,rng,cntmn,tgap,rmsmx, &
                 uvad,vvad,err,con,rmissing,irad,nrad)
   
        do i=1,num_rng
!            if( err(i) > 0.0) then
!               write(*,'(I5,10f10.2)') i,rng(i),uvad(i,1),vvad(i,1),err(i),con(i)
!            endif
            this%vaduu(i)=uvad(i,1)
            this%vadvv(i)=vvad(i,1)
        enddo
!
        deallocate(vel)
        deallocate(aza)
        deallocate(rng)
        deallocate(uvad)
        deallocate(vvad)
        deallocate(err)
        deallocate(con)
!
  end subroutine calculate_VAD

  subroutine dealiasing_with_localrw(this,rwdpqc,nsize)
!
!  dealiasinng based on local rw
!
        class(rw_dealiasing),intent(inout) :: this
        type(rw_dpqc), intent(inout)       :: rwdpqc
        integer, intent(in)                :: nsize

        real(r_single) :: azm, sinazm,cosazm
        real(r_single) :: tiltangle
        real(r_single) :: costilt,sintilt,cosazm_costilt,sinazm_costilt

        integer :: i,iaz,kk, numrw,ii,jj
        real(r_single) :: v0,vexpected,rw_obs,rkk
        real(r_single) :: sumrw, minsize
!
        
        write(*,*) '====> dealiasing with local rw. The size of local domain is ',nsize
        minsize=nsize*2*nsize*2*0.7

        do iaz=1,this%Azimuth
!           write(*,*) iaz, rwdpqc%rwAzimuth(iaz),rwdpqc%NyquistV(iaz),rwdpqc%Elevation

           v0=2.0_r_single*rwdpqc%NyquistV(iaz)

           do i=1,this%Gate
              if(rwdpqc%rw2d(i,iaz) > -500.0 .and. rwdpqc%rw2d(i,iaz) < 500.0) then  ! missing value -99900
 
                 sumrw=0.0
                 numrw=0
                 do jj=max(1,iaz-nsize),min(iaz+nsize,this%Azimuth)
                    do ii=max(1,i-nsize),  min(i+nsize,this%Gate)
                       if(abs(rwdpqc%rw2d(ii,jj)) < 500.0) then
                          sumrw=sumrw + rwdpqc%rw2d(ii,jj)
                          numrw=numrw+1
                       endif
                    enddo
                 enddo
                 if(numrw > minsize) then
                    vexpected=sumrw/float(numrw)
                 else
                    vexpected=-999.0
                 endif

                 if(abs(vexpected) < 888.0 ) then
                    rw_obs=rwdpqc%rw2d(i,iaz)
                    rkk=(vexpected-rw_obs)/v0
                    kk=0
                    if( rkk > 0.0) kk=int(rkk+0.5)
                    if( rkk < 0.0) kk=int(rkk-0.5)
         !           if(i < 20) write(*,*) iaz,i,vexpected,rw_obs,kk
                    if(abs(kk) > 0) then
                       rwdpqc%rw2d(i,iaz)=rw_obs + kk*v0
                       if(abs(kk) > 1) then
                          write(*,*) "dealiasing=",kk,vexpected,rw_obs,rwdpqc%rw2d(i,iaz)
                       endif
                    endif
                 endif  ! local wind is good
              endif
           enddo
        enddo
!
  end subroutine dealiasing_with_localrw

  subroutine dealiasing_with_vad(this,rwdpqc)
!
!  dealiasinng based on VAD
!
        class(rw_dealiasing),intent(inout) :: this
        type(rw_dpqc), intent(inout)          :: rwdpqc

        real(r_single) :: azm, sinazm,cosazm
        real(r_single) :: tiltangle
        real(r_single) :: costilt,sintilt,cosazm_costilt,sinazm_costilt

        integer :: i,iaz,kk
        real(r_single) :: v0,vexpected,rw_obs,rkk
        real(r_single) :: vaduu,vadvv
!
        
        write(*,*) '====> dealiasing with VAD'
!        do i=1,this%Gate
!           if( abs(this%vaduu(i)) < 888.0 .and. abs(this%vadvv(i)) < 888.0) then
!              write(*,'(I5,10f10.2)') i,this%vaduu(i),this%vadvv(i)
!           endif
!        enddo
!
        tiltangle=rwdpqc%Elevation*deg2rad

        do iaz=1,this%Azimuth
!           write(*,*) iaz, rwdpqc%rwAzimuth(iaz),rwdpqc%NyquistV(iaz),rwdpqc%Elevation

           azm=rwdpqc%rwAzimuth(iaz)*deg2rad
           cosazm  = cos(azm)  ! cos(azimuth angle)
           sinazm  = sin(azm)  ! sin(azimuth angle)
           costilt = cos(tiltangle) ! cos(tilt angle)
           sintilt = sin(tiltangle) ! sin(tilt angle)
           cosazm_costilt = cosazm*costilt
           sinazm_costilt = sinazm*costilt

           v0=2.0_r_single*rwdpqc%NyquistV(iaz)

           do i=1,this%Gate
              if(rwdpqc%rw2d(i,iaz) > -500.0 .and. rwdpqc%rw2d(i,iaz) < 500.0) then  ! missing value -99900
                 vaduu=this%vaduu(i)
                 vadvv=this%vadvv(i)
                 if(abs(vaduu) < 888.0 .and. abs(vadvv) < 888.0) then
                    vexpected=vaduu*cosazm_costilt+vadvv*sinazm_costilt
                    rw_obs=rwdpqc%rw2d(i,iaz)
                    rkk=(vexpected-rw_obs)/v0
                    kk=0
                    if( rkk > 0.0) kk=int(rkk+0.5)
                    if( rkk < 0.0) kk=int(rkk-0.5)
!                    if(i < 20) write(*,*) iaz,i,vexpected,rw_obs,vaduu,vadvv,v0,kk
                    if(abs(kk) > 0) then
                       rwdpqc%rw2d(i,iaz)=rw_obs + kk*v0
                       if(abs(kk) > 1) then
                          write(*,*) "dealiasing=",kk,vexpected,rw_obs,rwdpqc%rw2d(i,iaz)
                       endif
                    endif
                 endif  ! VAD wind is good
              endif
           enddo
        enddo
!
  end subroutine dealiasing_with_vad

  subroutine dealiasing_with_bkrw(this,rwdpqc)
!
!  dealiasinng based on background RW
!
        class(rw_dealiasing),intent(inout) :: this
        type(rw_dpqc), intent(inout)          :: rwdpqc

        integer :: i,iaz,kk
        real(r_single) :: v0,vexpected,rw_obs,rkk
!
        integer,parameter :: numbgfact=6
        real(r_single) :: bg_amp_factor(numbgfact)
        integer :: ibgfact
!
        write(*,*) '====> dealiasing with background RW'
! setup background amplify factor based on the height
        bg_amp_factor(1)=1.8_r_single   ! below 200m
        bg_amp_factor(2)=1.6_r_single   ! 200-400m
        bg_amp_factor(3)=1.4_r_single   ! 400-600m
        bg_amp_factor(4)=1.2_r_single   ! 600-800m
        bg_amp_factor(5)=1.2_r_single   ! 800-1000m
        bg_amp_factor(6)=1.0_r_single   ! above 1000m
!
        do iaz=1,this%Azimuth
!           write(*,*) iaz, rwdpqc%rwAzimuth(iaz),rwdpqc%NyquistV(iaz)
           v0=2.0_r_single*rwdpqc%NyquistV(iaz)
           do i=1,this%Gate
              if(rwdpqc%rw2d(i,iaz) > -500.0 .and. rwdpqc%rw2d(i,iaz) < 500.0) then  ! missing value -99900
                 if(abs(this%rw2d(i,iaz)) < 500.0) then
                    ibgfact=int(this%rw2d_hgt(i,iaz)/200.0) + 1
                    ibgfact=min(max(ibgfact,1),numbgfact)
                    vexpected=this%rw2d(i,iaz) * bg_amp_factor(ibgfact)
                    rw_obs=rwdpqc%rw2d(i,iaz)
                    rkk=(vexpected-rw_obs)/v0
                    kk=0
                    if( rkk > 0.0) kk=int(rkk+0.5)
                    if( rkk < 0.0) kk=int(rkk-0.5)
                 !   write(*,*) iaz,i,vexpected,rw_obs,v0,kk
                    if(abs(kk) > 0) then
                       rwdpqc%rw2d(i,iaz)=rw_obs + kk*v0
                       if(abs(kk) > 1) then
                          write(*,*) "dealiasing=",kk,vexpected,rw_obs,rwdpqc%rw2d(i,iaz)
                       endif
                    endif
                 endif
              endif
           enddo
        enddo
  end subroutine dealiasing_with_bkrw

  subroutine calculate_background_rw(this,rwdpqc,bkgd,map)
!
!  calculate background RW
!
        class(rw_dealiasing),intent(inout) :: this
        type(rw_dpqc), intent(in)          :: rwdpqc
        type(background), intent(inout)       :: bkgd
        type(map_util),intent(inout)       :: map

        real(r_kind) :: thisrange,thisazimuth,thistilt
        integer :: nz,k,i,iaz,kk
        real(r_single) :: xc,yc,rval,rlocation,dk
        real(r_single),allocatable :: rvalv(:)
        character(len=10) :: varname
!
        real(r_single) :: uges,vges,wges,ugesup,vgesup,wgesup
!
! temp variables for rwwind calculation
        real(r_single) :: dlat_earth_deg,dlon_earth_deg,dlat_earth,dlon_earth
        real(r_single) :: azm_earth,cosazm_earth,sinazm_earth
        real(r_single) :: azm, sinazm,cosazm
        real(r_single) :: tiltangle
        real(r_single) :: costilt,sintilt,cosazm_costilt,sinazm_costilt
        real(r_single) :: rwwind
!
! check background
!        call bkgd%listflds()
!        varname="U"
!        call bkgd%listfld(trim(varname))
!        call bkgd%listfld('V')
!        call bkgd%listfld('W')
!        call bkgd%listfld('H')
!        call bkgd%listfld('terrain')
! check rw observations
!        call rwdpqc%list() 
!
!
        nz=bkgd%nz
        allocate(rvalv(nz))

!        call bkgd%interp2d('V',10,10.5,10.5,rval)
!        write(*,*) rval
!        call bkgd%interp2dv('V',nz,10.5,10.5,rvalv)
!        do k=1,nz
!           write(*,*) k,rvalv(k)
!        enddo
!
        this%stalat=rwdpqc%Latitude
        this%stalon=rwdpqc%Longitude
        this%stahgt=rwdpqc%Height
        thistilt=rwdpqc%Elevation
!
        this%rw2d_hgt=-999.0
!        do iaz=1,2
        do iaz=1,this%Azimuth
!           write(*,*) iaz, rwdpqc%rwAzimuth(iaz),rwdpqc%NyquistV(iaz)
           thisazimuth=rwdpqc%rwAzimuth(iaz)
           do i=1,this%Gate
!for test              if(rwdpqc%rw2d(i,iaz) > -500.0 .and. rwdpqc%rw2d(i,iaz) < 500.0) then  ! missing value -99900
                 thisrange=rwdpqc%RangeToFirstGate+rwdpqc%GateWidth*(i-1)
                 call this%location(thisrange,thistilt,thisazimuth)
                 this%rw2d_hgt(i,iaz)=this%rwheight
!                 write(*,'(a,5f12.5)') 'observation height=',this%rwheight,this%stahgt,thisrange
!                 write(*,'(a,5f12.5)') 'corrected_tilt=',this%rwtilt,thistilt
!                 write(*,'(a,5f12.5)') 'rw obs latlon =',this%rwlat,this%rwlon,this%stalat,this%stalon
!                 write(*,'(a,5f12.5)') 'corrected_azimuth=',this%rwAzimuth,thisazimuth
!  find location in grid coordiante
                 call map%tll2xy(this%rwlon, this%rwlat,xc,yc)
                 if((xc >=1 .and. xc <=map%nlon) .and. &
                    (yc >=1 .and. yc <=map%nlat)) then
!                    write(*,*) 'get bk Vr here: xc=',xc,' yx=',yc,' height=',this%rwheight
                    call bkgd%interp2dv('H',nz,xc,yc,rvalv)
                    rlocation=binarySearch(nz,rvalv,this%rwheight)
                    kk=int(rlocation)
                    dk=rlocation-float(kk)
                    if(kk==nz) kk=kk-1
                    if(kk >=1 .and. kk < nz) then
                       call bkgd%interp2d('U',kk,  xc,yc,uges)
                       call bkgd%interp2d('U',kk+1,xc,yc,ugesup)
                       call bkgd%interp2d('V',kk,  xc,yc,vges)
                       call bkgd%interp2d('V',kk+1,xc,yc,vgesup)
                       call bkgd%interp2d('W',kk,  xc,yc,wges)
                       call bkgd%interp2d('W',kk+1,xc,yc,wgesup)
!                       write(*,*) uges,ugesup,vges,vgesup,wges,wgesup
! get u v w at observation RW point
                       uges= uges*(1.0-dk) + ugesup*dk
                       vges= vges*(1.0-dk) + vgesup*dk
                       wges= wges*(1.0-dk) + wgesup*dk
!                       write(*,*) '1=',uges,vges,wges,dk,rlocation
!                       write(*,*) '2=',this%rwlat,this%rwlon,this%rwAzimuth,this%rwtilt
! calculate RW
                       dlon_earth_deg = this%rwlon
                       if(dlon_earth_deg < 0.0_r_single  ) dlon_earth_deg=dlon_earth_deg+360.0
                       if(dlon_earth_deg > 360.0_r_single) dlon_earth_deg=dlon_earth_deg-360.0
                       azm_earth = this%rwAzimuth
!                       azm_earth = 360.0_r_single+(90.0_r_single - this%rwAzimuth)
!                       if(azm_earth < 0.0_r_single  ) azm_earth=azm_earth+360.0
!                       if(azm_earth > 360.0_r_single) azm_earth=azm_earth-360.0
                       cosazm_earth=cos(azm_earth*deg2rad)
                       sinazm_earth=sin(azm_earth*deg2rad)
                       call map%rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth_deg,xc,yc)
                       azm=atan2(sinazm,cosazm)*rad2deg

                       azm=azm*deg2rad
                       tiltangle=this%rwtilt*deg2rad

                       cosazm  = cos(azm)  ! cos(azimuth angle)
                       sinazm  = sin(azm)  ! sin(azimuth angle)
                       costilt = cos(tiltangle) ! cos(tilt angle)
                       sintilt = sin(tiltangle) ! sin(tilt angle)
                       cosazm_costilt = cosazm*costilt
                       sinazm_costilt = sinazm*costilt

                       rwwind=uges*cosazm_costilt+vges*sinazm_costilt
                       if(this%include_w) then
                          rwwind=rwwind+wges*sintilt
                       end if

                       this%rw2d(i,iaz)=rwwind
                    else
                       write(*,*) 'WARNING: may have problem with rlocation:', rlocation
                       this%rw2d(i,iaz)=-999.0
                    endif
                 else
                    this%rw2d(i,iaz)=-999.0
                 endif
!                 write(*,*) '=======>',i,iaz,this%rw2d(i,iaz),rwdpqc%rw2d(i,iaz)
! check the results 
!                 rwdpqc%rw2d(i,iaz)=this%rw2d(i,iaz)
!
! for test              endif
           enddo
        enddo
!
        deallocate(rvalv)
!
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
        this%include_w=.false.
        if(allocated(this%rw2d)) deallocate(this%rw2d)
        allocate(this%rw2d(this%Gate,this%Azimuth))
        if(allocated(this%rw2d_hgt)) deallocate(this%rw2d_hgt)
        allocate(this%rw2d_hgt(this%Gate,this%Azimuth))
        if(allocated(this%vaduu)) deallocate(this%vaduu)
        allocate(this%vaduu(this%Gate))
        if(allocated(this%vadvv)) deallocate(this%vadvv)
        allocate(this%vadvv(this%Gate))
        this%rw2d=99999.0
        this%rw2d_hgt=-99999.0
        this%vaduu=-99999.0
        this%vadvv=-99999.0

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
        if(allocated(this%rw2d_hgt)) deallocate(this%rw2d_hgt)
        if(allocated(this%vaduu)) deallocate(this%vaduu)
        if(allocated(this%vadvv)) deallocate(this%vadvv)

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
