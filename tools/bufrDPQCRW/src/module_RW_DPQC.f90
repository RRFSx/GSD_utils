module module_RW_DPQC
!
! read in radial wind from DPQC netcdf file and
!     write out as BUFR
!
!  use kinds, only: r_kind,r_single
  use module_ncio, only : ncio
  use module_time, only : mtime
  use module_radar_station_config, only : rdrsta_config

  implicit none
  type(ncio)  :: ncrwin
  type(mtime) :: mt

  public :: rw_dpqc
! set default to private
  private

  type :: rw_dpqc

     character(len=180) :: crwfile
     integer :: Azimuth,Gate
     real    :: Elevation,RangeToFirstGate
     real    :: Latitude,Longitude,Height
     integer :: Time
     integer :: iyy,imm,idd,ihh,imin,iss
     integer :: ivdd,ivhh,ivmin
     character(len=4)  :: radarvName
     character(len=20)  :: datatype 
     real    :: rvcp_value
     real    :: rUnambiguous_Range
     real    :: GateWidth
     character(len=4)  :: radarName
     real, allocatable :: rwAzimuth(:)
     real, allocatable :: NyquistV(:)
     real, allocatable :: rw2d(:,:)
     real, allocatable :: rw1d(:)

     integer :: pixel
  contains
     procedure ::  readnc  => read_rw_dpqc
     procedure ::  wrtbufr => write_rw2bufr
     procedure ::  scanid  => get_scan_id
     procedure ::  destroy => destroy_rw_dpqc
  end type rw_dpqc


  contains

     function get_scan_id(this) result(id)
! from elevation and VCP to decide scan id
        implicit none
        class(rw_dpqc),intent(in) :: this
        integer :: id

        real :: VCP11(7)
        real :: VCP12(14)
        real :: VCP21(9)
        real :: VCP221(9)
        real :: VCP212(14)
        real :: VCP215(15)
        real :: VCP121(9)
        real :: VCP35(9)
        real :: VCP31(5)
        real :: VCP32(6)
!
        integer :: ivcp
        integer :: i
        real    :: elevation 
!
        VCP11(1)  = 0.5
        VCP11(2)  = 1.45
        VCP11(3)  = 2.4
        VCP11(4)  = 3.35
        VCP11(5)  = 4.30
        VCP11(6)  = 5.25
        VCP11(7)  = 6.2
!
        VCP221(1) = 0.5
        VCP221(2) = 1.45
        VCP221(3) = 2.4
        VCP221(4) = 3.4
        VCP221(5) = 4.3
        VCP221(6) = 6.0
        VCP221(7) = 9.9
        VCP221(8) = 14.6
        VCP221(9) = 19.5
        VCP21=VCP221

        VCP31(1) = 0.5
        VCP31(2) = 1.5
        VCP31(3) = 2.4
        VCP31(4) = 3.4
        VCP31(5) = 4.3

        VCP32(1) = 0.2
        VCP32(2) = 0.5
        VCP32(3) = 1.5
        VCP32(4) = 2.4
        VCP32(5) = 3.4
        VCP32(6) = 4.3
!
        VCP35(1)=0.5
        VCP35(2)=0.9
        VCP35(3)=1.3
        VCP35(4)=1.8
        VCP35(5)=2.4
        VCP35(6)=3.1
        VCP35(7)=4.0
        VCP35(8)=5.1
        VCP35(9)=6.4
!
        VCP212(1)=0.5
        VCP212(2)=0.9
        VCP212(3)=1.3
        VCP212(4)=1.8
        VCP212(5)=2.4
        VCP212(6)=3.1
        VCP212(7)=4.0
        VCP212(8)=5.1
        VCP212(9)=6.4
        VCP212(10)=8.0
        VCP212(11)=10.0
        VCP212(12)=12.5
        VCP212(13)=15.6
        VCP212(14)=19.5
        VCP12=VCP212
!
        VCP215(1)=0.5
        VCP215(2)=0.9
        VCP215(3)=1.3
        VCP215(4)=1.8
        VCP215(5)=2.4
        VCP215(6)=3.1
        VCP215(7)=4.0
        VCP215(8)=5.1
        VCP215(9)=6.4
        VCP215(10)=8.0
        VCP215(11)=10.0
        VCP215(12)=12.0
        VCP215(13)=14.0
        VCP215(14)=16.7
        VCP215(15)=19.5
!        
        VCP121(1)=0.5
        VCP121(2)=1.5
        VCP121(3)=2.4
        VCP121(4)=3.4
        VCP121(5)=4.3
        VCP121(6)=6.0
        VCP121(7)=9.9
        VCP121(8)=14.6
        VCP121(9)=19.5
!
        ivcp=int(this%rvcp_value)
        elevation=this%Elevation
        id=0
        if( ivcp == 12 .or. ivcp==212 ) then
           do i=1,14
              if(abs(VCP212(i)-elevation) < 0.02) id=i
           enddo
        elseif( ivcp == 121 ) then
           do i=1,9
              if(abs(VCP121(i)-elevation) < 0.22) id=i
           enddo
        elseif( ivcp == 215 ) then
           do i=1,15
              if(abs(VCP215(i)-elevation) < 0.02) id=i
           enddo
        elseif( ivcp == 31 ) then
           do i=1,5
              if(abs(VCP31(i)-elevation) < 0.22) id=i
           enddo
        elseif( ivcp == 32 ) then
           do i=1,2
              if(abs(VCP32(i)-elevation) < 0.12) id=i
           enddo
           do i=3,6
              if(abs(VCP32(i)-elevation) < 0.22) id=i
           enddo
        elseif( ivcp == 21 ) then
           do i=1,9
              if(abs(VCP21(i)-elevation) < 0.22) id=i
           enddo
        elseif( ivcp == 221 ) then
           do i=1,9
              if(abs(VCP221(i)-elevation) < 0.22) id=i
           enddo
        elseif( ivcp == 35 ) then
           do i=1,9
              if(abs(VCP35(i)-elevation) < 0.12) id=i
           enddo
        elseif( ivcp == 11 ) then
           do i=1,7
              if(abs(VCP11(i)-elevation) < 0.22) id=i
           enddo
        else
           write(*,*) "error, not a VCP we know"
           stop 1234
        endif

        if(id<=0 .or. id > 21) then
           write(*,*) 'error, cannot find scan ID'
           write(*,*) 'VCP=',ivcp,' elevation=',elevation
           stop 1234
        endif

     end function 
!
     subroutine write_rw2bufr(this,cwrtfile,rscf)
!
!  write DPQC radial wind to BUFR file so they
!      can be used in GSI
!
! MNEMONIC used in this code
!    SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW
!    YEAR MNTH DAYS HOUR MINU SECO
!    SCID HNQV VOCP VOID
!    DIST125M DMVR DVSW
! Please refer BUFR table for explanations of MNEMONIC
!
!

        implicit none

        class(rw_dpqc),intent(in) :: this
        character(len=*),intent(in)  :: cwrtfile
        type(rdrsta_config),intent(in) :: rscf

        integer, parameter :: mxmn=10, mxlv=1500
        character(80):: hdstr= 'SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW'
        character(80):: hdstr2='YEAR MNTH DAYS HOUR MINU SECO'
        character(80):: hdstr3='SCID HNQV VOCP VOID'
        character(80):: obstr='DIST125M DMVR DVSW'
        real(8) :: hdr(mxmn),hdr2(mxmn),hdr3(mxmn),obs(3,mxlv)

        INTEGER        :: ireadmg,ireadsb

        character(8)   :: subset
        integer        :: unit_in=10,unit_out=20,unit_table=30
        integer        :: idate

        character(8)   :: c_sid
        real(8)        :: rstation_id
        equivalence(rstation_id,c_sid)

        integer        :: numrwbin
        integer        :: i,id,iaz,iret
!
        integer          :: valid_time
        character(len=8) :: subset2
        logical          :: file_exists
!
!
!
        open(unit_table,file='bufr_radar.table')
        inquire(file=trim(cwrtfile), exist=file_exists)
        if(file_exists) then
           open(unit_out,file=trim(cwrtfile),form='unformatted',status='old')
           call openbf(unit_out,'APN',unit_table)
        else
           open(unit_out,file=trim(cwrtfile),form='unformatted',status='new')
           call openbf(unit_out,'OUT',unit_table)
        endif
        call datelen(10)

        hdr=10.0e+10
        hdr2=10.0e+10
        hdr3=10.0e+10

        !RangeToFirstGate
        c_sid=this%radarName
        hdr(1)=rstation_id
        hdr(2)=this%Latitude
        hdr(3)=this%Longitude
        hdr(4)=this%Height        ! HEIGHT OF STATION GROUND ABOVE MSL
        hdr(5)=this%Height        ! HEIGHT OF ANTENNA ABOVE GROUND
        hdr(6)=this%Elevation
        hdr(8)=1.0

        id=rscf%findid(this%radarName)
        if(id /= 0) then
           hdr(4)=rscf%hsmsl(id)     ! HEIGHT OF STATION GROUND ABOVE MSL
           hdr(5)=rscf%hsalg(id)     ! HEIGHT OF ANTENNA ABOVE GROUND
        endif
        
        hdr2(1)=this%iyy
        hdr2(2)=this%imm
        hdr2(3)=this%idd
        hdr2(4)=this%ihh
        hdr2(5)=this%imin
        hdr2(6)=this%iss
        valid_time=this%iyy*1000000 + this%imm*10000 + &
                   this%idd*100 + this%ihh

        hdr3(1)=this%scanid()
        hdr3(3)=this%rvcp_value
        hdr3(4)=this%ivdd*10000+this%ivhh*100+this%ivmin
!
! set the report subtype based on the report hour - see the bufrtab.006  
! for the hour windows
! 
        subset2(1:6) = 'NC0060'
        WRITE (UNIT=subset2(7:8),FMT='(I2)') this%ihh + 10
  wrtrw:do iaz=1, this%Azimuth
!  wrtrw:do iaz=1, 5
          obs=10.0e+10
          hdr(7)=this%rwAzimuth(iaz)
          hdr3(2)=this%NyquistV(iaz)
          numrwbin=0
          do i=1,this%Gate
             if(this%rw2d(i,iaz) > -500.0 .and. this%rw2d(i,iaz) < 500.0) then  ! missing value -99900
               numrwbin=numrwbin+1
               obs(1,numrwbin)=(this%RangeToFirstGate+this%GateWidth*(i-1))/125.0
               obs(2,numrwbin)=this%rw2d(i,iaz)
!              obs(3,i)=2.5
             endif
          enddo
          if(numrwbin > 0 ) then
             call openmb(unit_out,subset2,valid_time)
             call ufbint(unit_out,hdr ,8,1  ,iret,hdstr)
             call ufbint(unit_out,hdr2,6,1  ,iret,hdstr2)
             call ufbint(unit_out,hdr3,4,1  ,iret,hdstr3)
             call ufbint(unit_out,obs ,3,  numrwbin,iret,obstr)
             call writsb(unit_out)
          endif
        enddo wrtrw
!
!        call writsb(unit_out)
        call closbf(unit_out)
        close(unit_table)
        close(unit_in)

     end subroutine write_rw2bufr

     subroutine read_rw_dpqc(this,crwfile)
!
!  read in DPQC netcdf file
!
        implicit none

        class(rw_dpqc),intent(inout) :: this
        character(len=*),intent(in)  :: crwfile
    
        character(len=10) :: vcp_value
        character(len=3)  :: Unambiguous_Range

        integer, allocatable :: ifld1d(:)
        integer :: minutes

        real, allocatable :: rw1d(:)
        integer(2), allocatable :: pixel_x(:)
        integer(2), allocatable :: pixel_y(:)
        integer, allocatable :: pixel_count(:)
        logical :: ifpixel
        integer :: i,ii,jj

        ifpixel=.false.
        
        this%crwfile=trim(crwfile)

        call ncrwin%open(trim(crwfile),"r",0)
        call ncrwin%get_dim("Azimuth",this%Azimuth)
        call ncrwin%get_dim("Gate",this%Gate)
        write(*,*) 'Azimuth,Gate    =',this%Azimuth,this%Gate
        call ncrwin%get_att("Elevation",this%Elevation)
        write(*,*) 'Elevation(deg)  =',this%Elevation
        call ncrwin%get_att("RangeToFirstGate",this%RangeToFirstGate)
        write(*,*) 'RangeToFirstGate=',this%RangeToFirstGate
        call ncrwin%get_att("Latitude",this%Latitude)
        write(*,*) 'Latitude        =',this%Latitude
        call ncrwin%get_att("Longitude",this%Longitude)
        write(*,*) 'Longitude       =',this%Longitude
        call ncrwin%get_att("Height",this%Height)
        write(*,*) 'Height          =',this%Height
        call ncrwin%get_att("Time",this%Time)
        write(*,*) 'Time            =',this%Time
        call ncrwin%get_att("vcp-value",vcp_value)
        read(vcp_value,'(f10.5)') this%rvcp_value
        write(*,*) 'vcp_value       =',vcp_value,this%rvcp_value
        call ncrwin%get_att("Unambiguous_Range-value",Unambiguous_Range)
        read(Unambiguous_Range,'(f3.0)') this%rUnambiguous_Range
        write(*,*) 'Unambiguous_Range=',Unambiguous_Range,this%rUnambiguous_Range
        call ncrwin%get_att("radarName-value",this%radarName)
        write(*,*) 'radarName       =',this%radarName
        call ncrwin%get_att("DataType",this%datatype)
        write(*,*) 'data type       =',this%datatype
!
!        if(int(this%rvcp_value) == 35 ) ifpixel=.true.
!        if(int(this%rvcp_value) == 212) ifpixel=.true.
        if(trim(this%datatype) == "SparseRadialSet") ifpixel=.true.
         
        minutes=this%Time/60
        this%iss=this%Time-minutes*60
        call mt%mins2date(minutes,this%iyy,this%imm,this%idd,this%ihh,this%imin)
        write(*,'(a,I6,5I3)') 'yy,mm,dd,hh,min,ss=', &
            this%iyy,this%imm,this%idd,this%ihh,this%imin,this%iss
!
        allocate(ifld1d(this%Azimuth))

        allocate(this%rwAzimuth(this%Azimuth))
        allocate(this%NyquistV(this%Azimuth))
        allocate(this%rw2d(this%Gate,this%Azimuth))
        this%rw2d=999.0

        if(ifpixel) then
           call ncrwin%get_dim("pixel",this%pixel)
           write(*,*) 'pixel=',this%pixel
           if(this%pixel > 0) then
              allocate(pixel_x(this%pixel))
              allocate(pixel_y(this%pixel))
              allocate(pixel_count(this%pixel))
              call ncrwin%get_var("pixel_x",this%pixel,pixel_x)
              call ncrwin%get_var("pixel_y",this%pixel,pixel_y)
              call ncrwin%get_var("pixel_count",this%pixel,pixel_count)
              allocate(rw1d(this%pixel))
              call ncrwin%get_var("AliasedVelocityDPQC",this%pixel,rw1d)
              this%rw2d=-999.0
              do i=1,this%pixel
                 ii=max(1,min(pixel_y(i)+1,this%Gate))
                 jj=max(1,min(pixel_x(i)+1,this%Azimuth))
                 this%rw2d(ii,jj)=rw1d(i)
              enddo
              deallocate(pixel_x)
              deallocate(pixel_y)
              deallocate(pixel_count)
              deallocate(rw1d)
           endif
        else
           call ncrwin%get_var("AliasedVelocityDPQC",this%Gate,this%Azimuth,this%rw2d)
        endif
!
        call ncrwin%get_var("BeamWidth",this%Azimuth,this%rwAzimuth)
        call ncrwin%get_var("AzimuthalSpacing",this%Azimuth,this%rwAzimuth)
        call ncrwin%get_var("GateWidth",this%Azimuth,this%rwAzimuth)
        this%GateWidth=this%rwAzimuth(1)
!        call ncrwin%get_var("RadialTime",this%Azimuth,ifld1d)

        call ncrwin%get_var("Azimuth",this%Azimuth,this%rwAzimuth)
        call ncrwin%get_var("NyquistVelocity",this%Azimuth,this%NyquistV)
        call ncrwin%close()
 
        if(this%scanid()==1 .or. this%radarvName /= this%radarName) then
           this%ivdd=this%idd 
           this%ivhh=this%ihh 
           this%ivmin=this%imin 
           this%radarvName=this%radarName
           write(*,*) 'start a new volume:',this%idd,this%ihh,this%imin,&
                                            this%radarName
        endif

        deallocate(ifld1d)

     end subroutine read_rw_dpqc

     subroutine destroy_rw_dpqc(this)
!
!  release memory
!
        implicit none

        class(rw_dpqc),intent(inout) :: this

        this%crwfile=''
        this%Azimuth=0
        this%Gate=0
        this%Elevation=0.0
        this%RangeToFirstGate=0.0
        this%Latitude=0.0
        this%Longitude=0.0
        this%Height=0.0
        this%Time=0
        this%rvcp_value=0.0
        this%rUnambiguous_Range=0.0
        this%radarName=''

        if(allocated(this%rwAzimuth)) deallocate(this%rwAzimuth)
        if(allocated(this%NyquistV)) deallocate(this%NyquistV)
        if(allocated(this%rw2d)) deallocate(this%rw2d)

     end subroutine destroy_rw_dpqc

End module module_RW_DPQC
