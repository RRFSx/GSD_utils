  subroutine write_NASALaRC_global_cloud(nxp,nyp,lat,lon,ptop,htop,teff,phase,lwp,cbase_time,time_offset,first)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   write_NASALaRC_global_cloud
!   prgmmr: Terra Ladwig           org: esrl/gsd                date: 2015-03-23
!   
! abstract: 
!           This code is based on write_bufr_NASALaRC
!           This routine writes raw NASA LaRC global obs into bufr
!   
! program history log:
!
!   input argument list:
!         nxp,nyp - dimensions of obs
!         lat,lon - latitude, longitude of obs
!         ptop - cloud top pressure
!         htop - cloud top height
!         teff - cloud effective temperature
!         phase - cloud phase
!         lwp_iwp - liquid or ice water path
!         cbase_time - units text from time_offset, indicates the date
!         time_offset - seconds since start of this day
!         first - true for first file is being written (creates new bufr file)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  linux 
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    implicit none

!
    integer(i_kind), intent(in) :: nxp,nyp
    REAL(r_single), intent(in) ::   lat(nxp,nyp)
    REAL(r_single), intent(in) ::   lon(nxp,nyp)
    REAL(r_single), intent(in) ::   ptop(nxp,nyp)
    REAL(r_single), intent(in) ::   htop(nxp,nyp)
    REAL(r_single), intent(in) ::   teff(nxp,nyp)
    REAL(r_single), intent(in) ::   phase(nxp,nyp)
    REAL(r_single), intent(in) ::   lwp(nxp,nyp)
    CHARACTER*40,   intent(in) ::   cbase_time
    REAL*8,         intent(in) ::   time_offset

    real(r_kind) :: hdr(6),obs(7)
    character(80):: hdrstr='YEAR  MNTH  DAYS HOUR  MINU  SECO'
    character(80):: obsstr='CLATH  CLONH CLDP HOCT CDTP EBBTH VILWC'
    ! CLDP     |  CLOUD PHASE
    ! HOCB     | HEIGHT OF BASE OF CLOUD
    ! HOCT     | HEIGHT OF TOP OF CLOUD             (METERS)
    ! CDBP     | PRESSURE AT BASE OF CLOUD
    ! CDTP     | PRESSURE AT TOP OF CLOUD           (PA)
    ! EBBTH    | EQUIVALENT BLACK BODY TEMPERATURE  (KELVIN)
    ! VILWC    | VERTICALLY-INTEGRATED LIQUID WATER CONTENT

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    integer(i_kind) :: ludx,lendian_in,idate

    INTEGER(i_kind)  ::  maxlvl, numref
    INTEGER(i_kind)  ::  i,j,n,k,iret
    LOGICAL :: first

    CHARACTER*40 :: ob_date
    INTEGER  :: ob_year, ob_month, ob_day, ob_hour, ob_minute
    CHARACTER*2 :: ob_hour_str, ob_minute_str

    ! analysis time
    if (first) then
      open(12,file='nasaLaRC_cycle_date')
        read(12,*) idate
      close(12)
      write(6,*) 'cycle time is : ', idate
    endif

    ! convert ob time
    read(cbase_time(15:18), '(i)') ob_year
    read(cbase_time(20:21), '(i)') ob_month
    read(cbase_time(23:24), '(i)') ob_day
    ob_hour = INT(time_offset/3600)
    if (ob_hour .lt. 10) then
      write(ob_hour_str, '(I1)') ob_hour
      ob_hour_str = "0"//ob_hour_str
    else
      write(ob_hour_str, '(I2)') ob_hour
    endif
    ob_minute = INT(MOD(time_offset,3600.)/60.)
    if (ob_minute .lt. 10) then
      write(ob_minute_str, '(I1)') ob_minute
      ob_minute_str = "0"//ob_minute_str
    else
      write(ob_minute_str, '(I2)') ob_minute
    endif
    ob_date = cbase_time(15:18)//cbase_time(20:21)//cbase_time(23:24)//ob_hour_str//ob_minute_str
    write(6,*) 'observation time is : ', ob_date

    hdr(1)=ob_year
    hdr(2)=ob_month
    hdr(3)=ob_day
    hdr(4)=ob_hour
    hdr(5)=ob_minute
    hdr(6)=00


    subset='NC012150'
    sid='NASALaRC'
    ludx=22
    lendian_in=10

    call datelen(10)
    open(ludx,file='prepobs_prep.bufrtable',action='read')
    if (first) then
      open(lendian_in,file='NASALaRCGlobalCloud.bufr',status="new",action='write',form='unformatted')
      call openbf(lendian_in,'OUT',ludx)
      write(6,*) 'Opening new bufr file'
    else
      open(lendian_in,file='NASALaRCGlobalCloud.bufr',status="old",position="append",action='write',form='unformatted')
      call openbf(lendian_in,'APX',ludx)
    endif

    maxlvl=0
    numref=0
    ! loop over obs
    do j=1,nyp
    do i=1,nxp
        numref = numref + 1

        if( lat(i,j) > 88888.0 ) CYCLE
        obs(1)=lat(i,j)

        if( lon(i,j) > 88888.0 ) CYCLE
        obs(2)=lon(i,j)
        maxlvl = maxlvl +1
    !    if (numref > 1222510) then
    !      write(6,*) obs(1), obs(2)
    !      write(6,*) maxlvl
    !      write(6,*) phase(i,j)
    !    endif

        if( phase(i,j) > 88888.0 .or. phase(i,j) < -0.001) then
          !obs(3)=9999.0
          obs(3)=0.0
        else
          if( phase(i,j) < 0.001) then
            obs(3)=0.0
          endif
        endif

        if( htop(i,j) > 88888.0 .or. htop(i,j) < -0.001) then
          obs(4)=9999.0
        else
          obs(4)=htop(i,j)
        endif

        if( ptop(i,j) > 88888.0 .or. ptop(i,j) < -0.001) then
          obs(5)=9999.0
        else
          obs(5)=ptop(i,j)
        endif

        if( teff(i,j) > 88888.0 .or. teff(i,j) < -0.001) then
          obs(6)=9999.0
        else
          obs(6)=teff(i,j)
        endif

        if( lwp(i,j) > 88888.0 .or. lwp(i,j) < -0.001) then
          obs(7)=9999.0
        else
          !obs(7)=lwp(i,j)*1000.0
          obs(7)=lwp(i,j)
        endif

  !      if (obs(7) > 132. .and. obs(7) .ne. 9999.0) then
  !        write(6,*) numref, maxlvl
  !        write(6,*) obs(7)
  !      endif
  
        !if (maxlvl > 970200) then
        !  write(6,*) numref, maxlvl
        !  write(6,*) obs
        !endif

        call openmb(lendian_in,subset,idate)
        call ufbint(lendian_in,hdr,6,   1,iret,hdrstr)
        call ufbint(lendian_in,obs,7,1,iret,obsstr)
        call writsb(lendian_in,ibfmsg,iret)
    !    write(6,*) 'bufr', numref, maxlvl
    enddo   !i
    enddo   !j
    call closbf(lendian_in)
    write(6,*) 'write_NASALaRC_global_cloud, DONE: write columns: ',numref
    write(6,*) 'write_NASALaRC_global_cloud, Number of bufr obs added: ', maxlvl

end subroutine  write_NASALaRC_global_cloud
