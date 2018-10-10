! module_lightning.f90
! Eric James
! NOAA/OAR/ESRL/GSD/ADB
! 14 Sep 2018
!
! This module defines lightning observation data structure and the method to
! read and write observations from and to those data structures.  It is used by
! process_Lightning.f90.

module module_lightning

   use kinds, only: r_kind, r_single, i_short, rmissing
   use module_lightning_base, only: lightningbase
   use module_map_utils, only: map_util
   use module_ncio, only: ncio

   implicit none

   public :: obs_lightning
   public :: lightning_type

   private
   type :: lightning_type
      character(len=6) :: datatype
      integer :: numvar, numStrike
      integer :: i_mult, i_sigStr, i_duration, i_height, i_energy
      character(len=20), allocatable :: varnames(:)
      character(len=20), allocatable :: latname
      character(len=20), allocatable :: lonname
      character(len=40), allocatable :: timename
      character(len=40), allocatable :: startname
      character(len=20), allocatable :: dimname
      integer, allocatable :: ltng_mult(:)
      real(r_single), allocatable :: ltng_sigStr(:)
      integer, allocatable :: ltng_duration(:)
      integer, allocatable :: ltng_height(:)
      integer(i_short), allocatable :: ltng_energy(:)
      real(r_single), allocatable :: ltng_lat(:)
      real(r_single), allocatable :: ltng_lon(:)
      real(r_kind), allocatable :: ltng_time(:)
      integer(i_short), allocatable :: glm_time(:)
      real(r_kind), allocatable :: glm_start(:)
      integer, allocatable :: ltng_quality(:)
      integer :: n_alloc=0
      integer :: idate, mm
      real(r_single) :: time_window  ! time_window in hour
   end type lightning_type

   type, extends(lightning_type) :: obs_lightning
      type(lightningbase), pointer :: head => NULL()
      type(lightningbase), pointer :: tail => NULL()
      contains
         procedure :: initial => initial_lightning
         procedure :: list_initial => list_initial_lightning
         procedure :: read_ltng => read_lightning
         procedure :: read_bufr => read_lightning_bufr
         procedure :: qc => qc_lightning
         procedure :: regrid => regrid_lightning
         procedure :: finish => finish_lightning
         procedure :: write_bufr => write_bufr_lightning
   end type obs_lightning

   type(ncio) :: ncdata
   type(map_util) :: map

   contains

      subroutine initial_lightning(this,itype)

!        This subroutine defines the number of variables and their names for
!        each lightning observation type.  The indices of the variables are
!        also defined for later reference.

         class(obs_lightning) :: this

         character(len=6), intent(in) :: itype

!        GLD360

         if (itype=='GLD360') then
            this%datatype = itype
            this%numvar = 3

            this%i_mult = 1
            this%i_sigStr = 2
            this%i_duration = 3
            this%i_height = 0
            this%i_energy = 0

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'mult'
            this%varnames(2) = 'sigStr'
            this%varnames(3) = 'strokeDuration'

            allocate(this%latname)
            allocate(this%lonname)
            allocate(this%timename)
            allocate(this%startname)
            this%latname = 'latitude'
            this%lonname = 'longitude'
            this%timename = 'time'

            allocate(this%dimname)
            this%dimname = 'strikeNum'

!        Earth Network Lightning Network

         else if (itype==' ENTLN') then
            this%datatype = itype
            this%numvar = 3

            this%i_mult = 1
            this%i_sigStr = 2
            this%i_duration = 0
            this%i_height = 3
            this%i_energy = 0

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'mult'
            this%varnames(2) = 'sigStr'
            this%varnames(3) = 'inCloudHeight'

            allocate(this%latname)
            allocate(this%lonname)
            allocate(this%timename)
            allocate(this%startname)
            this%latname = 'lat'
            this%lonname = 'lon'
            this%timename = 'time'

            allocate(this%dimname)
            this%dimname = 'strikeNum'

!        National Lightning Detection Network

         else if (itype=='  NLDN') then
            this%datatype = itype
            this%numvar = 2

            this%i_mult = 1
            this%i_sigStr = 2
            this%i_duration = 0
            this%i_height = 0
            this%i_energy = 0

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'mult'
            this%varnames(2) = 'sigStr'

            allocate(this%latname)
            allocate(this%lonname)
            allocate(this%timename)
            allocate(this%startname)
            this%latname = 'lat'
            this%lonname = 'lon'
            this%timename = 'time'

            allocate(this%dimname)
            this%dimname = 'strikeNum'

!        GOES Lightning Mapper

         else if (itype=='   GLM') then
            this%datatype = itype
            this%numvar = 1

            this%i_mult = 0
            this%i_sigStr = 0
            this%i_duration = 0
            this%i_height = 0
            this%i_energy = 1

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'flash_energy'

            allocate(this%latname)
            allocate(this%lonname)
            allocate(this%timename)
            allocate(this%startname)
            this%latname = 'flash_lat'
            this%lonname = 'flash_lon'
            this%timename = 'flash_time_offset_of_last_event'
            this%startname = 'product_time'

            allocate(this%dimname)
            this%dimname = 'number_of_flashes'

!        BLM Alaska lightning observations

         else if (itype=='    AK') then
            this%datatype = itype
            this%numvar = 1

            this%i_mult = 0
            this%i_sigStr = 1
            this%i_duration = 0
            this%i_height = 0
            this%i_energy = 0

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'sigStr'

            allocate(this%latname)
            allocate(this%lonname)
            allocate(this%timename)
            allocate(this%startname)
            this%latname = 'latitude'
            this%lonname = 'longitude'
            this%timename = 'time'

            allocate(this%dimname)
            this%dimname = 'strikeNum'

!        NCEP lightning BUFR data

         else if (itype=='  BUFR') then
            this%datatype = itype
            this%numvar = 3

            this%i_mult = 1
            this%i_sigStr = 2
            this%i_duration = 3

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'multiplicity'
            this%varnames(2) = 'signal strength (kA)'
            this%varnames(3) = 'duration (micro s)'

            allocate(this%latname)
            allocate(this%lonname)
            allocate(this%timename)
            allocate(this%startname)

            allocate(this%dimname)
            this%dimname = 'strikeNum'

!        If the data type does not match one of the known types, exit.

         else
            write(*,*) 'Unknown data type:', itype
            stop 1234
         end if

         this%head => NULL()
         this%tail => NULL()

         write(*,*) 'Finished initial_lightning'

      end subroutine initial_lightning

      subroutine list_initial_lightning(this)

!        This subroutine lists the setup for lightning data that was done by
!        the initial_lightning subroutine.

         class(obs_lightning) :: this

         integer :: k

         write(*,*) 'List initial setup for ', this%datatype
         write(*,*) 'number of variables ', this%numvar
         write(*,*) 'variable index: mult, sigStr, duration, height, energy'
         write(*,'(15x,10I3)') this%i_mult, this%i_sigStr, this%i_duration, &
      &      this%i_height, this%i_energy
         write(*,*) 'variable name:'
         do k=1,this%numvar
            write(*,*) k,trim(this%varnames(k))
         enddo

         write(*,*) 'Finished list_initial_lightning'

      end subroutine list_initial_lightning

      subroutine read_lightning(this,filename,itype,numtype,savelon,savelat)

!        This subroutine initializes arrays to receive the lightning obs,
!        and opens the file and gets the data.

         class(obs_lightning) :: this

         character(len=6), intent(in) :: itype
         character(len=*), intent(in) :: filename

         integer, intent(inout) :: numtype
         real(r_single), intent(inout) :: savelon(:), savelat(:)
         integer :: i, num

!        Open the file using module_ncio.f90 code, and find the number of
!        lightning strikes

         call ncdata%open(trim(filename),'r',200)
         call ncdata%get_dim(this%dimname,this%numStrike)

         write(*,*) 'number of strikes for file ', filename, this%numStrike

         numtype = numtype + this%numStrike

!        Allocate all the arrays to receive data

         allocate(this%ltng_mult(this%numStrike))
         allocate(this%ltng_sigStr(this%numStrike))
         allocate(this%ltng_duration(this%numStrike))
         allocate(this%ltng_height(this%numStrike))
         allocate(this%ltng_energy(this%numStrike))
         allocate(this%ltng_lat(this%numStrike))
         allocate(this%ltng_lon(this%numStrike))
         allocate(this%ltng_time(this%numStrike))
         allocate(this%glm_time(this%numStrike))
         allocate(this%glm_start(1))

!        Get variables from the lightning file, but only if the variable is
!        defined for that lightning observation type.
!        There are a couple of additional if statements to handle:
!        (1) If numStrike=1, this indicates an empty file (probably AK)
!        (2) GLM time variable is a different type than the other data sources:
!            Time is relative to a reference time "glm_start", which itself is
!            exactly 8 years later than the reference time for the other types.

         if (this%numStrike .gt. 1) then

            call ncdata%get_var(this%latname,this%numStrike,this%ltng_lat)
            call ncdata%get_var(this%lonname,this%numStrike,this%ltng_lon)
            if (itype .eq. '   GLM') then
               call ncdata%get_var(this%timename,this%numStrike,this%glm_time)
               call ncdata%get_var(this%startname,1,this%glm_start)
            else
               call ncdata%get_var(this%timename,this%numStrike,this%ltng_time)
            end if
            if (this%i_mult .gt. 0) then
               call ncdata%get_var(this%varnames(this%i_mult),this%numStrike,  &
                                   this%ltng_mult)
            end if
            if (this%i_sigStr .gt. 0) then
               call ncdata%get_var(this%varnames(this%i_sigStr),this%numStrike,  &
                                   this%ltng_sigStr)
            end if
            if (this%i_duration .gt. 0) then
               call ncdata%get_var(this%varnames(this%i_duration),this%numStrike,  &
                                   this%ltng_duration)
            end if
            if (this%i_height .gt. 0) then
               call ncdata%get_var(this%varnames(this%i_height),this%numStrike,  &
                                   this%ltng_height)
            end if
            if (this%i_energy .gt. 0) then
               call ncdata%get_var(this%varnames(this%i_energy),this%numStrike,  &
                                   this%ltng_energy)
            end if

!           Write out the lat/lon/time of lightning strikes

            if (itype .eq. '   GLM') then
               do i=1,this%numStrike
                  write(*,*) i, this%ltng_lon(i), this%ltng_lat(i), this%glm_time(i)
                  num = numtype - this%numStrike + i
                  savelon(num) = this%ltng_lon(i)
                  savelat(num) = this%ltng_lat(i)
               enddo
            else
               do i=1,this%numStrike
                  write(*,*) i, this%ltng_lon(i), this%ltng_lat(i), this%ltng_time(i)
                  num = numtype - this%numStrike + i
                  savelon(num) = this%ltng_lon(i)
                  savelat(num) = this%ltng_lat(i)
               enddo
            end if

         end if

!        Close the netCDF file.

         call ncdata%close

         write(*,*) 'Finished read_lightning'

      end subroutine read_lightning

      subroutine read_lightning_bufr(this,filename,maxsave,cdateana,minute,trange_start,trange_end,&
                                     numtype,savelon,savelat)

!        This subroutine initializes arrays to receive BUFR lightning obs,
!        and opens the file and gets the data. A time window check is included
!        here which greatly reduces the amount of obs considered, and speeds up
!        the code.

         class(obs_lightning) :: this

         character(len=*), intent(in) :: filename
         character(len=10), intent(in) :: cdateana
         integer, intent(in) :: maxsave, minute
         real, intent(in) :: trange_start, trange_end

         integer, intent(inout) :: numtype
         real(r_single), intent(inout) :: savelon(:), savelat(:)

         character(8) :: subset
         character(80) :: hdstr='YEAR  MNTH  DAYS  HOUR  MINU CLATH  CLONH'
         character(80) :: obstr='AMPLS  PLRTS  OWEP  NOFL'

         integer, parameter :: mxmn=35, mxlv=1
         integer :: ireadmg, ireadsb
         integer :: unit_in, idate, nmsg, ntb, ntball
         integer :: minobs, minan, timediffmin
         integer :: i, k, iret
         integer, dimension(5) :: idate5
         integer :: itime(maxsave), istrike(maxsave), isignal(maxsave)

         real(8) :: hdr(mxmn),obs(mxmn,mxlv)

         real(r_single) :: savetime(maxsave)

!        Find the analysis time in minutes relative to historic date.

         read(cdateana,'(I4,I2,I2,I2)') (idate5(i),i=1,4)
         idate5(5) = minute
         call w3fs21(idate5,minan)
         write(*,*) 'Analysis time=',idate5,minan

!        Open the BUFR file and start to read it.

         unit_in = 10

         open(unit_in,file=trim(filename),form='unformatted',status='old')
         call openbf(unit_in,'IN',unit_in)
         call datelen(10)

         nmsg = 0
         ntb = 0
         ntball = 0

         msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
            nmsg = nmsg + 1
            sb_report: do while (ireadsb(unit_in) == 0)
               ntball = ntball + 1
               call ufbint(unit_in,hdr,mxmn,1,iret,hdstr)
               call ufbint(unit_in,obs,mxmn,1,iret,obstr)

               idate5(1) = int(hdr(1))
               idate5(2) = int(hdr(2))
               idate5(3) = int(hdr(3))
               idate5(4) = int(hdr(4))
               idate5(5) = int(hdr(5))
               call w3fs21(idate5,minobs)

!              Add obs reference time, then subtract analysis time to get obs
!              time relative to analysis

               timediffmin = minobs - minan
               if(timediffmin > trange_start .and. timediffmin < trange_end ) then
                  ntb = ntb + 1
                  if(ntb > maxsave) then
                     write(*,*) 'Too many observations, need to increase ',&
                                ' maxsave!'
                     stop 12345
                  endif
                  if(int(hdr(1)) > 2000) then
                     itime(ntb) = (int(hdr(1)) - 2000)*100000000
                  else
                     itime(ntb) = (int(hdr(1)) - 1900)*100000000
                  endif
                  itime(ntb) = itime(ntb) + int(hdr(2))*1000000 + &
                             int(hdr(3))*10000 + int(hdr(4))*100 + int(hdr(5))
                  savelat(ntb) = hdr(6)
                  savelon(ntb) = hdr(7)
                  savetime(ntb) = (minobs*60.0) + (2922.0*24.0*3600.0)
                  isignal(ntb) = int(obs(1,1))
                  if(subset=='NC007001') then ! This is short-range Vaisala obs
                     if(obs(1,1) > 0.0) istrike(ntb) = int(obs(4,1))
                  elseif(subset=='NC007002') then ! This is long-range Vaisala obs
                     if(obs(1,1) > 0.0) istrike(ntb) = 1 ! obs(4,1)=0 for NC007002
                  else
                     write(*,*) 'Unknown lightning type ', subset
                     stop 1234
                  endif   ! lightning data type
               endif  ! check time
            enddo sb_report
         enddo msg_report

         call closbf(unit_in)

         this%numStrike = ntb
         write(*,*) 'There are ',this%numStrike,' out of total ',ntball,&
                    ' lightning observations in time window ',trange_start,&
                    ' to ',trange_end, ' minute'

!        Allocate all the arrays to receive data

         allocate(this%ltng_mult(this%numStrike))
         allocate(this%ltng_sigStr(this%numStrike))
         allocate(this%ltng_duration(this%numStrike))
         allocate(this%ltng_height(this%numStrike))
         allocate(this%ltng_energy(this%numStrike))
         allocate(this%ltng_lat(this%numStrike))
         allocate(this%ltng_lon(this%numStrike))
         allocate(this%ltng_time(this%numStrike))
         allocate(this%glm_time(this%numStrike))
         allocate(this%glm_start(1))

         do i=1,this%numStrike
            this%ltng_mult(i) = istrike(i)
            this%ltng_sigStr(i) = isignal(i)
            this%ltng_lat(i) = savelat(i)
            this%ltng_lon(i) = savelon(i)
            this%ltng_time(i) = savetime(i)
         enddo

         numtype = numtype + this%numStrike

      end subroutine read_lightning_bufr

      subroutine qc_lightning(this,itype,cdateana,minute,trange_start,trange_end)

!        This subroutine performs a crude quality check and assigns a quality
!        flag to each lightning strike.

         class(obs_lightning) :: this

         character(len=6), intent(in) :: itype
         character(len=10), intent(in) :: cdateana

         integer, intent(in) :: minute
         real, intent(in) :: trange_start, trange_end

         integer, dimension(5) :: idate5

         integer :: minan, i, j
         real(r_kind) :: secan, start_sec, end_sec

         real(r_single) :: savetime

         allocate(this%ltng_quality(this%numStrike))

         this%ltng_quality = 0    ! 0 good data,  > 0 bad data

!        Find the analysis time in minutes relative to historic date
!        (00 UTC 01 Jan 1978).  We need to convert this to seconds since
!        00 UTC 01 Jan 1970 to be consistent with actual obs time.

         read(cdateana,'(I4,I2,I2,I2)') (idate5(i),i=1,4)
         idate5(5) = minute
         call w3fs21(idate5,minan)
         write(*,*) 'Analysis time=',idate5,minan
         secan = minan*60.0 + 252460800.0
         start_sec = secan + trange_start*60.0
         end_sec = secan + trange_end*60.0

!        Check for duplicates if numStrike is greater than 1

         write(*,*) 'Sample anx time: ', secan
         write(*,*) 'Sample start window: ', start_sec
         write(*,*) 'Sample end window: ', end_sec

         if (this%numStrike .gt. 1) then

            do i=2,this%numStrike
               do j=1,i-1
                  if (abs(this%ltng_lon(i) - this%ltng_lon(j)) < 0.001 .and.  &
                      abs(this%ltng_lat(i) - this%ltng_lat(j)) < 0.001 .and.  &
                      this%ltng_time(i) == this%ltng_time(j)) then
                     this%ltng_quality(i) = 1
                  end if
               end do
            end do

!           Check that multiplicity >= 0

            if (this%i_mult .gt. 0) then
               do i=1,this%numStrike
                  if (this%ltng_mult(i) < 0) then
                     this%ltng_quality(i) = 1
                  end if
               end do
            end if

!           Check that the strike falls within the time window

            if (itype .eq. '   GLM') then
               do i=1,this%numStrike
                  savetime = (this%glm_time(i))/1000.0 + this%glm_start(1) + 946728000.0
                  if (savetime < start_sec .or. savetime > end_sec) then
                     this%ltng_quality(i) = 1
                  end if
               end do
            else
               do i=1,this%numStrike
                  if (this%ltng_time(i) < start_sec .or. &
                      this%ltng_time(i) > end_sec) then
                     this%ltng_quality(i) = 1
                  end if
               end do
            end if

         end if

         write(*,*) 'Finished qc_lightning'

      end subroutine qc_lightning

      subroutine regrid_lightning(this,lightning_array,numtype,nlon,nlat)

!        This subroutine interpolates the lightning data to the model grid.

         class(obs_lightning) :: this

         integer, intent(in) :: nlon, nlat
         integer, intent(inout) :: numtype
         integer :: i, igrid, jgrid

         real(r_single), intent(inout) :: lightning_array(:,:,:)
         real(r_single) :: rlon, rlat, xc, yc
         real(r_kind) :: deg2rad, pi, one

         one = 1.0
         pi = acos(-one)
         deg2rad = pi/180.0

         if (this%numStrike .gt. 1) then

            do i=1,this%numStrike
               if(this%ltng_quality(i) == 0 ) then
                  rlon = this%ltng_lon(i)*deg2rad
                  rlat = this%ltng_lat(i)*deg2rad
                  call map%tll2xy(rlon,rlat,xc,yc)

                  igrid = int(xc+0.5)
                  jgrid = int(yc+0.5)

                  if((igrid > 0 .and. igrid < nlon) .and.  &
                     (jgrid > 0 .and. jgrid < nlat)) then
                     lightning_array(1,igrid,jgrid) = lightning_array(1,igrid,  &
                                        jgrid) + 1
!                     if (this%i_mult .gt. 0) then
!                        lightning_array(1,igrid,jgrid) = &
!                                        this%ltng_mult(i)
!                     endif
                     if (this%i_sigStr .gt. 0) then
                        lightning_array(2,igrid,jgrid) = &
                                        this%ltng_sigStr(i)
                     endif
                     if (this%i_duration .gt. 0) then
                        lightning_array(3,igrid,jgrid) = &
                                        this%ltng_duration(i)
                     endif
                     if (this%i_height .gt. 0) then
                        lightning_array(4,igrid,jgrid) = &
                                        this%ltng_height(i)
                     endif
                     if (this%i_energy .gt. 0) then
                        lightning_array(5,igrid,jgrid) = &
                                        this%ltng_energy(i)
                     endif
                     numtype = numtype + 1
                  endif
               endif
            enddo

         end if

         write(*,*) 'Finished regrid_lightning'

      end subroutine regrid_lightning

      subroutine finish_lightning(this)

         class(obs_lightning) :: this

         type(lightningbase), pointer :: thisobs,thisobsnext

         deallocate(this%varnames)
         deallocate(this%latname)
         deallocate(this%lonname)
         deallocate(this%timename)
         deallocate(this%startname)
         deallocate(this%dimname)
         deallocate(this%ltng_mult)
         deallocate(this%ltng_sigStr)
         deallocate(this%ltng_duration)
         deallocate(this%ltng_height)
         deallocate(this%ltng_energy)
         deallocate(this%ltng_lat)
         deallocate(this%ltng_lon)
         deallocate(this%ltng_time)
         deallocate(this%glm_time)
         deallocate(this%glm_start)
         deallocate(this%ltng_quality)

         thisobs => this%head
         if(.NOT.associated(thisobs)) then
            write(*,*) 'No memory to release'
            return
         endif
         do while(associated(thisobs))
!            write(*,*) 'destroy ==',thisobs%name

            thisobsnext => thisobs%next
            call thisobs%destroy()
            thisobs => thisobsnext
         enddo

         write(*,*) 'Finished finish_lightning'

      end subroutine finish_lightning

      subroutine write_bufr_lightning(this,maxlvl,nlon,nlat,numref,lightning_out,idate)

         class(obs_lightning) :: this

         character(8) :: subset, sid
         character(80) :: hdrstr='SID XOB YOB DHR TYP'
         character(80) :: obsstr='POB'

         integer, parameter :: mxbf = 160000
         integer :: ibfmsg = mxbf/4
         integer :: ludx, lendian_in, idate
         integer, intent(in) :: maxlvl, nlon, nlat, numref
         integer :: numlvl, i, n, k, iret

         real(r_kind), intent(in) :: lightning_out(maxlvl+2,nlon*nlat)
         real(r_kind) :: hdr(5),obs(1,35)

         subset = 'ADPUPA'
         sid = 'LIGHTNI'
         ludx = 22
         lendian_in = 10

         open(ludx,file='prepobs_prep.bufrtable',action='read')
         open(lendian_in,file='LightningInGSI.bufr',action='write',form='unformatted')

         call datelen(10)
         call openbf(lendian_in,'OUT',ludx)

         do n=1,numref
            hdr(1)=transfer(sid,hdr(1))
            hdr(2)=lightning_out(1,n)/10.0
            hdr(3)=lightning_out(2,n)/10.0
            hdr(4)=0
            hdr(5)=500

            do k=1,maxlvl
               obs(1,k)=lightning_out(2+k,n)
            enddo

            call openmb(lendian_in,subset,idate)
            call ufbint(lendian_in,hdr,5,   1,iret,hdrstr)
            call ufbint(lendian_in,obs,1,maxlvl,iret,obsstr)
            call writsb(lendian_in,ibfmsg,iret)
         enddo

         call closbf(lendian_in)
         write(6,*) 'write_bufr_nsslref, DONE: write columns:',numref

      end subroutine write_bufr_lightning

end module module_lightning
