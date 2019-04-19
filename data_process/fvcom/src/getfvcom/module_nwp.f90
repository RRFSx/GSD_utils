! module_nwp.f90
! Eric James
! NOAA/OAR/ESRL/GSD/ADB
! 12 Dec 2018
!
! This module defines HRRR and FVCOM forecast data structure and the method to
! read and write observations from and to those data structures.  It is used by
! process_FVCOM.f90.

module module_nwp

   use kinds, only: r_kind, r_single, i_short, rmissing
   use module_nwp_base, only: nwpbase
   use module_map_utils, only: map_util
   use module_ncio, only: ncio

   implicit none

   public :: fcst_nwp
   public :: nwp_type

   private
   type :: nwp_type
      character(len=5) :: datatype
      integer :: numvar, xlat, xlon, xtime, xvert
      integer :: i_mask, i_sst, i_ice, i_sfcT, i_sltyp, i_vgtyp
      integer :: i_land, i_lu, i_snow, i_snowh, i_snowc
      character(len=20), allocatable :: varnames(:)
      character(len=20), allocatable :: varname3D
      character(len=20), allocatable :: latname
      character(len=20), allocatable :: lonname
      character(len=20), allocatable :: dimnameEW
      character(len=20), allocatable :: dimnameNS
      character(len=20), allocatable :: dimnameTIME
      character(len=20), allocatable :: dimnameVERT
      real(r_single), allocatable :: nwp_mask(:,:,:)
      real(r_single), allocatable :: nwp_sst(:,:,:)
      real(r_single), allocatable :: nwp_ice(:,:,:)
      real(r_single), allocatable :: nwp_sfcT(:,:,:)
      real(r_single), allocatable :: nwp_land(:,:,:)
      real(r_single), allocatable :: nwp_lu(:,:,:)
      real(r_single), allocatable :: nwp_snow(:,:,:)
      real(r_single), allocatable :: nwp_snowh(:,:,:)
      real(r_single), allocatable :: nwp_snowc(:,:,:)
      integer, allocatable :: nwp_sltyp(:,:,:)
      integer, allocatable :: nwp_vgtyp(:,:,:)
      real(r_single), allocatable :: nwp_soilmoisture(:,:,:,:)
   end type nwp_type

   type, extends(nwp_type) :: fcst_nwp
      type(nwpbase), pointer :: head => NULL()
      type(nwpbase), pointer :: tail => NULL()
      contains
         procedure :: initial => initial_nwp
         procedure :: list_initial => list_initial_nwp
         procedure :: read_n => read_nwp
         procedure :: read_n3 => read_nwp_3d
         procedure :: finish => finish_nwp
   end type fcst_nwp

   type(ncio) :: ncdata
   type(map_util) :: map

   contains

      subroutine initial_nwp(this,itype)

!        This subroutine defines the number of variables and their names for
!        each NWP data type.  The indices of the variables are
!        also defined for later reference.

         class(fcst_nwp) :: this

         character(len=5), intent(in) :: itype

!        FVCOM grid

         if (itype=='FVCOM') then
            this%datatype = itype
            this%numvar = 3

            this%i_mask = 1
            this%i_sst = 2
            this%i_ice = 3
            this%i_sfcT = 0
            this%i_sltyp = 0
            this%i_vgtyp = 0
            this%i_land = 0
            this%i_lu = 0
            this%i_snow = 0
            this%i_snowh = 0
            this%i_snowc = 0

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'LAKEMASK'
            this%varnames(2) = 'tsfc'
            this%varnames(3) = 'aice'

            allocate(this%latname)
            allocate(this%lonname)
            this%latname = 'XLAT_M'
            this%lonname = 'XLONG_M'

            allocate(this%dimnameEW)
            allocate(this%dimnameNS)
            allocate(this%dimnameTIME)
            allocate(this%dimnameVERT)
            this%dimnameEW = 'west_east'
            this%dimnameNS = 'south_north'
            this%dimnameTIME = 'time'
            this%dimnameVERT = 'soil_layers_stag'

            this%varname3D = 'SMOIS'

!        HRRR grid

         else if (itype==' HRRR') then
            this%datatype = itype
            this%numvar = 11

            this%i_mask = 1
            this%i_sst = 2
            this%i_ice = 3
            this%i_sfcT = 4
            this%i_sltyp = 5
            this%i_vgtyp = 6
            this%i_land = 7
            this%i_lu = 8
            this%i_snow = 9
            this%i_snowh = 10
            this%i_snowc = 11

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'LANDMASK'
            this%varnames(2) = 'SST'
            this%varnames(3) = 'SEAICE'
            this%varnames(4) = 'TSK'
            this%varnames(5) = 'ISLTYP'
            this%varnames(6) = 'IVGTYP'
            this%varnames(7) = 'XLAND'
            this%varnames(8) = 'LU_INDEX'
            this%varnames(9) = 'SNOW'
            this%varnames(10) = 'SNOWH'
            this%varnames(11) = 'SNOWC'

            allocate(this%latname)
            allocate(this%lonname)
            this%latname = 'XLAT'
            this%lonname = 'XLONG'

            allocate(this%dimnameEW)
            allocate(this%dimnameNS)
            allocate(this%dimnameTIME)
            allocate(this%dimnameVERT)
            this%dimnameEW = 'west_east'
            this%dimnameNS = 'south_north'
            this%dimnameTIME = 'Time'
            this%dimnameVERT = 'soil_layers_stag'

            this%varname3D = 'SMOIS'

!        If the data type does not match one of the known types, exit.

         else
            write(*,*) 'Unknown data type:', itype
            stop 1234
         end if

         this%head => NULL()
         this%tail => NULL()

         write(*,*) 'Finished initial_nwp'
         write(*,*) ' '

      end subroutine initial_nwp

      subroutine list_initial_nwp(this)

!        This subroutine lists the setup for NWP data that was done by
!        the initial_nwp subroutine.

         class(fcst_nwp) :: this

         integer :: k

         write(*,*) 'List initial setup for ', this%datatype
         write(*,*) 'number of variables ', this%numvar
         write(*,*) 'variable index: mask, sst, ice, sfcT, sltyp, vgtyp, land, &
      &      lu, snow, snowh, snowc'
         write(*,'(15x,10I3)') this%i_mask, this%i_sst, this%i_ice, &
      &      this%i_sfcT, this%i_sltyp, this%i_vgtyp, this%i_land, this%i_lu, &
      &      this%i_snow, this%i_snowh, this%i_snowc
         write(*,*) 'variable name:'
         do k=1,this%numvar
            write(*,*) k,trim(this%varnames(k))
         enddo

         write(*,*) 'Finished list_initial_nwp'
         write(*,*) ' '

      end subroutine list_initial_nwp

      subroutine read_nwp(this,filename,itype,numlon,numlat,numtimes,time_to_get,mask,sst,ice,sfcT,sltyp,vgtyp,land,lu,snow,snowh,snowc)

!        This subroutine initializes arrays to receive the NWP data,
!        and opens the file and gets the data.

         class(fcst_nwp) :: this

         character(len=5), intent(in) :: itype
         character(len=*), intent(in) :: filename

         integer, intent(in) :: time_to_get
         integer, intent(inout) :: numlon, numlat, numtimes
         real(r_single), intent(inout) :: mask(:,:), sst(:,:), ice(:,:), sfcT(:,:)
         real(r_single), intent(inout) :: land(:,:), lu(:,:), snow(:,:), snowh(:,:), snowc(:,:)
         integer, intent(inout) :: sltyp(:,:), vgtyp(:,:)

!        Open the file using module_ncio.f90 code, and find the number of
!        lat/lon points

         call ncdata%open(trim(filename),'r',200)
         call ncdata%get_dim(this%dimnameEW,this%xlon)
         call ncdata%get_dim(this%dimnameNS,this%xlat)
         call ncdata%get_dim(this%dimnameTIME,this%xtime)

         write(*,*) 'number of longitudes for file ', filename, this%xlon
         numlon = this%xlon
         write(*,*) 'number of latitudes for file ', filename, this%xlat
         numlat = this%xlat
         write(*,*) 'number of times for file ', filename, this%xtime
         numtimes = this%xtime

!        Allocate all the arrays to receive data

         allocate(this%nwp_mask(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_sst(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_ice(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_sfcT(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_sltyp(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_vgtyp(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_land(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_lu(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_snow(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_snowh(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_snowc(this%xlon,this%xlat,this%xtime))

!        Get variables from the data file, but only if the variable is
!        defined for that data type.

         if (this%i_mask .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_mask),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_mask)
            mask = this%nwp_mask(:,:,1)
         end if
         if (this%i_sst .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_sst),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_sst)
            sst = this%nwp_sst(:,:,time_to_get)
         end if
         if (this%i_ice .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_ice),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_ice)
            ice = this%nwp_ice(:,:,time_to_get)
         end if
         if (this%i_sfcT .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_sfcT),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_sfcT)
            sfcT = this%nwp_sfcT(:,:,time_to_get)
         end if
         if (this%i_sltyp .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_sltyp),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_sltyp)
            sltyp = this%nwp_sltyp(:,:,time_to_get)
         end if
         if (this%i_vgtyp .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_vgtyp),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_vgtyp)
            vgtyp = this%nwp_vgtyp(:,:,time_to_get)
         end if
         if (this%i_land .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_land),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_land)
            land = this%nwp_land(:,:,time_to_get)
         end if
         if (this%i_lu .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_lu),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_lu)
            lu = this%nwp_lu(:,:,time_to_get)
         end if
         if (this%i_snow .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_snow),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_snow)
            snow = this%nwp_snow(:,:,time_to_get)
         end if
         if (this%i_snowh .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_snowh),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_snowh)
            snowh = this%nwp_snowh(:,:,time_to_get)
         end if
         if (this%i_snowc .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_snowc),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_snowc)
            snowc = this%nwp_snowc(:,:,time_to_get)
         end if

!        Close the netCDF file.

         call ncdata%close

         write(*,*) 'Finished read_nwp'
         write(*,*) ' '

      end subroutine read_nwp

      subroutine read_nwp_3d(this,filename,itype,numlon,numlat,numvert,numtimes,time_to_get,soilmoisture)

!        This subroutine initializes arrays to receive the NWP data,
!        and opens the file and gets the data.

         class(fcst_nwp) :: this

         character(len=5), intent(in) :: itype
         character(len=*), intent(in) :: filename

         integer, intent(in) :: time_to_get
         integer, intent(inout) :: numlon, numlat, numvert, numtimes
         real(r_single), intent(inout) :: soilmoisture(:,:,:)

!        Open the file using module_ncio.f90 code, and find the number of
!        lat/lon points

         call ncdata%open(trim(filename),'r',200)
         call ncdata%get_dim(this%dimnameEW,this%xlon)
         call ncdata%get_dim(this%dimnameNS,this%xlat)
         call ncdata%get_dim(this%dimnameTIME,this%xtime)
         call ncdata%get_dim(this%dimnameVERT,this%xvert)

         write(*,*) 'number of longitudes for file ', filename, this%xlon
         numlon = this%xlon
         write(*,*) 'number of latitudes for file ', filename, this%xlat
         numlat = this%xlat
         write(*,*) 'number of times for file ', filename, this%xtime
         numtimes = this%xtime
         write(*,*) 'number of soil levels for file ', filename, this%xvert
         numvert = this%xvert

!        Allocate all the arrays to receive data

         allocate(this%nwp_soilmoisture(this%xlon,this%xlat,this%xvert,this%xtime))

!        Get variable from the data file

         call ncdata%get_var(this%varname3D,this%xlon,this%xlat,this%xvert,this%xtime,  &
                                this%nwp_soilmoisture)
         soilmoisture = this%nwp_soilmoisture(:,:,:,time_to_get)

!        Close the netCDF file.

         call ncdata%close

         write(*,*) 'Finished read_nwp_3d'
         write(*,*) ' '

      end subroutine read_nwp_3d

      subroutine finish_nwp(this)

         class(fcst_nwp) :: this

         type(nwpbase), pointer :: thisobs,thisobsnext

         deallocate(this%varnames)
         deallocate(this%latname)
         deallocate(this%lonname)
         deallocate(this%dimnameEW)
         deallocate(this%dimnameNS)
         deallocate(this%dimnameTIME)
         deallocate(this%dimnameVERT)
         deallocate(this%nwp_mask)
         deallocate(this%nwp_sst)
         deallocate(this%nwp_ice)
         deallocate(this%nwp_sfcT)
         deallocate(this%nwp_sltyp)
         deallocate(this%nwp_vgtyp)
         deallocate(this%nwp_land)
         deallocate(this%nwp_lu)
         deallocate(this%nwp_snow)
         deallocate(this%nwp_snowh)
         deallocate(this%nwp_snowc)
!         deallocate(this%nwp_soilmoisture)

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

         write(*,*) 'Finished finish_nwp'
         write(*,*) ' '

      end subroutine finish_nwp

end module module_nwp
