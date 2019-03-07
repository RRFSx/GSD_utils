! process_Lightning.f90
! Eric James
! NOAA/OAR/ESRL/GSD/ADB
! 23 Apr 2018
!
! This is the new RAP/HRRR lightning processing code (top level), which
! handles multiple sources of lightning observations:  Vaisala GLD360,
! ENTLN, NLDN, GLM, BLM Alaska, and NCEP BUFR files.  The code uses new
! object-oriented coding to handle the data, performing some minimal QC
! and mapping data onto GSI grid.
!
! The executable is now created via cmake.

program process_Lightning

   use module_map_utils, only: map_util
   use kinds2, only: r_kind, i_kind, r_single
   use module_ncio, only: ncio
   use module_lightning, only: obs_lightning

   implicit none

!  New object-oriented declarations

   type(ncio) :: geo
   type(obs_lightning) :: obs
   type(map_util) :: map

!  Grid variables

   character*180 :: geofile

   integer :: MAP_PROJ, NLON, NLAT

   real :: rad2deg = 180.0/3.1415926
   real :: userDX, userDY, CEN_LAT, CEN_LON
   real :: userTRUELAT1, userTRUELAT2, MOAD_CEN_LAT, STAND_LON
   real :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
   real :: knowni, knownj, dx
   real :: one, pi, deg2rad

!  For lightning data

   character*180 :: lightsngle

   integer :: numStrike
   integer :: numGLD360_all, numGLD360_used, numGLD360_total
   integer :: numENTLN_all, numENTLN_used, numENTLN_total
   integer :: numNLDN_all, numNLDN_used, numNLDN_total
   integer :: numGLM_all, numGLM_used, numGLM_total
   integer :: numAK_all, numAK_used, numAK_total
   integer :: numBUFR_all, numBUFR_used, numBUFR_total
   integer :: numTOT

   real(r_single), allocatable :: savelon_GLD360(:), savelat_GLD360(:)
   real(r_single), allocatable :: savelon_ENTLN(:), savelat_ENTLN(:)
   real(r_single), allocatable :: savelon_NLDN(:), savelat_NLDN(:)
   real(r_single), allocatable :: savelon_GLM(:), savelat_GLM(:)
   real(r_single), allocatable :: savelon_AK(:), savelat_AK(:)
   real(r_single), allocatable :: savelon_BUFR(:), savelat_BUFR(:)
   real, allocatable :: lightning(:,:,:) ! lightning strokes
   real(r_kind), allocatable :: lightning_out(:,:) ! lightning strokes

!  Declare namelists 
!  SETUP (general control namelist) :

   character*10 :: analysis_time

   integer :: analysis_min

   integer :: GLD360_filenum
   integer :: ENTLN_filenum
   integer :: NLDN_filenum
   integer :: GLM_filenum
   integer :: AK_filenum
   integer :: BUFR_filenum

   real :: trange_start, trange_end

   namelist/setup/analysis_time, analysis_min, trange_start, trange_end, &
      & GLD360_filenum, ENTLN_filenum, NLDN_filenum, GLM_filenum, AK_filenum, BUFR_filenum

!  Miscellaneous variables

   character*180 :: workpath

   integer :: i, j, nt
   integer :: numlightning, idate
   integer,parameter :: maxsave = 1000000

   real, allocatable :: xlon(:,:)
   real, allocatable :: ylat(:,:)
   real(r_single), allocatable :: rxlon(:,:)
   real(r_single), allocatable :: rylat(:,:)

!  Initialize number of files

   numGLD360_all=0
   numGLD360_used=0
   numGLD360_total=0
   numENTLN_all=0
   numENTLN_used=0
   numENTLN_total=0
   numNLDN_all=0
   numNLDN_used=0
   numNLDN_total=0
   numGLM_all=0
   numGLM_used=0
   numGLM_total=0
   numAK_all=0
   numAK_used=0
   numAK_total=0
   numBUFR_all=0
   numBUFR_used=0
   numBUFR_total=0

!  Read namelist

   read(5,setup)
   read(analysis_time,'(I10)') idate
   write(6,*) 'Cycle time is :', idate

!  Set geogrid fle name and obtain grid parameters

   workPath='./'
   write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'
   write(*,*) 'geofile', trim(geofile)
   call geo%open(trim(geofile),'r',200)
   call geo%get_dim("west_east",NLON)
   call geo%get_dim("south_north",NLAT)
   write(*,*) 'NLON,NLAT:', NLON, NLAT
   call geo%get_att("DX",userDX)
   call geo%get_att("DY",userDY)
   call geo%get_att("CEN_LAT",CEN_LAT)
   call geo%get_att("CEN_LON",CEN_LON)
   call geo%get_att("TRUELAT1",userTRUELAT1)
   call geo%get_att("TRUELAT2",userTRUELAT2)
   call geo%get_att("MOAD_CEN_LAT",MOAD_CEN_LAT)
   call geo%get_att("STAND_LON",STAND_LON)
   call geo%get_att("MAP_PROJ",MAP_PROJ)
   write(*,*) userDX, userDY, CEN_LAT, CEN_LON
   write(*,*) userTRUELAT1, userTRUELAT2, MOAD_CEN_LAT, STAND_LON, MAP_PROJ

!  Set up analysis map

   allocate(rxlon(nlon,nlat))
   allocate(rylat(nlon,nlat))

!  Get GSI horizontal grid in latitude and longitude

   allocate(xlon(nlon,nlat))
   allocate(ylat(nlon,nlat))

   call geo%get_var("XLAT_M",NLON,NLAT,ylat)
   call geo%get_var("XLONG_M",NLON,NLAT,xlon)

   one = 1.0
   pi = acos(-one)
   deg2rad = pi/180.0
   rylat=ylat*deg2rad
   rxlon=xlon*deg2rad

   call map%init_general_transform(nlon,nlat,rylat,rxlon)

   allocate(lightning(5,nlon,nlat))
   lightning=0

   write(*,*) 'Finished reading geofile.'
   write(*,*) ' '

!  Process GLD360 data

   allocate(savelon_GLD360(maxsave))
   allocate(savelat_GLD360(maxsave))
   savelon_GLD360=0
   savelat_GLD360=0

   do nt=1,GLD360_filenum
      if (nt .le. 9) then
         write(lightsngle,'(a,I1)') 'GLD360_lightning_', nt
      else
         write(lightsngle,'(a,I2)') 'GLD360_lightning_', nt
      end if
      write(*,*) trim(lightsngle)
      call obs%initial('GLD360')
      call obs%list_initial
      call obs%read_ltng(trim(lightsngle),'GLD360',numGLD360_all,savelon_GLD360,savelat_GLD360)
      call obs%qc('GLD360',analysis_time,analysis_min,trange_start,trange_end)
      call obs%regrid(lightning,numGLD360_used,nlon,nlat)
      call obs%finish
      numGLD360_total = numGLD360_total + numGLD360_used
   enddo ! nt

200 continue

   write(*,*) 'The total number of GLD360 obs available is:', numGLD360_all
   write(*,*) 'The final number of GLD360 grid points used is:', numGLD360_total
   write(*,*) 'Finished reading GLD360 files.'
   write(*,*) ' '

!  Process ENTLN data

   allocate(savelon_ENTLN(maxsave))
   allocate(savelat_ENTLN(maxsave))
   savelon_ENTLN=0
   savelat_ENTLN=0

   do nt=1,ENTLN_filenum
      if (nt .le. 9) then
         write(lightsngle,'(a,I1)') 'ENTLN_lightning_', nt
      else
         write(lightsngle,'(a,I2)') 'ENTLN_lightning_', nt
      end if
      write(*,*) trim(lightsngle)
      call obs%initial(' ENTLN')
      call obs%list_initial
      call obs%read_ltng(trim(lightsngle),' ENTLN',numENTLN_all,savelon_ENTLN,savelat_ENTLN)
      call obs%qc(' ENTLN',analysis_time,analysis_min,trange_start,trange_end)
      call obs%regrid(lightning,numENTLN_used,nlon,nlat)
      call obs%finish
      numENTLN_total = numENTLN_total + numENTLN_used
   enddo ! nt

300 continue

   write(*,*) 'The total number of ENTLN obs available is:', numENTLN_all
   write(*,*) 'The final number of ENTLN grid points used is:', numENTLN_total
   write(*,*) 'Finished reading ENTLN files.'
   write(*,*) ' '

!  Process AK data

   allocate(savelon_AK(maxsave))
   allocate(savelat_AK(maxsave))
   savelon_AK=0
   savelat_AK=0

   do nt=1,AK_filenum
      if (nt .le. 9) then
         write(lightsngle,'(a,I1)') 'AK_lightning_', nt
      else
         write(lightsngle,'(a,I2)') 'AK_lightning_', nt
      end if
      write(*,*) trim(lightsngle)
      call obs%initial('    AK')
      call obs%list_initial
      call obs%read_ltng(trim(lightsngle),'    AK',numAK_all,savelon_AK,savelat_AK)
      call obs%qc('    AK',analysis_time,analysis_min,trange_start,trange_end)
      call obs%regrid(lightning,numAK_used,nlon,nlat)
      call obs%finish
      numAK_total = numAK_total + numAK_used
   enddo ! nt

400 continue

   write(*,*) 'The total number of AK obs available is:', numAK_all
   write(*,*) 'The final number of AK grid points used is:', numAK_used
   write(*,*) 'Finished reading AK files.'
   write(*,*) ' '

!  Process GLM data

   allocate(savelon_GLM(maxsave))
   allocate(savelat_GLM(maxsave))
   savelon_GLM=0
   savelat_GLM=0

   do nt=1,GLM_filenum
      if (nt .le. 9) then
         write(lightsngle,'(a,I1)') 'GLM_lightning_', nt
      else if (nt .le. 99) then
         write(lightsngle,'(a,I2)') 'GLM_lightning_', nt
      else
         write(lightsngle,'(a,I3)') 'GLM_lightning_', nt
      end if
      write(*,*) trim(lightsngle)
      call obs%initial('   GLM')
      call obs%list_initial
      call obs%read_ltng(trim(lightsngle),'   GLM',numGLM_all,savelon_GLM,savelat_GLM)
      call obs%qc('   GLM',analysis_time,analysis_min,trange_start,trange_end)
      call obs%regrid(lightning,numGLM_used,nlon,nlat)
      call obs%finish
      numGLM_total = numGLM_total + numGLM_used
   enddo ! nt

500 continue

   write(*,*) 'The total number of GLM obs available is:', numGLM_all
   write(*,*) 'The final number of GLM grid points used is:', numGLM_used
   write(*,*) 'Finished reading GLM files.'
   write(*,*) ' '

!  Process BUFR data

   allocate(savelon_BUFR(maxsave))
   allocate(savelat_BUFR(maxsave))
   savelon_BUFR=0
   savelat_BUFR=0

   if (BUFR_filenum .gt. 0) then
      write(lightsngle,'(a,I1)') 'BUFR_lightning_1'
      write(*,*) trim(lightsngle)
      call obs%initial('  BUFR')
      call obs%list_initial
      call obs%read_bufr(trim(lightsngle),maxsave,analysis_time,analysis_min,&
                         trange_start,trange_end,numBUFR_all,savelon_BUFR,savelat_BUFR)
      call obs%qc('  BUFR',analysis_time,analysis_min,trange_start,trange_end)
      call obs%regrid(lightning,numBUFR_used,nlon,nlat)
      call obs%finish
      numBUFR_total = numBUFR_total + numBUFR_used
   end if

   write(*,*) 'The total number of BUFR obs available (within time window) is:', numBUFR_all
   write(*,*) 'The final number of BUFR grid points used is:', numBUFR_used
   write(*,*) 'Finished reading BUFR file.'
   write(*,*) ' '

!  Find max reflectivity in each column

   allocate(lightning_out(3,nlon*nlat))
   numlightning=0
   do j=1,nlat
   do i=1,nlon
      if(lightning(1,i,j) > 0 ) then
         numlightning=numlightning+1
         lightning_out(1,numlightning)=float(i)
         lightning_out(2,numlightning)=float(j)
         lightning_out(3,numlightning)=lightning(1,i,j)
         if(lightning_out(3,numlightning) > 1000.0 ) then
            lightning_out(3,numlightning)=1000.0
            write(6,*) 'high lightning strokes=',lightning(1,i,j),i,j
         endif
      endif
   enddo
   enddo

! Write out binary file

   numTOT = numGLD360_all + numENTLN_all + numAK_all + numGLM_all + numBUFR_all 

   write(*,*) 'Dump out results',numlightning,'out of',nlon*nlat
   OPEN(10,file=trim(workPath)//'LightningInGSI.dat',form='unformatted')
      write(10) 3,nlon,nlat,numlightning,1,2
      write(10) ((lightning_out(i,j),i=1,3),j=1,numlightning)
      write(10) lightning(1,:,:)
      write(10) numTOT
      write(10) savelon_GLD360(1:numGLD360_all),savelon_ENTLN(1:numENTLN_all),savelon_AK(1:numAK_all),savelon_GLM(1:numGLM_all),savelon_BUFR(1:numBUFR_all)
      write(10) savelat_GLD360(1:numGLD360_all),savelat_ENTLN(1:numENTLN_all),savelat_AK(1:numAK_all),savelat_GLM(1:numGLM_all),savelat_BUFR(1:numBUFR_all)
   close(10)

   write(6,*) ' write lightning in BUFR'
   call obs%write_bufr(1,nlon,nlat,numlightning,lightning_out,idate)

!   call geo%create_nc('GLD360_test.nc','lightning',nlon,nlat,lightning(1,:,:))
!   call geo%create_nc('GLM_test.nc','event_energy',nlon,nlat,lightning(5,:,:))

end program process_Lightning 
