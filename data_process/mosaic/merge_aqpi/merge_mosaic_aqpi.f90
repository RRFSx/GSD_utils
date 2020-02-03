program merge_mosaic_aqpi
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2020-02-01
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic fiels and 
!     merge with experimental one
!
!     tversion=1  : NSSL 1 tile grib2
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
!
!   OUTPUT FILES:
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
  use mpi

  implicit none
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  For reflectiivty mosaic
!
  CHARACTER*256   mosaicfile
  CHARACTER*256   mosaicfile_out
  CHARACTER*256   filenameall(200)
  CHARACTER*256   filenameall_exp(200)

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
  REAL, allocatable :: msclon(:)        ! longitude of mosaic data
  REAL, allocatable :: msclat(:)        ! latitude of mosaic data
  REAL, allocatable :: msclev(:)        ! level of mosaic data
  REAL, allocatable :: mscValue(:,:)    ! reflectivity
  REAL, allocatable :: mscValue3d(:,:,:)    ! reflectivity

  REAL   :: lonMin,latMin,lonMax,latMax
  REAL*8 :: dlon,dlat
  integer :: height
  integer,parameter :: maxMosaiclvl=33
  integer :: levelheight(maxMosaiclvl)
  data levelheight /500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500,&
                  2750,3000,3500, 4000,4500,5000,5500,6000,6500,7000,&
                  7500,8000,8500,9000,10000,11000,12000,13000,14000, &
                  15000,16000,17000,18000,19000/
                   
!
!  namelist files
!
!  ** misc
      
  logical     :: fileexist

  integer :: i,j,k,n,ier
  integer :: nx,ny,nz,ntot

  INTEGER  :: ip,jp,ipp1,jpp1
  REAL ::  rip,rjp
  REAL ::  dip,djp
  real ::  rlonmin,rlatmax,rdx,rdy

  INTEGER  ::  maxlvl, nlevel,worklevel
  INTEGER  ::  numlvl,numref
  integer :: status
  integer :: maxcores

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  if(mype==0) write(*,*) mype, 'deal with mosaic'

!  open(15, file='mosaic.namelist')
!    read(15,setup)
!  close(15)
!
!  safty check for cores used in this run
!
!  read(analysis_time,'(I10)') idate
!  if(mype==0) write(6,*) 'cycle time is :', idate

  maxcores=33
  write(6,*) 'total cores for this run is ',npe
  if(npe < maxcores) then
     write(6,*) 'ERROR, this run must use ',maxcores,' or more cores !!!'
     call MPI_FINALIZE(ierror)
     stop 1234
  endif
!
!
  maxlvl = 33
!  rthresh_ref=-90.0
!  rthresh_miss=-900.0
  if(npe < maxlvl) then
        write(*,*) 'npe must larger or equal to maxlvl'
        stop 1234
  endif
!
  mypeLocal=mype+1

     open(10,file='filelist_mrms_exp',form='formatted',err=300)
     do n=1,200
        read(10,'(a)',err=200,end=400) filenameall(n)
     enddo
300  write(6,*) 'read_grib2 open filelist_mrms failed ',mype
     stop(555)
200  write(6,*) 'read_grib2 read msmr file failed ',n,mype
     stop(555)
400  nlevel=n-1
     close(10)
     if(nlevel .gt. maxlvl) then
        write(*,*) 'vertical level is too large:',nlevel,maxlvl
        stop 666
     endif
     if(mypeLocal <= npe) then
        mosaicfile=trim(filenameall(mypeLocal))
        write(*,*) 'process level:',mypeLocal,trim(mosaicfile)
        mosaicfile_out=trim(filenameall(mypeLocal))//".new"
     else
        mosaicfile=''
     endif
     if(npe < nlevel ) then
        write(6,*) 'Error, need more cores to run',npe
        stop 234
     endif
!
!   deal with certain tile
!
  fileexist=.false.
  inquire(file=trim(mosaicfile),exist=fileexist)
  if(mypeLocal > nlevel) fileexist=.false.

  if(fileexist) then
         call read_grib2_head(mosaicfile,nx,ny,nz,rlonmin,rlatmax,&
                   rdx,rdy)
         mscNlon=nx
         mscNlat=ny
         mscNlev=nz
         dlon=rdx
         dlat=rdy
         lonMin=rlonmin
         lonMax=lonMin+dlon*(mscNlon-1)
         latMax=rlatmax
         latMin=latMax-dlat*(mscNlat-1)
      if(mype==0) then
            write(*,*) 'mscNlon,mscNlat,mscNlev'
            write(*,*) mscNlon,mscNlat,mscNlev
            write(*,*) 'dlon,dlat,lonMin,lonMax,latMax,latMin'
            write(*,*) dlon,dlat,lonMin,lonMax,latMax,latMin
      endif

      allocate(msclon(mscNlon))
      allocate(msclat(mscNlat))
      allocate(msclev(mscNlev))
      allocate(mscValue(mscNlon,mscNlat))

      DO i=1,mscNlon
         msclon(i)=lonMin+(i-1)*dlon
      ENDDO
      DO i=1,mscNlat
         msclat(i)=latMin+(i-1)*dlat
      ENDDO
!
!  ingest mosaic file and interpolation
! 
      ntot = mscNlon*mscNlat
      call readwrite_grib2_sngle(mosaicfile,mosaicfile_out,ntot,height,mscValue)
      write(*,*) 'height,max,min',height,maxval(mscValue),minval(mscValue)
      call mpi_barrier(MPI_COMM_WORLD,ierror)
   endif
!  stop 999
!  if(fileexist) then
!
!      mscNlev=1
!      DO k=1, mscNlev
!             write(*,*) 'level max min height',mypeLocal,maxval(mscValue),minval(mscValue),height
!               if(rlon<0.0) rlon=360.0+rlon
!               rip=(rlon-lonMin)/dlon+1
!               rjp=(latMax-rlat)/dlat+1
!               ip=int(rip)
!               jp=int(rjp)
!               dip=rip-ip
!               djp=rjp-jp
!      ENDDO  ! mscNlev
!
!   ENDIF
  if(mype==0)  write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="
  call MPI_FINALIZE(ierror)
!
end program merge_mosaic_aqpi
