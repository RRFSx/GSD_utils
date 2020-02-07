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
  use readwrite_grib2_mod, only : grib2mod

  implicit none
!
  type(grib2mod) :: mrms_opt
  type(grib2mod) :: mrms_exp
!
! MPI variables
  integer  :: npe, mype, mypeLocal,ierror
  real     :: rad2deg = 180.0/3.1415926
!
!  For reflectiivty mosaic
!
  CHARACTER*256   mosaicfile
  CHARACTER*256   mosaicfile_exp
  CHARACTER*256   mosaicfile_out
  CHARACTER*256   filenameall(50)
  CHARACTER*256   filenameall_exp(50)

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
  REAL, allocatable :: msclon(:)        ! longitude of mosaic data
  REAL, allocatable :: msclat(:)        ! latitude of mosaic data
  REAL, allocatable :: mscValue(:,:)    ! reflectivity
  REAL, allocatable :: mscValue_exp(:,:)    ! reflectivity

  REAL   :: lonMin,latMin,lonMax,latMax
  REAL*8 :: dlon,dlat
  integer :: height
!
!  ** misc
      
  logical     :: fileexist,fileexist_exp
  integer :: i,j,k,n,ier
  integer :: ibegin,iend,jbegin,jend
  real ::  rlonmin,rlatmax,rdx,rdy

  INTEGER  ::  maxlvl, nlevel, nlevel_exp, worklevel

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  if(mype==0) write(*,*) mype, 'deal with mosaic'
!
  maxlvl = 33
  if(npe < maxlvl) then
     write(*,*) 'npe must larger or equal to maxlvl'
     stop 1234
  endif
!
  call find_filelist("filelist_mrms_exp",filenameall_exp,maxlvl,nlevel_exp)
  call find_filelist("filelist_mrms",filenameall,maxlvl,nlevel)
  call match_filelist(filenameall,nlevel,filenameall_exp,nlevel_exp)
!  if(mype==0)  then
!     do i=1,nlevel
!        write(*,*) 'matched=='
!        write(*,*) trim(filenameall(i))
!        write(*,*) trim(filenameall_exp(i))
!     enddo
!  endif
!
  mypeLocal=mype+1
  if(mypeLocal <= npe) then
     mosaicfile=trim(filenameall(mypeLocal))
     mosaicfile_exp=trim(filenameall_exp(mypeLocal))
!     write(*,*) 'process level:',mypeLocal,trim(mosaicfile)
!     write(*,*) 'process level:',mypeLocal,trim(mosaicfile_exp)
     mosaicfile_out=trim(mosaicfile)//".merged"
  else
     mosaicfile=''
     mosaicfile_exp=''
     mosaicfile_out=''
  endif
!
!   deal with certain tile
!
  call mpi_barrier(MPI_COMM_WORLD,ierror)
  fileexist=.false.
  fileexist_exp=.false.
  inquire(file=trim(mosaicfile),exist=fileexist)
  inquire(file=trim(mosaicfile_exp),exist=fileexist_exp)
  if(mypeLocal > nlevel) fileexist=.false.

  if(fileexist .and. fileexist_exp) then
     call mrms_opt%read_head(mosaicfile)
     call mrms_exp%read_head(mosaicfile_exp)
     if( (mrms_opt%nx == mrms_exp%nx) .and.  &
         (mrms_opt%ny == mrms_exp%ny) .and.  & 
         (mrms_opt%nz == mrms_exp%nz) ) then
        mscNlon=mrms_opt%nx
        mscNlat=mrms_opt%ny
        mscNlev=mrms_opt%nz
        dlon=mrms_opt%rdx
        dlat=mrms_opt%rdy
        lonMin=mrms_opt%rlonmin
        lonMax=lonMin+dlon*(mscNlon-1)
        latMax=mrms_opt%rlatmax
        latMin=latMax-dlat*(mscNlat-1)
        if(mype==0) then
           write(*,*) 'mscNlon,mscNlat,mscNlev'
           write(*,*) mscNlon,mscNlat,mscNlev
           write(*,*) 'dlon,dlat,lonMin,lonMax,latMax,latMin'
           write(*,*) dlon,dlat,lonMin,lonMax,latMax,latMin
        endif

        allocate(msclon(mscNlon))
        allocate(msclat(mscNlat))

        DO i=1,mscNlon
           msclon(i)=lonMin+(i-1)*dlon
        ENDDO
        DO i=1,mscNlat
           msclat(mscNlat-i+1)=latMin+(i-1)*dlat
        ENDDO
!        if(mype==0) then
!           write(*,*) 'msclon=',msclon
!           write(*,*) 'msclat=',msclat
!        endif
     else
        write(*,*) 'mismatch of dimension'
        stop 1234
     endif
!
!  ingest mosaic file and interpolation
! 
      mrms_opt%ntot = mscNlon*mscNlat
      mrms_exp%ntot = mscNlon*mscNlat
      allocate(mscValue(mscNlon,mscNlat))
      call mrms_opt%read_single(mosaicfile,mscValue)
!      write(*,*) 'height,max,min',mrms_opt%height,maxval(mscValue),minval(mscValue)
      allocate(mscValue_exp(mscNlon,mscNlat))
      call mrms_exp%read_single(mosaicfile_exp,mscValue_exp)
!      write(*,*) 'height,max,min',mrms_exp%height,maxval(mscValue_exp),minval(mscValue_exp)
!
!  merge AQPI radar from experiment to operation
!  36.98N to 38.94N, -123.22W(236.78) to -121.42W(238.58) and below 4km
!
      if(mrms_opt%height <= 4000) then
         ibegin=int((236.78-msclon(1))/dlon)+1
         iend  =int((238.58-msclon(1))/dlon)+1+1
         jend  =int((msclat(1)-36.98)/dlat)+1+1
         jbegin=int((msclat(1)-38.94)/dlat)+1
         write(*,*) 'ibegin,iend,jbegin,jend=',ibegin,iend,jbegin,jend
!         write(*,*) msclon(ibegin-1:ibegin+1)
!         write(*,*) msclon(iend-1:iend+1)
!         write(*,*) msclat(jbegin-1:jbegin+1)
!         write(*,*) msclat(jend-1:jend+1)

         do j=jbegin,jend
           do i=ibegin,iend
              if( abs(mscValue(i,j)+999.0000) < 0.01 ) then
                 if( mscValue_exp(i,j) > mscValue(i,j) + 0.01)  then
                    !write(*,*) i,j,mrms_opt%height,mscValue(i,j),mscValue_exp(i,j)
                    mscValue(i,j)=mscValue_exp(i,j)
                 endif
              endif
           enddo
         enddo
         call mrms_opt%readwrt_single(mosaicfile,mosaicfile_out,mscValue)
      endif
  endif

  call mpi_barrier(MPI_COMM_WORLD,ierror)

  if(mype==0)  write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="
  call MPI_FINALIZE(ierror)
!
end program merge_mosaic_aqpi

subroutine find_filelist(filelist,filenameall,maxlvl,nlevel)
! read file from filelist
  implicit none 
  CHARACTER*100,intent(in)  :: filelist
  integer, intent(in)       :: maxlvl
  CHARACTER*256,intent(out) :: filenameall(50)
  integer, intent(out)      :: nlevel

  integer :: n

  open(10,file=trim(filelist),form='formatted',err=300)
     do n=1,50
        read(10,'(a)',err=200,end=400) filenameall(n)
     enddo
300  write(6,*) 'read_grib2 open filelist_mrms failed '
     stop(555)
200  write(6,*) 'read_grib2 read msmr file failed ',n
     stop(555)
400  nlevel=n-1
  close(10)

     if(nlevel .gt. maxlvl) then
        write(*,*) 'vertical level is too large:',nlevel,maxlvl
        stop 666
     endif

end subroutine find_filelist

subroutine match_filelist(filenameall,nlevel,filenameall_exp,nlevel_exp)
! read file from filelist
  implicit none 
  integer,parameter :: maxlvl=50
  CHARACTER*256,intent(inout) :: filenameall(maxlvl)
  integer, intent(inout)      :: nlevel
  CHARACTER*256,intent(inout) :: filenameall_exp(maxlvl)
  integer, intent(inout)      :: nlevel_exp
!
  CHARACTER*256 :: filenameall_temp(maxlvl)
  CHARACTER*256 :: filenameall_exp_temp(maxlvl)
!
  integer :: height(maxlvl), height_exp(maxlvl)
  integer :: match(maxlvl,2),nlvl
  real    :: rheight
  integer :: i, n
  CHARACTER*10 :: filename

  filenameall_temp=filenameall
  filenameall_exp_temp=filenameall_exp
  do n=1, nlevel
     filename=filenameall_temp(n)(43:47)
     read(filename,'(f5.2)') rheight
     height(n)=int(rheight*1000)
  enddo
  do n=1, nlevel_exp
     filename=filenameall_exp_temp(n)(47:51)
     read(filename,'(f5.2)') rheight
     height_exp(n)=int(rheight*1000)
  enddo

  nlvl=0
  match=0
  do n=1,nlevel
     do i=1,nlevel_exp
        if(height(n) == height_exp(i)) then
           nlvl=nlvl+1
           match(nlvl,1)=n
           match(nlvl,2)=i
        endif
     enddo
  enddo

  do i=1,nlvl
     filenameall(i)=filenameall_temp(match(i,1))
     filenameall_exp(i)=filenameall_exp_temp(match(i,2))
  enddo
  nlevel=nlvl

end subroutine match_filelist
