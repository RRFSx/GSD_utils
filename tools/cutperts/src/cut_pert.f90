program calculate_spread
!
!  read in regional ensemble perturbations from 
!  HRRRE and calculate ensemble spread
!
   use module_interp_perturbations, only : interp_perts
   use module_ncio, only : ncio
   use module_map_utils, only : map_util

   implicit none
   type(interp_perts) :: cutperts
   type(ncio) :: ncforg
   type(ncio) :: ncfcut
   type(map_util) :: map
   
   integer :: nfile
   character(len=180),allocatable :: filelist(:)
   character(len=180) :: filename
   character(len=180) :: outfilename
   character(len=10),allocatable :: varnamelist(:)
   integer :: numvar
   character(len=10) :: varname
!
   character(len=180) :: filelistname
   character(len=180) :: resultspath
   character(len=180) :: org_grid
   character(len=180) :: cut_grid
   character(len=80) :: cutname
   namelist/setup/ filelistname,resultspath,org_grid,cut_grid,cutname
!
   integer :: nxorg,nyorg
   integer :: nxcut,nycut
   real, allocatable :: rlat_org(:,:)
   real, allocatable :: rlon_org(:,:)
   real, allocatable :: rlat_cut(:,:)
   real, allocatable :: rlon_cut(:,:)
   real :: xc,yc,xc1,yc1
!
   integer :: i,j,nf
!
   numvar=5
   allocate(varnamelist(numvar))
   varnamelist(1)='ps'
   varnamelist(2)='tv'
   varnamelist(3)='u'
   varnamelist(4)='v'
   varnamelist(5)='rh'
!
!
   open(10,file='namelist.cutpert')
      read(10,setup)
   close(10)
   write(*,setup)
!
!
   nfile=0
   open(12,file=trim(filelistname))
      do
        read(12,*,end=100)
        nfile=nfile+1
        if(nfile > 1000) exit
      enddo
100 continue
     if(nfile > 0) then
        allocate(filelist(nfile))
        rewind(12)
        do i=1,nfile
           read(12,'(a)') filelist(i)
        enddo
     else
        write(*,*) 'no file in filelist'
        stop 123
     endif
   close(12)
   write(*,*) 'total file will Process  =',nfile

! read dimension and latlon frpm original file
   call ncforg%open(trim(org_grid),"r",200)
   call ncforg%get_dim("west_east",nxorg)
   call ncforg%get_dim("south_north",nyorg)
   write(*,*) 'nx,ny=',nxorg,nyorg
   allocate(rlat_org(nxorg,nyorg))
   allocate(rlon_org(nxorg,nyorg))
   call ncforg%get_var("XLAT",nxorg,nyorg,rlat_org)
   call ncforg%get_var("XLONG",nxorg,nyorg,rlon_org)
   call ncforg%close()
!initial map 
   call map%init_general_transform(nxorg,nyorg,rlat_org,rlon_org)
   deallocate(rlat_org)
   deallocate(rlon_org)

! read dimension and latlon from cut file
   call ncfcut%open(trim(cut_grid),"r",200)
   call ncfcut%get_dim("west_east",nxcut)
   call ncfcut%get_dim("south_north",nycut)
   write(*,*) 'nx,ny=',nxcut,nycut
   allocate(rlat_cut(nxcut,nycut))
   allocate(rlon_cut(nxcut,nycut))
   call ncfcut%get_var("XLAT",nxcut,nycut,rlat_cut)
   call ncfcut%get_var("XLONG",nxcut,nycut,rlon_cut)
   call ncfcut%close()
!
!
   call cutperts%initial(nxorg,nyorg,nxcut,nycut)
!
! calcaule bilinear interpolation weights
   do j=1,nycut
     do i=1,nxcut
       call map%tll2xy(rlon_cut(i,j),rlat_cut(i,j), xc,yc)
       call cutperts%genw(i,j,xc,yc)
     enddo
   enddo
   deallocate(rlon_cut)
   deallocate(rlat_cut)
   write(*,*) 'leftlow=',cutperts%ijloc(1,1,1),cutperts%ijloc(1,1,2)
   write(*,*) 'rightup=',cutperts%ijloc(nxcut,nycut,1),cutperts%ijloc(nxcut,nycut,2)
!
   do nf=1,nfile
      write(outfilename,'(4a,I3.3,a)') trim(resultspath),"/",trim(cutname),"_",nf,'.bin'
      filename=trim(filelist(nf))
      write(*,'(a,I3,a)') "processing ",nf,trim(filename)
      write(*,'(a,a)') "result file=",trim(outfilename)
      call cutperts%rwinterp(filename,outfilename)
   enddo

   deallocate(filelist)
   call cutperts%destroy()

end program calculate_spread
