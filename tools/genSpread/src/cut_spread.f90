program cut_spread
!
!  read in regional ensemble HRRR spread and cut part og it 
!
   use module_ncio, only : ncio

   implicit none
   integer,parameter :: r_kind=8
   type(ncio) :: ncfin
   
   character(len=180) :: filename

   character(len=180) :: spreadfile
   character(len=180) :: spreadfile_cut
   character(len=10),allocatable :: varnamelist(:)
   integer :: numvar
   character(len=10) :: varname

   integer :: nxbk,nybk,nzbk
   real, allocatable :: rlat(:,:)
   real, allocatable :: rlon(:,:)
   real, allocatable :: rhgt(:,:)

   integer :: nx,ny,nz
   real(r_kind),allocatable :: sprd(:,:,:)

   integer :: nx_s,ny_s,nz_s
   integer :: nx_e,ny_e,nz_e
   integer :: i,iv
   integer :: iunit,iunit_out
!
   iunit=12
   iunit_out=13
!
   filename='/mnt/lfs1/projects/rtwbl/mhu/test/gsi/hrrre/data/wrfout_d02_2018-07-17_09_00_00'
   call ncfin%open(trim(filename),"r",200)
   call ncfin%get_dim("west_east",nxbk)
   call ncfin%get_dim("south_north",nybk)
   call ncfin%get_dim("bottom_top",nzbk)
   write(*,*) 'nx,ny,nz=',nxbk,nybk,nzbk
   allocate(rlat(nxbk,nybk))
   allocate(rlon(nxbk,nybk))
   allocate(rhgt(nxbk,nybk))
   call ncfin%get_var("XLAT",nxbk,nybk,rlat)
   call ncfin%get_var("XLONG",nxbk,nybk,rlon)
   call ncfin%get_var("HGT",nxbk,nybk,rhgt)
   call ncfin%close()
!
   nx_s=1
   ny_s=nybk/2
   nz_s=1
   nx_e=nxbk/2
   ny_e=nybk
   nz_e=nzbk
   nz_e=2
!
   numvar=5
   allocate(varnamelist(numvar))
   varnamelist(1)='ps'
   varnamelist(2)='tv'
   varnamelist(3)='u'
   varnamelist(4)='v'
   varnamelist(5)='rh'
!
   do iv=1,numvar
      varname=trim(varnamelist(iv)) 
      write(spreadfile,'(a,a,a)') './results/spread_',trim(varname),'.bin'
      write(spreadfile_cut,'(a,a,6(a1,I4.4),a)') './results/spread_',trim(varname),'X',&
                     nx_s,'-',nx_e,'Y',ny_s,'-',ny_e,'Z',nz_s,'-',nz_e,'.bin'
      write(*,*) 'read in ',trim(spreadfile)
      open(iunit,file=trim(spreadfile),form='unformatted',convert='BIG_ENDIAN')
         read(iunit) nx,ny,nz
         if(nx==nxbk .and. ny==nybk) then
            if(trim(varname)=='ps' .and. nz==1) then
               allocate(sprd(nx,ny,1))
            elseif(nz==nzbk) then
               allocate(sprd(nx,ny,nz))
            else
               write(*,*) 'mismatch vertical dimension'
               stop 123
            endif
         else
            write(*,*) 'mismatch horizontal dimension'
            stop 123
         endif
!
         read(iunit) sprd
!
         open(iunit_out,file=trim(spreadfile_cut),form='unformatted')
            write(iunit_out) nx_e-nx_s+1,ny_e-ny_s+1,nz_e-nz_s+1
            write(iunit_out) rlat(nx_s:nx_e,ny_s:ny_e)
            write(iunit_out) rlon(nx_s:nx_e,ny_s:ny_e)
            write(iunit_out) rhgt(nx_s:nx_e,ny_s:ny_e)
            write(iunit_out) real(sprd(nx_s:nx_e,ny_s:ny_e,nz_s:nz_e))
         close(iunit_out)
         deallocate(sprd)
      close(iunit)
   enddo ! iv

   deallocate(varnamelist)
   deallocate(rlat)
   deallocate(rlon)
   deallocate(rhgt)

end program cut_spread
