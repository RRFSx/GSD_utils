  subroutine write_bufr_satcast(nlon,nlat,index,ir_cr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   write_bufr_satcast
!   prgmmr: smith           org: esrl/gsd                date: 2012-12-01
!   
! abstract: write UAH SATCAST in RR grid into bufr
!   
! program history log:
!   2012-09-18  Smith
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  linux 
!
!$$$
    use constants, only: zero, one
    use kinds, only: r_kind,i_kind,r_single
    implicit none

!
    integer(i_kind), intent(in) :: nlon,nlat
    integer, intent(in)  ::   index(nlon,nlat)
    REAL(r_single), intent(in) ::   ir_cr (nlon,nlat)

    real(r_kind) :: hdr(5),obs(1,5)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    integer(i_kind) :: ludx,lendian_in,idate

    INTEGER(i_kind)  ::  maxlvl, numref
    INTEGER(i_kind)  ::  i,j,n,k,iret


    open(12,file='satcast_cycle_date')
      read(12,*) idate
    close(12)
    write(6,*) 'cycle time is :', idate
!mhu    idate=2008120100
    subset='ADPUPA'
    sid='SATCAST '
    ludx=22
    lendian_in=10

    open(ludx,file='prepobs_prep.bufrtable',action='read')
    open(lendian_in,file='SATCASTInGSI.bufr',action='write',form='unformatted')

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    maxlvl=1
    numref=0
!mhu    do j=1,nlat
!mhu    do i=1,nlon
! GSI has peroidic boundary which can crash the cloud analysis if there are data along the boundary.
! wait for GSI to fix this issue.
    do j=2,nlat-1
    do i=2,nlon-1
      if(index(i,j) .ge. 3) then
        numref = numref + 1
        hdr(1)=transfer(sid,hdr(1))
        hdr(2)=float(i)/10.0_r_kind
        hdr(3)=float(j)/10.0_r_kind
        hdr(4)=0
        hdr(5)=500

        if( ir_cr(i,j) > 88888.0 .or. ir_cr(i,j) .eq. 0.0) then
          obs(1,1)=9999.0
        else
          obs(1,1)=ir_cr(i,j)
        endif


        call openmb(lendian_in,subset,idate)
        call ufbint(lendian_in,hdr,5,   1,iret,hdrstr)
        call ufbint(lendian_in,obs,1,maxlvl,iret,obsstr)
        call writsb(lendian_in,ibfmsg,iret)
      endif
    enddo   !i
    enddo   !j
    call closbf(lendian_in)
    write(6,*) 'write_bufr_satcast, DONE: write columns:',numref

end subroutine  write_bufr_satcast
