subroutine append_tamdar(totalnum,iyear,month,iday,ihh,time_window)
!
!  append a upper air observation into prepbufr file
!
 use kinds, only: r_kind,i_kind,r_single

 implicit none

 integer, intent(in) :: totalnum
 integer, intent(in) :: iyear,month,iday,ihh
 real,    intent(in) :: time_window

 integer, parameter :: mxmn=35, mxlv=200
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 character(8) :: subset
 integer      :: unit_out=10,unit_table=20,idate,iret,nlvl

 character(8) :: c_sid
 real(8)      :: rstation_id
 equivalence(rstation_id,c_sid)
!
! tamdar obs
  character(9)   :: formatACARS
  character(9)   :: tailNumber
  real(r_single) :: latitude,longitude,T,prs,wd,spd
  real(r_single) :: alt,rawrh,rhUnct
  integer(i_kind):: TQCin,WDQCin,WSQCin
  real    :: timedhr,uu,vv,qmix
  integer(i_kind):: nTgood, nTsuspect, nTbad
  integer(i_kind):: nQgood, nQsuspect, nQbad
  integer(i_kind):: nWgood, nWsuspect, nWbad

!  ** misc
  integer i,ii
!
!
!
  nTgood=0; nTsuspect=0; nTbad=0
  nQgood=0; nQsuspect=0; nQbad=0
  nWgood=0; nWsuspect=0; nWbad=0

! get bufr table from existing bufr file
 open(unit_table,file='prepbufr.table')
 open(unit_out,file='prepbufr_tamdar',status='old',form='unformatted')
 call openbf(unit_out,'IN',unit_out)
 call dxdump(unit_out,unit_table)
 call closbf(unit_out)
!
! write observation into prepbufr file
!
 idate=iyear*1000000+month*10000+iday*100+ihh
 subset='AIRCFT'  ! upper-air (raob, drops) reports

 open(20, file='tamdarobs.bin',form='unformatted')

 open(unit_out,file='prepbufr_tamdar',status='old',form='unformatted')
 call datelen(10)
 call openbf(unit_out,'APN',unit_table)

   call openmb(unit_out,subset,idate)

   do i=1,totalnum
      read(20) ii,formatACARS,latitude,longitude,alt,t,wd,spd,rawrh,&
              tailNumber,prs,uu,vv,qmix,&
              timedhr,TQCin,WDQCin,WSQCin,rhUnct

      if(abs(timedhr) > time_window) cycle
!      write(*,*) tailNumber,latitude,longitude,alt,timedhr
!      write(*,*) t,wd,spd,rawrh
!      write(*,*) prs,uu,vv,qmix
!      write(*,*) TQCin,WDQCin,WSQCin,rhUnct
! set headers
      hdr=10.0e10
      c_sid=tailNumber(1:8); hdr(1)=rstation_id
      hdr(2)=longitude; hdr(3)=latitude; hdr(4)=timedhr; hdr(6)=alt

! set obs, qcf, oer for  wind
      hdr(5)=234          ! report type: tamdar
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=prs; qcf(1,1)=2.0           
      
      obs(5,1)=uu;obs(6,1)=vv
      if(WDQCin==0 .and. WSQCin==0 ) then          ! Good Data
         qcf(5,1)=2.0
         nWgood= nWgood+1
      elseif(WDQCin==1 .and. WSQCin==1) then       ! Slightly Suspect 
         qcf(5,1)=4.0
         nWsuspect=nWsuspect+1
      elseif(WDQCin==2 .and. WSQCin==2) then       ! Highly Suspect
         qcf(5,1)=8.0
         nWsuspect=nWsuspect+1
      else                          ! Bad Data  (=3)
         qcf(5,1)=16.00
         nWbad=nWbad+1
      endif

      oer(5,1)=2.5
      nlvl=1
! encode  wind obs
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

! set obs, qcf, oer for  temperature and moisture
      hdr(5)=134          ! report type: tamdar
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      obs(1,1)=prs 
      qcf(1,1)=2.0

      obs(2,1)=qmix*1000.0
      if(rhUnct >= 90.0) then
          qcf(2,1)=2.0
          nQgood= nQgood+1
      elseif(rhUnct < 90.0 .and. rhUnct >= 70.0) then
          qcf(2,1)=8.0
          nQsuspect=nQsuspect+1
      else
          qcf(2,1)=16.0
          nQbad=nQbad+1
      endif

      obs(3,1)=t-273.15
      if(TQCin == 0 ) then          ! Good Data
         qcf(3,1)=2.0
         nTgood= nTgood+1
      elseif(TQCin == 1) then       ! Slightly Suspect 
         qcf(3,1)=4.0
         nTsuspect=nTsuspect+1
      elseif(TQCin == 2) then       ! Highly Suspect
         qcf(3,1)=8.0
         nTsuspect=nTsuspect+1
      else                          ! Bad Data  (=3)
         qcf(3,1)=16.0
         nTbad=nTbad+1
      endif

      oer(2,1)=1.2   ;oer(3,1)=1.3
      nlvl=1
! encode temperature and moisture
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

   enddo
   call closmg(unit_out)
 call closbf(unit_out)
 close(20)

 write(*,*) 
 write(*,*) 'summary encode the Tamdar observations:'
 write(*,*) 'T good, suspect, bad=', nTgood,nTsuspect,nTbad
 write(*,*) 'Q good, suspect, bad=', nQgood,nQsuspect,nQbad
 write(*,*) 'Wind good, suspect, bad=', nWgood,nWsuspect,nWbad
 write(*,*) ' total obs encoded =',nWgood+nWsuspect+nWbad

end subroutine
