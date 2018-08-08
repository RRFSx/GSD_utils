program get_pairs
!
  use kinds, only: r_kind,r_single,len_sta_name
  use module_obs_conv_array, only : obs_conv_array
  use module_pairs, only : fcstobspairs
  use module_statistic , only : statisticbase 
  use module_time, only : mtime
  use module_stationarray, only : stationarray
  use module_hashsearch, only : hashtable
!
  implicit none

  type(obs_conv_array) :: orgobs
  type(mtime) :: mt
  type(fcstobspairs) :: pairs
  type(statisticbase):: stats
  type(statisticbase):: stats_ts
  type(statisticbase):: stats_station
  type(statisticbase):: stats_station_ts
  type(stationarray) :: sta_arr
  type(hashtable)    :: hashobs
!
!  namelist
  integer :: startdate,startmin
  integer :: enddate,endmin
  integer :: fcst_init_interval,fcst_output_interval,fcst_length
  character(len=180) :: savePath,prefixobssavename
  character(len=180) :: prefixfcstobssavename
  character(len=180) :: prefix_stats
  namelist/setup/ startdate,startmin,enddate,endmin,&
                  savePath,prefixobssavename,prefixfcstobssavename,&
                  prefix_stats,&
                  fcst_init_interval,fcst_output_interval,fcst_length

  integer :: numobstype
  integer :: obstype(100)
  integer :: obs_interval(100)
  real(r_single) :: timewindow(100)
  namelist/verifyobsset/ numobstype,obstype,obs_interval,timewindow

  logical :: if_all,if_time_series,if_station_all,if_station_series
  character(len=180) :: setfile_all
  character(len=180) :: setfile_time_series
  character(len=180) :: setfile_station_all
  character(len=180) :: setfile_station_series
  integer :: numobstype_station
  integer :: obstype_station(100)
  namelist/statisticset/if_all,setfile_all,if_time_series,setfile_time_series,&
                        if_station_all,setfile_station_all, &
                        numobstype_station,obstype_station, &
                        if_station_series,setfile_station_series

  integer :: domainx,domainy,hfactor
  namelist/hashsearchset/ domainx,domainy,hfactor

  character(len=180) :: obsfile
  character(len=180) :: saveobsfile
  character(len=180) :: savefcstfile
  character(len=180) :: fcstfile
  character(len=12)  :: timetag
  character(len=32)  :: timetagout
  character(len=19)  :: START_DATE
  character(len=1)   :: FCST_TIMES(19)
  integer :: n,obsinc
  integer :: vy,vm,vd,vh,vf   ! valid time
  integer :: sy,sm,sd,sh,sf   ! start time
  integer :: smins,emins,vmins,fsmins
  integer :: fcstmins,fcstvhh,fcstvmin
  character(len=180) :: savestatsfile_all
  character(len=180) :: savestatsfile_ts
  character(len=180) :: savestatsfile_station
  character(len=180) :: savestatsfile_station_ts
  logical :: if_thisdatatype
!
  integer :: nx,ny,nz
  integer :: i,k,indx
!
! 
! 
  startdate=2017010103
  startmin=0
  enddate=2017010103
  endmin=0
  savePath='./'
  prefixobssavename='prepbufr'
  prefixfcstobssavename='prepbufr'
  prefix_stats='stats_'
  fcst_init_interval=1     !min
  fcst_output_interval=1   !min
  fcst_length=12           ! hour
  
  numobstype=1
  obstype=120
  obs_interval=1         !min
  timewindow=1.0

  if_all=.false.
  setfile_all='statisticset.txt'
  if_time_series=.false.
  setfile_time_series='statisticset_ts.txt'
  if_station_all=.false.
  setfile_station_all='statisticset_staall.txt'
  numobstype=0
  if_station_series=.false.
  setfile_station_series='statisticset_sta_ts.txt'

  domainx=250
  domainy=250
  hfactor=2
  open(15,file='namelist_verif.input')
      read(15,setup)
      read(15,verifyobsset)
      read(15,statisticset)
      read(15,hashsearchset)
  close(15)
!
! find start and time in mininus 
  write(*,*) 'verification start time=',startdate,startmin
  write(*,*) 'verification valid time=',enddate,endmin
  sy=startdate/1000000
  sf=startdate-sy*1000000
  sm=sf/10000
  sf=sf-sm*10000
  sd=sf/100
  sh=sf-sd*100
  sf=startmin
  write(*,'(a,5I4)') 'verification start time=',sy,sm,sd,sh,sf
  vy=enddate/1000000
  vf=enddate-vy*1000000
  vm=vf/10000
  vf=vf-vm*10000
  vd=vf/100
  vh=vf-vd*100
  vf=endmin
  write(*,'(a,5I4)') 'verification end time=',vy,vm,vd,vh,vf
  smins=mt%date2mins(sy,sm,sd,sh,sf)
  emins=mt%date2mins(vy,vm,vd,vh,vf)

  saveobsfile=trim(savePath)//"/"//trim(prefixobssavename)
  savefcstfile=trim(savePath)//"/"//trim(prefixfcstobssavename)
!
!
  do n=1,numobstype
     write(*,*)
     write(*,*) '================================='
!  make a sation array if needed
     if_thisdatatype=.false.
     if(if_station_all) then
!
        do i=1,numobstype_station
           if(obstype(n)==obstype_station(i)) if_thisdatatype=.true.
        enddo
        if(if_thisdatatype) then

! setup hash table 
           call hashobs%initial(domainx,domainy,hfactor)
!  setup station array
           call sta_arr%initial(200)
!
           vmins=smins
           do while (vmins<=emins)
              call mt%mins2date(vmins,vy,vm,vd,vh,vf)
              write(timetag,'(I4,4I2.2)') vy,vm,vd,vh,vf
              write(obsfile,'(a,a,I4.4,3a)') trim(saveobsfile),'_arrayXY_type',&
                               obstype(n),'_',timetag,'.bin'

              call orgobs%readaxy(trim(obsfile))
              if(orgobs%n_alloc > 0) then
                 call sta_arr%make(orgobs,hashobs)
              endif
              call orgobs%destroy()

              vmins=vmins+obs_interval(n)
           enddo ! vmins<=emins
!           call sta_arr%list()
!           call hashobs%list()
!
           call stats_station%initial(obstype(n),trim(setfile_station_all),sta_arr%numsta)
           write(savestatsfile_station,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_station_s',&
                      startdate,startmin,"e",enddate,endmin,'.bin'
           write(*,*) 'save time series results to:',trim(savestatsfile_station)
!
! station time series
           if(if_station_series) then
              call stats_station_ts%initial(obstype(n),trim(setfile_station_series),sta_arr%numsta)
              write(savestatsfile_station_ts,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_station_ts_s',&
                      startdate,startmin,"e",enddate,endmin,'.bin'
              write(*,*) 'save time series results to:',trim(savestatsfile_station_ts)
           endif
        endif
     endif
!
     if(if_all) then
        call stats%initial(obstype(n),trim(setfile_all))
        write(savestatsfile_all,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_all_s',&
                   startdate,startmin,"e",enddate,endmin,'.bin'
        write(*,*) 'save all results to:',trim(savestatsfile_all)
     endif
     if(if_time_series) then
        call stats_ts%initial(obstype(n),trim(setfile_time_series))
        write(savestatsfile_ts,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_ts_s',&
                   startdate,startmin,"e",enddate,endmin,'.bin'
        write(*,*) 'save time series results to:',trim(savestatsfile_ts)
     endif

     vmins=smins
     do while (vmins<=emins)
        call mt%mins2date(vmins,vy,vm,vd,vh,vf)
        write(*,'(a,5I4,a,I4)') 'working on obs time=',vy,vm,vd,vh,vf, &
                   ' for obs type=',obstype(n)
        write(timetag,'(I4,4I2.2)') vy,vm,vd,vh,vf
        write(obsfile,'(a,a,I4.4,3a)') trim(saveobsfile),'_arrayXY_type',obstype(n),&
                          '_',timetag,'.bin'
!        write(*,'(a,a)') 'read obs file =',trim(obsfile)

        call orgobs%readaxy(trim(obsfile))
        if(orgobs%n_alloc > 0) then
!           call orgobs%lista(1)
           fcstmins=0
           do while (fcstmins<=fcst_length*60)
! read the forecast observations 
              fsmins=vmins-fcstmins
              call mt%mins2date(fsmins,sy,sm,sd,sh,sf)
              fcstvhh=fcstmins/60
              fcstvmin=fcstmins-fcstvhh*60
              write(timetagout,'(a1,I4,4I2.2,a1,I3.3,I2.2,a1,I4,4I2.2)') "s",sy,sm,sd,sh,sf,&
                              "f",fcstvhh,fcstvmin,"v",vy,vm,vd,vh,vf
!              write(*,*) 'Time tag for foreast obs=',timetagout
              write(fcstfile,'(a,a,I4.4,3a)') trim(savefcstfile),'_fcstobs_type',obstype(n),&
                          '_',timetagout,'.bin'
              write(*,'(a,a)') 'read fcst obs file =',trim(fcstfile)
              call pairs%readall(trim(fcstfile))
!
!  now get pairs
! match the forecast observations to the real observations
!
              call pairs%match(orgobs)
              call pairs%listpair(1)

!
!  Now use those pairs
              if(if_all) then
                 call stats%find_varid(pairs)
                 call stats%accumulate(pairs,fcstmins)
              endif
              if(if_time_series) then
                 call stats_ts%find_varid(pairs)
                 call stats_ts%accumulate(pairs,fcstmins)
              endif
              if(if_station_all .and. if_thisdatatype) then
                 call stats_station%find_varid(pairs)
                 call stats_station%accumulate(pairs,fcstmins,hashobs)
!
                if(if_station_series) then
                   call stats_station_ts%find_varid(pairs)
                   stats_station_ts%ifsavefcst=.true.
                   call stats_station_ts%accumulate(pairs,fcstmins,hashobs)
                endif
              endif
!
!  find next forecast valid at this time
!
              fcstmins=fcstmins+fcst_output_interval
              call pairs%destroy()
           enddo
!
!  save time series 
!
           if(if_time_series) then
              call stats_ts%calculate()
              if(vmins==smins) then
                 call stats_ts%writes('new',trim(savestatsfile_ts),timetag)
              else
                 call stats_ts%writes('app',trim(savestatsfile_ts),timetag)
              endif
              call stats_ts%reset()
           endif
           if(if_station_all .and. if_thisdatatype .and. if_time_series) then
!              call stats_station_ts%calculate()
              call stats_station_ts%save_sta()
              if(vmins==smins) then
                 call stats_station_ts%writes_sta('new',trim(savestatsfile_station_ts),sta_arr,timetag)
              else
                 call stats_station_ts%writes_sta('app',trim(savestatsfile_station_ts),sta_arr,timetag)
              endif
!              call stats_station_ts%list()
              call stats_station_ts%reset()
           endif
        endif  ! if there are observations
!
!
        call orgobs%destroy()
!
! next observation time level 
        vmins=vmins+obs_interval(n)
!
     enddo ! vmins<=emins
!
!  release memory
!
     if(if_all) then
!  get final statistic results
        call stats%calculate()
!  list final statistic results
!        call stats%list()
!  write final statistic results
        call stats%writes('new',trim(savestatsfile_all))
! done with this data type, release the space
        call stats%destroy()
     endif
     if(if_time_series) then
        call stats_ts%destroy()
     endif
     if(if_station_all .and. if_thisdatatype) then
        call stats_station%calculate()
!  list final statistic results
!        call stats_station%list()
!  write final statistic results
        call stats_station%writes_sta('new',trim(savestatsfile_station),sta_arr)
!
        call stats_station%destroy()
        call sta_arr%destroy()
! release hash table 
        call hashobs%destroy()
        if(if_station_series) then
           call stats_station_ts%destroy()
        endif
     endif
!
  enddo   ! data type
!
end program get_pairs
