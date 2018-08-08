program read_statistic_results
!
  use kinds, only: r_kind,r_single,len_sta_name
  use module_readwrt_stats_results , only : readwrt_stats_results 
!
  implicit none

  type(readwrt_stats_results):: stats
  type(readwrt_stats_results):: stats_ts
  type(readwrt_stats_results):: stats_station
  type(readwrt_stats_results):: stats_station_ts
!
!  namelist
  integer :: startdate,startmin
  integer :: enddate,endmin
  character(len=180) prefix_stats
  namelist/setup/ startdate,startmin,enddate,endmin,prefix_stats

  integer :: numobstype
  integer :: obstype(100)
  namelist/verifyobsset/ numobstype,obstype

  logical :: if_all,if_time_series,if_station_all,if_station_series
  integer :: numobstype_station
  integer :: obstype_station(100)
  namelist/statisticset/if_all,if_time_series,if_station_all,if_station_series,&
                        numobstype_station,obstype_station

  character(len=180) :: savestatsfile_all
  character(len=180) :: savestatsfile_ts
  character(len=180) :: savestatsfile_station
  character(len=180) :: savestatsfile_station_ts
  character(len=180) :: plotstatsfile_all_head,plotstatsfile_all_data
  character(len=180) :: plotstatsfile_ts_head,plotstatsfile_ts_data
  character(len=180) :: plotstatsfile_station_head,plotstatsfile_station_data
  character(len=180) :: plotstatsfile_station_ts_head,plotstatsfile_station_ts_data          
  character(len=180) :: plotfile_station_list          
  logical :: if_thisdatatype
!
  integer :: nx,ny,nz,n
  integer :: i,k,indx
!
! 
! 
  startdate=2017010103
  startmin=0
  enddate=2017010103
  endmin=0
  prefix_stats='stat_'
  
  numobstype=1
  obstype=120

  if_all=.false.
  if_time_series=.false.
  if_station_all=.false.
  numobstype=0
  if_station_series=.false.

  open(15,file='namelist_read_results.input')
      read(15,setup)
      read(15,verifyobsset)
      read(15,statisticset)
  close(15)
!
! find start and time in mininus 
  write(*,*) 'verification start time=',startdate,startmin
  write(*,*) 'verification valid time=',enddate,endmin
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
!
           write(savestatsfile_station,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_station_s',&
                      startdate,startmin,"e",enddate,endmin,'.bin'
           write(*,*) 'read results from:',trim(savestatsfile_station)
           write(plotstatsfile_station_head,'(a,a,I4.4,a)') trim(prefix_stats),'head_',obstype(n),'_station.bin'
           write(plotstatsfile_station_data,'(a,a,I4.4,a)') trim(prefix_stats),'data_',obstype(n),'_station.bin'
           write(plotfile_station_list,'(a,I4.4,a)') 'stats_',obstype(n),'_stationlist.txt'
!
! station time series
           if(if_station_series) then
              write(savestatsfile_station_ts,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_station_ts_s',&
                      startdate,startmin,"e",enddate,endmin,'.bin'
              write(*,*) 'read results from:',trim(savestatsfile_station_ts)
              write(plotstatsfile_station_ts_head,'(a,a,I4.4,a)') trim(prefix_stats),'head_',obstype(n),'_station_ts.bin'
              write(plotstatsfile_station_ts_data,'(a,a,I4.4,a)') trim(prefix_stats),'data_',obstype(n),'_station_ts.bin'
           endif
        endif
     endif
!
     if(if_all) then
        write(savestatsfile_all,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_all_s',&
                   startdate,startmin,"e",enddate,endmin,'.bin'
        write(*,*) 'read results from:',trim(savestatsfile_all)
        write(plotstatsfile_all_head,'(a,a,I4.4,a)') trim(prefix_stats),'head_',obstype(n),'_all.bin'
        write(plotstatsfile_all_data,'(a,a,I4.4,a)') trim(prefix_stats),'data_',obstype(n),'_all.bin'
     endif
     if(if_time_series) then
        write(savestatsfile_ts,'(a,I4.4,a,I10,I2.2,a1,I10,I2.2,a)') trim(prefix_stats),obstype(n),'_ts_s',&
                   startdate,startmin,"e",enddate,endmin,'.bin'
        write(*,*) 'read results from:',trim(savestatsfile_ts)
        write(plotstatsfile_ts_head,'(a,a,I4.4,a)') trim(prefix_stats),'head_',obstype(n),'_ts.bin'
        write(plotstatsfile_ts_data,'(a,a,I4.4,a)') trim(prefix_stats),'data_',obstype(n),'_ts.bin'
     endif

     if(if_time_series) then
!        call stats_ts%reads(trim(savestatsfile_ts))
!        call stats_ts%list()
        call stats_ts%readwrt(trim(savestatsfile_ts),trim(plotstatsfile_ts_head),trim(plotstatsfile_ts_data),.false.)
        call stats_ts%destroy()
     endif
!
     if(if_all) then
!        call stats%reads(trim(savestatsfile_all))
!        call stats%list()
        call stats%readwrt(trim(savestatsfile_all),trim(plotstatsfile_all_head),trim(plotstatsfile_all_data),.false.)
        call stats%destroy()
     endif
     if(if_station_all .and. if_thisdatatype) then
!        call stats_station%reads(trim(savestatsfile_station))
!        call stats_station%list()
        call stats_station%readwrt(trim(savestatsfile_station),trim(plotstatsfile_station_head),&
                              trim(plotstatsfile_station_data),.false.,trim(plotfile_station_list))
        call stats_station%destroy()
        if(if_station_series) then
!           call stats_station_ts%reads(trim(savestatsfile_station_ts))
!           call stats_station_ts%list()

           call stats_station_ts%readwrt(trim(savestatsfile_station_ts),trim(plotstatsfile_station_ts_head),&
                              trim(plotstatsfile_station_ts_data),.true.)
           call stats_station_ts%destroy()
        endif
     endif
!
  enddo   ! data type
!
end program read_statistic_results
