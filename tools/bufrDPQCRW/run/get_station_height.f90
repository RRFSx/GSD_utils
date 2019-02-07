Program get_station_height
!
 implicit none
!
 integer        :: ntb,iret
 integer        :: i,k

 character(8)   :: c_sid
 real           :: hdr(4)
 integer        :: ist
!
 integer,parameter :: maxsta=1000
 character(8)   :: c_allsid(maxsta)
 real           :: thdr(maxsta,4)
 integer        :: numsta
 logical        :: ifnew
 logical        :: ifexist
!
 c_allsid=""
 thdr=-999.0
 numsta=0
!
!
  inquire(file="radar_station_list.txt",exist=ifexist)
  if(ifexist) then
     open(12,file="radar_station_list.txt")
      read(12,*) numsta
      do k=1,numsta
         read(12,'(a14,10f12.4)') c_allsid(k),thdr(k,:)
      enddo
     close(12)
  endif

!
 do while (1==1)
    read(80,'(2I10,a14,10f12.4)',END=100) ntb,iret,c_sid,(hdr(i),i=1,4)
    write(*,'(2I10,a14,10f12.4)') ntb,iret,c_sid,(hdr(i),i=1,4)
    if(numsta==0) then
       numsta=1
       c_allsid(numsta)=c_sid 
       thdr(numsta,:)=hdr(:)
    else
       if(numsta > 0 .or. numsta <= maxsta) then
          ifnew=.true.
          do k=1,numsta
            if(c_allsid(k)==c_sid) then
               ifnew=.false.
            endif
          enddo
          if(ifnew) then
             numsta=numsta+1
             c_allsid(numsta)=c_sid 
             thdr(numsta,:)=hdr(:)
          endif
       else
          write(*,*) 'wrong numsta=',numsta,maxsta
          stop(123)
       endif
    endif
 enddo
100 continue

   open(12,file='radar_station_list.txt')
      write(12,*) numsta
      do k=1,numsta
         write(12,'(a14,10f12.4)') c_allsid(k),thdr(k,:)
      enddo
   close(12) 

end program
