module module_hashsearch

   use kinds, only: r_kind,r_single,len_sta_name,rmissing

   implicit none

   public :: hashtable
   private

   type :: hashindex
     type(hashindex), pointer :: next => NULL() 
     integer :: hindex=0
     character(len=len_sta_name) :: hname
   end type

   type :: hashtable
      integer :: hashfactor         ! part of hash function to resuce the table size 
      integer :: domainx,domainy    ! dimension of original grid domain
      integer :: dimx,dimy          ! dimension of hash table
      integer :: dimxy
      type(hashindex),allocatable :: htable(:)
      contains
         procedure :: initial => initial_hashtable
         procedure :: destroy => destroy_hashtable
         procedure :: list    => list_hashtable
         procedure :: search  => search_hashtable
         procedure :: make    => make_hashtable
         procedure :: findkey => hash_function
   end type hashtable
   contains
      
      subroutine initial_hashtable(this,domainx,domainy,hashfactor)
! initial hash table
         implicit none
         class(hashtable) :: this
         integer, intent(in) :: domainx,domainy,hashfactor

         this%hashfactor=hashfactor
         this%dimx=domainx/hashfactor
         this%dimy=domainy/hashfactor
         this%dimxy=this%dimx*this%dimy
         if(allocated(this%htable)) deallocate(this%htable)
         allocate(this%htable(this%dimxy))

      end subroutine initial_hashtable

      subroutine destroy_hashtable(this)
! destroy hash table
         implicit none
         class(hashtable) :: this
         integer :: i
         type(hashindex),pointer :: thisindex=>NULL(),thisindexnext=>NULL()

         do i=1,this%dimxy
            if(this%htable(i)%hindex>0 .and. this%htable(i)%hindex<=this%dimxy) then 
               thisindex=>this%htable(i)%next
               this%htable(i)%next=>NULL()
               do while(associated(thisindex))
                  thisindexnext=>thisindex%next
                  thisindex%next=>NULL()
                  thisindex%hindex=0
                  thisindex%hname=""
                  thisindex => thisindexnext
               enddo
            endif    
         enddo
         if(allocated(this%htable)) deallocate(this%htable)

      end subroutine destroy_hashtable

      subroutine hash_function(this,ix,iy,hkey)
! hash function
         implicit none
         class(hashtable) :: this
         integer,intent(in) :: ix,iy
         integer,intent(inout) :: hkey

         hkey=iy/this%hashfactor*this%dimx+ix/this%hashfactor
         hkey=max(1,min(hkey,this%dimxy))

      end subroutine hash_function
!
      subroutine make_hashtable(this,name,ix,iy,hin)
! fill hash table
         implicit none
         class(hashtable) :: this
         character(len=*),intent(in) :: name
         integer,intent(in) :: ix,iy
         integer,intent(inout) :: hin   ! if =0, find a name here
        
         integer :: i,j,iad
         type(hashindex),pointer :: thisindex=>NULL(),thistail=>NULL()

         if(this%dimxy >0) then
            call this%findkey(ix,iy,i)
            if(i > 0 .and. i<=this%dimxy) then
! 
               if(this%htable(i)%hindex==0) then
                  this%htable(i)%hindex=hin
                  this%htable(i)%hname=name
                  this%htable(i)%next=>NULL()
               else
                  if(trim(this%htable(i)%hname)==trim(name)) then
                      hin=0
                  else
                     thisindex => this%htable(i)%next
                     if(associated(thisindex)) then
                        do while(associated(thisindex))
                           if(trim(thisindex%hname)==trim(name)) hin=0
                           thistail=>thisindex
                           thisindex => thisindex%next
                        enddo

                        if(hin > 0) then  ! add new item
                           allocate(thisindex)
                           thisindex%hindex=hin
                           thisindex%hname=name
                           thisindex%next=>NULL()
                           thistail%next=>thisindex
                        endif
                     else
                        allocate(thisindex)
                        thisindex%hindex=hin
                        thisindex%hname=name
                        thisindex%next=>NULL()
                        this%htable(i)%next=>thisindex
                     endif
                  endif
               endif
            else
               write(*,*) 'ERROR: invalid hash table index',i
               stop 234
            endif
         else
            write(*,*) 'hash table is empty'
            stop 234
         endif

      end subroutine make_hashtable
!
      subroutine search_hashtable(this,name,ix,iy,hindex)
! search index based on key from hash table
         implicit none
         class(hashtable) :: this
         character(len=*),intent(in) :: name
         integer,intent(in) :: ix,iy
         integer,intent(inout) :: hindex
        
         integer :: i
         type(hashindex),pointer :: thisindex=>NULL()

         hindex=0
         if(this%dimxy >0) then
            call this%findkey(ix,iy,i)
            if(i > 0 .and. i<=this%dimxy) then
               if(trim(this%htable(i)%hname)==trim(name)) then
                  hindex=this%htable(i)%hindex
               else
                  thisindex => this%htable(i)%next
                  do while(associated(thisindex))
                     if(trim(thisindex%hname)==trim(name)) hindex=thisindex%hindex
                     thisindex => thisindex%next
                  enddo
               endif
               if(hindex==0) then
                  write(*,*) 'WARNING: no found in hash table',name
               endif
            else
               write(*,*) 'ERROR: invalid hash table index',i
               stop 234
            endif
         else
            write(*,*) 'hash table is empty'
            stop 234
         endif

      end subroutine search_hashtable
!
      subroutine list_hashtable(this)
! list hash table
         implicit none
         class(hashtable) :: this
        
         integer :: i,ii
         type(hashindex),pointer :: thisindex=>NULL()

         if(this%dimxy >0) then
            
            write(*,*) 'The size of hash table is ',this%dimxy
            do i=1,this%dimxy
               if(this%htable(i)%hindex >0) then
                  write(*,'(I10,a,I10,2x,a,I20)') i,' hindex=',this%htable(i)%hindex, &
                                       this%htable(i)%hname,loc(this%htable(i)%next)
               
                  thisindex => this%htable(i)%next
                  do while(associated(thisindex))
                     write(*,'(a10,a,I10,2x,a,I20)') ' |-->',' hindex=',thisindex%hindex, &
                                            thisindex%hname,loc(thisindex)
                     thisindex => thisindex%next
                  enddo
               endif
            enddo
         else
            write(*,*) 'hash table is empty'
         endif

      end subroutine list_hashtable

end module module_hashsearch
