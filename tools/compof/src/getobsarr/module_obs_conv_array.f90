module module_obs_conv_array
!
!  this module defines observation data structure and the method to 
!           read and write conventional observations.
!
!
   use kinds, only: r_kind,r_single,len_sta_name,rmissing
   use module_obs_base, only : obsbase
   use module_obs_conv_pt, only : obsbase_conv
   use module_map_utils, only : map_util

   implicit none

   public :: obs_conv_array

   private
   type,extends(obsbase) :: obsbase_xy
      real(r_single) :: xc
      real(r_single) :: yc
   endtype obsbase_xy

   type,extends(obsbase_conv) :: obs_conv_array
      integer :: num_obs_indomain=0
      type(obsbase_xy),allocatable :: obsarxy(:) 
      contains
          procedure :: writea    => write_conv_array
          procedure :: writeaxy  => write_conv_array_xy
          procedure :: readaxy   => read_conv_array_xy
          procedure :: readapt   => read_conv_array_from_pt
          procedure :: lista     => list_conv_array
          procedure :: listpstn  => list_conv_array_position
          procedure :: destroy   => destroy_conv_array
          procedure :: calculatexy => calculatexy_conv 
   end type obs_conv_array

   contains

      subroutine list_conv_array(this,minlist)
! display the content in a conventional obs list
         class(obs_conv_array) :: this
         integer, intent(in), optional :: minlist
         integer :: i,k,ii
         integer :: listnum
      
         write(*,*) '====list this observation====',this%datatype
         write(*,*) 'maximum level is=',this%maxnumlvl,this%idate
      
         if(this%n_alloc <=0) then
            return
         endif
         listnum=this%n_alloc
         if(present(minlist)) listnum=min(this%n_alloc,minlist)

         write(*,'(a,4I10)') 'n_alloc,numvar,maxnumlvl,datatype=',this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
         write(*,'(10A)') this%varnames(1:this%numvar)
         write(*,'(a,10I10)') 'ip,it,iq,ih,iu,iv=',this%ip,this%it,this%iq,this%ih,this%iu,this%iv
         write(*,'(a,10I10)') 'idate and mm =',this%idate,this%mm

         do i=1,listnum
            write(*,*) 
            write(*,'(2I10)') i,this%obsarxy(i)%numlvl
            write(*,'(a,6f10.2,2L)') this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%ele,this%obsarxy(i)%time,this%obsarxy(i)%xc,this%obsarxy(i)%yc,&
                        this%obsarxy(i)%ifquality,this%obsarxy(i)%iferror

            do k=1,this%obsarxy(i)%numlvl
               write(*,'(I10,10f10.2)') k,(this%obsarxy(i)%obs((k-1)*this%numvar+ii),ii=1,this%numvar)
               if(this%obsarxy(i)%ifquality) &
               write(*,'(I10,10f10.2)') k,(this%obsarxy(i)%quality((k-1)*this%numvar+ii),ii=1,this%numvar)
               if(this%obsarxy(i)%iferror) &
               write(*,'(I10,10f10.2)') k,(this%obsarxy(i)%error((k-1)*this%numvar+ii),ii=1,this%numvar)
            enddo
         enddo

      end subroutine list_conv_array
!
      subroutine list_conv_array_position(this)
! display the content in a conventional obs list
         class(obs_conv_array) :: this
         integer :: i,k,ii
      
         write(*,*) '====list position for observation====',this%datatype
         write(*,*) 'maximum level is=',this%maxnumlvl,this%idate
      
         if(this%n_alloc <=0) then
            return
         endif

         write(*,'(a16,4x,6a10)') "name","lon","lat","ele","time","xc","yc"
         do i=1,this%n_alloc
            write(*,'(I10,2x,a,6f10.2)') i,this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%ele,this%obsarxy(i)%time,this%obsarxy(i)%xc,this%obsarxy(i)%yc
         enddo

      end subroutine list_conv_array_position
!
      subroutine read_conv_array_xy(this,filename)
! read in obsservations from obs_conv_xy
         implicit none
         class(obs_conv_array) :: this
         character(len=*),intent(in) :: filename
        
         integer :: i,j,k,numobs
         integer :: iunit
         logical :: ifexist

         iunit=10
         inquire( file=trim(filename), EXIST=ifexist )
         if(ifexist) then
            write(*,'(a,a)') 'read_conv_array_xy:',trim(filename)
            open(iunit,file=trim(filename),form='unformatted',status='old')
               read(iunit) this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
               write(*,*) 'read in=',this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
               allocate(this%varnames(this%numvar))
               read(iunit) this%varnames(1:this%numvar)
               read(iunit) this%ip,this%it,this%iq,this%ih,this%iu,this%iv,this%idate,this%mm

               allocate(this%obsarxy(this%n_alloc))
               do i=1,this%n_alloc
                  read(iunit) this%obsarxy(i)%numlvl
                  allocate(this%obsarxy(i)%obs(this%obsarxy(i)%numlvl*this%numvar))

                  read(iunit) this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%ele,this%obsarxy(i)%time,this%obsarxy(i)%xc,this%obsarxy(i)%yc,&
                        this%obsarxy(i)%ifquality,this%obsarxy(i)%iferror
                  read(iunit) this%obsarxy(i)%obs 
                  if(this%obsarxy(i)%ifquality) then
                     allocate(this%obsarxy(i)%quality(this%obsarxy(i)%numlvl*this%numvar))
                     read(iunit) this%obsarxy(i)%quality
                  endif
                  if(this%obsarxy(i)%iferror) then
                     allocate(this%obsarxy(i)%error(this%obsarxy(i)%numlvl*this%numvar))
                     read(iunit) this%obsarxy(i)%error
                  endif
               enddo

            close(iunit)
         else
            this%n_alloc=0
            write(*,'(a,a,a)') 'read_conv_array_xy:',trim(filename),' does not exist!'
         endif

      end subroutine read_conv_array_xy
!
      subroutine read_conv_array_from_pt(this,filename)
! read in obsservations from obs_conv_pt
         implicit none
         class(obs_conv_array) :: this
         character(len=*),intent(in) :: filename
        
         integer :: i,j,k,numobs
         integer :: iunit
         logical :: ifexist

         iunit=10
         write(*,'(a,a)') 'read_conv_array_from_pt:',trim(filename)

         inquire( file=trim(filename), EXIST=ifexist )
         if(ifexist) then

            open(iunit,file=trim(filename),form='unformatted',status='old')
               read(iunit) this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
               write(*,*) 'read in=',this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
               allocate(this%varnames(this%numvar))
               read(iunit) this%varnames(1:this%numvar)
               read(iunit) this%ip,this%it,this%iq,this%ih,this%iu,this%iv,this%idate,this%mm

               allocate(this%obsarxy(this%n_alloc))
               do i=1,this%n_alloc
                  read(iunit) this%obsarxy(i)%numlvl
                  allocate(this%obsarxy(i)%obs(this%obsarxy(i)%numlvl*this%numvar))

                  read(iunit) this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%ele,this%obsarxy(i)%time,                  &
                        this%obsarxy(i)%ifquality,this%obsarxy(i)%iferror
                  read(iunit) this%obsarxy(i)%obs 
                  if(this%obsarxy(i)%ifquality) then
                     allocate(this%obsarxy(i)%quality(this%obsarxy(i)%numlvl*this%numvar))
                     read(iunit) this%obsarxy(i)%quality
                  endif
                  if(this%obsarxy(i)%iferror) then
                     allocate(this%obsarxy(i)%error(this%obsarxy(i)%numlvl*this%numvar))
                     read(iunit) this%obsarxy(i)%error
                  endif
               enddo

            close(iunit)
         else
            this%n_alloc=0
         endif

      end subroutine read_conv_array_from_pt

      subroutine write_conv_array(this,filename)
! write obs in binary file
         implicit none
         class(obs_conv_array) :: this
         character(len=*),intent(in) :: filename
        
         integer :: i, iunit
         character(len=180) :: filenameall

         iunit=10

         write(filenameall,'(a)') trim(filename)  

         if(this%n_alloc>0) then
            write(*,'(a,I8,a,I8)') '>>>write_conv_array: Write',this%n_alloc,& 
                                 ' stations for data type:',this%datatype
         else
            write(*,*) '>>>write_conv_array: NO ',this%datatype,' station'
            return
         endif

         write(*,'(a,a)') 'write data to:',trim(filenameall)
         open(iunit,file=trim(filenameall),form='unformatted')
            write(iunit) this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
            write(iunit) this%varnames(1:this%numvar)
            write(iunit) this%ip,this%it,this%iq,this%ih,this%iu,this%iv,this%idate,this%mm

            do i=1,this%n_alloc
               write(iunit) this%obsarxy(i)%numlvl
               write(iunit) this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%ele,this%obsarxy(i)%time,this%obsarxy(i)%xc,this%obsarxy(i)%yc, &
                        this%obsarxy(i)%ifquality,this%obsarxy(i)%iferror

               write(iunit) this%obsarxy(i)%obs 
               if(this%obsarxy(i)%ifquality) write(iunit) this%obsarxy(i)%quality
               if(this%obsarxy(i)%iferror) write(iunit) this%obsarxy(i)%error
            enddo

         close(iunit)

      end subroutine write_conv_array

      subroutine write_conv_array_xy(this,filename)
! write obse in binary file with xy
         implicit none
         class(obs_conv_array) :: this
         character(len=*),intent(in) :: filename
        
         integer :: i, iunit, num_obs_indomain
         character(len=180) :: filenameall

         iunit=10

         write(filenameall,'(a)') trim(filename)  

         if(this%num_obs_indomain>0) then
            write(*,'(a,I8,a,I8)') '>>>write_conv_array_xy: Write',this%num_obs_indomain,& 
                                 ' stations for data type:',this%datatype
         else
            write(*,*) '>>>write_conv_array_xy: NO ',this%datatype,' station'
            return
         endif

         write(*,'(a,a)') 'write data to:',trim(filenameall)
         open(iunit,file=trim(filenameall),form='unformatted')
            write(iunit) this%num_obs_indomain,this%numvar,this%maxnumlvl,this%datatype
            write(iunit) this%varnames(1:this%numvar)
            write(iunit) this%ip,this%it,this%iq,this%ih,this%iu,this%iv,this%idate,this%mm

            num_obs_indomain=0
            do i=1,this%n_alloc
               if(this%obsarxy(i)%xc > 0.0 .and. this%obsarxy(i)%yc >0.0) then
                  num_obs_indomain=num_obs_indomain+1
                  write(iunit) this%obsarxy(i)%numlvl
                  write(iunit) this%obsarxy(i)%name,this%obsarxy(i)%lon,this%obsarxy(i)%lat,   &
                        this%obsarxy(i)%ele,this%obsarxy(i)%time,this%obsarxy(i)%xc,this%obsarxy(i)%yc,&
                        this%obsarxy(i)%ifquality,this%obsarxy(i)%iferror

                  write(iunit) this%obsarxy(i)%obs 
                  if(this%obsarxy(i)%ifquality) write(iunit) this%obsarxy(i)%quality
                  if(this%obsarxy(i)%iferror) write(iunit) this%obsarxy(i)%error
               endif
            enddo

            if(num_obs_indomain .ne. this%num_obs_indomain) then
               write(*,*)  'Error: num_obs_indomain mismatch:', &
                            num_obs_indomain,this%num_obs_indomain
            endif
         close(iunit)

      end subroutine write_conv_array_xy

      subroutine destroy_conv_array(this)
! release memory 
         implicit none
         class(obs_conv_array) :: this
        
         integer :: i, iunit

         if(this%n_alloc > 0) then
            this%num_obs_indomain=0
            if(allocated(this%varnames)) deallocate(this%varnames)

            do i=1,this%n_alloc
!               write(*,*) i, ' deallocate: ',this%obsarxy(i)%name
               if(allocated(this%obsarxy(i)%obs)) deallocate(this%obsarxy(i)%obs)
               if(this%obsarxy(i)%ifquality) then
                  if(allocated(this%obsarxy(i)%quality)) deallocate(this%obsarxy(i)%quality)
               endif
               if(this%obsarxy(i)%iferror) then
                  if(allocated(this%obsarxy(i)%error)) deallocate(this%obsarxy(i)%error)
               endif
            enddo
            if(allocated(this%obsarxy)) deallocate(this%obsarxy)
         else
            write(*,*) 'No data in this data target'
         endif
         this%n_alloc=0

      end subroutine destroy_conv_array

      subroutine calculatexy_conv(this,mapin)
! calculate grid  coordinate
         implicit none
         class(obs_conv_array) :: this
         type(map_util) ,intent(in) :: mapin
         integer :: i
         real(r_single) :: xc,yc,rlon,rlat

         this%num_obs_indomain=0
         if(this%n_alloc>0) then
            write(*,*) "fill in the grid coordinate info xc,yc"
            do i=1,this%n_alloc
                call mapin%tll2xy(this%obsarxy(i)%lon, this%obsarxy(i)%lat, &
                                xc,yc)
                if( (xc >=1 .and. xc <=mapin%nlon) .and. &
                    (yc >=1 .and. yc <=mapin%nlat) ) then
                   this%obsarxy(i)%xc=xc
                   this%obsarxy(i)%yc=yc
                   this%num_obs_indomain=this%num_obs_indomain+1
                else
                   this%obsarxy(i)%xc=-1
                   this%obsarxy(i)%yc=-1
                endif
            enddo
         else
            write(*,*) 'calculatxy_conv: No observations in this array'
         endif

      end subroutine calculatexy_conv

end module module_obs_conv_array
