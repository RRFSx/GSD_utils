module module_fcstobs

   use kinds, only: r_kind,r_single,len_sta_name,rmissing
   use module_obs_conv_array, only : obs_conv_array
   use module_obs_base, only : obslocation
   use module_map_utils, only : map_util

   implicit none

   public :: fcstobs_array
   public :: fcstobsbase 
   private
   type, extends(obslocation) :: fcstobsbase
      real(r_single)    :: xc
      real(r_single)    :: yc
      real(r_single)    :: time      ! forecast time  (hour)
      integer :: numlvl              ! number of observation levels
      real(r_single),allocatable :: obs(:)  ! observation value=numlvl*numvar
   end type fcstobsbase

   type :: fcstobs_array
      integer :: num_obs        ! number of observations 
      integer :: num_var        ! number of fields 
      integer :: numlvl_model   ! number of model levels
      integer :: inidate        ! YYYYMMDD
      integer :: inimm          ! HHMM
      integer :: ip,it,iq,ih,iu,iv  ! index of observation
      character(len=20), allocatable :: varname(:)
      type(fcstobsbase), allocatable :: fcstobs(:) ! num_obs
      real(r_single), allocatable    :: rvcord(:,:)  ! num_obs
      logical :: if_save_4sfcpoints
      real(r_single), allocatable    :: value4(:,:,:) ! num_obs,num_var,4
      contains
         procedure :: initial => initial_fcstobs
         procedure :: destroy => destroy_fcstobs
         procedure :: listall => list_fcstobs
         procedure :: readall => read_fcstobs
         procedure :: writeall=> write_fcstobs
         procedure :: interp  => interp_3d
         procedure :: convert_theta2t
         procedure :: rotate_uv
   end type

   contains
      
      subroutine initial_fcstobs(this,obsarr,num_levels,if_save_4sfcpoints)
! read in station info
         implicit none
         class(fcstobs_array) :: this
         type(obs_conv_array) ,intent(in) :: obsarr
         integer,intent(in) :: num_levels
         logical, intent(in), optional :: if_save_4sfcpoints

         integer :: i,k,idx,ivar

         if(obsarr%numvar > 0 .and. num_levels > 0 .and. obsarr%n_alloc > 0) then
            this%num_var=obsarr%numvar
            this%numlvl_model=num_levels
            this%num_obs=obsarr%n_alloc

            if(allocated(this%varname)) deallocate(this%varname)
            allocate(this%varname(this%num_var))
            do i=1,this%num_var
               this%varname(i)=trim(obsarr%varnames(i))
            enddo

            this%ip=obsarr%ip
            this%it=obsarr%it
            this%iq=obsarr%iq
            this%ih=obsarr%ih
            this%iu=obsarr%iu
            this%iv=obsarr%iv


            if(allocated(this%fcstobs)) deallocate(this%fcstobs)
            allocate(this%fcstobs(this%num_obs))
            do i=1,this%num_obs
               this%fcstobs(i)%name=obsarr%obsarxy(i)%name
               this%fcstobs(i)%lon=obsarr%obsarxy(i)%lon
               this%fcstobs(i)%lat=obsarr%obsarxy(i)%lat
               this%fcstobs(i)%ele=obsarr%obsarxy(i)%ele
               this%fcstobs(i)%time=obsarr%obsarxy(i)%time
               this%fcstobs(i)%numlvl=obsarr%obsarxy(i)%numlvl
               this%fcstobs(i)%xc=obsarr%obsarxy(i)%xc
               this%fcstobs(i)%yc=obsarr%obsarxy(i)%yc
               if(allocated(this%fcstobs(i)%obs)) deallocate(this%fcstobs(i)%obs)
               allocate(this%fcstobs(i)%obs(this%num_var*this%fcstobs(i)%numlvl))
               this%fcstobs(i)%obs=rmissing
               do k=1,this%fcstobs(i)%numlvl
                  idx=(k-1)*this%num_var
                  this%fcstobs(i)%obs(idx+this%ip)=obsarr%obsarxy(i)%obs((k-1)*obsarr%numvar+obsarr%ip)
               enddo
            enddo
            if(allocated(this%rvcord)) deallocate(this%rvcord)
            allocate(this%rvcord(this%num_obs,this%numlvl_model))

            this%if_save_4sfcpoints=.false.
            if(present(if_save_4sfcpoints)) this%if_save_4sfcpoints=if_save_4sfcpoints
            if(this%if_save_4sfcpoints) then
               if(allocated(this%value4)) deallocate(this%value4)
               allocate(this%value4(this%num_obs,this%num_var,4))
               this%value4=rmissing
            endif
         endif
      end subroutine initial_fcstobs

      subroutine destroy_fcstobs(this)
! read in station info
         implicit none
         class(fcstobs_array) :: this

         integer :: i

         if(allocated(this%varname)) deallocate(this%varname)

         do i=1,this%num_obs
            if(allocated(this%fcstobs(i)%obs)) deallocate(this%fcstobs(i)%obs)
         enddo
         if(allocated(this%fcstobs)) deallocate(this%fcstobs)

         if(allocated(this%rvcord)) deallocate(this%rvcord)

         if(allocated(this%value4)) deallocate(this%value4)

         this%num_obs=0
         this%num_var=0
         this%numlvl_model=0
         this%if_save_4sfcpoints=.false.

      end subroutine destroy_fcstobs

      subroutine rotate_uv(this,mapin)
! read in station info
         implicit none
         class(fcstobs_array) :: this
         type(map_util),intent(in) :: mapin

         integer :: iu,iv
         integer :: i,j,k,idx
         integer :: inx,iny
         real :: uin,vin,uout,vout
         real :: rlon,xc,yc

         if(this%num_var> 0 .and. this%num_obs >0) then

            iu=this%iu
            iv=this%iv
            if(iv>0.and.iu>0) then
               do i=1,this%num_obs
                  xc=this%fcstobs(i)%xc
                  yc=this%fcstobs(i)%yc
                  rlon=this%fcstobs(i)%lon
                  do k=1,this%fcstobs(i)%numlvl
                      idx=(k-1)*this%num_var
                      uin=this%fcstobs(i)%obs(idx+iu)
                      vin=this%fcstobs(i)%obs(idx+iv)
                      if(uin > rmissing+1.0 .and. vin > rmissing+1.0) then
                         call mapin%rotate_wind_xy2ll(uin,vin,uout,vout,rlon,xc,yc)
                         this%fcstobs(i)%obs(idx+iu)=uout 
                         this%fcstobs(i)%obs(idx+iv)=vout
                      endif
                  enddo
                  if(this%if_save_4sfcpoints) then
                     inx=int(xc)
                     iny=int(yc)
                     do k=1,4
                        uin=this%value4(i,iu,k)
                        vin=this%value4(i,iv,k)
                        if(uin > rmissing+1.0 .and. vin > rmissing+1.0) then
                           if(k==1) call mapin%rotate_wind_xy2ll(uin,vin,uout,vout,rlon,float(inx),float(iny))
                           if(k==2) call mapin%rotate_wind_xy2ll(uin,vin,uout,vout,rlon,float(inx+1),float(iny))
                           if(k==3) call mapin%rotate_wind_xy2ll(uin,vin,uout,vout,rlon,float(inx),float(iny+1))
                           if(k==4) call mapin%rotate_wind_xy2ll(uin,vin,uout,vout,rlon,float(inx+1),float(iny+1))
                           this%value4(i,iu,k)=uout 
                           this%value4(i,iv,k)=vout
                        endif
                     enddo
                  endif
               enddo
            else
               write(*,*) 'cannot find V and U'
            endif
         else
            write(*,*) 'fcst obs array is empty',this%num_var,this%num_obs
         endif

      end subroutine rotate_uv

      subroutine convert_theta2t(this)
! read in station info
         implicit none
         class(fcstobs_array) :: this

         integer :: it,ip,idx
         integer :: i,j,k
         real(r_kind) :: rd,cp,rd_over_cp

         
         rd     = 2.8705e+2_r_kind
         cp     = 1.0046e+3_r_kind  !  specific heat of air @pressure (J/kg/K)
         rd_over_cp = rd/cp

         if(this%num_var> 0 .and. this%num_obs >0) then

            it=this%it
            ip=this%ip
            if(it>0.and.ip>0) then
               do i=1,this%num_obs
                  do k=1,this%fcstobs(i)%numlvl
                      idx=(k-1)*this%num_var
                      if(this%fcstobs(i)%obs(it+idx) > rmissing+1.0 .and.  &
                         this%fcstobs(i)%obs(ip+idx) > rmissing+1.0) then
                         this%fcstobs(i)%obs(it+idx)=this%fcstobs(i)%obs(it+idx)* &
                                     (this%fcstobs(i)%obs(ip+idx)/1000.0)**rd_over_cp &
                                     -273.15
                      endif
                  enddo
                  if(this%if_save_4sfcpoints) then
                     do k=1,4
                        if(this%value4(i,it,k)  > rmissing+1.0 .and.  &
                           this%value4(i,ip,k)  > rmissing+1.0 ) then
                           this%value4(i,it,k)=this%value4(i,it,k)*    &
                                            (this%value4(i,ip,k)/1000.0)**rd_over_cp-273.15
                            
                        endif
                     enddo
                  endif
               enddo
            else
               write(*,*) 'cannot find T and P'
            endif
         else
            write(*,*) 'fcst obs array is empty',this%num_var,this%num_obs
         endif

      end subroutine convert_theta2t

      subroutine read_fcstobs(this,filename)
! read in station info
         implicit none
         class(fcstobs_array) :: this
         character(len=*),intent(in) :: filename

         integer :: i,j,k
         integer :: unitout
         logical :: ifexist

         unitout=11

         inquire( file=trim(filename), EXIST=ifexist )
         if(ifexist) then

            open(unitout,file=trim(filename), form='unformatted',status='old')
               read(unitout) this%num_var,this%num_obs,this%numlvl_model, &
                          this%inidate, this%inimm,this%if_save_4sfcpoints
               read(unitout) this%ip,this%it,this%iq,this%ih,this%iu,this%iv

               if(this%num_var> 0 .and. this%num_obs >0) then

                  if(allocated(this%varname)) deallocate(this%varname)
                  allocate(this%varname(this%num_var))
                  read(unitout) this%varname

                  if(allocated(this%fcstobs)) deallocate(this%fcstobs)
                  allocate(this%fcstobs(this%num_obs))

                  do i=1,this%num_obs
                     read(unitout) this%fcstobs(i)%name,this%fcstobs(i)%lon, &
                                this%fcstobs(i)%lat,this%fcstobs(i)%ele,  &
                                this%fcstobs(i)%time,this%fcstobs(i)%numlvl, &
                                this%fcstobs(i)%xc,this%fcstobs(i)%yc    
                     if(allocated(this%fcstobs(i)%obs)) deallocate(this%fcstobs(i)%obs)
                     allocate(this%fcstobs(i)%obs(this%num_var*this%fcstobs(i)%numlvl))
                     read(unitout) this%fcstobs(i)%obs
                  enddo
                  if(this%if_save_4sfcpoints) then
                     if(allocated(this%value4)) deallocate(this%value4)
                     allocate(this%value4(this%num_obs,this%num_var,4))
                     read(unitout) this%value4
                  endif
               else
                  write(*,*) 'fcst obs array is empty',this%num_var,this%num_obs
               endif
            close(unitout)
         else
            this%num_var=0
            this%num_obs=0
         endif
      end subroutine read_fcstobs

      subroutine write_fcstobs(this,filename)
! write forecast obs
         implicit none
         class(fcstobs_array) :: this
         character(len=*),intent(in) :: filename
        
         integer :: i,j,k
         integer :: unitout

         unitout=11
         if(this%num_var> 0 .and. this%num_obs >0) then
            open(unitout,file=trim(filename), form='unformatted')
               write(unitout) this%num_var,this%num_obs,this%numlvl_model, &
                          this%inidate, this%inimm,this%if_save_4sfcpoints
               write(unitout) this%ip,this%it,this%iq,this%ih,this%iu,this%iv

               write(unitout) this%varname

               do i=1,this%num_obs
                  write(unitout) this%fcstobs(i)%name,this%fcstobs(i)%lon, &
                                this%fcstobs(i)%lat,this%fcstobs(i)%ele,  &
                                this%fcstobs(i)%time,this%fcstobs(i)%numlvl, &
                                this%fcstobs(i)%xc,this%fcstobs(i)%yc    
                  write(unitout) this%fcstobs(i)%obs
               enddo

               if(this%if_save_4sfcpoints) then
                  write(unitout) this%value4
               endif
            
            close(unitout)
         else
            write(*,*) 'fcst obs array is empty',this%num_var,this%num_obs
         endif
      end subroutine write_fcstobs

      subroutine list_fcstobs(this,listnumin)
! read in station info
         implicit none
         class(fcstobs_array) :: this
         integer,optional,intent(in) :: listnumin
        
         integer :: i,j,k,idx
         integer :: listnum

         listnum=this%num_obs
         if(present(listnumin)) listnum=min(listnum,listnumin)
         if(this%num_var> 0 .and. listnum >0) then
            
            write(*,'(5I12,L)') this%num_var,this%num_obs,this%numlvl_model, &
                          this%inidate, this%inimm,this%if_save_4sfcpoints
            write(*,'(a,6I4)') 'ip,it,iq,ih,iu,iv=',this%ip,this%it,this%iq,this%ih,this%iu,this%iv

            write(*,'(20x,10a20)') (this%varname(j),j=1,this%num_var)
            do i=1,listnum
               write(*,'(I5,a,4F20.3,I20,2F20.3)') i,this%fcstobs(i)%name,this%fcstobs(i)%lon, &
                                this%fcstobs(i)%lat,this%fcstobs(i)%ele,  &
                                this%fcstobs(i)%time,this%fcstobs(i)%numlvl, &
                                this%fcstobs(i)%xc,this%fcstobs(i)%yc    
               do k=1,this%fcstobs(i)%numlvl
                  idx=(k-1)*this%num_var
                  write(*,'(I10,10f20.3)') k,(this%fcstobs(i)%obs(j+idx),j=1,this%num_var)
               enddo
            enddo

            if(this%if_save_4sfcpoints) then
               write(*,*) 'list 4 surronding points==='
               do i=1,listnum
                  do k=1,4
                     write(*,'(I10,10f12.3)') k,(this%value4(i,j,k),j=1,this%num_var)
                  enddo
               enddo
            endif
         else
            write(*,*) 'fcst obs array is empty',this%num_var,this%num_obs
         endif

      end subroutine list_fcstobs

      subroutine interp_3d(this,field,nx,ny,nz,var)
! read in station info

         implicit none
         class(fcstobs_array) :: this
         real(r_single), intent(inout)  :: field(nx,ny,nz)
         character(len=*),intent(in) :: var

         integer :: i,ivar,j,k,kk
         integer :: nx,ny,nz
         real :: xc,yc,ele
         real(r_single),allocatable :: fieldz(:),pfcst(:)
         real(r_single) :: fobs, pobs

         real(r_single) :: w11,w12,w21,w22,rx,ry,w1,w2
         real(r_single) :: f11,f12,f21,f22,f1,f2
         integer :: inx,jny
         integer :: idx,ip

!
         if(this%num_obs ==0) return
!
! check if forecast is available
         allocate(fieldz(nz))
         allocate(pfcst(nz))
         if(trim(var) == 'P' .and. nz .ne.this%numlvl_model) then
            write(*,*) ' vertical level mismatch =',nz,this%numlvl_model
            stop 123
         endif

         if(this%num_obs >0 .and. this%num_var >0) then

            ivar=0
            if('P' == trim(var) .or. 'Ps' == trim(var) ) ivar=this%ip
            if('T' == trim(var) .or. 'T2' == trim(var)) ivar=this%it
            if('Q' == trim(var) .or. 'Q2' == trim(var) ) ivar=this%iq
            if('H' == trim(var)) ivar=this%ih
            if('U' == trim(var) .or. 'U10' == trim(var)) ivar=this%iu
            if('V' == trim(var) .or. 'V10' == trim(var)) ivar=this%iv
            if(ivar<=0 .or. ivar > this%num_var) then
               write(*,*) 'Wrong variable ID for ',trim(var),ivar
               stop 123
            endif

            do i=1,this%num_obs
                xc=this%fcstobs(i)%xc
                yc=this%fcstobs(i)%yc
                ele=this%fcstobs(i)%ele
                if( (xc >0.9 .and. xc < nx) .and.     &
                    (yc >0.9 .and. yc < ny) ) then

                    inx=int(xc)
                    jny=int(yc)
                    rx=xc-float(inx)
                    ry=yc-float(jny)
                    w11=(1.0-rx)*(1.0-ry)
                    w12=rx*(1.0-ry)
                    w21=(1.0-rx)*ry
                    w22=rx*ry
                    do k=1,nz
                       f11=field(inx,jny,k)
                       f12=field(inx+1,jny,k)
                       f21=field(inx,jny+1,k)
                       f22=field(inx+1,jny+1,k)
                       fieldz(k)=f11*w11+f12*w12+f21*w21+f22*w22
                    enddo

                   if(trim(var) == 'P') then
                      do k=1,this%numlvl_model
                         this%rvcord(i,k)=fieldz(k)
                      enddo
                      if(this%if_save_4sfcpoints) then
                         this%value4(i,ivar,1)=field(inx,jny,1)
                         this%value4(i,ivar,2)=field(inx+1,jny,1)
                         this%value4(i,ivar,3)=field(inx,jny+1,1)
                         this%value4(i,ivar,4)=field(inx+1,jny+1,1)
                      endif
                   else
!
                      if(nz==1) then 
                         this%fcstobs(i)%obs(ivar)=fieldz(1)
                         if(this%if_save_4sfcpoints) then
                            this%value4(i,ivar,1)=field(inx,jny,1)
                            this%value4(i,ivar,2)=field(inx+1,jny,1)
                            this%value4(i,ivar,3)=field(inx,jny+1,1)
                            this%value4(i,ivar,4)=field(inx+1,jny+1,1)
                         endif
                         if(ivar==this%iq) then
                            this%fcstobs(i)%obs(ivar)=this%fcstobs(i)%obs(ivar)*1000.0
                            if(this%if_save_4sfcpoints) then
                               this%value4(i,ivar,:)=this%value4(i,ivar,:)*1000.0
                            endif
                         endif
                      else
                         pfcst(:)=this%rvcord(i,:)  ! get pressure

                         do k=1,this%fcstobs(i)%numlvl
                            idx=(k-1)*this%num_var
                            pobs=this%fcstobs(i)%obs(this%ip+idx)

                            fobs=0.0
                            if( pfcst(1) < pobs ) then
                               fobs=fieldz(1)
                               if(this%if_save_4sfcpoints) then
                                  this%value4(i,ivar,1)=field(inx,jny,1)
                                  this%value4(i,ivar,2)=field(inx+1,jny,1)
                                  this%value4(i,ivar,3)=field(inx,jny+1,1)
                                  this%value4(i,ivar,4)=field(inx+1,jny+1,1)
                               endif
                            elseif( pfcst(this%numlvl_model) > pobs ) then
                               fobs=fieldz(this%numlvl_model)
                               if(this%if_save_4sfcpoints) then
                                  this%value4(i,ivar,1)=field(inx,jny,this%numlvl_model)
                                  this%value4(i,ivar,2)=field(inx+1,jny,this%numlvl_model)
                                  this%value4(i,ivar,3)=field(inx,jny+1,this%numlvl_model)
                                  this%value4(i,ivar,4)=field(inx+1,jny+1,this%numlvl_model)
                               endif
                            else
                               do kk=1,this%numlvl_model-1
                                  if(pfcst(kk)>=pobs.and.pfcst(kk+1) <pobs) then    
                                     ip=kk
                                     exit
                                  endif
                               enddo
                               w2=log(pfcst(ip))-log(pfcst(ip+1))
                               w1=(log(pfcst(ip))-log(pobs))/w2
                               w2=1.0-w1
                               fobs=fieldz(ip)*w2+fieldz(ip+1)*w1
                               if(this%if_save_4sfcpoints) then
                                  kk=ip
                                  if(w1 > w2) kk=ip+1
                                  this%value4(i,ivar,1)=field(inx,jny,kk)
                                  this%value4(i,ivar,2)=field(inx+1,jny,kk)
                                  this%value4(i,ivar,3)=field(inx,jny+1,kk)
                                  this%value4(i,ivar,4)=field(inx+1,jny+1,kk)
                               endif
                            endif
                            if(ivar==this%iq) then
                               fobs=fobs*1000.0
                               if(this%if_save_4sfcpoints) then
                                  this%value4(i,ivar,:)=this%value4(i,ivar,:)*1000.0
                               endif
                            endif
                            this%fcstobs(i)%obs(ivar+idx)=fobs
                         enddo
                      endif
                   endif
                else
                   this%fcstobs(i)%obs=rmissing
                endif
            enddo
         else
            write(*,*) 'fcst obs array is empty',this%num_obs,this%num_var
         endif

      end subroutine interp_3d

end module module_fcstobs
