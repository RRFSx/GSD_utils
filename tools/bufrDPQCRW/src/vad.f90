!
!----------------------------------------------------------------------X
!
   subroutine vad(vel,aza,el,num_rng,num_ang,rng,cntmn,tgap,rmsmx, &
        uvad,vvad,err,con,rmissing,irad,nrad)
   implicit none
!
!  FUNCTION - COMPUTE MEAN RADIAL VELOCITY USING VAD ANALYSIS
!             F(I,J,OUT)=A0*A1*COS(A)*A2*COS(2*A)+B1*SIN(A)+B2*SIN(2*A)
!
!     VEL    - INPUT RADIAL VELOCITY FIELD. FIRST INDEX IS IN RANGE,
!                  SECOND IS FOR AZIMUTH ANGLE.
!     AZA    - ARRAY OF AZIMUTH ANGLES
!     EL     - ELEVATION ANGLE
!     NUM_RNG- NUMBER OF RANGE GATES
!     NUM_ANG- NUMBER OF AZIMUTH ANGLES
!     RNG    - RANGE (KM) TO EACH GATE OF DATA
!     CNTMN  - MINIMUM NUMBER OF GOOD DATA POINTS FOR VAD ANALYSIS
!     TGAP   - MAXIMUM ALLOWED AZIMUTH GAP         "   "      "	"
!     RMSMX  -    "       "    RMS DIFFERENCE BETWEEN INPUT AND VAD WINDS
   
!
!  VAD MEAN OUTPUT QUANTITIES
!
!     UVAD,VVAD  - HORIZONTAL WINDS       FOR THE ITH RANGE GATE
!     Z     - HEIGHT OF U,V POINT
!     CON    -     "      CONVERGENCE  "   "   "    "     "
!     ERR    - RMS DIFFERENCE BETWEEN MEASURED RADIAL VELOCITIES
!              AND VAD ANALYSIS
!
   
   integer irad, nrad, num_rng, num_ang
   
   real el, cntmn, tgap, rmsmx, rmissing
   
   real vel(num_rng,num_ang),aza(num_ang), &
      rng(num_rng),uvad(num_rng,nrad),vvad(num_rng,nrad), &
      err(num_rng),con(num_rng)
   
!     Local variables
   
   integer i, j
   
   real torad,todeg,sine,cose,z,a0,a1,a2,b1,b2,cnt,gapmx,gapmn,alft, &
        ang,angr,dat1,gap,sumsqdif,cntdif,vr_vad,vr_inp,rmsdif
   
   data torad,todeg/0.017453293,57.29577951/
   
   sine=sin(torad*el)
   cose=cos(torad*el)
   
   
!     DO 10 I=1,NUM_RNG-1
   do 10 i=1,num_rng
      uvad(i,irad) = rmissing
      vvad(i,irad) = rmissing
      con(i)= rmissing
      err(i)= rmissing
 10   CONTINUE
   
   
   
!
   
   
   do 100 i=1,num_rng
      if(rng(i).le.0.0)go to 100
      z=rng(i)*sine
      a0=0.0
      a1=0.0
      a2=0.0
      b1=0.0
      b2=0.0
      cnt=0.0
      gapmx=-999.0
      gapmn=999.0
      alft=rmissing
   
!        LOOP OVER ALL ANGLES TO CALCULATE THE FOURIER
!        COEFFICIENTS FOR ZEROTH, FIRST AND SECOND HARMONICS.
!
      do 90 j=1,num_ang
         ang=aza(j)
         angr=ang*torad
         dat1=vel(i,j)
         if(dat1.gt.rmissing)then
            a0=a0+dat1
            a1=a1+dat1*cos(angr)
            a2=a2+dat1*cos(angr*2.0)
            b1=b1+dat1*sin(angr)
!            print*,'b1=',j,b1,dat1,angr,alft,cnt
            b2=b2+dat1*sin(angr*2.0)
            cnt=cnt+1.0
            if(alft.le.rmissing)then
               alft=ang
            else
               gap=abs(ang-alft)
               alft=ang
               if(gap.gt.180.0)gap=abs(gap-360.0)
               if(gap.lt.gapmn)gapmn=gap
               if(gap.gt.gapmx)gapmx=gap
            end if
         end if
 90      CONTINUE
   
!     FROM FOURIER COEFFICIENTS:  CALCULATE THE VAD MEAN PARAMETERS
!     AND ANALYTIC WINDS FOR THIS RANGE, THEN GO ON TO NEXT RANGE
!
!     write (6,fmt='(a,I3,a,I4,a)') 'RANGE GATE: ',i, &
!          ' LARGEST GAP: ',int(gapmx),' deg.'
!     write (6,fmt='(a,a,I3)') 'NUMBER OF GOOD DATA POINTS ', &
!          'FOR VAD ANALYSIS: ',int(cnt)
   
      if(cnt.ge.cntmn.and.gapmx.le.tgap)then
         a0=    a0/cnt
         a1=2.0*a1/cnt
         a2=2.0*a2/cnt
         b1=2.0*b1/cnt
         b2=2.0*b2/cnt
!	 print*,'a1=,b1=',a1,b1
         uvad(i,irad)=(b1/cose)
         vvad(i,irad)=(a1/cose)
         con(i)=-2.0*a0/(rng(i)*cose*cose)
         sumsqdif=0.0
         cntdif=0.0
         do 92 j=1,num_ang
            ang=aza(j)
   
            angr=ang*torad
            vr_vad=a0+a1*cos(angr)+a2*cos(angr*2.0) &
                            +b1*sin(angr)+b2*sin(angr*2.0)
            vr_inp=vel(i,j)
   
            if(vr_inp.gt.rmissing)then
               cntdif=cntdif+1.0
               sumsqdif=sumsqdif+(vr_inp-vr_vad)**2
            end if
 92         CONTINUE
   
!           CHECK RMS DIFFERENCE BETWEEN VAD AND INPUT WINDS.
   
!
         if(cntdif.gt.cntmn)then
            rmsdif=sqrt(sumsqdif/cntdif)
            err(i)=rmsdif
         end if
         if(cntdif.le.cntmn.or.rmsdif.gt.rmsmx)then
            uvad(i,irad) =rmissing
            vvad(i,irad) =rmissing
            con(i)=rmissing
         end if
      end if
 100  CONTINUE
   
   
   return
end
