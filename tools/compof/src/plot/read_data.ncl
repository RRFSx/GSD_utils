
  datemm = fbinrecread(filedata, 0, (/2/), "integer")
  print("   date="+datemm(0) + "     minute="+datemm(1))
  numvar=dim(2)
  nfcst=dim(1)
  numstation=dim(3)
  nlvl=dim(0)

  time_ts=new((/numtimelevel,2/),"integer")
  no_ts=new((/numtimelevel/),"integer")
  rmse_ts=new((/numstation,nfcst,nlvl,numvar,numtimelevel/),"float")
  bias_ts=new((/numstation,nfcst,nlvl,numvar,numtimelevel/),"float")
  numpair_ts=new((/numstation,nfcst,nlvl,numvar,numtimelevel/),"integer")
  rmse_ts@_FillValue=-99999.0
  bias_ts@_FillValue=-99999.0
  numpair_ts@_FillValue=-99999
  
  rmse   = fbinrecread(filedata, 1, (/ numstation,nfcst,nlvl,numvar /), "float")
  bias   = fbinrecread(filedata, 2, (/ numstation,nfcst,nlvl,numvar /), "float")
  numpair= fbinrecread(filedata, 3, (/ numstation,nfcst,nlvl,numvar /), "integer")

  no_ts(0)=0
  time_ts(0,:)=datemm
  rmse_ts(:,:,:,:,0)= rmse
  bias_ts(:,:,:,:,0) = bias
  numpair_ts(:,:,:,:,0)=numpair
  if( numstation .gt. 1) then
     do i = 0,numstation-1
     do j = 0,nfcst-1
     do k = 0,nlvl-1
     do n = 0,numvar-1
        if(numpair(i,j,k,n) .gt. 0) then
           bias_ts(i,j,k,n,0)=rmse(i,j,k,n)-bias(i,j,k,n)
        end if
     end do
     end do
     end do
     end do
  end if

  if( numtimelevel .gt. 0 ) then
     do ii = 1 , numtimelevel-1
        datemm = fbinrecread(filedata, 0 + ii*4, (/2/), "integer")
        print("   date="+datemm(0) + "     minute="+datemm(1))
        rmse   = fbinrecread(filedata, 1 + ii*4, (/ numstation,nfcst,nlvl,numvar /), "float")
        bias   = fbinrecread(filedata, 2 + ii*4, (/ numstation,nfcst,nlvl,numvar /), "float")
        numpair= fbinrecread(filedata, 3 + ii*4, (/ numstation,nfcst,nlvl,numvar /), "integer")

        no_ts(ii)=ii
        time_ts(ii,:)=datemm
        rmse_ts(:,:,:,:,ii)= rmse
        bias_ts(:,:,:,:,ii) = bias
        numpair_ts(:,:,:,:,ii)=numpair
        if( numstation .gt. 1) then
           do i = 0,numstation-1
           do j = 0,nfcst-1
           do k = 0,nlvl-1
           do n = 0,numvar-1
              if(numpair(i,j,k,n) .gt. 0) then
                 bias_ts(i,j,k,n,ii)=rmse(i,j,k,n)-bias(i,j,k,n)
              end if
           end do
           end do
           end do
           end do
        end if
     end do
     timestring_ts=new((/numtimelevel/),"string")
     no_ts@units="hours since 2006-08-16 01:00:00"
     do ii = 0 , numtimelevel-1
        yyyy=time_ts(ii,0)/1000000
        dd=time_ts(ii,0)-yyyy*1000000
        mm=dd/10000
        hh=dd-mm*10000
        dd=hh/100
        hh=hh-dd*100 
        cmm=tostring(mm)
        cdd=tostring(dd)
        chh=tostring(hh)
        cff=tostring(time_ts(ii,1))
        if(mm .lt. 10) then
           cmm="0"+cmm
        end if
        if(dd .lt. 10) then
           cdd="0"+cdd
        end if
        if(hh .lt. 10) then
           chh="0"+chh
        end if
        if(time_ts(ii,1) .lt. 10) then
           cff="0"+cff
        end if
        timestring_ts(ii)=tostring(yyyy)+"-"+cmm+"-"+cdd+"_"+chh+":"+cff+":00"
        if( ii .eq. 0) then
           no_ts@units="hours since "+ tostring(yyyy)+"-"+cmm+"-"+cdd+" "+chh+":"+cff+":00"
        end if
;        print(timestring_ts(ii))
     end do
  end if
; numstation,nfcst,nlvl,numvar

