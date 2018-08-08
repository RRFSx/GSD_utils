
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
  
  nrecode=1
  rmse   = fbinrecread(filedata, nrecode, (/ numstation,nfcst,nlvl,numvar /), "float")
  nrecode=nrecode+1
  bias   = fbinrecread(filedata, nrecode, (/ numstation,nfcst,nlvl,numvar /), "float")
  nrecode=nrecode+1
  numpair= fbinrecread(filedata, nrecode, (/ numstation,nfcst,nlvl,numvar /), "integer")
  if( ifsavesurrond .eq. 1 ) then
     nrecode=nrecode+1
     value4= fbinrecread(filedata, nrecode, (/ numstation,nfcst,4,numvar /), "float")
  end if

  rmse_ts(:,:,:,:,0)= rmse
  bias_ts(:,:,:,:,0) = bias
  numpair_ts(:,:,:,:,0)=numpair
  ii=0
  if( numstation .gt. 1) then
     do i = 0,numstation-1
     do j = 0,nfcst-1
     do k = 0,nlvl-1
     do n = 0,numvar-1
        if(numpair(i,j,k,n) .le. 0) then
                 rmse_ts(i,j,k,n,ii)= -99999.0
                 bias_ts(i,j,k,n,ii)= -99999.0
        end if
     end do
     end do
     end do
     end do
  end if

  if( numtimelevel .gt. 0 ) then
     do ii = 1 , numtimelevel-1
        nrecode=nrecode+1
        datemm = fbinrecread(filedata, nrecode, (/2/), "integer")
        print("   date="+datemm(0) + "     minute="+datemm(1))
        nrecode=nrecode+1
        rmse   = fbinrecread(filedata, nrecode, (/ numstation,nfcst,nlvl,numvar /), "float")
        nrecode=nrecode+1
        bias   = fbinrecread(filedata, nrecode, (/ numstation,nfcst,nlvl,numvar /), "float")
        nrecode=nrecode+1
        numpair= fbinrecread(filedata, nrecode, (/ numstation,nfcst,nlvl,numvar /), "integer")
        
        if( ifsavesurrond .eq. 1 ) then
           nrecode=nrecode+1
           value4= fbinrecread(filedata, nrecode, (/ numstation,nfcst,4,numvar /), "float")
        end if

        rmse_ts(:,:,:,:,ii)= rmse
        bias_ts(:,:,:,:,ii) = bias
        numpair_ts(:,:,:,:,ii)=numpair
        if( numstation .gt. 1) then
           do i = 0,numstation-1
           do j = 0,nfcst-1
           do k = 0,nlvl-1
           do n = 0,numvar-1
              if(numpair(i,j,k,n) .le. 0) then
                 rmse_ts(i,j,k,n,ii)= -99999.0
                 bias_ts(i,j,k,n,ii)= -99999.0
              end if
           end do
           end do
           end do
           end do
        end if
     end do
  end if
; numstation,nfcst,nlvl,numvar

