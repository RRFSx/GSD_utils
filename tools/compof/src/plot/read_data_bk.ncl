
  datemmbk = fbinrecread(filedatabk, 0, (/2/), "integer")
  print("   date="+datemmbk(0) + "     minute="+datemmbk(1))
  numvarbk=dim(2)
  nfcstbk=dim(1)
  numstationbk=dim(3)
  nlvlbk=dim(0)

  rmsebk   = fbinrecread(filedatabk, 1, (/ numstationbk,nfcstbk,nlvlbk,numvarbk /), "float")
  biasbk   = fbinrecread(filedatabk, 2, (/ numstationbk,nfcstbk,nlvlbk,numvarbk /), "float")
  numpairbk= fbinrecread(filedatabk, 3, (/ numstationbk,nfcstbk,nlvlbk,numvarbk /), "integer")

  if( numtimelevelbk .gt. 0 ) then
     do i = 1 , numtimelevelbk-1
        datemmbk = fbinrecread(filedatabk, 0 + i*5, (/2/), "integer")
        print("   date="+datemmbk(0) + "     minute="+datemmbk(1))
        rmsebk   = fbinrecread(filedatabk, 1 + i*5, (/ numstationbk,nfcstbk,nlvlbk,numvarbk /), "float")
        biasbk   = fbinrecread(filedatabk, 2 + i*5, (/ numstationbk,nfcstbk,nlvlbk,numvarbk /), "float")
        numpairbk= fbinrecread(filedatabk, 3 + i*5, (/ numstationbk,nfcstbk,nlvlbk,numvarbk /), "integer")
        if( ifsavesurrond .eq. 1) then
           value4 = fbinrecread(filedatabk, 4 + i*5, (/ numstationbk,nfcstbk,nlvlbk,numvarbk /), "float")
        end if
     end do
  end if
; numstation,nfcst,nlvl,numvar

