
  print("====read dimensions=====")
  dim = fbinrecread(filehead, 0, (/4/), "integer")
  numvar=dim(2)
  nfcst=dim(1)
  numstation=dim(3)
  nlvl=dim(0)
  print("nlevel="+nlvl+"  nfcst="+nfcst+"  numvar="+numvar+"  numstation="+numstation)

  print("====read level type=====")
  leveltype = fbinrecread(filehead, 1, (/1/), "character")
  print("level type is:"+leveltype)

  print("====read if save surrond=====")
  ifsavesurrond = fbinrecread(filehead, 2, (/1/), "integer")
  print("if save surround is:"+ifsavesurrond)

  print("====read levels=====")
  nlevel = nlvl + 1
  if ( leveltype .eq. "A") then 
     nlevel = nlvl
  end if
  rlevel = fbinrecread(filehead, 3, (/nlevel/), "float")
  print( sprintf("%10.1f",rlevel))

  print("====read forecast times (hour)=====")
  ifcst = fbinrecread(filehead, 4, (/nfcst/), "integer")
  print(ifcst/60)

  print("====read variable names=====")
  varnames = fbinrecread(filehead, 5, (/numvar,20/), "character")
  do i=0,dim(2)-1
     print(chartostring(varnames(i,:)))
  end do

  if ( dim(3) .gt. 1 ) then
     print("====read stations: name, laon,lat,ele=====")
     numsta = fbinrecread(filehead, 6, (/1/), "integer")
     stanames = fbinrecread(filehead, 7, (/numsta,8/), "character")
     stalons = fbinrecread(filehead, 8, (/numsta/), "float")
     stalats = fbinrecread(filehead, 9, (/numsta/), "float")
     staeles = fbinrecread(filehead, 10, (/numsta/), "float")
     staxc = fbinrecread(filehead, 11, (/numsta/), "float")
     stayc = fbinrecread(filehead, 12, (/numsta/), "float")
     numtimelevel = fbinrecread(filehead, 13, (/1/), "integer")
     print(numsta + " name         lon        lat      ele     xc     yc")
     do i=0,numsta-1
        print(i + "  " + chartostring(stanames(i,:)) + sprintf("%10.2f",stalons(i)) + sprintf("%10.2f",stalats(i)) + sprintf("%10.1f",staeles(i)) + sprintf("%10.2f",staxc(i)) + sprintf("%10.2f",stayc(i))) 
     end do 
  end if

  print("====read time levels=====")
  if ( dim(3) .eq. 1 ) then
     numtimelevel = fbinrecread(filehead, 6, (/1/), "integer")
  end if
  print(" time levels =" + numtimelevel)
