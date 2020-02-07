load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

;  setfileoption ("bin", "ReadByteOrder", "BigEndian")
;  bk=addfile("../20200202-060026.MRMS_MergedReflectivityQC_01.00_20200202-060026.grib2","r")
;  bk=addfile("../20200202-060026.MRMS_MergedReflectivityQC_00.50_20200202-060026.grib2.merged","r")
  bk=addfile("/mnt/lfs1/projects/nrtrr/HRRR/run/2020020705/obsprd/60/20200207-045837.MRMS_EXP_MergedReflectivityQC_00.50_20200207-045837.grib2","r")
;  print(bk)
  ref = bk->CONUSMergedReflectivityQC_P0_L102_GLL0
  lon = bk->lon_0
  lat = bk->lat_0

;  refsmall = ref(1600:1810,650:900)
  refsmall = ref(1500:1900,550:1000)
  latsmall = lat(1500:1900)
  lonsmall = lon(550:1000) - 360.0

  rllsizes = dimsizes(refsmall)
  ny = rllsizes(0)
  nx = rllsizes(1)
  print(nx)
  print(ny)

  lat2d = new ((/ny,nx/),float)
  lon2d = new ((/ny,nx/),float)
  refs  = new ((/ny,nx/),float)

  do i = 0, nx-1
  do j = 0, ny-1
    if ( refsmall(j,i) .lt. -888 ) then
       refs(ny-j-1,i) = -10
    else if ( refsmall(j,i) .lt. -88 ) then
       refs(ny-j-1,i)= 0
    else
       refs(ny-j-1,i)= refsmall(j,i)
    end if
    end if
    lat2d(ny-j-1,i) = latsmall(j)
    lon2d(j,i) = lonsmall(i)
  end do
  end do
  print (max(refs))
  print (min(refs))
  print(latsmall(0))
  print(latsmall(ny-1))
  print(lonsmall(0))
  print(lonsmall(nx-1))
  refs@lat2d=lat2d
  refs@lon2d=lon2d

  wks = gsn_open_wks("pdf","reflectvity_merge")           ; send graphics to PNG file
  gsn_define_colormap(wks,"radar_1")

  res                   = True
  res@gsnMaximize       = True     ; maximize plot in frame
  res@mpLambertParallel1F     = latsmall(0)      ; two parallels
  res@mpLambertParallel2F     = latsmall(ny-1)
  res@mpLambertMeridianF      = lonsmall(nx/2)    ; central meridian

  res@mpProjection          = "LambertConformal"
  res@mpOutlineOn  = True
  res@mpFillOn     = False
  res@mpMinLonF = lonsmall(0) 
  res@mpMaxLonF = lonsmall(nx-1) 
  res@mpMinLatF = latsmall(ny-1)
  res@mpMaxLatF = latsmall(0) 
  res@mpLimitMode="LatLon"
  res@mpOutlineBoundarySets     = "GeophysicalAndUSStates"; turn on states

  res@cnFillOn          = True     ; turn on color fill
  res@cnLinesOn         = False    ; turn off contour lines
  res@cnLevelSelectionMode = "ExplicitLevels"   ; set explicit contour levels
  res@cnLevels     = (/-5,1,5,10,15,20,25,30,35,40,45,50,55,60/)
  res@cnFillColors = (/ 8,9,10,11,12,13,15,17,18,19,20,21,22,23,24/)  ; set the colors to be used
  res@cnLineLabelsOn = False                    ; turn the line labels off


  plot = gsn_csm_contour_map(wks,refs,res)

  delete(plot)

end

