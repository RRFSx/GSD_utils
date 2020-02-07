load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

;  setfileoption ("bin", "ReadByteOrder", "BigEndian")
  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_station.bin"

  bk=addfile("../20200202-060026.MRMS_MergedReflectivityQC_04.00_20200202-060026.grib2","r")
;  print(bk)
  ref = bk->CONUSMergedReflectivityQC_P0_L102_GLL0
  lon = bk->lon_0
  lat = bk->lat_0

;  refsmall = ref(1600:1810,650:900)
  refsmall = ref(1400:2000,400:1100)
  latsmall = lat(1400:2000)
  lonsmall = lon(400:1100) - 360.0

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

;  refsmallT=transpose(refsmall)

  wks = gsn_open_wks("pdf","reflectvity")           ; send graphics to PNG file
  gsn_define_colormap(wks,"radar_1")

  res                   = True
  res@gsnMaximize       = True     ; maximize plot in frame
  res@cnFillOn          = True     ; turn on color fill
  res@cnLinesOn         = False    ; turn off contour lines
  res@cnLevelSelectionMode = "ExplicitLevels"   ; set explicit contour levels
  res@cnLevels     = (/-5,1,5,10,15,20,25,30,35,40,45,50,55,60/)
  res@cnFillColors = (/ 8,9,10,11,12,13,15,17,18,19,20,21,22,23,24/)  ; set the colors to be used
  res@cnLineLabelsOn = False                    ; turn the line labels off

  plot = gsn_csm_contour(wks,refs,res)

  delete(plot)

end

