load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

;  setfileoption ("bin", "ReadByteOrder", "BigEndian")
;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0120_all.bin"
;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0227_station.bin"
;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0120_station.bin"
  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_station.bin"
;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_station_ts.bin"

  load "read_head.ncl"

  bk=addfile("/mnt/lfs3/projects/rtwbl/mhu/hrrrretro/save/wfip2_Aug2016_HRRR/run/2016081600/wrfprd/wrfout_d01_2016-08-16_01_00_00.cdf","r")
  rlon = bk->XLONG(0,:,:)
  rlat = bk->XLAT(0,:,:)
  terrain = bk->HGT(0,:,:)

  wks = gsn_open_wks("pdf","stations")           ; send graphics to PNG file

  resources                      = True
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame
  resources@gsnMaximize          = True

  cmap      = read_colormap_file("OceanLakeLandSnow")
  cmap(:,3) = 0.5     ; half transparency
  resources@cnFillOn              = True            ; color plot desired
  resources@cnFillPalette         = cmap
  resources@cnLinesOn             = False
  resources@cnLevelSelectionMode  = "ExplicitLevels"
  resources@cnLevels              = (/10,30,50,70,100,150,200,\
                                250,300,350,400,450,500,550,600,650,700,\
                                750,800,850,900,950,1000,1100,1200,1300,1400,\
                                1500,1600,1700,1800,1900,2000,2200,2400,2600,2800,\
                                3000,3200,3400,3600,4000/)

; map
  resources@sfXArray         = rlon
  resources@sfYArray         = rlat
  resources@mpProjection     = "LambertConformal"
  resources@mpLambertParallel1F     = 45.4      ; two parallels
  resources@mpLambertParallel2F     = 45.4
  resources@mpLambertMeridianF      = -120.5    ; central meridian

  resources@mpLimitMode             = "Corners"
  resources@mpLeftCornerLatF        = 41.963
  resources@mpLeftCornerLonF        = -124.994
  resources@mpRightCornerLatF       = 48.639
  resources@mpRightCornerLonF       = -115.441

  resources@pmTickMarkDisplayMode = "Always"
  resources@mpOutlineDrawOrder    = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
  resources@mpGridAndLimbOn = False                  ; turn off lat/lon lines
  resources@mpPerimOn = True
  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.

  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.
  resources@lbOrientation         = "Vertical"

  resources@tfDoNDCOverlay        = True        ; REQUIRED for plotting on native grid
  resources@tiMainString    = " 187 Station distribution"
  resources@tiMainFontHeightF=0.02
  resources@tmXBLabelFontHeightF=0.01
  resources@tmYLLabelFontHeightF=0.01
  map = gsn_csm_contour_map(wks,terrain,resources)

  txres               = True
  txres@txFontHeightF = 0.005
  txres@txFont        = "helvetica"
  do i=0,numsta-1
     plot = gsn_add_text(wks,map,chartostring(stanames(i,:)),stalons(i),stalats(i),txres) 
  end do 
  nstrs = dimsizes(plot)
  print(nstrs)

  draw(map)
  frame(wks)

  delete(plot)

end
