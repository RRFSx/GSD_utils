load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_station.bin"
  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0187_station.bin"

  load "read_head.ncl"
  load "read_data.ncl"
; numstation,nfcst,nlvl,numvar

  nrange=7
  clr= (/37,52,67,87,97,113,133,163,193/)
  ranges = (/-4.0,-2.0,-1.0,-0.5,0.5,1.0,2.0,4.0/)
  names = (/"<-4","-4 -2","-2 -1","-1 -.5","-.5 .5",".5 1","1 2","2 4",">4"/)

  ; x and y locations for the legends, for most of the plots, in normalized coordinates
  xdot = (/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9/)
  ydot = (/0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005/)
  xtxt = (/0.11,0.215,0.315,0.415,0.515,0.61,0.71,0.81,0.91/)
  ytxt = (/0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00/)


  bk=addfile("/mnt/lfs3/projects/rtwbl/mhu/hrrrretro/save/wfip2_Aug2016_HRRR/run/2016081600/wrfprd/wrfout_d01_2016-08-16_01_00_00.cdf","r")
  rlon = bk->XLONG(0,:,:)
  rlat = bk->XLAT(0,:,:)

  wks = gsn_open_wks("pdf","static_187_forecast_station_values_2d_noTerrain")
  gsn_define_colormap(wks,"testcmap")

  resources                      = True
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame
  resources@gsnMaximize          = True

  cmap      = read_colormap_file("OceanLakeLandSnow")
  cmap(:,3) = 0.5     ; half transparency
;  resources@cnFillOn              = True            ; color plot desired
;  resources@cnFillPalette         = cmap
;  resources@cnLinesOn             = False
;  resources@cnLevelSelectionMode  = "ExplicitLevels"
;  resources@cnLevels              = (/10,30,50,70,100,150,200,\
;                                250,300,350,400,450,500,550,600,650,700,\
;                                750,800,850,900,950,1000,1100,1200,1300,1400,\
;                                1500,1600,1700,1800,1900,2000,2200,2400,2600,2800,\
;                                3000,3200,3400,3600,4000/)

; map
  resources@sfXArray         = rlon
  resources@sfYArray         = rlat
  resources@mpProjection     = "LambertConformal"
  resources@mpFillOn         = False
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
;  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.

;  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.
;  resources@lbOrientation         = "Vertical"

  resources@tfDoNDCOverlay        = True        ; REQUIRED for plotting on native grid
  resources@tiMainString    = " 187 Station distribution"
  resources@tiMainFontHeightF=0.02
  resources@tmXBLabelFontHeightF=0.01
  resources@tmYLLabelFontHeightF=0.01
;  map = gsn_csm_contour_map(wks,terrain,resources)
  map = gsn_csm_map(wks,resources)

;---Create a resource list for the text.

  txres               = True
  txres@txFontHeightF = 0.005
  txres@txFont        = "helvetica"
  txres@txJust        = "BottomRight"
  do i=0,numsta-1
     plot = gsn_add_text(wks,map,chartostring(stanames(i,:)),stalons(i),stalats(i),txres) 
  end do 
  nstrs = dimsizes(plot)

;---Create a resource list for the markers.
  mkres = True
  mkres@gsMarkerSizeF    = 0.01
  mkres@gsMarkerIndex    = 16          ; Filled dots
  plotmk=new(numsta,graphic)
  do i=0,numsta-1
     thisvalue=bias(i,5,0,0)
     mkres@gsMarkerColor    = "orange"
     if(thisvalue .le. ranges(0)) then
        mkres@gsMarkerColor    = clr(0)
     end if
     if(thisvalue .ge. ranges(nrange-1)) then
        mkres@gsMarkerColor    = clr(8)
     end if
     do j=0,nrange-1
        if(thisvalue .ge. ranges(j) .and. thisvalue .le. ranges(j+1)) then
           mkres@gsMarkerColor    = clr(1+j)
        end if
     end do
    plotmk(i) = gsn_add_polymarker(wks,map,stalons(i),stalats(i),mkres) 
  end do 

;---add legend to plot
   gsres = True
   txres = True
   gsres@gsMarkerIndex = 16          ; Use filled dots for markers.
   txres@txFontHeightF = 0.012
   txres@txFontThicknessF = 2.5

   do k=0,dimsizes(names)-1
     gsres@gsMarkerColor    = clr(k)
     gsres@gsMarkerSizeF    = 0.04
     gsn_polymarker_ndc(wks,xdot(k),ydot(k),gsres)
     gsn_text_ndc(wks,names(k),xtxt(k),ytxt(k),txres)
   end do

  draw(map)
  frame(wks)

  delete(plot)
  delete(plotmk)

end
