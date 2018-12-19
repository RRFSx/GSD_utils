load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

  path="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/GSD_UTL/tools/buildsprd/bin/"
  testid="21z_m08m09" 
  variableid=(/"ps","tv","u","v","rh"/)
  domain="X0001-1580Y0001-1000Z0001-0002"
  domain="X0001-0800Y0001-0500Z0001-0002"
  filedata=path+"/plot_"+testid+"/spread_"+domain+".bin"

  resultsfile="terrian."+domain

  dim = fbinrecread(filedata, 0, (/3/), "integer")
  nx=dim(0)
  ny=dim(1)
  nz=dim(2)

  print(nx) 
  print(ny) 
  print(nz) 

  rlat = fbinrecread(filedata, 1, (/ ny,nx /), "float")
  rlon = fbinrecread(filedata, 2, (/ ny,nx /), "float")
  terrain = fbinrecread(filedata, 3, (/ ny,nx /), "float")

  wks = gsn_open_wks("pdf",resultsfile)
  gsn_define_colormap(wks,"testcmap")

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
  resources@mpLambertParallel1F     = 38.5     ; two parallels
  resources@mpLambertParallel2F     = 38.5
  resources@mpLambertMeridianF      = -97.5    ; central meridian

  resources@mpLimitMode             = "Corners"
  resources@mpLeftCornerLatF        = rlat(0,0)
  resources@mpLeftCornerLonF        = rlon(0,0)
  resources@mpRightCornerLatF       = rlat(ny-1,nx-1)
  resources@mpRightCornerLonF       = rlon(ny-1,nx-1)

  resources@pmTickMarkDisplayMode = "Always"
  resources@mpOutlineDrawOrder    = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
  resources@mpGridAndLimbOn = False                  ; turn off lat/lon lines
  resources@mpPerimOn = True
  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.

  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.
  resources@lbOrientation         = "Vertical"

  resources@tfDoNDCOverlay        = True        ; REQUIRED for plotting on native grid
  resources@tiMainString    = " terrian " + domain
  resources@tiMainFontHeightF=0.02
  resources@tmXBLabelFontHeightF=0.01
  resources@tmYLLabelFontHeightF=0.01
  map = gsn_csm_contour_map(wks,terrain,resources)

  draw(map)

end
