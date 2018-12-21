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
  domain="X0001-0700Y0300-0600Z0001-0002"
  domain="X0200-0650Y0300-0650Z0001-0002"
  domain="X0200-0450Y0350-0550Z0001-0002"
  domain="X0150-0450Y0050-0350Z0001-0002"
  filedata=path+"/plot_"+testid+"/spread_"+domain+".bin"

  resultsfile="Cspread."+testid+"."+domain

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

  fieldps = fbinrecread(filedata, 4, (/ 1,ny,nx /), "float")
  fieldtv = fbinrecread(filedata, 5, (/ nz,ny,nx /), "float")
  fieldu  = fbinrecread(filedata, 6, (/ nz,ny,nx /), "float")
  fieldv  = fbinrecread(filedata, 7, (/ nz,ny,nx /), "float")
  fieldrh = fbinrecread(filedata, 8, (/ nz,ny,nx /), "float")
  print(max(fieldps(0,:,:)))
  print(max(fieldtv(0,:,:)))
  print(max(fieldu(0,:,:)))
  print(max(fieldv(0,:,:)))
  print(max(fieldrh(0,:,:)))

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
  resources@tiMainFontHeightF=0.02
  resources@tmXBLabelFontHeightF=0.01
  resources@tmYLLabelFontHeightF=0.01

  presources                  =  True              ; plot mods desired
  presources@gsnDraw              = False              ; Do not draw plot
  presources@gsnFrame             = True              ; Do not advance frame

  presources@sfXArray         = rlon
  presources@sfYArray         = rlat

  ccmap      = read_colormap_file("BlAqGrYeOrRe")
  presources@cnFillOn              = False            ; color plot desired
  presources@cnFillPalette         = ccmap

  presources@tfDoNDCOverlay  = True
  presources@cnMonoLineColor    = False
  presources@gsnContourLineThicknessesScale    = 1.0
  presources@gsnContourNegLineDashPattern = 1         ; negtive line use dash
  presources@lbPerimOn             = False    ; Turn off perimeter on label bar.
  presources@lbOrientation         = "Vertical"
  presources@cnLineLabelDensityF   = 0.5
  presources@cnLineLabelFontHeightF= 0.01

  presources@tiMainFontHeightF=0.01

  presources@cnLevelSelectionMode  = "ExplicitLevels"
  presources@cnLevelSelectionMode  = "AutomaticLevels"
  presources@cnFillColors = (/ 0,11,21,31,38,48,58,68,78,88,98/)

  do iv = 0,4
     presources@tiMainString    = variableid(iv)+" Spread:"+domain+"+terrain"
     map = gsn_csm_contour_map(wks,terrain,resources)
     if ( iv .eq. 0 ) then
        presources@cnLevels              = (/0.06,0.07,0.08,0.10,0.12,0.14,0.18,0.22,0.26,0.30/) ; ps
        presources@cnLevelSpacingF=0.002
        plot = gsn_contour(wks,fieldps(0,:,:),presources)
     else if ( iv .eq. 1 ) then
        presources@cnLevels              = (/0.6,0.8,1.0,1.2,1.4,1.8,2.0,2.2,2.4,2.8/)  ; T
        presources@cnLevels              = (/0.2,0.3,0.4,0.5,0.6,0.8,1.0,1.2,1.4,1.8/)  ; T
        presources@cnLevelSpacingF=0.10
        plot = gsn_contour(wks,fieldtv(0,:,:),presources)
     else if ( iv .eq. 2 ) then
        presources@cnLevels             = (/0.6,1.0,1.4,1.8,2.2,2.6,3.0,3.4,3.8,4.2/)  ; U,V
        presources@cnLevels             = (/0.2,0.4,0.6,0.8,1.0,1.2,1.6,2.0,2.4,2.8/)  ; U,V
        presources@cnLevelSpacingF=0.15
        plot = gsn_contour(wks,fieldu(0,:,:),presources)
     else if ( iv .eq. 3 ) then
        presources@cnLevels              = (/0.6,1.0,1.4,1.8,2.2,2.6,3.0,3.4,3.8,4.2/)  ; U,V
        presources@cnLevels             = (/0.2,0.4,0.6,0.8,1.0,1.2,1.6,2.0,2.4,2.8/)  ; U,V
        presources@cnLevelSpacingF=0.15
        plot = gsn_contour(wks,fieldv(0,:,:),presources)
     else if ( iv .eq. 4 ) then
        presources@cnLevels              = (/0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20/) ; rh
        presources@cnLevels              = (/0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.10,0.12,0.14/) ; rh
        presources@cnLevelSpacingF=0.005
        plot = gsn_contour(wks,fieldrh(0,:,:),presources)
     end if
     end if
     end if
     end if
     end if

     overlay(map,plot)
     draw(map)
  end do 

  delete(plot)
  delete(map)

end
