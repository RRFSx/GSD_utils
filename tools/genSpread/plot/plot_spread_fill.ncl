load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

  path="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/GSD_UTL/tools/buildsprd/bin/"
  testid="12z_m11m12" 
  testid="21z_m08m09" 
  variableid=(/"ps","tv","u","v","rh"/)
  domain="X0001-1580Y0001-1000Z0001-0002"
  filedata=path+"/plot_"+testid+"/spread_"+domain+".bin"

  resultsfile="spread."+testid+"."+domain

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
  gsn_define_colormap(wks,"BlAqGrYeOrRe")

  resources                      = True
  resources@gsnDraw              = False             ; Do not draw plot
  resources@gsnFrame             = True              ; Do not advance frame
  resources@gsnMaximize          = True

  resources@cnFillOn              = True            ; color plot desired
  resources@cnLinesOn             = False

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

  resources@cnLevelSelectionMode  = "ExplicitLevels"
  resources@cnFillColors = (/ 0,11,21,31,38,48,58,68,78,88,98/)
  
  do iv=0,4
     resources@tiMainString    = variableid(iv) + " Spread: " + testid
     if(iv .eq. 0 ) then
        resources@cnLevels              = (/0.06,0.07,0.08,0.10,0.12,0.14,0.18,0.22,0.26,0.30/) ; ps 
;        resources@cnLevels              = (/0.06,0.07,0.08,0.09,0.10,0.11,0.12,0.13,0.14,0.16/) ; ps 
        map = gsn_csm_contour_map(wks,fieldps(0,:,:),resources)
     else if ( iv .eq. 1 ) then
;        resources@cnLevels              = (/0.6,0.8,1.0,1.2,1.4,1.8,2.0,2.2,2.4,2.8/)  ; T
        resources@cnLevels              = (/0.2,0.3,0.4,0.5,0.6,0.8,1.0,1.2,1.4,1.8/)  ; T
        map = gsn_csm_contour_map(wks,fieldtv(0,:,:),resources)
     else if ( iv .eq. 2 ) then
;        resources@cnLevels             = (/0.6,1.0,1.4,1.8,2.2,2.6,3.0,3.4,3.8,4.2/)  ; U,V
        resources@cnLevels              = (/0.2,0.4,0.6,0.8,1.0,1.2,1.6,2.0,2.4,2.8/)  ; U,V
        map = gsn_csm_contour_map(wks,fieldu(0,:,:),resources)
     else if ( iv .eq. 3 ) then
;        resources@cnLevels              = (/0.6,1.0,1.4,1.8,2.2,2.6,3.0,3.4,3.8,4.2/)  ; U,V
        resources@cnLevels              = (/0.2,0.4,0.6,0.8,1.0,1.2,1.6,2.0,2.4,2.8/)  ; U,V
        map = gsn_csm_contour_map(wks,fieldv(0,:,:),resources)
     else if ( iv .eq. 4 ) then
;        resources@cnLevels              = (/0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20/) ; rh 
        resources@cnLevels              = (/0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.10,0.12,0.14/) ; rh 
        map = gsn_csm_contour_map(wks,fieldrh(0,:,:),resources)
     end if
     end if
     end if
     end if
     end if

     draw(map)
  end do

end
