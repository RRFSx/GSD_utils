load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

  path="/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/run/plot/"
  variableid=(/"ps","tv","u","v","rh"/)
  fcstid=(/"12","15","18","21","24","27","30","33","36"/)
  domain="X0001-0399Y0001-0399Z0001-0050"

;do iff = 0,8
do iff = 0,0

  testid="M120102F" + fcstid(iff)
  filedata=path+"/"+testid+"_spread_"+domain+".bin"

  resultsfile="mountainCspread."+testid+"."+domain+".all"

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

  istart=0
  iend=nx-1
  jstart=0
  jend=ny-1

  istart=80
  iend=180
  jstart=220
  jend=320

  lat_ll  = rlat(jstart,istart)
  lat_ur  = rlat(jend,iend)
  lon_ll  = rlon(jstart,istart)
  lon_ur  = rlon(jend,iend)

  wks = gsn_open_wks("pdf",resultsfile)
  gsn_define_colormap(wks,"testcmap")

  resources                      = True
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame
;  resources@gsnMaximize          = True
;  resources@mpShapeMode  = "FreeAspect"
  resources@vpWidthF      = 0.5
  resources@vpHeightF     = 0.5


;  cmap      = read_colormap_file("OceanLakeLandSnow")
  cmap      = read_colormap_file("MPL_Greys")
;  cmap      = read_colormap_file("gsltod")
  cmap(:,3) = 0.5     ; half transparency
  resources@cnFillOn              = True            ; color plot desired
  resources@cnFillPalette         = cmap
  resources@cnLinesOn             = False
  resources@cnLevelSelectionMode  = "ExplicitLevels"
;  resources@cnLevels              = (/10,50,100,150,200,300,400,500,600,700,800,900,1000,1200,1400,1600,1800,2000,2500,3000,3500,4000/)
  resources@cnLevels              = (/200,250,300,350,400,450,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1800,2000,2500,3000,3500,4000/)
;  resources@cnLevels              = (/10,30,50,70,100,150,200,\
;                                250,300,350,400,450,500,550,600,650,700,\
;                                750,800,850,900,950,1000,1100,1200,1300,1400,\
;                                1500,1600,1700,1800,1900,2000,2200,2400,2600,2800,\
;                                3000,3200,3400,3600,4000/)
;
; map
  resources@sfXArray         = rlon(jstart:jend,istart:iend)
  resources@sfYArray         = rlat(jstart:jend,istart:iend)
  resources@mpProjection     = "LambertConformal"
  resources@mpLambertParallel1F     = 34.5     ; two parallels
  resources@mpLambertParallel2F     = 34.5
  resources@mpLambertMeridianF      = -112.5    ; central meridian

  resources@mpLimitMode             = "Corners"
  resources@mpLeftCornerLatF        = lat_ll
  resources@mpLeftCornerLonF        = lon_ll
  resources@mpRightCornerLatF       = lat_ur
  resources@mpRightCornerLonF       = lon_ur

  resources@pmTickMarkDisplayMode = "Always"
  resources@mpOutlineDrawOrder    = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
  resources@mpGridAndLimbOn = False                  ; turn off lat/lon lines
  resources@mpPerimOn = True

  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.
;  resources@lbOrientation         = "Vertical"


  resources@tfDoNDCOverlay        = True        ; REQUIRED for plotting on native grid
  resources@tiMainFontHeightF=0.02
  resources@tmXBLabelFontHeightF=0.01
  resources@tmYLLabelFontHeightF=0.01

  presources                  =  True              ; plot mods desired
  presources@gsnDraw              = False              ; Do not draw plot
  presources@gsnFrame             = True              ; Do not advance frame

  presources@sfXArray         = rlon(jstart:jend,istart:iend)
  presources@sfYArray         = rlat(jstart:jend,istart:iend)

;  ccmap      = read_colormap_file("BlAqGrYeOrRe")
  ccmap      = read_colormap_file("BlAqGrYeOrReVi200")
  presources@cnFillOn              = False            ; color plot desired
  presources@cnFillPalette         = ccmap

  presources@tfDoNDCOverlay  = True
  presources@cnMonoLineColor    = False
  presources@gsnContourLineThicknessesScale    = 1.0
  presources@gsnContourNegLineDashPattern = 1         ; negtive line use dash
;  presources@cnLineLabelDensityF   = 0.5
  presources@cnLineLabelsOn= False
;  presources@cnLineLabelFontHeightF= 0.01

  presources@tiMainFontHeightF=0.01

;  presources@cnLevelSelectionMode  = "ExplicitLevels"
  presources@cnLevelSelectionMode  = "AutomaticLevels"
;  presources@cnFillColors = (/ 0,11,21,31,38,48,58,68,78,88,98/)

  presources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  presources@lbPerimOn             = False    ; Turn off perimeter on label bar.
  presources@lbOrientation         = "Vertical"

  do iv = 0,4
     presources@tiMainString    = variableid(iv)+" Spread:"+testid+"+terrain"
     map = gsn_csm_contour_map(wks,terrain(jstart:jend,istart:iend),resources)
     if ( iv .eq. 0 ) then
;        presources@cnLevels              = (/0.06,0.07,0.08,0.10,0.12,0.14,0.18,0.22,0.26,0.30/) ; ps
     ;   presources@cnLevelSpacingF=0.002
        presources@cnLevelSpacingF=0.001
        plot = gsn_contour(wks,fieldps(0,jstart:jend,istart:iend),presources)
     else if ( iv .eq. 1 ) then
;        presources@cnLevels              = (/0.6,0.8,1.0,1.2,1.4,1.8,2.0,2.2,2.4,2.8/)  ; T
;        presources@cnLevels              = (/0.2,0.3,0.4,0.5,0.6,0.8,1.0,1.2,1.4,1.8/)  ; T
     ;   presources@cnLevelSpacingF=0.1
        presources@cnLevelSpacingF=0.04
        plot = gsn_contour(wks,fieldtv(0,jstart:jend,istart:iend),presources)
     else if ( iv .eq. 2 ) then
;        presources@cnLevels             = (/0.6,1.0,1.4,1.8,2.2,2.6,3.0,3.4,3.8,4.2/)  ; U,V
;        presources@cnLevels             = (/0.2,0.4,0.6,0.8,1.0,1.2,1.6,2.0,2.4,2.8/)  ; U,V
     ;   presources@cnLevelSpacingF=0.1
        presources@cnLevelSpacingF=0.04
        plot = gsn_contour(wks,fieldu(0,jstart:jend,istart:iend),presources)
     else if ( iv .eq. 3 ) then
;        presources@cnLevels              = (/0.6,1.0,1.4,1.8,2.2,2.6,3.0,3.4,3.8,4.2/)  ; U,V
;        presources@cnLevels             = (/0.2,0.4,0.6,0.8,1.0,1.2,1.6,2.0,2.4,2.8/)  ; U,V
     ;   presources@cnLevelSpacingF=0.1
        presources@cnLevelSpacingF=0.04
        plot = gsn_contour(wks,fieldv(0,jstart:jend,istart:iend),presources)
     else if ( iv .eq. 4 ) then
;        presources@cnLevels              = (/0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20/) ; rh
;        presources@cnLevels              = (/0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.10,0.12,0.14/) ; rh
     ;   presources@cnLevelSpacingF=0.001
        presources@cnLevelSpacingF=0.00005
        plot = gsn_contour(wks,fieldrh(0,jstart:jend,istart:iend),presources)
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
end do

end
