load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

  path="/mnt/lfs1/projects/wrfruc/mhu/rapcode/util/GSD_UTL/tools/cutperts/run/plot/"
  variableid=(/"tv","u","v","rh"/)
  fcstid=(/"12","15","18","21","24","27","30","33","36"/)
;  fcstid=(/"12","18","24","30", "36"/)
  domain="X0001-0399Y0001-0399Z0001-0050"

do iff = 0,8
;do iff = 0,4

;  testid="M0809F" + fcstid(iff)
  testid="M120102F" + fcstid(iff)

  filedata=path+"/"+testid+"_spread_"+domain+".bin"

  resultsfile="coastalCspread_cs."+testid+"."+domain+".all"

  dim = fbinrecread(filedata, 0, (/3/), "integer")
  nx=dim(0)
  ny=dim(1)
  nz=dim(2)

  print(nx) 
  print(ny) 
  print(nz) 

  rlat = fbinrecread(filedata, 1, (/ ny,nx /), "float")
  rlon = fbinrecread(filedata, 2, (/ ny,nx /), "float")

  fieldtv = fbinrecread(filedata, 5, (/ nz,ny,nx /), "float")
  fieldu  = fbinrecread(filedata, 6, (/ nz,ny,nx /), "float")
  fieldv  = fbinrecread(filedata, 7, (/ nz,ny,nx /), "float")
  fieldrh = fbinrecread(filedata, 8, (/ nz,ny,nx /), "float")
  print(max(fieldtv(0,:,:)))
  print(max(fieldu(0,:,:)))
  print(max(fieldv(0,:,:)))
  print(max(fieldrh(0,:,:)))

  istart=0
  iend=nx-1
  jstart=0
  jend=ny-1

  istart=0
  iend=170
  jstart=50
  jend=220
  jcs=jstart+(jend-jstart)/2

  wks = gsn_open_wks("pdf",resultsfile)
  gsn_define_colormap(wks,"testcmap")

  presources                  =  True              ; plot mods desired
  presources@vpWidthF      = 0.6
  presources@vpHeightF     = 0.3
  presources@gsnDraw              = False              ; Do not draw plot
  presources@gsnFrame             = True              ; Do not advance frame

  ccmap      = read_colormap_file("BlAqGrYeOrReVi200")
  presources@cnFillOn              = False            ; color plot desired
  presources@cnFillPalette         = ccmap

  presources@tfDoNDCOverlay  = True
  presources@cnMonoLineColor    = False
  presources@gsnContourLineThicknessesScale    = 1.0
  presources@gsnContourNegLineDashPattern = 1         ; negtive line use dash
  presources@cnLineLabelsOn= False

  presources@tiMainFontHeightF=0.01

  presources@cnLevelSelectionMode  = "AutomaticLevels"

  presources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  presources@lbPerimOn             = False    ; Turn off perimeter on label bar.
  presources@lbOrientation         = "Vertical"

  do iv = 0,3
     presources@tiMainString    = variableid(iv)+" Spread:"+testid+"+terrain"
     if ( iv .eq. 0 ) then
        presources@cnLevelSpacingF=0.05
        plot = gsn_contour(wks,fieldtv(:,jcs,istart:iend),presources)
     else if ( iv .eq. 1 ) then
        presources@cnLevelSpacingF=0.05
        plot = gsn_contour(wks,fieldu(:,jcs,istart:iend),presources)
     else if ( iv .eq. 2 ) then
        presources@cnLevelSpacingF=0.1
        plot = gsn_contour(wks,fieldv(:,jcs,istart:iend),presources)
     else if ( iv .eq. 3 ) then
        presources@cnLevelSpacingF=0.0001
        plot = gsn_contour(wks,fieldrh(:,jcs,istart:iend),presources)
     end if
     end if
     end if
     end if

     draw(plot)
  end do 

  delete(plot)

end do

end
