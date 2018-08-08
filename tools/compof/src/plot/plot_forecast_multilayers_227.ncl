load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0227_all.bin"
  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0227_all.bin"
;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0227_station.bin"
;  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0227_station.bin"

  load "read_head.ncl"
  load "read_data.ncl"

; numstation,nfcst,nlvl,numvar
;  print(rmse(0,:,:,0))
  xx = new ((/nfcst/), "float")
  yy = new ((/nlvl/), "float")
  xx = ifcst/60
  yy = rlevel(0:nlvl-1)

  xwks = gsn_open_wks("pdf","static_227_forecast")
  gsn_define_colormap(xwks,"cosam12")

  resources                 =  True              ; plot mods desired
  resources@gsnDraw         = False              ; Do not draw plot
  resources@gsnFrame        = False              ; Do not advance frame

  resources@sfXArray        = xx
  resources@sfYArray        = yy
  resources@gsnYAxisIrregular2Linear = True

  resources@cnFillOn            = True          ; turn on color
  resources@cnFillPalette       = "gui_default" ; set color map
  resources@cnFillMode          = "CellFill"
  resources@cnLinesOn           = False
  resources@cnLineLabelsOn      = False

  resources@vpHeightF = 0.30        ; Changes the aspect ratio
  resources@vpWidthF  = 0.80
  resources@gsnMaximize = True

  resources@tiXAxisString = "Forecast Hours" ; y-axis label
  resources@tiYAxisString = "Height(m)"     

  titles = new(6,string)
  plot = new(6,graphic)

  rmse@_FillValue = -99999.0
  bias@_FillValue = -99999.0

  do ii=0,numstation-1
     titles(0)="U RMSE (m/s)"
     titles(1)="U BIAS (m/s)"
     titles(2)="V RMSE (m/s)"
     titles(3)="V BIAS (m/s)"
     titles(4)="U NUM"
     titles(5)="V NUM"

    print(" process "+ii)
    if( numstation .gt. 1 ) then
      resources@tiYAxisString = "Height(m) "+chartostring(stanames(ii,:))     
    end if

    plot(0) = gsn_csm_contour(xwks,transpose(rmse(ii,:,:,0)), resources)
    plot(1) = gsn_csm_contour(xwks,transpose(bias(ii,:,:,0)), resources)

    plot(2) = gsn_csm_contour(xwks, transpose(rmse(ii,:,:,1)), resources)
    plot(3) = gsn_csm_contour(xwks, transpose(bias(ii,:,:,1)), resources)

    plot(4) = gsn_csm_contour(xwks,transpose(numpair(ii,:,:,0)), resources)
    plot(5) = gsn_csm_contour(xwks, transpose(numpair(ii,:,:,1)), resources)

    resourcesP                    =  True              ; plot mods desired
    resourcesP@gsnPanelFigureStrings = titles
    resourcesP@gsnPanelFigureStringsFontHeightF = 0.005
    resourcesP@amJust = "TopRight"
    resourcesP@gsnPanelFigureStringsPerimOn = False
    gsn_panel(xwks,plot,(/3,2/),resourcesP)

  end do

  delete(plot)
  delete(resources)
  delete(resourcesP)

end
