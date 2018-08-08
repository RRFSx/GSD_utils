load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0120_all.bin"
  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0120_all.bin"
;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0120_station.bin"
;  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0120_station.bin"

  load "read_head.ncl"
  load "read_data.ncl"

; numstation,nfcst,nlvl,numvar
;  print(rmse(0,:,:,0))
;  exit
  value = new ((/nlvl,nfcst/), "float")
  xx = new ((/nfcst/), "float")
  yy = new ((/nlvl/), "float")
  xx = ifcst/60
  yy = rlevel(0:nlvl-1)
  yy@units     = "hPa"
  rmse!1="time"
  rmse!2="lev"
  rmse&lev  = yy
  rmse&time = xx
  bias!1="time"
  bias!2="lev"
  bias&lev  = yy
  bias&time = xx
  numpair!1="time"
  numpair!2="lev"
  numpair&lev  = yy
  numpair&time = xx

  xwks = gsn_open_wks("pdf","static_120_forecast")
  gsn_define_colormap(xwks,"cosam12")

  resources                 =  True              ; plot mods desired
  resources@gsnDraw         = False              ; Do not draw plot
  resources@gsnFrame        = False              ; Do not advance frame

  resources@sfXArray        = xx
  resources@sfYArray        = yy

  resources@cnFillOn            = True          ; turn on color
  resources@cnFillPalette       = "gui_default" ; set color map
  resources@cnFillMode          = "CellFill"
  resources@cnLinesOn           = False
  resources@cnLineLabelsOn      = False

  resources@vpHeightF = 0.30        ; Changes the aspect ratio
  resources@vpWidthF  = 0.60
  resources@gsnMaximize = True

  resources@tiXAxisString = "Forecast Hours" ; y-axis label
  resources@tiYAxisString = "Pressure(hPa)"     

  titles = new(12,string)
  plot = new(12,graphic)

  rmse@_FillValue = -99999.0
  bias@_FillValue = -99999.0

  do ii=0,numstation-1
     titles(0)="T RMSE (c)"
     titles(1)="T BIAS (c)"
     titles(2)="T NUM"
     titles(3)="Q RMSE (g/kg)"
     titles(4)="Q BIAS (g/kg)"
     titles(5)="Q NUM"
     titles(6)="U RMSE (m/s)"
     titles(7)="U BIAS (m/s)"
     titles(8)="U NUM"
     titles(9)="V RMSE (m/s)"
     titles(10)="V BIAS (m/s)"
     titles(11)="V NUM"

    if( numstation .gt. 1 ) then
       resources@tiYAxisString = "Pressure(hPa) " + chartostring(stanames(ii,:))     
    end if

    print(" process "+ii)

    value(:,:) = transpose(rmse(ii,:,:,0))
    plot(0) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(bias(ii,:,:,0))
    plot(1) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(numpair(ii,:,:,0))
    plot(2) = gsn_csm_pres_hgt(xwks,value, resources)

    value(:,:) = transpose(rmse(ii,:,:,1))
    plot(3) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(bias(ii,:,:,1))
    plot(4) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(numpair(ii,:,:,1))
    plot(5) = gsn_csm_pres_hgt(xwks,value, resources)

    value(:,:) = transpose(rmse(ii,:,:,2))
    plot(6) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(bias(ii,:,:,2))
    plot(7) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(numpair(ii,:,:,2))
    plot(8) = gsn_csm_pres_hgt(xwks,value, resources)

    value(:,:) = transpose(rmse(ii,:,:,3))
    plot(9) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(bias(ii,:,:,3))
    plot(10) = gsn_csm_pres_hgt(xwks,value, resources)
    value(:,:) = transpose(numpair(ii,:,:,3))
    plot(11) = gsn_csm_pres_hgt(xwks,value, resources)

    resourcesP                    =  True              ; plot mods desired
    resourcesP@gsnPanelFigureStrings = titles
    resourcesP@gsnPanelFigureStringsFontHeightF = 0.005
    resourcesP@amJust = "TopRight"
    resourcesP@gsnPanelFigureStringsPerimOn = False
    gsn_panel(xwks,plot,(/4,3/),resourcesP)

  end do

  delete(plot)
  delete(resources)
  delete(resourcesP)

end
