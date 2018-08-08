load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

;=================================================;
; open file and read in data
;=================================================;

;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_all.bin"
;  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0187_all.bin"
;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0133_all.bin"
;  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0133_all.bin"
  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_station.bin"
  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0187_station.bin"

  load "read_head.ncl"
  load "read_data.ncl"

; numstation,nfcst,nlvl,numvar

  yy = new ((/1,nfcst/), "float")
  xwks = gsn_open_wks("pdf","static_133_forecast")
  gsn_define_colormap(xwks,"cosam12")

  resources                  =  True              ; plot mods desired
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame

  resources@xyMarkLineModes   = "Markers"                ; choose which have markers
  resources@xyMarkerColor = "black"

  resources@xyMarkLineModes   = "Lines"                ; choose which have markers

  resources@gsnYRefLine           = 0.              ; reference line   
;  resources@gsnXYBarChart         = True            ; create bar chart 
;  resources@gsnAboveYRefLineColor = "red"           ; above ref line fill red
;  resources@gsnBelowYRefLineColor = "blue"          ; below ref line fill blue
;  resources@gsnXYBarChartBarWidth = 0.5

  resources@xyLineColors      = (/"red","green"/)  ; line colors
  resources@xyLineThicknesses = (/2.0,2.0/)        ; line thicknesses
  resources@xyDashPatterns    = (/0.0,0.0/)        ; line patterns

  resources@vpHeightF = 0.30        ; Changes the aspect ratio
  resources@vpWidthF  = 0.75
  resources@gsnMaximize = True

  resources@tiXAxisString = "Forecast Hours" ; y-axis label

  ifcst = ifcst/60
  titles = new(12,string)
  plot = new(12,graphic)

  do ii=0,numstation-1
     titles(0)="T RMSE"
     titles(1)="T BIAS"
     titles(2)="T NUM"
     titles(3)="Q RMSE"
     titles(4)="Q BIAS"
     titles(5)="Q NUM"
     titles(6)="U RMSE"
     titles(7)="U BIAS"
     titles(8)="U NUM"
     titles(9)="V RMSE"
     titles(10)="V BIAS"
     titles(11)="V NUM"

;    resources@trYMinF   =  0.0        ; min value on y-axis
;    resources@trYMaxF   =  4.0        ; max value on y-axis
;    yy(0,:)=rmse(ii,2,:,0)
;    yy(1,:)=rmse(ii,2,:,0)
    
    print(" process "+ii)

    resources@tiYAxisString = "c    all"      
    if( numstation .gt. 1 ) then
       resources@tiYAxisString = "c " + chartostring(stanames(ii,:))     
    end if
    plot(0) = gsn_csm_xy(xwks, ifcst, rmse(ii,:,0,0), resources)
    plot(1) = gsn_csm_xy(xwks, ifcst, bias(ii,:,0,0), resources)
    resources@tiYAxisString = "number"      
    plot(2) = gsn_csm_xy(xwks, ifcst, numpair(ii,:,0,0), resources)

    resources@tiYAxisString = "g/kg"      
    plot(3) = gsn_csm_xy(xwks, ifcst, rmse(ii,:,0,1), resources)
    plot(4) = gsn_csm_xy(xwks, ifcst, bias(ii,:,0,1), resources)
    resources@tiYAxisString = "number"      
    plot(5) = gsn_csm_xy(xwks, ifcst, numpair(ii,:,0,1), resources)

    resources@tiYAxisString = "m/s"      
    plot(6) = gsn_csm_xy(xwks, ifcst, rmse(ii,:,0,2), resources)
    plot(7) = gsn_csm_xy(xwks, ifcst, bias(ii,:,0,2), resources)
    resources@tiYAxisString = "number"      
    plot(8) = gsn_csm_xy(xwks, ifcst, numpair(ii,:,0,2), resources)

    resources@tiYAxisString = "m/s"      
    plot(9) = gsn_csm_xy(xwks, ifcst, rmse(ii,:,0,3), resources)
    plot(10) = gsn_csm_xy(xwks, ifcst, bias(ii,:,0,3), resources)
    resources@tiYAxisString = "number"      
    plot(11) = gsn_csm_xy(xwks, ifcst, numpair(ii,:,0,3), resources)

    resourcesP                    =  True              ; plot mods desired
    resourcesP@gsnPanelFigureStrings = titles
    resourcesP@gsnPanelFigureStringsFontHeightF = 0.01
    resourcesP@amJust = "TopRight"
    resourcesP@gsnPanelFigureStringsPerimOn = False
;    resourcesP@tiMainString    = " forecasr error ofr Station " + chartostring(stanames(ii,:))
    gsn_panel(xwks,plot,(/4,3/),resourcesP)

  end do

  delete(plot)
  delete(resources)
  delete(resourcesP)

end
