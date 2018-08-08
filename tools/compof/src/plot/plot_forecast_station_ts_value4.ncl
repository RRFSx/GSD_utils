load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/time_axis_labels.ncl"


begin

;=================================================;
; open file and read in data
;=================================================;

;  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_ts.bin"
;  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0187_ts.bin"
  filehead="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_head_0187_station_ts.bin"
  filedata="/mnt/lfs1/projects/rtwbl/mhu/rapcode/work/tool/run/stats_data_0187_station_ts.bin"

  load "read_head.ncl"
  load "read_data_ts.ncl"

;  print(rmse)
;  print(bias)
;  print(numpair)
;  numstation,nfcst,nlvl,numvar
;  numtimelevel
;  print(rmse_ts(0,0,0,0,:))
;  print(bias_ts(0,0,0,0,:))
;  print(numpair_ts(0,0,0,0,:))

;  print(timestring_ts)
  times  = no_ts
  ntimes = dimsizes(times)

  yy = new ((/6,numtimelevel/), "float")
  xwks = gsn_open_wks("pdf","static_187_station_forecast_ts_value4")
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

  resources@xyLineColors      = (/"red","black","green","blue","purple","yellow"/)  ; line colors
  resources@xyLineThicknesses = (/2.0,2.0,1.0,1.0,1.0,1.0/)        ; line thicknesses
  resources@xyDashPatterns    = (/0.0,2.0,0.0,0.0,0.0,0.0/)        ; line patterns

  resources@vpHeightF = 0.30        ; Changes the aspect ratio
  resources@vpWidthF  = 0.75
  resources@gsnMaximize = True

  resources@tiXAxisString = "Forecast Hours" ; y-axis label

  resources@tmXTOn  = False
  restick           = True
  restick@ttmFormat = "%N/%D %H:%M"
  time_axis_labels(times,resources,restick)

  titles = new(12,string)
  plot = new(12,graphic)

;
; Set up resources for a customized legend.
;
  lgres                    = True
  lgres@lgLineColors       = (/"red","black","green","blue","purple","yellow"/)
  lgres@lgItemType         = "Lines"        ; show lines only (default)
  lgres@lgLabelFontHeightF = .08            ; legend label font thickness
  lgres@vpWidthF           = 0.13           ; width of legend (NDC)
  lgres@vpHeightF          = 0.15           ; height of legend (NDC)
  lgres@lgPerimThicknessF  = 2.0            ; thicken the box perimeter
  lgres@lgMonoDashIndex    = True
  lgres@lgDashIndex        = 0

  labels = (/"fcst","obs","i,j","i+1,j","i,j+1","i+1,j+1"/)
  nitems = dimsizes(labels)                 ; number of legend items
;
; Create legend
  lbid = gsn_create_legend(xwks,nitems,labels,lgres)

;
; Use gsn_add_annotation to attach this legend to our existing plot.
; This way, if we resize the plot, the legend will stay with the
; plot and be resized automatically.
;
  amres                  = True

;
; Point (0,0) is the dead center of the plot. Point (0,.5) is center,
; flush bottom. Point (0.5,0.5) is flush bottom, flush right.
;
  amres@amJust           = "BottomRight"    ; Use bottom right corner of box
                                            ; for determining its location.
  amres@amParallelPosF   = 0.5              ; Move legend to right
  amres@amOrthogonalPosF = 0.5              ; Move legend down.

;  do ii=60,62
  do ii=0,numstation-1
     titles(0)="T 0-h"
     titles(1)="T 1-h"
     titles(2)="T 2-h"
     titles(3)="Q 0-h"
     titles(4)="Q 1-h"
     titles(5)="Q 2-h"
     titles(6)="U 0-h"
     titles(7)="U 1-h"
     titles(8)="U 2-h"
     titles(9)="V 0-h"
     titles(10)="V 1-h"
     titles(11)="V 2-h"

;    resources@trYMinF   =  0.0        ; min value on y-axis
;    resources@trYMaxF   =  4.0        ; max value on y-axis
;    yy(0,:)=rmse(ii,2,:,0)
;    yy(1,:)=rmse(ii,2,:,0)
    
    print(" process "+ii)

    do nn=0,3
      if( nn .eq. 0) then
         resources@tiYAxisString = "c    all"      
         if( numstation .gt. 1 ) then
           resources@tiYAxisString = "c " + chartostring(stanames(ii,:))     
         end if
      end if
      if( nn .eq. 1) then
        resources@tiYAxisString = "g/kg"      
      end if
      if( nn .eq. 2 .or. nn .eq. 3) then
        resources@tiYAxisString = "m/s"      
      end if
      yy(0,:)=rmse_ts(ii,0,0,nn,:)
      yy(1,:)=bias_ts(ii,0,0,nn,:)
      yy(2,:)=value4_ts(ii,0,0,nn,:)
      yy(3,:)=value4_ts(ii,0,1,nn,:)
      yy(4,:)=value4_ts(ii,0,2,nn,:)
      yy(5,:)=value4_ts(ii,0,3,nn,:)
      plot(nn*3+0) = gsn_csm_xy(xwks, times, yy, resources)
      yy(0,:)=rmse_ts(ii,1,0,nn,:)
      yy(1,:)=bias_ts(ii,1,0,nn,:)
      yy(2,:)=value4_ts(ii,1,0,nn,:)
      yy(3,:)=value4_ts(ii,1,1,nn,:)
      yy(4,:)=value4_ts(ii,1,2,nn,:)
      yy(5,:)=value4_ts(ii,1,3,nn,:)
      plot(nn*3+1) = gsn_csm_xy(xwks, times, yy, resources)
;      yy(0,:)=rmse_ts(ii,2,0,nn,:)
;      yy(1,:)=bias_ts(ii,2,0,nn,:)
;      yy(2,:)=value4_ts(ii,2,0,nn,:)
;      yy(3,:)=value4_ts(ii,2,1,nn,:)
;      yy(4,:)=value4_ts(ii,2,2,nn,:)
;      yy(5,:)=value4_ts(ii,2,3,nn,:)
;      plot(nn*3+2) = gsn_csm_xy(xwks, times, yy, resources)
      if( nn .eq. 0 .or. nn .eq. 2 .or. nn .eq. 3 ) then
         yy(0,:)=value4_ts(ii,1,0,4,:)
         yy(1,:)=value4_ts(ii,1,0,4,:)
         yy(2,:)=value4_ts(ii,1,0,4,:)
         yy(3,:)=value4_ts(ii,1,1,4,:)
         yy(4,:)=value4_ts(ii,1,2,4,:)
         yy(5,:)=value4_ts(ii,1,3,4,:)
      end if
      if( nn .eq. 1 ) then
         yy(0,:)=value4_ts(ii,1,0,5,:)
         yy(1,:)=value4_ts(ii,1,0,5,:)
         yy(2,:)=value4_ts(ii,1,0,5,:)
         yy(3,:)=value4_ts(ii,1,1,5,:)
         yy(4,:)=value4_ts(ii,1,2,5,:)
         yy(5,:)=value4_ts(ii,1,3,5,:)
      end if
      plot(nn*3+2) = gsn_csm_xy(xwks, times, yy, resources)
    end do
    annoid = gsn_add_annotation(plot(0),lbid,amres)  ; add legend to plot

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
