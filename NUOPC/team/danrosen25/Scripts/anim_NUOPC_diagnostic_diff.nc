; NUOPC Diagnostic File Difference Plots
; Authors: Daniel Rosen
; Email: daniel.rosen@noaa.gov
; Last Modified: 2020-03-17
; Usage: ncl anim_NUOPC_diagnostic_diff.ncD ['d="<dir>"']
;          'p="<COMP1,COMP2>"' ['v="<variables>"']
;          ['c="<scale_min,scale_max"'] ['s="<time_steps>"']
;          ['a="<True/False>"'] ['g="<True/False>"']
;          ['r="+dblq+"<minLat,maxLat,minLon,maxLon>"]

begin
  ;--- parameters ---
  pwd = systemfunc("pwd")
  script = get_script_name()
  dblq = integertochar(34)
  usage = "ncl "+script
  usage = usage+" 'p="+dblq+"<COMP1,COMP2>"+dblq+"'"
  usage = usage+" ['d="+dblq+"<dir>"+dblq+"']"
  usage = usage+" ['v="+dblq+"<variables>"+dblq+"']"
  usage = usage+" ['s="+dblq+"<time_steps>"+dblq+"']"
  usage = usage+" ['a="+dblq+"<True/False>"+dblq+"']"
  usage = usage+" ['g="+dblq+"<True/False>"+dblq+"']"
  usage = usage+" ['r="+dblq+"<minLat,maxLat,minLon,maxLon>"+dblq+"']"
  dflt_components = (/ "ATM", "OCN" /)
  dflt_phase = "Run"
  dflt_points = (/ "enter", "enter" /)
  dflt_states = (/ "export", "import" /)
  dflt_variables = (/ "sst,sea_surface_temperature", \
                      "inst_merid_wind_height10m,v10", \
                      "inst_pres_height_surface,mslprs", \
                      "inst_spec_humid_height2m,airhum", \
                      "inst_temp_height2m,airtmp", \
                      "inst_temp_height_surface,gt", \
                      "inst_zonal_wind_height10m,u10", \
                      "mean_down_lw_flx,lwflxd", \
                      "mean_down_sw_flx,swflxd", \
                      "mean_prec_rate,prcp" /)
  dflt_filenamev = (/ "sst", "sea_surface_temperature" /)
;  dflt_filenamev = (/ "inst_merid_wind_height10m", "u10" /)
;  dflt_filenamev = (/ "inst_pres_height_surface", "mslprs" /)
;  dflt_filenamev = (/ "inst_spec_humid_height2m", "airhum" /)
;  dflt_filenamev = (/ "inst_temp_height2m", "airtmp" /)
;  dflt_filenamev = (/ "inst_temp_height_surface", "gt" /)
;  dflt_filenamev = (/ "inst_zonal_wind_height10m", "v10" /)
;  dflt_filenamev = (/ "mean_down_lw_flx", "lwflxd" /)
;  dflt_filenamev = (/ "mean_down_sw_flx", "swflxd" /)
;  dflt_filenamev = (/ "mean_prec_rate", "prcp" /)
;  dflt_timesteps = (/ "2019_08_29_00_00_00_000" /)
  dflt_timesteps = (/ "2019_08_29_00_00_00_000", "2019_08_29_00_06_00_000", \
                      "2019_08_29_00_12_00_000", "2019_08_29_00_18_00_000", \
                      "2019_08_29_00_24_00_000", "2019_08_29_00_30_00_000", \
                      "2019_08_29_00_36_00_000", "2019_08_29_00_42_00_000", \
                      "2019_08_29_00_48_00_000", "2019_08_29_00_54_00_000" /)
  dflt_gbl = False
  dflt_anim = True
  dflt_usrscale = False
  dflt_fillValue = (/ 1.267650600228229e+30, 1.267650600228229e+30 /)
 
  ;--- command line arguments ---
  if (isvar("p")) then
    cmpList = str_split(p,",")
  else
    print("### ERROR ###")
    print("  Components are not defined. ['p="+dblq+"<COMP1,COMP2>"+dblq+"']")
    print("  "+usage)
    exit()
  end if
  if (isvar("d")) then
    rundir = d
  else
    rundir = pwd
  end if
  if (isvar("v")) then
    filenamev = str_split(v,",")
  else
    filenamev = dflt_filenamev
  end if
  if (isvar("c")) then
    usrscale = True
    scalevals = str_split(c,",")
    scalemin = stringtofloat(scalevals(0))
    scalemax = stringtofloat(scalevals(1))
  else
    usrscale = False
  end if
  if (isvar("s")) then
    timesteps = str_split(s,",")
  else
    timesteps = dflt_timesteps
  end if
  if (isvar("a")) then
    anim = (str_upper(a) .eq. "TRUE")
  else
    anim = dflt_anim
  end if
  if (isvar("g")) then
    gblmap = (str_upper(g) .eq. "TRUE")
  else
    gblmap = dflt_gbl
  end if
  if (isvar("r")) then
    mpZoom = True
    if (str_upper(r) .eq. "CONUS") then
      mpRegion = (/ 18.0 , 49.0, -125.0, -62.5 /)
    else if (str_upper(r) .eq. "N.ATLANTIC") then
      mpRegion = (/ 10.0, 40.0, -90.0, -45.0 /)
    else
      mpRegion = stringtofloat(str_split(r,","))
    end if
    end if
  else
    mpZoom = False
    mpRegion = (/ 0.0, 0.0, 0.0, 0.0 /)
  end if

  phase = dflt_phase
  points = dflt_points
  states = dflt_states

  ;--- command line arguments ---
  print("### Usage ###")
  print("  "+usage)
  print("### Pre-Configured Variables ###")
  print("  "+dflt_variables(:))
  print("### Options ###")
  print("  Component(s)      = "+cmpList(0)+","+cmpList(1))
  print("  Directory         = "+rundir)
  print("  Variable(s)       = "+filenamev(0)+","+filenamev(1))
  print("  Time Step(s)      = "+timesteps(:))
  print("  Phase             = "+phase)
  print("  Points            = "+points(0)+","+points(1))
  print("  States            = "+states(0)+","+states(1))
  print("  Animation         = "+anim)
  print("  Global Map        = "+gblmap)
  if (mpZoom) then
    print("  Map LatLon        = ("+ \
      mpRegion(0)+":"+mpRegion(1)+","+ \
      mpRegion(2)+":"+mpRegion(3)+")")
  end if
  ;--- datasets ---
  dsets = (/ (/ "NUOPC-DIAG-COMP1", rundir+"/diagnostic_"+ \
                cmpList(0)+"_"+phase+"_"+points(0)+"_"+states(0)+"_" /), \
             (/ "NUOPC-DIAG-COMP2", rundir+"/diagnostic_"+ \
                cmpList(1)+"_"+phase+"_"+points(1)+"_"+states(1)+"_" /) /)
  ndsets = dimsizes(dsets(:,0))
  nts = dimsizes(timesteps)
  units = new((/ ndsets /), "string")
  label = new((/ ndsets /), "string")
  min_data = new((/ ndsets+1 /), "double")
  max_data = new((/ ndsets+1 /), "double")

  ;--- loop over files and process data ---
  files = new((/ 2 /), "string")
  variables = new((/ 2 /), "string")

  do k = 0, nts-1
  files(0) = dsets(0,1)+timesteps(k)+"_"+filenamev(0)+".nc"
  files(1) = dsets(1,1)+timesteps(k)+"_"+filenamev(1)+".nc"

  print("Processing "+dsets(0,0)+" "+files(0))

  ;--- read file ---
  if (isfilepresent(files(0))) then
    nc = addfile(files(0), "r")
  else
    print("### ERROR ###")
    print("  File is missing: "+files(0))
    continue
  end if

  vNames = getfilevarnames(nc)
  variables(0) = vNames(0)

  ;--- create temporary variable ---
  dimsx = dimsizes(nc->$variables(0)$)
  dimsy = dimsizes(nc->$variables(0)$)
  nx = dimsx(1)
  ny = dimsy(2)
  data1 = new((/ nx, ny /), "double")

  ;--- read data ---
  data1(:,:) = (/ nc->$variables(0)$(0,:,:) /)
  data1@_FillValue = dflt_fillValue(0)
  units(0) = variables(0)
  label(0) = variables(0)

  min_data(0) = min(data1(:,:))
  max_data(0) = max(data1(:,:))
  print(variables(0)+": range="+min_data(0)+":"+max_data(0)+" in "+units(0))

  if (isnan_ieee(min_data(0)) .or. isnan_ieee(max_data(0))) then
    print("### ERROR ###")
    print("  NaN found. Replacing with "+dflt_fillValue(0))
    replace_ieeenan(data1,data1@_FillValue, 0)
  end if

;  ;--- add dimensions to temporary variables ---
;  data!0 = "dsets"
;  data&dsets = dsets(:,0)
;  data!1 = "lat"
;  data!2 = "lon"
;  rad2deg = 180.0/get_pi("f")
;  lat1d = nc->lat(:,0)
;  data&lat = lat1d
;  lon1d = nc->lon(0,:)
;  data&lon = lon1d

  print("Processing "+dsets(1,0)+" "+files(1))

  ;--- read file ---
  if (isfilepresent(files(1))) then 
    nc = addfile(files(1), "r")
  else
    print("### ERROR ###")
    print("  File is missing: "+files(1))
    continue
  end if

  vNames = getfilevarnames(nc)
  variables(1) = vNames(0)

  ;--- create temporary variable ---
  dimsx = dimsizes(nc->$variables(1)$)
  dimsy = dimsizes(nc->$variables(1)$)
  nx = dimsx(1)
  ny = dimsy(2)
  data2 = new((/ nx, ny /), "double")

  ;--- read data ---
  data2(:,:) = (/ nc->$variables(1)$(0,:,:) /)
  data2@_FillValue = dflt_fillValue(1)
  units(1) = variables(1)
  label(1) = variables(1)

  min_data(1) = min(data2(:,:))
  max_data(1) = max(data2(:,:))
  print(variables(1)+": range="+min_data(1)+":"+max_data(1)+" in "+units(1))

  if (isnan_ieee(min_data(1)) .or. isnan_ieee(max_data(1))) then
    print("### ERROR ###")
    print("  NaN found. Replacing with "+dflt_fillValue(1))
    replace_ieeenan(data2,data2@_FillValue, 0)
  end if

;  ;--- add dimensions to temporary variables ---
;  data!0 = "dsets"
;  data&dsets = dsets(:,0)
;  data!1 = "lat"
;  data!2 = "lon"
;  rad2deg = 180.0/get_pi("f")
;  lat1d = nc->lat(:,0)
;  data&lat = lat1d
;  lon1d = nc->lon(0,:)
;  data&lon = lon1d

  ;--- plot ---
  wks = gsn_open_wks("png", "plot_NUOPC_diag_diff_"+variables(0)+"_"+timesteps(k))

  plot = new(2,graphic)
  res = True
  res@gsnDraw  = False 
  res@gsnFrame = False
  res@cnInfoLabelOn = False
  res@cnFillOn = True
  if (gblmap) then
    res@mpFillOn = False
    res@mpOutlineOn = True
  end if
  res@cnMissingValFillColor = "gray70"
  res@cnLinesOn = False
  res@cnLineLabelsOn = False
  res@tmYLOn = False
  res@tmYLLabelsOn = False
  res@tmYROn = False
  res@tmYRLabelsOn = False
  res@tmXBOn = False
  res@tmXBLabelsOn = False
  res@tmXTOn = False
  res@tmXTLabelsOn = False
  res@lbLabelBarOn = False
  if (gblmap) then
    res@gsnAddCyclic = True
  else
    res@gsnAddCyclic = False
  end if
  if (mpZoom) then
    res@mpLimitMode   = "LatLon"
    res@mpMinLatF     = mpRegion(0)
    res@mpMaxLatF     = mpRegion(1)
    res@mpMinLonF     = mpRegion(2)
    res@mpMaxLonF     = mpRegion(3)
  end if
  res@gsnLeftString = ""
  res@gsnRightString = ""
  res@gsnCenterString = ""
  res@gsnStringFontHeightF = .03
  res@trGridType = "TriangularMesh"
  res@cnFillMode = "RasterFill"
  if (usrscale) then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(scalemin,scalemax,51)
  else if (variables(0) .eq. "sea_surface_temperature") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(215.0,315.0,51)
  else if (variables(0) .eq. "inst_merid_wind_height10m") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(-30.0,30.0,61)
  else if (variables(0) .eq. "inst_pres_height_surface") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(65000.0,105000.0,41)
  else if (variables(0) .eq. "inst_spec_humid_height2m") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(0.0,0.03,61)
  else if (variables(0) .eq. "inst_temp_height2m") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(215.0,315.0,51)
  else if (variables(0) .eq. "inst_temp_height_surface") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(215.0,315.0,51)
  else if (variables(0) .eq. "inst_zonal_wind_height10m") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(-30.0,30.0,41)
  else if (variables(0) .eq. "mean_down_lw_flx") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(200.0,500.0,61)
  else if (variables(0) .eq. "mean_down_sw_flx") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(0.0,600.0,61)
  else if (variables(0) .eq. "mean_prec_rate") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(0.0,0.025,51)
  else
    res@cnFillPalette = "rainbow"
    if (min_data(0) .eq. max_data(0)) then
      res@cnLevelSelectionMode = "ExplicitLevels"
      res@cnLevels = fspan(min_data(0)-1,max_data(0)+1,21)
    else
      res@cnLevelSelectionMode = "ManualLevels"
      res@cnMinLevelValF = min_data(0)
      res@cnMaxLevelValF = max_data(0)
      res@cnLevelSpacingF  = (max_data(0)-min_data(0))/20
      res@cnLevels = fspan(-100,100,21)
    end if
  end if
  end if
  end if
  end if
  end if
  end if
  end if
  end if
  end if
  end if
  end if

  ;--- plot control and test ---
  res@gsnCenterString = ""
  if (gblmap .or. mpZoom) then
    res@gsnCenterString = "Control Run: "+ctldir
    plot(0) = gsn_csm_contour_map(wks, data1(:,:), res)
    res@gsnCenterString = "Test Run:    "+tstdir
    plot(1) = gsn_csm_contour_map(wks, data2(:,:), res)
  else
    res@gsnCenterString = "Component 1: "+cmpList(0)
    plot(0) = gsn_csm_contour(wks, data1(:,:), res)
    res@gsnCenterString = "Component 2:    "+cmpList(1)
    plot(1) = gsn_csm_contour(wks, data2(:,:), res)
  end if

  ;--- panel first two plots ---
  pres1                     = True
  pres1@gsnPanelMainString  = str_upper(label(0))+" ("+units(0)+") @"+timesteps(k)+"h"
  pres1@gsnPanelMainFont    = "helvetica-bold"
  pres1@gsnPanelLabelBar    = True
  pres1@gsnFrame            = False
  pres1@lbOrientation       = "horizontal"
  pres1@gsnPanelBottom      = 0.4
  gsn_panel(wks,plot,(/2,1/),pres1)
  delete(res@cnLevels)

  frame(wks)

  end do ; nts

  if (anim .and. (nts .gt. 1)) then
    gname = "plot_NUOPC_diag_diff_"+variables(0)+"_*.png"
    aname = "anim_NUOPC_diag_diff_"+variables(0)+".gif"
    print("Animating "+aname)
    stat = systemfunc("convert -delay 50 "+gname+" "+aname)
  else if(anim .and. (nts .le. 1)) then
    print("Skipping animation: time step count less than 2.")
  end if
  end if

end
