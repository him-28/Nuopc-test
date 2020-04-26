; NUOPC Diagnostic File Difference Plots
; Authors: Daniel Rosen
; Email: daniel.rosen@noaa.gov
; Last Modified: 2020-03-17
; Usage: ncl anim_NUOPC_diagnostic_diff.nc 'p="<COMP1,COMP2>"'
;          ['d="<dir>"'] ['v="<variables>"']
;          ['c="<scale_min,scale_max"'] ['s="<time_steps>"']
;          ['o="<write_point1,write_point2>"'] ['t="<state1,state2>"']
;          ['grids="+dblq+"<grid_file1,grid_file2>"+dblq+"']
;          ['a="<True/False>"'] ['g="<True/False>"']
;          ['r="+dblq+"<minLat,maxLat,minLon,maxLon>"]
;          ['prefix="<output_prefix>"']
;          ['reverse="<True/False,True/False>"']
;          ['phase="<phase>"']

begin
  ;--- parameters ---
  pwd = systemfunc("pwd")
  script = get_script_name()
  dblq = integertochar(34)
  nl = integertochar(10)
  tab = integertochar(9)
  usage = "### Usage ###"
  usage = usage+nl+tab+"  ncl "+script+" OPTIONS"
  usage = usage+nl+tab+"### Options ###"
  usage = usage+nl+tab+"  'p="+dblq+"<COMP1,COMP2>"+dblq+"'"
  usage = usage+nl+tab+"  ['d="+dblq+"<dir>"+dblq+"']"
  usage = usage+nl+tab+"  ['v="+dblq+"<variables>"+dblq+"']"
  usage = usage+nl+tab+"  ['c="+dblq+"<scale_min,scale_max>"+dblq+"']"
  usage = usage+nl+tab+"  ['s="+dblq+"<time_steps>"+dblq+"']"
  usage = usage+nl+tab+"  ['o="+dblq+"<write_point1,write_point2>"+dblq+"']"
  usage = usage+nl+tab+"  ['t="+dblq+"<state1,state2>"+dblq+"']"
  usage = usage+nl+tab+"  ['grids="+dblq+"<grid_file1,grid_file2>"+dblq+"']"
  usage = usage+nl+tab+"  ['a="+dblq+"<True/False>"+dblq+"']"
  usage = usage+nl+tab+"  ['g="+dblq+"<True/False>"+dblq+"']"
  usage = usage+nl+tab+"  ['r="+dblq+"<minLat,maxLat,minLon,maxLon>"+dblq+"']"
  usage = usage+nl+tab+"  ['prefix="+dblq+"<output_prefix>"+dblq+"']"
  usage = usage+nl+tab+"  ['reverse="+dblq+"<True/False,True/False>"+dblq+"']"
  usage = usage+nl+tab+"  ['phase="+dblq+"<phase>"+dblq+"']"

  dflt_outtype = "nuopc_diag"
  dflt_title = "NUOPC Diag"
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
                      "inst_temp_height_lowest,gt", \
                      "inst_zonal_wind_height10m,u10", \
                      "mean_down_lw_flx,lwflxd", \
                      "mean_down_sw_flx,swflxd", \
                      "mean_net_lw_flx,lwflxd", \
                      "mean_net_sw_flx,swflxd", \
                      "mean_zonal_moment_flx,u10", \
                      "mean_merid_moment_flx,v10", \
                      "mean_prec_rate,prcp" /)
;  dflt_filenamev = (/ "sst", "sst" /)
;  dflt_filenamev = (/ "sst", "sea_surface_temperature" /)
;  dflt_filenamev = (/ "inst_merid_wind_height10m", "u10" /)
;  dflt_filenamev = (/ "inst_pres_height_surface", "mslprs" /)
;  dflt_filenamev = (/ "inst_spec_humid_height2m", "airhum" /)
;  dflt_filenamev = (/ "inst_temp_height2m", "airtmp" /)
  dflt_filenamev = (/ "inst_temp_height_surface", "gt" /)
;  dflt_filenamev = (/ "inst_zonal_wind_height10m", "v10" /)
;  dflt_filenamev = (/ "mean_down_lw_flx", "lwflxd" /)
;  dflt_filenamev = (/ "mean_down_sw_flx", "swflxd" /)
;  dflt_filenamev = (/ "mean_prec_rate", "prcp" /)
  dflt_timesteps = (/ "2019_08_29_00_06_00_000" /)
;  dflt_timesteps = (/ "2019_08_29_00_00_00_000", "2019_08_29_00_06_00_000", \
;                      "2019_08_29_00_12_00_000", "2019_08_29_00_18_00_000", \
;                      "2019_08_29_00_24_00_000", "2019_08_29_00_30_00_000", \
;                      "2019_08_29_00_36_00_000", "2019_08_29_00_42_00_000", \
;                      "2019_08_29_00_48_00_000", "2019_08_29_00_54_00_000" /)
  dflt_gbl = False
  dflt_anim = True
  dflt_prefix = "NUOPC DIFF"
  dflt_fillValue = (/ 1.267650600228229e+30, 1.267650600228229e+30 /)
;  dflt_fillValue = (/ 9.99e+20, 9.99e+20 /)
 
  ;--- command line arguments ---
  if (isvar("p")) then
    cmpList = str_split(p,",")
  else
    print("### ERROR ###")
    print("  Components are not defined. ['p="+dblq+"<COMP1,COMP2>"+dblq+"']")
    print(""+usage)
    exit()
  end if
  if (isvar("d")) then
    rundir = d
  else
    rundir = pwd
  end if
  if (isvar("prefix")) then
    title = prefix
    outtype = str_sub_str(prefix," ","_")
  else
    title = dflt_title
    outtype = dflt_outtype
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
  if (isvar("o")) then
    points = str_split(o,",")
  else
    points = dflt_points
  end if
  if (isvar("t")) then
    states = str_split(t,",")
  else
    states = dflt_states
  end if
  if (isvar("grids")) then
    usegrids = True
    grid_files = str_split(grids,",")
  else
    usegrids = False
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
      outtype = outtype+"_CONUS"
      title = title+" Conus"
    else if (str_upper(r) .eq. "N.ATLANTIC") then
      mpRegion = (/ 10.0, 40.0, -90.0, -45.0 /)
      outtype = outtype+"_NATLANTIC"
      title = title+" N.Atlantic"
    else if (str_upper(r) .eq. "PUERTORICO") then
      mpRegion = (/ 17.5, 19.5, -67.5, -65.0 /)
      outtype = outtype+"_PUERTORICO"
      title = title+" Puerto Rico"
    else if (str_upper(r) .eq. "FLORIDA") then
      mpRegion = (/ 24.5, 30.5, -83.0, -79.5 /)
      outtype = outtype+"_FLORIDA"
      title = title+" Florida"
    else
      mpRegion = stringtofloat(str_split(r,","))
      outtype = outtype+"_REGIONAL"
      title = title+" Regional"
    end if
    end if
    end if
    end if
  else
    mpZoom = False
    mpRegion = (/ 0.0, 0.0, 0.0, 0.0 /)
  end if
  if (isvar("reverse")) then
    reverse_data = str_split(reverse,",")
    reverse1 = (str_upper(reverse_data(0)) .eq. "TRUE")
    reverse2 = (str_upper(reverse_data(1)) .eq. "TRUE")
  else
    reverse1 = False
    reverse2 = False
  end if
  if (isvar("phase")) then
    phase = phase
  else
    phase = dflt_phase
  end if

  ;--- command line arguments ---
  print(""+usage)
  print("### Pre-Configured Variables ###")
  print("  "+dflt_variables(:))
  print("### Run Settings ###")
  print("  Component(s)      = "+cmpList(0)+","+cmpList(1))
  print("  Directory         = "+rundir)
  print("  Variable(s)       = "+filenamev(0)+","+filenamev(1))
  print("  Time Step(s)      = "+timesteps(:))
  print("  Phase             = "+phase)
  print("  Points            = "+points(0)+","+points(1))
  print("  Reverse Data      = "+reverse1+","+reverse2)
  print("  States            = "+states(0)+","+states(1))
  if (usegrids) then
    print("  Grid Files        = "+grid_files(:))
  end if
  print("  Animation         = "+anim)
  print("  Global Map        = "+gblmap)
  print("  Title             = "+title)
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

  ;--- add dimensions to temporary variables ---
  if (usegrids) then
    rad2deg = 180.0/get_pi("f")
    if (isfilepresent(rundir+"/"+grid_files(0))) then
      gf1 = addfile(rundir+"/"+grid_files(0), "r")
    else
      print("### ERROR ###")
      print("  File is missing: "+grid_files(0))
      exit()
    end if
    dimslat = dimsizes(gf1->lat_center)
    dimslon = dimsizes(gf1->lon_center)
    ny = dimslat(1)
    nx = dimslon(0)
    lat1 = new((/ nx,ny /), "double")
    lat1 = gf1->lat_center(:,:)
    lat1@long_name = "T-cell latitude"
    lat1@units = "degrees_N"
    lat1@_FillValue = dflt_fillValue(0)
    lon1 = new((/ nx,ny /), "double")
    lon1 = gf1->lon_center(:,:)
    lon1@long_name = "T-cell longitude"    
    lon1@units = "degrees_E"
    lon1@_FillValue = dflt_fillValue(0)

    if (isfilepresent(rundir+"/"+grid_files(1))) then
      gf2 = addfile(rundir+"/"+grid_files(1), "r")
    else
      print("### ERROR ###")
      print("  File is missing: "+grid_files(1))
      exit()
    end if
    dimslat = dimsizes(gf2->lat_center)
    dimslon = dimsizes(gf2->lon_center)
    ny = dimslat(1)
    nx = dimslon(0)
    lat2 = new((/ nx,ny /), "double")
    lat2 = gf2->lat_center(:,:)
    lat2@long_name = "T-cell latitude"
    lat2@units = "degrees_N"
    lat2@_FillValue = dflt_fillValue(1)
    lon2 = new((/ nx,ny /), "double")
    lon2 = gf2->lon_center(:,:)
    lon2@long_name = "T-cell longitude"
    lon2@units = "degrees_E"
    lon2@_FillValue = dflt_fillValue(1)

  end if

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
  vardims = dimsizes(nc->$vNames(0)$)
  nx = vardims(1)
  ny = vardims(2)
  data1 = new((/ nx, ny /), "double")

  ;--- read data ---
  data1(:,:) = (/ nc->$vNames(0)$(0,:,:) /)
  if (reverse1) then
    data1(:,:) = data1(::-1,::-1)
  end if
  data1@_FillValue = dflt_fillValue(0)
  units(0) = variables(0)
  label(0) = variables(0)
;  data1 = mask(data1,gf1->mask,1)

  min_data(0) = min(data1(:,:))
  max_data(0) = max(data1(:,:))
  print(variables(0)+": range="+min_data(0)+":"+max_data(0)+" in "+units(0))

  if (isnan_ieee(min_data(0)) .or. isnan_ieee(max_data(0))) then
    print("### ERROR ###")
    print("  NaN found. Replacing with "+dflt_fillValue(0))
    replace_ieeenan(data1,data1@_FillValue, 0)
  end if

  ;--- add dimensions to temporary variables ---
  if (usegrids) then
    data1@lat2d = lat1
    data1@lon2d = lon1
  end if

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
  vardims = dimsizes(nc->$vNames(0)$)
  nx = vardims(1)
  ny = vardims(2)
  data2 = new((/ nx, ny /), "double")

  ;--- read data ---
  data2(:,:) = (/ nc->$vNames(0)$(0,:,:) /)
  if (reverse2) then
    data2(:,:) = data2(::-1,::-1)
  end if
  data2@_FillValue = dflt_fillValue(1)
  units(1) = variables(1)
  label(1) = variables(1)
;  data2 = mask(data2,gf2->mask,1)

  min_data(1) = min(data2(:,:))
  max_data(1) = max(data2(:,:))
  print(variables(1)+": range="+min_data(1)+":"+max_data(1)+" in "+units(1))

  if (isnan_ieee(min_data(1)) .or. isnan_ieee(max_data(1))) then
    print("### ERROR ###")
    print("  NaN found. Replacing with "+dflt_fillValue(1))
    replace_ieeenan(data2,data2@_FillValue, 0)
  end if

  ;--- add dimensions to temporary variables ---
  if (usegrids) then
    data2@lat2d = lat2
    data2@lon2d = lon2
  end if

  ;--- plot ---
  wks = gsn_open_wks("png", "plot_"+outtype+"_"+variables(0)+"_"+timesteps(k))

  plot = new(2,graphic)
  res = True
  res@gsnDraw  = False 
  res@gsnFrame = False
  res@gsnMaximize   = True
  res@cnInfoLabelOn = False
  res@cnFillOn = True
  if (mpZoom) then
    res@cnFillMode = "CellFill"
  else
    res@cnFillMode = "RasterFill"
  end if
;  res@cnFillPalette  = "cyclic"
  res@cnLinesOn      = False
  res@cnLineLabelsOn = False
  res@cnFixFillBleed = False
  res@cnMissingValFillColor = "gray70"
  if (gblmap) then
    res@mpFillOn = False
    res@mpOutlineOn = True
  end if
  res@tmYLOn = False
  res@tmYLLabelsOn = False
  res@tmYROn = False
  res@tmYRLabelsOn = False
  res@tmXBOn = False
  res@tmXBLabelsOn = False
  res@tmXTOn = False
  res@tmXTLabelsOn = False
  res@tiYAxisString = ""+str_upper(label(0))
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
;  res@gsnStringFontHeightF = .03
  res@trGridType = "TriangularMesh"
  if (usrscale) then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(scalemin,scalemax,51)
  else if (variables(0) .eq. "sea_surface_temperature") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(265.0,315.0,51)
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
    res@cnLevels = fspan(265.0,315.0,51)
  else if (variables(0) .eq. "inst_temp_height_surface") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(265.0,315.0,51)
  else if (variables(0) .eq. "inst_temp_height_lowest") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(265.0,315.0,51)
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
  else if (variables(0) .eq. "mean_net_lw_flx") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(-200.0,100.0,61)
  else if (variables(0) .eq. "mean_net_sw_flx") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(0.0,300.0,61)
  else if (variables(0) .eq. "mean_zonal_moment_flx") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(-1,1.0,51)
  else if (variables(0) .eq. "mean_merid_moment_flx") then
    res@cnFillPalette = "NCV_jet"
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = fspan(-1.0,1.0,51)
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
  end if
  end if
  end if
  end if
  end if

  ;--- plot control and test ---
  res@gsnCenterString = ""
  if (gblmap .or. mpZoom) then
    res@gsnCenterString = cmpList(0)+" @ "+timesteps(k)+"h"
    plot(0) = gsn_csm_contour_map(wks, data1(:,:), res)
    res@gsnCenterString = cmpList(1)+" @ "+timesteps(k)+"h"
    plot(1) = gsn_csm_contour_map(wks, data2(:,:), res)
  else
    res@gsnCenterString = cmpList(0)+" @ "+timesteps(k)+"h"
    plot(0) = gsn_csm_contour(wks, data1(:,:), res)
    res@gsnCenterString = cmpList(1)+" @ "+timesteps(k)+"h"
    plot(1) = gsn_csm_contour(wks, data2(:,:), res)
  end if

  ;--- panel first two plots ---
  pres1                     = True
  pres1@gsnPanelMainString  = str_capital(title)
  pres1@gsnPanelMainFont    = "helvetica-bold"
  pres1@gsnPanelLabelBar    = True
  pres1@gsnFrame            = False
  pres1@lbOrientation       = "horizontal"
;  pres1@gsnPanelBottom      = 0.4
  gsn_panel(wks,plot,(/2,1/),pres1)
  delete(res@cnLevels)

  frame(wks)

  end do ; nts

  if (anim .and. (nts .gt. 1)) then
    gname = "plot_"+outtype+"_"+variables(0)+"_*.png"
    aname = "anim_"+outtype+"_"+variables(0)+".gif"
    print("Animating "+aname)
    stat = systemfunc("convert -delay 50 "+gname+" "+aname)
  else if(anim .and. (nts .le. 1)) then
    print("Skipping animation: time step count less than 2.")
  end if
  end if

end
