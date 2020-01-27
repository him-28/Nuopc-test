; FV3 phyf Difference Plots
; Authors: Ufuk Turuncoglu, Daniel Rosen
; Email: turuncu@ucar.edu,daniel.rosen@noaa.gov
; Last Modified: 2020-01-27
; Usage: ncl plot_ufs_phyf_diff.ncl 'c="<control_dir>"' 't="<test_dir>"'
;          ['v="<variables>"'] ['s="<time_steps>"']

begin
  ;--- parameters ---
  pwd = systemfunc("pwd")
  script = get_script_name()
  dblq = integertochar(34)
  usage = "ncl "+script
  usage = usage+" 'c="+dblq+"<control_dir>"+dblq+"'"
  usage = usage+" 't="+dblq+"<test_dir>"+dblq+"'"
  usage = usage+" ['v="+dblq+"<variables>"+dblq+"']"
  usage = usage+" ['s="+dblq+"<time_steps>"+dblq+"']"
  dflt_variables = (/ "tcdc_aveclm","tmp2m","tprcp" /)
  dflt_timesteps = (/ "024" /)
 
  ;--- command line arguments ---
  if (isvar("c")) then
    ctldir = c
  else
    print("### ERROR ###")
    print("  Control directory is not defined. [i.e. "+pwd+"]")
    print("  "+usage)
    exit()
  end if
  if (isvar("t")) then
    tstdir = t
  else
    print("### ERROR ###")
    print("  Test directory is not defined. [i.e. "+pwd+"]")
    print("  "+usage)
    exit()
  end if
  if (isvar("v")) then
    variables = str_split(v,",")
  else
    variables = dflt_variables
  end if
  if (isvar("s")) then
    timesteps = str_split(s,",")
  else
    timesteps = dflt_timesteps
  end if

  ;--- command line arguments ---
  print("### Usage ###")
  print("  "+usage)
  print("### Supported Variables ###")
  print("  "+dflt_variables(:))
  print("### Options ###")
  print("  Control Directory = "+ctldir)
  print("  Test Directory    = "+tstdir)
  print("  Variable(s)       = "+variables(:))
  print("  Time Step(s)      = "+timesteps(:))
  ;--- datasets ---
  dsets = (/ (/ "UFS-WEATHER-CTRL", ctldir+"" /), \
             (/ "UFS-WEATHER-TEST", tstdir+"" /) /)
  ndsets = dimsizes(dsets(:,0))
  nts = dimsizes(timesteps)
  nvars = dimsizes(variables)

  ;--- loop over files and process data ---
  do k = 0, nts-1
  do j = 0, nvars-1
  do i = 0, ndsets-1
    ifile = dsets(i,1)+"/phyf"+timesteps(k)+".nc"
    print("Processing "+dsets(i,0)+" "+ifile)

    ;--- read file ---
    nc = addfile(ifile(0), "r")

    ;--- create temporary variable ---
    if (i .eq. 0) then 
      dimsx = dimsizes(nc->grid_xt)
      dimsy = dimsizes(nc->grid_yt)
      nx = dimsx(0)
      ny = dimsy(0)
      data = new((/ ndsets, ny, nx /), "float")
      units = new((/ ndsets /), "string")
      label = new((/ ndsets /), "string")
      min_data = new((/ ndsets+1 /), "float")
      max_data = new((/ ndsets+1 /), "float")
    end if    

    ;--- read data ---
    data(i,:,:) = (/ nc->$variables(j)$(0,:,:) /)
    units(i) = nc->$variables(j)$@units
    label(i) = nc->$variables(j)$@long_name

    min_data(i) = min(data(i,:,:))
    max_data(i) = max(data(i,:,:))
    print(variables(j)+": range="+min_data(i)+":"+max_data(i)+" in "+units(i))
  end do

  ;--- add dimensions to temporary variables ---
  data!0 = "dsets"
  data&dsets = dsets(:,0)
  data!1 = "lat"
  data!2 = "lon"
  rad2deg = 180.0/get_pi("f")
  lat1d = nc->lat(:,0)
  data&lat = lat1d
  lon1d = nc->lon(0,:)
  data&lon = lon1d

  ;--- plot ---
  wks = gsn_open_wks("png", "plot_ufs_phyf_diff_"+variables(j))  

  plot = new(2,graphic)
  res = True
  res@gsnDraw  = False 
  res@gsnFrame = False
  res@cnInfoLabelOn = False
  res@cnFillOn = True
  res@mpFillOn = False 
  res@cnMissingValFillColor = "gray70"
  res@mpOutlineOn = True ;False
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
  res@gsnAddCyclic = True
  res@gsnLeftString = ""
  res@gsnRightString = ""
  res@gsnCenterString = ""
  res@gsnStringFontHeightF = .03
  res@trGridType = "TriangularMesh"
  res@cnFillMode = "RasterFill"
  res@cnLevelSelectionMode = "ExplicitLevels"
  if (variables(j) .eq. "tcdc_aveclm") then
    res@cnFillPalette = "WhiteBlueGreenYellowRed"
    res@cnLevels = fspan(0.000,100.0,21)
  else if (variables(j) .eq. "tmp2m") then
    res@cnFillPalette = "BlAqGrYeOrReVi200"
    res@cnLevels = fspan(170.0,350.0,21)
  else if (variables(j) .eq. "tprcp") then
    res@cnFillPalette = "WhBlGrYeRe"
    res@cnLevels = fspan(0.0001,0.0021,21)
  end if
  end if
  end if

  ;--- plot control and test ---
  res@gsnCenterString = "Control"
  plot(0) = gsn_csm_contour_map(wks, data(0,:,:), res)
  res@gsnCenterString = "Test"
  plot(1) = gsn_csm_contour_map(wks, data(1,:,:), res)

  ;--- panel first two plots ---
  pres1                     = True
  pres1@gsnPanelMainString = "Values for "+label(0)+"@"+timesteps(k)+"h in "+units(0)
  pres1@gsnPanelLabelBar    = True
  pres1@gsnFrame            = False
  pres1@lbOrientation       = "horizontal"
  pres1@gsnPanelBottom      = 0.4
  gsn_panel(wks,plot,(/2,1/),pres1)

  ;--- calculate difference ---
  diff = data(1,:,:)
  diff = (data(1,:,:)-data(0,:,:))
  min_data(ndsets) = min(diff(:,:))
  max_data(ndsets) = max(diff(:,:))
  print(variables(j)+": diff_range="+min_data(ndsets)+":"+max_data(ndsets)+" in "+units(0))

  ;--- plot difference ---
  delete(res@cnLevels)
  res@cnFillPalette = "NCV_blu_red"
  res@gsnCenterString = "Difference"
  if (variables(j) .eq. "tcdc_aveclm") then
    res@cnLevels = fspan(-100.000,100.0,21)
  else if (variables(j) .eq. "tmp2m") then
    res@cnLevels = fspan(-20.0,20.0,21)
  else if (variables(j) .eq. "tprcp") then
    res@cnLevels = fspan(-0.001,0.001,21)
  else
  end if
  end if
  end if
  plot2 = gsn_csm_contour_map(wks, diff, res)

  ;--- panel the pdiff plot ---
  pres2                     = True
  pres2@gsnPanelLabelBar    = True
  pres2@gsnPanelTop         = 0.4
  pres2@gsnPanelBottom      = 0.1
  pres2@gsnFrame            = False
  pres2@lbOrientation       = "horizontal"
  gsn_panel(wks,plot2,(/1,1/),pres2)

  frame(wks)

  end do ; nvars
  end do ; nts

end
