; FV3 phyf Plots
; Authors: Ufuk Turuncoglu, Daniel Rosen
; Email: turuncu@ucar.edu,daniel.rosen@noaa.gov
; Last Modified: 2020-01-27
; Usage: ncl plot_ufs_phyf.ncl ['d="<dir>"']
;          ['v="<variables>"'] ['s="<time_steps>"']

begin
  ;--- parameters ---
  pwd = systemfunc("pwd")
  script = get_script_name()
  dblq = integertochar(34)
  usage = "ncl "+script
  usage = usage+" ['d="+dblq+"<dir>"+dblq+"']"
  usage = usage+" ['v="+dblq+"<variables>"+dblq+"']"
  usage = usage+" ['s="+dblq+"<time_steps>"+dblq+"']"
  dflt_variables = (/ "tmp2m","tprcp" /)
  dflt_timesteps = (/ "024" /)
 
  ;--- command line arguments ---
  if (isvar("d")) then
    rundir = d
  else
    rundir = pwd
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
  print("### Pre-Configured Variables ###")
  print("  "+dflt_variables(:))
  print("### Options ###")
  print("  Run Directory     = "+rundir)
  print("  Variable(s)       = "+variables(:))
  print("  Time Step(s)      = "+timesteps(:))
  ;--- datasets ---
  dsets = (/ "UFS-WEATHER-MODEL", rundir+"" /)
  nts = dimsizes(timesteps)
  nvars = dimsizes(variables)

  ;--- loop over files and process data ---
  do k = 0, nts-1
  do j = 0, nvars-1

    ifile = dsets(1)+"/phyf"+timesteps(k)+".nc"
    print("Processing "+dsets(0)+" "+ifile)

    ;--- read file ---
    nc = addfile(ifile, "r")

    ;--- create temporary variable ---
    dimsx = dimsizes(nc->grid_xt)
    dimsy = dimsizes(nc->grid_yt)
    nx = dimsx(0)
    ny = dimsy(0)
    data = new((/ ny, nx /), "float")

    ;--- read data ---
    data(:,:) = (/ nc->$variables(j)$(0,:,:) /)
    units = nc->$variables(j)$@units
    label = nc->$variables(j)$@long_name

    min_data = min(data(:,:))
    max_data = max(data(:,:))
    print(variables(j)+": range="+min_data+":"+max_data+" in "+units)

  ;--- add dimensions to temporary variables ---
  data!0 = "lat"
  data!1 = "lon"
  lat1d = nc->lat(:,0)
  data&lat = lat1d
  lon1d = nc->lon(0,:)
  data&lon = lon1d

  ;--- plot ---
  wks = gsn_open_wks("png", "plot_ufs_phyf_"+variables(j))  

  plot = new(1,graphic)
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
  if (variables(j) .eq. "tcdc_aveclm") then
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnFillPalette = "WhiteBlueGreenYellowRed"
    res@cnLevels = fspan(0.000,100.0,21)
  else if (variables(j) .eq. "tmp2m") then
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnFillPalette = "NCV_jet"
    res@cnLevels = fspan(215.0,315.0,51)
  else if (variables(j) .eq. "tprcp") then
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnFillPalette = "WhBlGrYeRe"
    res@cnLevels = fspan(0.0001,0.0031,21)
  else
    res@cnLevelSelectionMode = "AutomaticLevels"
    res@cnFillPalette = "rainbow"
    res@cnLevels = fspan(-100,100,21)
  end if
  end if
  end if

  ;--- plot control and test ---
  res@gsnCenterString = ""
  plot(0) = gsn_csm_contour_map(wks, data(:,:), res)

  ;--- add plot to panel ---
  pres1                     = True
  pres1@gsnPanelMainString  = str_upper(label(0))+" ("+units(0)+") @"+timesteps(k)+"h"
  pres1@gsnPanelMainFont    = "helvetica-bold"
  pres1@gsnPanelLabelBar    = True
  pres1@gsnFrame            = False
  pres1@lbOrientation       = "horizontal"
  pres1@gsnPanelBottom      = 0.4
  gsn_panel(wks,plot,(/1,1/),pres1)
  delete(res@cnLevels)

  frame(wks)

  end do ; nvars
  end do ; nts

end
