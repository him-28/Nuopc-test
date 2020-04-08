; FV3 phyf Animated Plots
; Authors: Ufuk Turuncoglu, Daniel Rosen
; Email: turuncu@ucar.edu,daniel.rosen@noaa.gov
; Last Modified: 2020-02-03
; Usage: ncl anim_ufs_phyf.ncl ['d="<dir>"']
;          ['v="<variables>"'] ['s="<time_steps>"']
;          ['a="<True/False>"'] ['g="<True/False>"']
;          ['r="+dblq+"<minLat,maxLat,minLon,maxLon>"]

begin
  ;--- parameters ---
  pwd = systemfunc("pwd")
  script = get_script_name()
  dblq = integertochar(34)
  usage = "ncl "+script
  usage = usage+" ['d="+dblq+"<dir>"+dblq+"']"
  usage = usage+" ['v="+dblq+"<variables>"+dblq+"']"
  usage = usage+" ['s="+dblq+"<time_steps>"+dblq+"']"
  usage = usage+" ['a="+dblq+"<True/False>"+dblq+"']"
  usage = usage+" ['g="+dblq+"<True/False>"+dblq+"']"
  usage = usage+" ['r="+dblq+"<minLat,maxLat,minLon,maxLon>"+dblq+"']"
;  dflt_variables = (/ "tcdc_aveclm","tmp2m","tprcp" /)
  dflt_variables = (/ "spd10max" /)
;  dflt_timesteps = (/ "006","012","018","024","030","036","042","048", \
;                      "054","060","066","072","078","084","090","096", \
;                      "102","108","114","120","126" /)
  dflt_timesteps = (/ "000","003","006" /) 

  dflt_gbl = False
  dflt_anim = True
 
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

  ;--- command line arguments ---
  print("### Usage ###")
  print("  "+usage)
  print("### Pre-Configured Variables ###")
  print("  "+dflt_variables(:))
  print("### Options ###")
  print("  Run Directory     = "+rundir)
  print("  Variable(s)       = "+variables(:))
  print("  Time Step(s)      = "+timesteps(:))
  print("  Animation         = "+anim)
  print("  Global Map        = "+gblmap)
  if (mpZoom) then
    print("  Map LatLon        = ("+ \
      mpRegion(0)+":"+mpRegion(1)+","+ \
      mpRegion(2)+":"+mpRegion(3)+")")
  end if
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
    wks = gsn_open_wks("png", "plot_ufs_phyf_"+variables(j)+"_"+timesteps(k))

    plot = new(1,graphic)
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
    else if (variables(j) .eq. "spd10max") then
      res@cnLevelSelectionMode = "ExplicitLevels"
      res@cnFillPalette = "WhBlGrYeRe"
      res@cnLevels = fspan(5,25,21)
    else
      res@cnLevelSelectionMode = "AutomaticLevels"
      res@cnFillPalette = "rainbow"
      res@cnLevels = fspan(-100,100,21)
    end if
    end if
    end if
    end if

    ;--- plot control and test ---
    res@gsnCenterString = ""
    if (gblmap .or. mpZoom) then
      plot(0) = gsn_csm_contour_map(wks, data(:,:), res)
    else
      plot(0) = gsn_csm_contour(wks, data(:,:), res)
    end if

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

  if (anim .and. (nts .gt. 1)) then
    do j = 0, nvars-1
      gname = "plot_ufs_phyf_"+variables(j)+"_*.png"
      aname = "anim_ufs_phyf_"+variables(j)+".gif"
      print("Animating "+aname)
      stat = systemfunc("convert -delay 50 "+gname+" "+aname)
      stat = systemfunc("convert "+aname+" -coalesce -repage 0x0 -crop 768x620+128+0 +repage "+aname)
    end do ; nvars
  else if(anim .and. (nts .le. 1)) then
    print("Skipping animation: time step count less than 2.")
  end if
  end if

end
