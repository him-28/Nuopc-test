;--------------------------------------------------
; NUOPC Output Grid
;--------------------------------------------------
; Authors: Daniel Rosen
; Email: daniel.rosen@noaa.gov
; Last Modified: 2020-04-07
; Usage: ncl plot_NUOPC_coords.ncl 'grd="<grid_file>"'
;          ['dir="<dir>"']
;          ['rgn="<minLat,maxLat,minLon,maxLon>"]
;          ['cor="<True/False>"']
;          ['cen="<True/False>"']
;          ['msk="<True/False>"']
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  ;--- defaults ---
  outtype = ""
  title = "Plot"
  dflt_fillValue = 9.99e+20
  dflt_corners = False
  dflt_centers = True

  ;--- parameters ---
  pwd = systemfunc("pwd")
  script = get_script_name()
  dblq = integertochar(34)
  usage = "ncl "+script
  usage = usage+" 'grd="+dblq+"<grid_file>"+dblq+"'"
  usage = usage+" ['dir="+dblq+"<dir>"+dblq+"']"
  usage = usage+" ['cor="+dblq+"<True/False>"+dblq+"']"
  usage = usage+" ['cen="+dblq+"<True/False>"+dblq+"']"
  usage = usage+" ['rgn="+dblq+"<minLat,maxLat,minLon,maxLon>"+dblq+"']"

  ;--- command line arguments ---
  if (isvar("grd")) then
    grid_file = grd
  else
    print("### ERROR ###")
    print("  Grid file not defined. ['grd="+dblq+"<grid_file>"+dblq+"']")
    print("  "+usage)
    exit()
  end if
  if (isvar("dir")) then
    rundir = dir
  else
    rundir = pwd
  end if
  if (isvar("cor")) then
    corners = (str_upper(cor) .eq. "TRUE") 
  else
    corners = dflt_corners
  end if
  if (corners) then
    outtype = outtype+"_CORNERS"
    title = title+" Corners"
  end if
  if (isvar("cen")) then
    centers = (str_upper(cen) .eq. "TRUE")
  else
    centers = dflt_centers
  end if
  if (centers) then
    outtype = outtype+"_CENTERS"
    title = title+" Centers"
  end if
  if (isvar("rgn")) then
    mpZoom = True
    if (str_upper(rgn) .eq. "CONUS") then
      mpRegion = (/ 18.0 , 49.0, -125.0, -62.5 /)
      outtype = outtype+"_CONUS"
      title = title+" Conus"
    else if (str_upper(rgn) .eq. "N.ATLANTIC") then
      mpRegion = (/ 10.0, 40.0, -90.0, -45.0 /)
      outtype = outtype+"_NATLANTIC"
      title = title+" N.Atlantic"
    else if (str_upper(rgn) .eq. "PUERTORICO") then
      mpRegion = (/ 17.5, 19.5, -67.5, -65.0 /)
      outtype = outtype+"_PUERTORICO"
      title = title+" Puerto Rico"
    else if (str_upper(rgn) .eq. "FLORIDA") then
      mpRegion = (/ 24.5, 30.5, -83.0, -79.5 /)
      outtype = outtype+"_FLORIDA"
      title = title+" Florida"
    else
      mpRegion = stringtofloat(str_split(rgn,","))
      outtype = outtype+"_REGIONAL"
      title = title+" Regional"
    end if
    end if
    end if
    end if
  else
    mpZoom = False
    mpRegion = (/ 0.0, 0.0, 0.0, 0.0 /)
    outtype = outtype+"_GLOBAL"
    title = title+" Global"
  end if
  if (isvar("msk")) then
    if (str_upper(msk) .eq. "TRUE") then
      maskPlot = True
      outtype = outtype+"_MASK"
      title = title+" Mask"
    else
      maskPlot = False
    end if
  else
    maskPlot = False
  end if

  ;--- print usage and settings ---
  input = rundir+"/"+grid_file
  file_suffix = get_file_suffix(grid_file,0)
  output = file_suffix@fBase+outtype
  print("### Usage ###")
  print("  "+usage)
  print ("Grid File="+input)
  print ("Output File="+output+".png")

  ;--- read netcdf file ---
  if (isfilepresent(input)) then
    in = addfile(input,"r")
  else
    print("### ERROR ###")
    print("  File is missing: "+input)
    exit()
  end if

  ;--- configure graphics ---
  wks = gsn_open_wks("png",output)

  res               = True
;  res@gsnMaximize   = True
;  res@gsnBoxMargin  = 0.0
  res@gsnDraw       = False
  res@gsnFrame      = False
  res@tiMainString  = title
  if (maskPlot) then
    if (mpZoom) then
      res@gsnAddCyclic = False
    end if
    res@cnFillOn      = True
    res@cnLinesOn     = False
    res@cnFillMode    = "CellFill"
;    res@cnFillPalette  = "cyclic"
    res@cnFillPalette  = "grads_rainbow"
    res@cnFixFillBleed = False
    res@cnLevelSelectionMode = "ExplicitLevels"
    res@cnLevels = (/0,1,2,3,4/)
    res@cnLabelScalingMode = "AllIntegers"
  end if

  if (mpZoom) then
    res@mpLimitMode   = "LatLon"
    res@mpMinLatF     = mpRegion(0)
    res@mpMaxLatF     = mpRegion(1)
    res@mpMinLonF     = mpRegion(2)
    res@mpMaxLonF     = mpRegion(3)
  end if

  ;--- load coordinates ---
  if (maskPlot) then
    mskval = in->mask(:,:)
    mskval@lat2d = in->lat_center(:,:)
    mskval@lon2d = in->lon_center(:,:)
    res@tfDoNDCOverlay = False
    plot = gsn_csm_contour_map(wks,mskval,res)
    if (corners) then
      corlc       = in->lon_corner(:,:)
      corlc@lat2d = in->lat_corner(:,:)
      corlc@lon2d = in->lon_corner(:,:)
      prescor = True
      prescor@gsnCoordsAsLines = True
      prescor@gsnCoordsLat     = corlc@lat2d
      prescor@gsnCoordsLon     = corlc@lon2d
      gsn_coordinates(wks,plot,corlc,prescor)
    else if (centers) then
      cenlc       = in->lon_center(:,:)
      cenlc@lat2d = in->lat_center(:,:)
      cenlc@lon2d = in->lon_center(:,:)
      prescen = True
      prescen@gsnCoordsAsLines = False
      prescen@gsMarkerIndex    = 1
;      prescen@gsMarkerSizeF    = 0.01
      prescen@gsnCoordsLat     = cenlc@lat2d
      prescen@gsnCoordsLon     = cenlc@lon2d
      gsn_coordinates(wks,plot,cenlc,prescen)
    else
      draw(plot)
      frame(wks)
    end if
    end if
  else
    plot = gsn_csm_map_ce(wks,res)
    if (corners) then
      corlc       = in->lon_corner(:,:)
      corlc@lat2d = in->lat_corner(:,:)
      corlc@lon2d = in->lon_corner(:,:)
      prescor = True
      prescor@gsnCoordsAsLines = True
      prescor@gsnCoordsLat     = corlc@lat2d
      prescor@gsnCoordsLon     = corlc@lon2d
      gsn_coordinates(wks,plot,corlc,prescor)
    else
      cenlc       = in->lon_center(:,:)
      cenlc@lat2d = in->lat_center(:,:)
      cenlc@lon2d = in->lon_center(:,:)
      prescen = True
      prescen@gsnCoordsAsLines = False
      prescen@gsMarkerIndex    = 1
;      prescen@gsMarkerSizeF    = 0.01
      prescen@gsnCoordsLat     = cenlc@lat2d
      prescen@gsnCoordsLon     = cenlc@lon2d
      gsn_coordinates(wks,plot,cenlc,prescen)
    end if
  end if

end
