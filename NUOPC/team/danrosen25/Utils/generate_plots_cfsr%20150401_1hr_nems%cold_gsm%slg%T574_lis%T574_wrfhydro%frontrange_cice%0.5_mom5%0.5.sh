#!/bin/bash

SHADE="shade_output.jnl"
PLOT="plot_output.jnl"
VARIABLES="variables.dat"

MED_SLICE=4
MED_ATM_LON="array_med_atm_grid_coord1.nc"
MED_ATM_LAT="array_med_atm_grid_coord2.nc"
MED_LND_LON="array_med_lnd_grid_coord1.nc"
MED_LND_LAT="array_med_lnd_grid_coord2.nc"
MED_HYD_LON="array_med_hyd_grid_coord1.nc"
MED_HYD_LAT="array_med_hyd_grid_coord2.nc"
MED_LWDOWN="inst_down_lw_flx"
MED_SWDOWN="inst_down_sw_flx"
MED_WIND_N="inst_merid_wind_height_lowest"
MED_SFCPRS="inst_pres_height_surface"
MED_SPCHUM="inst_spec_humid_height_lowest"
MED_SFCTMP="inst_temp_height_lowest"
MED_WIND_E="inst_zonal_wind_height_lowest"
MED_PRECRT="mean_prec_rate"
MED_SLMOIS="moisture_content_of_soil_layer"
MED_SLTEMP="temperature_of_soil_layer"
MED_SLLQMS="liquid_water_content_of_soil_layer"
MED_SBSFRN="subsurface_runoff_flux"
MED_SURFRN="surface_runoff_flux"

LIS_DATE="201504010100"
LIS_DOM="d01"
LIS_INDIR="LIS_INPUTS"
LIS_OUTDIR="LIS_OUTPUT/SURFACEMODEL"
LIS_FILE="LIS_HIST_$LIS_DATE.$LIS_DOM.nc"
LIS_LON="lis_input.t574r.nc"
LIS_LAT="lis_input.t574r.nc"

WRFHYDRO_RSRTDATE="2015-04-01_01:00"
WRFHYDRO_CHDATE="201504010100"
WRFHYDRO_DOM="1"
WRFHYDRO_DOMDIR="WRFHYDRO_DOMAIN"
WRFHYDRO_RSRTFILE="HYDRO_RST.${WRFHYDRO_RSRTDATE}_DOMAIN$WRFHYDRO_DOM"
WRFHYDRO_CHRTFILE="$WRFHYDRO_CHDATE.CHRTOUT_GRID$WRFHYDRO_DOM"
WRFHYDRO_LSMLON="wrfinput_d03"
WRFHYDRO_LSMLAT="wrfinput_d03"
WRFHYDRO_RTLON="Fulldom_hires_hydrofile_ohd_new_basns_w_cal_params_full_domain.nc"
WRFHYDRO_RTLAT="Fulldom_hires_hydrofile_ohd_new_basns_w_cal_params_full_domain.nc"

ln -sf $LIS_INDIR/$LIS_LON .
ln -sf $LIS_INDIR/$LIS_LAT .
ln -sf $LIS_OUTDIR/$LIS_FILE .
ln -sf $WRFHYDRO_DOMDIR/$WRFHYDRO_LSMLON
ln -sf $WRFHYDRO_DOMDIR/$WRFHYDRO_LSMLAT
ln -sf $WRFHYDRO_DOMDIR/$WRFHYDRO_RTLON
ln -sf $WRFHYDRO_DOMDIR/$WRFHYDRO_RTLAT

function shade {
 if [ "$#" -lt 5 ]
 then
   echo "ERROR USAGE: $0 <FILE> <LON> <LAT> <FIELD> <SLICE/LEVEL>"
   exit 1
 fi

 FILE="$1"
 LON="$2"
 LAT="$3"
 FIELD="$4"
 LEVEL="$5"
 ferret -gif -script $SHADE $VARIABLES $FILE $LON $LAT $FIELD $LEVEL
}

function plot {
 if [ "$#" -lt 3 ]
 then
   echo "ERROR USAGE: $0 <FILE> <FIELD> <SLICE/LEVEL>"
   exit 1
 fi

 FILE="$1"
 FIELD="$2"
 LEVEL="$3"
 ferret -gif -script $PLOT $VARIABLES $FILE $FIELD $LEVEL
}

# FBAtm_a
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_LWDOWN" 1
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SWDOWN" 1
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_WIND_N" 1
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SFCPRS" 1
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SPCHUM" 1
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SFCTMP" 1
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_WIND_E" 1
shade "mediator_FBAtm_a_restart.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_PRECRT" 1
# FBLnd_l
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_1" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_2" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_3" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_4" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_1" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_2" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_3" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_4" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "$MED_SBSFRN" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "$MED_SURFRN" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_1" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_2" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_3" 1
shade "mediator_FBLnd_l_restart.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_4" 1
# LIS HIST Output
shade $LIS_FILE $LIS_LON $LIS_LAT "Qs_acc" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "Qsb_acc" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilMoist_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilMoist_tavg" 2
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilMoist_tavg" 3
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilMoist_tavg" 4
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilTemp_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilTemp_tavg" 2
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilTemp_tavg" 3
shade $LIS_FILE $LIS_LON $LIS_LAT "SoilTemp_tavg" 4
shade $LIS_FILE $LIS_LON $LIS_LAT "SmLiqFrac_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "SmLiqFrac_tavg" 2
shade $LIS_FILE $LIS_LON $LIS_LAT "SmLiqFrac_tavg" 3
shade $LIS_FILE $LIS_LON $LIS_LAT "SmLiqFrac_tavg" 4
shade $LIS_FILE $LIS_LON $LIS_LAT "Rainf_f_inst" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "Tair_f_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "Qair_f_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "Psurf_f_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "SWdown_f_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "LWdown_f_tavg" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "NWind_f_inst" 1
shade $LIS_FILE $LIS_LON $LIS_LAT "EWind_f_inst" 1
# Shade WRFHYDRO Restart Output
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "smc1" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "smc2" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "smc3" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "smc4" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "stc1" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "stc2" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "stc3" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "stc4" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "sh2ox1" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "sh2ox2" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "sh2ox3" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "sh2ox4" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "smcmax1" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "smcref1" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "smcwlt1" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "infxsrt" 1
shade $WRFHYDRO_RSRTFILE $WRFHYDRO_LSMLON $WRFHYDRO_LSMLAT "soldrain" 1
# Plot WRFHYDRO Restart Output
plot $WRFHYDRO_RSRTFILE "qlink1" 1
plot $WRFHYDRO_RSRTFILE "hlink" 1
plot $WRFHYDRO_RSRTFILE "z_gwsubbas" 1
# Shade WRFHYDRO Streamflow
shade $WRFHYDRO_CHRTFILE $WRFHYDRO_RTLON $WRFHYDRO_RTLAT "streamflow" 1

# From ATM to NEMS Mediator
shade "field_med_from_atm_$MED_LWDOWN.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_LWDOWN" $MED_SLICE
shade "field_med_from_atm_$MED_SWDOWN.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SWDOWN" $MED_SLICE
shade "field_med_from_atm_$MED_WIND_N.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_WIND_N" $MED_SLICE
shade "field_med_from_atm_$MED_SFCPRS.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SFCPRS" $MED_SLICE
shade "field_med_from_atm_$MED_SPCHUM.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SPCHUM" $MED_SLICE
shade "field_med_from_atm_$MED_SFCTMP.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_SFCTMP" $MED_SLICE
shade "field_med_from_atm_$MED_WIND_E.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_WIND_E" $MED_SLICE
shade "field_med_from_atm_$MED_PRECRT.nc" $MED_ATM_LON $MED_ATM_LAT "$MED_PRECRT" $MED_SLICE
# From NEMS Mediator to LND
shade "field_med_to_lnd_$MED_LWDOWN.nc" $MED_LND_LON $MED_LND_LAT "$MED_LWDOWN" $MED_SLICE
shade "field_med_to_lnd_$MED_SWDOWN.nc" $MED_LND_LON $MED_LND_LAT "$MED_SWDOWN" $MED_SLICE
shade "field_med_to_lnd_$MED_WIND_N.nc" $MED_LND_LON $MED_LND_LAT "$MED_WIND_N" $MED_SLICE
shade "field_med_to_lnd_$MED_SFCPRS.nc" $MED_LND_LON $MED_LND_LAT "$MED_SFCPRS" $MED_SLICE
shade "field_med_to_lnd_$MED_SPCHUM.nc" $MED_LND_LON $MED_LND_LAT "$MED_SPCHUM" $MED_SLICE
shade "field_med_to_lnd_$MED_SFCTMP.nc" $MED_LND_LON $MED_LND_LAT "$MED_SFCTMP" $MED_SLICE
shade "field_med_to_lnd_$MED_WIND_E.nc" $MED_LND_LON $MED_LND_LAT "$MED_WIND_E" $MED_SLICE
shade "field_med_to_lnd_$MED_PRECRT.nc" $MED_LND_LON $MED_LND_LAT "$MED_PRECRT" $MED_SLICE
# From LND to NEMS Mediator
shade "field_med_from_lnd_${MED_SLLQMS}_1.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_1" $MED_SLICE
shade "field_med_from_lnd_${MED_SLLQMS}_2.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_2" $MED_SLICE
shade "field_med_from_lnd_${MED_SLLQMS}_3.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_3" $MED_SLICE
shade "field_med_from_lnd_${MED_SLLQMS}_4.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLLQMS}_4" $MED_SLICE
shade "field_med_from_lnd_${MED_SLMOIS}_1.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_1" $MED_SLICE
shade "field_med_from_lnd_${MED_SLMOIS}_2.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_2" $MED_SLICE
shade "field_med_from_lnd_${MED_SLMOIS}_3.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_3" $MED_SLICE
shade "field_med_from_lnd_${MED_SLMOIS}_4.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLMOIS}_4" $MED_SLICE
shade "field_med_from_lnd_$MED_SBSFRN.nc"     $MED_LND_LON $MED_LND_LAT "$MED_SBSFRN" $MED_SLICE
shade "field_med_from_lnd_$MED_SURFRN.nc"     $MED_LND_LON $MED_LND_LAT "$MED_SURFRN" $MED_SLICE
shade "field_med_from_lnd_${MED_SLTEMP}_1.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_1" $MED_SLICE
shade "field_med_from_lnd_${MED_SLTEMP}_2.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_2" $MED_SLICE
shade "field_med_from_lnd_${MED_SLTEMP}_3.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_3" $MED_SLICE
shade "field_med_from_lnd_${MED_SLTEMP}_4.nc" $MED_LND_LON $MED_LND_LAT "${MED_SLTEMP}_4" $MED_SLICE
# From NEMS Mediator to HYD
shade "field_med_to_hyd_${MED_SLLQMS}_1.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLLQMS}_1" $MED_SLICE
shade "field_med_to_hyd_${MED_SLLQMS}_2.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLLQMS}_2" $MED_SLICE
shade "field_med_to_hyd_${MED_SLLQMS}_3.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLLQMS}_3" $MED_SLICE
shade "field_med_to_hyd_${MED_SLLQMS}_4.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLLQMS}_4" $MED_SLICE
shade "field_med_to_hyd_${MED_SLMOIS}_1.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLMOIS}_1" $MED_SLICE
shade "field_med_to_hyd_${MED_SLMOIS}_2.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLMOIS}_2" $MED_SLICE
shade "field_med_to_hyd_${MED_SLMOIS}_3.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLMOIS}_3" $MED_SLICE
shade "field_med_to_hyd_${MED_SLMOIS}_4.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLMOIS}_4" $MED_SLICE
shade "field_med_to_hyd_$MED_SBSFRN.nc"       $MED_HYD_LON $MED_HYD_LAT "$MED_SBSFRN" $MED_SLICE
shade "field_med_to_hyd_$MED_SURFRN.nc"       $MED_HYD_LON $MED_HYD_LAT "$MED_SURFRN" $MED_SLICE
shade "field_med_to_hyd_${MED_SLTEMP}_1.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLTEMP}_1" $MED_SLICE
shade "field_med_to_hyd_${MED_SLTEMP}_2.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLTEMP}_2" $MED_SLICE
shade "field_med_to_hyd_${MED_SLTEMP}_3.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLTEMP}_3" $MED_SLICE
shade "field_med_to_hyd_${MED_SLTEMP}_4.nc"   $MED_HYD_LON $MED_HYD_LAT "${MED_SLTEMP}_4" $MED_SLICE

# Create compressed tar archive of all .gif files
echo "*** Adding to plots.tar.gz ***"
tar -cf plots_NEMS_5day.tar *.gif

