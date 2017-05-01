#!/bin/bash

SCRIPT=ferret_NEMS_MED_shade.jnl

ATM_LON=array_med_atm_grid_coord1.nc
ATM_LAT=array_med_atm_grid_coord2.nc
LND_LON=array_med_lnd_grid_coord1.nc
LND_LAT=array_med_lnd_grid_coord2.nc
HYD_LON=array_med_hyd_grid_coord1.nc
HYD_LAT=array_med_hyd_grid_coord2.nc

SLICEK="K=4"
SLICEL="L=4"

# convert to single image function
function generate_plot {

if [ "$#" -lt 5 ]
then
  echo "ERROR USAGE: $0 <FILE> <LON> <LAT> <SLICE> <LEVELS>"
  exit 1
fi

FILE="$1"
LON="$2"
LAT="$3"
SLICE="$4"
LEVELS="$5"
ferret -gif -script $SCRIPT $FILE $LON $LAT $SLICE "$LEVELS"

}

generate_plot "field_med_from_atm_inst_down_lw_flx.nc"              $ATM_LON $ATM_LAT $SLICEL "(0),(50,500,10),(1000)"
generate_plot "field_med_from_atm_inst_down_sw_flx.nc"              $ATM_LON $ATM_LAT $SLICEL "(0),(0,1000,20),(2000)"
generate_plot "field_med_from_atm_inst_merid_wind_height_lowest.nc" $ATM_LON $ATM_LAT $SLICEL "(-100),(-30,30,1),(100)"
generate_plot "field_med_from_atm_inst_pres_height_surface.nc"      $ATM_LON $ATM_LAT $SLICEL "(40000),(45000,105000,1000),(110000)"
generate_plot "field_med_from_atm_inst_spec_humid_height_lowest.nc" $ATM_LON $ATM_LAT $SLICEL "(-0.01),(0.0,0.02,0.0005),(0.05)"
generate_plot "field_med_from_atm_inst_temp_height_lowest.nc"       $ATM_LON $ATM_LAT $SLICEL "(200),(210,310,1),(350)"
generate_plot "field_med_from_atm_inst_zonal_wind_height_lowest.nc" $ATM_LON $ATM_LAT $SLICEL "(-100),(-30,30,1),(100)"
generate_plot "field_med_from_atm_mean_prec_rate.nc"                $ATM_LON $ATM_LAT $SLICEL "(0.0),(0.0,0.0006,0.00001),(0.1)"

generate_plot "field_med_to_lnd_inst_down_lw_flx.nc"              $LND_LON $LND_LAT $SLICEL "(0),(50,500,10),(1000)"
generate_plot "field_med_to_lnd_inst_down_sw_flx.nc"              $LND_LON $LND_LAT $SLICEL "(0),(0,1000,20),(2000)"
generate_plot "field_med_to_lnd_inst_merid_wind_height_lowest.nc" $LND_LON $LND_LAT $SLICEL "(-100),(-30,30,1),(100)"
generate_plot "field_med_to_lnd_inst_pres_height_surface.nc"      $LND_LON $LND_LAT $SLICEL "(40000),(45000,105000,1000),(110000)"
generate_plot "field_med_to_lnd_inst_spec_humid_height_lowest.nc" $LND_LON $LND_LAT $SLICEL "(-0.01),(0.0,0.02,0.0005),(0.05)"
generate_plot "field_med_to_lnd_inst_temp_height_lowest.nc"       $LND_LON $LND_LAT $SLICEL "(200),(210,310,1),(350)"
generate_plot "field_med_to_lnd_inst_zonal_wind_height_lowest.nc" $LND_LON $LND_LAT $SLICEL "(-100),(-30,30,1),(100)"
generate_plot "field_med_to_lnd_mean_prec_rate.nc"                $LND_LON $LND_LAT $SLICEL "(0.0),(0.0,0.0006,0.00001),(0.1)"

generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_1.nc" $LND_LON $LND_LAT $SLICEL "(0),(0.10,0.30,0.004),(1)"
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_2.nc" $LND_LON $LND_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_3.nc" $LND_LON $LND_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_4.nc" $LND_LON $LND_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_1.nc"     $LND_LON $LND_LAT $SLICEL "(0),(0.10,0.30,0.004),(1)"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_2.nc"     $LND_LON $LND_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_3.nc"     $LND_LON $LND_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_4.nc"     $LND_LON $LND_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_from_lnd_subsurface_runoff_flux.nc"               $LND_LON $LND_LAT $SLICEL "(0),(0,0.000001,0.00000002),(0.01)"
generate_plot "field_med_from_lnd_surface_runoff_flux.nc"                  $LND_LON $LND_LAT $SLICEL "(0),(0,0.000001,0.00000002),(0.01)"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_1.nc"          $LND_LON $LND_LAT $SLICEK "(200),(280,300,0.4),(350)"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_2.nc"          $LND_LON $LND_LAT $SLICEK "(200),(285,295,0.2),(350)"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_3.nc"          $LND_LON $LND_LAT $SLICEK "(200),(285,295,0.2),(350)"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_4.nc"          $LND_LON $LND_LAT $SLICEK "(200),(285,295,0.2),(350)"

generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_1.nc" $HYD_LON $HYD_LAT $SLICEL "(0),(0.10,0.30,0.004),(1)"
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_2.nc" $HYD_LON $HYD_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_3.nc" $HYD_LON $HYD_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_4.nc" $HYD_LON $HYD_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_1.nc"     $HYD_LON $HYD_LAT $SLICEL "(0),(0.10,0.30,0.004),(1)"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_2.nc"     $HYD_LON $HYD_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_3.nc"     $HYD_LON $HYD_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_4.nc"     $HYD_LON $HYD_LAT $SLICEL "(0),(0.17,0.23,0.001),(1)"
generate_plot "field_med_to_hyd_subsurface_runoff_flux.nc"               $HYD_LON $HYD_LAT $SLICEL "(0),(0.0,0.000001,0.00000002),(0.01)"
generate_plot "field_med_to_hyd_surface_runoff_flux.nc"                  $HYD_LON $HYD_LAT $SLICEL "(0),(0.0,0.000001,0.00000002),(0.01)"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_1.nc"          $HYD_LON $HYD_LAT $SLICEK "(200),(280,300,0.4),(350)"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_2.nc"          $HYD_LON $HYD_LAT $SLICEK "(200),(285,295,0.2),(350)"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_3.nc"          $HYD_LON $HYD_LAT $SLICEK "(200),(285,295,0.2),(350)"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_4.nc"          $HYD_LON $HYD_LAT $SLICEK "(200),(285,295,0.2),(350)"

# Create compressed tar archive of all .gif files
echo "*** Adding to plots.tar.gz ***"
tar -czf plots.tar.gz *.gif
