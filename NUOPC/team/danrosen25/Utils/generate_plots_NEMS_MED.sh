#!/bin/bash

SCRIPT=ferret_NEMS_MED_shade.jnl

ATM_LON=array_med_atm_grid_coord1.nc
ATM_LAT=array_med_atm_grid_coord2.nc
LND_LON=array_med_lnd_grid_coord1.nc
LND_LAT=array_med_lnd_grid_coord2.nc
HYD_LON=array_med_hyd_grid_coord1.nc
HYD_LAT=array_med_hyd_grid_coord2.nc

SLICE="4"
VARIABLES="variables.dat"

# convert to single image function
function generate_plot {

if [ "$#" -lt 3 ]
then
  echo "ERROR USAGE: $0 <FILE> <LON> <LAT>"
  exit 1
fi

FILE="$1"
LON="$2"
LAT="$3"
ferret -gif -script $SCRIPT $VARIABLES $FILE $LON $LAT $SLICE

}

generate_plot "field_med_from_atm_inst_down_lw_flx.nc"              $ATM_LON $ATM_LAT
generate_plot "field_med_from_atm_inst_down_sw_flx.nc"              $ATM_LON $ATM_LAT
generate_plot "field_med_from_atm_inst_merid_wind_height_lowest.nc" $ATM_LON $ATM_LAT
generate_plot "field_med_from_atm_inst_pres_height_surface.nc"      $ATM_LON $ATM_LAT
generate_plot "field_med_from_atm_inst_spec_humid_height_lowest.nc" $ATM_LON $ATM_LAT
generate_plot "field_med_from_atm_inst_temp_height_lowest.nc"       $ATM_LON $ATM_LAT
generate_plot "field_med_from_atm_inst_zonal_wind_height_lowest.nc" $ATM_LON $ATM_LAT
generate_plot "field_med_from_atm_mean_prec_rate.nc"                $ATM_LON $ATM_LAT

generate_plot "field_med_to_lnd_inst_down_lw_flx.nc"              $LND_LON $LND_LAT
generate_plot "field_med_to_lnd_inst_down_sw_flx.nc"              $LND_LON $LND_LAT
generate_plot "field_med_to_lnd_inst_merid_wind_height_lowest.nc" $LND_LON $LND_LAT
generate_plot "field_med_to_lnd_inst_pres_height_surface.nc"      $LND_LON $LND_LAT
generate_plot "field_med_to_lnd_inst_spec_humid_height_lowest.nc" $LND_LON $LND_LAT
generate_plot "field_med_to_lnd_inst_temp_height_lowest.nc"       $LND_LON $LND_LAT
generate_plot "field_med_to_lnd_inst_zonal_wind_height_lowest.nc" $LND_LON $LND_LAT
generate_plot "field_med_to_lnd_mean_prec_rate.nc"                $LND_LON $LND_LAT

generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_1.nc" $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_2.nc" $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_3.nc" $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_4.nc" $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_1.nc"     $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_2.nc"     $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_3.nc"     $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_4.nc"     $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_subsurface_runoff_flux.nc"               $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_surface_runoff_flux.nc"                  $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_temperature_of_soil_layer_1.nc"          $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_temperature_of_soil_layer_2.nc"          $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_temperature_of_soil_layer_3.nc"          $LND_LON $LND_LAT
generate_plot "field_med_from_lnd_temperature_of_soil_layer_4.nc"          $LND_LON $LND_LAT

generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_1.nc" $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_2.nc" $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_3.nc" $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_4.nc" $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_1.nc"     $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_2.nc"     $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_3.nc"     $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_4.nc"     $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_subsurface_runoff_flux.nc"               $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_surface_runoff_flux.nc"                  $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_temperature_of_soil_layer_1.nc"          $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_temperature_of_soil_layer_2.nc"          $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_temperature_of_soil_layer_3.nc"          $HYD_LON $HYD_LAT
generate_plot "field_med_to_hyd_temperature_of_soil_layer_4.nc"          $HYD_LON $HYD_LAT

# Create compressed tar archive of all .gif files
echo "*** Adding to plots.tar.gz ***"
tar -czf plots.tar.gz *.gif
