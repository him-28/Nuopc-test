#!/bin/bash

SCRIPT=shade_NEMS.jnl

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

 if [ "$#" -lt 4 ]
 then
   echo "ERROR USAGE: $0 <FILE> <LON> <LAT> <FIELD>"
   exit 1
 fi

 FILE="$1"
 LON="$2"
 LAT="$3"
 FIELD="$4"
 ferret -gif -script $SCRIPT $VARIABLES $FILE $LON $LAT $FIELD $SLICE

}

generate_plot "field_med_from_atm_inst_down_lw_flx.nc"              $ATM_LON $ATM_LAT "inst_down_lw_flx"
generate_plot "field_med_from_atm_inst_down_sw_flx.nc"              $ATM_LON $ATM_LAT "inst_down_sw_flx"
generate_plot "field_med_from_atm_inst_merid_wind_height_lowest.nc" $ATM_LON $ATM_LAT "inst_merid_wind_height_lowest"
generate_plot "field_med_from_atm_inst_pres_height_surface.nc"      $ATM_LON $ATM_LAT "inst_pres_height_surface"
generate_plot "field_med_from_atm_inst_spec_humid_height_lowest.nc" $ATM_LON $ATM_LAT "inst_spec_humid_height_lowest"
generate_plot "field_med_from_atm_inst_temp_height_lowest.nc"       $ATM_LON $ATM_LAT "inst_temp_height_lowest"
generate_plot "field_med_from_atm_inst_zonal_wind_height_lowest.nc" $ATM_LON $ATM_LAT "inst_zonal_wind_height_lowest"
generate_plot "field_med_from_atm_mean_prec_rate.nc"                $ATM_LON $ATM_LAT "mean_prec_rate"

generate_plot "field_med_to_lnd_inst_down_lw_flx.nc"              $LND_LON $LND_LAT "inst_down_lw_flx"
generate_plot "field_med_to_lnd_inst_down_sw_flx.nc"              $LND_LON $LND_LAT "inst_down_sw_flx"
generate_plot "field_med_to_lnd_inst_merid_wind_height_lowest.nc" $LND_LON $LND_LAT "inst_merid_wind_height_lowest"
generate_plot "field_med_to_lnd_inst_pres_height_surface.nc"      $LND_LON $LND_LAT "inst_pres_height_surface"
generate_plot "field_med_to_lnd_inst_spec_humid_height_lowest.nc" $LND_LON $LND_LAT "inst_spec_humid_height_lowest"
generate_plot "field_med_to_lnd_inst_temp_height_lowest.nc"       $LND_LON $LND_LAT "inst_temp_height_lowest"
generate_plot "field_med_to_lnd_inst_zonal_wind_height_lowest.nc" $LND_LON $LND_LAT "inst_zonal_wind_height_lowest"
generate_plot "field_med_to_lnd_mean_prec_rate.nc"                $LND_LON $LND_LAT "mean_prec_rate"

generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_1.nc" $LND_LON $LND_LAT "liquid_water_content_of_soil_layer_1"
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_2.nc" $LND_LON $LND_LAT "liquid_water_content_of_soil_layer_2"
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_3.nc" $LND_LON $LND_LAT "liquid_water_content_of_soil_layer_3"
generate_plot "field_med_from_lnd_liquid_water_content_of_soil_layer_4.nc" $LND_LON $LND_LAT "liquid_water_content_of_soil_layer_4"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_1.nc"     $LND_LON $LND_LAT "moisture_content_of_soil_layer_1"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_2.nc"     $LND_LON $LND_LAT "moisture_content_of_soil_layer_2"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_3.nc"     $LND_LON $LND_LAT "moisture_content_of_soil_layer_3"
generate_plot "field_med_from_lnd_moisture_content_of_soil_layer_4.nc"     $LND_LON $LND_LAT "moisture_content_of_soil_layer_4"
generate_plot "field_med_from_lnd_subsurface_runoff_flux.nc"               $LND_LON $LND_LAT "subsurface_runoff_flux"
generate_plot "field_med_from_lnd_surface_runoff_flux.nc"                  $LND_LON $LND_LAT "surface_runoff_flux"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_1.nc"          $LND_LON $LND_LAT "temperature_of_soil_layer_1"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_2.nc"          $LND_LON $LND_LAT "temperature_of_soil_layer_2"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_3.nc"          $LND_LON $LND_LAT "temperature_of_soil_layer_3"
generate_plot "field_med_from_lnd_temperature_of_soil_layer_4.nc"          $LND_LON $LND_LAT "temperature_of_soil_layer_4"

generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_1.nc" $HYD_LON $HYD_LAT "liquid_water_content_of_soil_layer_1"
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_2.nc" $HYD_LON $HYD_LAT "liquid_water_content_of_soil_layer_2"
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_3.nc" $HYD_LON $HYD_LAT "liquid_water_content_of_soil_layer_3"
generate_plot "field_med_to_hyd_liquid_water_content_of_soil_layer_4.nc" $HYD_LON $HYD_LAT "liquid_water_content_of_soil_layer_4"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_1.nc"     $HYD_LON $HYD_LAT "moisture_content_of_soil_layer_1"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_2.nc"     $HYD_LON $HYD_LAT "moisture_content_of_soil_layer_2"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_3.nc"     $HYD_LON $HYD_LAT "moisture_content_of_soil_layer_3"
generate_plot "field_med_to_hyd_moisture_content_of_soil_layer_4.nc"     $HYD_LON $HYD_LAT "moisture_content_of_soil_layer_4"
generate_plot "field_med_to_hyd_subsurface_runoff_flux.nc"               $HYD_LON $HYD_LAT "subsurface_runoff_flux"
generate_plot "field_med_to_hyd_surface_runoff_flux.nc"                  $HYD_LON $HYD_LAT "surface_runoff_flux"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_1.nc"          $HYD_LON $HYD_LAT "temperature_of_soil_layer_1"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_2.nc"          $HYD_LON $HYD_LAT "temperature_of_soil_layer_2"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_3.nc"          $HYD_LON $HYD_LAT "temperature_of_soil_layer_3"
generate_plot "field_med_to_hyd_temperature_of_soil_layer_4.nc"          $HYD_LON $HYD_LAT "temperature_of_soil_layer_4"

# Create compressed tar archive of all .gif files
echo "*** Adding to plots.tar.gz ***"
tar -cf plots_MED.tar *MED_FROM_ATM*.gif
tar -rf plots_MED.tar *MED_FROM_LND*.gif
tar -rf plots_MED.tar *MED_TO_LND*.gif
tar -rf plots_MED.tar *MED_TO_HYD*.gif

# echo "*** Removing MED_FROM_ATM MED_FROM_LND MED_TO_LND MED_TO_HYD ***"
# rm -f *MED_FROM_ATM*.gif *MED_FROM_LND*.gif *MED_TO_LND*.gif *MED_TO_HYD*.gif
