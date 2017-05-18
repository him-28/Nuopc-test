#!/bin/bash

SCRIPT=shade_NEMS.jnl
VARIABLES="variables.dat"

MED_SLICE="4"
MED_ATM_LON=array_med_atm_grid_coord1.nc
MED_ATM_LAT=array_med_atm_grid_coord2.nc
MED_LND_LON=array_med_lnd_grid_coord1.nc
MED_LND_LAT=array_med_lnd_grid_coord2.nc
MED_HYD_LON=array_med_hyd_grid_coord1.nc
MED_HYD_LAT=array_med_hyd_grid_coord2.nc

LIS_DATE=201504010100
LIS_DOM=d01
LIS_INDIR=LIS_INPUTS
LIS_OUTDIR=LIS_OUTPUT/SURFACEMODEL
LIS_FILE=LIS_HIST_$LIS_DATE.$LIS_DOM.nc
LIS_LON=lis_input.t574r.nc
LIS_LAT=lis_input.t574r.nc

ln -sf $LIS_INDIR/$LIS_LON .
ln -sf $LIS_INDIR/$LIS_LAT .
ln -sf $LIS_OUTDIR/$LIS_FILE .

function plot_med {
 if [ "$#" -lt 4 ]
 then
   echo "ERROR USAGE: $0 <FILE> <LON> <LAT> <FIELD>"
   exit 1
 fi

 FILE="$1"
 LON="$2"
 LAT="$3"
 FIELD="$4"
 ferret -gif -script $SCRIPT $VARIABLES $FILE $LON $LAT $FIELD $MED_SLICE
}

function plot_lis {
 if [ "$#" -lt 2 ]
 then
   echo "ERROR USAGE: $0 <FIELD> <LEVEL>"
   exit 1
 fi

 FIELD="$1"
 LEVEL="$2"
 ferret -gif -script $SCRIPT $VARIABLES $LIS_FILE $LIS_LON $LIS_LAT $FIELD $LEVEL
}

# From ATM to NEMS Mediator
plot_med "field_med_from_atm_inst_down_lw_flx.nc"                     $MED_ATM_LON $MED_ATM_LAT "inst_down_lw_flx"
plot_med "field_med_from_atm_inst_down_sw_flx.nc"                     $MED_ATM_LON $MED_ATM_LAT "inst_down_sw_flx"
plot_med "field_med_from_atm_inst_merid_wind_height_lowest.nc"        $MED_ATM_LON $MED_ATM_LAT "inst_merid_wind_height_lowest"
plot_med "field_med_from_atm_inst_pres_height_surface.nc"             $MED_ATM_LON $MED_ATM_LAT "inst_pres_height_surface"
plot_med "field_med_from_atm_inst_spec_humid_height_lowest.nc"        $MED_ATM_LON $MED_ATM_LAT "inst_spec_humid_height_lowest"
plot_med "field_med_from_atm_inst_temp_height_lowest.nc"              $MED_ATM_LON $MED_ATM_LAT "inst_temp_height_lowest"
plot_med "field_med_from_atm_inst_zonal_wind_height_lowest.nc"        $MED_ATM_LON $MED_ATM_LAT "inst_zonal_wind_height_lowest"
plot_med "field_med_from_atm_mean_prec_rate.nc"                       $MED_ATM_LON $MED_ATM_LAT "mean_prec_rate"
# From NEMS Mediator to LND
plot_med "field_med_to_lnd_inst_down_lw_flx.nc"                       $MED_LND_LON $MED_LND_LAT "inst_down_lw_flx"
plot_med "field_med_to_lnd_inst_down_sw_flx.nc"                       $MED_LND_LON $MED_LND_LAT "inst_down_sw_flx"
plot_med "field_med_to_lnd_inst_merid_wind_height_lowest.nc"          $MED_LND_LON $MED_LND_LAT "inst_merid_wind_height_lowest"
plot_med "field_med_to_lnd_inst_pres_height_surface.nc"               $MED_LND_LON $MED_LND_LAT "inst_pres_height_surface"
plot_med "field_med_to_lnd_inst_spec_humid_height_lowest.nc"          $MED_LND_LON $MED_LND_LAT "inst_spec_humid_height_lowest"
plot_med "field_med_to_lnd_inst_temp_height_lowest.nc"                $MED_LND_LON $MED_LND_LAT "inst_temp_height_lowest"
plot_med "field_med_to_lnd_inst_zonal_wind_height_lowest.nc"          $MED_LND_LON $MED_LND_LAT "inst_zonal_wind_height_lowest"
plot_med "field_med_to_lnd_mean_prec_rate.nc"                         $MED_LND_LON $MED_LND_LAT "mean_prec_rate"
# From LND to NEMS Mediator
plot_med "field_med_from_lnd_liquid_water_content_of_soil_layer_1.nc" $MED_LND_LON $MED_LND_LAT "liquid_water_content_of_soil_layer_1"
plot_med "field_med_from_lnd_liquid_water_content_of_soil_layer_2.nc" $MED_LND_LON $MED_LND_LAT "liquid_water_content_of_soil_layer_2"
plot_med "field_med_from_lnd_liquid_water_content_of_soil_layer_3.nc" $MED_LND_LON $MED_LND_LAT "liquid_water_content_of_soil_layer_3"
plot_med "field_med_from_lnd_liquid_water_content_of_soil_layer_4.nc" $MED_LND_LON $MED_LND_LAT "liquid_water_content_of_soil_layer_4"
plot_med "field_med_from_lnd_moisture_content_of_soil_layer_1.nc"     $MED_LND_LON $MED_LND_LAT "moisture_content_of_soil_layer_1"
plot_med "field_med_from_lnd_moisture_content_of_soil_layer_2.nc"     $MED_LND_LON $MED_LND_LAT "moisture_content_of_soil_layer_2"
plot_med "field_med_from_lnd_moisture_content_of_soil_layer_3.nc"     $MED_LND_LON $MED_LND_LAT "moisture_content_of_soil_layer_3"
plot_med "field_med_from_lnd_moisture_content_of_soil_layer_4.nc"     $MED_LND_LON $MED_LND_LAT "moisture_content_of_soil_layer_4"
plot_med "field_med_from_lnd_subsurface_runoff_flux.nc"               $MED_LND_LON $MED_LND_LAT "subsurface_runoff_flux"
plot_med "field_med_from_lnd_surface_runoff_flux.nc"                  $MED_LND_LON $MED_LND_LAT "surface_runoff_flux"
plot_med "field_med_from_lnd_temperature_of_soil_layer_1.nc"          $MED_LND_LON $MED_LND_LAT "temperature_of_soil_layer_1"
plot_med "field_med_from_lnd_temperature_of_soil_layer_2.nc"          $MED_LND_LON $MED_LND_LAT "temperature_of_soil_layer_2"
plot_med "field_med_from_lnd_temperature_of_soil_layer_3.nc"          $MED_LND_LON $MED_LND_LAT "temperature_of_soil_layer_3"
plot_med "field_med_from_lnd_temperature_of_soil_layer_4.nc"          $MED_LND_LON $MED_LND_LAT "temperature_of_soil_layer_4"
# From NEMS Mediator to HYD
plot_med "field_med_to_hyd_liquid_water_content_of_soil_layer_1.nc"   $MED_HYD_LON $MED_HYD_LAT "liquid_water_content_of_soil_layer_1"
plot_med "field_med_to_hyd_liquid_water_content_of_soil_layer_2.nc"   $MED_HYD_LON $MED_HYD_LAT "liquid_water_content_of_soil_layer_2"
plot_med "field_med_to_hyd_liquid_water_content_of_soil_layer_3.nc"   $MED_HYD_LON $MED_HYD_LAT "liquid_water_content_of_soil_layer_3"
plot_med "field_med_to_hyd_liquid_water_content_of_soil_layer_4.nc"   $MED_HYD_LON $MED_HYD_LAT "liquid_water_content_of_soil_layer_4"
plot_med "field_med_to_hyd_moisture_content_of_soil_layer_1.nc"       $MED_HYD_LON $MED_HYD_LAT "moisture_content_of_soil_layer_1"
plot_med "field_med_to_hyd_moisture_content_of_soil_layer_2.nc"       $MED_HYD_LON $MED_HYD_LAT "moisture_content_of_soil_layer_2"
plot_med "field_med_to_hyd_moisture_content_of_soil_layer_3.nc"       $MED_HYD_LON $MED_HYD_LAT "moisture_content_of_soil_layer_3"
plot_med "field_med_to_hyd_moisture_content_of_soil_layer_4.nc"       $MED_HYD_LON $MED_HYD_LAT "moisture_content_of_soil_layer_4"
plot_med "field_med_to_hyd_subsurface_runoff_flux.nc"                 $MED_HYD_LON $MED_HYD_LAT "subsurface_runoff_flux"
plot_med "field_med_to_hyd_surface_runoff_flux.nc"                    $MED_HYD_LON $MED_HYD_LAT "surface_runoff_flux"
plot_med "field_med_to_hyd_temperature_of_soil_layer_1.nc"            $MED_HYD_LON $MED_HYD_LAT "temperature_of_soil_layer_1"
plot_med "field_med_to_hyd_temperature_of_soil_layer_2.nc"            $MED_HYD_LON $MED_HYD_LAT "temperature_of_soil_layer_2"
plot_med "field_med_to_hyd_temperature_of_soil_layer_3.nc"            $MED_HYD_LON $MED_HYD_LAT "temperature_of_soil_layer_3"
plot_med "field_med_to_hyd_temperature_of_soil_layer_4.nc"            $MED_HYD_LON $MED_HYD_LAT "temperature_of_soil_layer_4"
# LIS HIST Output
plot_lis "Qs_acc" 1
plot_lis "Qsb_acc" 1
plot_lis "SoilMoist_tavg" 1
plot_lis "SoilMoist_tavg" 2
plot_lis "SoilMoist_tavg" 3
plot_lis "SoilMoist_tavg" 4
plot_lis "SoilTemp_tavg" 1
plot_lis "SoilTemp_tavg" 2
plot_lis "SoilTemp_tavg" 3
plot_lis "SoilTemp_tavg" 4
plot_lis "SmLiqFrac_tavg" 1
plot_lis "SmLiqFrac_tavg" 2
plot_lis "SmLiqFrac_tavg" 3
plot_lis "SmLiqFrac_tavg" 4
plot_lis "Rainf_f_inst" 1
plot_lis "Tair_f_tavg" 1
plot_lis "Qair_f_tavg" 1
plot_lis "Psurf_f_tavg" 1
plot_lis "SWdown_f_tavg" 1
plot_lis "LWdown_f_tavg" 1
plot_lis "NWind_f_inst" 1
plot_lis "EWind_f_inst" 1

# Create compressed tar archive of all .gif files
echo "*** Adding to plots.tar.gz ***"
tar -cf plots_NEMS_coldstart.tar *.gif

# echo "*** Removing MED_FROM_ATM MED_FROM_LND MED_TO_LND MED_TO_HYD ***"

