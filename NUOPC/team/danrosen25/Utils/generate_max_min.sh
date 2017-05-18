#!/bin/sh

ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_liquid_water_content_of_soil_layer_1.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_liquid_water_content_of_soil_layer_2.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_liquid_water_content_of_soil_layer_3.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_liquid_water_content_of_soil_layer_4.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_moisture_content_of_soil_layer_1.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_moisture_content_of_soil_layer_2.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_moisture_content_of_soil_layer_3.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_moisture_content_of_soil_layer_4.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_subsurface_runoff_flux.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_surface_runoff_flux.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_temperature_of_soil_layer_1.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_temperature_of_soil_layer_2.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_temperature_of_soil_layer_3.nc
ncatted -a _FillValue,,o,f,999999 field_med_from_lnd_temperature_of_soil_layer_4.nc

ncwa -A -y max field_med_from_atm_inst_down_lw_flx.nc                     field_med_from_atm_max.nc
ncwa -A -y max field_med_from_atm_inst_down_sw_flx.nc                     field_med_from_atm_max.nc
ncwa -A -y max field_med_from_atm_inst_merid_wind_height_lowest.nc        field_med_from_atm_max.nc
ncwa -A -y max field_med_from_atm_inst_pres_height_surface.nc             field_med_from_atm_max.nc
ncwa -A -y max field_med_from_atm_inst_spec_humid_height_lowest.nc        field_med_from_atm_max.nc
ncwa -A -y max field_med_from_atm_inst_temp_height_lowest.nc              field_med_from_atm_max.nc
ncwa -A -y max field_med_from_atm_inst_zonal_wind_height_lowest.nc        field_med_from_atm_max.nc
ncwa -A -y max field_med_from_atm_mean_prec_rate.nc                       field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_liquid_water_content_of_soil_layer_1.nc field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_liquid_water_content_of_soil_layer_2.nc field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_liquid_water_content_of_soil_layer_3.nc field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_liquid_water_content_of_soil_layer_4.nc field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_moisture_content_of_soil_layer_1.nc     field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_moisture_content_of_soil_layer_2.nc     field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_moisture_content_of_soil_layer_3.nc     field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_moisture_content_of_soil_layer_4.nc     field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_subsurface_runoff_flux.nc               field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_surface_runoff_flux.nc                  field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_temperature_of_soil_layer_1.nc          field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_temperature_of_soil_layer_2.nc          field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_temperature_of_soil_layer_3.nc          field_med_from_atm_max.nc
ncwa -A -y max field_med_from_lnd_temperature_of_soil_layer_4.nc          field_med_from_atm_max.nc

ncwa -A -y min field_med_from_atm_inst_down_lw_flx.nc                     field_med_from_atm_min.nc
ncwa -A -y min field_med_from_atm_inst_down_sw_flx.nc                     field_med_from_atm_min.nc
ncwa -A -y min field_med_from_atm_inst_merid_wind_height_lowest.nc        field_med_from_atm_min.nc
ncwa -A -y min field_med_from_atm_inst_pres_height_surface.nc             field_med_from_atm_min.nc
ncwa -A -y min field_med_from_atm_inst_spec_humid_height_lowest.nc        field_med_from_atm_min.nc
ncwa -A -y min field_med_from_atm_inst_temp_height_lowest.nc              field_med_from_atm_min.nc
ncwa -A -y min field_med_from_atm_inst_zonal_wind_height_lowest.nc        field_med_from_atm_min.nc
ncwa -A -y min field_med_from_atm_mean_prec_rate.nc                       field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_liquid_water_content_of_soil_layer_1.nc field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_liquid_water_content_of_soil_layer_2.nc field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_liquid_water_content_of_soil_layer_3.nc field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_liquid_water_content_of_soil_layer_4.nc field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_moisture_content_of_soil_layer_1.nc     field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_moisture_content_of_soil_layer_2.nc     field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_moisture_content_of_soil_layer_3.nc     field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_moisture_content_of_soil_layer_4.nc     field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_subsurface_runoff_flux.nc               field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_surface_runoff_flux.nc                  field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_temperature_of_soil_layer_1.nc          field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_temperature_of_soil_layer_2.nc          field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_temperature_of_soil_layer_3.nc          field_med_from_atm_min.nc
ncwa -A -y min field_med_from_lnd_temperature_of_soil_layer_4.nc          field_med_from_atm_min.nc

ncdump field_med_from_atm_min.nc
ncdump field_med_from_atm_max.nc
