#!/bin/bash

VARIABLES="variables.dat"

SCRIPT=shade_NEMS.jnl
DATE=201504020000
DOM=d01
FILE=LIS_HIST_$DATE.$DOM.nc

LON=lis_input.t574r.nc
LAT=lis_input.t574r.nc

# convert to single image function
function generate_plot {

 if [ "$#" -lt 2 ]
 then
   echo "ERROR USAGE: $0 <FIELD> <LEVEL>"
   exit 1
 fi

 FIELD="$1"
 LEVEL="$2"
 ferret -gif -script $SCRIPT $VARIABLES $FILE $LON $LAT $FIELD $LEVEL

}

generate_plot "Qs_acc" 1
generate_plot "Qsb_acc" 1
generate_plot "SoilMoist_tavg" 1
generate_plot "SoilMoist_tavg" 2
generate_plot "SoilMoist_tavg" 3
generate_plot "SoilMoist_tavg" 4
generate_plot "SoilTemp_tavg" 1
generate_plot "SoilTemp_tavg" 2
generate_plot "SoilTemp_tavg" 3
generate_plot "SoilTemp_tavg" 4
generate_plot "SmLiqFrac_tavg" 1
generate_plot "SmLiqFrac_tavg" 2
generate_plot "SmLiqFrac_tavg" 3
generate_plot "SmLiqFrac_tavg" 4
generate_plot "Rainf_f_inst" 1
generate_plot "Tair_f_tavg" 1
generate_plot "Qair_f_tavg" 1
generate_plot "Psurf_f_tavg" 1
generate_plot "SWdown_f_tavg" 1
generate_plot "LWdown_f_tavg" 1
generate_plot "NWind_f_inst" 1
generate_plot "EWind_f_inst" 1

# Create compressed tar archive of all .gif files
echo "*** Adding to plots.tar.gz ***"
tar -cf plots_LIS.tar shade_LIS*.gif

# echo "*** Removing MED_FROM_ATM MED_FROM_LND MED_TO_LND MED_TO_HYD ***"
# rm -f LIS*.gif
