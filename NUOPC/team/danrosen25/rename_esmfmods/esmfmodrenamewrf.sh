#!/bin/bash

# This script is intended to replace text within all fortran files so that
# module name clashes for esmf can be resolved
# Dan Rosen, CIRES, NOAA, EarthSystemsBridge

FILEREGEX=".*\.\(f\|F\|f90\|F90\)"

function replace
{	
	if [ -z $1 ]
	then
		echo "Cannot replace null string"
		return 1
	elif ([[ ${1,,} == *"esmf"* ]])
	then
		REPLACEREGEX='s/\b'$1'\b/W'$1'/gI'

		find -regex $FILEREGEX -exec sed -i $REPLACEREGEX '{}' \;
		echo "Complete: "$REPLACEREGEX
	else
		echo "String replacement intended for esmf"
		return	
	fi
}

function undoReplace
{	
	if [ -z $1 ]
	then
		echo "Cannot undo null string"
		return
	elif ([[ ${1,,} == *"esmf"* ]])
	then
		REPLACEREGEX='s/\bW'$1'\b/'$1}'/gI'

		find -regex $FILEREGEX -exec sed -i $REPLACEREGEX '{}' \;
		echo "Complete: "$REPLACEREGEX
	else
		echo "String undo intended for esmf"
		return	
	fi
}

declare -a ESMFMODULES=("ESMF_TimeMod" "ESMF_TimeIntervalMod" "ESMF_CalendarMod" "ESMF_Stubs" "ESMF_BaseTimeMod" "ESMF_ClockMod" "ESMF_BaseMod" "ESMF_AlarmMod" "ESMF_AlarmClockMod")

for i in "${ESMFMODULES[@]}"
do
	if ([[ ${1,,} == "-undo" ]])
	then
		undoReplace "$i"
	else
		replace "$i"
	fi
done
