#!/bin/bash

declare -i COLS=`tput cols`
HR=""
for i in `seq 1 ${COLS}`; do HR="${HR}#"; done

LOGF="log.forecast"
LOGB="log.2011082600"
NMLH="hydro.namelist"

echo $HR
echo "# Components"
echo $HR
grep "ATM_active:" coamps.rc
grep "HYD_active:" coamps.rc
grep "LND_active:" coamps.rc

echo $HR
echo "# Configuration"
echo $HR
grep "srcDir" $LOGB
grep "run_duration: " coamps.rc
grep "run_sequence: " coamps.rc
printf "PETs: "; ls -1q PET* | wc -l
grep "ATM_verbosity" coamps.rc
grep "ATM_realize_all_export" coamps.rc
grep "ATM_restart_interval" coamps.rc
grep "ATM_debug_interval" coamps.rc
grep "LND_attributes::" coamps.rc -A 8
grep "HYD_attributes::" coamps.rc -A 8
grep "pet_count:" coamps.rc
grep "ATM-TO-LND" coamps.rc
grep "LND-TO-ATM" coamps.rc
grep "LND-TO-HYD" coamps.rc
grep "HYD-TO-LND" coamps.rc
DTRT=`grep "DTRT = " $NMLH`
echo "Routing model timestep "$DTRT
SRON=`grep "SUBRTSWCRT = " $NMLH`
echo "Subsurface routing "$SRON
ORON=`grep "OVRTSWCRT = " $NMLH`
echo "Surface overland flow routing "$ORON
CRON=`grep "CHANRTSWCRT = " $NMLH`
echo "Channel routing "$CRON
GWON=`grep "GWBASESWCRT = " $NMLH`
echo "Baseflow bucket model "$GWON

echo $HR
echo "# Results"
echo $HR
PF1=`ls -1 PET* | head -1`
FL=`head -n 1 $PF1`
LL=`tail -n 1 $PF1`
LP=`grep "<<<\|>>>" $PF1 | tail -n 1`
echo "First Log Entry:   "$FL
echo "Final Log Entry:   "$LL
echo "Final Phase Entry: "$LP

declare -i start_day=`echo ${FL:6:2} | sed 's/^0*//'`
declare -i start_hour=`echo ${FL:9:2} | sed 's/^0*//'`
declare -i start_min=`echo ${FL:11:2} | sed 's/^0*//'`
declare -i start_sec=`echo ${FL:13:2} | sed 's/^0*//'`
declare -i end_day=`echo ${LL:6:2} | sed 's/^0*//'`
declare -i end_hour=`echo ${LL:9:2} | sed 's/^0*//'`
declare -i end_min=`echo ${LL:11:2} | sed 's/^0*//'`
declare -i end_sec=`echo ${LL:13:2} | sed 's/^0*//'`
let end_day=(${end_day} - 1)
let end_hour=(${end_hour} + 24 - 1)
let end_min=(${end_min} + 60 - 1)
let end_sec=(${end_sec} + 60)
let logtime_s=(${end_day} - ${start_day})
let logtime_s=(${logtime_s} * 24 + ${end_hour} - ${start_hour})
let logtime_s=(${logtime_s} * 60 + ${end_min} - ${start_min})
let logtime_s=(${logtime_s} * 60 + ${end_sec} - ${start_sec})
let logtime_m=(${logtime_s} / 60)
echo "Log Time Diff (s): "$logtime_s
echo "Log Time Diff (m): "$logtime_m

LC=`grep "crash" $LOGB`
EL=`grep "exceeded limit" $LOGB`
echo "PBS Log Crash:     "$LC
echo "PBS Log Walltime:  "$EL

HE=`grep -m1 "fatal error" $LOGF`
echo "WRFHydro Stop:     "$HE

echo "FORTRAN Traceback: "
grep -m1 "forrtl" $LOGF -A 15

echo $HR
echo "# PET ERRORS"
echo $HR
grep ERROR PET* | grep -v "ESMF_ConfigFindLabel Not found" | head -n15
