#!/bin/bash
#PBS -S /bin/bash
#PBS -N ESMF_Test
#PBS -o output.log
#PBS -j oe
#PBS -q __QUEUE__
#PBS -A __ACCOUNT__
#PBS -l select=__NODES__:ncpus=__PROCSPERNODE__:mpiprocs=__PROCSPERNODE__
#PBS -l walltime=__HR__:__MIN__:__SEC__

# exit on error
set -e

# set umask
umask 022

# set limits
ulimit -t unlimited
ulimit -f unlimited
ulimit -d unlimited
ulimit -s unlimited
ulimit -c unlimited
ulimit -m unlimited

# list modules
module list

echo "PBS_O_WORKDIR: $PBS_O_WORKDIR"
echo "PBS_JOBID: $PBS_JOBID"

#export ESMF_RUNTIME_COMPLIANCECHECK=OFF
export ESMF_RUNTIME_TRACE=ON
#export ESMF_RUNTIME_TRACE_PETLIST="0"
#export ESMF_RUNTIME_PROFILE=ON
#export ESMF_RUNTIME_PROFILE_OUTPUT="TEXT BINARY"
#export ESMF_RUNTIME_PROFILE_PETLIST="0 1 2 3"

# -----------------------------------------------------------------------
# Execute the model
# -----------------------------------------------------------------------
cd ${PBS_O_WORKDIR}

s_tm=$(date +%s)
s_hr=$(date +%H); s_mn=$(date +%M); s_sc=$(date +%S)
echo "Model Start    ${s_hr}:${s_mn}:${s_sc}"

__MPIRUN__ __FLAGS__ __PROCS__ ./esmTest.exe

e_tm=$(date +%s)
e_hr=$(date +%H); e_mn=$(date +%M); e_sc=$(date +%S)
echo "Model End      ${e_hr}:${e_mn}:${e_sc}"

r_tm=$((e_tm-s_tm))
r_hr=$(($r_tm/3600)); r_mn=$((($r_tm%3600)/60)); r_sc=$(($r_tm%60))
echo "Model Runtime  ${r_hr}:${r_mn}:${r_sc}"

exit 0
