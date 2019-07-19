#!/bin/bash
  
MPIRUN="mpirun -oversubscribe -np"

gmake dust
gmake
{ set -x; } 2>/dev/null
mpirun -oversubscribe -np 4 ./esmApp > output.log 2>&1
result=$?
{ set +x; } 2>/dev/null
if [[ $result -eq 0 ]]
then
echo FINISHED: PASS
else
echo FINISHED: FAIL
fi

