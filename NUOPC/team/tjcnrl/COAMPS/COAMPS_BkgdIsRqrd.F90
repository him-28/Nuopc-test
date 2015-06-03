!-------------------------------------------------------------------------------
! COAMPS "background is required" utility
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_BkgdIsRqrd.F90"
#include "COAMPS_Macros.h"


program BkgdIsRqrd

  use ESMF
  use COAMPS_Gutil

  implicit none

  integer                 :: rc, urc
  character(ESMF_MAXSTR)  :: logFile="log.BkgdIsRqrd"
  integer                 :: argCount
  character(ESMF_MAXSTR)  :: srcGridnl
  character(ESMF_MAXSTR)  :: dstGridnl
  integer                 :: srcNest=0
  integer                 :: dstNest=0
  logical                 :: srcCoversDst
  
  ! Initialize ESMF
  call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_SINGLE, &
    defaultLogFileName=trim(logFile), &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get command-line arguments
  call ESMF_UtilGetArgC(argCount, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (argCount.eq.2) then
    call ESMF_UtilGetArg(1, argValue=srcGridnl, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_UtilGetArg(2, argValue=dstGridnl, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='Missing required arguments')
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='Usage: COAMPS_BkgdIsRqrd srcGridnl dstGridnl')
      call ESMF_Finalize(endFlag=ESMF_END_ABORT)
  endif

  ! Check if src grid covers dst grid
  srcCoversDst = GridIsCovered(srcGridnl, srcNest, dstGridnl, dstNest, rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Finalize ESMF
  call ESMF_Finalize()

  ! Set return based on cover flag
  if (srcCoversDst) then
    write(*,'(1a1)') 'f'
  else
    write(*,'(1a1)') 't'
  endif

end program  
