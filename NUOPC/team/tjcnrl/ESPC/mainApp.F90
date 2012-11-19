#define FILENAME "mainApp.F90"

program mainApp

  !-----------------------------------------------------------------------------
  ! Generic application driver
  !-----------------------------------------------------------------------------

  use ESMF
  use DRV, only: drvSS => SetServices

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: gcomp
  integer                 :: argCount
  type(ESMF_Config)       :: config
  character(ESMF_MAXSTR)  :: configFile
  
  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the earth system Component
  gcomp = ESMF_GridCompCreate(name="DRV", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set config for the driver Component
  call ESMF_UtilGetArgC(argCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (argCount.eq.1) then
    call ESMF_UtilGetArg(1, argValue=configFile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ConfigLoadFile(config, trim(configFile), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
    
  ! SetServices for the driver Component
  call ESMF_GridCompSetServices(gcomp, drvSS, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  ! Call Initialize for the driver Component
  call ESMF_GridCompInitialize(gcomp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Call Run for the driver Component
  call ESMF_GridCompRun(gcomp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Call Finalize for the driver Component
  call ESMF_GridCompFinalize(gcomp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  ! Destroy the driver Component
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Finalize ESMF
  call ESMF_Finalize()
  
end program  
