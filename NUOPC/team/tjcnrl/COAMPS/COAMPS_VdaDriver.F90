!-------------------------------------------------------------------------------
! COAMPS 4dvar application driver
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_VdaDriver.F90"
#include "COAMPS_Macros.h"


program mainApp

  use ESMF
  use NUOPC
  use FRONT_ADM, only: drmSS_ad => SetServices
  use FRONT_RPM, only: drmSS_rp => SetServices
  use COAMPS_Dutil

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: gcomp_ad, gcomp_rp
  integer                 :: argCount
  type(ESMF_Config)       :: config
  character(ESMF_MAXSTR)  :: configFile
  character(ESMF_MAXSTR)  :: msgString
  type(ESMF_VM)           :: vm
  integer                 :: petCount, localPet
  integer, parameter      :: rootPet = 0
  logical                 :: atm_active, ocn_active, wav_active

  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set log defaults
  call ESMF_LogSet(flush=.true.)

  ! Get VM info
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Add required fields to NUOPC field dictionary
  call InitFieldDictionary(rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the driver Components
  gcomp_ad = ESMF_GridCompCreate(name='COAMPS_AD', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  gcomp_rp = ESMF_GridCompCreate(name='COAMPS_RP', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create & set config for the driver Components
  configFile = 'coamps.rc'
  call ESMF_UtilGetArgC(argCount, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (argCount.eq.1) then
    call ESMF_UtilGetArg(1, argValue=configFile, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  config = ESMF_ConfigCreate(rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigLoadFile(config, trim(configFile), rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompSet(gcomp_ad, config=config, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompSet(gcomp_rp, config=config, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! SetServices for the adjoint driver Component
  call ESMF_GridCompSetServices(gcomp_ad, drmSS_ad, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! SetServices for the TL driver Component
  call ESMF_GridCompSetServices(gcomp_rp, drmSS_rp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get model active flags from config
  call ESMF_ConfigGetAttribute(config, atm_active, default=.false., &
       label='ATM_active:', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config, ocn_active, default=.false., &
       label='OCN_active:', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config, wav_active, default=.false., &
       label='WAV_active:', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !!! ***** CG set-up will go here; begin CG loop ***** !!!
  if (localPet.eq.rootPet) then
    write (*,'(a,3l2)') 'model active flags: ',atm_active,ocn_active,wav_active
  endif

  !!! ***** Begin matrix multiplication (action of adjoint and TLM) ***** !!!

  !!! ***** Run the adjoint model ***** !!!
       ! Call Initialize for the adjoint driver Component
       call ESMF_GridCompInitialize(gcomp_ad, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       ! Call Run for the adjoint driver Component
       call ESMF_GridCompRun(gcomp_ad, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       ! Call Finalize for the adjoint driver Component
       call ESMF_GridCompFinalize(gcomp_ad, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !!! ***** Run the tangent linear model ***** !!!
       ! Call Initialize for the TL driver Component
       call ESMF_GridCompInitialize(gcomp_rp, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       ! Call Run for the TL driver Component
       call ESMF_GridCompRun(gcomp_rp, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       ! Call Finalize for the TL driver Component
       call ESMF_GridCompFinalize(gcomp_rp, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !!! ***** Check for convergence of solution ***** !!!

  !!! ***** If converged; run post multiplication step (action of AD & TL) ***** !!!

  ! Destroy the driver Components
  call ESMF_GridCompDestroy(gcomp_ad, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(gcomp_rp, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize()

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine DummyRoutine(rc)
    integer, intent(out) :: rc

    ! local variables
    ! none

    rc = ESMF_SUCCESS

  end subroutine

end program  
