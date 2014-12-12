!-------------------------------------------------------------------------------
! COAMPS 4dvar application driver
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_VdaDriver.F90"
#include "COAMPS_Macros.h"
#define USE_RUN_PREP_POST


program mainApp

  use ESMF
  use NUOPC
  use FRONT_ADM, only: drmSS_ad => SetServices
  use FRONT_RPM, only: drmSS_rp => SetServices
  use COAMPS_Dutil

  implicit none

  !! ESMF driver variables
  integer                 :: rc, urc
  type(ESMF_GridComp)     :: gcomp_ad, gcomp_rp
  integer                 :: argCount
  type(ESMF_Config)       :: config
  character(ESMF_MAXSTR)  :: configFile
  type(ESMF_VM)           :: vm
  character(ESMF_MAXSTR)  :: msgString
  integer                 :: petCount, localPet
  integer, parameter      :: rootPet = 0
  integer     , parameter :: run_prep_phase = 2
  integer     , parameter :: run_post_phase = 3

  !! configuration file variables
  logical                 :: atm_active, ocn_active, wav_active

  !! ocean observation variables
  integer                 :: n_ocn
  real                    :: oc_inv,oc_val,oc_err
  real                    :: oc_xi,oc_yj,oc_lvl
  real                    :: oc_lat,oc_lon
  integer,allocatable     :: ocn_dtg(:),ocn_tme( :)
  integer,allocatable     :: ocn_var(:),ocn_typ( :)
  real(8),allocatable     :: ocn_inv(:),ocn_err( :)
  real(8),allocatable     :: ocn_cgp(:),ocn_cgAp(:)
  real(8),allocatable     :: ocn_xi( :),ocn_yj(  :)
  real(8),allocatable     :: ocn_lvl(:)

  !! atmospheric observation variables (more added later)
  integer                 :: n_atm

  !! wave observation variables (more added later)
  integer                 :: n_wav

  !! Conjugate gradient variables
  integer, parameter      :: num_cg_itr = 5
  integer                 :: n_obs,cg_itr
  real(8),allocatable     :: beta(:),cgAp(:),cgr(:)
  real(8),allocatable     :: cgp( :),cgp1(:),err(:)
  real(8)                 :: cg_rzOld,cg_beta
  real(8)                 :: cg_alpha,cg_omega
  real(8)                 :: cg_bsum,cg_rz
  real(8)                 :: cg_rAz,cg_rho

  !! local variables
  integer,parameter       :: iounit=1005
  integer                 :: i,j,k,ob,n_data
  integer                 :: ana_dtg
  character(ESMF_MAXSTR)  :: fname,sfx,cenv,anadtg
  character(ESMF_MAXSTR)  :: obs_dir,io_dir

!!===================================================================================!!
!!                          ESMF SETUP FUNCTIONS                                     !!
!!===================================================================================!!

  !! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! Set log defaults
  call ESMF_LogSet(flush=.true.)

  !! Get VM info
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! Add required fields to NUOPC field dictionary
  call InitFieldDictionary(rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! Create the driver Components
  gcomp_ad = ESMF_GridCompCreate(name='COAMPS_AD', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  gcomp_rp = ESMF_GridCompCreate(name='COAMPS_RP', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! Create & set config for the driver Components
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

  !! Get model active flags from config
  call ESMF_ConfigGetAttribute(config, atm_active, default=.false., &
       label='ATM_active:', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config, ocn_active, default=.false., &
       label='OCN_active:', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config, wav_active, default=.false., &
       label='WAV_active:', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! SetServices for the AD & TL driver Components
  call ESMF_GridCompSetServices(gcomp_ad, drmSS_ad, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompSetServices(gcomp_rp, drmSS_rp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef USE_RUN_PREP_POST
  !! Initialize for the AD & TL driver Components
  call ESMF_GridCompInitialize(gcomp_ad, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(gcomp_rp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

!!===================================================================================!!
!!                       Obtain observations and set-up CG loop                      !!
!!===================================================================================!!

  !! Print active flags
  if (localPet.eq.rootPet) then
    write (*,'(a,3l2)') 'model active flags: ',atm_active,ocn_active,wav_active
  endif

!!===================================================================================!!
!!                           Begin CG Minimization Loop                              !!
!!===================================================================================!!

  cg_iteration_loop: do cg_itr = 1,num_cg_itr

     write(msgString,'(a,1i6)') 'CG iteration: ',cg_itr
     call ESMF_LogWrite('', ESMF_LOGMSG_INFO)
     call ESMF_LogWrite('', ESMF_LOGMSG_INFO)
     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
     if (localPet.eq.rootPet) write (*,'(//a)') trim(msgString)

  !!! ***** Begin matrix multiplication (action of adjoint and TLM) ***** !!!

  !!! ***** Run the adjoint model ***** !!!
#ifdef USE_RUN_PREP_POST
       ! Call RunPrep for the adjoint driver Component
       call ESMF_GridCompRun(gcomp_ad, phase=run_prep_phase, userRc=urc, rc=rc)
#else
       ! Call Initialize for the adjoint driver Component
       call ESMF_GridCompInitialize(gcomp_ad, userRc=urc, rc=rc)
#endif
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       ! Call Run     for the adjoint driver Component
       call ESMF_GridCompRun(gcomp_ad, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#ifdef USE_RUN_PREP_POST
       ! Call RunPost for the adjoint driver Component
       call ESMF_GridCompRun(gcomp_ad, phase=run_post_phase, userRc=urc, rc=rc)
#else
       ! Call Finalize for the adjoint driver Component
       call ESMF_GridCompFinalize(gcomp_ad, userRc=urc, rc=rc)
#endif
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !!! ***** Run the tangent linear model ***** !!!
#ifdef USE_RUN_PREP_POST
       ! Call RunPrep for the TL driver Component
       call ESMF_GridCompRun(gcomp_rp, phase=run_prep_phase, userRc=urc, rc=rc)
#else
       ! Call Initialize for the TL driver Component
       call ESMF_GridCompInitialize(gcomp_rp, userRc=urc, rc=rc)
#endif
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       ! Call Run     for the TL driver Component
       call ESMF_GridCompRun(gcomp_rp, userRc=urc, rc=rc)
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#ifdef USE_RUN_PREP_POST
       ! Call RunPost for the TL driver Component
       call ESMF_GridCompRun(gcomp_rp, phase=run_post_phase, userRc=urc, rc=rc)
#else
       ! Call Finalize for the TL driver Component
       call ESMF_GridCompFinalize(gcomp_rp, userRc=urc, rc=rc)
#endif
       if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !!! ***** Check for convergence of solution ***** !!!

!!===================================================================================!!
!!                             Post-Multiplication Step                              !!
!!===================================================================================!!

  enddo cg_iteration_loop

!!===================================================================================!!
!!                                   Finalize Run                                    !!
!!===================================================================================!!

#ifdef USE_RUN_PREP_POST
  !! Finalize for the AD & TL driver Components
  call ESMF_GridCompFinalize(gcomp_ad, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompFinalize(gcomp_rp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  !! Destroy the driver Components
  call ESMF_GridCompDestroy(gcomp_ad, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(gcomp_rp, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! Finalize ESMF
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
