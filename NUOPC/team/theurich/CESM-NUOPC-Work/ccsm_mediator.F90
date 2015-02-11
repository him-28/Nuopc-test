module ccsm_mediator_mod

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  !-----------------------------------------------------------------------------

#ifdef ESMF_INTERFACE

  use ESMF
  use NUOPC
  use NUOPC_Mediator, only: &
    mediator_routine_SS           => routine_SetServices, &
    mediator_label_DataInitialize => label_DataInitialize, &
    mediator_label_Advance        => label_Advance
!  use NUOPC_ModelBase, only: &
!    ModelBase_label_TimestampExport => label_TimestampExport


   use seq_flds_mod
   use esmfshr_mod
   use esmfshr_nuopc_mod
   use ccsm_comp_mod
   use component_mod
   use component_type_mod
   use prep_ocn_mod
   use prep_lnd_mod
   use prep_ice_mod
   use prep_wav_mod
   use prep_rof_mod
   use prep_glc_mod
   use prep_atm_mod
   use prep_aoflux_mod
   use seq_infodata_mod, only: seq_infodata_type
   use seq_infodata_mod, only: seq_infodata_orb_variable_year
   use seq_infodata_mod, only: seq_infodata_GetData, seq_infodata_PutData
   use mct_mod
   use shr_kind_mod, only: r8 => SHR_KIND_R8
   use shr_kind_mod, only: CL => SHR_KIND_CL
   use shr_kind_mod, only: CS => SHR_KIND_CS
   use shr_orb_mod
   use seq_rest_mod, only: seq_rest_read, seq_rest_write
   use seq_hist_mod, only: seq_hist_write, seq_hist_writeavg, seq_hist_writeaux
   use seq_flux_mct, only: seq_flux_ocnalb_mct, seq_flux_atmocn_mct
   use seq_flux_mct, only: seq_flux_atmocnexch_mct
   use seq_diag_mct, only: seq_diag_lnd_mct, seq_diag_ice_mct  
   use seq_diag_mct, only: seq_diag_rof_mct, seq_diag_ocn_mct
   use seq_diag_mct, only: seq_diag_atm_mct
   use seq_diag_mct, only: seq_diag_accum_mct, seq_diag_zero_mct
   use seq_frac_mct, only: seq_frac_set

   !! KDS: may be able to get rid of these and use ESMF functions instead
   use seq_timemgr_mod
   use shr_cal_mod
   use seq_comm_mct, only: num_inst_atm, num_inst_lnd, num_inst_rof
   use seq_comm_mct, only: num_inst_ocn, num_inst_ice, num_inst_glc
   use seq_comm_mct, only: num_inst_wav, num_inst_xao, num_inst_frc


  implicit none

  private
  
  ! module variables that contain field from/to model components
  type(ESMF_State)  :: frATM, toATM
  type(ESMF_State)  :: frLND, toLND
  type(ESMF_State)  :: frOCN, toOCN
  type(ESMF_State)  :: frICE, toICE
  type(ESMF_State)  :: frGLC, toGLC
  type(ESMF_State)  :: frROF, toROF
  type(ESMF_State)  :: frWAV, toWAV

  public SetServices
#include <mpif.h>

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

print *, 'KDS: Mediator SetServices begin'
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, mediator_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    
    ! --- Initialization phases --------------------------------------

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p1: advertise Fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! IPDv03p5: realize all Fields with transfer action "accept"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeP5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! --- Run phases -------------------------------------------------

    ! Run phase: UPDATE_ORBITAL_DATA_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_UpdateOrbitalData, phase=UPDATE_ORBITAL_DATA_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: PREP_OCN_ICE_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepOCN_ICE, phase=PREP_OCN_ICE_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Run phase: PREP_OCN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepOCN, phase=PREP_OCN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: PREP_LND_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepLND, phase=PREP_LND_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: PREP_ICE_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepICE, phase=PREP_ICE_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: PREP_WAV_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepWAV, phase=PREP_WAV_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: PREP_ROF_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepROF, phase=PREP_ROF_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_OCN_TIGHT_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostOCN_Tight, phase=POST_OCN_TIGHT_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_LND_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostLND, phase=POST_LND_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: PREP_GLC_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepGLC, phase=PREP_GLC_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_ROF_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostROF, phase=POST_ROF_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: BUDGET_OLD_FRACTIONS_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_BudgetOldFractions, &
      phase=BUDGET_OLD_FRACTIONS_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_ICE_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostICE, phase=POST_ICE_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: UPDATE_FRACTIONS_FROM_ICE_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_UpdateFractionsFromICE, &
      phase=UPDATE_FRACTIONS_FROM_ICE_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: FLUX_CALC_ON_ATM_GRID_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_FluxCalcOnATM_Grid, &
      phase=FLUX_CALC_ON_ATM_GRID_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: PREP_ATM_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PrepATM, phase=PREP_ATM_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_WAV_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostWAV, phase=POST_WAV_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_GLC_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostGLC, phase=POST_GLC_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_ATM_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostATM, phase=POST_ATM_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: BUDGET_NEW_FRACTIONS_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_BudgetNewFractions, &
      phase=BUDGET_NEW_FRACTIONS_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: POST_OCN_LOOSE_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_PostOCN_Loose, phase=POST_OCN_LOOSE_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: SAVE_RESTART_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_SaveRestart, phase=SAVE_RESTART_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: WRITE_HISTORY_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_WriteHistory, phase=WRITE_HISTORY_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: ADVANCE_SYNC_CLOCK_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_AdvanceSyncClock, phase=ADVANCE_SYNC_CLOCK_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: OVERRIDE_OCN_ALARMS_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run_OverrideOcnAlarms, &
      phase=OVERRIDE_OCN_ALARMS_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: ATM_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_atmPreRun, phase=ATM_PRE_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: ATM_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_atmPostRun, phase=ATM_POST_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: GLC_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_glcPreRun, phase=GLC_PRE_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: GLC_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_glcPostRun, phase=GLC_POST_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: ICE_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_icePreRun, phase=ICE_PRE_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: ICE_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_icePostRun, phase=ICE_POST_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: LND_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_lndPreRun, phase=LND_PRE_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: LND_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_lndPostRun, phase=LND_POST_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: OCN_TIGHT_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_ocnPreRunTight, phase=OCN_TIGHT_PRE_RUN_PHASE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: OCN_TIGHT_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_ocnPostRunTight, phase=OCN_TIGHT_POST_RUN_PHASE, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: OCN_LOOSE_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_ocnPreRunLoose, phase=OCN_LOOSE_PRE_RUN_PHASE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: OCN_LOOSE_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_ocnPostRunLoose, phase=OCN_LOOSE_POST_RUN_PHASE, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: ROF_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_rofPreRun, phase=ROF_PRE_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: ROF_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_rofPostRun, phase=ROF_POST_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: WAV_PRE_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_wavPreRun, phase=WAV_PRE_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run phase: WAV_POST_RUN_PHASE
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=routine_wavPostRun, phase=WAV_POST_RUN_PHASE, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Run specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specRoutine=MediatorAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Specialize Run -> timestamp export Fields
!    call ESMF_MethodAdd(gcomp, label=MediatorBase_label_TimestampExport, &
!      userRoutine=TimestampExport, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    rc = ESMF_SUCCESS

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

#ifdef PREVIOUS

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer          :: localrc
    type(ESMF_State) :: atmImportState, atmExportState
    type(ESMF_State) :: ocnImportState, ocnExportState
    type(ESMF_State) :: iceImportState, iceExportState
    type(ESMF_State) :: lndImportState, lndExportState
    type(ESMF_State) :: rofImportState, rofExportState

    type(seq_infodata_type) :: infodata
    logical                 :: atm_present
    logical                 :: glc_present
    logical                 :: ice_present
    logical                 :: lnd_present
    logical                 :: ocn_present
    logical                 :: rof_present
    logical                 :: wav_present


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator InitializeP1 begin'

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Get infodata about which components are present and which aren't
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        atm_present=atm_present, &
        glc_present=glc_present, &
        ice_present=ice_present, &
        lnd_present=lnd_present, &
        ocn_present=ocn_present, &
        rof_present=rof_present, &
        wav_present=wav_present)
print *, 'KDS: Mediator InitializeP1 atm_present = ', atm_present
print *, 'KDS: Mediator InitializeP1 glc_present = ', glc_present
print *, 'KDS: Mediator InitializeP1 ice_present = ', ice_present
print *, 'KDS: Mediator InitializeP1 lnd_present = ', lnd_present
print *, 'KDS: Mediator InitializeP1 ocn_present = ', ocn_present
print *, 'KDS: Mediator InitializeP1 rof_present = ', rof_present
print *, 'KDS: Mediator InitializeP1 wav_present = ', wav_present

    call ESMF_AttributeGet(atm(1)%export_state, name='atm_present', value=atm_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(glc(1)%export_state, name='glc_present', value=glc_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(ice(1)%export_state, name='ice_present', value=ice_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(lnd(1)%export_state, name='lnd_present', value=lnd_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(ocn(1)%export_state, name='ocn_present', value=ocn_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(rof(1)%export_state, name='rof_present', value=rof_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(wav(1)%export_state, name='wav_present', value=wav_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

print *, 'KDS: ESMF Mediator InitializeP1 atm_present = ', atm_present
print *, 'KDS: ESMF Mediator InitializeP1 glc_present = ', glc_present
print *, 'KDS: ESMF Mediator InitializeP1 ice_present = ', ice_present
print *, 'KDS: ESMF Mediator InitializeP1 lnd_present = ', lnd_present
print *, 'KDS: ESMF Mediator InitializeP1 ocn_present = ', ocn_present
print *, 'KDS: ESMF Mediator InitializeP1 rof_present = ', rof_present
print *, 'KDS: ESMF Mediator InitializeP1 wav_present = ', wav_present

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Setup import-able fields
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! ocn
    call NUOPC_StateNamespaceAdd(importState, &
        namespace="OCN", &
        nestedStateName="NestedState-OCN", &
        nestedState=ocnImportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       ocn_export_fields, ocnImportState, tag="MED<-OCN Import", &
       comp_present=ocn_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! atm
    call NUOPC_StateNamespaceAdd(importState, &
        namespace="ATM", &
        nestedStateName="NestedState-ATM", &
        nestedState=atmImportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       atm_export_fields, atmImportState, tag="MED<-ATM Import", &
       comp_present=atm_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! lnd
    call NUOPC_StateNamespaceAdd(importState, &
        namespace="LND", &
        nestedStateName="NestedState-LND", &
        nestedState=lndImportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       lnd_export_fields, lndImportState, tag="MED<-LND Import", &
       comp_present=lnd_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! ice
    call NUOPC_StateNamespaceAdd(importState, &
        namespace="ICE", &
        nestedStateName="NestedState-ICE", &
        nestedState=iceImportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       ice_export_fields, iceImportState, tag="MED<-ICE Import", &
       comp_present=ice_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! rof
    call NUOPC_StateNamespaceAdd(importState, &
        namespace="ROF", &
        nestedStateName="NestedState-ROF", &
        nestedState=rofImportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       rof_export_fields, rofImportState, tag="MED<-ROF Import", &
       comp_present=rof_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! glc
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_g2x_states, importState, localrc)
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_g2x_fluxes, importState, localrc)

    ! wav
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_w2x_states, importState, localrc)
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_w2x_fluxes, importState, localrc)


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Setup export-able fields
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! ocn
    call NUOPC_StateNamespaceAdd(exportState, &
        namespace="OCN", &
        nestedStateName="NestedState-OCN", &
        nestedState=ocnExportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       ocn_import_fields, ocnExportState, tag="MED->OCN Export", &
       comp_present=ocn_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! atm
    call NUOPC_StateNamespaceAdd(exportState, &
        namespace="ATM", &
        nestedStateName="NestedState-ATM", &
        nestedState=atmExportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       atm_import_fields, atmExportState, tag="MED->ATM Export", &
       comp_present=atm_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! lnd
    call NUOPC_StateNamespaceAdd(exportState, &
        namespace="LND", &
        nestedStateName="NestedState-LND", &
        nestedState=lndExportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       lnd_import_fields, lndExportState, tag="MED->LND Export", &
       comp_present=lnd_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! ice
    call NUOPC_StateNamespaceAdd(exportState, &
        namespace="ICE", &
        nestedStateName="NestedState-ICE", &
        nestedState=iceExportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       ice_import_fields, iceExportState, tag="MED->ICE Export", &
       comp_present=ice_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! rof
    call NUOPC_StateNamespaceAdd(exportState, &
        namespace="ROF", &
        nestedStateName="NestedState-ROF", &
        nestedState=rofExportState, &
        rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call esmfshr_nuopc_advertise_fields( &
       rof_import_fields, rofExportState, tag="MED->ROF Export", &
       comp_present=rof_present, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! glc
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_x2g_states, exportState, localrc)
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_x2g_fluxes, exportState, localrc)

    ! wav
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_x2w_states, exportState, localrc)
!    call esmfshr_nuopc_advertise_fields( &
!       seq_flds_x2w_fluxes, exportState, localrc)

print *, 'KDS: Mediator InitializeP1 end'

  end subroutine

#else

  subroutine InitializeP1(mediator, importState, exportState, clock, rc)
    ! IPDv03p1: advertise Fields
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! Fields from ATM
    !   use namespace in the importState
    call NUOPC_StateNamespaceAdd(importState, namespace="ATM", &
      nestedState=frATM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(frATM, &
      StandardNames=atm_export_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields to ATM
    !   use namespace in the exportState
    call NUOPC_StateNamespaceAdd(exportState, namespace="ATM", &
      nestedState=toATM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(toATM, &
      StandardNames=atm_import_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields from LND
    !   use namespace in the importState
    call NUOPC_StateNamespaceAdd(importState, namespace="LND", &
      nestedState=frLND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(frLND, &
      StandardNames=lnd_export_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields to LND
    !   use namespace in the exportState
    call NUOPC_StateNamespaceAdd(exportState, namespace="LND", &
      nestedState=toLND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(toLND, &
      StandardNames=lnd_import_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields from OCN
    !   use namespace in the importState
    call NUOPC_StateNamespaceAdd(importState, namespace="OCN", &
      nestedState=frOCN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(frOCN, &
      StandardNames=ocn_export_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields to OCN
    !   use namespace in the exportState
    call NUOPC_StateNamespaceAdd(exportState, namespace="OCN", &
      nestedState=toOCN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(toOCN, &
      StandardNames=ocn_import_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields from ICE
    !   use namespace in the importState
    call NUOPC_StateNamespaceAdd(importState, namespace="ICE", &
      nestedState=frICE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(frICE, &
      StandardNames=ice_export_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields to ICE
    !   use namespace in the exportState
    call NUOPC_StateNamespaceAdd(exportState, namespace="ICE", &
      nestedState=toICE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(toICE, &
      StandardNames=ice_import_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields from ROF
    !   use namespace in the importState
    call NUOPC_StateNamespaceAdd(importState, namespace="ROF", &
      nestedState=frROF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(frROF, &
      StandardNames=rof_export_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields to ROF
    !   use namespace in the exportState
    call NUOPC_StateNamespaceAdd(exportState, namespace="ROF", &
      nestedState=toROF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
    call newNUOPC_StateAdvertiseFields(toROF, &
      StandardNames=rof_import_fields, &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Fields from GLC
    !   use namespace in the importState
    call NUOPC_StateNamespaceAdd(importState, namespace="GLC", &
      nestedState=frGLC, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
!    call newNUOPC_StateAdvertiseFields(frGLC, &
!      StandardNames=glc_export_fields, &
!      TransferOfferGeomObject="cannot provide", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
    ! Fields to GLC
    !   use namespace in the exportState
    call NUOPC_StateNamespaceAdd(exportState, namespace="GLC", &
      nestedState=toGLC, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
!    call newNUOPC_StateAdvertiseFields(toGLC, &
!      StandardNames=glc_import_fields, &
!      TransferOfferGeomObject="cannot provide", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
    ! Fields from WAV
    !   use namespace in the importState
    call NUOPC_StateNamespaceAdd(importState, namespace="WAV", &
      nestedState=frWAV, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
!    call newNUOPC_StateAdvertiseFields(frWAV, &
!      StandardNames=wav_export_fields, &
!      TransferOfferGeomObject="cannot provide", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
    ! Fields to WAV
    !   use namespace in the exportState
    call NUOPC_StateNamespaceAdd(exportState, namespace="WAV", &
      nestedState=toWAV, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !   advertise fields in the nested state
!    call newNUOPC_StateAdvertiseFields(toWAV, &
!      StandardNames=wav_import_fields, &
!      TransferOfferGeomObject="cannot provide", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    subroutine newNUOPC_StateAdvertiseFields(state, StandardNames, &
      ! The ESMF master already contains this changed interface -> can be 
      ! removed here once the latest ESMF snapshot is used.
      TransferOfferGeomObject, rc)
      type(ESMF_State), intent(inout)         :: state
      character(*),     intent(in)            :: StandardNames(:)
      character(*),     intent(in),  optional :: TransferOfferGeomObject
      integer,          intent(out), optional :: rc
      ! local variables
      integer                 :: i
    
      if (present(rc)) rc = ESMF_SUCCESS

      do i=1, size(StandardNames)
        call NUOPC_StateAdvertiseField(state, StandardName=StandardNames(i), &
          TransferOfferGeomObject=TransferOfferGeomObject, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
      
    end subroutine

  end subroutine
  
#endif

  !-----------------------------------------------------------------------------

#ifdef PREVIOUS

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)     :: atmImportState, atmExportState
    type(ESMF_State)     :: ocnImportState, ocnExportState
    type(ESMF_State)     :: iceImportState, iceExportState
    type(ESMF_State)     :: lndImportState, lndExportState
    type(ESMF_State)     :: rofImportState, rofExportState
    type(ESMF_Field)     :: field
    type(ESMF_Grid)      :: gridIn
    type(ESMF_Grid)      :: gridOut
    type(ESMF_Mesh)      :: meshIn
    type(ESMF_Mesh)      :: meshOut
    integer              :: localrc

    type(seq_infodata_type) :: infodata
    logical                 :: atm_present
    logical                 :: glc_present
    logical                 :: ice_present
    logical                 :: lnd_present
    logical                 :: ocn_present
    logical                 :: rof_present
    logical                 :: wav_present


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator InitializeP3 begin'
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        atm_present=atm_present, &
        glc_present=glc_present, &
        ice_present=ice_present, &
        lnd_present=lnd_present, &
        ocn_present=ocn_present, &
        rof_present=rof_present, &
        wav_present=wav_present)
print *, 'KDS: Mediator InitializeP3 atm_present = ', atm_present
print *, 'KDS: Mediator InitializeP3 glc_present = ', glc_present
print *, 'KDS: Mediator InitializeP3 ice_present = ', ice_present
print *, 'KDS: Mediator InitializeP3 lnd_present = ', lnd_present
print *, 'KDS: Mediator InitializeP3 ocn_present = ', ocn_present
print *, 'KDS: Mediator InitializeP3 rof_present = ', rof_present
print *, 'KDS: Mediator InitializeP3 wav_present = ', wav_present

    call ESMF_AttributeGet(atm(1)%export_state, name='atm_present', value=atm_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(glc(1)%export_state, name='glc_present', value=glc_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(ice(1)%export_state, name='ice_present', value=ice_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(lnd(1)%export_state, name='lnd_present', value=lnd_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(ocn(1)%export_state, name='ocn_present', value=ocn_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(rof(1)%export_state, name='rof_present', value=rof_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out
    call ESMF_AttributeGet(wav(1)%export_state, name='wav_present', value=wav_present, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

print *, 'KDS: ESMF Mediator InitializeP3 atm_present = ', atm_present
print *, 'KDS: ESMF Mediator InitializeP3 glc_present = ', glc_present
print *, 'KDS: ESMF Mediator InitializeP3 ice_present = ', ice_present
print *, 'KDS: ESMF Mediator InitializeP3 lnd_present = ', lnd_present
print *, 'KDS: ESMF Mediator InitializeP3 ocn_present = ', ocn_present
print *, 'KDS: ESMF Mediator InitializeP3 rof_present = ', rof_present
print *, 'KDS: ESMF Mediator InitializeP3 wav_present = ', wav_present


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Create and Realize Importable fields
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! ocn
    call ESMF_StateGet(importState, itemName="NestedState-OCN", &
       nestedState=ocnImportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(ocn_export_fields, ocn(1)%export_state, 'd2x', &
         ocnImportState, ocn_present, &
         tag='MED<-OCN', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

print *, 'KDS: Mediator atm im begin'
    ! atm
    call ESMF_StateGet(importState, itemName="NestedState-ATM", &
       nestedState=atmImportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(atm_export_fields, atm(1)%export_state, 'd2x', &
         atmImportState, atm_present, &
         tag='MED<-ATM', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

print *, 'KDS: Mediator lnd im begin'

    ! lnd
    call ESMF_StateGet(importState, itemName="NestedState-LND", &
       nestedState=lndImportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(lnd_export_fields, lnd(1)%export_state, 'd2x', &
         lndImportState, lnd_present, &
         tag='MED<-LND', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

print *, 'KDS: Mediator ice im begin'
    ! ice
    call ESMF_StateGet(importState, itemName="NestedState-ICE", &
       nestedState=iceImportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(ice_export_fields, ice(1)%export_state, 'd2x', &
         iceImportState, ice_present, &
         tag='MED<-ICE', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    ! rof
    call ESMF_StateGet(importState, itemName="NestedState-ROF", &
       nestedState=rofImportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(rof_export_fields, rof(1)%export_state, 'd2x', &
         rofImportState, rof_present, &
         tag='MED<-ROF', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    ! glc
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_g2x_states, gridIn, importState, rc=localrc)
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_g2x_fluxes, gridIn, importState, rc=localrc)

    ! wav
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_w2x_states, gridIn, importState, rc=localrc)
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_w2x_fluxes, gridIn, importState, rc=localrc)


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Create and Realize Exportable fields
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! ocn
    call ESMF_StateGet(exportState, itemName="NestedState-OCN", &
       nestedState=ocnExportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(ocn_import_fields, ocn(1)%import_state, 'x2d', &
         ocnExportState, ocn_present, &
         tag='MED->OCN', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    ! atm
    call ESMF_StateGet(exportState, itemName="NestedState-ATM", &
       nestedState=atmExportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(atm_import_fields, atm(1)%import_state, 'x2d', &
         atmExportState, atm_present, &
         tag='MED->ATM', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    ! lnd
    call ESMF_StateGet(exportState, itemName="NestedState-LND", &
       nestedState=lndExportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(lnd_import_fields, lnd(1)%import_state, 'x2d', &
         lndExportState, lnd_present, &
         tag='MED->LND', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    ! ice
    call ESMF_StateGet(exportState, itemName="NestedState-ICE", &
       nestedState=iceExportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(ice_import_fields, ice(1)%import_state, 'x2d', &
         iceExportState, ice_present, &
         tag='MED->ICE', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    ! rof
    call ESMF_StateGet(exportState, itemName="NestedState-ROF", &
       nestedState=rofExportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfshr_nuopc_create_fields(rof_import_fields, rof(1)%import_state, 'x2d', &
         rofExportState, rof_present, &
         tag='MED->ROF', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return ! bail out

    ! glc
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_x2g_states, gridOut, exportState, rc=localrc)
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_x2g_fluxes, gridOut, exportState, rc=localrc)

    ! wav
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_x2w_states, gridOut, exportState, rc=localrc)
!    call esmfshr_nuopc_create_fields( &
!       seq_flds_x2w_fluxes, gridOut, exportState, rc=localrc)

print *, 'KDS: Mediator InitializeP3 end'

  end subroutine

#else

  subroutine InitializeP3(mediator, importState, exportState, clock, rc)
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    ! and remove Fields that are not connected
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call checkConnectedFlagProvide(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call checkConnectedFlagProvide(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    subroutine checkConnectedFlagProvide(state, rc)
      ! Look at all of the fields in state, including in nested states. Error
      ! out if a connected field is found for which geom object must be 
      ! provided here. Remove all not connected fields.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      character(len=80)                       :: stateName
      type(ESMF_Field)                        :: field
      character(len=80)                       :: connectedValue
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    
      if (present(rc)) rc = ESMF_SUCCESS
    
      call ESMF_StateGet(state, name=stateName, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
    
      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_FieldAttributeGet(field, name="Connected", &
            value=connectedValue, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (connectedValue=="false") then
            ! remove the field from the state
            call ESMF_StateRemove(state, itemNameList(item), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          else
            call NUOPC_FieldAttributeGet(field, name="TransferActionGeomObject", &
              value=transferAction, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (trim(transferAction)=="provide") then
              ! the Connector instructed the Mediator to provide geom object
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Cannot fulfill request to provide geom object for "// &
                trim(itemNameList(item))//" in State "//trim(stateName), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
              return ! bail out
            endif
          endif
        endif
      enddo
      
      deallocate(itemNameList, itemTypeList)
    
    end subroutine

  end subroutine

#endif

  !-----------------------------------------------------------------------------

  subroutine InitializeP4(mediator, importState, exportState, clock, rc)
    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    call adjustAcceptedGeom(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call adjustAcceptedGeom(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    subroutine adjustAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Adjust
      ! the distribution of the accepted geom object to a 1 DE/PET distribution.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      type(ESMF_Field)                        :: field
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_GeomType_Flag)                :: geomtype
      type(ESMF_Grid)                         :: grid
      type(ESMF_Mesh)                         :: mesh
      character(160)                          :: msgString
      type(ESMF_DistGrid)                     :: distgrid
      integer                                 :: dimCount, tileCount, petCount
      integer                                 :: deCountPTile, extraDEs
      integer, allocatable                    :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable                    :: regDecompPTile(:,:)
      integer                                 :: i, j
    
      if (present(rc)) rc = ESMF_SUCCESS
      
      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
    
      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_FieldAttributeGet(field, name="TransferActionGeomObject", &
            value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (trim(transferAction)=="accept") then
            ! the Connector instructed the Mediator to accept geom object
            ! -> find out which type geom object the field holds
            call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (geomtype==ESMF_GEOMTYPE_GRID) then
              ! empty field holds a Grid with DistGrid
              call ESMF_FieldGet(field, grid=grid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! access the DistGrid
              call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.
              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                tileCount=tileCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                maxIndexPTile(dimCount, tileCount))
              ! get minIndex and maxIndex arrays
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! construct a default regDecompPTile -> TODO: move this into ESMF as default
              call ESMF_GridCompGet(mediator, petCount=petCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              allocate(regDecompPTile(dimCount, tileCount))
              deCountPTile = petCount/tileCount
              extraDEs = max(0, petCount-deCountPTile)
              do i=1, tileCount
                if (i<=extraDEs) then
                  regDecompPTile(1, i) = deCountPTile + 1
                else
                  regDecompPTile(1, i) = deCountPTile
                endif
                do j=2, dimCount
                  regDecompPTile(j, i) = 1
                enddo
              enddo
              ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
              ! but with a default regDecompPTile
              distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a new Grid on the new DistGrid and swap it in the Field
              grid = ESMF_GridCreate(distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! local clean-up
              deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)
            elseif (geomtype==ESMF_GEOMTYPE_MESH) then
              ! empty field holds a Mesh with DistGrid
              call ESMF_FieldGet(field, mesh=mesh, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! access the DistGrid
              call ESMF_MeshGet(mesh, elementDistgrid=distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.
              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                tileCount=tileCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                maxIndexPTile(dimCount, tileCount))
              ! get minIndex and maxIndex arrays
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! construct a default regDecompPTile -> TODO: move this into ESMF as default
              call ESMF_GridCompGet(mediator, petCount=petCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              allocate(regDecompPTile(dimCount, tileCount))
              deCountPTile = petCount/tileCount
              extraDEs = max(0, petCount-deCountPTile)
              do i=1, tileCount
                if (i<=extraDEs) then
                  regDecompPTile(1, i) = deCountPTile + 1
                else
                  regDecompPTile(1, i) = deCountPTile
                endif
                do j=2, dimCount
                  regDecompPTile(j, i) = 1
                enddo
              enddo
              ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
              ! but with a default regDecompPTile
              distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a new Grid on the new DistGrid and swap it in the Field
              mesh = ESMF_MeshCreate(distgrid, distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)    
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! local clean-up
              deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)
            else
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Unsupported geom object found in "// &
                trim(itemNameList(item)), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
              return ! bail out
            endif
          endif
        endif
      enddo
      
      deallocate(itemNameList, itemTypeList)
    
    end subroutine
    
  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine InitializeP5(mediator, importState, exportState, clock, rc)
    ! IPDv03p5: realize all Fields with transfer action "accept"
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)              :: field

    rc = ESMF_SUCCESS

    call realizeWithAcceptedGeom(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call realizeWithAcceptedGeom(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! -> Now that all fields are realized on the transferred Grids/Meshes, it
    ! -> is the time to issue RegridStore() calls. 
    ! -> Code should inspected itemCount from the fr/to States to be robust 
    ! -> wrt absent model components.
    
    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    subroutine realizeWithAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Realize
      ! with the accepted and adjusted geom object.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      type(ESMF_Field)                        :: field
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    
      if (present(rc)) rc = ESMF_SUCCESS
      
      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
    
      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_FieldAttributeGet(field, name="TransferActionGeomObject", &
            value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (trim(transferAction)=="accept") then
            ! the Connector instructed the Mediator to accept geom object
            ! the transferred geom object is already set, allocate memory for data by complete
            call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          endif
        endif
      enddo
      
      deallocate(itemNameList, itemTypeList)
    
    end subroutine

  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine DataInitialize(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(mediator, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Advance begin'

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the
    ! mediators Clock.

    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.

    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.

    call NUOPC_ClockPrintCurrTime(clock, &
      "-------->MED Advance() mediating for: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_ClockPrintStopTime(clock, &
      "----------------> model time step to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

print *, 'KDS: Mediator Advance end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_UpdateOrbitalData(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    character(CL)           :: orb_mode   ! orbital mode

    integer  :: orb_iyear       ! orbital year
    integer  :: orb_iyear_align ! associated with model year
    integer  :: orb_nyear       ! orbital year assoc. with currrent model year
    real(r8) :: orb_eccen       ! orbital eccentricity
    real(r8) :: orb_obliq       ! obliquity in degrees
    real(r8) :: orb_mvelp       ! moving vernal equinox long
    real(r8) :: orb_obliqr      ! Earths obliquity in rad
    real(r8) :: orb_lambm0      ! Mean long of perihelion at vernal equinox (radians)
    real(r8) :: orb_mvelpp      ! moving vernal equinox long

    integer  :: ymd             ! Current date (YYYYMMDD)
    integer  :: year            ! Current date (YYYY)
    integer  :: month           ! Current date (MM)
    integer  :: day             ! Current date (DD)



    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 2 begin'
print *, 'KDS: Update orbital data'

    infodata = ccsm_get_infodata()

    call seq_infodata_getData(infodata,   &
        orb_iyear=orb_iyear,             &
        orb_iyear_align=orb_iyear_align, &
        orb_mode=orb_mode,               &
        orb_eccen=orb_eccen,             &
        orb_obliqr=orb_obliqr,           &
        orb_lambm0=orb_lambm0,           &
        orb_mvelpp=orb_mvelpp)

    if (trim(orb_mode) == trim(seq_infodata_orb_variable_year)) then

        call seq_timemgr_EClockGetData(ccsm_EClock_d, curr_ymd=ymd)
        call shr_cal_date2ymd(ymd, year, month, day)

        orb_nyear = orb_iyear + (year - orb_iyear_align)

        !! In the original code, we check to see of orb_nyear not equal to
        !! orb_cyear... but it looks like orb_cyear was calculated the same 
        !! way as orb_nyear... so it appears that it's just letting us skip
        !! doing this calculation again if it's not necessary.
        !! However, in order for us to know if this value has changed, we'd
        !! have to keep track of the orb_cyear, which is not a global variable.
        !! So, we're just going to do this calculation again whether it's
        !! needed or not.
        !! So there.
        !!if (orb_nyear /= orb_cyear) then

            call shr_orb_params(orb_nyear, orb_eccen, orb_obliq, orb_mvelp, &
                 orb_obliqr, orb_lambm0, orb_mvelpp, .false.)

            call seq_infodata_putData(infodata, &
                 orb_eccen=orb_eccen, &
                 orb_obliqr=orb_obliqr, &
                 orb_lambm0=orb_lambm0, &
                 orb_mvelpp=orb_mvelpp)

         !!endif

      endif


print *, 'KDS: Mediator Run 2 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepOCN_ICE(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: ocn_prognostic
    logical  :: atm_present
    logical  :: ice_prognostic
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 3 begin'
print *, 'KDS: OCN/ICE Prep'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        atm_present=atm_present, &
        ice_prognostic=ice_prognostic, &
        ocn_prognostic=ocn_prognostic)

    ! if (atm_c2_ocn .or. atm_c2_ice)
    if (atm_present .and. (ocn_prognostic .or. ice_prognostic)) then

       call MediatorBarrier(rc=localrc)

       call prep_ocn_calc_a2x_ox(atm, timer='driver_ocnprep_atm2ocn')

    end if

print *, 'KDS: Mediator Run 3 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepOCN(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: ocn_present
    logical  :: ocn_prognostic
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 4 begin'
print *, 'KDS: OCN Prep'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ocn_present=ocn_present, &
        ocn_prognostic=ocn_prognostic, &
        info_debug=info_debug)

    if (ocn_present  .and.  ocnrun_alarm) then

      if (ocn_prognostic) then

         call MediatorBarrier(rc=localrc)

         call prep_ocn_accum_avg(ocn, timer_accum='driver_ocnprep_avg')

         call component_diag(infodata, ocn, flow='x2c', comment='send ocn', &
                   info_debug=info_debug, timer_diag='driver_ocnprep_diagav')

      endif
      call esmfshr_nuopc_copy(ocn_import_fields, ocn(1)%x2c_cc, ocn(1)%import_state, tag="OCN", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    endif

print *, 'KDS: Mediator Run 4 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepLND(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: lnd_present
    logical  :: lnd_prognostic
    logical  :: atm_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 5 begin'
print *, 'KDS: LND Prep'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        lnd_present=lnd_present, &
        atm_present=atm_present, &
        lnd_prognostic=lnd_prognostic, &
        info_debug=info_debug)

    ! lnd prep
    if (lnd_present  .and.  lndrun_alarm) then

       call MediatorBarrier(rc=localrc)

       if (lnd_prognostic) then

          ! if atm_c2_lnd
          if (atm_present) then
             call prep_lnd_calc_a2x_lx(atm, timer='driver_lndprep_atm2lnd')
          end if

          call prep_lnd_mrg(infodata, lnd, timer_mrg='driver_lndprep_mrgx2l')

          call component_diag(infodata, lnd, flow='x2c', comment='send lnd', &
               info_debug=info_debug, timer_diag='driver_lndprep_diagav')

       end if

    end if

print *, 'KDS: Mediator Run 5 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepICE(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    type(mct_aVect) , pointer :: a2x_ox(:)
    logical  :: ice_present
    logical  :: ice_prognostic
    logical  :: atm_present
    logical  :: ocn_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 6 begin'
print *, 'KDS: ICE Prep'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ice_present=ice_present, &
        atm_present=atm_present, &
        ocn_present=ocn_present, &
        ice_prognostic=ice_prognostic, &
        info_debug=info_debug)

    ! ice prep
    if (ice_present  .and.  icerun_alarm) then

       if (ice_prognostic) then

          call MediatorBarrier(rc=localrc)

          ! if ocn_c2_ice
          if (ocn_present) then
             call prep_ice_calc_o2x_ix(ocn, timer='driver_iceprep_ocn2ice')
          end if

          ! if atm_c2_ice
          if (atm_present) then
             a2x_ox => prep_ocn_get_a2x_ox() ! array
             call prep_ice_calc_a2x_ix(a2x_ox, timer='driver_iceprep_atm2ice')
          end if

          call prep_ice_mrg(infodata, ice, timer_mrg='driver_iceprep_mrgx2i')

          call component_diag(infodata, ice, flow='x2c', comment='send ice', &
               info_debug=info_debug, timer_diag='driver_iceprep_diagav')

       end if

    end if

print *, 'KDS: Mediator Run 6 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepWAV(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: wav_present
    logical  :: wav_prognostic
    logical  :: atm_present
    logical  :: ocn_present
    logical  :: ice_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 7 begin'
print *, 'KDS: WAV Prep'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        wav_present=wav_present, &
        atm_present=atm_present, &
        ocn_present=ocn_present, &
        ice_present=ice_present, &
        wav_prognostic=wav_prognostic, &
        info_debug=info_debug)

    ! wav prep
    if (wav_present  .and.  wavrun_alarm) then

       if (wav_prognostic) then

          call MediatorBarrier(rc=localrc)

          ! if atm_c2_wav
          if (atm_present) then
             call prep_wav_calc_a2x_wx(atm, timer='driver_wavprep_atm2wav')
          end if

          ! if ocn_c2_wav
          if (ocn_present) then
             call prep_wav_calc_o2x_wx(ocn, timer='driver_wavprep_ocn2wav')
          end if

          ! if ice_c2_wav
          if (ice_present) then
             call prep_wav_calc_i2x_wx(ice, timer='driver_wavprep_ice2wav')
          end if

          call prep_wav_mrg(infodata, wav, &
               fractions_wx, timer_mrg='driver_wavprep_mrgx2w')

          call component_diag(infodata, wav, flow='x2c', comment= 'send wav', &
               info_debug=info_debug, timer_diag='driver_wavprep_diagav')

       end if

    end if


print *, 'KDS: Mediator Run 7 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepROF(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: rof_present
    logical  :: rof_prognostic
    logical  :: lnd_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 8 begin'
print *, 'KDS: ROF Prep'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        rof_present=rof_present, &
        lnd_present=lnd_present, &
        rof_prognostic=rof_prognostic, &
        info_debug=info_debug)

    ! rof prep
    if (rof_present  .and.  rofrun_alarm) then

       if (rof_prognostic) then

          call MediatorBarrier(rc=localrc)

          call prep_rof_accum_avg(timer='driver_rofprep_l2xavg')

          if (lnd_present) then
             call prep_rof_calc_l2r_rx(fractions_lx=fractions_lx, &
                                       timer='driver_rofprep_lnd2rof')
          end if

          call prep_rof_mrg(infodata, rof, &
                 fractions_rx=fractions_rx, timer_mrg='driver_rofprep_mrgx2r')

          call component_diag(infodata, rof, flow='x2c', comment= 'send rof', &
                 info_debug=info_debug, timer_diag='driver_rofprep_diagav')

       end if

    end if

print *, 'KDS: Mediator Run 8 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostOCN_Tight(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: ocean_tight_coupling
    logical  :: ocn_present
    logical  :: ice_present
    logical  :: wav_present
    logical  :: ocn_prognostic
    logical  :: ice_c2_ocn
    logical  :: wav_c2_ocn
    integer  :: info_debug = 0
    character(CS) :: aoflux_grid       ! grid for a/o flux calc: atm xor ocn
    integer  :: localrc
type(mct_avect) :: x2c_cx_ptr

    integer :: eai, eli, eoi, eii, egi, eri, ewi, exi, efi  


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 9 begin'
print *, 'KDS: Post OCN v1'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ocean_tight_coupling=ocean_tight_coupling, &
        wav_present=wav_present, &
        ice_present=ice_present, &
        ocn_present=ocn_present, &
        ocn_prognostic=ocn_prognostic, &
        aoflux_grid=aoflux_grid, &
        info_debug=info_debug)

    !! 
    !! This section was grouped with the exchange (ocn->cpl) section, but
    !! this belongs in the mediator and since it is a "post ocn" operation,
    !! I think it's good to put it here
    !! 
    if (ocean_tight_coupling) then

       if (ocn_present .and. ocnnext_alarm) then

          call MediatorBarrier(rc=localrc)

          call component_diag(infodata, ocn, flow='c2x', comment= 'recv ocn', &
             info_debug=info_debug, timer_diag='driver_ocnpostt_diagav')

       end if

    end if

    !! 
    !! This section was under the comments "OCN PREP"
    !! 
    if (ice_present  .and.  ocn_prognostic) then
        ice_c2_ocn = .true.
    else
        ice_c2_ocn = .false.
    end if

    if (wav_present  .and.  ocn_prognostic) then
        wav_c2_ocn = .true.
    else
        wav_c2_ocn = .false.
    end if

    if (ocn_present) then

       call MediatorBarrier(rc=localrc)

       ! Compute atm/ocn fluxes (virtual "recv" from ocn) on
       ! ocn grid or exchange grid

       do exi = 1,num_inst_xao
          eai = mod((exi-1),num_inst_atm) + 1
          eoi = mod((exi-1),num_inst_ocn) + 1
          efi = mod((exi-1),num_inst_frc) + 1

          if (trim(aoflux_grid) == 'ocn') then
             a2x_ox => prep_ocn_get_a2x_ox()
             xao_ox => prep_aoflux_get_xao_ox()

             call seq_flux_atmocn_mct(infodata, &
                 ocn(eoi), a2x_ox(eai), 'ocn', xao_ox(exi))

          else if (trim(aoflux_grid) == 'atm') then
             !--- compute later ---

          else if (trim(aoflux_grid) == 'exch') then
             xao_ax   => prep_aoflux_get_xao_ax()
             xao_ox   => prep_aoflux_get_xao_ox()

             call seq_flux_atmocnexch_mct( infodata, atm(eai), ocn(eoi), &
                  fractions_ax(efi), fractions_ox(efi), xao_ax(exi), &
                  xao_ox(exi) )
          endif  ! aoflux_grid
       enddo

       ! Compute ocean inputs

       if (ocn_prognostic) then
          ! Map ice to ocn
          if (ice_c2_ocn) then
              call prep_ocn_calc_i2x_ox(ice, timer='driver_atmocnp_ice2ocn')
          endif

          ! Map wav to ocn
          if (wav_c2_ocn) then
              call prep_ocn_calc_w2x_ox(wav, timer='driver_atmocnp_wav2ocn')
          endif

          ! Merge ocn inputs
          xao_ox => prep_aoflux_get_xao_ox()
          call prep_ocn_mrg(infodata, ocn, fractions_ox=fractions_ox, &
              xao_ox=xao_ox, timer_mrg='driver_atmocnp_mrgx2o')

          ! Accumulate ocn inputs - form partial sum of tavg ocn inputs 
          ! (virtual "send" to ocn)
          call prep_ocn_accum(ocn, timer='driver_atmocnp_accum')
       end if

       ! Compute ocean albedos (MUST BE AFTER prep_ocn_mrg for swnet to 
       ! ocn to be computed properly
       do exi = 1,num_inst_xao
          eoi = mod((exi-1),num_inst_ocn) + 1
          efi = mod((exi-1),num_inst_frc) + 1

          if ((trim(aoflux_grid) == 'ocn') .or.  &
              (trim(aoflux_grid) == 'exch')) then

             xao_ox => prep_aoflux_get_xao_ox()   ! array over all instances
             call seq_flux_ocnalb_mct(infodata, &
                 ocn(1), fractions_ox(efi), xao_ox(exi))

          else if (trim(aoflux_grid) == 'atm') then
             !--- compute later ---
          end if
       end do

    end if

print *, 'KDS: Mediator Run 9 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostLND(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: lnd_present
    logical  :: rof_prognostic
    logical  :: glc_prognostic
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 10 begin'
print *, 'KDS: Post LND'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        lnd_present=lnd_present, &
        rof_prognostic=rof_prognostic, &
        glc_prognostic=glc_prognostic, &
        info_debug=info_debug)

    if (lnd_present  .and.  lndrun_alarm) then

       call MediatorBarrier(rc=localrc)

       call component_diag(infodata, lnd, flow='c2x', comment='recv lnd', &
               info_debug=info_debug, timer_diag='driver_lndpost_diagav')

       if (rof_prognostic) then
          call prep_rof_accum(lnd, timer='driver_lndpost_accl2r')
       end if

       if (glc_prognostic) then
          call prep_glc_accum(lnd, timer='driver_lndpost_accl2g')
       end if

    end if

print *, 'KDS: Mediator Run 10 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepGLC(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: glc_present
    logical  :: glc_prognostic
    logical  :: lnd_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 11 begin'
print *, 'KDS: Prep GLC'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        glc_present=glc_present, &
        lnd_present=lnd_present, &
        glc_prognostic=glc_prognostic, &
        info_debug=info_debug)

    ! glc prep
    if (glc_present  .and.  glcrun_alarm) then

       if (glc_prognostic) then

          call MediatorBarrier(rc=localrc)

          if (lnd_present) then

             call prep_glc_accum_avg(timer='driver_glcprep_avg')

             ! Note that l2x_gx is obtained from mapping the module variable
             ! l2gacc_lx
             call prep_glc_calc_l2x_gx(timer='driver_glcprep_lnd2glc')
             call prep_glc_mrg(infodata, glc, &
                timer_mrg='driver_glcprep_mrgx2g', &
                timer_diag='driver_glcprep_diagav')

          end if

       end if

    end if

print *, 'KDS: Mediator Run 11 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostROF(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    character(len=CL) :: suffix
    logical  :: do_hist_r2x
    logical  :: rof_present
    integer  :: rof_nx, rof_ny
    logical  :: lnd_prognostic
    logical  :: ocnrof_prognostic
    logical  :: rofice_present
    logical  :: iceberg_prognostic
    integer  :: info_debug = 0
    integer  :: eri
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 12 begin'
print *, 'KDS: Post ROF'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata,        &
        histaux_r2x=do_hist_r2x,               &
        rof_present=rof_present,               &
        rof_nx=rof_nx,                         &
        rof_ny=rof_ny,                         &
        lnd_prognostic=lnd_prognostic,         &
        ocnrof_prognostic=ocnrof_prognostic,   &
        rofice_present=rofice_present,         &
        iceberg_prognostic=iceberg_prognostic, &
        info_debug=info_debug)

    ! rof -> cpl
    if (rof_present  .and.  rofrun_alarm) then

       call MediatorBarrier(rc=localrc)

       call component_diag(infodata, rof, flow='c2x', comment= 'recv rof', &
               info_debug=info_debug, timer_diag='driver_rofpost_diagav')

       if (do_hist_r2x) then
          do eri = 1,num_inst_rof
             suffix =  component_get_suffix(rof(eri))
             call seq_hist_writeaux(infodata, ccsm_EClock_d, rof(eri), flow='c2x', &
                aname='r2x'//trim(suffix), dname='domrb', &
                nx=rof_nx, ny=rof_ny, nt=1)
          enddo
       end if

       if (lnd_prognostic) then
          call prep_lnd_calc_r2x_lx(rof, timer='driver_rofpost_rof2lnd')
       endif

       if (rofice_present .and. iceberg_prognostic) then
          call prep_ice_calc_r2x_ix(rof, timer='driver_rofpost_rof2ice')
       endif

       if (ocnrof_prognostic) then
          call prep_ocn_calc_r2x_ox(rof, timer='driver_rofpost_rof2ocn')
       endif

    end if

print *, 'KDS: Mediator Run 12 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_BudgetOldFractions(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical :: do_budgets              ! heat/water budgets on
    logical :: lnd_present
    logical :: rof_present
    logical :: ocn_present
    logical :: ice_present
    integer :: localrc

    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 13 begin'
print *, 'KDS: Budget with old fractions'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        do_budgets=do_budgets,          &
        lnd_present=lnd_present,        &
        rof_present=rof_present,        &
        ocn_present=ocn_present,        &
        ice_present=ice_present)

    if (do_budgets) then

        call MediatorBarrier(rc=localrc)

        if (lnd_present) then
            call seq_diag_lnd_mct(lnd(1), fractions_lx(1), &
                 do_l2x=.true., do_x2l=.true.)
         endif

         if (rof_present) then
            call seq_diag_rof_mct(rof(1), fractions_rx(1))
         endif

         if (ocn_present) then
            xao_ox => prep_aoflux_get_xao_ox() ! array over all instances
            call seq_diag_ocn_mct(ocn(1), xao_ox(1), fractions_ox(1), &
                 do_o2x=.true., do_x2o=.true., do_xao=.true.)
         endif

         if (ice_present) then
            call seq_diag_ice_mct(ice(1), fractions_ix(1), do_x2i=.true.)
         endif

    endif

print *, 'KDS: Mediator Run 13 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostICE(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: ice_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 14 begin'
print *, 'KDS: Post ICE'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ice_present=ice_present, &
        info_debug=info_debug)

    ! ice -> cpl
    if (ice_present  .and.  icerun_alarm) then

       call MediatorBarrier(rc=localrc)

       call component_diag(infodata, ice, flow='c2x', comment= 'recv ice', &
               info_debug=info_debug, timer_diag='driver_icepost_diagav')

    end if

print *, 'KDS: Mediator Run 14 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_UpdateFractionsFromICE(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    integer :: efi
    integer :: eii
    integer :: localrc

    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 15 begin'
print *, 'KDS: Update fractions based on ice fractions'

    call MediatorBarrier(rc=localrc)

    infodata = ccsm_get_infodata()

    do efi = 1,num_inst_frc

        eii = mod((efi-1),num_inst_ice) + 1

        call seq_frac_set(infodata, ice(eii), &
            fractions_ax(efi), fractions_ix(efi), fractions_ox(efi))

    end do

print *, 'KDS: Mediator Run 15 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_FluxCalcOnATM_Grid(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: atm_present
    logical  :: ocn_present
    logical  :: ocn_c2_atm
    logical  :: atm_c2_ocn
    character(CS) :: aoflux_grid       ! grid for a/o flux calc: atm xor ocn
    integer  :: eai, eoi, exi, efi  
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 16 begin'
print *, 'KDS: ATM/OCN Flux calc on ATM Grid with New Fractions'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        atm_present=atm_present, &
        ocn_present=ocn_present, &
        aoflux_grid=aoflux_grid)

    if (ocn_present) then

        if (atm_present) then
           ocn_c2_atm = .true.
           atm_c2_ocn = .true.
        endif 

        ! Compute atm/ocn fluxes (virtual "recv" from ocn)
        if (trim(aoflux_grid) == 'atm') then

            call MediatorBarrier(rc=localrc)

            if (ocn_c2_atm)  then
               call prep_atm_calc_o2x_ax(ocn, fractions_ox=fractions_ox, &
                    timer='driver_atmocnq_ocn2atm12')

               do exi = 1,num_inst_xao
                  eai = mod((exi-1),num_inst_atm) + 1
                  eoi = mod((exi-1),num_inst_ocn) + 1
                  efi = mod((exi-1),num_inst_frc) + 1

                  xao_ax => prep_aoflux_get_xao_ax() ! array over all instances
                  o2x_ax => prep_atm_get_o2x_ax()    ! array over all instances
                  call seq_flux_atmocn_mct(infodata, &
                      atm(1), o2x_ax(eoi), 'atm', xao_ax(exi))

                  xao_ox => prep_aoflux_get_xao_ox() ! array over all instances
                  call seq_flux_ocnalb_mct(infodata, &
                      ocn(1), fractions_ox(efi), xao_ox(exi))
               enddo
            end if

            if (atm_c2_ocn) then
               call prep_aoflux_calc_xao_ox(timer='driver_atmocnq_atm2ocnf')
            end if

        end if

    endif

print *, 'KDS: Mediator Run 16 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PrepATM(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    character(CS) :: aoflux_grid
    logical  :: atm_present
    logical  :: lnd_present
    logical  :: ice_present
    logical  :: ocn_present
    logical  :: atm_prognostic
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 17 begin'
print *, 'KDS: ATM Prep'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        aoflux_grid=aoflux_grid, &
        atm_present=atm_present, &
        lnd_present=lnd_present, &
        ice_present=ice_present, &
        ocn_present=ocn_present, &
        atm_prognostic=atm_prognostic, &
        info_debug=info_debug)

    ! atm prep
    if (atm_present  .and.  atmrun_alarm) then

       if (atm_prognostic) then

          call MediatorBarrier(rc=localrc)

          if (ocn_present) then

             if (trim(aoflux_grid) == 'ocn') then
                call prep_aoflux_calc_xao_ax(fractions_ox=fractions_ox, &
                    flds='states_and_fluxes', timer='driver_init_atminit')
             end if

             if (trim(aoflux_grid) == 'ocn' .or.  &
                 trim(aoflux_grid) == 'exch') then
                ! Get ocn output on atm grid
                call prep_atm_calc_o2x_ax(ocn, fractions_ox=fractions_ox, &
                     timer='driver_atmocnq_atm2ocn12')
             end if

             call prep_aoflux_calc_xao_ax(fractions_ox=fractions_ox, &
                  flds='albedos', timer='driver_atmocnq_atm2ocnb')

          end if

          if (ice_present) then
             call prep_atm_calc_i2x_ax(ice, fractions_ix, &
                  timer='driver_atmprep_ice2atm')
          end if

          if (lnd_present) then
             call prep_atm_calc_l2x_ax(lnd, fractions_lx, &
                  timer='driver_atmprep_lnd2atm')
          end if

          ! Merge inputs to atm
          if (associated(xao_ax)) then
             call prep_atm_mrg(infodata, atm, &
                 fractions_ax=fractions_ax, xao_ax=xao_ax, &
                 timer_mrg='driver_atmprep_mrgx2a')
          end if

          call component_diag(infodata, atm, flow='x2c', comment= 'send atm', &
               info_debug=info_debug, timer_diag='driver_atmprep_diagav')

       end if

    end if

print *, 'KDS: Mediator Run 17 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostWAV(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: wav_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 18 begin'
print *, 'KDS: Post WAV'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        wav_present=wav_present, &
        info_debug=info_debug)

    ! wav -> cpl
    if (wav_present  .and.  wavrun_alarm) then

       call MediatorBarrier(rc=localrc)

       call component_diag(infodata, wav, flow='c2x', comment= 'recv wav', &
               info_debug=info_debug, timer_diag='driver_wavpost_diagav')

    end if

print *, 'KDS: Mediator Run 18 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostGLC(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: glc_present
    logical  :: glclnd_present
    logical  :: glcocn_present
    logical  :: glcice_present
    logical  :: lnd_prognostic
    logical  :: ocn_prognostic
    logical  :: iceberg_prognostic
    integer  :: info_debug = 0
    integer  :: localrc

    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 19 begin'
print *, 'KDS: Post GLC'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        glc_present=glc_present, &
        glclnd_present=glclnd_present, &
        glcocn_present=glcocn_present, &
        glcice_present=glcice_present, &
        lnd_prognostic=lnd_prognostic, &
        ocn_prognostic=ocn_prognostic, &
        iceberg_prognostic=iceberg_prognostic, &
        info_debug=info_debug)

    if (glc_present  .and.  glcrun_alarm) then

       call MediatorBarrier(rc=localrc)

       call component_diag(infodata, glc, flow='c2x', comment= 'recv glc', &
              info_debug=info_debug, timer_diag='driver_glcpost_diagav')

       if (glclnd_present .and. lnd_prognostic) then
          call prep_lnd_calc_g2x_lx(glc, timer='driver_glcpost_glc2lnd')
       end if

       if (glcice_present .and. iceberg_prognostic) then
          call prep_ice_calc_g2x_ix(glc, timer='driver_glcpost_glc2ice')
       end if

       if (glcocn_present .and. ocn_prognostic) then
          call prep_ocn_calc_g2x_ox(glc, timer='driver_glcpost_glc2ocn')
       end if

    end if

print *, 'KDS: Mediator Run 19 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostATM(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: atm_present
    integer  :: info_debug = 0
    integer  :: localrc

    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 20 begin'
print *, 'KDS: Post ATM'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        atm_present=atm_present, &
        info_debug=info_debug)

    if (atm_present  .and.  atmrun_alarm) then

       call MediatorBarrier(rc=localrc)

       call component_diag(infodata, atm, flow='c2x', comment= 'recv atm', &
               info_debug=info_debug, timer_diag='driver_atmpost_diagav')

    end if

print *, 'KDS: Mediator Run 20 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_BudgetNewFractions(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical :: do_budgets              ! heat/water budgets on
    logical :: atm_present
    logical :: ice_present
    logical :: dead_comps
    integer :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 21 begin'
print *, 'KDS: Budget with new fractions'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        do_budgets=do_budgets,          &
        atm_present=atm_present,        &
        ice_present=ice_present,        &
        dead_comps=dead_comps)

    if (do_budgets) then

        call MediatorBarrier(rc=localrc)

        if (atm_present) then
            call seq_diag_atm_mct(atm(1), fractions_ax(1), &
                 do_a2x=.true., do_x2a=.true.)
        endif

        if (ice_present) then
            call seq_diag_ice_mct(ice(1), fractions_ix(1), do_i2x=.true.)
        endif

        call seq_diag_accum_mct()

        if (.not. dead_comps) then
!            call seq_diag_print_mct(clock, stop_alarm, budget_inst, &
!                 budget_daily, budget_month, budget_ann, budget_ltann, &
!                 budget_ltend)
        end if

        call seq_diag_zero_mct(EClock=ccsm_EClock_d)

    endif


print *, 'KDS: Mediator Run 21 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_PostOCN_Loose(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    logical  :: ocean_tight_coupling
    logical  :: ocn_present
    integer  :: info_debug = 0
    integer  :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 22 begin'
print *, 'KDS: Post OCN v2'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ocean_tight_coupling=ocean_tight_coupling, &
        ocn_present=ocn_present, &
        info_debug=info_debug)

    if (ocn_present  .and.  ocnnext_alarm) then

       if (.not. ocean_tight_coupling) then

          call MediatorBarrier(rc=localrc)

          call component_diag(infodata, ocn, flow='c2x', comment= 'recv ocn', &
                  info_debug=info_debug, timer_diag='driver_ocnpost_diagav')

       end if

    end if

print *, 'KDS: Mediator Run 22 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_SaveRestart(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    type(seq_timemgr_type)  :: seq_SyncClock_NotUsed
    integer                 :: localrc


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 23 begin'
print *, 'KDS: Save driver restart info'

    infodata = ccsm_get_infodata()

    if (restart_alarm) then

        call MediatorBarrier(rc=localrc)

        call seq_rest_write(ccsm_EClock_d, seq_SyncClock_NotUsed, infodata, &
            atm, lnd, ice, ocn, rof, glc, wav,                      &
            fractions_ax, fractions_lx, fractions_ix, fractions_ox, &
            fractions_rx, fractions_gx, fractions_wx)

    endif

print *, 'KDS: Mediator Run 23 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_WriteHistory(gcomp, importState, exportState, clock, rc)
    use seq_comm_mct, only: CPLID
    use seq_comm_mct, only: logunit
    use seq_comm_mct, only: seq_comm_getinfo => seq_comm_setptrs
   use shr_sys_mod,   only: shr_sys_flush

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    character(len=CL) :: suffix
    logical  :: do_hist_a2x             ! create aux files: a2x
    logical  :: do_hist_a2x3hr          ! create aux files: a2x 3hr states
    logical  :: do_hist_a2x3hrp         ! create aux files: a2x 3hr precip
    logical  :: do_hist_a2x24hr         ! create aux files: a2x
    logical  :: do_hist_l2x             ! create aux files: l2x
    logical  :: do_hist_l2x1yr          ! create aux files: l2x

    integer  :: atm_nx, atm_ny  ! nx, ny of 2d grid, if known
    integer  :: lnd_nx, lnd_ny
    integer  :: ice_nx, ice_ny
    integer  :: ocn_nx, ocn_ny
    integer  :: rof_nx, rof_ny
    integer  :: glc_nx, glc_ny
    integer  :: wav_nx, wav_ny
    integer  :: dtime            ! dt of one coupling interval
    integer  :: ymd              ! Current date (YYYYMMDD)
    integer  :: year             ! Current date (YYYY)
    integer  :: month            ! Current date (MM)
    integer  :: day              ! Current date (DD)
    integer  :: tod              ! Current time of day (seconds)
    integer  :: ncpl             ! number of coupling intervals per day
    integer  :: eai, eli
    logical  :: t3hr_alarm       ! alarm every three hours
    logical  :: t24hr_alarm      ! alarm every twentyfour hours
    logical  :: t1yr_alarm       ! alarm every year, at start of year
    integer  :: localrc

    logical       :: iamroot_CPLID     ! CPLID masterproc
    integer       :: info_debug = 0    ! local info_debug level
    character( 8) :: dstr              ! date string
    character(10) :: tstr              ! time string
    integer       :: begStep, endStep  ! Begining and ending step number
    real(r8)      :: simDays           ! Number of simulated days
    real(r8)      :: SYPD              ! Simulated years per day
    real(r8)      :: Time_begin        ! Start time
    real(r8)      :: Time_end          ! Ending time
    real(r8)      :: Time_bstep        ! Start time
    real(r8)      :: Time_estep        ! Ending time
    real(r8)      :: time_brun         ! Start time
    real(r8)      :: time_erun         ! Ending time
    real(r8)      :: cktime            ! delta time
    real(r8)      :: cktime_acc(10)    ! cktime accumulator array 1 = all, 2 = atm, etc
    integer       :: cktime_cnt(10)    ! cktime counter array
    character(CL) :: timing_file       ! Local path to tprof filename
    character(CL) :: timing_dir        ! timing directory
    character(CL) :: tchkpt_dir        ! timing checkpoint directory
type(mct_avect) :: x2c_cx_ptr


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 24 begin'
print *, 'KDS: Write history file'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata,  &
        histaux_a2x=do_hist_a2x,         &
        histaux_a2x3hr=do_hist_a2x3hr,   &
        histaux_a2x3hrp=do_hist_a2x3hrp, &
        histaux_a2x24hr=do_hist_a2x24hr, &
        histaux_l2x=do_hist_l2x,         &
        histaux_l2x1yr=do_hist_l2x1yr,   &
        atm_nx=atm_nx, atm_ny=atm_ny,    &
        lnd_nx=lnd_nx, lnd_ny=lnd_ny,    &
        rof_nx=rof_nx, rof_ny=rof_ny,    &
        ice_nx=ice_nx, ice_ny=ice_ny,    &
        glc_nx=glc_nx, glc_ny=glc_ny,    &
        ocn_nx=ocn_nx, ocn_ny=ocn_ny,    &
        wav_nx=wav_nx, wav_ny=wav_ny)


    call MediatorBarrier(rc=localrc)

    if ( history_alarm) then
        call seq_hist_write(infodata, ccsm_EClock_d, &
                 atm, lnd, ice, ocn, rof, glc, wav, &
                 fractions_ax, fractions_lx, fractions_ix, fractions_ox,     &
                 fractions_rx, fractions_gx, fractions_wx)
    endif

    !! Need to get histavg alarm
    if (do_histavg) then
        call seq_hist_writeavg(infodata, ccsm_EClock_d, &
            atm, lnd, ice, ocn, rof, glc, wav, .true.)
    endif

    call seq_timemgr_EClockGetData(ccsm_EClock_d, &
        dtime=dtime, curr_tod=tod, curr_ymd=ymd)
    ncpl = 86400/dtime

    t3hr_alarm = .false.
    if (mod(tod, 10800) == 0) then
        t3hr_alarm = .true.
    endif

    t24hr_alarm = .false.
    if (tod == 0) then
        t24hr_alarm = .true.
    endif

    t1yr_alarm = .false.
    call shr_cal_date2ymd(ymd, year, month, day)
    if (month==1 .and. day==1 .and. tod==0) then
        t1yr_alarm = .true.
    endif


    if (do_hist_a2x) then
        do eai = 1,num_inst_atm
            suffix =  component_get_suffix(atm(eai))
            if (trim(hist_a2x_flds) == 'all') then
                call seq_hist_writeaux(infodata, ccsm_EClock_d, atm(eai), flow='c2x', &
                    aname='a2x'//trim(suffix), dname='doma', &
                    nx=atm_nx, ny=atm_ny, nt=ncpl)
            else
                call seq_hist_writeaux(infodata, ccsm_EClock_d, atm(eai), flow='c2x', &
                    aname='a2x'//trim(suffix), dname='doma', &
                    nx=atm_nx, ny=atm_ny, nt=ncpl, flds=hist_a2x_flds)
            endif
        enddo
    endif

    if (do_hist_a2x3hr) then
        do eai = 1,num_inst_atm
            suffix =  component_get_suffix(atm(eai))
            if (trim(hist_a2x3hr_flds) == 'all') then
                call seq_hist_writeaux(infodata, ccsm_EClock_d, atm(eai), flow='c2x', &
                    aname='a2x3h'//trim(suffix), dname='doma', &
                    nx=atm_nx, ny=atm_ny, nt=8, write_now=t3hr_alarm)
            else
                call seq_hist_writeaux(infodata, ccsm_EClock_d, atm(eai), flow='c2x', &
                    aname='a2x3h'//trim(suffix), dname='doma', &
                    nx=atm_nx, ny=atm_ny, nt=8, write_now=t3hr_alarm, &
                    flds=hist_a2x3hr_flds)
            end if
        enddo
    endif

    if (do_hist_a2x3hrp) then
        do eai = 1,num_inst_atm
            suffix = component_get_suffix(atm(eai))
            if (trim(hist_a2x3hrp_flds) == 'all') then
                call seq_hist_writeaux(infodata, ccsm_EClock_d, atm(eai), flow='c2x', &
                    aname='a2x3h_prec'//trim(suffix), dname='doma', &
                    nx=atm_nx, ny=atm_ny, nt=8, write_now=t3hr_alarm)
            else
                call seq_hist_writeaux(infodata, ccsm_EClock_d, atm(eai), flow='c2x', &
                    aname='a2x3h_prec'//trim(suffix), dname='doma', &
                    nx=atm_nx, ny=atm_ny, nt=8, write_now=t3hr_alarm, &
                    flds=hist_a2x3hrp_flds)
            end if
        enddo
    endif

    if (do_hist_a2x24hr) then
        do eai = 1,num_inst_atm
            suffix = component_get_suffix(atm(eai))
            call seq_hist_writeaux(infodata, ccsm_EClock_d, atm(eai), flow='c2x', &
                aname='a2x1d'//trim(suffix), dname='doma', &
                nx=atm_nx, ny=atm_ny, nt=1, write_now=t24hr_alarm)
        enddo
    endif

    ! Need to get glcrun_alarm, but set to true for now
    !if (do_hist_l2x1yr .and. glcrun_alarm) then
    if (do_hist_l2x1yr) then
        ! Use yr_offset=-1 so the file with fields from year 1 has time stamp
        ! 0001-01-01 rather than 0002-01-01, etc.
        do eli = 1,num_inst_lnd
            suffix = component_get_suffix(lnd(eli))
            call seq_hist_writeaux(infodata, ccsm_EClock_d, lnd(eli), flow='c2x', &
                aname='l2x'//trim(suffix), dname='doml', &
                nx=lnd_nx, ny=lnd_ny, nt=1, write_now=t1yr_alarm, yr_offset=-1)
        enddo
    endif

    if (do_hist_l2x) then
        do eli = 1,num_inst_lnd
            suffix =  component_get_suffix(lnd(eli))
            call seq_hist_writeaux(infodata, ccsm_EClock_d, lnd(eli), flow='c2x', &
                aname='l2x'//trim(suffix), dname='doml', &
                nx=lnd_nx, ny=lnd_ny, nt=ncpl)
        enddo
    endif

    call seq_timemgr_EClockGetData( EClock_d, curr_ymd=ymd, curr_tod=tod )
    call seq_infodata_GetData(infodata, info_debug=info_debug)
    call seq_comm_getinfo(CPLID, iamroot=iamroot_CPLID)

 101  format( A, 2i8, 12A, A, F8.2, A, F8.2 )

    if (tod == 0 .or. info_debug > 1) then
       if (iamroot_CPLID) then
          call date_and_time(dstr,tstr)
          Time_estep = mpi_wtime()
          cktime = time_estep-time_bstep
          cktime_acc(1) = cktime_acc(1) + cktime
          cktime_cnt(1) = cktime_cnt(1) + 1
          write(logunit,101) ' tStamp_write: model date = ',ymd,tod, &
               ' wall clock = ',dstr(1:4),'-',dstr(5:6),'-',dstr(7:8),' ',&
               tstr(1:2),':',tstr(3:4),':',tstr(5:6), &
               ' avg dt = ',cktime_acc(1)/cktime_cnt(1),' dt = ',cktime 
          Time_bstep = mpi_wtime()
          call shr_sys_flush(logunit)
       endif
    endif

print *, 'KDS: Mediator Run 24 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_AdvanceSyncClock(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(seq_infodata_type) :: infodata
    integer  :: ymd                    ! Current date (YYYYMMDD)
    integer  :: tod                    ! Current time of day (seconds)
    integer  :: year                   ! Current date (YYYY)
    integer  :: month                  ! Current date (MM)
    integer  :: day                    ! Current date (DD)


    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 25 begin'
print *, 'KDS: Advance Sync Clock'

    infodata = ccsm_get_infodata()

      call seq_timemgr_clockAdvance( seq_SyncClock)
      call seq_timemgr_EClockGetData( EClock_d, curr_ymd=ymd, curr_tod=tod )
      call shr_cal_date2ymd(ymd,year,month,day)
      stop_alarm    = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_stop)
      atmrun_alarm  = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_atmrun)
      lndrun_alarm  = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_lndrun)
      rofrun_alarm  = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_rofrun)
      icerun_alarm  = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_icerun)
      glcrun_alarm  = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_glcrun)
      wavrun_alarm  = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_wavrun)
      ocnrun_alarm  = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_ocnrun)
      ocnnext_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_ocnnext)
      restart_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_restart)
      history_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_history)
      histavg_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_histavg)
      tprof_alarm   = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_tprof)

      ! this probably belongs in seq_timemgr somewhere using proper clocks
      t1hr_alarm = .false.
      t2hr_alarm = .false.
      t3hr_alarm = .false.
      t6hr_alarm = .false.
      t12hr_alarm = .false.
      t24hr_alarm = .false.
      t1yr_alarm = .false.
      if (mod(tod, 3600) == 0) t1hr_alarm = .true.
      if (mod(tod, 7200) == 0) t2hr_alarm = .true.
      if (mod(tod,10800) == 0) t3hr_alarm = .true.
      if (mod(tod,21600) == 0) t6hr_alarm = .true.
      if (mod(tod,43200) == 0) t12hr_alarm = .true.
      if (tod            == 0) t24hr_alarm = .true.
      if (month==1 .and. day==1 .and. tod==0) t1yr_alarm = .true.

      call seq_infodata_putData(infodata, glcrun_alarm=glcrun_alarm)

      if (seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_datestop)) then
!         if (iamroot_CPLID) then
!            write(logunit,*) ' '
!            write(logunit,103) subname,' NOTE: Stopping from alarm STOP DATE'
!            write(logunit,*) ' '
!         endif
         stop_alarm = .true.
      endif

print *, 'KDS: Mediator Run 25 end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine run_OverrideOcnAlarms(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables

    rc = ESMF_SUCCESS

print *, 'KDS: Mediator Run 26 begin'
print *, 'KDS: Override OCN Alarms'

      ! override ocnrun_alarm and ocnnext_alarm for first ocn run
      ! skip_ocean_run is initialized above to true if it's a startup
      ! if it's not a startup, ignore all of this
      ! stop the overide on the second ocnrun_alarm

      if (ocnrun_alarm) ocnrun_count = ocnrun_count + 1
print *, 'KDS: ocnrun_count = ', ocnrun_count
      if (ocnrun_count > 1) skip_ocean_run = .false.
print *, 'KDS: skip_ocean_run = ', skip_ocean_run
      if (skip_ocean_run) then
         ocnrun_alarm = .false.
         ocnnext_alarm = .false.
      endif

!      if (iamroot_CPLID) then
!         if (loglevel > 1) then
!            write(logunit,102) ' Alarm_state: model date = ',ymd,tod, &
!                 ' aliogrw run alarms = ',  atmrun_alarm, lndrun_alarm, &
!                 icerun_alarm, ocnrun_alarm, glcrun_alarm, &
!                 rofrun_alarm, wavrun_alarm
!            write(logunit,102) ' Alarm_state: model date = ',ymd,tod, &
!                 ' 1.2.3.6.12.24 run alarms = ',  t1hr_alarm, t2hr_alarm, &
!                 t3hr_alarm, t6hr_alarm, t12hr_alarm, t24hr_alarm
!            call shr_sys_flush(logunit)
!         endif
!      endif


print *, 'KDS: Mediator Run 26 end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine MediatorBarrier(rc)
    integer, intent(out)         :: rc

    ! local variables
    type(ESMF_VM)  :: vm
    integer        :: mpicom_dup, mpicom_vm
    integer        :: ierr

    rc = ESMF_SUCCESS

    ! duplicate the mpi communicator from the current VM
!    call ESMF_VMGetCurrent(vm, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

!    call ESMF_VMGet(vm, mpiCommunicator=mpicom_vm, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

!    call MPI_Comm_dup(mpicom_vm, mpicom_dup, rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

!    call MPI_Barrier(mpicom_dup, ierr)

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine TimestampExport(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc

    ! local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_State)      :: exportState
    type(ESMF_State)      :: importState
    character(ESMF_MAXSTR):: name
    character(ESMF_MAXSTR):: msgString
logical :: isValid
type(ESMF_Time) :: time


    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, clock=clock, &
      exportState=exportState, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! update timestamp on export Fields
call NUOPC_ClockPrintCurrTime(clock, ">>>"// &
   trim(name)//" in TimestampExport setting timestamp: ", msgString, rc=rc)
call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call NUOPC_StateSetTimestamp(exportState, clock, rc=rc)

call NUOPC_ClockPrintCurrTime(clock, ">>>"// &
   trim(name)//" in TimestampExport done setting timestamp: ", msgString, rc=rc)
call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

call ESMF_ClockGet(clock, currTime=time, rc=rc)
isValid = NUOPC_StateIsAtTime(exportState, time, rc=rc)
write (msgString, *) trim(name)//" State is at time? ", isValid
call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_atmPreRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: atm_present
    logical                 :: atm_prognostic
type(mct_avect) :: x2c_cx_ptr


    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2atm Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        atm_present=atm_present, &
        atm_prognostic=atm_prognostic)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, atm(1)%export_state, &
           id=atm(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put atmrun_alarm into export state
    call ESMF_AttributeSet(atm(1)%export_state, &
        name="atmrun_alarm", value=atmrun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    if (atm_present  .and.  atmrun_alarm) then

        ! Determine import state from input attribute vector on
!        call ESMF_StateGet(atm(1)%import_state, itemName="x2d", &
!            array=x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction factor from x2c on mct attribute vector
        if (atm_prognostic) then
           call mct_avect_vecmult(atm(1)%x2c_cc, atm(1)%drv2mdl, &
               seq_flds_x2a_fluxes) 
        end if 

        ! Convert mct attribute vector to esmf array
!        call mct2esmf_copy(atm(1)%x2c_cc, x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy to ESMF_Fields
call esmfshr_nuopc_copy(atm_import_fields, atm(1)%x2c_cc, atm(1)%import_state, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    end if

print *, 'KDS: Connector med2atm Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_atmPostRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: atm_present
type(mct_avect) :: x2c_cx_ptr


    rc = ESMF_SUCCESS

print *, 'KDS: Connector atm2med Post Run begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        atm_present=atm_present)

    if (atm_present  .and.  atmrun_alarm) then

        call ESMF_AttributeSet(atm(1)%export_state, name="ID", &
            value=atm(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is updated 
        ! in infodata
        call esmfshr_infodata_state2infodata(&
            atm(1)%export_state, infodata, oneletterid=atm(1)%oneletterid, &
            rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        call ccsm_put_infodata(infodata)

        ! Determine export state and obtain output esmf array
!        call ESMF_StateGet(atm(1)%export_state, itemName="d2x", &
!            array=d2x_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert output esmf array to mct attribute vector
!        call esmf2mct_copy(d2x_array, atm(1)%c2x_cc, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy from ESMF_Fields
call esmfshr_nuopc_copy(atm_export_fields, atm(1)%export_state, atm(1)%c2x_cc, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(atm(1)%c2x_cc, atm(1)%mdl2drv, &
            seq_flds_a2x_fluxes)

    end if ! atm present

print *, 'KDS: Connector atm2med Post Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_glcPreRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: glc_present
    logical                 :: glc_prognostic


    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2glc Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        glc_present=glc_present, &
        glc_prognostic=glc_prognostic)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, glc(1)%export_state, &
           id=glc(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put glcrun_alarm into export state
    call ESMF_AttributeSet(glc(1)%export_state, &
       name="glcrun_alarm", value=glcrun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    if (glc_present  .and.  glcrun_alarm) then

        ! Determine import state from input attribute vector on
        call ESMF_StateGet(glc(1)%import_state, itemName="x2d", &
            array=x2d_array, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction factor from x2c on mct attribute vector
        if (glc_prognostic) then
           call mct_avect_vecmult(glc(1)%x2c_cc, glc(1)%drv2mdl, &
               seq_flds_x2g_fluxes)
        end if

        ! Convert mct attribute vector to esmf array
        call mct2esmf_copy(glc(1)%x2c_cc, x2d_array, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    end if

print *, 'KDS: Connector med2glc Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_glcPostRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: glc_present


    rc = ESMF_SUCCESS

print *, 'KDS: Connector glc2med Post Run begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        glc_present=glc_present)

    if (glc_present  .and.  glcrun_alarm) then

        call ESMF_AttributeSet(glc(1)%export_state, name="ID", &
            value=glc(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is updated
        ! in infodata
        call esmfshr_infodata_state2infodata(&
            glc(1)%export_state, infodata, oneletterid=glc(1)%oneletterid, &
            rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Determine export state and obtain output esmf array
        call ESMF_StateGet(glc(1)%export_state, itemName="d2x", &
            array=d2x_array, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert output esmf array to mct attribute vector
        call esmf2mct_copy(d2x_array, glc(1)%c2x_cc, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(glc(1)%c2x_cc, glc(1)%mdl2drv, &
            seq_flds_g2x_fluxes)

    end if ! glc_present

print *, 'KDS: Connector glc2med Post Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_icePreRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: ice_present
    logical                 :: ice_prognostic
    integer                 :: n, i
    character(len=256), allocatable :: itemNameList(:)


    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2ice Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ice_present=ice_present, &
        ice_prognostic=ice_prognostic)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, ice(1)%export_state, &
           id=ice(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put icerun_alarm into export state 
    call ESMF_AttributeSet(ice(1)%export_state, &
       name="icerun_alarm", value=icerun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    if (ice_present  .and.  icerun_alarm) then

        ! Determine import state from input attribute vector on
!        call ESMF_StateGet(ice(1)%import_state, itemName="x2d", &
!            array=x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction factor from x2c on mct attribute vector
        if (ice_prognostic) then
           call mct_avect_vecmult(ice(1)%x2c_cc, ice(1)%drv2mdl, &
               seq_flds_x2i_fluxes)
        end if

        ! Convert mct attribute vector to esmf array
!        call mct2esmf_copy(ice(1)%x2c_cc, x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    !call ESMF_StateGet(ice(1)%import_state, itemCount=n, rc=rc)
    !if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    !allocate(itemNameList(n))
    !call ESMF_StateGet(ice(1)%import_state, itemNameList=itemNameList, rc=rc)
    !if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    !do i = 1, n
    !  call ESMF_LogWrite('CICE PRERUN import_state: ' // itemNameList(i), &
    !    ESMF_LOGMSG_INFO, &
    !    line=__LINE__, &
    !    file=__FILE__, &
    !    rc=rc)
    !enddo
    !do i = 1, size(ice_import_fields)
    !  call ESMF_LogWrite('CICE PRERUN ice_import_fields: ' // ice_import_fields(i), &
    !    ESMF_LOGMSG_INFO, &
    !    line=__LINE__, &
    !    file=__FILE__, &
    !    rc=rc)
    !enddo
    !deallocate(itemNameList)

    call esmfshr_nuopc_copy(ice_import_fields, ice(1)%x2c_cc, ice(1)%import_state, 'CICE', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)


    end if

print *, 'KDS: Connector med2ice Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_icePostRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: ice_present


    rc = ESMF_SUCCESS

print *, 'KDS: Connector ice2med Post Run begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ice_present=ice_present)

    if (ice_present  .and.  icerun_alarm) then

        call ESMF_AttributeSet(ice(1)%export_state, name="ID", &
            value=ice(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is updated
        ! in infodata
        call esmfshr_infodata_state2infodata(&
            ice(1)%export_state, infodata, oneletterid=ice(1)%oneletterid, &
            rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        call esmfshr_nuopc_copy(ice_export_fields, ice(1)%export_state, ice(1)%c2x_cc, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(ice(1)%c2x_cc, ice(1)%mdl2drv, &
            seq_flds_i2x_fluxes)

    end if

print *, 'KDS: Connector ice2med Post Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_lndPreRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: lnd_present
    logical                 :: lnd_prognostic


    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2lnd Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        lnd_present=lnd_present, &
        lnd_prognostic=lnd_prognostic)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, lnd(1)%export_state, &
           id=lnd(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put lndrun_alarm into export state
    call ESMF_AttributeSet(lnd(1)%export_state, &
       name="lndrun_alarm", value=lndrun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    if (lnd_present  .and.  lndrun_alarm) then

        ! Determine import state from input attribute vector on
!        call ESMF_StateGet(lnd(1)%import_state, itemName="x2d", &
!            array=x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction factor from x2c on mct attribute vector
        if (lnd_prognostic) then
           call mct_avect_vecmult(lnd(1)%x2c_cc, lnd(1)%drv2mdl, &
               seq_flds_x2l_fluxes)
        end if

        ! Convert mct attribute vector to esmf array
!        call mct2esmf_copy(lnd(1)%x2c_cc, x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy to ESMF_Fields
call esmfshr_nuopc_copy(lnd_import_fields, lnd(1)%x2c_cc, lnd(1)%import_state, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    end if

print *, 'KDS: Connector med2lnd Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_lndPostRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: lnd_present

    rc = ESMF_SUCCESS

print *, 'KDS: Connector lnd2med Post Run begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        lnd_present=lnd_present)

    if (lnd_present  .and.  lndrun_alarm) then

        call ESMF_AttributeSet(lnd(1)%export_state, name="ID", &
            value=lnd(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is updated
        ! in infodata
        call esmfshr_infodata_state2infodata(&
            lnd(1)%export_state, infodata, oneletterid=lnd(1)%oneletterid, &
            rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Determine export state and obtain output esmf array
!        call ESMF_StateGet(lnd(1)%export_state, itemName="d2x", &
!            array=d2x_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert output esmf array to mct attribute vector
!        call esmf2mct_copy(d2x_array, lnd(1)%c2x_cc, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy from ESMF_Fields
call esmfshr_nuopc_copy(lnd_export_fields, lnd(1)%export_state, lnd(1)%c2x_cc, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(lnd(1)%c2x_cc, lnd(1)%mdl2drv, &
            seq_flds_l2x_fluxes)

    end if

print *, 'KDS: Connector lnd2med Post Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_ocnPreRunTight(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: ocn_present
    logical                 :: ocn_prognostic
    logical                 :: ocean_tight_coupling


    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2ocn Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ocn_present=ocn_present, &
        ocn_prognostic=ocn_prognostic, &
        ocean_tight_coupling=ocean_tight_coupling)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, ocn(1)%export_state, &
               id=ocn(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put ocnrun_alarm into export state
    call ESMF_AttributeSet(ocn(1)%export_state, &
       name="ocnrun_alarm", value=ocnrun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put ocnrun_alarm into export state
    call ESMF_AttributeSet(ocn(1)%export_state, &
       name="skip_ocean_run", value=skip_ocean_run, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

print *, 'KDS: ocn_present, ocnrun_alarm (tight): ', ocn_present, ocnrun_alarm, ocean_tight_coupling
    if (ocn_present  .and.  ocnrun_alarm  .and.  ocean_tight_coupling) then

        ! Apply area correction factor from x2c on mct attribute vector
        if (ocn_prognostic) then
           call mct_avect_vecmult(ocn(1)%x2c_cc, ocn(1)%drv2mdl, &
               seq_flds_x2o_fluxes)
        end if

        call component_diag(infodata, ocn, flow='x2c', comment='send ocn tight', &
          info_debug=2, timer_diag='driver_ocnprep_diagav')


!! KDS: Copy to ESMF_Fields
print *, 'KDS: copying vect to field (tight)'
call esmfshr_nuopc_copy(ocn_import_fields, ocn(1)%x2c_cc, ocn(1)%import_state, tag="OCN TIGHT", rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    end if

print *, 'KDS: Connector med2ocn Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_ocnPostRunTight(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array


    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: ocean_tight_coupling
    logical  :: ocn_present

    rc = ESMF_SUCCESS

print *, 'KDS: Connector ocn2med Post Run Tight begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ocean_tight_coupling=ocean_tight_coupling, &
        ocn_present=ocn_present)

    if (ocn_present  .and.  ocnrun_alarm  .and.  ocean_tight_coupling) then

        call ESMF_AttributeSet(ocn(1)%export_state, name="ID", &
            value=ocn(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is 
        ! updated in infodata
        call esmfshr_infodata_state2infodata(&
            ocn(1)%export_state, infodata, oneletterid=ocn(1)%oneletterid, &
            rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Determine export state and obtain output esmf array
!        call ESMF_StateGet(ocn(1)%export_state, itemName="d2x", &
!            array=d2x_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert output esmf array to mct attribute vector
!        call esmf2mct_copy(d2x_array, ocn(1)%c2x_cc, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy from ESMF_Fields
call esmfshr_nuopc_copy(ocn_export_fields, ocn(1)%export_state, ocn(1)%c2x_cc, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(ocn(1)%c2x_cc, ocn(1)%mdl2drv, &
            seq_flds_o2x_fluxes)

    end if

print *, 'KDS: Connector ocn2med Post Run Tight end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_ocnPreRunLoose(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: ocn_present
    logical                 :: ocn_prognostic
    logical                 :: ocean_tight_coupling


    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2ocn Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ocn_present=ocn_present, &
        ocn_prognostic=ocn_prognostic, &
        ocean_tight_coupling=ocean_tight_coupling)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, ocn(1)%export_state, &
               id=ocn(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put ocnrun_alarm into export state
    call ESMF_AttributeSet(ocn(1)%export_state, &
       name="ocnrun_alarm", value=ocnrun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

print *, 'KDS: ocn_present, ocnrun_alarm (loose): ', ocn_present, ocnrun_alarm, ocean_tight_coupling
    if (ocn_present  .and.  ocnrun_alarm  .and. &
        (.not. ocean_tight_coupling)) then

        ! Apply area correction factor from x2c on mct attribute vector
        if (ocn_prognostic) then
           call mct_avect_vecmult(ocn(1)%x2c_cc, ocn(1)%drv2mdl, &
               seq_flds_x2o_fluxes)
        end if

          call component_diag(infodata, ocn, flow='x2c', comment='send ocn loose', &
                    info_debug=2, timer_diag='driver_ocnprep_diagav')

!! KDS: Copy to ESMF_Fields
print *, 'KDS: copying vect to field (loose)'
call esmfshr_nuopc_copy(ocn_import_fields, ocn(1)%x2c_cc, ocn(1)%import_state, tag="OCN LOOSE", rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    end if

print *, 'KDS: Connector med2ocn Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_ocnPostRunLoose(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array


    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: ocean_tight_coupling
    logical  :: ocn_present

    rc = ESMF_SUCCESS

print *, 'KDS: Connector ocn2med Post Run Loose begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        ocean_tight_coupling=ocean_tight_coupling, &
        ocn_present=ocn_present)

    if (ocn_present  .and.  ocnrun_alarm  .and.  &
        (.not. ocean_tight_coupling)) then

        call ESMF_AttributeSet(ocn(1)%export_state, name="ID", &
            value=ocn(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is 
        ! updated in infodata
        call esmfshr_infodata_state2infodata(&
            ocn(1)%export_state, infodata, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Determine export state and obtain output esmf array
!        call ESMF_StateGet(ocn(1)%export_state, itemName="d2x", &
!            array=d2x_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert output esmf array to mct attribute vector
!        call esmf2mct_copy(d2x_array, ocn(1)%c2x_cc, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy from ESMF_Fields
call esmfshr_nuopc_copy(ocn_export_fields, ocn(1)%export_state, ocn(1)%c2x_cc, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(ocn(1)%c2x_cc, ocn(1)%mdl2drv, &
            seq_flds_o2x_fluxes)

    end if

print *, 'KDS: Connector ocn2med Post Run Loose end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_rofPreRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: rof_present
    logical                 :: rof_prognostic

    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2rof Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        rof_present=rof_present, &
        rof_prognostic=rof_prognostic)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, rof(1)%export_state, &
           id=rof(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put rofrun_alarm into export state
    call ESMF_AttributeSet(rof(1)%export_state, &
       name="rofrun_alarm", value=rofrun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    if (rof_present  .and.  rofrun_alarm) then

        ! Determine import state from input attribute vector on
!        call ESMF_StateGet(rof(1)%import_state, itemName="x2d", &
!            array=x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction factor from x2c on mct attribute vector
        if (rof_prognostic) then
           call mct_avect_vecmult(rof(1)%x2c_cc, rof(1)%drv2mdl, &
               seq_flds_x2r_fluxes)
        end if

        ! Convert mct attribute vector to esmf array
!        call mct2esmf_copy(rof(1)%x2c_cc, x2d_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy to ESMF_Fields
call esmfshr_nuopc_copy(rof_import_fields, rof(1)%x2c_cc, rof(1)%import_state, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    end if

print *, 'KDS: Connector med2rof Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_rofPostRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: rof_present


    rc = ESMF_SUCCESS

print *, 'KDS: Connector rof2med Post Run begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        rof_present=rof_present)

    if (rof_present  .and.  rofrun_alarm) then

        call ESMF_AttributeSet(rof(1)%export_state, name="ID", &
            value=rof(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is updated
        ! in infodata
        call esmfshr_infodata_state2infodata(&
            rof(1)%export_state, infodata, oneletterid=rof(1)%oneletterid, &
            rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Determine export state and obtain output esmf array
!        call ESMF_StateGet(rof(1)%export_state, itemName="d2x", &
!            array=d2x_array, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert output esmf array to mct attribute vector
!        call esmf2mct_copy(d2x_array, rof(1)%c2x_cc, rc=rc)
!        if (rc /= ESMF_SUCCESS) &
!            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!! KDS: Copy from ESMF_Fields
call esmfshr_nuopc_copy(atm_export_fields, atm(1)%export_state, atm(1)%c2x_cc, rc=rc)
if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(rof(1)%c2x_cc, rof(1)%mdl2drv, &
            seq_flds_r2x_fluxes)

    end if

print *, 'KDS: Connector rof2med Post Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_wavPreRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: x2d_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical                 :: wav_present
    logical                 :: wav_prognostic

    rc = ESMF_SUCCESS

print *, 'KDS: Connector med2wav Pre Run begin'

    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        wav_present=wav_present, &
        wav_prognostic=wav_prognostic)

    ! Put infodata information into export state
    call esmfshr_infodata_infodata2state(infodata, wav(1)%export_state, &
           id=wav(1)%compid, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Put wavrun_alarm into export state
    call ESMF_AttributeSet(wav(1)%export_state, &
       name="wavrun_alarm", value=wavrun_alarm, rc=rc)
    if (rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    if (wav_present  .and.  wavrun_alarm) then

        ! Determine import state from input attribute vector on
        call ESMF_StateGet(wav(1)%import_state, itemName="x2d", &
            array=x2d_array, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction factor from x2c on mct attribute vector
        if (wav_prognostic) then
           call mct_avect_vecmult(wav(1)%x2c_cc, wav(1)%drv2mdl, &
               seq_flds_x2w_fluxes)
        end if

        ! Convert mct attribute vector to esmf array
        call mct2esmf_copy(wav(1)%x2c_cc, x2d_array, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    end if

print *, 'KDS: Connector med2wav Pre Run end'

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine routine_wavPostRun(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                  :: localrc
    type(ESMF_Array)         :: d2x_array

    ! CCSM variables
    type(seq_infodata_type) :: infodata
    logical  :: wav_present


    rc = ESMF_SUCCESS

print *, 'KDS: Connector wav2med Post Run begin'

    !!!
    !! Do stuff currently part of ccsm_run/component_run routine
    !!!
    infodata = ccsm_get_infodata()

    call seq_infodata_GetData(infodata, &
        wav_present=wav_present)

    if (wav_present  .and.  wavrun_alarm) then

        call ESMF_AttributeSet(wav(1)%export_state, name="ID", &
            value=wav(1)%compid, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert export state back to infodata, the new nextsw_cday is updated
        ! in infodata
        call esmfshr_infodata_state2infodata(&
            wav(1)%export_state, infodata, oneletterid=wav(1)%oneletterid, &
            rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Determine export state and obtain output esmf array
        call ESMF_StateGet(wav(1)%export_state, itemName="d2x", &
            array=d2x_array, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Convert output esmf array to mct attribute vector
        call esmf2mct_copy(d2x_array, wav(1)%c2x_cc, rc=rc)
        if (rc /= ESMF_SUCCESS) &
            call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

        ! Apply area correction for c2x on mct attribute vector
        call mct_avect_vecmult(wav(1)%c2x_cc, wav(1)%mdl2drv, &
            seq_flds_w2x_fluxes)

    end if

print *, 'KDS: Connector wav2med Post Run end'

  end subroutine


#endif

end module ccsm_mediator_mod
