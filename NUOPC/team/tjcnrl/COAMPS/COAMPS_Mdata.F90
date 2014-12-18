!-------------------------------------------------------------------------------
! COAMPS Model Data Component
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_Mdata.F90"
#include "COAMPS_Macros.h"


module COAMPS_Mdata

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS            => SetServices, &
    model_label_DataInitialize  => label_DataInitialize, &
    model_label_SetClock        => label_SetClock, &
    model_label_Advance         => label_Advance, &
    model_label_Finalize        => label_Finalize
  use COAMPS_Futil
  use COAMPS_Gutil

  implicit none

  private

  public SetServices

  character (*), parameter :: label_InternalState = 'InternalState'
  character (*), parameter :: inputAlarmName = 'InputAlarm'
  integer      , parameter :: maxFields = 25
  integer      , parameter :: run_prep_phase = 2
  integer      , parameter :: run_post_phase = 3

  integer, parameter :: modTypeConstant = 0
  integer, parameter :: modTypeTendency = 1
  integer, parameter :: modTypeForecast = 2
  integer, parameter :: modTypeHindcast = 3

  type type_InternalStateStruct
    logical                         :: verbose
    integer                         :: modType
    type(ESMF_TimeInterval)         :: timeStep
    integer                         :: updateCycle
    real(ESMF_KIND_RX)              :: dtRatio
    character(ESMF_MAXSTR)          :: dataDir
    integer                         :: nz
    real(4)                ,pointer :: zl(:) => null()
    type(CGrid)                     :: cgs
    character(7)                    :: fldtyp
    integer                         :: numf
    logical                ,pointer :: isActive(:) => null()
    type(FFile)            ,pointer :: ffs(:) => null()
    character(ESMF_MAXSTR) ,pointer :: sname(:) => null()
    character(6)           ,pointer :: fname(:) => null()
    real(ESMF_KIND_RX)     ,pointer :: scale(:) => null()
    real(ESMF_KIND_RX)     ,pointer :: offst(:) => null()
    type(ESMF_Field)       ,pointer :: field(:) => null()
    type(ESMF_Field)       ,pointer :: ftend(:) => null()
    real(ESMF_KIND_RX)     ,pointer :: const(:) => null()
    integer                ,pointer :: ndim(:) => null()
    logical                         :: realizeAllExport
    logical                         :: loutff_exp
    type(ESMF_TimeInterval)         :: toutff_exp
    character(ESMF_MAXSTR)          :: doutff
    integer(ESMF_KIND_I4)           :: numwt
    character(ESMF_MAXSTR) ,pointer :: wtnam(:) => null()
    integer(ESMF_KIND_I4)  ,pointer :: wtcnt(:) => null()
    real(ESMF_KIND_R8)     ,pointer :: wtime(:) => null()
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: msgString
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! initialize timers
    is%wrap%numwt = 6
    allocate(is%wrap%wtnam(is%wrap%numwt), is%wrap%wtcnt(is%wrap%numwt), &
      is%wrap%wtime(is%wrap%numwt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of wall timer memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    is%wrap%wtnam(1) = 'InitializeP0'
    is%wrap%wtnam(2) = 'InitializeP1'
    is%wrap%wtnam(3) = 'InitializeP3'
    is%wrap%wtnam(4) = 'DataInitialize'
    is%wrap%wtnam(5) = 'ModelAdvance'
    is%wrap%wtnam(6) = 'Finalize'
    is%wrap%wtcnt(:) = 0
    is%wrap%wtime(:) = 0d0

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set initialize phase 0 requires use of ESMF method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set entry points for initialize methods
    ! >= IPDv03 supports satisfying inter-model data dependencies and the transfer of ESMF
    ! Grid & Mesh objects between Model and/or Mediator components during initialization
    ! IPDv03p1: advertise import & export fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    ! IPDv03p2: unspecified by NUOPC -- not required
    ! IPDv03p3: realize import & export fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    ! IPDv03p4: relevant for TransferActionGeomObject=="accept"
    ! IPDv03p5: relevant for TransferActionGeomObject=="accept"
    ! IPDv03p6: check compatibility of fields connected status
    ! IPDv03p7: handle field data initialization

    ! set entry points for prep- and post-run methods
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=RunPrep, phase=run_prep_phase, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=RunPost, phase=run_post_phase, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
         specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
         specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
         specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
         specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    character(ESMF_MAXSTR)        :: verbosity
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=1, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! determine verbosity
    call NUOPC_CompAttributeGet(gcomp, name='Verbosity', value=verbosity, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (trim(verbosity)=='high') then
      is%wrap%verbose = .true.
    else
      is%wrap%verbose = .false.
    endif
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP0', ESMF_LOGMSG_INFO)

    ! check supported component names
    select case (trim(cname))
    case ('ATM','ABG')
    case ('OCN','OBG')
    case ('WAV','WBG')
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg='COAMPS_Mdata: unsupported component name'//trim(cname))
      return ! bail out
    endselect

    ! switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP0', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=2, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    type(ESMF_VM)                 :: vm
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    character(ESMF_MAXSTR)        :: inpstr
    integer                       :: i, numf, nest
    character(1)                  :: cfluid
    character(ESMF_MAXSTR)        :: gnlFile
    character(24)                 :: lvltyp
    real(4)                       :: rlev(2)
    character(7)                  :: fldtyp

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP1', ESMF_LOGMSG_INFO)

    ! query Component for its config & vm
    call ESMF_GridCompGet(gcomp, config=config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get modType
    label=trim(cname)//'_data_type:'
    call ESMF_ConfigGetAttribute(config, inpstr, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    select case (trim(inpstr))
    case ('constant')
      is%wrap%modType = modTypeConstant
    case ('tendency')
      is%wrap%modType = modTypeTendency
    case ('forecast')
      is%wrap%modType = modTypeForecast
    case ('hindcast')
      is%wrap%modType = modTypeHindcast
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': unsupported '//trim(label)//' '//trim(inpstr))
      return ! bail out
    endselect

    ! set number of fields and cfluid
    select case (trim(cname))
    case ('ATM','ABG')
      cfluid = 'a'
    case ('OCN','OBG')
      cfluid = 'o'
    case ('WAV','WBG')
      cfluid = 'w'
    endselect

    ! get realize all export flag
    label=trim(cname)//'_realize_all_export:'
    call ESMF_ConfigGetAttribute(config, is%wrap%realizeAllExport, &
      default=.false., label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get data directory
    label=trim(cname)//'_work_dir:'
    call ESMF_ConfigGetAttribute(config, is%wrap%dataDir, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get gridnl
    label=trim(cname)//'_gridnl:'
    call ESMF_ConfigGetAttribute(config, gnlFile, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get nest
    label=trim(cname)//'_nest:'
    call ESMF_ConfigGetAttribute(config, nest, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get/set fldtyp
    select case (is%wrap%modType)
    case (modTypeTendency)
      fldtyp = 'tendfld'
    case (modTypeForecast,modTypeHindcast)
      fldtyp = 'fcstfld'
    endselect
    label=trim(cname)//'_fldtyp:'
    call ESMF_ConfigGetAttribute(config, is%wrap%fldtyp, default=fldtyp, &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set COAMPS grid structure
    is%wrap%cgs = CGridCreate(gnlFile, cfluid, nest, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! allocate field/data arrays
    allocate(is%wrap%isActive(maxFields), is%wrap%ffs(maxFields), &
      is%wrap%sname(maxFields), is%wrap%fname(maxFields), &
      is%wrap%scale(maxFields), is%wrap%offst(maxFields), &
      is%wrap%field(maxFields), is%wrap%ftend(maxFields), &
      is%wrap%const(maxFields), is%wrap%ndim(maxFields), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state data arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    is%wrap%isActive = .false.

    ! set attributes for background fields
    i = 0
    select case (trim(cname))
    case ('ATM','ABG')
      i = i+1
      is%wrap%sname(i) = 'air_pressure_at_sea_level'
      is%wrap%fname(i) = 'slpres' !'pres'
      is%wrap%scale(i) = 100.0 !mb -> Pa
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 1000.0
      is%wrap%ndim(i) = 2
      lvltyp = 'marn_lvl'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'eastward_wind_at_10m_height'
      is%wrap%fname(i) = 'uutrue' !'wnd_utru'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 10.0
      is%wrap%ndim(i) = 2
      lvltyp = 'ht_sfc'; rlev = (/10.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'northward_wind_at_10m_height'
      is%wrap%fname(i) = 'vvtrue' !'wnd_vtru'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 10.0
      is%wrap%ndim(i) = 2
      lvltyp = 'ht_sfc'; rlev = (/10.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'magnitude_of_surface_downward_stress'
      is%wrap%fname(i) = 'wstres' !'wnd_strs'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 10.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'air_temperature_at_2m_height'
      is%wrap%fname(i) = 'airtmp' !'air_temp'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 300.0
      is%wrap%ndim(i) = 2
      lvltyp = 'ht_sfc'; rlev = (/2.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'relative_humidity_at_2m_height'
      is%wrap%fname(i) = 'relhum' !'rltv_hum'
      is%wrap%scale(i) = 0.01 !percent -> fraction
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 60.0
      is%wrap%ndim(i) = 2
      lvltyp = 'ht_sfc'; rlev = (/2.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'surface_net_downward_shortwave_flux'
      is%wrap%fname(i) = 'solflx' !'sol_rad'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 300.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'surface_net_downward_longwave_flux'
      is%wrap%fname(i) = 'lonflx' !'ir_flux'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 100.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case ('OCN','OBG')
      i = i+1
      is%wrap%sname(i) = 'sea_surface_temperature'
      is%wrap%fname(i) = 'seatmp'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 290.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'sea_surface_salinity'
      is%wrap%fname(i) = 'salint'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 35.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'sea_surface_height_above_sea_level'
      is%wrap%fname(i) = 'seahgt'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'surface_eastward_sea_water_velocity'
      is%wrap%fname(i) = 'uucurr'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'surface_northward_sea_water_velocity'
      is%wrap%fname(i) = 'vvcurr'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case ('WAV','WBG')
      i = i+1
      is%wrap%sname(i) = 'wave_induced_charnock_parameter'
      is%wrap%fname(i) = 'charno'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.011
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'surface_total_wave_induced_stress'
      is%wrap%fname(i) = 'wvstrs'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'surface_eastward_wave_induced_stress'
      is%wrap%fname(i) = 'wvstru'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'surface_northward_wave_induced_stress'
      is%wrap%fname(i) = 'wvstrv'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'eastward_stokes_drift_current'
      is%wrap%fname(i) = 'uscurr'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 3
      lvltyp = 'dpth_sfc'; rlev = (/0.0,300.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'northward_stokes_drift_current'
      is%wrap%fname(i) = 'vscurr'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 3
      lvltyp = 'dpth_sfc'; rlev = (/0.0,300.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'eastward_wave_bottom_current'
      is%wrap%fname(i) = 'wbcuru'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'northward_wave_bottom_current'
      is%wrap%fname(i) = 'wbcurv'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'wave_bottom_current_radian_frequency'
      is%wrap%fname(i) = 'wbcurf'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'eastward_wave_radiation_stress'
      is%wrap%fname(i) = 'wavsuu'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'eastward_northward_wave_radiation_stress'
      is%wrap%fname(i) = 'wavsuv'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'northward_wave_radiation_stress'
      is%wrap%fname(i) = 'wavsvv'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'eastward_wave_radiation_stress_gradient'
      is%wrap%fname(i) = 'wavsgu'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      i = i+1
      is%wrap%sname(i) = 'northward_wave_radiation_stress_gradient'
      is%wrap%fname(i) = 'wavsgv'
      is%wrap%scale(i) = 1.0
      is%wrap%offst(i) = 0.0
      is%wrap%const(i) = 0.0
      is%wrap%ndim(i) = 2
      lvltyp = 'surface'; rlev = (/0.0,0.0/);
      is%wrap%ffs(i) = FFileCreate(is%wrap%cgs, is%wrap%fname(i), lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endselect
    numf = i
    is%wrap%numf = numf
    if (numf.gt.maxFields) then
      write(msgString,'(a,i3)') trim(cname)//': increase maxFields to ',numf
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
      return ! bail out
    endif

    ! adjust stdName
    select case (trim(cname))
    case ('OBG','WBG')
      do i=1,numf
        is%wrap%sname(i) = 'mbg_'//trim(is%wrap%sname(i))
      enddo
    end select

    ! advertise exportable fields
    do i=1,numf
      call NUOPC_StateAdvertiseField(exportState, &
        StandardName=trim(is%wrap%sname(i)), name=trim(is%wrap%fname(i)), &
        TransferOfferGeomObject="will provide", rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! report advertised export fields
    write(msgString,'(a,i0,a)') trim(cname)// &
      ': List of advertised export fields(',numf,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,a5,a,a10,a3,a)') trim(cname)// &
      ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    do i=1,numf
      write(msgString,'(a,i5,a,a10,a3,a)') trim(cname)// &
        ': ',i,' ',trim(is%wrap%fname(i)),' ',trim(is%wrap%sname(i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP1', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=3, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer, parameter            :: localDE=0
    type(ESMF_VM)                 :: vm
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    character(ESMF_MAXSTR)        :: inpstr
    character(6)                  :: mskpfx='lndsea'
    type(ESMF_Time)               :: startTime
    integer                       :: i, n
    type(ESMF_ArraySpec)          :: arraySpec2d, arraySpec3d
    type(ESMF_Grid)               :: grid
    logical                       :: inc3d
    logical                       :: isConnected

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP3', ESMF_LOGMSG_INFO)

    ! query Component for its config & vm
    call ESMF_GridCompGet(gcomp, config=config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! flag connected export fields
    ! remove unconnected if not realize all export
    n = 0
    do i = 1,is%wrap%numf
      isConnected = NUOPC_StateIsFieldConnected(exportState, is%wrap%fname(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      is%wrap%isActive(i) = isConnected .or. is%wrap%realizeAllExport
      if (is%wrap%isActive(i)) then
        n = n + 1
      else
        call ESMF_StateRemove(exportState, (/is%wrap%fname(i)/), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    enddo

    ! report realized export fields
    write(msgString,'(a,i0,a)') trim(cname)// &
      ': List of realized export fields(',n,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,a5,a,a10,a3,a)') trim(cname)// &
      ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    n = 0
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      n = n + 1
      write(msgString,'(a,i5,a,a10,a3,a)') trim(cname)// &
        ': ',n,' ',trim(is%wrap%fname(i)),' ',trim(is%wrap%sname(i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! if no active export fields, then skip the rest
    if (.not.any(is%wrap%isActive)) goto 1

    ! query Component for its config & vm
    call ESMF_GridCompGet(gcomp, config=config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time information from clock
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get coord source (flatfile or compute)
    ! and create grid
    label=trim(cname)//'_coord_source:'
    call ESMF_ConfigGetAttribute(config, inpstr, default='flatfile', &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    select case (trim(inpstr))
    case ('flatfile')
      grid = GridCreate(is%wrap%cgs, trim(is%wrap%dataDir), vm, &
        startTime, startTime, read_coord=.true., rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case ('compute')
      grid = GridCreate(is%wrap%cgs, trim(is%wrap%dataDir), vm, &
        startTime, startTime, read_coord=.false., rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': unsupported '//trim(label)//' '//trim(inpstr))
      return ! bail out
    endselect

    ! get mask source (flatfile or none or path to GLOBE DEM)
    ! and set mask (assume COAMPS mask convention)
    label=trim(cname)//'_mask_source:'
    call ESMF_ConfigGetAttribute(config, inpstr, default='flatfile', &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    select case (trim(inpstr))
    case ('flatfile')
      call GridSetMask(grid, is%wrap%cgs, trim(is%wrap%dataDir), vm, &
        startTime, startTime, mask_prefix=mskpfx, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case ('none')
      call GridSetMask(grid, COAMPS_MASK_WATER, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case default
      call GridSetMask(grid, trim(inpstr), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endselect

    ! flatfile output
    label=trim(cname)//'_outff_exp_flag:'
    call ESMF_ConfigGetAttribute(config, is%wrap%loutff_exp, default=.false., &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (is%wrap%loutff_exp) then
      label=trim(cname)//'_outff_dir:'
      call ESMF_ConfigGetAttribute(config, is%wrap%doutff, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

    ! setup zlevels
    inc3d = .false.
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      if (is%wrap%ndim(i).eq.3) inc3d = .true.
    enddo
    if (inc3d) then
      call GetZlevels(is%wrap%nz, is%wrap%zl, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

    ! create arraySpec
    call ESMF_ArraySpecSet(arraySpec2d, rank=2, typeKind=ESMF_TYPEKIND_RX, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ArraySpecSet(arraySpec3d, rank=3, typeKind=ESMF_TYPEKIND_RX, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! realize active export fields
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      select case (is%wrap%ndim(i))
      case (2)
        is%wrap%field(i) = ESMF_FieldCreate(grid, arraySpec2d, &
          name=is%wrap%fname(i), indexFlag=ESMF_INDEX_GLOBAL, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        if (is%wrap%modType.ne.modTypeConstant) then
          is%wrap%ftend(i) = ESMF_FieldCreate(grid, arraySpec2d, &
            name=is%wrap%fname(i), indexFlag=ESMF_INDEX_GLOBAL, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        endif
      case (3)
        is%wrap%field(i) = ESMF_FieldCreate(grid, arraySpec3d, &
          name=is%wrap%fname(i), indexFlag=ESMF_INDEX_GLOBAL, &
          ungriddedLBound=(/1/), ungriddedUBound=(/is%wrap%nz/), &
          staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        if (is%wrap%modType.ne.modTypeConstant) then
          is%wrap%ftend(i) = ESMF_FieldCreate(grid, arraySpec3d, &
            name=is%wrap%fname(i), indexFlag=ESMF_INDEX_GLOBAL, &
            ungriddedLBound=(/1/), ungriddedUBound=(/is%wrap%nz/), &
            staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        endif
      case default
        write(msgString,'(a,i3)') trim(cname)// &
          ': unsupported field dimension: ',is%wrap%ndim(i)
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
        return ! bail out
      endselect
      call NUOPC_StateRealizeField(exportState, is%wrap%field(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call FFileSetDims(is%wrap%ffs(i), is%wrap%field(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! flatfile output
    if (is%wrap%loutff_exp) then
      call FFileWriteGrid(grid, vm, trim(is%wrap%doutff), is%wrap%cgs%nest, &
        is%wrap%cgs%cfluid, startTime, startTime, opt='both', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP3', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=4, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer, parameter            :: localDE=0
    integer                       :: localPet
    integer                       :: i

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! query the Component for PET info
    call ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report
    write(msgString,'(a)') trim(cname)//': entered DataInitialize'
    if (verbose) call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    if (localPet.eq.0) write(*,'(///a)') trim(msgString)

    ! set export fields
    call SetExport(gcomp, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report all import dependencies are satisfied
    write(msgString,'(a)') trim(cname)//': all inter-model data dependencies SATISFIED'
    if (verbose) call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    if (localPet.eq.0) write(*,'(a)') trim(msgString)

    ! set Updated Field Attribute to "true", indicating to the
    ! generic code to set the timestamp for these fields
    do i=1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      call NUOPC_FieldAttributeSet(is%wrap%field(i), name="Updated", value="true", rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report
1   write(msgString,'(a)') trim(cname)//': leaving DataInitialize'
    if (verbose) call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    if (localPet.eq.0) write(*,'(/a)') trim(msgString)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=5, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    type(ESMF_Clock)              :: clock
    integer                       :: localPet

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ModelAdvance', ESMF_LOGMSG_INFO)

    ! query the Component for PET info
    call ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the Component for its clock
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (localPet.eq.0) then
      write(*,'(///)')
      call NUOPC_ClockPrintCurrTime(clock, &
        '-->Advancing '//trim(cname)//' from: ', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call NUOPC_ClockPrintStopTime(clock, &
        '-----------------> to: ', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

    ! set export fields
    call SetExport(gcomp, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ModelAdvance', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RunPrep(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=6, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i
    type(ESMF_Clock)              :: internalClock
    type(ESMF_Time)               :: startTime
    integer(ESMF_KIND_I8)         :: zero = 0

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered RunPrep', ESMF_LOGMSG_INFO)

    ! reset internal clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ClockGet(internalClock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ClockSet(internalClock, currTime=startTime, advanceCount=zero, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving RunPrep', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RunPost(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=7, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered RunPost', ESMF_LOGMSG_INFO)

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving RunPost', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=6, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered Finalize', ESMF_LOGMSG_INFO)

    ! destroy fields
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      call ESMF_FieldDestroy(is%wrap%field(i),rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (is%wrap%modType.ne.modTypeConstant) then
        call ESMF_FieldDestroy(is%wrap%ftend(i),rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    enddo
    if (associated(is%wrap%zl)) then
      deallocate(is%wrap%zl, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of zl array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    ! deallocate field/data arrays
    deallocate(is%wrap%isActive, is%wrap%ffs, &
      is%wrap%sname, is%wrap%fname, &
      is%wrap%scale, is%wrap%offst, &
      is%wrap%field, is%wrap%ftend, &
      is%wrap%const, is%wrap%ndim, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of internal state data arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

    ! print timing
    call PrintTimers(trim(cname), is%wrap%wtnam, is%wrap%wtcnt, is%wrap%wtime)

    ! deallocate timers
    if (associated(is%wrap%wtnam)) then
      deallocate(is%wrap%wtnam, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtnam array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%wtcnt)) then
      deallocate(is%wrap%wtcnt, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtcnt array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%wtime)) then
      deallocate(is%wrap%wtime, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtime array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: zeroti
    type(ESMF_TimeInterval)       :: timeStep
    integer(ESMF_KIND_I4)         :: time(3)
    type(ESMF_TimeInterval)       :: updateCycle
    type(ESMF_Alarm)              :: inputAlarm
    real(ESMF_KIND_R8)            :: dtRatio

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if constant, then return
    if (is%wrap%modType.eq.modTypeConstant) return

    ! set zero time interval
    call ESMF_TimeIntervalSet(zeroti, s=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get clock and config
    call ESMF_GridCompGet(gcomp, clock=clock, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get driver time step (coupling interval)
    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time step (data input interval) from config
    label=trim(cname)//'_time_step:'
    call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ConfigGetAttribute(config, time, count=3, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_TimeIntervalSet(is%wrap%timeStep, h=time(1), m=time(2), s=time(3), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (mod(is%wrap%timeStep,timeStep) /= zeroti) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': '//trim(label)// &
        ' must be a multiple of driver time step')
      return ! bail out
    endif

    ! setup alarm for reading input fields
    inputAlarm = ESMF_AlarmCreate(clock, ringInterval=is%wrap%timeStep, &
      name=inputAlarmName, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! compute ratio of driver timeStep to model timeStep
    dtRatio = timeStep / is%wrap%timeStep
    is%wrap%dtRatio = real(dtRatio,ESMF_KIND_RX)

    ! get update cycle from config
    select case (is%wrap%modType)
    case (modTypeHindcast)
      label=trim(cname)//'_update_cycle:'
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ConfigGetAttribute(config, is%wrap%updateCycle, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_TimeIntervalSet(updateCycle, h=is%wrap%updateCycle, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (mod(updateCycle,timeStep) /= zeroti) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)// &
          ' must be a multiple of driver time step')
        return ! bail out
      endif
      if (mod(updateCycle,is%wrap%timeStep) /= zeroti) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)// &
          ' must be a multiple of model time step')
        return ! bail out
      endif
    endselect

    if (is%wrap%loutff_exp) then
      label=trim(cname)//'_outff_exp_interval:'
      call ESMF_ConfigGetAttribute(config, time(3), &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_TimeIntervalSet(is%wrap%toutff_exp, s=time(3), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (mod(is%wrap%toutff_exp,timeStep) /= zeroti) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)// &
          ' must be a multiple of driver time step')
        return ! bail out
      endif
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetExport(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    type(ESMF_VM)                 :: vm
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: startTime, currTime
    type(ESMF_Time)               :: dtgTime, tauTime
    type(ESMF_TimeInterval)       :: zeroti, elapsedTime
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no active export fields, then return
    if (.not.any(is%wrap%isActive)) return

    ! query the Component for its clock & vm
    call ESMF_GridCompGet(gcomp, clock=clock, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time information from clock
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set export fields
    select case (is%wrap%modType)
    case (modTypeConstant)
      call SetConstant(gcomp, rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case default
      call SetFromFile(gcomp, rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endselect

    ! flatfile output
    if (is%wrap%loutff_exp) then
      call ESMF_TimeIntervalSet(zeroti, s=0, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      elapsedTime = currTime - startTime
      if (mod(elapsedTime,is%wrap%toutff_exp) == zeroti) then
        dtgTime = startTime
        tauTime = currTime
        do i=1,is%wrap%numf
          if (.not.is%wrap%isActive(i)) cycle
          call FFileWrite(is%wrap%ffs(i), is%wrap%field(i), vm, &
            trim(is%wrap%doutff), dtgTime, tauTime, 'exptfld', rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        enddo
      endif
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetConstant(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    type(ESMF_VM)                 :: vm
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: startTime, currTime
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no active export fields, then return
    if (.not.any(is%wrap%isActive)) return

    ! query the Component for its clock & vm
    call ESMF_GridCompGet(gcomp, clock=clock, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time information from clock
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! only set data for tau0
    if (currTime.ne.startTime) return

    ! set constant data
    do i=1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      call FieldFill(is%wrap%field(i), is%wrap%const(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call FieldScale(is%wrap%field(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetFromFile(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    type(ESMF_VM)                 :: vm
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: startTime, currTime
    type(ESMF_Time)               :: dtgTime, tauTime
    type(ESMF_Time)               :: nextTime
    integer(ESMF_KIND_I4)         :: day,hour
    integer                       :: i
    type(ESMF_Alarm)              :: inputAlarm

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no active export fields, then return
    if (.not.any(is%wrap%isActive)) return

    ! query the Component for its clock & vm
    call ESMF_GridCompGet(gcomp, clock=clock, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time information from clock
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process tau0 fields and return
    if (currTime.eq.startTime) then

      do i=1,is%wrap%numf
        ! skip if not active
        if (.not.is%wrap%isActive(i)) cycle
        ! branch according to modType
        select case (is%wrap%modType)
        case (modTypeTendency)
          dtgTime = startTime
          tauTime = startTime
          ! read startTime field
          call FFileRead(is%wrap%ffs(i), is%wrap%field(i), vm, &
            trim(is%wrap%dataDir), dtgTime, startTime, 'fcstfld', rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%field(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          ! read tendency
          call FFileRead(is%wrap%ffs(i), is%wrap%ftend(i), vm, &
            trim(is%wrap%dataDir), dtgTime, tauTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%ftend(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        case (modTypeForecast)
          dtgTime = startTime
          tauTime = startTime + is%wrap%timeStep
          ! read startTime field
          call FFileRead(is%wrap%ffs(i), is%wrap%field(i), vm, &
            trim(is%wrap%dataDir), dtgTime, startTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%field(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          ! read next field
          call FFileRead(is%wrap%ffs(i), is%wrap%ftend(i), vm, &
            trim(is%wrap%dataDir), dtgTime, tauTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%ftend(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          ! compute tendency
          call FieldAdd(is%wrap%ftend(i), is%wrap%ftend(i), is%wrap%field(i), &
            src1Fac=1._ESMF_KIND_RX, src2Fac=-1._ESMF_KIND_RX, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        case (modTypeHindcast)
          nextTime = startTime + is%wrap%timeStep
          call ESMF_TimeGet(nextTime, dd=day, h=hour, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          if (mod(hour,is%wrap%updateCycle) == 0) then
            dtgTime = nextTime
            tauTime = dtgTime
          else
            dtgTime = startTime
            tauTime = nextTime
          endif
          ! read startTime field
          call FFileRead(is%wrap%ffs(i), is%wrap%field(i), vm, &
            trim(is%wrap%dataDir), dtgTime, startTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%field(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          ! read next field
          call FFileRead(is%wrap%ffs(i), is%wrap%ftend(i), vm, &
            trim(is%wrap%dataDir), dtgTime, tauTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%ftend(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          ! compute tendency
          call FieldAdd(is%wrap%ftend(i), is%wrap%ftend(i), is%wrap%field(i), &
            src1Fac=1._ESMF_KIND_RX, src2Fac=-1._ESMF_KIND_RX, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        endselect
      enddo

      return
    endif !tau0

    ! after tau0 -- process tendency fields
    do i=1,is%wrap%numf
      ! skip if not active
      if (.not.is%wrap%isActive(i)) cycle
      ! compute new forecast field
      call FieldAdd(is%wrap%field(i), is%wrap%field(i), is%wrap%ftend(i), &
        src1Fac=1._ESMF_KIND_RX, src2Fac=is%wrap%dtRatio, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! get input alarm from clock
    call ESMF_ClockGetAlarm(clock, inputAlarmName, inputAlarm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! time for new input
    if (ESMF_AlarmIsRinging(inputAlarm)) then
      do i=1,is%wrap%numf
        ! skip if not active
        if (.not.is%wrap%isActive(i)) cycle
        ! branch according to modType
        select case (is%wrap%modType)
        case (modTypeTendency)
          dtgTime = startTime
          tauTime = currTime
          ! read tendency
          call FFileRead(is%wrap%ffs(i), is%wrap%ftend(i), vm, &
            trim(is%wrap%dataDir), dtgTime, tauTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%ftend(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        case (modTypeForecast)
          dtgTime = startTime
          tauTime = currTime + is%wrap%timeStep
          ! read next field
          call FFileRead(is%wrap%ffs(i), is%wrap%ftend(i), vm, &
            trim(is%wrap%dataDir), dtgTime, tauTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%ftend(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          ! compute tendency
          call FieldAdd(is%wrap%ftend(i), is%wrap%ftend(i), is%wrap%field(i), &
            src1Fac=1._ESMF_KIND_RX, src2Fac=-1._ESMF_KIND_RX, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        case (modTypeHindcast)
          nextTime = currTime + is%wrap%timeStep
          call ESMF_TimeGet(nextTime, dd=day, h=hour, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          if (mod(hour,is%wrap%updateCycle) == 0) then
            dtgTime = nextTime
            tauTime = dtgTime
          else
            dtgTime = currTime
            tauTime = nextTime
          endif
          ! read next field
          call FFileRead(is%wrap%ffs(i), is%wrap%ftend(i), vm, &
            trim(is%wrap%dataDir), dtgTime, tauTime, is%wrap%fldtyp, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          call FieldScale(is%wrap%ftend(i), is%wrap%scale(i), is%wrap%offst(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
          ! compute tendency
          call FieldAdd(is%wrap%ftend(i), is%wrap%ftend(i), is%wrap%field(i), &
            src1Fac=1._ESMF_KIND_RX, src2Fac=-1._ESMF_KIND_RX, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        endselect
      enddo
      ! turn off alarm
      call ESMF_AlarmRingerOff(inputAlarm)
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine GetZlevels(nz, zl, rc)
    integer           :: nz
    real(4), pointer  :: zl(:)
    integer, optional :: rc

    ! local variables
    character(14)     :: cenv
    character(256)    :: cfile
    character(64)     :: msg=' '
    integer           :: iunit, stat, k

    if (present(rc)) rc = ESMF_SUCCESS

    cenv = 'NCOM_OZWAV_1D'
    cfile = ' ';
    call getenv(cenv,cfile)

    call ESMF_UtilIOUnitGet(iunit, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    open (unit=iunit, file=trim(cfile), form='formatted', action='read', &
      status='old', iostat=stat, iomsg=msg)
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(cfile), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    read (iunit, *, iostat=stat, iomsg=msg) nz
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(cfile), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    allocate (zl(nz), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of zl failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    do k=1,nz
      read(iunit, *, iostat=stat, iomsg=msg) zl(k)
      if (stat.ne.0) then
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
          CONTEXT, rcToReturn=rc)
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(cfile), &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
    enddo

    close (iunit, iostat=stat, iomsg=msg)
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(cfile), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  function FieldIndex(fnameList, fname) result (indx)
    character(6)      :: fnameList(:)
    character(6)      :: fname
    integer           :: indx

    ! local variables
    integer           :: i

    indx = 0
    do i=lbound(fnameList,1),ubound(fnameList,1)
      if (fnameList(i).eq.fname) then
        indx = i
        exit
      endif
    enddo

  end function

end module
