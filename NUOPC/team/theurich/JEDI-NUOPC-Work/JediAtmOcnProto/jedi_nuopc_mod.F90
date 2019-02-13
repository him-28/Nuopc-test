! (C) Copyright 2017-2019 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module jedi_nuopc_mod

  use iso_c_binding
  !use config_mod
  !use datetime_mod
  !use duration_mod
  use netcdf

  !use fv3jedi_kinds_mod
  !use fv3jedi_geom_mod, only: fv3jedi_geom
  !use fv3jedi_state_mod, only: fv3jedi_state
  !use fv3jedi_increment_mod, only: fv3jedi_increment 

  !use fckit_mpi_module, only: fckit_mpi_comm, fckit_mpi_sum

  use ESMF
  use NUOPC, only: NUOPC_Advertise, NUOPC_Write, NUOPC_SetTimestamp
  use NUOPC_Comp, only: NUOPC_CompSetClock, NUOPC_CompSearchPhaseMap, &
                        NUOPC_CompAttributeSet
  use NUOPC_Driver, only: NUOPC_DriverGetComp
  use NUOPC_Model, only: NUOPC_ModelGet
  use ESM, only: esmSS => SetServices

  implicit none

  private

  public :: model_nuopc_type
  public :: model_nuopc_create
  public :: model_nuopc_initialize
  public :: model_nuopc_step
  public :: model_nuopc_finalize
  public :: model_nuopc_delete

! ------------------------------------------------------------------------------

  !> Fortran derived type to hold model definition
  type :: model_nuopc_type
    type(ESMF_GridComp) :: esmComp !NUOPC driver
    type(ESMF_State)    :: importState, exportState
    type(ESMF_Clock)    :: clock
  end type model_nuopc_type

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
! Dummy types not available w/o JEDI installation
  public fv3jedi_geom
  type fv3jedi_geom
  end type
  !
  public fv3jedi_state
  type fv3jedi_state
  end type
  !
  type duration
  end type
  !
  public datetime
  type datetime
    character(len=20) :: string_date  ! just as a place holder to allow passing
  end type
  !
  type fckit_mpi_comm
  end type
  !
  integer, parameter  :: kind_real = ESMF_KIND_R8
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

  contains

! ------------------------------------------------------------------------------

  subroutine model_nuopc_create(self, geom, c_conf)
    type(model_nuopc_type), intent(inout) :: self
    type(fv3jedi_geom),     intent(in)    :: geom
    type(c_ptr),            intent(in)    :: c_conf

    integer               :: rc, urc

    character(len=20)     :: ststep
    type(duration)        :: dtstep
    character(len=20)     :: cdate_start
    character(len=20)     :: cdate_final
    type(fckit_mpi_comm)  :: f_comm
    integer               :: phase

    ! FCKIT MPI wrapper for global communicator
    !f_comm = fckit_mpi_comm()

    ! Initialize ESMF
    call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
      defaultCalkind=ESMF_CALKIND_GREGORIAN, &
      !mpiCommunicator=f_comm%communicator(), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("JEDI control of ESM STARTING", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Create the ESM component
    self%esmComp = ESMF_GridCompCreate(name="esm", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    ! SetServices for the ESM component
    call ESMF_GridCompSetServices(self%esmComp, esmSS, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
    ! Set ESM's Verbosity
    call NUOPC_CompAttributeSet(self%esmComp, name="Verbosity", value="1793", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    

    ! Reset the clock based on what JEDI provides
    ! -------------------------------------------

    !ststep = config_get_string(c_conf,len(ststep),"tstep")
    !dtstep = trim(ststep)
    !self%dt = int(duration_seconds(dtstep))

    cdate_start = "2018-04-14T21:00:00Z"
    cdate_final = "2018-04-14T22:00:00Z"

    call construct_clock(1200, cdate_start, cdate_final, clock=self%clock)
    
    ! create import- and exportState to be used as conduits in/out NUOPC ESM
    self%importState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    self%exportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    ! Advertise fields on the exportState, for data coming out of ESM component
    call NUOPC_Advertise(self%exportState, &
      StandardNames=(/"sea_surface_temperature  ", &
                      "air_pressure_at_sea_level"/), &
      TransferOfferField="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Advertise fields on the importState, for data going into of ESM component
    call NUOPC_Advertise(self%importState, &
      StandardNames=(/"precipitation_flux"/), &
      TransferOfferField="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! call ExternalAdvertise phase
    call NUOPC_CompSearchPhaseMap(self%esmComp, &
      methodflag=ESMF_METHOD_INITIALIZE, &
      phaseLabel="ExternalAdvertise", phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(self%esmComp, phase=phase, &
      importState=self%importState, exportState=self%exportState, &
      clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    ! call ExternalRealize phase
    call NUOPC_CompSearchPhaseMap(self%esmComp, &
      methodflag=ESMF_METHOD_INITIALIZE, &
      phaseLabel="ExternalRealize", phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(self%esmComp, phase=phase, &
      importState=self%importState, exportState=self%exportState, &
      clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine model_nuopc_create

! ------------------------------------------------------------------------------

  subroutine model_nuopc_initialize(self, state, vdate)
    type(model_nuopc_type), intent(inout) :: self
    type(fv3jedi_state),    intent(in)    :: state
    type(datetime),         intent(in)    :: vdate

    integer               :: rc, urc
    integer               :: phase
    character(len=20)     :: vdatec
    type(ESMF_Time)       :: time

    !Convert datetimes
    !call datetime_to_string(vdate, vdatec)
    vdatec = vdate%string_date

    ! Set the currTime of the self%clock to new vdate before passing to ESM
    call construct_time(vdatec, time=time)
    call ESMF_ClockSet(self%clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! call ExternalDataInitialize phase
    call NUOPC_CompSearchPhaseMap(self%esmComp, &
      methodflag=ESMF_METHOD_INITIALIZE, &
      phaseLabel="ExternalDataInitialize", phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(self%esmComp, phase=phase, &
      importState=self%importState, exportState=self%exportState, &
      clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
    ! for testing, write out the fields in the exportState to file
    call NUOPC_Write(self%exportState, fileNamePrefix="field_in_esm_export_", &
      timeslice=1, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine model_nuopc_initialize

! ------------------------------------------------------------------------------

  subroutine model_nuopc_step(self, state, vdate1, vdate2)
    type(model_nuopc_type), intent(inout) :: self
    type(fv3jedi_state),    intent(inout) :: state
    type(datetime),         intent(in)    :: vdate1
    type(datetime),         intent(in)    :: vdate2

    integer               :: rc, urc
    character(len=20)     :: vdatec1, vdatec2
    integer, save         :: slice=2

    !Convert datetimes
    !call datetime_to_string(vdate1, vdatec1)
    !call datetime_to_string(vdate2, vdatec2)

    vdatec1 = vdate1%string_date
    vdatec2 = vdate2%string_date

    !Reset the GridComp clock for this advance step
    call construct_clock(1200, vdatec1, vdatec2, clock=self%clock)
    
    ! timestamp the data going into the ESM or else NUOPC will flag incompatible
    call NUOPC_SetTimestamp(self%importState, self%clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Step the ESM forward from vdate1 -> vdate2
    call ESMF_GridCompRun(self%esmComp, &
      importState=self%importState, exportState=self%exportState, &
      clock=self%clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! for testing, write out the fields in the exportState to file
    call NUOPC_Write(self%exportState, fileNamePrefix="field_in_esm_export_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    slice = slice+1

  end subroutine model_nuopc_step

! ------------------------------------------------------------------------------

  subroutine model_nuopc_finalize(self, state, vdate)
    type(model_nuopc_type), intent(inout) :: self
    type(fv3jedi_state),    intent(in)    :: state
    type(datetime),         intent(in)    :: vdate

    integer               :: rc, urc
    integer               :: phase
    character(len=20)     :: vdatec
    type(ESMF_Time)       :: time

    ! Finalize the ESM forward stepping by resetting the startTime on self%clock

    !Convert datetimes
    !call datetime_to_string(vdate, vdatec)
    vdatec = vdate%string_date

    ! Set the currTime of the self%clock to new vdate before passing to ESM
    call construct_time(vdatec, time=time)
    call ESMF_ClockSet(self%clock, startTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
    ! FinalizeReset the ESM
    call NUOPC_CompSearchPhaseMap(self%esmComp, &
      methodflag=ESMF_METHOD_FINALIZE, &
      phaseLabel="ExternalFinalizeReset", phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompFinalize(self%esmComp, phase=phase, &
      importState=self%importState, exportState=self%exportState, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine model_nuopc_finalize

! ------------------------------------------------------------------------------

  subroutine model_nuopc_delete(self)
    type(model_nuopc_type), intent(inout) :: self

    integer               :: rc, urc

    ! completely take down the ESM system
    call ESMF_GridCompFinalize(self%esmComp, &
      importState=self%importState, exportState=self%exportState, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Destroy the Earth system Component
    call ESMF_GridCompDestroy(self%esmComp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("JEDI control of ESM FINISHED", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Finalize ESMF
    !call ESMF_Finalize(endflag=ESMF_END_KEEPMPI,rc=rc)
    call ESMF_Finalize(rc=rc)

  end subroutine model_nuopc_delete

! ------------------------------------------------------------------------------

  subroutine construct_clock(dt, cdate_start, cdate_final, clock)
    integer,            intent(in)     :: dt
    character(len=20),  intent(in)     :: cdate_start
    character(len=20),  intent(in)     :: cdate_final
    type(ESMF_Clock),   intent(out)    :: clock

    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep

    integer :: rc
    
    call construct_time(cdate_start, time=startTime)
    call construct_time(cdate_final, time=stopTime)

    call ESMF_TimeIntervalSet(timeStep, s=dt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    clock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine construct_clock

! ------------------------------------------------------------------------------

  subroutine construct_time(cdate, time)
    character(len=20),  intent(in)     :: cdate
    type(ESMF_Time),    intent(out)    :: time

    integer :: rc

    integer yy,mm,dd,hh,mn

    !Convert character dates to integers
    read(cdate(1:4),'(i4)') yy
    read(cdate(6:7),'(i2)') mm
    read(cdate(9:10),'(i2)') dd
    read(cdate(12:13),'(i2)') hh
    read(cdate(15:16),'(i2)') mn

    call ESMF_TimeSet(time, yy=yy, mm=mm, dd=dd, h=hh, m=mn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine construct_time

! ------------------------------------------------------------------------------

end module jedi_nuopc_mod
