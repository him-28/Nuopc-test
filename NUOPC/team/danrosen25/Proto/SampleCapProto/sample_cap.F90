!>
!! @mainpage Model Name NUOPC Cap Template
!! @author Author Name (author_email@domain)
!! @date MM/DD/YYYY First change description
!! @author Author Name (author_email@domain)
!! @date MM/DD/YYYY Second change description
!!
!! @tableofcontents
!!
!! @section Overview Overview
!!
!! **Bold Text.**
!!
!! Documentation description.
!! Documentation description continued on same line.
!!
!! Space required to start new paragraph.
!!
!! Sample External Link [Doxygen Documentation] 
!! (http://www.doxygen.org/index.html)
!!
!! Sample Code Link [SetServices] (@ref sample_cap_mod::setservices)
!!
!! Sample Numbered List
!! -# item 1
!! -# item 2
!! -# item 3
!!
!! Sample Bullet List
!! - item 1
!! - item 2 
!!
!! Sample Formula
!! \f[
!! \begin{bmatrix}
!! \tau_x' \\
!! \tau_y'
!! \end{bmatrix} =
!! \begin{bmatrix}
!!  cos \theta   & sin \theta \\
!!  -sin \theta  & cos \theta
!! \end{bmatrix} *
!! \begin{bmatrix}
!! \tau_x \\
!! \tau_y
!! \end{bmatrix}
!! \f]
!!
!!
!! @subsection CapSubroutines Cap Subroutines
!!
!! Description of NUOPC Model specialization.
!! 
!! Table summarizing the NUOPC-required subroutines that appear in the
!! specialized model.  The "Phase" column says whether the subroutine is
!! called during the initialization, run, or finalize part of the coupled
!! system run. 
!!
!! Phase  |     Cap Subroutine                                      | Description
!! -------|---------------------------------------------------------|-------------------------------------------------------------
!! Init   | [InitializeP0] (@ref sample_cap_mod::InitializeP0)     | Set the Initialize Phase Definition (IPD). Configure model
!! Init   | [InitializeP1] (@ref sample_cap_mod::InitializeP1)     | Initialize model.  Advertize import and export fields
!! Init   | [InitializeP3] (@ref sample_cap_mod::InitializeP3)     | Realize import and export fields
!! Init   | [DataInitialize] (@ref sample_cap_mod::DataInitialize) | Initialize data
!! Init   | [SetClock] (@ref sample_cap_mod::SetClock)             | Set model clock during initialization
!! Run    | [ModelAdvance] (@ref sample_cap_mod::ModelAdvance)     | Advances the model by a timestep
!! Final  | [ModelFinalize] (@ref sample_cap_mod::ModelFinalize)   | Releases memory
!!
!! @section UnderlyingModelInterfaces Underlying Model Interfaces
!!
!!
!! @subsection DomainCreation Domain Creation
!!
!! Description of the model grid/mesh creation.
!!
!!
!! @subsection Initialization Initialization
!!
!! Description of the initialization phases and internal model calls.
!! - [InitializeP0] (@ref sample_cap_mod::InitializeP0)
!! - [InitializeP1] (@ref sample_cap_mod::InitializeP1)
!! - [InitializeP3] (@ref sample_cap_mod::InitializeP3)
!! - [DataInitialize] (@ref sample_cap_mod::DataInitialize)
!! - [SetClock] (@ref sample_cap_mod::SetClock)
!!
!! During initialize phase 0 the runtime configuration is read in from a 
!! configuration file and the initialization phase definition version is 
!! set to IPDv03.
!! 
!! During initialize phase 1 the model is initialized [model_init] 
!! (@ref model_drv::model_init) and the import and export fields are
!! advertised. Import fields are not advertised is coupled forcing is off.
!!
!! During initialize phase 3 import and export fields are realized if they
!! are connected through NUOPC.  Fields are retrieved from the [model fields]
!! (@ref model_fld). These fields were created during [model_init] 
!! (@ref model_drv::model_init). All export fields are realized 
!! if realize all export fields is turned on.
!!
!! During data initialize this cap checks the timestamp of all import fields
!! dependent on a coupled model.  Once all dependent import fields have been
!! initialized this cap is marked initalized.
!! 
!! During set clock the the cap's time step is updated to the time step
!! configured in initialize phase 0.  If the time step is 0 then the time step
!! is set to the driver's timestep. Also, if the restart write interval time 
!! step is not zero then the retart write interval is created.
!!
!! @subsection Run Run
!!
!! Description of the run phase(s) and internal model calls.
!! - [ModelAdvance] (@ref sample_cap_mod::ModelAdvance)
!!
!! During model advance the model is advanced [model_run] 
!! (@ref model_drv::model_run) and restart state files are written
!! if the restart write tracker hits restart write interval.
!!
!! @subsection Finalization Finalization
!!
!! Description of the finalize phase and internal model calls.
!! - [ModelFinalize] (@ref sample_cap_mod::ModelFinalize)
!!
!! During model finalize the model is finalized [model_finalize] 
!! (@ref model_drv::model_finalize).  This destroys all of the [model fields]
!! (@ref model_fld::model_fields_destroy).  Internal state memory is also
!! released.
!!
!! @section ModelFields Model Fields
!!
!! The following tables list the import and export fields.
!!
!! @subsection ImportFields Import Fields 
!!
!! Import fields arelisted in the import_list parameter.
!!
!! Standard Name  | Units  | Model Variable  | Description                                | Notes
!! ---------------|--------|-----------------|--------------------------------------------|--------------------------------------
!! dummy_field_1  | Pa     | forcing_1       | field description for first import field   | |
!! dummy_field_2  | kg     | forcing_2       | field description for second import field  | |
!! dummy_field_3  | W m-2  | forcing_3       | field description for third import field   | field notes
!!
!! @subsection ExportField Export Fields
!!
!! Export fields are listed in the export_list parameter.
!!
!! Standard Name  | Units   | Model Variable  | Description                               | Notes
!! ---------------|---------|-----------------|-------------------------------------------|---------------------------
!! dummy_field_1  | m       | output_1        | field description for first export field  | field notes
!! dummy_field_2  | kg      | output_2        | field description for second export field | |
!! dummy_field_3  | m s-1   | output_3        | field description for third export field  | field notes
!!
!! @subsection MemoryManagement Memory Management
!!
!! Description of memory allocated in cap.
!! - Internal state data structure
!! - Internal state wrapper data structure (single pointer)
!! - Field memory allocation
!!
!! Internal State
!!
!!    type model_internalstate_type
!!       integer                 :: verbosity        = 1
!!       integer                 :: timeStepSeconds  = 0
!!       logical                 :: coupledForcing   = .TRUE.
!!       logical                 :: realizeAllExport = .FALSE.
!!       integer                 :: restartSeconds   = 0
!!       type(ESMF_TimeInterval) :: restartInterval
!!       type(ESMF_TimeInterval) :: restartTracker
!!       integer                 :: slice            = 1
!!     end type
!!
!! Internal State Wrapper
!!
!!     type model_internalstate_wrapper
!!       type(model_internalstate_type), pointer :: wrap
!!     end type
!!
!! Field memory is allocated in the model.  The cap directly reference the
!! fields created in [model_fields_create] 
!! (@ref model_fld::model_fields_create).  These fields are destroyed in
!! [model_fields_destroy] (@ref model_fld::model_fields_destroy).
!!
!! @subsection IO I/O
!!
!! 
!! The cap controls ESMF log output with the verbosity variable.  Verbosity
!! is set by the model attributes.  The default verbsoity is 1.
!!
!! The cap controls the state restart write with the restartSeconds variable.
!! RestartSeconds is set by the model attributes.  The default is 0, which 
!! tell the model to never write state restart files.  Files are written as
!! NetCDF files.  Each state file is written to its own file.
!!
!! @section Dependencies Dependencies
!!
!! Dependencies
!! - List of library dependencies (i.e. NetCDF)
!!
!! Description of configuration for dependencies
!!
!! @section BuildingAndInstalling Building and Installing
!!
!! Environment Variables
!! - ESMFMKFILE
!! - Library dependency variables (i.e. NETCDF)
!!
!! NUOPC Makefile Targets
!! - nuopc
!! - nuopcclean
!!
!! The build system in [makefile.sample_cap] (@ref makefile.sample_cap) 
!! wraps the models build system in [makefile.model] (@ref makefile.model)
!! and adds the nuopc and nuopcclean targets.
!!
!! To build and install into the current directory run:
!!    $ make -f makefile.sample_cap nuopc
!!
!! To build and install into an alternative directory run:
!!    $ make -f makefile.sample_cap nuopc DESTDIR=<DIR> INSTDIR=<SUBDIR>
!!
!! To build with debugging run:
!!    $ make -f makefile.sample_cap nuopc DEBUG=on
!!
!! @section RuntimeConfiguration Runtime Configuration
!!
!! At runtime, the cap reads attributes attached to the model.
!!
!! Attributes
!! * `verbosity` - Set the log output level. Default = 1
!! * `time_step_seconds` - Set the model timestep. Default = Driver's Timestep
!! * `coupled_forcing` - Set this flag to determine forcing mode. Default = T
!! * `realize_all_export` - Set this flag to realize all export fields. Default = F
!! * `restart_seconds` - Set the restart write interval. Default = NONE
!! 
!! @section Repository
!! The CAP NUOPC cap is maintained in a GitHub repository:
!! https://github.com/this
!!
!! @section References 
!! 
!! - [CAP Home Page] (http://location)
!!
!!

#define MODNAME "SAMPLE_CAP"

module sample_cap_mod

  !-----------------------------------------------------------------------------
  ! Sample Model Component
  !
  ! Dead model components advertise fields in the importState and the
  ! exportState. They act to the outside like fully prognostic models,
  ! however, they typically internally ignore the imported data and
  ! export scientifically meaningless data. Dead model components remove
  ! any field from the import/export State that is not connected, thus
  ! preventing NUOPC incompatibility errors for not connected fields,
  ! independent on what the other side of the connection advertises.
  !-----------------------------------------------------------------------------

  use model_drv_mod, only: model_init, model_run, model_finalize
  use model_dom_mod, only: model_grid
  use model_fld_mod, only: model_field_get
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS           => SetServices, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_SetClock       => label_SetClock, &
    model_label_CheckImport    => label_CheckImport, &
    model_label_Advance        => label_Advance, &
    model_label_Finalize       => label_Finalize

  implicit none

  private

  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'
  CHARACTER(LEN=20), PARAMETER, DIMENSION(3) :: import_list = (/ &
    "dummy_field_1       ", &
    "dummy_field_2       ", &
    "dummy_field_3       " /)
  CHARACTER(LEN=20), PARAMETER, DIMENSION(3) :: export_list = (/ &
    "dummy_field_4       ", &
    "dummy_field_5       ", &
    "dummy_field_6       " /)

  type model_internalstate_type
    integer                 :: verbosity        = 1
    integer                 :: timeStepSeconds  = 0
    logical                 :: coupledForcing   = .TRUE.
    logical                 :: realizeAllExport = .FALSE.
    integer                 :: restartSeconds   = 0
    type(ESMF_TimeInterval) :: restartInterval
    type(ESMF_TimeInterval) :: restartTracker
    integer                 :: slice            = 1
  end type

  type model_internalstate_wrapper
    type(model_internalstate_type), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "SetServices"

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Derive from the generic NUOPC model component
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv01
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
       specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP0"

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)               :: gcomp
    type(ESMF_State)                  :: importState, exportState
    type(ESMF_Clock)                  :: clock
    integer, intent(out)              :: rc

    ! local variables
    character(ESMF_MAXSTR)            :: cname
    integer                           :: stat
    type(model_internalstate_wrapper) :: is
    logical                           :: configIsPresent
    type(ESMF_Config)                 :: config
    type(NUOPC_FreeFormat)            :: attrFF
    character(ESMF_MAXSTR)            :: tmpStr

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! allocate memory for this internal state and set it in the
    ! component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      method=METHOD, file=__FILE__, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! check gcomp for config
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    if (configIsPresent) then

      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out  else

      ! read and ingest free format component attributes
      attrFF = NUOPC_FreeFormatCreate(config, &
        label=trim(cname)//"_attributes::", relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    endif

    call ESMF_AttributeGet(gcomp, name="verbosity", value=tmpStr, &
      defaultValue="default", convention="NUOPC", purpose="Instance", &
      rc=rc)
    is%wrap%verbosity = ESMF_UtilString2Int(tmpStr, &
      specialStringList=(/"default","none","max"/), &
      specialValueList=(/is%wrap%verbosity,0,255/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call ESMF_AttributeGet(gcomp, name="time_step_seconds", value=tmpStr, &
      defaultValue="default", convention="NUOPC", purpose="Instance", &
      rc=rc)
    is%wrap%timeStepSeconds = ESMF_UtilString2Int(tmpStr, &
      specialStringList=(/"default"/), &
      specialValueList=(/is%wrap%timeStepSeconds/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call ESMF_AttributeGet(gcomp, name="coupled_forcing", value=tmpStr, &
      defaultValue="false", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    is%wrap%coupledForcing= (trim(tmpStr)=="true")

    call ESMF_AttributeGet(gcomp, name="realize_all_export", value=tmpStr, &
      defaultValue="false", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    is%wrap%realizeAllExport = (trim(tmpStr)=="true")

    call ESMF_AttributeGet(gcomp, name="restart_seconds", value=tmpStr, &
      defaultValue="default", convention="NUOPC", purpose="Instance", &
      rc=rc)
    is%wrap%restartSeconds = ESMF_UtilString2Int(tmpStr, &
      specialStringList=(/"default"/), &
      specialValueList=(/is%wrap%restartSeconds/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    if (is%wrap%verbosity .gt. 0) then

      write (tmpStr, "(A,A)") trim(cname)//': ', &
        'Component Attributes'
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)
      write (tmpStr, "(A,A20,I0)") trim(cname)//': ', &
        'Verbosity: ',is%wrap%verbosity
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

      write (tmpStr, "(A,A20,I0)") trim(cname)//': ', &
        'Time Step: ',is%wrap%timeStepSeconds
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

      write (tmpStr, "(A,A20,L1)") trim(cname)//': ', &
        'Coupled Forcing: ',is%wrap%coupledForcing
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

      write (tmpStr, "(A,A20,L1)") trim(cname)//': ', &
        'Realize All Exp: ',is%wrap%realizeAllExport
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

      write (tmpStr, "(A,A20,I0)") trim(cname)//': ', &
        'Restart Interval: ',is%wrap%restartSeconds
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

    endif

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP1"

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)               :: cname
    type(model_internalstate_wrapper)    :: is
    integer                              :: stat
    integer                              :: i
    type(ESMF_StateItem_Flag)            :: itemType
    character(len=20)                    :: stateName
    character(len=10)                    :: units
    character(ESMF_MAXSTR)               :: tmpStr

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call model_init(offline=.NOT.is%wrap%coupledForcing, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    if (is%wrap%coupledForcing) then

      do i=1, size(import_list)

        ! get stateName and units from model by standard name
        call model_field_get(trim(import_list(i)), &
          stateName=stateName, units=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out

        ! advertise import
        call NUOPC_Advertise(importState, &
          StandardName=trim(import_list(i)), &
          Units=trim(units), &
          name=trim(stateName), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out

      enddo

    endif

    do i=1, size(export_list)

      ! get stateName and units from model by standard name
      call model_field_get(trim(export_list(i)), &
        stateName=stateName, units=units, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      ! advertise export
      call NUOPC_Advertise(exportState, &
        StandardName=trim(export_list(i)), &
        Units=trim(units), &
        name=trim(stateName), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    enddo

    if (is%wrap%verbosity .gt. 0) then

      ! Report advertised import fields
      write(tmpStr,'(A,A)') trim(cname)//': ', &
        'List of advertised import fields'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      write(tmpStr,'(A,A20,A,A10)') trim(cname)//': ', &
        'standardName',' ','stateName'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      do i=1, size(import_list)

        call model_field_get(trim(import_list(i)), &
          stateName=stateName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        call ESMF_StateGet(importState, itemName=stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        if (itemType /= ESMF_STATEITEM_NOTFOUND) then
          write(tmpStr,'(A,A20,A,A10)') TRIM(cname)//': ', &
           trim(import_list(i)),' ',trim(stateName)
          call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
        endif

      enddo

      ! Report advertised export fields
      write(tmpStr,'(A,A)') trim(cname)//': ', &
        'List of advertised export fields'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      write(tmpStr,'(A,A20,A,A10)') trim(cname)//': ', &
        'standardName',' ','stateName'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      do i=1, size(export_list)

        call model_field_get(trim(export_list(i)), &
          stateName=stateName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        call ESMF_StateGet(exportState, itemName=stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        if (itemType /= ESMF_STATEITEM_NOTFOUND) then
          write(tmpStr,'(A,A20,A,A10)') TRIM(cname)//': ', &
           trim(export_list(i)),' ',trim(stateName)
          call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
        endif

      enddo

    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP3"

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)               :: cname
    type(model_internalstate_wrapper)    :: is
    integer                              :: stat
    integer                              :: i
    type(ESMF_StateItem_Flag)            :: itemType
    character(len=20)                    :: stateName
    type(ESMF_Field)                     :: field
    character(ESMF_MAXSTR)               :: tmpStr

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    if (.NOT.ESMF_GridIsCreated(model_grid)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_NOT_CREATED, &
        msg='Model grid has not been created.', &
        method=METHOD, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

    ! realize connected Fields in the importState
    do i=1, size(import_list)

      ! get stateName from model by standard name
      call model_field_get(trim(import_list(i)), stateName=stateName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      if (NUOPC_IsConnected(importState, fieldName=stateName)) then

        ! get Field from model by standard name
        call model_field_get(trim(import_list(i)), field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out

        if (.NOT.ESMF_FieldIsCreated(field)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_NOT_CREATED, &
            msg=trim(import_list(i))//' has not been created.', &
            method=METHOD, file=__FILE__, rcToReturn=rc)
            return ! bail out
        endif

        ! realize the connected Field using the just created Field
        call NUOPC_Realize(importState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out

      elseif (is%wrap%coupledForcing) then

        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_FOUND, &
          msg=trim(import_list(i))//' missing coupled forcing connection', &
          method=METHOD, file=__FILE__, rcToReturn=rc)
          return ! bail out      

      else

        ! remove a not connected Field from State
        call ESMF_StateRemove(importState, (/stateName/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
      
      endif

    enddo

    ! realize connected Fields in the exportState
    do i=1, size(export_list)

      ! get stateName from model by standard name
      call model_field_get(trim(export_list(i)), stateName=stateName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      if (NUOPC_IsConnected(exportState, fieldName=stateName) &
        .OR. is%wrap%realizeAllExport) then

        ! get Field from model by standard name
        call model_field_get(trim(export_list(i)), field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out

        if (.NOT.ESMF_FieldIsCreated(field)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_NOT_CREATED, &
          msg=trim(export_list(i))//' has not been created.', &
          method=METHOD, file=__FILE__, rcToReturn=rc)
          return ! bail out
        endif

        ! realize the connected Field using the just created Field
        call NUOPC_Realize(exportState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
      
      else

        ! remove a not connected Field from State
        call ESMF_StateRemove(exportState, (/stateName/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
      
      endif

    enddo

    if (is%wrap%verbosity .gt. 0) then

      ! Report realized import fields
      write(tmpStr,'(A,A)') trim(cname)//': ', &
        'List of realized import fields'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      write(tmpStr,'(A,A20,A,A10)') trim(cname)//': ', &
        'standardName',' ','stateName'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      do i=1, size(import_list)

        call model_field_get(trim(import_list(i)), &
          stateName=stateName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        call ESMF_StateGet(importState, itemName=stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        if (itemType /= ESMF_STATEITEM_NOTFOUND) then
          write(tmpStr,'(A,A20,A,A10)') TRIM(cname)//': ', &
           trim(import_list(i)),' ',trim(stateName)
          call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
        endif

      enddo

      ! Report realized export fields
      write(tmpStr,'(A,A)') trim(cname)//': ', &
        'List of realized export fields'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      write(tmpStr,'(A,A20,A,A10)') trim(cname)//': ', &
        'standardName',' ','stateName'
      call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
      do i=1, size(export_list)

        call model_field_get(trim(export_list(i)), &
          stateName=stateName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        call ESMF_StateGet(exportState, itemName=stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        if (itemType /= ESMF_STATEITEM_NOTFOUND) then
          write(tmpStr,'(A,A20,A,A10)') TRIM(cname)//': ', &
           trim(export_list(i)),' ',trim(stateName)
          call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
        endif

      enddo

    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "DataInitialize"

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)                 :: cname
    type(model_internalstate_wrapper)      :: is
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: currTime
    logical                                :: allDepStsfy
    integer                                :: i
    integer                                :: itemCount
    character(len=80),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat
    character(ESMF_MAXSTR)                 :: tmpStr

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    allDepStsfy = .TRUE.

    call ESMF_StateGet(importState,itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    allocate( &
      itemNameList(itemCount), &
      itemTypeList(itemCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(importState, &
      itemNameList=itemNameList, &
      itemTypeList=itemTypeList, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    do i=1, itemCount
      if ( itemTypeList(i) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, field=field, &
          itemName=itemNameList(i),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        if (.NOT.NUOPC_IsAtTime(field, currTime)) then
          if (is%wrap%verbosity .gt. 1) then
            write(tmpStr,'(a,a16,a)') trim(cname)//': ', &
              trim(itemNameList(i)), &
              ' inter-model initialization dependency SATISFIED.'
            call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
          endif
        else
          allDepStsfy = .FALSE.
          if (is%wrap%verbosity .gt. 1) then
            write(tmpStr,'(a,a16,a)') trim(cname)//': ', &
              trim(itemNameList(i)), &
              ' inter-model initialization dependency NOT SATISFIED.'
            call ESMF_LogWrite(trim(tmpStr), ESMF_LOGMSG_INFO)
          endif
        endif
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of state item list memory failed.", &
        line=__LINE__, file=__FILE__)) return  ! bail out

    ! if not all import dependencies are satisfied, then return
    if (.NOT.allDepStsfy) then

      if (is%wrap%verbosity .gt. 0) then
        write(tmpStr,'(a,a)') trim(cname)//': ', &
          'all inter-model initialization dependencies NOT SATISFIED'
        call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)
      endif

    else

      if (is%wrap%verbosity .gt. 0) then
        write(tmpStr,'(a,a)') trim(cname)//': ', &
          'all inter-model initialization dependencies SATISFIED'
        call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)
      endif

      call ESMF_StateGet(exportState,itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      allocate( &
        itemNameList(itemCount), &
        itemTypeList(itemCount), &
        stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of state item list memory failed.", &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call ESMF_StateGet(exportState, &
        itemNameList=itemNameList, &
        itemTypeList=itemTypeList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      do i=1, itemCount
        if ( itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(exportState, field=field, &
            itemName=itemNameList(i),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return ! bail out

          call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return ! bail out
        endif
      enddo

      deallocate(itemNameList, itemTypeList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of state item list memory failed.", &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! set InitializeDataComplete Attribute to "true", indicating to the
      ! generic code that all inter-model initialization dependencies are 
      ! satisfied
      call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", &
        value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "SetClock"

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)            :: cname
    type(model_internalstate_wrapper) :: is
    type(ESMF_Clock)                  :: clock
    type(ESMF_TimeInterval)           :: timeStep

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    if (is%wrap%timeStepSeconds .ne. 0) then
      ! create a new timeStep
      call ESMF_TimeIntervalSet(timeStep, &
        s=is%wrap%timeStepSeconds, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      ! query the Component for its clock
      call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      ! override the clock's default timeStep
      call ESMF_ClockSet(clock, timeStep=timeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
    endif

    ! create the restart write timeStep
    if (is%wrap%restartSeconds .ne. 0) then
      call ESMF_TimeIntervalSet(is%wrap%restartInterval, &
        s=is%wrap%restartSeconds, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
    else
      call ESMF_TimeIntervalSet(is%wrap%restartInterval, &
        s=huge(0), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
    endif

    ! reset the restart write tracker
    call ESMF_TimeIntervalSet(is%wrap%restartTracker, &
      s=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "ModelAdvance"

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)            :: cname
    type(model_internalstate_wrapper) :: is
    type(ESMF_Clock)                  :: clock
    type(ESMF_State)                  :: importState, exportState
    type(ESMF_Time)                   :: currTime, stopTime
    character(len=30)                 :: stopTimeStr
    type(ESMF_TimeInterval)           :: timeStep
    integer                           :: i, itemCount
    type(ESMF_Field)                  :: field

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call ESMF_ClockGet(clock, stopTime=stopTime, currTime=currTime, &
      timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    call model_run(offline=.NOT.is%wrap%CoupledForcing, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    is%wrap%restartTracker = is%wrap%restartTracker + timeStep

    if (is%wrap%restartTracker >= is%wrap%restartInterval) then

      call ESMF_TimeGet(stopTime, timeString=stopTimeStr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      ! write out the Fields in the importState
      call NUOPC_Write(importState, &
        fileNamePrefix=trim(cname)//"_RSTRT_"//trim(stopTimeStr)//"_", &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      ! write out the Fields in the exportState
      call NUOPC_Write(exportState, &
        fileNamePrefix=trim(cname)//"_RSTRT_"//trim(stopTimeStr)//"_", &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      ! reset restart write tracker
      call ESMF_TimeIntervalSet(is%wrap%restartTracker, &
        s=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    endif

    ! advance the time slice counter
    is%wrap%slice = is%wrap%slice + 1

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "ModelFinalize"

  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)            :: cname
    type(model_internalstate_wrapper) :: is

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call model_finalize(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

end module sample_cap_mod
