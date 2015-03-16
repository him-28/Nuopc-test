module esmfBmiAdapter

    use ESMF
    use BmiDefinitions

    implicit none

    private

    public BMIAdapter_SetModel, &
        BMIAdapter_Initialize, &
        BMIAdapter_Update, &
        BMIAdapter_Finalize, &
        BMIAdapter_ESMFClockCreate, &
        BMIAdapter_ESMFFieldCreate, &
        BMIAdapter_ESMFGridCreate, &
        BMIAdapter_FieldUnitsGet, &
        BMIAdapter_ImportFieldListGet, &
        BMIAdapter_ExportFieldListGet, &
        BMIAdapter_GridComparison

    public bmiInitialize, &
        bmiUpdate, &
        bmiFinalize, &
        bmiGetStartTime, &
        bmiGetEndTime, &
        bmiGetCurrentTime, &
        bmiGetTimeStep, &
        bmiGetTimeUnits, &
        bmiGetVarType, &
        bmiGetVarUnits, &
        bmiGetVarRank, &
        bmiGetGridType, &
        bmiGetGridShape, &
        bmiGetGridSpacing, &
        bmiGetGridOrigin, &
        bmiGetDouble, &
        bmiGetDoubleAt, &
        bmiSetDouble, &
        bmiSetDoubleAt, &
        bmiGetInputVarNames, &
        bmiGetOutputVarNames, &
        bmiGetComponentName

    interface
        subroutine bmiInitialize(config_file)
            character(*), intent(in) :: config_file
        end subroutine
        subroutine bmiUpdate()
        end subroutine
        subroutine bmiFinalize()
        end subroutine
        subroutine bmiGetStartTime(start)
            real,intent(out) :: start
        end subroutine
        subroutine bmiGetEndTime(end)
            real, intent (out) :: end
        end subroutine
        subroutine bmiGetCurrentTime(time)
            real, intent (out) :: time
        end subroutine
        subroutine bmiGetTimeStep(dt)
            real, intent (out) :: dt
        end subroutine
        subroutine bmiGetTimeUnits(units)
            character (len=*), intent (out) :: units
        end subroutine
        subroutine bmiGetVarType(var_name, type)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: type
        end subroutine
        subroutine bmiGetVarUnits(var_name, units)
            character (len=*), intent (in) :: var_name
            character (len=*), intent (out) :: units
        end subroutine
        subroutine bmiGetVarRank(var_name, rank)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: rank
        end subroutine
        subroutine bmiGetGridType(var_name, type)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: type
        end subroutine
        subroutine bmiGetGridShape(var_name, shape)
            character (len=*), intent (in) :: var_name
            integer, dimension (:), intent (out) :: shape
        end subroutine
        subroutine bmiGetGridSpacing(var_name, spacing)
            character (len=*), intent (in) :: var_name
            real, dimension (:), intent (out) :: spacing
        end subroutine
        subroutine bmiGetGridOrigin(var_name, origin)
            character (len=*), intent (in) :: var_name
            real, dimension (:), intent (out) :: origin
        end subroutine
        subroutine bmiGetDouble(var_name, dest)
            character (len=*), intent (in) :: var_name
            real, pointer, intent (inout) :: dest(:)
        end subroutine
        subroutine bmiGetDoubleAt(var_name, dest, inds)
            character (len=*), intent (in) :: var_name
            real, pointer, intent (inout) :: dest(:)
            integer, intent (in) :: inds(:)
        end subroutine
        subroutine bmiSetDouble(var_name, src)
            character (len=*), intent (in) :: var_name
            real, intent (in) :: src (*)
        end subroutine
        subroutine bmiSetDoubleAt(var_name, inds, src)
            character (len=*), intent (in) :: var_name
            integer, intent (in) :: inds(:)
            real, intent (in) :: src (*)
        end subroutine
        subroutine bmiGetInputVarNames(names)
            character (*),pointer, intent (out) :: names(:)
        end subroutine
        subroutine bmiGetOutputVarNames(names)
            character (*),pointer, intent (out) :: names(:)
        end subroutine
        subroutine bmiGetComponentName(name)
            character (len=*),pointer, intent (out) :: name
        end subroutine
    end interface

    type :: ModelState
        private
        logical :: set = .false.
        logical :: initialized = .false.
        logical :: finalized = .false.
    end type ModelState

    procedure(bmiInitialize), pointer :: pBmiInitialize => null()
    procedure(bmiUpdate), pointer :: pBmiUpdate => null()
    procedure(bmiFinalize), pointer :: pBmiFinalize => null()
    procedure(bmiGetStartTime), pointer :: pBmiGetStartTime => null()
    procedure(bmiGetEndTime), pointer :: pBmiGetEndTime => null()
    procedure(bmiGetCurrentTime), pointer :: pBmiGetCurrentTime => null()
    procedure(bmiGetTimeStep), pointer :: pBmiGetTimeStep => null()
    procedure(bmiGetTimeUnits), pointer :: pBmiGetTimeUnits => null()
    procedure(bmiGetVarType), pointer :: pBmiGetVarType => null()
    procedure(bmiGetVarUnits), pointer :: pBmiGetVarUnits => null()
    procedure(bmiGetVarRank), pointer :: pBmiGetVarRank => null()
    procedure(bmiGetGridType), pointer :: pBmiGetGridType => null()
    procedure(bmiGetGridShape), pointer :: pBmiGetGridShape => null()
    procedure(bmiGetGridSpacing), pointer :: pBmiGetGridSpacing => null()
    procedure(bmiGetGridOrigin), pointer :: pBmiGetGridOrigin => null()
    procedure(bmiGetDouble), pointer :: pBmiGetDouble => null()
    procedure(bmiGetDoubleAt), pointer :: pBmiGetDoubleAt => null()
    procedure(bmiSetDouble), pointer :: pBmiSetDouble => null()
    procedure(bmiSetDoubleAt), pointer :: pBmiSetDoubleAt => null()
    procedure(bmiGetInputVarNames), pointer :: pBmiGetInputVarNames => null()
    procedure(bmiGetOutputVarNames), pointer :: pBmiGetOutputVarNames => null()
    procedure(bmiGetComponentName), pointer :: pBmiGetComponentName => null()

    type(ModelState) :: state
!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------

    !##################################
    !# Section I:                     #
    !# Model Setup Procedures         #
    !##################################

    subroutine BMIAdapter_SetModel(initialize,finalize,update, getStartTime, getEndTime, getCurrentTime, getTimeStep, getTimeUnits, getVarType, getVarUnits, getVarRank, getGridType, getGridShape, getGridSpacing, getGridOrigin, getDouble, getDoubleAt, setDouble, setDoubleAt, getInputVarNames, getOutputVarNames, getComponentName, rc)
        procedure(bmiInitialize) :: initialize
        procedure(bmiUpdate) :: update
        procedure(bmiFinalize) :: finalize
        procedure(bmiGetStartTime) :: getStartTime
        procedure(bmiGetEndTime) :: getEndTime
        procedure(bmiGetCurrentTime) :: getCurrentTime
        procedure(bmiGetTimeStep) :: getTimeStep
        procedure(bmiGetTimeUnits) :: getTimeUnits
        procedure(bmiGetVarType) :: getVarType
        procedure(bmiGetVarUnits) :: getVarUnits
        procedure(bmiGetVarRank) :: getVarRank
        procedure(bmiGetGridType) :: getGridType
        procedure(bmiGetGridShape) :: getGridShape
        procedure(bmiGetGridSpacing) :: getGridSpacing
        procedure(bmiGetGridOrigin) :: getGridOrigin
        procedure(bmiGetDouble) :: getDouble
        procedure(bmiGetDoubleAt) :: getDoubleAt
        procedure(bmiSetDouble) :: setDouble
        procedure(bmiSetDoubleAt) :: setDoubleAt
        procedure(bmiGetInputVarNames) :: getInputVarNames
        procedure(bmiGetOutputVarNames) :: getOutputVarNames
        procedure(bmiGetComponentName) :: getComponentName
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        pBmiInitialize => initialize
        pBmiUpdate => update
        pBmiFinalize => finalize
        pBmiGetTimeStep => getTimeStep
        pBmiGetStartTime => getStartTime
        pBmiGetEndTime => getEndTime
        pBmiGetCurrentTime => getCurrentTime
        pBmiGetTimeUnits => getTimeUnits
        pBmiGetVarType => getVarType
        pBmiGetVarUnits => getVarUnits
        pBmiGetVarRank => getVarRank
        pBmiGetGridType => getGridType
        pBmiGetGridShape => getGridShape
        pBmiGetGridSpacing => getGridSpacing
        pBmiGetGridOrigin => getGridOrigin
        pBmiGetDouble => getDouble
        pBmiGetDoubleAt => getDoubleAt
        pBmiSetDouble => setDouble
        pBmiSetDoubleAt => setDoubleAt
        pBmiGetInputVarNames => getInputVarNames
        pBmiGetOutputVarNames => getOutputVarNames
        pBmiGetComponentName => getComponentName

        if(associated(pBmiInitialize) .and. &
            associated(pBmiUpdate) .and. &
            associated(pBmiFinalize) .and. &
            associated(pBmiGetStartTime) .and. &
            associated(pBmiGetEndTime) .and. &
            associated(pBmiGetCurrentTime) .and. &
            associated(pBmiGetTimeStep) .and. &
            associated(pBmiGetTimeUnits) .and. &
            associated(pBmiGetVarType) .and. &
            associated(pBmiGetVarUnits) .and. &
            associated(pBmiGetVarRank) .and. &
            associated(pBmiGetGridType) .and. &
            associated(pBmiGetGridShape) .and. &
            associated(pBmiGetGridSpacing) .and. &
            associated(pBmiGetGridOrigin) .and. &
            associated(pBmiGetDouble) .and. &
            associated(pBmiGetDoubleAt) .and. &
            associated(pBmiSetDouble) .and. &
            associated(pBmiSetDoubleAt) .and. &
            associated(pBmiGetInputVarNames) .and. &
            associated(pBmiGetOutputVarNames) .and. &
            associated(pBmiGetComponentName)) then

            state%set = .true.
            call BMIAdapter_LogWrite("Model set.", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, msg="Model set failed! Check external model.", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

    end subroutine BMIAdapter_SetModel

    !#########################################
    !# Section II:                           #
    !# Model control flow adapter procedures #
    !#########################################

    !========================================
    ! Initialize BMI model
    !========================================

    subroutine BMIAdapter_Initialize(file,rc)
        character(*),intent(in) :: file
        integer,intent(out) :: rc

        ! Local Variables
        character(BMI_MAXCOMPNAMESTR), pointer :: compname
        real :: timeStep, timeEnd

        rc = ESMF_SUCCESS

        if (state%set) then

            call pBmiInitialize(file)

            ! To be discussed: Initialization requirements
            call pBmiGetComponentName(compname)
            call pBmiGetTimeStep(timeStep)
            call pBmiGetEndTime(timeEnd)

            if( timeStep > 0 .and. timeEnd > 0) then
                state%initialized = .true.
                call BMIAdapter_LogWrite("Model initialized <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
                if (rc .ne. ESMF_SUCCESS) return
                call BMIAdapter_PrintComponentInfo(rc) ! Print BMI information after initializing
                call BMIAdapter_PrintAllVarInfo(rc)
            else
                rc = ESMF_RC_OBJ_INIT
                if (BMIAdapter_LogFoundError(rcToCheck=rc, msg="Model initialization failed <" // trim(compname) //">! Check model configuration.", &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
            end if
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, msg="Cannot initialize model before model is set!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

    end subroutine BMIAdapter_Initialize

    !========================================
    ! Update BMI model
    !========================================

    subroutine BMIAdapter_Update(rc)
        integer, intent(out) :: rc
        character(BMI_MAXCOMPNAMESTR),pointer :: compname

        rc = ESMF_SUCCESS

        if (state%initialized) then
            if(.not. state%finalized) then
                call pBmiUpdate() ! Update BMI
                ! call BMIAdapter_PrintCurrentTime() ! Print BMI model time after update
                call pBmiGetComponentName(compname)
                call BMIAdapter_LogWrite("Model updated <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
                if (rc .ne. ESMF_SUCCESS) return
                call BMIAdapter_PrintCurrentTime(rc)
            else
                rc = ESMF_RC_OBJ_INIT
                if (BMIAdapter_LogFoundError(rcToCheck=rc, msg="Cannot update model after finalization!", &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
            end if
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, msg="Cannot update model before model is initialized!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if
    end subroutine BMIAdapter_Update

    !========================================
    ! Finalize BMI model
    !========================================

    subroutine BMIAdapter_Finalize(rc)
        integer, intent(out) :: rc
        character(BMI_MAXCOMPNAMESTR),pointer :: compname

        rc = ESMF_SUCCESS

        if(state%initialized) then
            call pBmiFinalize() ! Finalize BMI
            call pBmiGetComponentName(compname)
            state%finalized = .true.
            call BMIAdapter_LogWrite("model finalized <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        else
            rc = ESMF_RC_OBJ_INIT
            if (BMIAdapter_LogFoundError(rcToCheck=rc, msg="Cannot finalize model before model is initialized!", &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
        end if

    end subroutine BMIAdapter_Finalize

    !#################
    !# Variable Info #
    !#################

    function BMIAdapter_ImportFieldListGet(rc) result(list)
        integer, intent(out) :: rc
        character(len=BMI_MAXVARNAMESTR),dimension(:),allocatable    :: list
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetInputVarNames(varlist)

        allocate(list(size(varlist)))
        list=varlist

    end function BMIAdapter_ImportFieldListGet

    function BMIAdapter_ExportFieldListGet(rc) result(list)
        integer, intent(out) :: rc
        character(len=BMI_MAXVARNAMESTR),dimension(:),allocatable    :: list
        character(len=BMI_MAXVARNAMESTR),pointer :: varlist(:)

        rc = ESMF_SUCCESS
        call pBmiGetOutputVarNames(varlist)
        allocate(list(size(varlist)))
        list=varlist

    end function BMIAdapter_ExportFieldListGet

    function BMIAdapter_FieldUnitsGet(field,rc) result(units)
        integer, intent(out) :: rc
        character(*),intent(in) :: field
        character(len=BMI_MAXUNITSSTR) :: units

        rc = ESMF_SUCCESS
        call pBmiGetVarUnits(field,units)

    end function BMIAdapter_FieldUnitsGet

    function BMIAdapter_FieldRank(varname,rc) result(rank)
        integer, intent(out) :: rc
        character(*),intent(in) :: varname
        integer :: rank

        rc = ESMF_SUCCESS
        call pBmiGetVarRank(varname,rank)
    end function BMIAdapter_FieldRank

    !######################################
    !# Create ESMF objects based on model #
    !######################################

    !=================================
    ! Create ESMF Clock based on model
    !=================================

    function BMIAdapter_ESMFClockCreate(refClock, rc) result(clock)
        ! reference clock. usually the system clock
        type(ESMF_Clock),intent(in)  :: refClock
        ! instantiate a clock
        type(ESMF_Clock) :: clock

        ! instantiate time_step, start and stop times
        type(ESMF_TimeInterval) :: timeStep,startDelay,endReference
        type(ESMF_Time) :: refStartTime, startTime, stopTime

        ! local variables for Get methods
        real :: step,start,end
        character(len=10) :: units

        ! return code
        integer :: rc

        rc = ESMF_SUCCESS

        call pBmiGetTimeStep(step)
        call pBmiGetStartTime(start)
        call pBmiGetEndTime(end)
        call pBmiGetTimeUnits(units)

          ! Put warning in for truncation
        if (units .eq. 's') then
            call ESMF_TimeIntervalSet(timeStep, s = INT(step), rc=rc)
            call ESMF_TimeIntervalSet(startDelay, s = INT(start),rc=rc)
            call ESMF_TimeIntervalSet(endReference, s = INT(end),rc=rc)
        else if(units .eq. 'm') then
            call ESMF_TimeIntervalSet(timeStep, m = INT(step), rc=rc)
            call ESMF_TimeIntervalSet(startDelay, m = INT(start),rc=rc)
            call ESMF_TimeIntervalSet(endReference, m = INT(end),rc=rc)
        else
            rc = ESMF_RC_ARG_VALUE
        end if

        call ESMF_ClockGet(refClock,startTime=refStartTime,rc=rc)

        startTime = refStartTime + startDelay
        stopTime = refStartTime + endReference

        ! initialize the clock with the above values
        clock = ESMF_ClockCreate(timeStep, startTime=startTime, stopTime=stopTime, &
            name="bmiclock1", rc=rc)

    end function BMIAdapter_ESMFClockCreate

    !==========================================
    ! Create ESMF Field based on model variable
    !==========================================

    function BMIAdapter_ESMFFieldCreate(grid,name,rc) result(field)
        type(ESMF_Field) :: field
        type(ESMF_Grid),intent(in) :: grid
        character(*),intent(in) :: name
        integer :: rc
        real,pointer:: bmi1d(:)
        real,pointer:: bmi2d(:,:)
        real,pointer:: arrayportion(:,:)
        integer, dimension (2) :: gridshape
        integer, dimension(2) :: gec

        rc = ESMF_SUCCESS

        call pBmiGetGridShape ( name, gridshape)

        call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
            exclusiveCount=gec, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call pBmiGetDouble(name,bmi1d)

        bmi2d (1:gridshape(1), 1:gridshape(2)) => bmi1d
        arrayportion => bmi2d(1:gec(1),1:gec(2))
        !bmi2d = reshape(bmi1d,(/gec(1),gec(2)/))

        field = ESMF_FieldCreate(grid, farrayptr=arrayportion,name=name, rc=rc)
    end function BMIAdapter_ESMFFieldCreate

    !========================================
    ! Create grid based on variable
    !========================================

    function BMIAdapter_ESMFGridCreate(varname,rc) result(return_grid)
        type(ESMF_Grid) :: return_grid
        integer, intent(out) :: rc
        character(*),intent(in) ::varname

        ! Local Variables
        integer :: rank

        rc = ESMF_SUCCESS
        return_grid = privateGridCreate(varname,BMIAdapter_FieldRank(varname,rc),rc)
    end function BMIAdapter_ESMFGridCreate

    function privateGridCreate(varname,rank,rc) result(return_grid)
        type(ESMF_Grid) :: return_grid
        integer, intent(out) :: rc
        integer, intent(in) :: rank
        character(*),intent(in) ::varname

        ! local variables
        integer :: gridtype
        real :: gridspacing(1:rank)
        real :: gridorigin(1:rank)
        integer :: gridshape(1:rank)
        integer :: x_min, x_max, y_min, y_max


        rc = ESMF_SUCCESS

        call pBmiGetGridType (varname,gridtype)
        call pBmiGetGridOrigin (varname, gridorigin)
        call pBmiGetGridSpacing (varname,gridspacing)
        call pBmiGetGridShape (varname,gridshape)

        ! If grid is not uniform then FAILURE.  Logic to create non-uniform
        ! grids is not yet implemented
        select case (gridtype)
            case(BMI_GRID_TYPE_UNIFORM)
                if(rank .eq. 2) then
                    ! To be reviewed
                    x_min = gridorigin(1)
                    x_max = gridorigin(1)+gridshape(1)-1
                    y_min = gridorigin(2)
                    y_max = gridorigin(2)+gridshape(2)-1

                    ! ESMF_GridCreateNoPeriDim - Create a Grid with no periodic dim and a regular distribution
                    ! Temporarily no decomp
                    return_grid = ESMF_GridCreateNoPeriDim(minIndex=(/x_min,y_min/), maxIndex=(/x_max,y_max/), &
                        regDecomp=(/1,1/), name="bmi_uniform2d", rc=rc)
!                    return_grid = NUOPC_GridCreateSimpleXY(x_min, y_min, x_max, y_max, &
!                        i_count, j_count, rc)
                    if (rc .ne. ESMF_SUCCESS) return
                    call BMIAdapter_LogWrite("model grid created <bmi_uniform2d>", ESMF_LOGMSG_INFO, rc=rc)
                    if (rc .ne. ESMF_SUCCESS) return
                else
                    rc = ESMF_RC_NOT_IMPL
                    return
                end if
            case(BMI_GRID_TYPE_RECTILINEAR)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_STRUCTURED)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_UNSTRUCTURED)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_NUMBER)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_UNKNOWN)
                rc = ESMF_RC_NOT_IMPL
                return
            case default
                rc = ESMF_RC_NOT_IMPL
                return
        end select

    END FUNCTION privateGridCreate

    !========================================
    ! Grid Comparison Function for BMI variables
    !========================================

    function BMIAdapter_GridComparison(var_name_1,var_name_2,rc) result (equivalent)
        character (BMI_MAXVARNAMESTR), intent(in) :: var_name_1,var_name_2
        real :: spacing_1(1:2), spacing_2(1:2)
        real :: origin_1(1:2), origin_2(1:2)
        integer :: shape_1(1:2), shape_2(1:2)
        integer :: gridtype_1, gridtype_2
        logical :: equivalent
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS
        equivalent = .true.

        call pBmiGetGridOrigin (var_name_1,origin_1)
        call pBmiGetGridOrigin (var_name_2, origin_2)

        if(origin_1(1) .ne. origin_2 (1) .or. origin_1(2) .ne. origin_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridSpacing (var_name_1,spacing_1)
        call pBmiGetGridSpacing (var_name_2, spacing_2)

        if(spacing_1(1) .ne. spacing_2 (1) .or. spacing_1(2) .ne. spacing_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridShape (var_name_1,shape_1)
        call pBmiGetGridShape (var_name_2,shape_2)

        if(shape_1(1) .ne. shape_2 (1) .or. shape_1(2) .ne. shape_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridType(var_name_2,gridtype_1)
        call pBmiGetGridType(var_name_1,gridtype_2)

        if(gridtype_1 .ne. gridtype_2) then
            equivalent = .false.
            return
        end if

    end function BMIAdapter_GridComparison

    !###########################
    !# Section nnnn            #
    !# Standard Out Procedures #
    !###########################

    !=========================
    !=Print Model Information=
    !=========================

    subroutine BMIAdapter_PrintComponentInfo(rc)
        implicit none
        character(len=BMI_MAXVARNAMESTR), pointer        :: invarnames(:)
        character(len=BMI_MAXVARNAMESTR), pointer     :: outvarnames(:)
        character(len=BMI_MAXCOMPNAMESTR),pointer    :: compname
        character(len=10)                               ::tunits
        real                            :: start
        real                            :: end
        real                            :: step
        integer :: i
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        call pBmiGetInputVarNames(invarnames)
        call pBmiGetOutputVarNames(outvarnames)
        call pBmiGetComponentName(compname)
        call pBmiGetTimeStep(step)
        call pBmiGetStartTime(start)
        call pBmiGetEndTime(end)
        call pBmiGetTimeUnits(tunits)

        print *, "BMI Component Info"
        print *, "     Step: ",step
        print *, "     Start: ",start
        print *, "     End: ",end
        print *, "     Time Units: ",tunits
        print *, "     # Input Variables: ",SIZE(invarnames)
        print *, "     Input Variable Names: ",invarnames
        print *, "     # Output Variables: ",SIZE(outvarnames)
        print *, "     Output Variable Names: ",outvarnames
        print *, "     Component Name: ",compname
    end subroutine

    !========================================
    ! Print all available info for a sigle
    ! variable
    !========================================

    subroutine BMIAdapter_PrintVarInfo(var_name,var_rank,rc)
        implicit none
        character (len=BMI_MAXVARNAMESTR), intent (in) :: var_name
        integer, intent(in) :: var_rank
        integer, intent(out)     :: rc
        integer             :: type
        character(len=10)   :: units, dict_units ! Assumed length for units string
        integer             :: gtype
        integer     :: gshape(1:var_rank)   ! Assumed shape for grid shape array
        real        :: gspacing(1:var_rank), gorigin(1:var_rank) ! Assumed shape for grid spacing array

        rc = ESMF_SUCCESS

        call pBmiGetVarType ( var_name, type)
        call pBmiGetVarUnits ( var_name, units)
        call pBmiGetGridType ( var_name, gtype)
        call pBmiGetGridShape ( var_name, gshape)
        call pBmiGetGridSpacing ( var_name, gspacing)
        call pBmiGetGridOrigin ( var_name, gorigin)

        print *, "     Type: ", type
        print *, "     Units: ", units
        print *, "     Rank: ", var_rank
        print *, "     Grid Type: ", gtype
        print *, "     Grid Shape: ", gshape
        print *, "     Grid Spacing: ", gspacing
        print *, "     Grid Origin: ", gorigin

    end subroutine

    !========================================
    ! Print Field Value
    !========================================

    SUBROUTINE BMIAdapter_PrintFieldArray(state,varname,rc)
        type(ESMF_State),intent(in) :: state
        character(*),intent(in) :: varname
        integer,intent(out) :: rc

        type(ESMF_Field) :: field
        type(ESMF_Array) :: array

        rc = ESMF_SUCCESS

        call ESMF_StateGet(state,itemName=varname,field=field,rc=rc)
        if(rc .ne. ESMF_SUCCESS) return
        call ESMF_FieldGet(field, array=array, rc=rc)
        if(rc .ne. ESMF_SUCCESS) return
        call ESMF_ArrayPrint(array, rc=rc)
        if(rc .ne. ESMF_SUCCESS) return

    END SUBROUTINE

    !========================================
    ! Print BMI model current time
    !========================================

    SUBROUTINE BMIAdapter_PrintCurrentTime(rc)
        real                          :: bmi_time
        integer,intent(out) :: rc

        rc = ESMF_SUCCESS

        call pBmiGetCurrentTime(bmi_time)
        print *,"BMI Current Time: ", bmi_time

    END SUBROUTINE

    !========================================
    ! Iterate over all input and output
    ! variables and print info.
    !========================================

    subroutine BMIAdapter_PrintAllVarInfo(rc)
        implicit none
        integer,intent(out) :: rc
        character(len=BMI_MAXVARNAMESTR), pointer    :: invarnames(:), outvarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        call pBmiGetInputVarNames(invarnames)
        call pBmiGetOutputVarNames(outvarnames)

        do i=1,SIZE(invarnames)
            print *,"In Variable Info: ",invarnames(i)
            call BMIAdapter_PrintVarInfo(invarnames(i),BMIAdapter_FieldRank(invarnames(i),rc),rc)
        end do

        do i=1,SIZE(outvarnames)
            print *,"Out Variable Info: ",outvarnames(i)
            call BMIAdapter_PrintVarInfo(outvarnames(i),BMIAdapter_FieldRank(invarnames(i),rc),rc)
        end do

    end subroutine


    !##########################
    !# Section                #
    !# OS Interface Procedures#
    !##########################

    !############################
    !# BMI Adapter Log Wrappers #
    !############################

    logical function BMIAdapter_LogFoundError(rcToCheck,msg,line,file,method,rcToReturn,log)
        integer,          intent(in),    optional :: rcToCheck
        character(len=*), intent(in),    optional :: msg
        integer,          intent(in),    optional :: line
        character(len=*), intent(in),    optional :: file
        character(len=*), intent(in),    optional :: method
        integer,          intent(inout), optional :: rcToReturn
        type(ESMF_Log),   intent(inout), optional :: log

        ! Local Variable
        character(len=*),parameter :: errPrefix = "BMI Adapter ERROR: "

        BMIAdapter_LogFoundError = ESMF_LogFoundError(rcToCheck=rcToCheck, msg = errPrefix // msg, line=line, file=file, method=method, rcToReturn=rcToReturn, log=log)

    end function BMIAdapter_LogFoundError

    recursive subroutine BMIAdapter_LogWrite(msg, logmsgFlag, line, file, method, log, rc)
        character(len=*),      intent(in)             :: msg
        type(ESMF_LogMsg_Flag),intent(in),optional    :: logmsgFlag
        integer,               intent(in),   optional :: line
        character(len=*),      intent(in),   optional :: file
        character(len=*),      intent(in),   optional :: method
        type(ESMF_Log),        intent(inout),optional :: log
        integer,               intent(out),  optional :: rc

        ! Local Variable
        character(len=*),parameter :: msgPrefix = "BMI Adapter MSG: "

        call ESMF_LogWrite(msg=msgPrefix // msg,logmsgFlag=logmsgFlag,line=line,file=file,method=method,log=log,rc=rc)

    end subroutine BMIAdapter_LogWrite

end module esmfBmiAdapter
