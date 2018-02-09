module CHM

  !-----------------------------------------------------------------------------
  ! CHM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, inheritModel => SetServices

  implicit none

  integer, parameter :: importFieldCount = 25
  character(len=*), dimension(importFieldCount), parameter :: &
    importFieldNames = (/ &
      "air_pressure                             ", &
      "air_pressure_in_model_layers             ", &
      "geopotential                             ", &
      "geopotential_in_model_layers             ", &
      "air_temperature                          ", &
      "x_wind                                   ", &
      "y_wind                                   ", &
      "omega                                    ", &
      "mass_fraction_of_tracers_in_air          ", &
      "area_type                                ", &
      "atmosphere_boundary_layer_thickness      ", &
      "cell_area                                ", &
      "convective_rainfall_amount               ", &
      "exchange                                 ", &
      "friction_velocity                        ", &
      "rainfall_amount                          ", &
      "soil_moisture_content                    ", &
      "surface_downwelling_shortwave_flux_in_air", &
      "surface_mask                             ", &
      "surface_skin_temperature                 ", &
      "surface_upward_sensible_heat_flux        ", &
      "thickness_of_snowfall_amount             ", &
      "vegetation_type                          ", &
      "vegetation_area_fraction                 ", &
      "z_over_l                                 "  &
    /)

  integer :: localDeCount

  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, inheritModel, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! -- advertise imported fields
    ! -- use SharePolicyField "share" to access imported fields by reference
    if (importFieldCount > 0) then
      call NUOPC_Advertise(importState, importFieldNames, &
        TransferOfferGeomObject="cannot provide", &
        TransferOfferField="cannot provide", &
        SharePolicyField="share", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    end if

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)              :: importState
    type(ESMF_Field)              :: field
    type(ESMF_Clock)              :: clock
    type(ESMF_Grid)               :: grid
    type(ESMF_VM)                 :: vm
    type(ESMF_GeomType_flag)      :: geomtype
    type(ESMF_DistGrid)           :: distgrid
    type(ESMF_Array)              :: array
    integer                       :: de, item, localrc, localDe, tile
    integer                       :: comm, localPet
    real(ESMF_KIND_R8), dimension(:,:), pointer :: coord

    integer :: dimCount, tileCount, deCount
    integer, dimension(:),   allocatable :: deToTileMap, localDeToDeMap
    integer, dimension(:,:), allocatable :: minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile
    integer, dimension(:,:), allocatable :: computationalLBound, computationalUBound

    integer :: yy, mm, dd, h, m
    real(ESMF_KIND_R8) :: dts
    type(ESMF_Time) :: startTime
    type(ESMF_TimeInterval) :: TimeStep

    ! -- initialize chemistry model
    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- check if import fields are defined
    if (importFieldCount < 1) then 
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="This component requires imported fields to be defined.", &
        line=__LINE__, file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, importState=importState, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get coordinates from Grid object
    ! assume all fields on same grid
    ! use first field 
    call ESMF_StateGet(importState, field=field, &
      itemName=trim(importFieldNames(1)), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, geomtype=geomtype, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (geomtype == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_ArrayGet(array, deCount=deCount, dimCount=dimCount, &
        tileCount=tileCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(minIndexPDe(dimCount, deCount), maxIndexPDe(dimCount, deCount),  &
        minIndexPTile(dimCount, tileCount), maxIndexPTile(dimCount, tileCount), &
        computationalLBound(dimCount, localDeCount), computationalUBound(dimCount, localDeCount), &
        deToTileMap(deCount), localDeToDeMap(localDeCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      call ESMF_ArrayGet(array, distgrid=distgrid, &
        deToTileMap=deToTileMap, localDeToDeMap=localDeToDeMap, &
        computationalLBound=computationalLBound, &
        computationalUBound=computationalUBound, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_DistGridGet(distgrid, &
        minIndexPDe=minIndexPDe, maxIndexPDe=maxIndexPDe, &
        minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_VMGet(vm, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! begin model initialization on a grid

      do localDe = 0, localDeCount-1
        de   = localDeToDeMap(localDe+1) + 1
        tile = deToTileMap(de)

        write(6,'("CHEM: PET:",i02," localDe: ",i2," DE: ",i2, " tile=",i2," minIndexPDe=",2i4,2x," maxIndexPDe=",2i4," minIndexPTile=",2i4," maxIndexPTile=",2i4,4i4)') &
          localPet, localDe, de-1, tile, minIndexPDe(:,de), maxIndexPDe(:,de), minIndexPTile(:,tile), maxIndexPTile(:,tile), &
          computationalLBound(:,localDe+1), computationalUBound(:,localDe+1)

        ! set model domain decomposition for local DEs here

      end do
      deallocate(minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile, &
        computationalLBound, computationalUBound, &
        deToTileMap, localDeToDeMap, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Imported fields can only be defined on Grid objects.", &
        line=__LINE__, file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! complete model initialization here

    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(model, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               :: currTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Field)              :: field
    type(ESMF_VM)                 :: vm
    integer                       :: item

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing CHM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_TimePrint(currTime + timeStep, &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! print field diagnostics
    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do item = 1, importFieldCount
      call ESMF_StateGet(importState, field=field, &
        itemName=trim(importFieldNames(item)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call fieldPrintMinMax(field, vm, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    end do

    ! advance model here

  end subroutine ModelAdvance


  subroutine ModelFinalize(model, rc)
    type(ESMF_GridComp)   :: model
    integer, intent(out)  :: rc

    rc = ESMF_SUCCESS

    ! finalize model here

  end subroutine ModelFinalize

  !-----------------------------------------------------------------------------

  subroutine fieldPrintMinMax(field, vm, rc)
    type(ESMF_Field), intent(in) :: field
    type(ESMF_VM),    intent(in) :: vm
    integer, intent(out) :: rc

    ! local variables
    real(ESMF_KIND_R8), pointer :: fp1d(:), fp2d(:,:), fp3d(:,:,:), fp4d(:,:,:,:)
    real(ESMF_KIND_R8)          :: fieldMaxValue, fieldMinValue, maxValue, minValue
    real(ESMF_KIND_R8)          :: globalMaxValue(1), globalMinValue(1)
    integer                     :: localDe, localDeCount, localPet, rank
    character(len=ESMF_MAXSTR)  :: fieldName

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, rank=rank, localDeCount=localDeCount, &
      name=fieldName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail

    fieldMinValue = huge(1.0_ESMF_KIND_R8)
    fieldMaxValue = -fieldMinValue

    do localDe = 0, localDeCount - 1
      select case(rank)
        case(1)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp1d, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          minValue = minval(fp1d)
          maxValue = maxval(fp1d)
        case(2)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp2d, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          minValue = minval(fp2d)
          maxValue = maxval(fp2d)
        case(3)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp3d, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          minValue = minval(fp3d)
          maxValue = maxval(fp3d)
        case(4)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp4d, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          minValue = minval(fp4d)
          maxValue = maxval(fp4d)
        case default
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Field rank not implemented.", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return ! bail out
      end select
      fieldMinValue = min(fieldMinValue, minValue)
      fieldMaxValue = max(fieldMaxValue, maxValue)
      write(6,'(a,":",i0,2x,"DE: ",i0,2x,a," - checking  - min/max = ",2g16.6)') 'PET', &
         localPet, localDe, trim(fieldName), minValue, maxValue
    end do

    globalMinValue(1) = 0._ESMF_KIND_R8
    globalMaxValue(1) = 0._ESMF_KIND_R8

    call ESMF_VMReduce(vm, (/ fieldMinValue /), globalMinValue, 1, &
      reduceflag=ESMF_REDUCE_MIN, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMReduce(vm, (/ fieldMaxValue /), globalMaxValue, 1, &
      reduceflag=ESMF_REDUCE_MAX, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail

    if (localPet == 0) then
       write(6,'(a,":",a," - checking  - min/max = ",2g16.6)') 'Field', &
         trim(fieldName), globalMinValue, globalMaxValue
    end if

  end subroutine fieldPrintMinMax

  !-----------------------------------------------------------------------------

end module CHM
