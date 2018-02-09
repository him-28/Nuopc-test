module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic driver Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_ModifyCplLists   => label_ModifyCplLists, &
    driver_label_SetRunSequence   => label_SetRunSequence
  
! use Mediator, only: medSS => SetServices
  use CHM,    only: chmSS => SetServices
  use fv3Cap, only: fv3SS => SetServices
  
  use NUOPC_Connector, only: cplSS => SetServices
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    type(ESMF_Config) :: config
    
    rc = ESMF_SUCCESS
    
    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_ModifyCplLists, &
      specRoutine=ModifyCplLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      ! register an internal initialization method
!     call NUOPC_CompSetInternalEntryPoint(EARTH_GRID_COMP, ESMF_METHOD_INITIALIZE, &
!       phaseLabelList=(/"IPDv04p2"/), userRoutine=ModifyCplLists, rc=rc)
!     ESMF_ERR_RETURN(RC,RC_REG)


    ! create, open, and set the config
    config = ESMF_ConfigCreate(rc=RC)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigLoadFile(config, "esm.configure", rc=RC)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSet(driver, config=config, rc=RC)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

      subroutine SetModelServices(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc

        ! local variables
        integer                         :: stat, i, j, petCount
        character(ESMF_MAXSTR)          :: name
        type(ESMF_GridComp)             :: comp
        type(ESMF_Config)               :: config
        character(len=32), allocatable  :: compLabels(:)
        integer, allocatable            :: petList(:)
        character(len=10)               :: value
        character(len=20)               :: model, prefix
        character(len=160)              :: msg
        integer                         :: petListBounds(2)
        integer                         :: componentCount
        type(NUOPC_FreeFormat)          :: attrFF, fdFF

        type(ESMF_Clock) :: internalClock
        type(ESMF_Time)  :: startTime, stopTime
        type(ESMF_TimeInterval) :: timeStep

        rc = ESMF_SUCCESS

        print *, 'Entering SetModelServices'

        ! -- fields for 1-way WAM->IPE coupling
        call NUOPC_FieldDictionaryAddEntry(standardName="average_height", canonicalUnits="m", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="height", canonicalUnits="m", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="temp_neutral", canonicalUnits="K", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="eastward_wind_neutral", canonicalUnits="m s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="northward_wind_neutral", canonicalUnits="m s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="upward_wind_neutral", canonicalUnits="m s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="O_Density", canonicalUnits="m-3", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="O2_Density", canonicalUnits="m-3", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="N2_Density", canonicalUnits="m-3", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        ! -- fields for 2-way WAM-IPE coupling
        call NUOPC_FieldDictionaryAddEntry(standardName="eastward_momentum_tendency", canonicalUnits="m-2 kg s-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="northward_momentum_tendency", canonicalUnits="m-2 kg s-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="neutral_heating_rate", canonicalUnits="J m-3 s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="o2_dissociation_rate", canonicalUnits="s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="air_pressure", canonicalUnits="Pa", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="air_pressure_in_model_layers", canonicalUnits="Pa", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="geopotential", canonicalUnits="m2 s-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="geopotential_in_model_layers", canonicalUnits="m2 s-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="air_temperature", canonicalUnits="K", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="x_wind", canonicalUnits="m s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="y_wind", canonicalUnits="m s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="omega", canonicalUnits="Pa s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="mass_fraction_of_tracers_in_air", canonicalUnits="kg kg-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="area_type", canonicalUnits="1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="atmosphere_boundary_layer_thickness", canonicalUnits="m", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="cell_area", canonicalUnits="m2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="convective_rainfall_amount", canonicalUnits="kg m-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="exchange", canonicalUnits="1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="friction_velocity", canonicalUnits="1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="rainfall_amount", canonicalUnits="kg m-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="soil_moisture_content", canonicalUnits="kg m-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="surface_downwelling_shortwave_flux_in_air", canonicalUnits="W m-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="surface_mask", canonicalUnits="1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="surface_skin_temperature", canonicalUnits="K", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="surface_upward_sensible_heat_flux", canonicalUnits="W m-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="thickness_of_snowfall_amount", canonicalUnits="m", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="vegetation_type", canonicalUnits="1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="vegetation_area_fraction", canonicalUnits="1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="z_over_l", canonicalUnits="1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="inst_zonal_wind_height10m", canonicalUnits="m s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_FieldDictionaryAddEntry(standardName="inst_merid_wind_height10m", canonicalUnits="m s-1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        ! query the Component for info
        call ESMF_GridCompGet(driver, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

        ! get petCount and config
        call ESMF_GridCompGet(driver, petCount=petCount, config=config, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        
        print *, 'SetModelServices: petCount = ',petCount

        print *, 'SetModelServices: read and ingest free format driver attributes'
        ! read and ingest free format driver attributes
        attrFF = NUOPC_FreeFormatCreate(config, label="EARTH_attributes::", &
          relaxedflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        print *, 'SetModelServices: done'
        print *, 'SetModelServices: ingest free format driver attributes'
        call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        print *, 'SetModelServices: done'
        print *, 'SetModelServices: destroy object'
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        print *, 'SetModelServices: done'
        
        ! dump the current field dictionary into the Log file
        call ESMF_AttributeGet(driver, name="DumpFieldDictionary", &
          value=value, defaultValue="false", &
          convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        if (trim(value)=="true") then
          call ESMF_LogWrite( &
            "===>===>===>===> Begin Dumping Field Dictionary <===<===<===<===",&
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_FieldDictionaryEgest(fdFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_FreeFormatLog(fdFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call ESMF_LogWrite( &
            "===>===>===>===> Done Dumping Field Dictionary <===<===<===<===", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        endif
        
        ! determine the generic component labels
        componentCount = ESMF_ConfigGetLen(config, &
          label="EARTH_component_list:", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        allocate(compLabels(componentCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of compLabels failed.", &
          line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
          return  ! bail out
        call ESMF_ConfigGetAttribute(config, valueList=compLabels, &
          label="EARTH_component_list:", count=componentCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        
        ! determine information for each component and add to the driver
        do i=1, componentCount
          ! construct component prefix
          prefix=trim(compLabels(i))
          print *, 'drv: compLabels',i,trim(compLabels(i))
          ! read in petList bounds
          call ESMF_ConfigGetAttribute(config, petListBounds, &
            label=trim(prefix)//"_petlist_bounds:", default=-1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          ! handle the default situation
          if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
            petListBounds(1) = 0
            petListBounds(2) = petCount - 1
          endif
          ! read in model instance name
          call ESMF_ConfigGetAttribute(config, model, &
            label=trim(prefix)//"_model:", default="none", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          print *, 'drv: model',i,trim(model)
          ! check that there was a model instance specified
          if (trim(model) == "none") then
            ! Error condition: no model was specified
            write (msg, *) "No model was specified for component: ",trim(prefix)
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
          endif
          ! set petList for this component
          allocate(petList(petListBounds(2)-petListBounds(1)+1))
          do j=petListBounds(1), petListBounds(2)
            petList(j-petListBounds(1)+1) = j ! PETs are 0 based
          enddo

          if (trim(model) == "fv3") then
            call NUOPC_DriverAddComp(driver, trim(prefix), fv3SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=trim(name)//":"//__FILE__)) &
              return  ! bail out
          else if (trim(model) == "gsdchem") then
            call NUOPC_DriverAddComp(driver, trim(prefix), chmSS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=trim(name)//":"//__FILE__)) &
              return  ! bail out
          else
            ! Error condition: unknown model requested
            write (msg, *) "The requested model '", trim(model), &
              "' is an invalid choice!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
          endif
          
          ! read and ingest free format component attributes
          attrFF = NUOPC_FreeFormatCreate(config, &
            label=trim(prefix)//"_attributes::", relaxedflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_CompAttributeIngest(comp, attrFF, addFlag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          
          ! clean-up
          deallocate(petList)
          
        enddo
        
        ! SetServices for Connectors
        call SetFromConfig(driver, mode="setServicesConnectors", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          
        ! clean-up
        deallocate(compLabels)
    print *,'SetModelServices: setting clock'
    ! set the model clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print *,'SetModelServices: clock is set'

      end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)          :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! access runSeq in the config
    call SetFromConfig(driver, mode="setRunSequence", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! Diagnostic output
    call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine SetFromConfig(driver, mode, rc)
    type(ESMF_GridComp)   :: driver
    character(len=*)      :: mode
    integer, intent(out)  :: rc

    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(ESMF_Config)               :: config
    integer                         :: lineCount, columnCount, i, slotCount
    integer, allocatable            :: count(:)
    character(len=32), allocatable  :: line(:)
    character(len=32)               :: tempString
    integer                         :: level, slot, slotHWM
    real(ESMF_KIND_R8)              :: seconds
    integer, allocatable            :: slotStack(:)
    type(ESMF_TimeInterval)         :: timeStep
    type(ESMF_Clock)                :: internalClock, subClock
    character(len=60), allocatable  :: connectorInstance(:)
    integer                         :: connectorCount, j
    type(ESMF_CplComp)              :: conn

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! reset config to beginning of runSeq:: block
    call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call ESMF_ConfigGetDim(config, lineCount, columnCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    allocate(count(lineCount))

    if (trim(mode)=="setServicesConnectors") then
      allocate(connectorInstance(lineCount))  ! max number of connectors
      connectorInstance = ""
      connectorCount = 0 ! reset
    endif

    ! reset config to beginning of runSeq:: block
    call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! determine number of entries on each line
    do i=1, lineCount
      call ESMF_ConfigNextLine(config)
      count(i) = ESMF_ConfigGetLen(config) ! entries on line i
    enddo

    ! reset config to beginning of runSeq:: block
    call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! read each line and determine slotCount
    slotCount = 0
    do i=1, lineCount
      call ESMF_ConfigNextLine(config)
      allocate(line(count(i)))
      call ESMF_ConfigGetAttribute(config, line, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

      ! process the configuration line
      if (size(line) == 1) then
        if (index(trim(line(1)),"@") == 1) then
          slotCount = slotCount + 1
        endif
      elseif ((size(line) == 3) .or. (size(line) == 4)) then
        if (trim(mode)=="setServicesConnectors") then
          ! a connector if the second element is "->"
          if (trim(line(2)) /= "->") then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="Configuration line incorrectly formatted.", &
              line=__LINE__, &
              file=__FILE__)
            return  ! bail out
          else
            ! found a connector entry, see if it is the first instance
            do j=1, connectorCount
              if (trim(connectorInstance(j)) == &
                trim(line(1))//trim(line(2))//trim(line(3))) exit
            enddo
            if (j>connectorCount) then
              ! this is a new Connector instance
              connectorCount = j
              connectorInstance(j) = trim(line(1))//trim(line(2))//trim(line(3))
              ! SetServices for new Connector instance
              call NUOPC_DriverAddComp(driver, &
                srcCompLabel=trim(line(1)), dstCompLabel=trim(line(3)), &
                compSetServicesRoutine=cplSS, comp=conn, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
              call NUOPC_CompAttributeSet(conn, name="Verbosity", value="max", &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
              if (size(line) == 4) then
                ! there are additional connection options specified
                ! -> set as Attribute for now on the connector object
                call ESMF_AttributeSet(conn, name="ConnectionOptions", &
                  value=trim(line(4)), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
              endif
            endif
          endif
        endif
      endif
      ! clean-up
      deallocate(line)
    enddo
    slotCount = (slotCount+1) / 2
    slotCount = max(slotCount, 1) ! at least one slot

    if (trim(mode)=="setRunSequence") then

      allocate(slotStack(slotCount))

      ! Replace the default RunSequence with a customized one
      call NUOPC_DriverNewRunSequence(driver, slotCount=slotCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Get driver intenalClock
      call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! reset config to beginning of runSeq:: block
      call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

      level = 0
      slot = 0
      slotHWM = 0
      do i=1, lineCount
        call ESMF_ConfigNextLine(config)
        allocate(line(count(i)))
        call ESMF_ConfigGetAttribute(config, line, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

        ! process the configuration line
        if ((size(line) < 1) .or. (size(line) > 4)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
            msg="Configuration line incorrectly formatted.", &
            line=__LINE__, &
            file=__FILE__)
          return  ! bail out
        elseif (size(line) == 1) then
          ! either a model or a time step indicator
          if (index(trim(line(1)),"@") == 1) then
            ! time step indicator
            tempString=trim(line(1))
            if (len(trim(tempString)) > 1) then
              ! entering new time loop level
              level = level + 1
              slotStack(level)=slot
              slot = slotHWM + 1
              slotHWM = slotHWM + 1
              read(tempString(2:len(tempString)), *) seconds
              print *, "found time step indicator: ", seconds
              call ESMF_TimeIntervalSet(timeStep, s_r8=seconds, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

              if (slot==1) then
                ! Set the timeStep of the internalClock
                call ESMF_ClockSet(internalClock, timeStep=timeStep, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
              else
                ! Insert the link to a new slot, and set the timeStep
                call NUOPC_DriverAddRunElement(driver, slot=slotStack(level), &
                  linkSlot=slot, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
                subClock = ESMF_ClockCreate(internalClock, rc=rc)  ! make a copy first
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
                call ESMF_ClockSet(subClock, timeStep=timeStep, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
                call NUOPC_DriverSetRunSequence(driver, slot=slot, &
                  clock=subClock, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
              endif
            else
              ! exiting time loop level
              slot = slotStack(level)
              level = level - 1
            endif
          else
            ! model
            slot = max(slot, 1) ! model outside of a time loop
            call NUOPC_DriverAddRunElement(driver, slot=slot, &
              compLabel=trim(line(1)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          endif
        elseif (size(line) == 2) then
          ! a model with a specific phase label
          call NUOPC_DriverAddRunElement(driver, slot=slot, &
            compLabel=trim(line(1)), phaseLabel=trim(line(2)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        elseif ((size(line) == 3) .or. (size(line) == 4)) then
          ! a connector if the second element is "->", with options if 4th part
          if (trim(line(2)) /= "->") then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="Configuration line incorrectly formatted.", &
              line=__LINE__, &
              file=__FILE__)
            return  ! bail out
          endif
          call NUOPC_DriverAddRunElement(driver, slot=slot, &
            srcCompLabel=trim(line(1)), dstCompLabel=trim(line(3)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif    
        
        ! clean-up
        deallocate(line)
      enddo
      ! clean-up
      deallocate(slotStack)
    endif

    ! clean-up
    deallocate(count)
    if (trim(mode)=="setServicesConnectors") then
      deallocate(connectorInstance)
    endif
    print *,'SetFromConfig: exiting'

  end subroutine

  subroutine ModifyCplLists(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    character(len=160)              :: name, msg
    type(ESMF_CplComp), pointer     :: connectorList(:)
    integer                         :: i, j, cplListSize
    character(len=160), allocatable :: cplList(:)
    character(len=160)              :: value

    rc = ESMF_SUCCESS
    print *,'ModifyCplLists: entering'
    
    call ESMF_LogWrite("Driver is in ModifyCplLists()", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msg,*) "Found ", size(connectorList), " Connectors."// &
      " Modifying CplList Attribute...."
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    do i=1, size(connectorList)
      ! query Connector i for its name
      call ESMF_CplCompGet(connectorList(i), name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! access CplList for Connector i
      call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (cplListSize>0) then
        allocate(cplList(cplListSize))
        call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
          valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! go through all of the entries in the cplList and add options
        do j=1, cplListSize
          cplList(j) = trim(cplList(j))//":DumpWeights=true"
          cplList(j) = trim(cplList(j))//":SrcTermProcessing=1:TermOrder=SrcSeq"
          ! add connection options read in from configuration file
          call ESMF_AttributeGet(connectorList(i), name="ConnectionOptions", &
            value=value, defaultValue="", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          cplList(j) = trim(cplList(j))//trim(value)
        enddo
        ! store the modified cplList in CplList attribute of connector i
        call NUOPC_CompAttributeSet(connectorList(i), &
          name="CplList", valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call NUOPC_CompAttributeSet(connectorList(i), name="Verbosity", &
          value="max", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        deallocate(cplList)
      endif
    enddo
      
    deallocate(connectorList)
    print *,'ModifyCplLists: exiting'
    write (msg,*) "ModifyCplLists: exiting"
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

end module ESM
