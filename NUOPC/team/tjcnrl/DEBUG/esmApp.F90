program esmApp

  !-----------------------------------------------------------------------------
  ! Generic ESM application driver
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use ESM, only: esmSS => SetServices

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: esmComp
  
  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set log defaults
  call ESMF_LogSet(flush=.true.)

  ! Add required fields to NUOPC field dictionary
  call SetFieldDictionary(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
  
  call ESMF_LogWrite("esmApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the earth system Component
  esmComp = ESMF_GridCompCreate(name="esm", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  ! SetServices for the earth system Component
  call ESMF_GridCompSetServices(esmComp, esmSS, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  ! Call Initialize for the earth system Component
  call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Call Run  for earth the system Component
  call ESMF_GridCompRun(esmComp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Call Finalize for the earth system Component
  call ESMF_GridCompFinalize(esmComp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  ! Destroy the earth system Component
  call ESMF_GridCompDestroy(esmComp, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_LogWrite("esmApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize()

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetFieldDictionary(rc)
    integer, intent(out) :: rc

    ! local variables
    integer, parameter :: maxFields = 50
    character(ESMF_MAXSTR) :: standardName(maxFields)
    character(ESMF_MAXSTR) :: canonicalUnits(maxFields)
    integer :: i, numFields
    logical :: isPresent

    rc = ESMF_SUCCESS

    i = 0
    ! ATM export
    i = i+1; standardName(i) = 'air_pressure_at_sea_level'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'eastward_wind_at_10m_height'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_wind_at_10m_height'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'magnitude_of_surface_downward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_downward_eastward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_downward_northward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'air_temperature_at_2m_height'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'relative_humidity_at_2m_height'
             canonicalUnits(i)='1'
    i = i+1; standardName(i) = 'surface_downward_latent_heat_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_downward_sensible_heat_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_net_downward_shortwave_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_net_downward_longwave_flux'
             canonicalUnits(i)='W m-2'
    ! OCN export
    i = i+1; standardName(i) = 'sea_surface_height_above_sea_level'
             canonicalUnits(i)='m'
    i = i+1; standardName(i) = 'sea_surface_temperature'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'sea_surface_salinity'
             canonicalUnits(i)='1e-3'
    i = i+1; standardName(i) = 'surface_eastward_sea_water_velocity'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'surface_northward_sea_water_velocity'
             canonicalUnits(i)='m s-1'
    ! WAV export
    i = i+1; standardName(i) = 'wave_induced_charnock_parameter'
             canonicalUnits(i)='1'
    i = i+1; standardName(i) = 'surface_total_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_eastward_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_northward_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'eastward_stokes_drift_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_stokes_drift_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'eastward_wave_bottom_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_wave_bottom_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'wave_bottom_current_radian_frequency'
             canonicalUnits(i)='rad s-1'
    i = i+1; standardName(i) = 'eastward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'eastward_northward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'northward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'eastward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'northward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'wave_orbital_turbulence_production'
             canonicalUnits(i)='m2 s-3'
    numFields = i

    do i=1,numFields
      isPresent = NUOPC_FieldDictionaryHasEntry(trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry(trim(standardName(i)), &
          trim(canonicalUnits(i)), defaultLongName='none', defaultShortName='none', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo

  end subroutine
  
end program  
