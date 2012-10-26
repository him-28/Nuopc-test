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

!----------------------------------------------------------------------
! Add required entries NUOPC Field Dictionary
!----------------------------------------------------------------------

  CALL NUOPC_FieldDictionaryAddEntry( &
       "eastward_wind_at_10m_height", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="wind_10m_u", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "northward_wind_at_10m_height", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="wind_10m_v", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "air_temperature_at_2m_height", &
       canonicalUnits="K", &
       defaultLongName="N/A", &
       defaultShortName="air_temp_2m", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "surface_eastward_wind_to_wave_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_wav_u", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "surface_northward_wind_to_wave_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_wav_v", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "surface_eastward_wave_to_ocean_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_wav_ocn_u", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "surface_northward_wave_to_ocean_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_wav_ocn_v", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_eastward_drift_velocity", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="ice_drift_u", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_northward_drift_velocity", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="ice_drift_v", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_concentration", &
       canonicalUnits="ice", &
       defaultLongName="N/A", &
       defaultShortName="ice_conc", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_thickness", &
       canonicalUnits="m", &
       defaultLongName="N/A", &
       defaultShortName="ice_thick", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_temperature", &
       canonicalUnits="K", &
       defaultLongName="N/A", &
       defaultShortName="ice_temp", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_surface_downward_eastward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_u", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_surface_downward_northward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_v", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_surface_downward_eastward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_ice_u", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_surface_downward_northward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_ice_v", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_basal_upward_eastward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_ice_u", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_basal_upward_northward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_ice_v", &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!----------------------------------------------------------------------
!----------------------------------------------------------------------
  
  ! Create the earth system Component
  esmComp = ESMF_GridCompCreate(name="ESM", rc=rc)
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
  
  ! Finalize ESMF
  call ESMF_Finalize()
  
end program  
