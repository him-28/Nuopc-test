module model_drv_mod

#define MODNAME "MODEL_DRV"

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

  use model_dom_mod
  use model_fld_mod
  use ESMF

  implicit none

  private

  public model_init
  public model_run
  public model_finalize

  logical :: initialized = .FALSE.

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_init"

  subroutine model_init(offline, rc)
    logical, intent(in)  :: offline
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR) :: tmpStr

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    call model_grid_init(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    call model_fields_create(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    if (offline) then
      call ESMF_FieldFill(forcing_1%esmf_field, &
        dataFillScheme = "sincos", &
        member=1, step=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call ESMF_FieldFill(forcing_2%esmf_field, &
        dataFillScheme = "sincos", &
        member=2, step=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call ESMF_FieldFill(forcing_3%esmf_field, &
        dataFillScheme = "sincos", &
        member=3, step=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
    else
      forcing_1%farray_ptr = 0.0
      forcing_2%farray_ptr = 0.0
      forcing_3%farray_ptr = 0.0
    endif

    output_1%farray_ptr = 0.0
    output_2%farray_ptr = 0.0
    output_3%farray_ptr = 0.0

    initialized = .TRUE.

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_run"

  subroutine model_run(offline, rc)
    logical, intent(in)  :: offline
    integer, intent(out) :: rc

    ! local variables
    integer, save :: step = 1

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if (.NOT.initialized) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_INIT, &
        msg='Model has not been initialized.', &
        method=METHOD, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

    if (offline) then

      call ESMF_FieldFill(forcing_1%esmf_field, &
        dataFillScheme = "sincos", &
        member=1, step=step, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call ESMF_FieldFill(forcing_2%esmf_field, &
        dataFillScheme = "sincos", &
        member=2, step=step, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call ESMF_FieldFill(forcing_3%esmf_field, &
        dataFillScheme = "sincos", &
        member=3, step=step, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    endif

    output_1%farray_ptr = forcing_1%farray_ptr * 1
    output_2%farray_ptr = forcing_2%farray_ptr * 2
    output_3%farray_ptr = forcing_3%farray_ptr * 3

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_finalize"

  subroutine model_finalize(rc)
    integer, intent(out) :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

  rc = ESMF_SUCCESS

  call model_fields_destroy(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

end module
