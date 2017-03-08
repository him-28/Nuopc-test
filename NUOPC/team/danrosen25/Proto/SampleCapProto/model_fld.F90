module model_fld_mod

#define MODNAME "MODEL_FLD"

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

  use model_dom_mod, only: model_grid
  use ESMF

  implicit none

  private

  public model_fields_create
  public model_fields_destroy
  public model_field_get
  public forcing_1, forcing_2, forcing_3
  public output_1, output_2, output_3

  INTEGER, PARAMETER                  :: model_field_kind = ESMF_KIND_R8
  TYPE(ESMF_TypeKind_Flag), PARAMETER :: model_field_tk   = ESMF_TYPEKIND_R8

  type model_field_type
    character(len=80)              :: standardName     = ""
    character(len=16)              :: stateName        = ""
    character(len=10)              :: units            = ""
    real(model_field_kind),pointer :: farray_ptr(:,:)  => null()
    type(ESMF_Field)               :: esmf_field
  end type

  type model_field_ptr
    type(model_field_type),pointer :: ptr
  end type

  type(model_field_type),target :: forcing_1
  type(model_field_type),target :: forcing_2
  type(model_field_type),target :: forcing_3
  type(model_field_type),target :: output_1
  type(model_field_type),target :: output_2
  type(model_field_type),target  :: output_3

  type(model_field_ptr),dimension(6) :: model_field_list
 
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_fields_create"

  subroutine model_fields_create(rc)
    integer, intent(out) :: rc

    ! local variables
    integer :: stat
    integer :: i
    integer :: localDeCount
    integer :: rank

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    forcing_1%standardName='dummy_field_1'
    forcing_1%stateName='forcing_1'
    forcing_1%units='Pa'
    forcing_2%standardName='dummy_field_2'
    forcing_2%stateName='forcing_2'
    forcing_2%units='kg'
    forcing_3%standardName='dummy_field_3'
    forcing_3%stateName='forcing_3'
    forcing_3%units='W m-2'
    output_1%standardName='dummy_field_4'
    output_1%stateName='output_1'
    output_1%units='m'
    output_2%standardName='dummy_field_5'
    output_2%stateName='output_2'
    output_2%units='kg'
    output_3%standardName='dummy_field_6'
    output_3%stateName='output_3'
    output_3%units='m s-1'

    model_field_list(1)%ptr => forcing_1
    model_field_list(2)%ptr => forcing_2
    model_field_list(3)%ptr => forcing_3
    model_field_list(4)%ptr => output_1
    model_field_list(5)%ptr => output_2
    model_field_list(6)%ptr => output_3

    if (.NOT.ESMF_GridIsCreated(model_grid)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_NOT_CREATED, &
        msg='Model grid has not been created.', &
        method=METHOD, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

    do i=1, size(model_field_list)

      model_field_list(i)%ptr%esmf_field = ESMF_FieldCreate( &
        name=model_field_list(i)%ptr%stateName, &
        grid=model_grid, &
        typekind=model_field_tk, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      call ESMF_FieldGet(model_field_list(i)%ptr%esmf_field, &
        rank=rank, localDeCount=localDeCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      if (rank .ne. 2) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
          msg='Model does not support rank not equal to 2.', &
          method=METHOD, file=__FILE__, rcToReturn=rc)
        return ! bail out
      else if (localDeCount .gt. 1) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
          msg='Model does not support multiple DEs on a single PET.', &
          method=METHOD, file=__FILE__, rcToReturn=rc)
        return ! bail out
      endif

      call ESMF_FieldGet(model_field_list(i)%ptr%esmf_field, &
        farrayPtr=model_field_list(i)%ptr%farray_ptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_field_get"

  subroutine model_field_get(standardName,field,stateName,units,rc)
    character(len=*), intent(in)            :: standardName
    character(len=*), intent(out), optional :: stateName
    character(len=*), intent(out), optional :: units
    type(ESMF_Field), intent(out), optional :: field
    integer, intent(out)                    :: rc

    ! local variables
    integer :: i
    integer :: localDeCount
    integer :: rank
    logical :: found

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    found = .FALSE.

    do i=1, size(model_field_list)

      if (standardName .eq. model_field_list(i)%ptr%standardName) then
        found = .TRUE.
        if (present(stateName)) then
          stateName = model_field_list(i)%ptr%stateName
        endif
        if (present(units)) then
          units = model_field_list(i)%ptr%units
        endif
        if (present(field)) then
          field = model_field_list(i)%ptr%esmf_field
        endif
        exit
      endif

    enddo

    if (.NOT.found) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_FOUND, &
        msg=trim(standardName)//' was not found.', &
        method=METHOD, file=__FILE__, rcToReturn=rc)
        return ! bail out
    endif

    if (.NOT.ESMF_FieldIsCreated(field)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_NOT_CREATED, &
        msg=trim(standardName)//' has not been created. Please call '// &
          'model_field_init', &
        method=METHOD, file=__FILE__, rcToReturn=rc)
        return ! bail out
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_fields_destroy"

  subroutine model_fields_destroy(rc)
    integer, intent(out)          :: rc

    ! local variables
    integer :: i

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    do i=1, size(model_field_list)
      call ESMF_FieldDestroy(model_field_list(i)%ptr%esmf_field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
    enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

end module
