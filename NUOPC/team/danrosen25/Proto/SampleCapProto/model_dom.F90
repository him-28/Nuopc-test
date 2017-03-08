module model_dom_mod

#define MODNAME "MODEL_DOM"

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

  use ESMF

  implicit none

  private

  public model_grid
  public model_grid_init

  type(ESMF_Grid) :: model_grid

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_grid_init"

  subroutine model_grid_init(rc)
    integer, intent(out) :: rc

    ! local variables

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! create a Grid object for Fields
    model_grid = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/10, 100/), &
      minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
      maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

end module
