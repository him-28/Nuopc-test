module module_cplfields

  !-----------------------------------------------------------------------------
  ! This module contains the fv3 Coupling Fields: export and import
  !
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none

  private

  integer, public, parameter  :: NexportFields = 25
  type(ESMF_Field), target, public    :: exportFields(NexportFields)
  character(len=*), public, parameter :: exportFieldsList(NexportFields) = (/ &
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
  character(len=*), public, parameter :: exportFieldTypes(NexportFields) = (/ &
       "i","l","i","l","l","l","l","l","t", &
       "s","s","s","s","l","s","s","g",     &
       "s","s","s","s","s","s","s","s"      &
  /)

  public :: cplFieldGet

contains

  subroutine cplStateGet(state, fieldList, fieldCount, rc)

    character(len=*), intent(in)            :: state
    type(ESMF_Field), pointer,     optional :: fieldList(:)
    integer,          intent(out), optional :: fieldCount
    integer,          intent(out), optional :: rc

    !--- begin
    if (present(rc)) rc = ESMF_SUCCESS

    select case (trim(state))
      case ('import','i')
!       if (present(fieldList )) fieldList  => importFields
!       if (present(fieldCount)) fieldCount =  size(importFields)
      case ('export','o')
        if (present(fieldList )) fieldList  => exportFields
        if (present(fieldCount)) fieldCount =  size(exportFields)
      case default
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="state argument can only be import(i)/export(o).", &
          line=__LINE__, file=__FILE__,&
          rcToReturn=rc)
        return
    end select

  end subroutine cplStateGet

  subroutine cplFieldGet(state, name, localDe, &
    farrayPtr2d, farrayPtr3d, farrayPtr4d, rc)

    character(len=*),   intent(in)            :: state
    character(len=*),   intent(in)            :: name
    integer,            intent(in),  optional :: localDe
    real(ESMF_KIND_R8), pointer,     optional :: farrayPtr2d(:,:)
    real(ESMF_KIND_R8), pointer,     optional :: farrayPtr3d(:,:,:)
    real(ESMF_KIND_R8), pointer,     optional :: farrayPtr4d(:,:,:,:)
    integer,            intent(out), optional :: rc

    !--- local variables
    integer                    :: localrc
    integer                    :: de, item, fieldCount, rank
    type(ESMF_Field), pointer  :: fieldList(:)
    character(len=ESMF_MAXSTR) :: fieldName

    !--- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (present(farrayPtr2d)) nullify(farrayPtr2d)
    if (present(farrayPtr3d)) nullify(farrayPtr3d)
    if (present(farrayPtr4d)) nullify(farrayPtr4d)

    de = 0
    if (present(localDe)) de = localDe

    call cplStateGet(state, fieldList=fieldList, fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, &
      rcToReturn=rc)) return

    do item = 1, fieldCount
      if (NUOPC_IsConnected(fieldList(item))) then
        call ESMF_FieldGet(fieldList(item), name=fieldName, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__, &
          rcToReturn=rc)) return
        if (trim(fieldName) == trim(name)) then
          call ESMF_FieldGet(fieldList(item), rank=rank, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, &
            rcToReturn=rc)) return
          select case (rank)
            case (2)
              if (present(farrayPtr2d)) then
                call ESMF_FieldGet(fieldList(item), localDe=de, farrayPtr=farrayPtr2d, &
                  rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__, &
                  rcToReturn=rc)) return
              end if
            case (3)
              if (present(farrayPtr3d)) then
                call ESMF_FieldGet(fieldList(item), localDe=de, farrayPtr=farrayPtr3d, &
                  rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__, &
                  rcToReturn=rc)) return
              end if
            case (4)
              if (present(farrayPtr4d)) then
                call ESMF_FieldGet(fieldList(item), localDe=de, farrayPtr=farrayPtr4d, &
                  rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__, &
                  rcToReturn=rc)) return
              end if
            case default
              call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
                msg="field rank should be 2, 3, or 4.", &
                line=__LINE__, file=__FILE__, &
                rcToReturn=rc)
              return
          end select
          exit
        end if
      end if
    end do

  end subroutine cplFieldGet

end module module_cplfields
