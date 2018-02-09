module chem_methods

  use ESMF
  use NUOPC
  use chem_rc_mod
  use chem_comm_mod
  use chem_types_mod, only : CHEM_MAXSTR
  use chem_model_mod
  use chem_io_mod
  use chem_iodata_mod
  use gocart_model_mod

  implicit none

! private

  public 

! public :: chem_comp_advance
! public :: chem_comp_connect
! public :: chem_comp_finalize
! public :: chem_comp_init
! public :: chem_rc_check

contains

  subroutine chem_comp_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: deCount
    type(chem_config_type), pointer :: config

    ! -- begin
    if (present(rc)) rc = ESMF_FAILURE

    call chem_model_get(deCount=deCount, config=config, rc=localrc)
    if (chem_rc_check(localrc, file=__FILE__, line=__LINE__)) return

    if (deCount > 0) then
      select case (config % chem_opt)
        case(CHEM_OPT_GOCART, CHEM_OPT_GOCART_RACM, CHEM_OPT_RACM_SOA_VBS)
          print *,'chem: calling gocart init'
          call gocart_model_init(rc=localrc)
          if (chem_rc_check(localrc, file=__FILE__, line=__LINE__)) then
            call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to initialize model", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return  ! bail out
          end if
          print *,'chem: done gocart init'
        case default
          return
      end select
    end if

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine chem_comp_init


  subroutine chem_comp_advance(clock, rc)

    type(ESMF_Clock), intent(in) :: clock
    integer,         intent(out) :: rc

    ! -- local variables
    integer                 :: deCount
    integer                 :: julday, yy, mm, dd, h, m, s
    integer(ESMF_KIND_I8)   :: advanceCount
    real(ESMF_KIND_R8)      :: dts
    character(len=CHEM_MAXSTR) :: tStamp
    type(ESMF_Time)         :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(chem_config_type), pointer :: config

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if model is active on this PET, bail out if not
    call chem_model_get(deCount=deCount, config=config, rc=rc)
    if (chem_rc_check(rc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to set model's internal clock", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    if (deCount < 1) return

    ! -- get current time and set model's internal clock
    call ESMF_ClockPrint(clock, &
      preString="chem: run(): time step : ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="chem: run(): time stamp: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, &
      advanceCount=advanceCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeIntervalGet(timeStep, s_r8=dts, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, &
      dayOfYear=julday, timeString=tStamp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call chem_model_clock_set(julday=julday, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, dts=dts, &
      advanceCount=int(advanceCount), rc=rc) 
!     advanceCount=int(advanceCount), tStamp=tStamp, rc=rc) 
    if (chem_rc_check(rc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to set model's internal clock", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    select case (config % chem_opt)
      case(CHEM_OPT_GOCART, CHEM_OPT_GOCART_RACM, CHEM_OPT_RACM_SOA_VBS)
        print *,'chem_comp_advance() -- starting GOCART step ...'
        flush 6
        call gocart_model_advance(rc=rc)
        if (chem_rc_check(rc, file=__FILE__, line=__LINE__)) then
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to advance model", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
        end if
        print *,'chem_comp_advance() -- ending GOCART step...'
        flush 6
      case default
        return
    end select

    ! -- write history
!   call chem_history_write(chem_config, its)

  end subroutine chem_comp_advance


  subroutine chem_comp_finalize(rc)
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call chem_model_destroy()

  end subroutine chem_comp_finalize

  !-----------------------------------------------------------------------------

  subroutine chem_comp_connect(stateType, state, fieldNames, rc)
    character(len=*),  intent(in)  :: stateType
    type(ESMF_State),  intent(in)  :: state
    character(len=*),  intent(in)  :: fieldNames(:)
    integer,           intent(out) :: rc

    ! -- begin
    select case (trim(stateType))
      case('import','i')
        call chem_comp_import(state, fieldNames, rc)
      case('export','e')
        call chem_comp_export(state, fieldNames, rc)
      case default
        rc = ESMF_RC_NOT_IMPL
    end select

  end subroutine chem_comp_connect


  subroutine chem_comp_export(state, fieldNames, rc)
    type(ESMF_State),               intent(in) :: state
    character(len=*), dimension(:), intent(in) :: fieldNames
    integer, intent(out) :: rc

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- nothing to export (yet)

  end subroutine chem_comp_export

  subroutine chem_comp_import(state, fieldNames, rc)
    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: fieldNames(:)
    integer,          intent(out) :: rc

    ! local variables
    type(chem_state_type), pointer :: stateIn
    type(ESMF_Field)               :: field
    integer                        :: item, localDe, localDeCount

    rc = ESMF_SUCCESS

    do item = 1, size(fieldNames)

      call ESMF_StateGet(state, field=field, &
        itemName=trim(fieldNames(item)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail
      call ESMF_FieldGet(field, localDeCount=localDeCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail

      do localDe = 0, localDeCount-1

        call chem_model_get(stateIn=stateIn, de=localDe, rc=rc)
        if (chem_rc_check(rc)) then
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
            msg="Failed to retrieve model's import state", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc) 
          return  ! bail out
        end if

        select case (trim(fieldNames(item)))
          case ("air_pressure")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % pr3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call chem_model_set(numIntLayers=size(stateIn % pr3d,dim=3), de=localDe)
          case ("air_pressure_in_model_layers")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % prl3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call chem_model_set(numModLayers=size(stateIn % prl3d,dim=3), de=localDe)
          case ("air_temperature")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % tk3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("area_type")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % stype2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("atmosphere_boundary_layer_thickness")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % pb2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("cell_area")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % area, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("convective_rainfall_amount")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rc2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("exchange")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % exch, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("friction_velocity")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % us2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("geopotential")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ph3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("geopotential_in_model_layers")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % phl3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("mass_fraction_of_tracers_in_air")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % tr3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call chem_model_set(numTracers=size(stateIn % tr3d, dim=4), de=localDe)
          case ("omega")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ws3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("rainfall_amount")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rn2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("soil_moisture_content")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % sm3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call chem_model_set(numSoilLayers=size(stateIn % sm3d, dim=3), de=localDe)
          case ("surface_downwelling_shortwave_flux_in_air")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rsds, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("surface_mask")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % slmsk2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("surface_skin_temperature")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ts2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("surface_upward_sensible_heat_flux")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % hf2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("thickness_of_snowfall_amount")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % snwdph2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("vegetation_type")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vtype2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("vegetation_area_fraction")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vfrac2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("x_wind")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % us3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("y_wind")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vs3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("z_over_l")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % zorl2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
!         case ("inst_zonal_wind_height10m")
!           call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % u10mi, rc=rc)
!           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!             line=__LINE__, &
!             file=__FILE__)) &
!             return  ! bail
!         case ("inst_merid_wind_height10m")
!           call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % v10mi, rc=rc)
!           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!             line=__LINE__, &
!             file=__FILE__)) &
!             return  ! bail
        end select
      end do
      call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail
    end do

  end subroutine chem_comp_import

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

end module chem_methods
