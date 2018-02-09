module atmos_model_mod

  implicit none

  private

  public :: update_atmos_model_fields

contains

subroutine update_atmos_model_fields(de, rc)

  use ESMF
  use module_cplfields,  only: cplFieldGet

  integer, optional, intent(in)  :: de
  integer, optional, intent(out) :: rc

  !--- local variables
  integer :: localrc
  integer :: localDe
  integer :: ni, nj, nk, nt
  integer :: nb, ix, i, j, k, it

  ! -- pointers for fields not yet connected
  real(ESMF_KIND_R8), pointer :: p2d(:,:), p3d(:,:,:)

  real(ESMF_KIND_R8), dimension(:,:,:),   pointer :: prsl, phil, &
                                                     prsi, phii, &
                                                     temp, &
                                                     ua, va, vvl
  real(ESMF_KIND_R8), dimension(:,:,:,:), pointer :: q

  integer, save :: timeSlice = 0

  ! -- begin
  if (present(rc)) rc = ESMF_SUCCESS
  
  localDe = 0
  if (present(de)) localDe = de


  timeSlice = timeSlice + 1

  !--- retrieve references to allocated memory for each field
  call cplFieldGet('export','air_pressure', localDe=de, farrayPtr3d=prsi, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','air_pressure_in_model_layers', localDe=de, farrayPtr3d=prsl, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','geopotential', localDe=de, farrayPtr3d=phii, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','geopotential_in_model_layers', localDe=de, farrayPtr3d=phil, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','air_temperature', localDe=de, farrayPtr3d=temp, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','x_wind', localDe=de, farrayPtr3d=ua,   rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','y_wind', localDe=de, farrayPtr3d=va,   rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','omega', localDe=de, farrayPtr3d=vvl,  rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  call cplFieldGet('export','mass_fraction_of_tracers_in_air', localDe=de, farrayPtr4d=q, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return

  ni = size(prsl, dim=1)
  nj = size(prsl, dim=2)
  nk = size(prsl, dim=3)
  nt = size(q, dim=4)

  !--- handle all three-dimensional variables
  do k = 1, nk
    do j = 1, nj
      do i = 1, ni
        ix = i+(j-1)*ni + (k-1)*ni*nj
        ix = timeSlice * ix
        !--- interface values
        prsi(i,j,k) = ix
        phii(i,j,k) = ix
        !--- layer values
        prsl(i,j,k) = ix
        phil(i,j,k) = ix
        temp(i,j,k) = ix
        ua  (i,j,k) = ix
        va  (i,j,k) = ix
        vvl (i,j,k) = ix
      enddo
    enddo
  enddo

  !--- top interface values
  k = nk+1
  do j = 1, nj
    do i = 1, ni
      ix = i+(j-1)*ni + (k-1)*ni*nj
        ix = timeSlice*ix
      prsi(i,j,k) = ix
      phii(i,j,k) = ix
    enddo
  enddo

  !--- tracers quantities
  do it = 1, nt
    do k = 1, nk
      do j = 1, nj
        do i = 1, ni
          ix = i+(j-1)*ni + (k-1)*ni*nj
        ix = timeSlice*ix
          q(i,j,k,it) = 1.e-05_ESMF_KIND_R8 * ix * nt
        enddo
      enddo
    enddo
  enddo

  ! -- fields not yet available from FV3

  call cplFieldGet('export','area_type', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 1.
  call cplFieldGet('export','atmosphere_boundary_layer_thickness', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 60.
  call cplFieldGet('export','cell_area', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 1.
  call cplFieldGet('export','convective_rainfall_amount', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 0.01
  call cplFieldGet('export','exchange', localDe=de, farrayPtr3d=p3d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p3d = 0.
  call cplFieldGet('export','friction_velocity', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 0.1
  call cplFieldGet('export','rainfall_amount', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 0.1
  call cplFieldGet('export','soil_moisture_content', localDe=de, farrayPtr3d=p3d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 0.01
  call cplFieldGet('export','surface_downwelling_shortwave_flux_in_air', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 100.
  call cplFieldGet('export','surface_mask', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 1.
  call cplFieldGet('export','surface_skin_temperature', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 300.
  call cplFieldGet('export','surface_upward_sensible_heat_flux', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 150.
  call cplFieldGet('export','thickness_of_snowfall_amount', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 0.
  call cplFieldGet('export','vegetation_type', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 1.
  call cplFieldGet('export','vegetation_area_fraction', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 0.01
  call cplFieldGet('export','z_over_l', localDe=de, farrayPtr2d=p2d, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__, rcToReturn=rc)) return
  p2d = 0.1

end subroutine update_atmos_model_fields

end module atmos_model_mod
