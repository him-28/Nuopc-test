!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

program jediDummyApp

  !-----------------------------------------------------------------------------
  ! Mimick JEDI application driver
  !-----------------------------------------------------------------------------

  use ESMF
  use ESM, only: esmSS => SetServices
  
  use iso_c_binding
  use jedi_nuopc_mod

  implicit none
  
  type(model_nuopc_type)  :: self
  type(fv3jedi_geom)      :: geom
  type(c_ptr)             :: c_conf
  type(fv3jedi_state)     :: state
  type(datetime)          :: fdate, fdate1, fdate2
  
  ! create
  call model_nuopc_create(self, geom, c_conf)

  ! first "outer loop"
  ! initialize
  fdate%string_date = "2018-04-14T21:00:00Z"  ! set execution to this currTime
  call model_nuopc_initialize(self, state, fdate)
  ! step 1
  fdate1%string_date = "2018-04-14T21:00:00Z"
  fdate2%string_date = "2018-04-14T21:20:00Z"
  call model_nuopc_step(self, state, fdate1, fdate2)
  ! step 2
  fdate1%string_date = fdate2%string_date
  fdate2%string_date = "2018-04-14T21:40:00Z"
  call model_nuopc_step(self, state, fdate1, fdate2)
  ! step 3
  fdate1%string_date = fdate2%string_date
  fdate2%string_date = "2018-04-14T22:00:00Z"
  call model_nuopc_step(self, state, fdate1, fdate2)
  ! finalize
  call model_nuopc_finalize(self, state, fdate)
  
  ! second "outer loop"
  ! initialize
  fdate%string_date = "2018-04-14T21:00:00Z"  ! reset execution to this currTime
  call model_nuopc_initialize(self, state, fdate)
  ! step 1
  fdate1%string_date = "2018-04-14T21:00:00Z"
  fdate2%string_date = "2018-04-14T21:20:00Z"
  call model_nuopc_step(self, state, fdate1, fdate2)
  ! step 2
  fdate1%string_date = fdate2%string_date
  fdate2%string_date = "2018-04-14T21:40:00Z"
  call model_nuopc_step(self, state, fdate1, fdate2)
  ! step 3
  fdate1%string_date = fdate2%string_date
  fdate2%string_date = "2018-04-14T22:00:00Z"
  call model_nuopc_step(self, state, fdate1, fdate2)
  ! finalize
  call model_nuopc_finalize(self, state, fdate)
  
  ! delete
  call model_nuopc_delete(self)
  
end program  
