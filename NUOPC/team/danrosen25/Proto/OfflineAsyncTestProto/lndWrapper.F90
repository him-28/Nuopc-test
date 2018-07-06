#include "settings.h"

module lndWrapper

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndDriver

  implicit none
  
  private

  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(lnd_cmp, rc)
    type(ESMF_GridComp)  :: lnd_cmp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(lnd_cmp, ESMF_METHOD_INITIALIZE, &
      userRoutine=esmf_lnd_ini, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(lnd_cmp, ESMF_METHOD_RUN, &
      userRoutine=esmf_lnd_run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(lnd_cmp, ESMF_METHOD_FINALIZE, &
      userRoutine=esmf_lnd_fin, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine SetServices

  subroutine esmf_lnd_ini(lnd_cmp,imp_state,exp_state,clock,rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: lnd_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    type(ESMF_FieldBundle)                  :: fields
    type(ESMF_RouteHandle)                  :: rh
    integer                                 :: itemCount
    character (len=ESMF_MAXSTR),allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag),allocatable   :: itemTypeList(:)
    integer                                 :: iIndex

    rc = ESMF_SUCCESS

    ! Get RH from imp_state
    call ESMF_StateGet(imp_state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    allocate(itemNameList(itemCount),stat=rc)
    if (ESMF_LogFoundAllocError(statusToCheck=rc, &
      msg="Allocation of itemNameList memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    allocate(itemTypeList(itemCount),stat=rc)
    if (ESMF_LogFoundAllocError(statusToCheck=rc, &
      msg="Allocation of itemTypeList memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateGet(imp_state, itemorderflag=ESMF_ITEMORDER_ADDORDER &
      , itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_ROUTEHANDLE ) then
        call ESMF_StateGet(imp_state, itemName=itemNameList(iIndex) &
          , routehandle=rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
    deallocate(itemNameList,stat=rc)
    if (ESMF_LogFoundDeallocError(statusToCheck=rc, &
      msg="Deallocation of itemNameList memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    deallocate(itemTypeList,stat=rc)
    if (ESMF_LogFoundDeallocError(statusToCheck=rc, &
      msg="Deallocation of itemTypeList memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Initialize Applicaiton
    call lnd_ini(rh=rh,fields=fields,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Store fields in imp_state
    call ESMF_StateAdd(imp_state, fieldbundleList=(/fields/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine esmf_lnd_ini

  !-----------------------------------------------------------------------------

  subroutine esmf_lnd_run(lnd_cmp,imp_state,exp_state,clock,rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: lnd_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    ! Run Application
    call lnd_run(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine esmf_lnd_run

  !-----------------------------------------------------------------------------

  subroutine esmf_lnd_fin(lnd_cmp,imp_state,exp_state,clock, rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: lnd_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    ! Finalize Application
    call lnd_fin(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine esmf_lnd_fin

  !-----------------------------------------------------------------------------

end module lndWrapper
