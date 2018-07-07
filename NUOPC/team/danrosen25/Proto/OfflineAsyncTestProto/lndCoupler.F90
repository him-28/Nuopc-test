#include "settings.h"

module lndCoupler

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF

  implicit none
  
  private

  public SetServices
  public cpl_run

  logical                :: initialized = .false.
  type(ESMF_FieldBundle) :: phyFields
  type(ESMF_FieldBundle) :: wrtFields
  type(ESMF_RouteHandle) :: rh
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(cpl_cmp, rc)
    type(ESMF_CplComp)   :: cpl_cmp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_CplCompSetEntryPoint(cpl_cmp, ESMF_METHOD_INITIALIZE, &
      userRoutine=esmf_cpl_ini, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_CplCompSetEntryPoint(cpl_cmp, ESMF_METHOD_FINALIZE, &
      userRoutine=esmf_cpl_fin, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine SetServices

  subroutine esmf_cpl_ini(cpl_cmp,imp_state,exp_state,clock,rc)
    ! ARGUMENTS
    type(ESMF_CplComp)  :: cpl_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    integer                                 :: itemCount
    character (len=ESMF_MAXSTR),allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag),allocatable   :: itemTypeList(:)
    integer                                 :: iIndex

    rc = ESMF_SUCCESS

    ! Get fieldbundle from imp_state
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
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELDBUNDLE ) then
        call ESMF_StateGet(imp_state, itemName=itemNameList(iIndex) &
          , fieldbundle=phyFields, rc=rc)
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

    ! Get fieldbundle from exp_state
    call ESMF_StateGet(exp_state, itemCount=itemCount, rc=rc)
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
    call ESMF_StateGet(exp_state, itemorderflag=ESMF_ITEMORDER_ADDORDER &
      , itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELDBUNDLE ) then
        call ESMF_StateGet(exp_state, itemName=itemNameList(iIndex) &
          , fieldbundle=wrtFields, rc=rc)
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

    call ESMF_FieldBundleRedistStore(phyFields, wrtFields, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    initialized = .true.
  end subroutine esmf_cpl_ini

  !-----------------------------------------------------------------------------

  subroutine cpl_run(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    if ( initialized ) then
      call ESMF_FieldBundleRedist(phyFields, wrtFields, routehandle=rh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else
      call ESMF_LogSetError(ESMF_RC_OBJ_INIT, &
        msg="Coupler is not initialized.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
  end subroutine cpl_run

  !-----------------------------------------------------------------------------

  subroutine esmf_cpl_fin(cpl_cmp,imp_state,exp_state,clock, rc)
    ! ARGUMENTS
    type(ESMF_CplComp)  :: cpl_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_FieldBundleRedistRelease(routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    initialized = .false.
  end subroutine esmf_cpl_fin

  !-----------------------------------------------------------------------------

end module lndCoupler
