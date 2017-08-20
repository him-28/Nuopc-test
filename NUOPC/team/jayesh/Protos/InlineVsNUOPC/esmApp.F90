program esmApp

  !-----------------------------------------------------------------------------
  ! Generic ESM application driver
  !-----------------------------------------------------------------------------

  use mpi
  use ESMF
  use NUOPC
  use ESM, only: esmSS => SetServices

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: esmComp
  double precision :: start_time, end_time, dtime
  integer :: rank, sz
  
  call MPI_Init(rc)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, rc)
  call MPI_Comm_size(MPI_COMM_WORLD, sz, rc)
  start_time = MPI_Wtime()
  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_LogWrite("esmApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the earth system Component
  esmComp = ESMF_GridCompCreate(name="esm", rc=rc)
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

  ! Set Profiling Attribute    
  call NUOPC_CompAttributeSet(esmComp, name="Profiling", value="0", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
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
  
  call ESMF_LogWrite("esmApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=rc)

  end_time = MPI_Wtime()
  dtime = end_time - start_time
  if(rank == 0) then
    call MPI_Reduce(MPI_IN_PLACE, dtime, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, rc)
    print *, sz, " procs: Took ", dtime, " s"
  else
    call MPI_Reduce(dtime, dtime, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, rc)
  end if
  call MPI_Finalize(rc)
  
end program  
