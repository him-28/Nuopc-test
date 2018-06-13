#include "settings.h"

program offlineApp

  !-----------------------------------------------------------------------------
  ! Generic offline application
  !-----------------------------------------------------------------------------

  use lndDriver, only : lnd_ini, lnd_run, lnd_fin

  implicit none

  ! Local Variables
  integer :: rc

  ! Initialize Applicaiton
  call lnd_ini(rc=rc)
  ! Run Application
  call lnd_run(rc=rc)
  ! Finalize Application
  call lnd_fin(rc=rc)

end program

