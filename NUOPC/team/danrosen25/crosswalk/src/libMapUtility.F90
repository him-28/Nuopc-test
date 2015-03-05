
module StdNameMap_class
    integer, parameter :: SNM_SUCCESS = 0
    integer, parameter :: SNM_ERROR_NOFILE = 1
    integer, parameter :: SNM_ERROR_FILEFMT = 2
    integer, parameter :: SNM_ERROR_NOCSDMS = 3
    integer, parameter :: SNM_ERROR_NOCF = 4
    integer, parameter :: SNM_ERROR_STRLEN = 5

    integer, parameter :: SNM_MAXSTR = 128

    type StdNamePair
        character(SNM_MAXSTR) :: csdms
        character(SNM_MAXSTR) :: cf
    end type

    type(StdNamePair),dimension(:) :: stdNameMap(10)
    integer :: entries

contains

    integer function getSize()
        getSize = entries
    end function

    subroutine addEntry(csdms,cf)
        character(*),intent(in) :: csdms
        character(*),intent(in) :: cf

        entries = entries + 1

        stdNameMap(entries)%csdms = csdms
        stdNameMap(entries)%cf = cf
    end subroutine

    subroutine findEntryCsdms(csdms,cf)
        character(*),intent(in) :: csdms
        character(*),intent(out) :: cf

        integer :: i

        do i=1,size(stdNameMap)
            if(stdNameMap(i)%csdms .eq. csdms) then
                cf = stdNameMap(i)%cf
                return
            end if
        end do
    end subroutine

    subroutine findEntryCf(cf,csdms)
        character(*),intent(in) :: cf
        character(*),intent(out) :: csdms

        integer :: i

        do i=1,size(stdNameMap)
            if(stdNameMap(i)%cf .eq. cf) then
                csdms = stdNameMap(i)%csdms
                return
            end if
        end do
    end subroutine

    subroutine printEntries()
        integer :: i
        character(10) :: entry

        do i=1,entries
            write( entry, '(i10)' ) i
            print *,"Entry(",trim(adjustl(entry)),")=", &
                trim(adjustl(stdNameMap(i)%csdms)),":", &
                trim(adjustl(stdNameMap(i)%cf))
        end do
    end subroutine

end module

subroutine loadMapping(file,rc)
    use StdNameMap_class
    implicit none
    ! Dummy Variables
    character(*),intent(in) :: file
    integer,intent(out),optional :: rc
    ! Local Variables
    character(SNM_MAXSTR) :: readlineFrom,readlineTo
    logical :: fexists
    integer :: io

    if(present(rc)) rc = SNM_SUCCESS

    if(file .eq. "") then
        return
    end if

    inquire(file=file, exist=fexists)
    if(.not. fexists) then
        if(present(rc)) rc = SNM_ERROR_NOFILE
        return
    end if

    open(15,file=file)
    do
        readlineFrom = ""
        readlineTo = ""
        read(15,*,IOSTAT=io) readlineFrom,readlineTo

        if (len_trim(readlineFrom) .gt. 0) then
            if(io .lt. 0) then
                if(present(rc)) rc = SNM_ERROR_FILEFMT
                exit
            end if
            call addEntry(readlineFrom,readlineTo)
        else if (io .gt. 0) then
            if(present(rc)) rc = SNM_ERROR_FILEFMT
            exit
        else if (io .lt. 0) then
            exit
        end if

    end do
    close(15)

end subroutine

subroutine csdmsToCF(from,to,rc)
    use StdNameMap_class
    implicit none
    ! Dummy Variables
    character(*),intent(in) :: from
    character(*),intent(out) :: to
    integer,intent(out),optional :: rc
    ! Local Variables
    character(SNM_MAXSTR) :: lto = "out_standard_name"

    if(present(rc)) rc = SNM_SUCCESS
    to = lto

end subroutine

subroutine cfToCsdms(from,to,rc)
    use StdNameMap_class
    implicit none
    ! Dummy Variables
    character(*),intent(in) :: from
    character(*),intent(out) :: to
    integer,intent(out),optional :: rc
    ! Local Variables
    character(SNM_MAXSTR) :: lto = "out_standard_name"

    if(present(rc)) rc = SNM_SUCCESS
    to = lto

end subroutine

subroutine printError(errorCode)
    use StdNameMap_class
    implicit none
    ! Dummy Variables
    integer, intent(in) :: errorCode

    select case(errorCode)
        case(SNM_SUCCESS)
            print *,"SUCCESS: No Error"
        case(SNM_ERROR_NOFILE)
            print *,"ERROR: File Not Found"
        case(SNM_ERROR_FILEFMT)
            print *,"ERROR: File format error"
        case(SNM_ERROR_NOCSDMS)
            print *,"ERROR: No CSDMS standard name entry"
        case(SNM_ERROR_NOCF)
            print *,"ERROR: No CF standard name entry"
        case(SNM_ERROR_STRLEN)
            print *,"ERROR: String length out of bounds"
        case default
            print *,"ERROR: Unknown"
    end select

end subroutine

subroutine printMap()
    use StdNameMap_class
    implicit none

    call printEntries()
end subroutine
