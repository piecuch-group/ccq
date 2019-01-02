module errors

    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit

    implicit none

contains

    subroutine stop_all(var, msg)

        character(len=*), intent(in) :: var
        character(len=*), intent(in) :: msg

        write(error_unit, '(a)') trim(var//' '//msg)
        call exit(1)

    end subroutine stop_all

end module errors
