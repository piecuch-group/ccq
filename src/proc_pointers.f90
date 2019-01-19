module proc_pointers

    implicit none

    abstract interface
        subroutine i_update(sys, run, cc)

            use system, only: sys_t, run_t
            use cc_types, only: cc_t

            type(sys_t), intent(in) :: sys
            type(run_t), intent(in) :: run
            type(cc_t), intent(inout), target :: cc
        end subroutine i_update

        function i_calculate_energy(sys, cc) result(energy)

            use const, only: p
            use system, only: sys_t
            use cc_types, only: cc_t

            real(p) :: energy
            type(sys_t), intent(in) :: sys
            type(cc_t), intent(in), target :: cc

        end function i_calculate_energy
    end interface

    procedure(i_update), pointer :: update_ptr => null()
    procedure(i_calculate_energy), pointer :: calculate_energy_ptr => null()

end module proc_pointers
