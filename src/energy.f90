module energy

    implicit none

contains

    function calculate_energy(sys, cc) result(energy)

        use const, only: dp
        use system, only: sys_t, cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in) :: cc

        real(dp) :: e1a,e1b,e2a,e2b,e2c,e1a1a,e1a1b,e1b1b
        real(dp) :: energy

        ! Fill this
        call old_energy(sys%froz, sys%occ_a, sys%occ_b, sys%orbs, &
            sys%ints%f_a, sys%ints%f_b,sys%ints%v_aa,sys%ints%v_bb,sys%ints%v_ab, &
            cc%t_vec(cc%pos(1)), &
            cc%t_vec(cc%pos(2)), &
            cc%t_vec(cc%pos(3)), &
            cc%t_vec(cc%pos(4)), &
            cc%t_vec(cc%pos(5)), &
            e1a,e1b,e2a,e2b,e2c,e1a1a,e1a1b,e1b1b)

        energy = e1a+e1b+e2a+e2b+e2c+e1a1a+e1b1b+e1a1b

    end function calculate_energy

end module energy
