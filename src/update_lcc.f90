module update_lcc

    ! Module holding the update routines analog to
    ! update_cc, but for the lambda  CC state

    implicit none

contains

    subroutine update_l2(sys, run, cc)

        use const, only: p
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        real(p), pointer :: t(:) => null()
        real(p), pointer :: l(:) => null()
        real(p), pointer :: lh(:) => null()

        integer :: n0, n1, n2 ,n3
        integer :: k1, k2, k3, k4
        integer :: k1a, k1b, k2a, k2b, k2c
        integer :: iroot

        iroot = 0

        n0 = sys%froz
        n1 = sys%occ_a
        n2 = sys%occ_b
        n3 = sys%orbs
        k1 = n1 - n0
        k2 = n2 - n0
        k3 = n3 - n1
        k4 = n3 - n2

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)

        t => cc%t_vec
        l => cc%l_vec
        lh => cc%lh_vec

        lh = 0.0_p

        associate(fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
                intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab, &
                h1a=>cc%hbar%a, h1b=>cc%hbar%b, h2a=>cc%hbar%aa, h2b=>cc%hbar%ab, &
                h2c=>cc%hbar%bb)

            call L_input(N0,N1,N2,N3,iroot, &
                H1A,H1B,H2A,H2B,H2C,l(K1A),l(K1B),l(K2A),l(K2B),l(K2C), &
                LH(K1A),LH(K1B),LH(K2A),LH(K2B),LH(K2C))

            call L1A_update(N0,N1,N2,N3,LH(K1A), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L1B_update(N0,N1,N2,N3,LH(K1B), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L2A_update(N0,N1,N2,N3,LH(K2A), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L2B_update(N0,N1,N2,N3,LH(K2B), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L2C_update(N0,N1,N2,N3,LH(K2C), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            !if(iroot.ne.0)LH=LH-ECor*L
            call L_update(N0,N1,N2,N3,run%shift,H1A,H1B,cc%en_cor, &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C), &
                LH(K1A),LH(K1B),LH(K2A),LH(K2B),LH(K2C))

        end associate

    end subroutine update_l2

end module update_lcc
