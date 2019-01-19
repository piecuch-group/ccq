module calc_driver

    implicit none

contains

    subroutine run_calcs(sys, run, cc)

        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        use const, only: p
        use printing, only: print_calc_params, print_date, print_summary

        use cluster_analysis, only: cluster_analysis_driver_opt
        use solver, only: solve_cc, solve_lcc
        use hbar, only: hbar2
        use mm_correct, only: crcc23

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        logical :: cc_failed = .false.

        ! Start clock
        call print_date('  ccq started on:')
        call init_system(sys, run, cc)


        ! Print calculation options parameters
        call print_calc_params(sys, run, cc)

        ! Externally corrected
        ! --------------------

        if (run%ext_cor) then
            call cluster_analysis_driver_opt(sys, run, cc)
        endif

        ! Solve coupled cluster
        ! ---------------------
        call solve_cc(sys, run, cc, cc_failed)

        ! Hbar generation
        ! ---------------
        if (run%hbar) then
            call hbar2(sys, run, cc)
        endif

        ! Solve left coupled cluster
        if (run%lcc) then
            call solve_lcc(sys, run, cc, cc_failed)
        endif

        ! Calculate MM correction
        if (run%mm_23) then
            call crcc23(sys, run, cc)
        endif

        ! ---------------------

        ! Wrap up
        call clean_system(sys, run, cc, cc_failed)
        call print_summary(sys, run, cc)
        call print_date('  ccq finished on:')


    end subroutine run_calcs

    subroutine init_system(sys, run, cc)

        use system, only: sys_t, run_t
        use cc_utils, only: open_t4_files
        use cc_types, only: cc_t
        use integrals, only: load_ints, load_sorted_ints
        use basis_types, only: init_basis_strings
        use excitations, only: init_excitations

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        ! [TODO] improve calculation type passing maybe use procedure pointers?
        if (run%sorted_ints) then
            call get_t_sizes_act(sys, cc)
        else
            call get_t_sizes(sys, cc)
        endif

        ! Load integrals
        call load_ints(sys, run)
        if (run%sorted_ints) then
            call load_sorted_ints(sys, run)
        endif

        ! Initialize determinant and excitation systems
        call init_basis_strings(sys%basis)
        call init_excitations(sys%basis)

        ! Initialize vectors
        if (run%lvl_q) call open_t4_files(sys, run)
        call init_t_vec(run, cc)
        if (run%lcc) then
            cc%l_size = cc%pos(6) - 1
            allocate(cc%l_vec(cc%l_size))
        endif


    end subroutine init_system

    subroutine clean_system(sys, run, cc, cc_failed)

        use const, only: t_unit, t_vecs_unit
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use integrals, only: unload_ints, unload_sorted_ints
        use basis_types, only: dealloc_basis_t
        use excitations, only: end_excitations
        use cc_utils, only: close_t4_files

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc
        logical, intent(in) :: cc_failed

        ! [TODO] clean deallocations and allocations. Prolly better to have a module handling this
        ! Deallocate T
        if (allocated(cc%t_vec)) deallocate(cc%t_vec)
        if (allocated(cc%acc%t2_mc)) deallocate(cc%acc%t2_mc)

        ! Deallocate L
        if (allocated(cc%l_vec)) deallocate(cc%l_vec)

        ! Close T vec file
        if (run%keep_bin .or. cc_failed) then
            close(t_unit)
        else
            close(t_unit, status='delete')
        endif
        if (run%lvl_q) call close_t4_files(sys, run%keep_bin, cc_failed)

        call dealloc_basis_t(sys%basis)
        call end_excitations(sys%basis%excit_mask)
        call unload_ints(sys)
        if (run%sorted_ints) then
            call unload_sorted_ints(sys)
        endif

    end subroutine clean_system

    subroutine init_t_vec(run, cc)

        use const, only: p, t_unit
        use printing, only: abort_cc
        use system, only: run_t
        use cc_types, only: cc_t

        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc
        logical :: t_exists

        if (.not. allocated(cc%t_vec)) allocate(cc%t_vec(cc%t_size))
        if (.not. allocated(cc%acc%t2_mc)) allocate(cc%acc%t2_mc(cc%pos(6)-cc%pos(3)))

        if (run%restart) then
            inquire(file=trim(run%bin_file), exist=t_exists)
            if (.not. t_exists) &
                call abort_cc("RESTART ERROR: Vector file not found "//trim(run%bin_file))

            open(t_unit,file=trim(run%bin_file),form='unformatted')
            rewind(t_unit)
            read(t_unit) cc%t_vec
            cc%acc%t2_mc = cc%t_vec(cc%pos(3):cc%pos(6)-1)
        else
            open(t_unit,file=trim(run%bin_file),form='unformatted')
            cc%t_vec=0.0_p
            cc%acc%t2_mc = 0.0_p
        endif

    end subroutine init_t_vec

    subroutine get_t_sizes(sys, cc)

        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc

        integer :: k1, k2, k3, k4
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d, t_size

        K1 = sys%occ_a-sys%froz
        K3 = sys%orbs-sys%occ_a
        K2 = sys%occ_b-sys%froz
        K4 = sys%orbs-sys%occ_b

        t_size = 0
        cc%pos(1) = t_size+1

        t_size = t_size+K1*K3
        cc%pos(2) = t_size+1

        t_size = t_size+K2*K4
        cc%pos(3) = t_size+1

        t_size = t_size+K1*K1*K3*K3
        cc%pos(4) = t_size+1

        t_size = t_size+K2*K2*K4*K4
        cc%pos(5) = t_size+1

        t_size = t_size+K1*K2*K3*K4
        cc%pos(6) = t_size+1

        t_size = t_size+K3*K3*K3*K1*K1*K1
        cc%pos(7) = t_size+1

        t_size = t_size+K4*K4*K4*K2*K2*K2
        cc%pos(8) = t_size+1

        t_size = t_size+K3*K4*K3*K1*K2*K1
        cc%pos(9) = t_size+1

        cc%t_size = t_size+K4*K4*K3*K2*K2*K1

    end subroutine get_t_sizes

    subroutine get_t_sizes_act(sys, cc)

        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc

        integer :: nocc_a, nocc_b, nunocc_a, nunocc_b
        integer :: actocc_a, actocc_b, actunocc_a, actunocc_b
        integer :: non_actocc, non_actunocc

        nocc_a = sys%occ_a - sys%froz
        nocc_b = sys%occ_b - sys%froz
        nunocc_a = sys%orbs - sys%occ_a
        nunocc_b = sys%orbs - sys%occ_b
        non_actocc = sys%act_occ_b - sys%froz
        non_actunocc = sys%orbs - sys%act_unocc_a
        actocc_a = sys%occ_a - sys%act_occ_b
        actocc_b = sys%occ_b - sys%act_occ_b
        actunocc_a = sys%act_unocc_a - sys%occ_a
        actunocc_b = sys%act_unocc_a - sys%occ_b

        ! T1 alpha size
        ! K1A
        cc%pos(1) = 1

        ! T1 beta size
        ! K1B
        cc%pos(2)=cc%pos(1)+nocc_a*nunocc_a

        ! T2 alpha-alpha size
        ! K2A
        cc%pos(3)=cc%pos(2)+nocc_b*nunocc_b

        ! T2 alpha-beta size
        ! K2B
        cc%pos(4)=cc%pos(3)+nocc_a*nocc_a*nunocc_a*nunocc_a

        ! T2 beta-beta size
        ! K2C
        cc%pos(5)= cc%pos(4)+nocc_a*nocc_b*nunocc_a*nunocc_b

        ! T3 sizes
        ! K3A
        cc%pos(6)=cc%pos(5)+nocc_b*nocc_b*nunocc_b*nunocc_b
        ! K3B1
        cc%pos(7)=cc%pos(6)+nunocc_a*nunocc_a*actunocc_a*nocc_a*nocc_a*actocc_a  !1**1**
        ! K3B2
        cc%pos(8)=cc%pos(7)+nunocc_b*nunocc_a*actunocc_a*nocc_b*nocc_a*actocc_a  !1**1**
        ! K3B3
        cc%pos(9)=cc%pos(8)+actunocc_b*non_actunocc*non_actunocc*actocc_b*non_actocc*non_actocc  !001001
        ! K3B4
        cc%pos(10)=cc%pos(9)+actunocc_b*non_actunocc*non_actunocc*nocc_b*nocc_a*actocc_a  !1**001
        ! K3C1
        cc%pos(11)=cc%pos(10)+nunocc_b*nunocc_a*actunocc_a*actocc_b*non_actocc*non_actocc  !0011**
        ! K3C2
        cc%pos(12)=cc%pos(11)+nunocc_b*actunocc_b*nunocc_a*nocc_b*actocc_b*nocc_a  !*1**1*
        ! K3C3
        cc%pos(13)=cc%pos(12)+non_actunocc*non_actunocc*actunocc_a*non_actocc*non_actocc*actocc_a  !100100
        ! K3C4
        cc%pos(14)=cc%pos(13)+non_actunocc*non_actunocc*actunocc_a*nocc_b*actocc_b*nocc_a  !*1*100
        ! K3D
        cc%pos(15)=cc%pos(14)+nunocc_b*actunocc_b*nunocc_a*non_actocc*non_actocc*actocc_a  !100*1*
        ! End
        cc%pos(16)=cc%pos(15)+nunocc_b*nunocc_b*actunocc_b*nocc_b*nocc_b*actocc_b  !1**1**

        ! Total T size
        cc%t_size = cc%pos(16) - 1

    end subroutine get_t_sizes_act

end module calc_driver

