module calc_driver

    implicit none

contains

    subroutine run_calcs(sys, run, cc)

        use cc_utils, only: open_t4_files
        use const, only: p, t_unit, t_vecs_unit
        use cluster_analysis, only: cluster_analysis_driver_opt
        use basis_types, only: init_basis_strings, dealloc_basis_t
        use diis, only: init_t_vecs
        use excitations, only: init_excitations, end_excitations
        use integrals, only: load_ints, unload_ints
        use printing, only: io, init_print, print_header, print_calc_params, print_config, close_print, print_date, print_summary
        use solver, only: solve_cc
        use system, only: sys_t, run_t, cc_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        logical :: cc_failed = .false.

        ! Start clock
        call print_date('  ccq started on:')

        ! Load integrals
        call load_ints(sys, run)

        ! Initialize determinant and excitation systems
        call init_basis_strings(sys%basis)
        call init_excitations(sys%basis)

        ! Print calculation options parameters
        call print_calc_params(sys, run, cc)

        ! Initialize vectors
        call get_t_sizes(sys, cc)
        call init_t_vecs(cc%t_size)
        !if (run%lvl_q) call open_t4_files(sys, run%restart)
        call init_t_vec(run, cc)

        ! Run cluster analysis for externally corrected calculations
        if (run%ext_cor) then
            call cluster_analysis_driver_opt(sys, run, cc)
            !call antisym_t(sys, cc, run%lvl_q)
        else
            cc%en_cor = 0.0_p
            if (run%lvl_q) call open_t4_files(sys, run)
        endif

        ! Solve coupled cluster
        call solve_cc(sys, run, cc, cc_failed)

        ! Close T vec file
        close(t_vecs_unit, status='delete')
        if (run%keep_bin .or. cc_failed) then
            close(t_unit)
        else
            close(t_unit, status='delete')
        endif

        call print_summary(sys, run, cc)
        call print_date('  ccq finished on:')

        call dealloc_basis_t(sys%basis)
        call end_excitations(sys%basis%excit_mask)
        call unload_ints(sys)

    end subroutine run_calcs

    subroutine init_t_vec(run, cc)

        use const, only: dp, t_unit
        use printing, only: abort_cc
        use system, only: run_t, cc_t

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
            cc%t_vec=0.0_dp
            cc%acc%t2_mc = 0.0_dp
        endif

    end subroutine init_t_vec

    subroutine get_t_sizes(sys, cc)

        use system, only: sys_t, cc_t

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

end module calc_driver

