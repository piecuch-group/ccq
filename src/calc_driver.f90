module calc_driver

    ! This module contains routines that drive the various CC/EOMCC
    ! methods of this package. It takes care of initializing arrays
    ! and binary files.

    implicit none

contains

    subroutine run_calcs(sys, run, cc)

        ! Main routine. This is the entry point for all calculations

        ! In:
        !    sys: system information
        !    run: runtime information

        ! In/Out:
        !    cc: coupled-cluster information (including amplitudes, energies,
        !        and any other resulting data)

        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        use const, only: p
        use printing, only: print_calc_params, print_date, print_summary

        use external_correction, only: external_correction_driver
        use solver, only: solve_cc, solve_lcc
        use hbar_gen, only: hbar2
        use mm_correct, only: crcc23

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        logical :: cc_failed = .false.

        ! Start main wall clock
        call print_date('  ccq started on:')
        call init_system(sys, run, cc)


        ! Print calculation options parameters
        call print_calc_params(sys, run, cc)

        ! Externally corrected CC methods
        ! -------------------------------
        if (run%ext_cor) then
            call external_correction_driver(sys, run, cc)
        endif

        ! Solve coupled cluster
        ! ---------------------
        call solve_cc(sys, run, cc, cc_failed)

        ! Hbar generation
        ! ---------------
        ! [TODO] improve naming of hbar2
        if (run%hbar) then
            call hbar2(sys, cc)
        endif

        ! Solve left coupled cluster
        if (run%lcc) then
            call solve_lcc(sys, run, cc, cc_failed)
        endif

        ! Calculate MM correction
        if (run%mm_23) then
            call crcc23(sys, cc)
        endif

        ! Wrap up
        call clean_system(sys, run, cc, cc_failed)
        call print_summary(sys, run, cc)
        call print_date('  ccq finished on:')


    end subroutine run_calcs

    subroutine init_system(sys, run, cc)

        ! Initialize system for CC calculations

        use system, only: sys_t, run_t
        use cc_types, only: cc_t, init_p_space_slater

        use integrals, only: load_ints, load_sorted_ints
        use energy, only: calc_hf_energy, calc_orbital_energy

        use basis_types, only: init_basis_strings
        use excitations, only: init_excitations

        use cc_utils, only: get_t_sizes, get_t_sizes_act, open_t4_files

        use hdf5_io, only: init_h5_file

        use omp_lib, only: omp_set_num_threads

        type(sys_t), intent(in out) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(in out) :: cc

        ! [TODO] improve calculation type passing maybe use procedure pointers?
        if (run%sorted_ints) then
            call get_t_sizes_act(sys, cc)
        else
            call get_t_sizes(sys, cc, run%calc_type)
        endif

        ! Load integrals
        call load_ints(sys, run)
        ! [TODO] everything should be sorted in the future
        call load_sorted_ints(sys)

        ! Calculate initial energies
        call calc_orbital_energy(sys, sys%ints%e1int, sys%ints%e2int)
        sys%en_ref = calc_hf_energy(sys, sys%ints%e1int, sys%ints%e2int) + sys%en_repul

        ! Initialize determinant and excitation systems
        call init_basis_strings(sys%basis)
        call init_excitations(sys%basis)

        ! Initialize HDF5 master file
        call init_h5_file(run%h5_master_file)

        ! Initialize CC vectors
        if (run%lvl_q) call open_t4_files(sys, run)
        call init_vecs(run, cc)

        ! Original p space init
        ! [TODO] clean this
        !if (run%stoch) call init_p_space(sys, cc%stoch)
        if (run%stoch) call init_p_space_slater(sys, 'p_space_det', 3, cc%stoch)

        ! Initialize OpenMP threads
        call omp_set_num_threads(run%num_threads)

    end subroutine init_system

    subroutine clean_system(sys, run, cc, cc_failed)

        use const, only: tmp_unit, c_len
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        use integrals, only: unload_ints, unload_sorted_ints

        use basis_types, only: dealloc_basis_t
        use excitations, only: end_excitations

        use cc_utils, only: close_t4_files

        use errors, only: stop_all

        type(sys_t), intent(in out) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(in out) :: cc
        logical, intent(in) :: cc_failed

        character(len=c_len) :: cmd
        integer :: ierr


        ! Remove binary data
        if (.not. run%keep_bin) then
            write(cmd, '(a, a)') "rm -f ", trim(run%h5_master_file)
            call execute_command_line(trim(cmd), exitstat=ierr)
            if (ierr /= 0) &
                call stop_all('clean_system', 'ERROR: could not remove H5 master file')

            ! [TODO] add binary file too (run%bin_file)
        endif

        ! Close T vec file
        ! [TODO] cleanup: write bin files and deal with failure to converge
        if (run%lvl_q) call close_t4_files(run%keep_bin, cc_failed)

        ! [TODO] clean deallocations and allocations. Prolly better to have a module handling this
        ! Deallocate T
        if (allocated(cc%t_vec)) deallocate(cc%t_vec)
        if (allocated(cc%acc%t2_mc)) deallocate(cc%acc%t2_mc)

        ! Deallocate L
        if (allocated(cc%l_vec)) deallocate(cc%l_vec)

        call dealloc_basis_t(sys%basis)
        call end_excitations(sys%basis%excit_mask)

        call unload_ints(sys)
        call unload_sorted_ints(sys)

    end subroutine clean_system

    subroutine init_vecs(run, cc)

        use const, only: p, t_unit
        use cc_types, only: cc_t
        use errors, only: stop_all
        use hdf5_io, only: init_dset
        use system, only: run_t

        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        ! Initialize T vector
        if (.not. allocated(cc%t_vec)) then
            allocate(cc%t_vec(cc%t_size))
            cc%t_vec=0.0_p

            ! [TODO] this should not be here. The proper way of initializing vectors should be prepared
            if (run%calc_type == 'CCSD' .or. run%ext_cor) then
                call init_dset(run%h5_master_file, 't_vec', [cc%pos(6) - 1])
            else
                call init_dset(run%h5_master_file, 't_vec', [cc%t_size])
            endif
        endif

        ! T2 vector coming from MC
        ! [TODO] only load this when externally correcting
        if (.not. allocated(cc%acc%t2_mc)) then
            allocate(cc%acc%t2_mc(cc%pos(6)-cc%pos(3)))
            cc%acc%t2_mc = 0.0_p
        endif

        ! Lambda vector
        if (run%lcc) then
            ! For now up to doubles. This has to change based on the level of theory
            cc%l_size = cc%pos(6) - 1
            if (.not. allocated(cc%l_vec)) allocate(cc%l_vec(cc%l_size))

            ! Open binary file
            call init_dset(run%h5_master_file, 'l_vec', [cc%t_size])
        endif

    end subroutine init_vecs

end module calc_driver
