module solver

    ! Module containing all solver routines.

    use const, only: p

    implicit none


contains

    subroutine solve_cc(sys, run, cc, failed)

        ! Solve ground-state CC equations

        ! In:
        !    sys: system information
        !    run: runtime information

        ! In/Out:
        !    cc: coupled-cluster information (including T vector, energies, etc.)
        !    failed: a boolean flag used to signal the calc driver that convergence
        !    failed and amplitudes must be stored

        use const, only: p, t_unit, t_vecs_unit
        use energy, only: calculate_sorted_energy, calculate_unsorted_energy
        use printing, only: io, print_iter_head, print_date
        use proc_pointers, only: update_ptr, calculate_energy_ptr
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use update_cc, only: update_clusters_t4, update_clusters_t3_opt
        use solver_types, only: conv_t

        implicit none

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        logical, intent(inout) :: failed
        type(conv_t) :: conv

        ! Coupled cluster program title
        write(io, '(a)') 'Coupled cluster'
        write(io, '(a)') '---------------'

        ! Select CC procedure
        if (run%sorted_ints) then
            update_ptr => update_clusters_t3_opt
            calculate_energy_ptr => calculate_sorted_energy
        else
            update_ptr => update_clusters_t4
            calculate_energy_ptr => calculate_unsorted_energy
        endif

        ! [TODO] The restarting code should be improved
        if (run%restart) then
            conv%en_cor = calculate_energy_ptr(sys, cc)
            write(io, '(2x,a27,2x,f16.10/)') 'Restart correlation energy', conv%en_cor
        endif

        ! Print wall time and iteration table header
        call print_date('  CC program started on:')
        call print_iter_head()


        ! Only perform DIIS with up to doubles when running
        ! externally corrected calculations
        if (run%ext_cor) then
            conv%vec_size = cc%pos(6) - 1
        ! [TODO] this is only temporal for saving memory. Should be
        ! moved to the calculation init
        else if (trim(run%calc_type) == 'CCSD') then
            conv%vec_size = cc%pos(6) - 1
        else
            conv%vec_size = cc%t_size
        endif

        ! Load convergence pointers. These are used to allow the Jacobi
        ! iterator to work on any method

        conv%vec_ptr => cc%t_vec
        conv%filename = run%h5_master_file
        conv%iter_dset_name = 'cc_iter_mat'
        conv%vec_dset_name = 't_vec'
        conv%vec_unit = t_unit
        conv%vecs_unit = t_vecs_unit

        ! Perform Jacobi iterations
        call jacobi_iter(sys, run, cc, conv)

        ! [TODO] clean interface a bit more
        cc%en_cor = conv%en_cor
        failed = conv%failed

        ! Print ending wall time
        call print_date('  CC program ended on:')

    end subroutine solve_cc

    subroutine solve_lcc(sys, run, cc, failed)

        ! Solve left ground-state CC equations. It calculates the
        ! linear de-excitation amplitudes that match the CC ground-state
        ! energy.

        ! In:
        !    sys: system information
        !    run: runtime information

        ! In/Out:
        !    cc: coupled-cluster information (including T vector, energies, etc.)
        !    failed: a boolean flag used to signal the calc driver that convergence
        !    failed and amplitudes must be stored

        use const, only: l_unit, l_vecs_unit
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use energy, only: calculate_left_energy
        use proc_pointers, only: update_ptr, calculate_energy_ptr
        use hbar_gen, only: add_twobody_hbar
        use update_lcc, only: update_l2
        use solver_types, only: conv_t
        use printing, only: io, print_date, print_iter_head

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        logical, intent(inout) :: failed
        type(conv_t) :: conv

        ! [TODO] this affects the correction and EOM
        call add_twobody_hbar(sys, cc)

        ! Coupled cluster program title
        write(io, '(a)') 'Left Coupled cluster'
        write(io, '(a)') '--------------------'

        ! Up to doubles for now
        update_ptr => update_l2
        calculate_energy_ptr => calculate_left_energy
        conv%vec_size = cc%l_size
        allocate(cc%lh_vec(cc%l_size))

        ! Print starting wall clock
        call print_date('  L-CC program started on:')
        call print_iter_head()

        ! Load convergence pointers. These are used to allow the Jacobi
        ! iterator to work on any method
        conv%vec_ptr => cc%l_vec
        conv%filename = run%h5_master_file
        conv%iter_dset_name = 'lcc_iter_mat'
        conv%vec_dset_name = 'l_vec'
        conv%vec_unit = l_unit
        conv%vecs_unit = l_vecs_unit

        ! Perform Jacobi iterations
        call jacobi_iter(sys, run, cc, conv)

        ! [TODO] check that energies match
        !conv%en_cor
        failed = conv%failed

        ! Print ending wall clock
        call print_date('  L-CC program ended on:')

        ! Deallocate aux L vector
        ! [TODO] might be a better way to do this
        if (allocated(cc%lh_vec)) deallocate(cc%lh_vec)

    end subroutine solve_lcc

    subroutine jacobi_iter(sys, run, cc, conv)

        ! Actual solver routine where the target vector is solved
        ! using Jacobi iterations

        ! In:
        !   sys: molecular system data
        !   run: runtime configuration data

        ! In/Out
        !   cc: CC vector and data
        !   conv: convergence data

        use const, only: p, dp, t_unit, t_vecs_unit
        use diis, only: calc_diis, write_vecs, init_vecs, residuum, clean_up_files
        use printing, only: io, print_iteration, print_date
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use cc_utils, only: open_t4_files
        use proc_pointers, only: update_ptr, calculate_energy_ptr
        use solver_types, only: conv_t
        use utils, only: get_wall_time

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        type(conv_t), intent(inout) :: conv

        real(p) :: e_cor_new, energy_diff
        real(dp) :: new_time, prev_time
        real(p) :: res
        integer :: iter

        ! Start main CC loop timing
        conv%failed = .false.
        prev_time = get_wall_time()

        call init_vecs(conv, run%diis_space)

        do iter=1, run%max_iter

            ! Update cluster components
            call update_ptr(sys, run, cc)

            ! Calculate energy and convergence parameters
            e_cor_new = calculate_energy_ptr(sys, cc)
            energy_diff = conv%en_cor - e_cor_new
            conv%conv(1) = conv%conv(2)
            conv%conv(2) = conv%conv(3)
            conv%conv(3) = energy_diff
            conv%en_cor = e_cor_new

            ! Write T vecs
            call write_vecs(conv, iter, run%diis_space)

            ! Calculate residuum
            res = residuum(conv, iter, run%diis_space)

            ! Do DIIS
            if (mod(iter, run%diis_space + 1) == 0) then
                write(io, '(a)') '      DIIS cycle'
                call calc_diis(run, conv)
            endif

            ! Print the current iteration's information
            new_time = get_wall_time()
            call print_iteration(iter, e_cor_new, energy_diff, res, prev_time, new_time)
            prev_time = new_time


            if (check_sig()) then
                write(io, '(a)') 'Signaled to end'
                exit
            endif

            ! Check for convergence
            if (check_convergence(conv%conv, run%tol)) exit
            if (isnan(e_cor_new)) then
                write(io, '(a)') 'FAILED TO CONVERGE'
                exit
            endif

            ! Abort if out of iterations
            if (iter == run%max_iter) then
                write(io, '(a)') 'FAILED TO CONVERGE IN', run%max_iter , &
                     ' ITERATIONS. KEEPING LAST T VECTOR.'
                conv%failed = .true.
            endif
        enddo

        ! Cleanup DIIS files
        call clean_up_files(conv, run%diis_space)

    end subroutine jacobi_iter

    function check_sig() result(exists)

        ! Check for external commands while the program
        ! is running. This function will read a file
        ! "cc.comm" and perform operations based on what
        ! is in the file

        use const, only: tmp_unit

        logical :: exists

        inquire(file='cc.comm', exist=exists)
        ! [TODO] right now the "cc.comm" only causes the program
        ! to stop. In the future, this should do more things (i.e.
        ! change the DIIS space, change convergence, etc.)
        if (exists) then
            open(tmp_unit, file='cc.comm', status='old')
            close(tmp_unit, status='delete')
        endif

    end function check_sig

    function check_convergence(conv, tol) result(res)

        ! Check whether the iterative procedure has converged

        ! In:
        !   conv: convergence data
        !   tol: convergence tolerance

        ! Out:
        !   res: logical value indicating convergence

        use const, only: p

        logical :: res
        real(p), intent(in) :: conv(:)
        real(p), intent(in) :: tol

        if (all(dabs(conv) < tol)) then
            res = .true.
        else
            res = .false.
        endif

    end function check_convergence

end module solver