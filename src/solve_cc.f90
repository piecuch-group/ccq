module solver

    use const, only: p

    implicit none


contains

    subroutine solve_cc(sys, run, cc, failed)

        use const, only: p, t_unit, t_vecs_unit
        use energy, only: calculate_sorted_energy, calculate_unsorted_energy
        use printing, only: io, print_iter_head, abort_cc, print_date
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

        ! Select procedure
        if (run%sorted_ints) then
            update_ptr => update_clusters_t3_opt
            calculate_energy_ptr => calculate_sorted_energy
        else
            update_ptr => update_clusters_t4
            calculate_energy_ptr => calculate_unsorted_energy
        endif

        if (run%restart) then
            conv%en_cor = calculate_energy_ptr(sys, cc)
            write(io, '(2x,a27,2x,f16.10/)') 'Restart correlation energy', conv%en_cor
        endif

        ! Write to ouput
        call print_date('  CC program started on:')
        call print_iter_head()


        ! Only perform DIIS with up to doubles when running
        ! externally corrected calculations
        if (run%ext_cor) then
            conv%vec_size = cc%pos(6) - 1
        else
            conv%vec_size = cc%t_size
        endif

        conv%vec_ptr => cc%t_vec
        conv%vec_unit = t_unit
        conv%vecs_unit = t_vecs_unit

        call jacobi_iter(sys, run, cc, conv)

        ! [TODO] clean interface a bit more
        cc%en_cor = conv%en_cor
        failed = conv%failed


        call print_date('  CC program ended on:')

    end subroutine solve_cc

    subroutine solve_lcc(sys, run, cc, failed)

        use const, only: l_unit, l_vecs_unit
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use energy, only: calculate_left_energy
        use proc_pointers, only: update_ptr, calculate_energy_ptr
        use hbar, only: add_twobody_hbar
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

        ! Write to ouput
        call print_date('  L-CC program started on:')
        call print_iter_head()

        conv%vec_ptr => cc%l_vec
        conv%vec_unit = l_unit
        conv%vecs_unit = l_vecs_unit

        call jacobi_iter(sys, run, cc, conv)

        ! [TODO] check that energies match
        !conv%en_cor
        failed = conv%failed

        call print_date('  L-CC program ended on:')

        if (allocated(cc%lh_vec)) deallocate(cc%lh_vec)

        !close(l_vecs_unit, status='delete')
        !close(l_unit)

    end subroutine solve_lcc

    subroutine jacobi_iter(sys, run, cc, conv)

        use const, only: p, t_unit, t_vecs_unit
        use diis, only: calc_diis, write_vecs, init_vecs
        use printing, only: io, print_iteration, abort_cc, print_date
        use cc_utils, only: residuum
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use cc_utils, only: residuum, open_t4_files
        use proc_pointers, only: update_ptr, calculate_energy_ptr
        use solver_types, only: conv_t

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        type(conv_t), intent(inout) :: conv

        real(p) :: e_cor_new, energy_diff, prev_time
        real(p) :: res
        integer :: iter

        ! Start main CC loop timing
        conv%failed = .false.
        call cpu_time(prev_time)

        call init_vecs(conv)

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

            ! Write T vector on a file
            rewind(conv%vec_unit)
            write(conv%vec_unit) conv%vec_ptr

            ! Write T vecs
            call write_vecs(conv, iter, run%diis_space)

            ! Calculate residuum
            res = residuum(iter, run%diis_space, conv%vecs_unit, conv%vec_size)

            ! Do DIIS
            if (mod(iter, run%diis_space + 1) == 0) then
                write(io, '(a)') '      DIIS cycle'
                call calc_diis(run, conv)
            endif

            ! Print iteration information
            call print_iteration(iter, e_cor_new, energy_diff, res, prev_time)
            call cpu_time(prev_time)


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
                write(io, '(a)') 'FAILED TO CONVERGE. KEEPING LAST T VECTOR.'
                conv%failed = .true.
            endif
        enddo

        close(conv%vecs_unit, status='delete')

    end subroutine jacobi_iter

    function check_sig() result(exists)

        use const, only: end_unit

        logical :: exists

        inquire(file='cc.comm', exist=exists)
        if (exists) then
            open(end_unit, file='cc.comm', status='old')
            close(end_unit, status='delete')
        endif

    end function check_sig


    function check_convergence(conv, tol) result(res)

        use const, only: dp

        real(dp), intent(in) :: conv(:)
        real(dp), intent(in) :: tol

        logical :: res

        if (all(dabs(conv) < tol)) then
            res = .true.
        else
            res = .false.
        endif

    end function check_convergence


end module solver
