module solver

    implicit none

contains

    subroutine solve_cc(sys, run, cc, failed)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use const, only: dp, t_unit, t_vecs_unit
        use diis, only: calc_diis, write_t_vecs
        use energy, only: calculate_energy
        use printing, only: io, print_iter_head, print_iteration, abort_cc, print_date
        use system, only: sys_t, run_t, cc_t
        use cc_utils, only: antisym_t, residuum, open_t4_files
        use update_cc, only: update_clusters_t4

        implicit none

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        logical, intent(inout) :: failed
        real(dp) :: e_cor_new, energy_diff, prev_time
        real(dp) :: res
        real(dp) :: conv(3) = 1.0_dp
        integer :: iter

        ! Select CC update
        !update_clusters_ptr => update_clusters_t4

        ! Coupled cluster program title
        write(io, '(a)') 'Coupled cluster'
        write(io, '(a)') '---------------'

        if (run%restart) then
            e_cor_new = calculate_energy(sys, cc)
            write(io, '(2x,a27,2x,f16.10/)') 'Restart correlation energy', e_cor_new
        endif

        ! Write to ouput
        call print_date('  CC program started on:')
        call print_iter_head()

        ! Start main CC loop timing
        call cpu_time(prev_time)

        do iter=1, run%max_iter

            ! Update cluster components
            call update_clusters_t4(sys, run, cc)

            ! Calculate energy and convergence parameters
            e_cor_new = calculate_energy(sys, cc)
            energy_diff = cc%en_cor - e_cor_new
            conv(1) = conv(2)
            conv(2) = conv(3)
            conv(3) = energy_diff
            cc%en_cor = e_cor_new

            ! Write T vector on a file
            rewind(t_unit)
            write(t_unit) cc%t_vec

            ! Write T vecs
            call write_t_vecs(iter, run%diis_space, cc)

            ! Calculate residuum
            res = residuum(iter, run%diis_space, cc%t_size)

            ! Do DIIS
            if (mod(iter, run%diis_space + 1) == 0) then
                write(io, '(a)') '      DIIS cycle'
                call calc_diis(run%diis_space, run, cc)
            endif

            ! Print iteration information
            call print_iteration(iter, e_cor_new, energy_diff, res, prev_time)
            call cpu_time(prev_time)


            if (check_sig()) then
                write(io, '(a)') 'Signaled to end'
                exit
            endif

            ! Check for convergence
            if (check_convergence(conv, run%tol)) exit
            if (isnan(e_cor_new)) then
                write(io, '(a)') 'FAILED TO CONVERGE'
                exit
            endif

            ! Abort if out of iterations
            if (iter == run%max_iter) then
                write(io, '(a)') 'FAILED TO CONVERGE. KEEPING LAST T VECTOR.'
                failed = .true.
            endif
        enddo


        if (allocated(cc%t_vec)) deallocate(cc%t_vec)
        if (allocated(cc%acc%t2_mc)) deallocate(cc%acc%t2_mc)

        call print_date('  CC program ended on:')

    end subroutine solve_cc

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
