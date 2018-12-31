module solver

    implicit none

contains

    subroutine solve_cc(sys, run, cc)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use const, only: dp, t_unit, t_vecs_unit, t_unit
        use cluster_analysis, only: cluster_analysis_driver_opt
        use diis, only: calc_diis, write_t_vecs, init_t_vecs
        use printing, only: io, print_header, print_date, print_iter_head, print_iteration, abort_cc, print_summary
        use system, only: sys_t, run_t, cc_t
        use utils, only: antisym_t, residuum, open_t4_files
        use update_cc, only: update_clusters_t4

        implicit none

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        logical :: failed = .false.
        real(dp) :: e_cor_new, energy_diff, prev_time
        real(dp) :: res
        real(dp) :: conv(3) = 1.0_dp
        integer :: iter

        ! Select CC update
        !update_clusters_ptr => update_clusters_t4

        ! Initialize vectors
        open(t_unit,file=trim(run%bin_file),form='unformatted')
        call get_t_sizes(sys, cc)
        call init_t_vecs(cc%t_size)
        !if (run%lvl_q) call open_t4_files(sys, run%restart)
        call init_t_vec(cc, run%restart)

        if (run%ext_cor) then
            call cluster_analysis_driver_opt(sys, run, cc)
            !call antisym_t(sys, cc, run%lvl_q)
            e_cor_new = calculate_energy(sys, cc)
            write(io, '(2x,a27,2x,f16.10/)') 'External correlation energy', e_cor_new
        else
            if (run%lvl_q) call open_t4_files(sys, run)
        endif

        if (run%restart) then
            e_cor_new = calculate_energy(sys, cc)
            write(io, '(2x,a27,2x,f16.10/)') 'Restart correlation energy', e_cor_new
        endif

        ! Write to ouput
        call print_date('  Starting ccq calculation on')
        call print_iter_head()

        ! Start main CC loop timing
        call cpu_time(prev_time)

        cc%en_cor = 0.0_dp
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

        close(t_vecs_unit, status='delete')
        if (run%keep_bin .or. failed) then
            close(t_unit)
        else
            close(t_unit, status='delete')
        endif

        call print_date('  ccq finished on:')
        call print_summary(sys, run, cc)

        if (allocated(cc%t_vec)) deallocate(cc%t_vec)
        if (allocated(cc%acc%t2_mc)) deallocate(cc%acc%t2_mc)

    end subroutine solve_cc

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

    function check_sig() result(exists)

        use const, only: end_unit

        logical :: exists

        inquire(file='cc.comm', exist=exists)
        if (exists) then
            open(end_unit, file='cc.comm', status='old')
            close(end_unit, status='delete')
        endif

    end function check_sig

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

    subroutine init_t_vec(cc, restart)

        use const, only: dp, t_unit
        use system, only: cc_t

        type(cc_t), intent(inout) :: cc
        logical, intent(in) :: restart

        if (.not. allocated(cc%t_vec)) allocate(cc%t_vec(cc%t_size))
        if (.not. allocated(cc%acc%t2_mc)) allocate(cc%acc%t2_mc(cc%pos(6)-cc%pos(3)))
        if (restart) then
            rewind(t_unit)
            read(t_unit) cc%t_vec
            cc%acc%t2_mc = cc%t_vec(cc%pos(3):cc%pos(6)-1)
        else
            cc%t_vec=0.0_dp
            cc%acc%t2_mc = 0.0_dp
        endif

    end subroutine init_t_vec

end module solver
