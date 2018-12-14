module solver

    implicit none

contains

    subroutine solve_cc(sys, run, cc)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use const, only: dp, t_unit, t_vecs_unit, t_unit
        use diis, only: calc_diis, write_t_vecs, init_t_vecs
        use printing, only: io, print_header, print_date, print_iter_head, print_iteration, abort_cc, print_summary
        use system, only: sys_t, run_t, cc_t
        use utils, only: residuum
        use update_cc, only: update_clusters

        implicit none

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc


        !integer :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

        real(dp) :: e_cor, e_cor_new, energy_diff, prev_time
        real(dp) :: res
        real(dp) :: conv(3) = 1

        integer :: iter

        real(dp) :: E1A,E1B,E2A,E2B,E2C,E1A1A,E1B1B,E1A1B

        ! [TODO] clean this vars
        integer :: K1A,K1B,K2A,K2B,K2C,K3A,K3B
        integer :: K3C,K3D
        integer :: K1,K2,K3,K4,i0

        ! Indices
        integer :: i,j,k,l,m,n
        integer :: a,b,c,d,e,f

        character(len=200) :: test_env
        integer :: test_env_i

        integer :: nsocc, nsorb


        call get_t_sizes(sys, cc)
        call init_t_vecs(cc%t_size)

        call open_t4_files(sys)

        ! Initialize T vector
        if (.not. allocated(cc%t_vec)) allocate(cc%t_vec(cc%t_size))
        cc%t_vec=0.0_dp

        write(io, '(a)') ''
        call print_date('  Starting CC(P) calculation on')
        write(io, '(a)') ''

        ! Output formatting
        call print_iter_head()

        ! Start main CC loop
        call cpu_time(prev_time)

        e_cor = 0.0_dp
        conv = 0.0_dp
        do iter=1, run%max_iter

            call update_clusters(sys, run, cc)

            e_cor_new = calculate_energy(sys, cc)
            energy_diff = e_cor - e_cor_new

            ! Convergence information. Currently using a moving average. This can change
            conv(1) = conv(2)
            conv(2) = conv(3)
            conv(3) = energy_diff

            e_cor = e_cor_new

            ! Write T vector on a file
            rewind(t_unit)
            write(t_unit) cc%t_vec

            ! Write T vecs
            ! [TODO] update diis.f90
            call write_t_vecs(iter, run%diis_space, cc)

            ! Calculate residuum
            ! [TODO] update utils.f90
            res = residuum(iter, run%diis_space, cc%t_size)

            ! Do DIIS
            if (mod(iter, run%diis_space + 1) == 0) then
                write(io, '(a)') '      DIIS cycle'
                call calc_diis(run%diis_space, cc)
            endif

            call print_iteration(iter, e_cor_new, energy_diff, res, prev_time)
            call cpu_time(prev_time)

            cc%en_cor = e_cor

            if (check_sig()) then
                write(io, '(a)') 'Signaled to end'
                exit
            endif

            ! Check for convergence
            if (check_convergence(conv, run%tol)) exit

            ! Abort if out of iterations
            if (iter == run%max_iter) write(io, '(a)') 'FAILED TO CONVERGE'
        enddo

        close(t_vecs_unit, status='delete')

        write(io, '(a)') ''
        call print_date('  ccq finished on:')

        call print_summary(sys, run, cc)

        if (allocated(cc%t_vec)) deallocate(cc%t_vec)

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

        inquire(file='end_cc', exist=exists)
        if (exists) then
            open(end_unit, file='end_cc', status='old')
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

    subroutine open_t4_files(sys)

        use const, only: dp, ta, tb, tc, td, te
        use system, only: sys_t

        type(sys_t) :: sys
        real(dp), allocatable :: t(:)
        integer :: k1, k2, k3, k4

        K1 = sys%occ_a-sys%froz
        K3 = sys%orbs-sys%occ_a
        K2 = sys%occ_b-sys%froz
        K4 = sys%orbs-sys%occ_b


        open(ta, file='t4a.bin', status='unknown', form='unformatted')
        allocate(t(k1**4*k3**4))
        t = 0.0_dp
        write(ta) t
        deallocate(t)

        open(tb, file='t4b.bin', status='unknown', form='unformatted')
        allocate(t(k1**3*k2*k3**3*k4))
        t = 0.0_dp
        write(tb) t
        deallocate(t)

        open(tc, file='t4c.bin', status='unknown', form='unformatted')
        allocate(t(k1**2*k2**2*k3**2*k4**2))
        t = 0.0_dp
        write(tc) t
        deallocate(t)

        open(td, file='t4d.bin', status='unknown', form='unformatted')
        allocate(t(k2**3*k1*k4**3*k3))
        t = 0.0_dp
        write(td) t
        deallocate(t)

        open(te, file='t4e.bin', status='unknown', form='unformatted')
        allocate(t(k2**4*k4**4))
        t = 0.0_dp
        write(te) t
        deallocate(t)

    end subroutine open_t4_files

end module solver
