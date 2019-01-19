module cc_utils

    implicit none

contains

    function residuum(iter, diis_space, vecs_unit, vec_size)

        use const, only: p

        integer, intent(in) :: iter
        integer, intent(in) :: diis_space
        integer, intent(in) :: vecs_unit
        integer, intent(in) :: vec_size

        real(p) :: residuum
        real(p), allocatable :: vec_aux(:,:)
        real(p) :: ddot
        integer :: i, indx_rec


        allocate(vec_aux(vec_size, 2))

        do i=0, 1

            indx_rec = mod(iter - i, diis_space + 1)
            if (indx_rec == 0) indx_rec = diis_space + 1

            if (iter - i == 0) then
                vec_aux(:,i+1) = 0
            else
                read(vecs_unit, rec=indx_rec) vec_aux(:,i+1)
            endif

        enddo

        vec_aux(:,1) = vec_aux(:,2) - vec_aux(:,1)
        residuum = ddot(vec_size, vec_aux(:,1), 1, vec_aux(:,1), 1)

        deallocate(vec_aux)

        residuum = dsqrt(residuum)

    end function residuum

    subroutine antisymmetrize_p_space(froz, occ_a, occ_b, orbs, p_space)

        integer, intent(in) :: froz, occ_a, occ_b, orbs
        integer, intent(inout) :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

        integer :: a, b, c, i, j, k

        do i=froz+1, occ_a-2
            do j=i+1, occ_a-1
                do k=j+1, occ_a
                    do a=occ_a+1, orbs-2
                        do b=a+1, orbs-1
                            do c=b+1, orbs
                                print '(6i3)', c,b,a,k,j,i
                                p_space(c,b,a,k,i,j) = p_space(c,b,a,k,j,i)
                                p_space(c,b,a,i,j,k) = p_space(c,b,a,k,j,i)
                                p_space(c,b,a,i,k,j) = p_space(c,b,a,k,j,i)
                                p_space(c,b,a,j,k,i) = p_space(c,b,a,k,j,i)
                                p_space(c,b,a,j,i,k) = p_space(c,b,a,k,j,i)
                                p_space(c,a,b,k,j,i) = p_space(c,b,a,k,j,i)
                                p_space(c,a,b,k,i,j) = p_space(c,b,a,k,j,i)
                                p_space(c,a,b,i,j,k) = p_space(c,b,a,k,j,i)
                                p_space(c,a,b,i,k,j) = p_space(c,b,a,k,j,i)
                                p_space(c,a,b,j,k,i) = p_space(c,b,a,k,j,i)
                                p_space(c,a,b,j,i,k) = p_space(c,b,a,k,j,i)
                                p_space(a,b,c,k,j,i) = p_space(c,b,a,k,j,i)
                                p_space(a,b,c,k,i,j) = p_space(c,b,a,k,j,i)
                                p_space(a,b,c,i,j,k) = p_space(c,b,a,k,j,i)
                                p_space(a,b,c,i,k,j) = p_space(c,b,a,k,j,i)
                                p_space(a,b,c,j,k,i) = p_space(c,b,a,k,j,i)
                                p_space(a,b,c,j,i,k) = p_space(c,b,a,k,j,i)
                                p_space(a,c,b,k,j,i) = p_space(c,b,a,k,j,i)
                                p_space(a,c,b,k,i,j) = p_space(c,b,a,k,j,i)
                                p_space(a,c,b,i,j,k) = p_space(c,b,a,k,j,i)
                                p_space(a,c,b,i,k,j) = p_space(c,b,a,k,j,i)
                                p_space(a,c,b,j,k,i) = p_space(c,b,a,k,j,i)
                                p_space(a,c,b,j,i,k) = p_space(c,b,a,k,j,i)
                                p_space(b,c,a,k,j,i) = p_space(c,b,a,k,j,i)
                                p_space(b,c,a,k,i,j) = p_space(c,b,a,k,j,i)
                                p_space(b,c,a,i,j,k) = p_space(c,b,a,k,j,i)
                                p_space(b,c,a,i,k,j) = p_space(c,b,a,k,j,i)
                                p_space(b,c,a,j,k,i) = p_space(c,b,a,k,j,i)
                                p_space(b,c,a,j,i,k) = p_space(c,b,a,k,j,i)
                                p_space(b,a,c,k,j,i) = p_space(c,b,a,k,j,i)
                                p_space(b,a,c,k,i,j) = p_space(c,b,a,k,j,i)
                                p_space(b,a,c,i,j,k) = p_space(c,b,a,k,j,i)
                                p_space(b,a,c,i,k,j) = p_space(c,b,a,k,j,i)
                                p_space(b,a,c,j,k,i) = p_space(c,b,a,k,j,i)
                                p_space(b,a,c,j,i,k) = p_space(c,b,a,k,j,i)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine antisymmetrize_p_space

    subroutine antisym_t(sys, cc, lvl_q)
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        logical, intent(in) :: lvl_q

        call t3a_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(6)))
        call t3b_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(7)))
        call t3c_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(8)))
        call t3d_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(9)))

        !if (lvl_q) then
        !    call t4a_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
        !    call t4b_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
        !    call t4c_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
        !    call t4d_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
        !    call t4e_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
        !endif

    end subroutine antisym_t

    subroutine open_t4_files(sys, run)

        use const, only: dp, ta, tb, tc, td, te
        use system, only: sys_t, run_t

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        real(dp), allocatable :: t(:)
        integer :: k1, k2, k3, k4

        K1 = sys%occ_a-sys%froz
        K3 = sys%orbs-sys%occ_a
        K2 = sys%occ_b-sys%froz
        K4 = sys%orbs-sys%occ_b


        open(ta, file=trim(run%bin_file)//'_4a.bin', status='unknown', form='unformatted')
        if (.not. run%restart) then
            allocate(t(k1**4*k3**4))
            t = 0.0_dp
            write(ta) t
            deallocate(t)
        endif

        open(tb, file=trim(run%bin_file)//'_4b.bin', status='unknown', form='unformatted')
        if (.not. run%restart) then
            allocate(t(k1**3*k2*k3**3*k4))
            t = 0.0_dp
            write(tb) t
            deallocate(t)
        endif

        open(tc, file=trim(run%bin_file)//'_4c.bin', status='unknown', form='unformatted')
        if (.not. run%restart) then
            allocate(t(k1**2*k2**2*k3**2*k4**2))
            t = 0.0_dp
            write(tc) t
            deallocate(t)
        endif

        open(td, file=trim(run%bin_file)//'_4d.bin', status='unknown', form='unformatted')
        if (.not. run%restart) then
            allocate(t(k2**3*k1*k4**3*k3))
            t = 0.0_dp
            write(td) t
            deallocate(t)
        endif

        open(te, file=trim(run%bin_file)//'_4e.bin', status='unknown', form='unformatted')
        if (.not. run%restart) then
            allocate(t(k2**4*k4**4))
            t = 0.0_dp
            write(te) t
            deallocate(t)
        endif

    end subroutine open_t4_files

    subroutine close_t4_files(sys, keep, cc_failed)

        use const, only: dp, ta, tb, tc, td, te
        use system, only: sys_t, run_t

        type(sys_t), intent(in) :: sys
        logical, intent(in) :: keep, cc_failed


        if (keep .or. cc_failed) then
            close(ta)
            close(tb)
            close(tc)
            close(td)
            close(te)
        else
            close(ta, status="delete")
            close(tb, status="delete")
            close(tc, status="delete")
            close(td, status="delete")
            close(te, status="delete")
        endif

    end subroutine close_t4_files

end module cc_utils
