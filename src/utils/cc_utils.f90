module cc_utils

    implicit none

contains

    ! [TODO] this is temporary and should be cleaned
    subroutine antisym_p_space(froz, occ_a, orbs, p_space)

        integer, intent(in) :: froz, occ_a, orbs
        integer, intent(in out) :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

        integer :: a, b, c, i, j, k

        do i=froz+1, occ_a-2
            do j=i+1, occ_a-1
                do k=j+1, occ_a
                    do a=occ_a+1, orbs-2
                        do b=a+1, orbs-1
                            do c=b+1, orbs
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

    end subroutine antisym_p_space

    !subroutine antisym_t(sys, cc, lvl_q)

    !    use system, only: sys_t
    !    use cc_types, only: cc_t

    !    type(sys_t), intent(in) :: sys
    !    type(cc_t), intent(inout) :: cc
    !    logical, intent(in) :: lvl_q

    !    call t3a_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(6)))
    !    call t3b_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(7)))
    !    call t3c_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(8)))
    !    call t3d_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,cc%t_vec(cc%pos(9)))

    !    if (lvl_q) then
    !        call t4a_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
    !        call t4b_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
    !        call t4c_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
    !        call t4d_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
    !        call t4e_antisym(sys%froz,sys%occ_a,sys%occ_b,sys%orbs)
    !    endif

    !end subroutine antisym_t

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

    subroutine close_t4_files(keep, cc_failed)

        use const, only: ta, tb, tc, td, te

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

    subroutine t3_aab_to_t3_abb(sys, t3_aab, t3_abb)

        ! Proper transfer of t3_aab to t3_abb for closed-shell systems
        use const, only: dp
        use system, only: sys_t

        ! Indices
        type(sys_t), intent(in) :: sys
        integer :: i, j, k
        integer :: a, b, c

        ! t3_aab and t3_abb amplitudes
        real(dp), allocatable, intent(in) :: t3_aab(:,:,:,:,:,:)
        real(dp), allocatable, intent(inout) :: t3_abb(:,:,:,:,:,:)

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            ! Transfer t3_aab values to t3_abb
            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do k=j+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_b+1,total
                                do c=b+1,total

                                    t3_abb(a,b,c,i,j,k) = t3_aab(b,c,a,j,k,i)

                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        end associate

    end subroutine t3_aab_to_t3_abb

    subroutine t4_aaab_to_t4_abbb(sys, t4_aaab, t4_abbb)

        ! Proper transfer of t4_aaab to t4_abbb for closed-shell systems

        use const, only: dp
        use system, only: sys_t

        ! Indices
        type(sys_t), intent(in) :: sys
        integer :: i, j, k, l
        integer :: a, b, c, d

        ! t4_aaab and t4_abbb amplitudes
        real(dp), allocatable, intent(in) :: t4_aaab(:,:,:,:,:,:,:,:)
        real(dp), allocatable, intent(inout) :: t4_abbb(:,:,:,:,:,:,:,:)

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            ! Transfer t4_aaab values to t4_abbb
            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do k=j+1,occ_b
                        do l=k+1,occ_b
                            do a=occ_a+1,total
                                do b=occ_b+1,total
                                    do c=b+1,total
                                        do d=c+1,total

                                            t4_abbb(a,b,c,d,i,j,k,l) = t4_aaab(b,c,d,a,j,k,l,i)

                                        enddo
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        end associate

    end subroutine t4_aaab_to_t4_abbb

    subroutine antisymmetrize_t2(t2a, t2c)

        use const, only: p

        real(p), intent(in out) :: t2a(:,:,:,:), t2c(:,:,:,:)

        integer :: a, b
        integer :: i, j
        integer :: ub_occ_a, ub_unocc_a
        integer :: ub_occ_b, ub_unocc_b
        integer :: lb_occ_a, lb_unocc_a
        integer :: lb_occ_b, lb_unocc_b

        ub_occ_a = ubound(t2a, 4)
        ub_occ_b = ubound(t2c, 4)
        ub_unocc_a = ubound(t2a, 2)
        ub_unocc_b = ubound(t2c, 2)

        lb_occ_a = lbound(t2a, 4)
        lb_occ_b = lbound(t2c, 4)
        lb_unocc_a = lbound(t2a, 2)
        lb_unocc_b = lbound(t2c, 2)




        do i=lb_occ_a, ub_occ_a
            do j=i+1, ub_occ_a
                do a=lb_unocc_a, ub_unocc_a
                    do b=a+1, ub_unocc_a
                        t2a(b,a,i,j)=-t2a(b,a,j,i) !(ij)
                        t2a(a,b,j,i)=-t2a(b,a,j,i) !(ab)
                        t2a(a,b,i,j)=t2a(b,a,j,i) !(ab)(ij)
                    enddo
                enddo
            enddo
        enddo

        do i=lb_occ_b, ub_occ_b
            do j=i+1, ub_occ_b
                do a=lb_unocc_b, ub_unocc_b
                    do b=a+1, ub_unocc_b
                        t2c(b,a,i,j)=-t2c(b,a,j,i) !(ij)
                        t2c(a,b,j,i)=-t2c(b,a,j,i) !(ab)
                        t2c(a,b,i,j)=t2c(b,a,j,i) !(ab)(ij)
                    enddo
                enddo
            enddo
        enddo

    end subroutine antisymmetrize_t2


    subroutine antisymmetrize(sys, c_vec)

        ! Antisymmetrize arrays
        !
        ! In:
        !   sys: system information
        ! In/Out:
        !   c_vec: CI or CC coefficient/amplitude arrays. On entry, arrays are not antisymmetrized
        !          on output arrays are antisymmetrized

        use ext_cor_types, only: vec3_t
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(vec3_t), intent(inout) :: c_vec

        integer :: a, b, c
        integer :: i, j, k

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            do i=froz+1, occ_a
                do j=i+1, occ_a
                    do a=occ_a+1, total
                        do b=a+1, total
                            c_vec%o2_aa(a,b,j,i)=-c_vec%o2_aa(a,b,i,j) !(ij)
                            c_vec%o2_aa(b,a,i,j)=-c_vec%o2_aa(a,b,i,j) !(ab)
                            c_vec%o2_aa(b,a,j,i)=c_vec%o2_aa(a,b,i,j) !(ab)(ij)
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1, occ_b
                do j=i+1, occ_b
                    do a=occ_b+1, total
                        do b=a+1, total
                            c_vec%o2_bb(a,b,j,i)=-c_vec%o2_bb(a,b,i,j) !(ij)
                            c_vec%o2_bb(b,a,i,j)=-c_vec%o2_bb(a,b,i,j) !(ab)
                            c_vec%o2_bb(b,a,j,i)=c_vec%o2_bb(a,b,i,j) !(ab)(ij)
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1, occ_a
                do j=i+1, occ_a
                    do k=j+1, occ_a
                        do a=occ_a+1, total
                            do b=a+1, total
                                do c=b+1, total
                                    c_vec%o3_aaa(a,b,c,j,i,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ij)
                                    c_vec%o3_aaa(a,b,c,k,j,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ik)
                                    c_vec%o3_aaa(a,b,c,i,k,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(jk)
                                    c_vec%o3_aaa(a,b,c,j,k,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(ijk)
                                    c_vec%o3_aaa(a,b,c,k,i,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(ikj)
                                    c_vec%o3_aaa(b,a,c,i,j,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ab)
                                    c_vec%o3_aaa(b,a,c,j,i,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ij)
                                    c_vec%o3_aaa(b,a,c,k,j,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ik)
                                    c_vec%o3_aaa(b,a,c,i,k,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(jk)
                                    c_vec%o3_aaa(b,a,c,j,k,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ijk)
                                    c_vec%o3_aaa(b,a,c,k,i,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ikj)
                                    c_vec%o3_aaa(c,b,a,i,j,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ac)
                                    c_vec%o3_aaa(c,b,a,j,i,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ij)
                                    c_vec%o3_aaa(c,b,a,k,j,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ik)
                                    c_vec%o3_aaa(c,b,a,i,k,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(jk)
                                    c_vec%o3_aaa(c,b,a,j,k,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ijk)
                                    c_vec%o3_aaa(c,b,a,k,i,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ikj)
                                    c_vec%o3_aaa(a,c,b,i,j,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(bc)
                                    c_vec%o3_aaa(a,c,b,j,i,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ij)
                                    c_vec%o3_aaa(a,c,b,k,j,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ik)
                                    c_vec%o3_aaa(a,c,b,i,k,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(jk)
                                    c_vec%o3_aaa(a,c,b,j,k,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ijk)
                                    c_vec%o3_aaa(a,c,b,k,i,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ikj)
                                    c_vec%o3_aaa(b,c,a,i,j,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(abc)
                                    c_vec%o3_aaa(b,c,a,j,i,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ij)
                                    c_vec%o3_aaa(b,c,a,k,j,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ik)
                                    c_vec%o3_aaa(b,c,a,i,k,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(jk)
                                    c_vec%o3_aaa(b,c,a,j,k,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ijk)
                                    c_vec%o3_aaa(b,c,a,k,i,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ikj)
                                    c_vec%o3_aaa(c,a,b,i,j,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(acb)
                                    c_vec%o3_aaa(c,a,b,j,i,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ij)
                                    c_vec%o3_aaa(c,a,b,k,j,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ik)
                                    c_vec%o3_aaa(c,a,b,i,k,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(jk)
                                    c_vec%o3_aaa(c,a,b,j,k,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ijk)
                                    c_vec%o3_aaa(c,a,b,k,i,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ikj)
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1, occ_a
                do j=i+1, occ_a
                    do k=froz+1, occ_b
                        do a=occ_a+1, total
                            do b=a+1, total
                                do c=occ_b+1, total
                                    c_vec%o3_aab(a,b,c,j,i,k)=-c_vec%o3_aab(a,b,c,i,j,k) !(ij)
                                    c_vec%o3_aab(b,a,c,i,j,k)=-c_vec%o3_aab(a,b,c,i,j,k) !(ab)
                                    c_vec%o3_aab(b,a,c,j,i,k)=c_vec%o3_aab(a,b,c,i,j,k) !(ab)(ij)
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1, occ_a
                do j=froz+1, occ_b
                    do k=j+1, occ_b
                        do a=occ_a+1, total
                            do b=occ_b+1, total
                                do c=b+1, total
                                    c_vec%o3_abb(a,b,c,i,k,j)=-c_vec%o3_abb(a,b,c,i,j,k) !(jk)
                                    c_vec%o3_abb(a,c,b,i,j,k)=-c_vec%o3_abb(a,b,c,i,j,k) !(bc)
                                    c_vec%o3_abb(a,c,b,i,k,j)=c_vec%o3_abb(a,b,c,i,j,k) !(bc)(jk)
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1, occ_b
                do j=i+1, occ_b
                    do k=j+1, occ_b
                        do a=occ_b+1, total
                            do b=a+1, total
                                do c=b+1, total
                                    c_vec%o3_bbb(a,b,c,j,i,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ij)
                                    c_vec%o3_bbb(a,b,c,k,j,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ik)
                                    c_vec%o3_bbb(a,b,c,i,k,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(jk)
                                    c_vec%o3_bbb(a,b,c,j,k,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(ijk)
                                    c_vec%o3_bbb(a,b,c,k,i,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(ikj)
                                    c_vec%o3_bbb(b,a,c,i,j,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ab)
                                    c_vec%o3_bbb(b,a,c,j,i,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ij)
                                    c_vec%o3_bbb(b,a,c,k,j,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ik)
                                    c_vec%o3_bbb(b,a,c,i,k,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(jk)
                                    c_vec%o3_bbb(b,a,c,j,k,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ijk)
                                    c_vec%o3_bbb(b,a,c,k,i,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ikj)
                                    c_vec%o3_bbb(c,b,a,i,j,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ac)
                                    c_vec%o3_bbb(c,b,a,j,i,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ij)
                                    c_vec%o3_bbb(c,b,a,k,j,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ik)
                                    c_vec%o3_bbb(c,b,a,i,k,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(jk)
                                    c_vec%o3_bbb(c,b,a,j,k,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ijk)
                                    c_vec%o3_bbb(c,b,a,k,i,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ikj)
                                    c_vec%o3_bbb(a,c,b,i,j,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(bc)
                                    c_vec%o3_bbb(a,c,b,j,i,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ij)
                                    c_vec%o3_bbb(a,c,b,k,j,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ik)
                                    c_vec%o3_bbb(a,c,b,i,k,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(jk)
                                    c_vec%o3_bbb(a,c,b,j,k,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ijk)
                                    c_vec%o3_bbb(a,c,b,k,i,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ikj)
                                    c_vec%o3_bbb(b,c,a,i,j,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(abc)
                                    c_vec%o3_bbb(b,c,a,j,i,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ij)
                                    c_vec%o3_bbb(b,c,a,k,j,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ik)
                                    c_vec%o3_bbb(b,c,a,i,k,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(jk)
                                    c_vec%o3_bbb(b,c,a,j,k,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ijk)
                                    c_vec%o3_bbb(b,c,a,k,i,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ikj)
                                    c_vec%o3_bbb(c,a,b,i,j,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(acb)
                                    c_vec%o3_bbb(c,a,b,j,i,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ij)
                                    c_vec%o3_bbb(c,a,b,k,j,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ik)
                                    c_vec%o3_bbb(c,a,b,i,k,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(jk)
                                    c_vec%o3_bbb(c,a,b,j,k,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ijk)
                                    c_vec%o3_bbb(c,a,b,k,i,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ikj)
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

        end associate

    end subroutine antisymmetrize

    subroutine get_t_sizes(sys, cc, calc_type)

        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        character(len=*), intent(in) :: calc_type

        integer :: occs_a, occs_b
        integer :: unoccs_a, unoccs_b

        integer :: t_size

        occs_a = sys%occ_a-sys%froz
        unoccs_a = sys%orbs-sys%occ_a

        occs_b = sys%occ_b-sys%froz
        unoccs_b = sys%orbs-sys%occ_b

        ! Zero positions
        cc%pos = 0
        t_size = 0

        ! T1A
        cc%pos(1) = t_size+1

        ! T1B
        t_size = t_size + occs_a * unoccs_a
        cc%pos(2) = t_size+1

        ! T2A
        t_size = t_size + occs_b * unoccs_b
        cc%pos(3) = t_size+1

        ! T2B
        t_size = t_size + (occs_a**2) * (unoccs_a**2)
        cc%pos(4) = t_size+1

        ! T2C
        t_size = t_size + occs_a * occs_b * unoccs_a * unoccs_b
        cc%pos(5) = t_size+1

        ! T3A
        t_size = t_size + (occs_b**2) * (unoccs_b**2)
        cc%pos(6) = t_size+1

        ! T3B
        t_size = t_size + (occs_a**3) * (unoccs_a**3)
        cc%pos(7) = t_size+1

        ! T3C
        t_size = t_size + (occs_a**2) * occs_b * (unoccs_a**2) * unoccs_b
        cc%pos(8) = t_size+1

        ! T3D
        t_size = t_size + (occs_b**2) * occs_a * (unoccs_b**2) * unoccs_a
        cc%pos(9) = t_size+1

        ! T max
        t_size = t_size + (occs_b**3) * (unoccs_b**3)
        cc%pos(10) = t_size+1

        ! [TODO] make this work. Currently, everything is being passed to the
        ! update t4 routine which requires passing triples even though they
        ! might not be needed
        select case (trim(calc_type))
        case ('CCSD')
            !cc%t_size = cc%pos(6) - 1
            cc%t_size = cc%pos(10) - 1

        case ('CCSDT', 'CCSDTQ')
            cc%t_size = cc%pos(10) - 1

        case default
            cc%t_size = cc%pos(10) - 1

        end select


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

    subroutine reorder_stripe(rank, mat_shape, mat_size, perm_str, A, B)

        use const, only: p

        integer, intent(in) :: rank
        integer, intent(in) :: mat_shape(:)
        integer, intent(in) :: mat_size
        character(len=*), intent(in) :: perm_str

        real(p), intent(in) :: A(mat_size)
        real(p), intent(inout) :: B(mat_size)

        integer :: perm(rank)

        integer :: ident(rank)

        integer :: stride_a(rank)
        integer :: stride_a_inner
        integer :: size_outer, size_inner
        integer :: offset_a, offset_b

        integer :: i, j, j_tmp
        integer :: current_index

        call gen_perm_array(perm_str, perm)

        do i=1, rank
            ident(i) = i
        enddo

        if (all(ident == perm)) then
            B = A
            return
        endif

        stride_a(1) = 1
        do i=2, rank
            stride_a(i) = stride_a(i-1) * mat_shape(i-1)
        enddo
        !print *, 'size a',  size(a)
        !print *, 'stide_a', stride_a
        !print *, 'mat_shape', mat_shape

        size_outer = 1
        do i=1, rank
            if (i /= perm(1)) size_outer = size_outer * mat_shape(i)
        enddo
        !print *, 'size_outer', size_outer

        size_inner = mat_shape(perm(1))

        !$omp parallel default(none), &
        !$omp shared(size_outer, rank, mat_shape, perm, stride_a, size_inner, a, b), &
        !$omp private(i, j, j_tmp, offset_a, offset_b, current_index, stride_a_inner)

        !$omp do
        do j=0, size_outer-1
            offset_a = 0

            j_tmp = j

            do i=2, rank

                current_index = modulo(j_tmp, mat_shape(perm(i)))
                j_tmp = j_tmp / mat_shape(perm(i))
                offset_a = offset_a + (current_index * stride_a(perm(i)))

            enddo

            offset_b = j * size_inner
            stride_a_inner = stride_a(perm(1))

            do i=0, size_inner-1
                b(offset_b + i + 1) = a(offset_a + (i * stride_a_inner) + 1)
                !b(offset_b + i + 1) = 3.0d0
            enddo


        enddo
        !$omp end do

        !$omp end parallel


    end subroutine reorder_stripe

    subroutine sum_stripe(rank, mat_shape, mat_size, perm_str, beta, A, B)

        use const, only: p, sp

        integer, intent(in) :: rank
        integer, intent(in) :: mat_shape(:)
        integer, intent(in) :: mat_size
        character(len=*), intent(in) :: perm_str
        real(sp), intent(in) :: beta
        real(p), intent(in out) :: A(mat_size)
        real(p), intent(in) :: B(mat_size)

        integer :: perm(rank)

        integer :: ident(rank)

        integer :: stride_a(rank)
        integer :: stride_a_inner
        integer :: size_outer, size_inner
        integer :: offset_a, offset_b

        integer :: i, j, j_tmp
        integer :: current_index

        call gen_perm_array(perm_str, perm)

        do i=1, rank
            ident(i) = i
        enddo

        if (all(ident == perm)) then
            A = A + beta * B
            return
        endif

        stride_a(1) = 1
        do i=2, rank
            stride_a(i) = stride_a(i-1) * mat_shape(i-1)
        enddo
        !print *, 'size a',  size(a)
        !print *, 'stide_a', stride_a
        !print *, 'mat_shape', mat_shape

        size_outer = 1
        do i=1, rank
            if (i /= perm(1)) size_outer = size_outer * mat_shape(i)
        enddo
        !print *, 'size_outer', size_outer


        size_inner = mat_shape(perm(1))

        !$omp parallel default(none), &
        !$omp shared(size_outer, rank, mat_shape, perm, stride_a, size_inner, a, b, beta), &
        !$omp private(i, j, j_tmp, offset_a, offset_b, current_index, stride_a_inner)

        !$omp do
        do j=0, size_outer-1
            offset_a = 0

            j_tmp = j

            do i=2, rank

                current_index = modulo(j_tmp, mat_shape(perm(i)))
                j_tmp = j_tmp / mat_shape(perm(i))
                offset_a = offset_a + (current_index * stride_a(perm(i)))

            enddo

            offset_b = j * size_inner
            stride_a_inner = stride_a(perm(1))

            do i=0, size_inner-1
                a(offset_a + (i * stride_a_inner) + 1) = a(offset_a + (i * stride_a_inner) + 1) &
                    + beta * b(offset_b + i + 1)
            enddo


        enddo
        !$omp end do

        !$omp end parallel


    end subroutine sum_stripe

    subroutine gen_perm_array(perm_str, perm_array)

        character(len=*), intent(in) :: perm_str
        integer, intent(in out) :: perm_array(:)

        integer :: perm_rank
        integer :: i, idx

        perm_rank = len_trim(perm_str)

        do i=1, perm_rank
            idx = iachar(perm_str(i:i)) - 48
            perm_array(i) = idx
        enddo

    end subroutine gen_perm_array

    !subroutine t3a_antisym(n0,n1,n2,n3,t3a)

    !    use const, only: p

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), intent(inout) :: t3a(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1)
    !    integer :: a,b,c,i,j,k

    !    do i=n0+1,n1-2
    !        do j=i+1,n1-1
    !            do k=j+1,n1
    !                do a=n1+1,n3-2
    !                    do b=a+1,n3-1
    !                        do c=b+1,n3
    !                            t3a(c,b,a,k,i,j)=-t3a(c,b,a,k,j,i)
    !                            t3a(c,b,a,i,j,k)=-t3a(c,b,a,k,j,i)
    !                            t3a(c,b,a,i,k,j)= t3a(c,b,a,k,j,i)
    !                            t3a(c,b,a,j,k,i)=-t3a(c,b,a,k,j,i)
    !                            t3a(c,b,a,j,i,k)= t3a(c,b,a,k,j,i)
    !                            t3a(c,a,b,k,j,i)=-t3a(c,b,a,k,j,i)
    !                            t3a(c,a,b,k,i,j)= t3a(c,b,a,k,j,i)
    !                            t3a(c,a,b,i,j,k)= t3a(c,b,a,k,j,i)
    !                            t3a(c,a,b,i,k,j)=-t3a(c,b,a,k,j,i)
    !                            t3a(c,a,b,j,k,i)= t3a(c,b,a,k,j,i)
    !                            t3a(c,a,b,j,i,k)=-t3a(c,b,a,k,j,i)
    !                            t3a(a,b,c,k,j,i)=-t3a(c,b,a,k,j,i)
    !                            t3a(a,b,c,k,i,j)= t3a(c,b,a,k,j,i)
    !                            t3a(a,b,c,i,j,k)= t3a(c,b,a,k,j,i)
    !                            t3a(a,b,c,i,k,j)=-t3a(c,b,a,k,j,i)
    !                            t3a(a,b,c,j,k,i)= t3a(c,b,a,k,j,i)
    !                            t3a(a,b,c,j,i,k)=-t3a(c,b,a,k,j,i)
    !                            t3a(a,c,b,k,j,i)= t3a(c,b,a,k,j,i)
    !                            t3a(a,c,b,k,i,j)=-t3a(c,b,a,k,j,i)
    !                            t3a(a,c,b,i,j,k)=-t3a(c,b,a,k,j,i)
    !                            t3a(a,c,b,i,k,j)= t3a(c,b,a,k,j,i)
    !                            t3a(a,c,b,j,k,i)=-t3a(c,b,a,k,j,i)
    !                            t3a(a,c,b,j,i,k)= t3a(c,b,a,k,j,i)
    !                            t3a(b,c,a,k,j,i)=-t3a(c,b,a,k,j,i)
    !                            t3a(b,c,a,k,i,j)= t3a(c,b,a,k,j,i)
    !                            t3a(b,c,a,i,j,k)= t3a(c,b,a,k,j,i)
    !                            t3a(b,c,a,i,k,j)=-t3a(c,b,a,k,j,i)
    !                            t3a(b,c,a,j,k,i)= t3a(c,b,a,k,j,i)
    !                            t3a(b,c,a,j,i,k)=-t3a(c,b,a,k,j,i)
    !                            t3a(b,a,c,k,j,i)= t3a(c,b,a,k,j,i)
    !                            t3a(b,a,c,k,i,j)=-t3a(c,b,a,k,j,i)
    !                            t3a(b,a,c,i,j,k)=-t3a(c,b,a,k,j,i)
    !                            t3a(b,a,c,i,k,j)= t3a(c,b,a,k,j,i)
    !                            t3a(b,a,c,j,k,i)=-t3a(c,b,a,k,j,i)
    !                            t3a(b,a,c,j,i,k)= t3a(c,b,a,k,j,i)
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !end subroutine t3a_antisym

    !subroutine t3b_antisym(n0,n1,n2,n3,t3b)

    !    use const, only: p

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), intent(inout) :: t3b(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1)
    !    integer :: a,b,c,i,j,k

    !    do i=n0+1,n1-1
    !        do j=i+1,n1
    !            do k=n0+1,n2
    !                do a=n1+1,n3-1
    !                    do b=a+1,n3
    !                        do c=n2+1,n3
    !                            t3b(c,b,a,k,i,j)=-t3b(c,b,a,k,j,i)
    !                            t3b(c,a,b,k,j,i)=-t3b(c,b,a,k,j,i)
    !                            t3b(c,a,b,k,i,j)= t3b(c,b,a,k,j,i)
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !end subroutine t3b_antisym

    !subroutine t3c_antisym(n0,n1,n2,n3,t3c)

    !    use const, only: p

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), intent(inout) :: t3c(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1)
    !    integer :: a,b,c,i,j,k

    !    do i=n0+1,n1
    !        do j=n0+1,n2-1
    !            do k=j+1,n2
    !                do a=n1+1,n3
    !                    do b=n2+1,n3-1
    !                        do c=b+1,n3
    !                            t3c(c,b,a,j,k,i)=-t3c(c,b,a,k,j,i)
    !                            t3c(b,c,a,k,j,i)=-t3c(c,b,a,k,j,i)
    !                            t3c(b,c,a,j,k,i)= t3c(c,b,a,k,j,i)
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !end subroutine t3c_antisym

    !subroutine t3d_antisym(n0,n1,n2,n3,t3d)

    !    use const, only: p

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), intent(inout) :: t3d(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2)
    !    integer :: a,b,c,i,j,k

    !    do i=n0+1,n2-2
    !        do j=i+1,n2-1
    !            do k=j+1,n2
    !                do a=n2+1,n3-2
    !                    do b=a+1,n3-1
    !                        do c=b+1,n3
    !                            t3d(c,b,a,k,i,j)=-t3d(c,b,a,k,j,i)
    !                            t3d(c,b,a,i,j,k)=-t3d(c,b,a,k,j,i)
    !                            t3d(c,b,a,i,k,j)= t3d(c,b,a,k,j,i)
    !                            t3d(c,b,a,j,k,i)=-t3d(c,b,a,k,j,i)
    !                            t3d(c,b,a,j,i,k)= t3d(c,b,a,k,j,i)
    !                            t3d(c,a,b,k,j,i)=-t3d(c,b,a,k,j,i)
    !                            t3d(c,a,b,k,i,j)= t3d(c,b,a,k,j,i)
    !                            t3d(c,a,b,i,j,k)= t3d(c,b,a,k,j,i)
    !                            t3d(c,a,b,i,k,j)=-t3d(c,b,a,k,j,i)
    !                            t3d(c,a,b,j,k,i)= t3d(c,b,a,k,j,i)
    !                            t3d(c,a,b,j,i,k)=-t3d(c,b,a,k,j,i)
    !                            t3d(a,b,c,k,j,i)=-t3d(c,b,a,k,j,i)
    !                            t3d(a,b,c,k,i,j)= t3d(c,b,a,k,j,i)
    !                            t3d(a,b,c,i,j,k)= t3d(c,b,a,k,j,i)
    !                            t3d(a,b,c,i,k,j)=-t3d(c,b,a,k,j,i)
    !                            t3d(a,b,c,j,k,i)= t3d(c,b,a,k,j,i)
    !                            t3d(a,b,c,j,i,k)=-t3d(c,b,a,k,j,i)
    !                            t3d(a,c,b,k,j,i)= t3d(c,b,a,k,j,i)
    !                            t3d(a,c,b,k,i,j)=-t3d(c,b,a,k,j,i)
    !                            t3d(a,c,b,i,j,k)=-t3d(c,b,a,k,j,i)
    !                            t3d(a,c,b,i,k,j)= t3d(c,b,a,k,j,i)
    !                            t3d(a,c,b,j,k,i)=-t3d(c,b,a,k,j,i)
    !                            t3d(a,c,b,j,i,k)= t3d(c,b,a,k,j,i)
    !                            t3d(b,c,a,k,j,i)=-t3d(c,b,a,k,j,i)
    !                            t3d(b,c,a,k,i,j)= t3d(c,b,a,k,j,i)
    !                            t3d(b,c,a,i,j,k)= t3d(c,b,a,k,j,i)
    !                            t3d(b,c,a,i,k,j)=-t3d(c,b,a,k,j,i)
    !                            t3d(b,c,a,j,k,i)= t3d(c,b,a,k,j,i)
    !                            t3d(b,c,a,j,i,k)=-t3d(c,b,a,k,j,i)
    !                            t3d(b,a,c,k,j,i)= t3d(c,b,a,k,j,i)
    !                            t3d(b,a,c,k,i,j)=-t3d(c,b,a,k,j,i)
    !                            t3d(b,a,c,i,j,k)=-t3d(c,b,a,k,j,i)
    !                            t3d(b,a,c,i,k,j)= t3d(c,b,a,k,j,i)
    !                            t3d(b,a,c,j,k,i)=-t3d(c,b,a,k,j,i)
    !                            t3d(b,a,c,j,i,k)= t3d(c,b,a,k,j,i)
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !end subroutine t3d_antisym

    !subroutine t4a_antisym(n0, n1, n2, n3)

    !    use const, only: p, ta

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), allocatable :: t4a(:,:,:,:,:,:,:,:)
    !    integer :: a,b,c,d,i,j,k,l

    !    allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
    !    rewind(ta)
    !    read(ta)t4a

    !    do i=n0+1,n1-3
    !        do j=i+1,n1-2
    !            do k=j+1,n1-1
    !                do l=k+1,n1
    !                    do a=n1+1,n3-3
    !                        do b=a+1,n3-2
    !                            do c=b+1,n3-1
    !                                do d=c+1,n3
    !                                    t4a(d,c,b,a,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,b,a,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,c,a,b,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,b,c,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,a,c,b,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,b,d,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,c,d,b,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,b,c,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,d,c,b,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,c,a,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(d,b,a,c,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,c,d,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(a,b,d,c,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,d,a,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,c,a,d,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,d,c,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,a,c,d,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,c,a,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(b,d,a,c,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,b,a,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,d,a,b,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,b,d,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,a,d,b,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,d,a,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
    !                                    t4a(c,b,a,d,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
    !                                enddo
    !                            enddo
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo
    !    rewind(ta)
    !    write(ta)t4a

    !end subroutine t4a_antisym

    !subroutine t4b_antisym(n0, n1, n2, n3)

    !    use const, only: p, tb

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), allocatable :: t4b(:,:,:,:,:,:,:,:)
    !    integer :: a,b,c,d,i,j,k,l

    !    allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
    !    rewind(tb)
    !    read(tb)t4b

    !    do i=n0+1,n1-2
    !        do j=i+1,n1-1
    !            do k=j+1,n1
    !                do l=n0+1,n2
    !                    do a=n1+1,n3-2
    !                        do b=a+1,n3-1
    !                            do c=b+1,n3
    !                                do d=n2+1,n3

    !                                    t4b(d,c,b,a,l,k,i,j)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,b,a,l,i,j,k)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,b,a,l,i,k,j)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,b,a,l,j,k,i)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,b,a,l,j,i,k)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,a,b,l,k,j,i)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,a,b,l,k,i,j)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,a,b,l,i,j,k)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,a,b,l,i,k,j)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,a,b,l,j,k,i)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,c,a,b,l,j,i,k)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,b,c,l,k,j,i)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,b,c,l,k,i,j)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,b,c,l,i,j,k)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,b,c,l,i,k,j)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,b,c,l,j,k,i)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,b,c,l,j,i,k)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,c,b,l,k,j,i)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,c,b,l,k,i,j)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,c,b,l,i,j,k)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,c,b,l,i,k,j)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,c,b,l,j,k,i)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,a,c,b,l,j,i,k)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,c,a,l,k,j,i)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,c,a,l,k,i,j)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,c,a,l,i,j,k)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,c,a,l,i,k,j)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,c,a,l,j,k,i)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,c,a,l,j,i,k)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,a,c,l,k,j,i)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,a,c,l,k,i,j)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,a,c,l,i,j,k)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,a,c,l,i,k,j)= t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,a,c,l,j,k,i)=-t4b(d,c,b,a,l,k,j,i)
    !                                    t4b(d,b,a,c,l,j,i,k)= t4b(d,c,b,a,l,k,j,i)
    !                                enddo
    !                            enddo
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !    rewind(tb)
    !    write(tb)t4b

    !end subroutine t4b_antisym

    !subroutine t4c_antisym(n0, n1, n2, n3)

    !    use const, only: p, tc

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), allocatable :: t4c(:,:,:,:,:,:,:,:)
    !    integer :: a,b,c,d,i,j,k,l

    !    allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
    !    rewind(tc)
    !    read(tc)t4c
    !    do i=n0+1,n1-1
    !        do j=i+1,n1
    !            do k=n0+1,n2-1
    !                do l=k+1,n2
    !                    do a=n1+1,n3-1
    !                        do b=a+1,n3
    !                            do c=n2+1,n3-1
    !                                do d=c+1,n3
    !                                    t4c(d,c,b,a,l,k,i,j)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(d,c,b,a,k,l,j,i)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(d,c,b,a,k,l,i,j)= t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(d,c,a,b,l,k,j,i)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(d,c,a,b,l,k,i,j)= t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(d,c,a,b,k,l,j,i)= t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(d,c,a,b,k,l,i,j)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,b,a,l,k,j,i)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,b,a,l,k,i,j)= t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,b,a,k,l,j,i)= t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,b,a,k,l,i,j)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,a,b,l,k,j,i)= t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,a,b,l,k,i,j)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,a,b,k,l,j,i)=-t4c(d,c,b,a,l,k,j,i)
    !                                    t4c(c,d,a,b,k,l,i,j)= t4c(d,c,b,a,l,k,j,i)
    !                                enddo
    !                            enddo
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !    rewind(tc)
    !    write(tc)t4c

    !end subroutine t4c_antisym

    !subroutine t4d_antisym(n0, n1, n2, n3)

    !    use const, only: p, td

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), allocatable :: t4d(:,:,:,:,:,:,:,:)
    !    integer :: a,b,c,d,i,j,k,l

    !    allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
    !    rewind(td)
    !    read(td)t4d

    !    do i=n0+1,n1
    !        do j=n0+1,n2-2
    !            do k=j+1,n2-1
    !                do l=k+1,n2
    !                    do a=n1+1,n3
    !                        do b=n2+1,n3-2
    !                            do c=b+1,n3-1
    !                                do d=c+1,n3
    !                                    t4d(d,c,b,a,l,j,k,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,c,b,a,j,k,l,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,c,b,a,j,l,k,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,c,b,a,k,l,j,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,c,b,a,k,j,l,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,b,c,a,l,k,j,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,b,c,a,l,j,k,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,b,c,a,j,k,l,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,b,c,a,j,l,k,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,b,c,a,k,l,j,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(d,b,c,a,k,j,l,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,c,d,a,l,k,j,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,c,d,a,l,j,k,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,c,d,a,j,k,l,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,c,d,a,j,l,k,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,c,d,a,k,l,j,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,c,d,a,k,j,l,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,d,c,a,l,k,j,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,d,c,a,l,j,k,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,d,c,a,j,k,l,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,d,c,a,j,l,k,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,d,c,a,k,l,j,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(b,d,c,a,k,j,l,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,d,b,a,l,k,j,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,d,b,a,l,j,k,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,d,b,a,j,k,l,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,d,b,a,j,l,k,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,d,b,a,k,l,j,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,d,b,a,k,j,l,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,b,d,a,l,k,j,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,b,d,a,l,j,k,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,b,d,a,j,k,l,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,b,d,a,j,l,k,i)= t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,b,d,a,k,l,j,i)=-t4d(d,c,b,a,l,k,j,i)
    !                                    t4d(c,b,d,a,k,j,l,i)= t4d(d,c,b,a,l,k,j,i)
    !                                enddo
    !                            enddo
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !    rewind(td)
    !    write(td)t4d

    !end subroutine t4d_antisym

    !subroutine t4e_antisym(n0, n1, n2, n3)

    !    use const, only: p, te

    !    integer, intent(in) :: n0, n1, n2, n3
    !    real(p), allocatable :: t4e(:,:,:,:,:,:,:,:)
    !    integer :: a,b,c,d,i,j,k,l

    !    allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
    !    rewind(te)
    !    read(te)t4e

    !    do i=n0+1,n2-3
    !        do j=i+1,n2-2
    !            do k=j+1,n2-1
    !                do l=k+1,n2
    !                    do a=n2+1,n3-3
    !                        do b=a+1,n3-2
    !                            do c=b+1,n3-1
    !                                do d=c+1,n3
    !                                    t4e(d,c,b,a,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,b,a,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,c,a,b,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,b,c,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,a,c,b,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,b,d,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,c,d,b,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,b,c,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,d,c,b,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,c,a,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(d,b,a,c,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,c,d,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(a,b,d,c,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,d,a,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,c,a,d,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,d,c,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,a,c,d,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,c,a,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(b,d,a,c,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,b,a,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,d,a,b,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,b,d,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,a,d,b,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,l,k,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,l,k,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,l,i,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,l,i,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,i,k,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,i,k,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,i,l,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,i,l,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,l,j,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,l,j,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,i,j,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,i,j,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,j,k,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,j,k,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,j,i,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,j,i,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,j,l,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,j,l,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,k,l,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,k,l,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,k,i,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,k,i,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,k,j,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,d,a,k,j,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,l,k,j,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,l,k,i,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,l,i,j,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,l,i,k,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,i,k,j,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,i,k,l,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,i,l,j,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,i,l,k,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,l,j,k,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,l,j,i,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,i,j,k,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,i,j,l,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,j,k,l,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,j,k,i,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,j,i,l,k)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,j,i,k,l)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,j,l,k,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,j,l,i,k)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,k,l,j,i)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,k,l,i,j)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,k,i,j,l)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,k,i,l,j)= t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,k,j,l,i)=-t4e(d,c,b,a,l,k,j,i)
    !                                    t4e(c,b,a,d,k,j,i,l)= t4e(d,c,b,a,l,k,j,i)
    !                                enddo
    !                            enddo
    !                        enddo
    !                    enddo
    !                enddo
    !            enddo
    !        enddo
    !    enddo

    !    rewind(te)
    !    write(te)t4e

    !end subroutine t4e_antisym




end module cc_utils
