module cc_utils

    implicit none

contains

    function residuum(conv, iter, diis_space)

        use const, only: p
        use solver_types, only: conv_t

        use hdf5_io, only: get_chunk_residuum


        type(conv_t), intent(in) :: conv
        integer, intent(in) :: iter
        integer, intent(in) :: diis_space

        real(p) :: residuum
        integer :: indx1, indx2


        indx1 = mod(iter - 1, diis_space + 1)
        if (indx1 == 0) indx1 = diis_space + 1

        indx2 = mod(iter, diis_space + 1)
        if (indx2 == 0) indx2 = diis_space + 1


        residuum = get_chunk_residuum(conv%filename, conv%iter_dset_name, &
             indx1, indx2)

        !residuum = 0.0_p

        residuum = dsqrt(residuum)

    end function residuum

    subroutine antisym_p_space(froz, occ_a, occ_b, orbs, p_space)

        integer, intent(in) :: froz, occ_a, occ_b, orbs
        integer, intent(inout) :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

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

    subroutine antisymmetrize_t2(sys, t2a, t2c)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        real(p), intent(inout) :: t2a(:,:,:,:), t2c(:,:,:,:)

        integer :: a, b, c
        integer :: i, j, k


        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)


            do i=froz+1, occ_a
                do j=i+1, occ_a
                    do a=1, total-occ_a
                        do b=a+1, total-occ_a
                            t2a(b,a,i,j)=-t2a(b,a,j,i) !(ij)
                            t2a(a,b,j,i)=-t2a(b,a,j,i) !(ab)
                            t2a(a,b,i,j)=t2a(b,a,j,i) !(ab)(ij)
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1, occ_b
                do j=i+1, occ_b
                    do a=1, total-occ_b
                        do b=a+1, total-occ_b
                            t2c(b,a,i,j)=-t2c(b,a,j,i) !(ij)
                            t2c(a,b,j,i)=-t2c(b,a,j,i) !(ab)
                            t2c(a,b,i,j)=t2c(b,a,j,i) !(ab)(ij)
                        enddo
                    enddo
                enddo
            enddo

        end associate

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

        integer :: k1, k2, k3, k4
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d, t_size

        K1 = sys%occ_a-sys%froz
        K3 = sys%orbs-sys%occ_a
        K2 = sys%occ_b-sys%froz
        K4 = sys%orbs-sys%occ_b

        ! Zero positions to prevent issues
        cc%pos = 0

        ! T1A
        t_size = 0
        cc%pos(1) = t_size+1

        ! T1B
        t_size = t_size+K1*K3
        cc%pos(2) = t_size+1

        ! T2A
        t_size = t_size+K2*K4
        cc%pos(3) = t_size+1

        ! T2B
        t_size = t_size+K1*K1*K3*K3
        cc%pos(4) = t_size+1

        ! T2C
        t_size = t_size+K2*K2*K4*K4
        cc%pos(5) = t_size+1

        ! T3A
        t_size = t_size+K1*K2*K3*K4
        cc%pos(6) = t_size+1

        ! T3B
        t_size = t_size+K3*K3*K3*K1*K1*K1
        cc%pos(7) = t_size+1

        ! T3C
        t_size = t_size+K4*K4*K4*K2*K2*K2
        cc%pos(8) = t_size+1

        ! T3D
        t_size = t_size+K3*K4*K3*K1*K2*K1
        cc%pos(9) = t_size+1

        ! T max
        t_size = t_size+K4*K4*K3*K2*K2*K1
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

end module cc_utils
