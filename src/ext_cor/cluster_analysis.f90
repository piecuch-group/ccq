module cluster_analysis

    ! Cluster analysis module

    implicit none

contains


    subroutine analyze_t3(sys, c_vec, t_vec, rhf, rm_dscnctd)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use const, only: dp
        use ext_cor_types, only: vec3_t
        use symmetry, only: is_sym
        use system, only: sys_t
        use cc_utils, only: t3_aab_to_t3_abb

        type(sys_t), intent(in) :: sys
        ! CI coefficients
        type(vec3_t), intent(in) :: c_vec
        ! T amplitudes
        type(vec3_t), intent(inout) :: t_vec

        logical, intent(in) :: rhf
        logical, intent(in) :: rm_dscnctd


        ! Place holders
        real(dp) :: c1c3, c12c2, c22, c13
        integer :: ex_orbs(8)

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d
        integer :: indx


        ex_orbs = 0

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            do i=froz+1,occ_a
                do j=i+1,occ_a
                    do a=occ_a+1,total
                        do b=a+1,total
                            if (rm_dscnctd .and. c_vec%o2_aa(a,b,i,j) == 0.0) cycle
                            t_vec%o2_aa(a,b,i,j)=(c_vec%o2_aa(a,b,i,j) &
                                -c_vec%o1_a(a,i)*c_vec%o1_a(b,j) &
                                +c_vec%o1_a(a,j)*c_vec%o1_a(b,i))
                        enddo
                    enddo
                enddo
            enddo


            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do a=occ_a+1,total
                        do b=occ_b+1,total
                            if (rm_dscnctd .and. c_vec%o2_ab(a,b,i,j) == 0.0) cycle
                            t_vec%o2_ab(a,b,i,j)=(c_vec%o2_ab(a,b,i,j) &
                                -c_vec%o1_a(a,i)*c_vec%o1_b(b,j))
                        enddo
                    enddo
                enddo
            enddo

            if (rhf) then
                t_vec%o2_bb = t_vec%o2_aa
            else
                do i=froz+1,occ_b
                    do j=i+1,occ_b
                        do a=occ_b+1,total
                            do b=a+1,total
                                if (rm_dscnctd .and. c_vec%o2_bb(a,b,i,j) == 0.0) cycle
                                t_vec%o2_bb(a,b,i,j)=(c_vec%o2_bb(a,b,i,j) &
                                    -c_vec%o1_b(a,i)*c_vec%o1_b(b,j) &
                                    +c_vec%o1_b(a,j)*c_vec%o1_b(b,i))
                            enddo
                        enddo
                    enddo
                enddo
            endif

            do i=froz+1,occ_a
                do j=i+1,occ_a
                    do k=j+1,occ_a
                        do a=occ_a+1,total
                            do b=a+1,total
                                do c=b+1,total

                                    ex_orbs(1) = a
                                    ex_orbs(2) = b
                                    ex_orbs(3) = c
                                    ex_orbs(4) = i
                                    ex_orbs(5) = j
                                    ex_orbs(6) = k

                                    if (.not. is_sym(ex_orbs, 6)) cycle
                                    if (rm_dscnctd .and. c_vec%o3_aaa(a,b,c,i,j,k) == 0.0) cycle
                                    t_vec%o3_aaa(a,b,c,i,j,k)=(c_vec%o3_aaa(a,b,c,i,j,k) &
                                        -c_vec%o1_a(a,i)*c_vec%o2_aa(b,c,j,k) &
                                        +c_vec%o1_a(a,j)*c_vec%o2_aa(b,c,i,k) &
                                        +c_vec%o1_a(a,k)*c_vec%o2_aa(b,c,j,i) &
                                        +c_vec%o1_a(b,i)*c_vec%o2_aa(a,c,j,k) &
                                        +c_vec%o1_a(c,i)*c_vec%o2_aa(b,a,j,k) &
                                        -c_vec%o1_a(b,j)*c_vec%o2_aa(a,c,i,k) &
                                        -c_vec%o1_a(c,j)*c_vec%o2_aa(b,a,i,k) &
                                        -c_vec%o1_a(b,k)*c_vec%o2_aa(a,c,j,i) &
                                        -c_vec%o1_a(c,k)*c_vec%o2_aa(b,a,j,i) &
                                        +2*(c_vec%o1_a(a,i)*c_vec%o1_a(b,j)*c_vec%o1_a(c,k) &
                                        -c_vec%o1_a(a,j)*c_vec%o1_a(b,i)*c_vec%o1_a(c,k) &
                                        -c_vec%o1_a(a,k)*c_vec%o1_a(b,j)*c_vec%o1_a(c,i) &
                                        -c_vec%o1_a(a,i)*c_vec%o1_a(b,k)*c_vec%o1_a(c,j) &
                                        +c_vec%o1_a(a,j)*c_vec%o1_a(b,k)*c_vec%o1_a(c,i) &
                                        +c_vec%o1_a(a,k)*c_vec%o1_a(b,i)*c_vec%o1_a(c,j)))
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1,occ_a
                do j=i+1,occ_a
                    do k=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=a+1,total
                                do c=occ_b+1,total

                                    ex_orbs(1) = a
                                    ex_orbs(2) = b
                                    ex_orbs(3) = c
                                    ex_orbs(4) = i
                                    ex_orbs(5) = j
                                    ex_orbs(6) = k

                                    if (.not. is_sym(ex_orbs, 6)) cycle
                                    if (rm_dscnctd .and. c_vec%o3_aab(a,b,c,i,j,k) == 0.0) cycle
                                    t_vec%o3_aab(a,b,c,i,j,k)=(c_vec%o3_aab(a,b,c,i,j,k) &
                                        -c_vec%o1_a(a,i)*c_vec%o2_ab(b,c,j,k) &
                                        +c_vec%o1_a(a,j)*c_vec%o2_ab(b,c,i,k) &
                                        +c_vec%o1_a(b,i)*c_vec%o2_ab(a,c,j,k) &
                                        -c_vec%o1_a(b,j)*c_vec%o2_ab(a,c,i,k) &
                                        -c_vec%o1_b(c,k)*c_vec%o2_aa(a,b,i,j) &
                                        +2*(c_vec%o1_a(a,i)*c_vec%o1_a(b,j)*c_vec%o1_b(c,k) &
                                        -c_vec%o1_a(a,j)*c_vec%o1_a(b,i)*c_vec%o1_b(c,k)))
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            if (rhf) then
                call t3_aab_to_t3_abb(sys, t_vec%o3_aab, t_vec%o3_abb)
                t_vec%o3_bbb = t_vec%o3_aaa
            else

                do i=froz+1,occ_a
                    do j=froz+1,occ_b
                        do k=j+1,occ_b
                            do a=occ_a+1,total
                                do b=occ_b+1,total
                                    do c=b+1,total

                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = i
                                        ex_orbs(5) = j
                                        ex_orbs(6) = k

                                        if (.not. is_sym(ex_orbs, 6)) cycle
                                        if (rm_dscnctd .and. c_vec%o3_abb(a,b,c,i,j,k) == 0.0) cycle
                                        t_vec%o3_abb(a,b,c,i,j,k)=(c_vec%o3_abb(a,b,c,i,j,k) &
                                            -c_vec%o1_a(a,i)*c_vec%o2_bb(b,c,j,k) &
                                            -c_vec%o1_b(c,k)*c_vec%o2_ab(a,b,i,j) &
                                            +c_vec%o1_b(c,j)*c_vec%o2_ab(a,b,i,k) &
                                            +c_vec%o1_b(b,k)*c_vec%o2_ab(a,c,i,j) &
                                            -c_vec%o1_b(b,j)*c_vec%o2_ab(a,c,i,k) &
                                            +2*(c_vec%o1_a(a,i)*c_vec%o1_b(b,j)*c_vec%o1_b(c,k) &
                                            -c_vec%o1_a(a,i)*c_vec%o1_b(b,k)*c_vec%o1_b(c,j)))
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

                do i=froz+1,occ_b
                    do j=i+1,occ_b
                        do k=j+1,occ_b
                            do a=occ_b+1,total
                                do b=a+1,total
                                    do c=b+1,total

                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = i
                                        ex_orbs(5) = j
                                        ex_orbs(6) = k

                                        if (.not. is_sym(ex_orbs, 6)) cycle
                                        if (rm_dscnctd .and. c_vec%o3_bbb(a,b,c,i,j,k) == 0.0) cycle
                                        t_vec%o3_bbb(a,b,c,i,j,k)=(c_vec%o3_bbb(a,b,c,i,j,k) &
                                            -c_vec%o1_b(a,i)*c_vec%o2_bb(b,c,j,k) &
                                            +c_vec%o1_b(a,j)*c_vec%o2_bb(b,c,i,k) &
                                            +c_vec%o1_b(a,k)*c_vec%o2_bb(b,c,j,i) &
                                            +c_vec%o1_b(b,i)*c_vec%o2_bb(a,c,j,k) &
                                            +c_vec%o1_b(c,i)*c_vec%o2_bb(b,a,j,k) &
                                            -c_vec%o1_b(b,j)*c_vec%o2_bb(a,c,i,k) &
                                            -c_vec%o1_b(c,j)*c_vec%o2_bb(b,a,i,k) &
                                            -c_vec%o1_b(b,k)*c_vec%o2_bb(a,c,j,i) &
                                            -c_vec%o1_b(c,k)*c_vec%o2_bb(b,a,j,i) &
                                            +2*(c_vec%o1_b(a,i)*c_vec%o1_b(b,j)*c_vec%o1_b(c,k) &
                                            -c_vec%o1_b(a,j)*c_vec%o1_b(b,i)*c_vec%o1_b(c,k) &
                                            -c_vec%o1_b(a,k)*c_vec%o1_b(b,j)*c_vec%o1_b(c,i) &
                                            -c_vec%o1_b(a,i)*c_vec%o1_b(b,k)*c_vec%o1_b(c,j) &
                                            +c_vec%o1_b(a,j)*c_vec%o1_b(b,k)*c_vec%o1_b(c,i) &
                                            +c_vec%o1_b(a,k)*c_vec%o1_b(b,i)*c_vec%o1_b(c,j)))
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            endif

        end associate

    end subroutine analyze_t3

    subroutine analyze_t4_aaaa(c_vec, excit, c4, t4)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use const, only: dp
        use excitations, only: excit_t
        use ext_cor_types, only: vec3_t

        ! CI coefficients
        type(vec3_t), intent(in) :: c_vec
        real(dp), intent(in) :: c4
        type(excit_t), intent(in) :: excit

        ! T amplitudes
        real(dp), intent(inout) :: t4

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        ! Place holders
        real(dp) :: c1c3, c12c2, c22, c13


        i = excit%from_a(1)
        j = excit%from_a(2)
        k = excit%from_a(3)
        l = excit%from_a(4)

        a = excit%to_a(1)
        b = excit%to_a(2)
        c = excit%to_a(3)
        d = excit%to_a(4)

        associate(c1_a=>c_vec%o1_a, c1_b=>c_vec%o1_b, &
                c2_aa=>c_vec%o2_aa, c2_ab=>c_vec%o2_ab, c2_bb=>c_vec%o2_bb, &
                c3_aaa=>c_vec%o3_aaa, c3_aab=>c_vec%o3_aab, &
                c3_abb=>c_vec%o3_abb, c3_bbb=>c_vec%o3_bbb)

            t4 = (c4 &
                ! -C_1 * C_3
            -c1_a(a,i)*c3_aaa(b,c,d,j,k,l) &
                +c1_a(a,j)*c3_aaa(b,c,d,i,k,l) &
                +c1_a(a,k)*c3_aaa(b,c,d,j,i,l) &
                +c1_a(a,l)*c3_aaa(b,c,d,j,k,i) &
                +c1_a(b,i)*c3_aaa(a,c,d,j,k,l) &
                +c1_a(c,i)*c3_aaa(b,a,d,j,k,l) &
                +c1_a(d,i)*c3_aaa(b,c,a,j,k,l) &
                -c1_a(b,j)*c3_aaa(a,c,d,i,k,l) &
                -c1_a(c,j)*c3_aaa(b,a,d,i,k,l) &
                -c1_a(d,j)*c3_aaa(b,c,a,i,k,l) &
                -c1_a(b,k)*c3_aaa(a,c,d,j,i,l) &
                -c1_a(c,k)*c3_aaa(b,a,d,j,i,l) &
                -c1_a(d,k)*c3_aaa(b,c,a,j,i,l) &
                -c1_a(b,l)*c3_aaa(a,c,d,j,k,i) &
                -c1_a(c,l)*c3_aaa(b,a,d,j,k,i) &
                -c1_a(d,l)*c3_aaa(b,c,a,j,k,i) &
                ! 2 * 1/2 C_1^2 * C_2
            +2*(c1_a(a,i)*c1_a(b,j)*c2_aa(c,d,k,l) & !1
                -c1_a(a,k)*c1_a(b,j)*c2_aa(c,d,i,l) & !(ik)
                -c1_a(a,l)*c1_a(b,j)*c2_aa(c,d,k,i) & !(il)
                -c1_a(a,j)*c1_a(b,i)*c2_aa(c,d,k,l) & !(ij)
                -c1_a(a,i)*c1_a(b,k)*c2_aa(c,d,j,l) & !(jk)
                -c1_a(a,i)*c1_a(b,l)*c2_aa(c,d,k,j) & !(jl)
                -c1_a(c,i)*c1_a(b,j)*c2_aa(a,d,k,l) & !(ac)
                -c1_a(d,i)*c1_a(b,j)*c2_aa(c,a,k,l) & !(ad)
                -c1_a(a,i)*c1_a(c,j)*c2_aa(b,d,k,l) & !(bc)
                -c1_a(a,i)*c1_a(d,j)*c2_aa(c,b,k,l) & !(bd)
                +c1_a(c,i)*c1_a(d,j)*c2_aa(a,b,k,l) & !(ac)(bd)
                +c1_a(c,k)*c1_a(b,j)*c2_aa(a,d,i,l) & !(ac)(ik)
                +c1_a(d,k)*c1_a(b,j)*c2_aa(c,a,i,l) & !(ad)(ik)
                +c1_a(a,k)*c1_a(c,j)*c2_aa(b,d,i,l) & !(bc)(ik)
                +c1_a(a,k)*c1_a(d,j)*c2_aa(c,b,i,l) & !(bd)(ik)
                +c1_a(c,l)*c1_a(b,j)*c2_aa(a,d,k,i) & !(ac)(il)
                +c1_a(d,l)*c1_a(b,j)*c2_aa(c,a,k,i) & !(ad)(il)
                +c1_a(a,l)*c1_a(c,j)*c2_aa(b,d,k,i) & !(bc)(il)
                +c1_a(a,l)*c1_a(d,j)*c2_aa(c,b,k,i) & !(bd)(il)
                +c1_a(c,j)*c1_a(b,i)*c2_aa(a,d,k,l) & !(ac)(ij)
                +c1_a(d,j)*c1_a(b,i)*c2_aa(c,a,k,l) & !(ad)(ij)
                +c1_a(a,j)*c1_a(c,i)*c2_aa(b,d,k,l) & !(bc)(ij)
                +c1_a(a,j)*c1_a(d,i)*c2_aa(c,b,k,l) & !(bd)(ij)
                +c1_a(c,i)*c1_a(b,k)*c2_aa(a,d,j,l) & !(ac)(jk)
                +c1_a(d,i)*c1_a(b,k)*c2_aa(c,a,j,l) & !(ad)(jk)
                +c1_a(a,i)*c1_a(c,k)*c2_aa(b,d,j,l) & !(bc)(jk)
                +c1_a(a,i)*c1_a(d,k)*c2_aa(c,b,j,l) & !(bd)(jk)
                +c1_a(c,i)*c1_a(b,l)*c2_aa(a,d,k,j) & !(ac)(jl)
                +c1_a(d,i)*c1_a(b,l)*c2_aa(c,a,k,j) & !(ad)(jl)
                +c1_a(a,i)*c1_a(c,l)*c2_aa(b,d,k,j) & !(bc)(jl)
                +c1_a(a,i)*c1_a(d,l)*c2_aa(c,b,k,j) & !(bd)(jl)
                +c1_a(a,j)*c1_a(b,k)*c2_aa(c,d,i,l) & !(ijk)
                +c1_a(a,k)*c1_a(b,i)*c2_aa(c,d,j,l) & !(ikj)
                +c1_a(a,j)*c1_a(b,l)*c2_aa(c,d,k,i) & !(ijl)
                +c1_a(a,l)*c1_a(b,i)*c2_aa(c,d,k,j) & !(ilj)
                -c1_a(a,l)*c1_a(b,k)*c2_aa(c,d,i,j) & !(kilj)
                -c1_a(c,k)*c1_a(d,j)*c2_aa(a,b,i,l) & !(ac)(bd)(ik)
                -c1_a(c,l)*c1_a(d,j)*c2_aa(a,b,k,i) & !(ac)(bd)(il)
                -c1_a(c,j)*c1_a(d,i)*c2_aa(a,b,k,l) & !(ac)(bd)(ij)
                -c1_a(c,i)*c1_a(d,k)*c2_aa(a,b,j,l) & !(ac)(bd)(jk)
                -c1_a(c,i)*c1_a(d,l)*c2_aa(a,b,k,j) & !(ac)(bd)(jl)
                -c1_a(c,j)*c1_a(b,k)*c2_aa(a,d,i,l) & !(ac)(ijk)
                -c1_a(d,j)*c1_a(b,k)*c2_aa(c,a,i,l) & !(ad)(ijk)
                -c1_a(a,j)*c1_a(c,k)*c2_aa(b,d,i,l) & !(bc)(ijk)
                -c1_a(a,j)*c1_a(d,k)*c2_aa(c,b,i,l) & !(bd)(ijk)
                -c1_a(c,k)*c1_a(b,i)*c2_aa(a,d,j,l) & !(ac)(ikj)
                -c1_a(d,k)*c1_a(b,i)*c2_aa(c,a,j,l) & !(ad)(ikj)
                -c1_a(a,k)*c1_a(c,i)*c2_aa(b,d,j,l) & !(bc)(ikj)
                -c1_a(a,k)*c1_a(d,i)*c2_aa(c,b,j,l) & !(bd)(ikj)
                -c1_a(c,j)*c1_a(b,l)*c2_aa(a,d,k,i) & !(ac)(ijl)
                -c1_a(d,j)*c1_a(b,l)*c2_aa(c,a,k,i) & !(ad)(ijl)
                -c1_a(a,j)*c1_a(c,l)*c2_aa(b,d,k,i) & !(bc)(ijl)
                -c1_a(a,j)*c1_a(d,l)*c2_aa(c,b,k,i) & !(bd)(ijl)
                -c1_a(c,l)*c1_a(b,i)*c2_aa(a,d,k,j) & !(ac)(ilj)
                -c1_a(d,l)*c1_a(b,i)*c2_aa(c,a,k,j) & !(ad)(ilj)
                -c1_a(a,l)*c1_a(c,i)*c2_aa(b,d,k,j) & !(bc)(ilj)
                -c1_a(a,l)*c1_a(d,i)*c2_aa(c,b,k,j) & !(bd)(ilj)
                +c1_a(c,j)*c1_a(d,k)*c2_aa(a,b,i,l) & !(ac)(bd)(ijk)
                +c1_a(c,k)*c1_a(d,i)*c2_aa(a,b,j,l) & !(ac)(bd)(ikj)
                +c1_a(c,j)*c1_a(d,l)*c2_aa(a,b,k,i) & !(ac)(bd)(ijl)
                +c1_a(c,l)*c1_a(d,i)*c2_aa(a,b,k,j) & !(ac)(bd)(ilj)
                +c1_a(c,l)*c1_a(b,k)*c2_aa(a,d,i,j) & !(ac)(kilj)
                +c1_a(d,l)*c1_a(b,k)*c2_aa(c,a,i,j) & !(ad)(kilj)
                +c1_a(a,l)*c1_a(c,k)*c2_aa(b,d,i,j) & !(bc)(kilj)
                +c1_a(a,l)*c1_a(d,k)*c2_aa(c,b,i,j) & !(bd)(kilj)
                -c1_a(c,l)*c1_a(d,k)*c2_aa(a,b,i,j) & !(ac)(bd)(kilj)
                -c1_a(c,k)*c1_a(b,l)*c2_aa(a,d,i,j) & !(ac)(ik)(jl)
                -c1_a(d,k)*c1_a(b,l)*c2_aa(c,a,i,j) & !(ad)(ik)(jl)
                -c1_a(a,k)*c1_a(c,l)*c2_aa(b,d,i,j) & !(bc)(ik)(jl)
                -c1_a(a,k)*c1_a(d,l)*c2_aa(c,b,i,j) & !(bd)(ik)(jl)
                +c1_a(c,k)*c1_a(d,l)*c2_aa(a,b,i,j) & !(ac)(bd)(ik)(jl)
                +c1_a(a,k)*c1_a(b,l)*c2_aa(c,d,i,j)) & !(ik)(jl)
                -c2_aa(a,b,i,j)*c2_aa(c,d,k,l) &
                +c2_aa(a,b,k,j)*c2_aa(c,d,i,l) &
                +c2_aa(a,b,l,j)*c2_aa(c,d,k,i) &
                +c2_aa(a,b,i,k)*c2_aa(c,d,j,l) &
                +c2_aa(a,b,i,l)*c2_aa(c,d,k,j) &
                +c2_aa(c,b,i,j)*c2_aa(a,d,k,l) &
                +c2_aa(d,b,i,j)*c2_aa(c,a,k,l) &
                -c2_aa(a,b,k,l)*c2_aa(c,d,i,j) &
                -c2_aa(c,b,k,j)*c2_aa(a,d,i,l) &
                -c2_aa(c,b,l,j)*c2_aa(a,d,k,i) &
                -c2_aa(c,b,i,k)*c2_aa(a,d,j,l) &
                -c2_aa(c,b,i,l)*c2_aa(a,d,k,j) &
                -c2_aa(d,b,k,j)*c2_aa(c,a,i,l) &
                -c2_aa(d,b,l,j)*c2_aa(c,a,k,i) &
                -c2_aa(d,b,i,k)*c2_aa(c,a,j,l) &
                -c2_aa(d,b,i,l)*c2_aa(c,a,k,j) &
                +c2_aa(c,b,k,l)*c2_aa(a,d,i,j) &
                +c2_aa(d,b,k,l)*c2_aa(c,a,i,j) &
                -6*(c1_a(a,i)*c1_a(b,j)*c1_a(c,k)*c1_a(d,l) &
                -c1_a(a,j)*c1_a(b,i)*c1_a(c,k)*c1_a(d,l) &
                -c1_a(a,k)*c1_a(b,j)*c1_a(c,i)*c1_a(d,l) &
                -c1_a(a,l)*c1_a(b,j)*c1_a(c,k)*c1_a(d,i) &
                -c1_a(a,i)*c1_a(b,k)*c1_a(c,j)*c1_a(d,l) &
                -c1_a(a,i)*c1_a(b,l)*c1_a(c,k)*c1_a(d,j) &
                -c1_a(a,i)*c1_a(b,j)*c1_a(c,l)*c1_a(d,k) &
                +c1_a(a,j)*c1_a(b,k)*c1_a(c,i)*c1_a(d,l) &
                +c1_a(a,k)*c1_a(b,i)*c1_a(c,j)*c1_a(d,l) &
                +c1_a(a,j)*c1_a(b,l)*c1_a(c,k)*c1_a(d,i) &
                +c1_a(a,l)*c1_a(b,i)*c1_a(c,k)*c1_a(d,j) &
                +c1_a(a,k)*c1_a(b,j)*c1_a(c,l)*c1_a(d,i) &
                +c1_a(a,l)*c1_a(b,j)*c1_a(c,i)*c1_a(d,k) &
                +c1_a(a,i)*c1_a(b,k)*c1_a(c,l)*c1_a(d,j) &
                +c1_a(a,i)*c1_a(b,l)*c1_a(c,j)*c1_a(d,k) &
                +c1_a(a,j)*c1_a(b,i)*c1_a(c,l)*c1_a(d,k) &
                +c1_a(a,k)*c1_a(b,l)*c1_a(c,i)*c1_a(d,j) &
                +c1_a(a,l)*c1_a(b,k)*c1_a(c,j)*c1_a(d,i) &
                -c1_a(a,j)*c1_a(b,k)*c1_a(c,l)*c1_a(d,i) &
                -c1_a(a,j)*c1_a(b,l)*c1_a(c,i)*c1_a(d,k) &
                -c1_a(a,k)*c1_a(b,l)*c1_a(c,j)*c1_a(d,i) &
                -c1_a(a,k)*c1_a(b,i)*c1_a(c,l)*c1_a(d,j) &
                -c1_a(a,l)*c1_a(b,i)*c1_a(c,j)*c1_a(d,k) &
                -c1_a(a,l)*c1_a(b,k)*c1_a(c,i)*c1_a(d,j)))

        end associate

    end subroutine analyze_t4_aaaa

    subroutine analyze_t4_aaab(c_vec, excit, c4, t4)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use const, only: dp
        use excitations, only: excit_t
        use ext_cor_types, only: vec3_t

        ! CI coefficients
        type(vec3_t), intent(in) :: c_vec
        real(dp), intent(in) :: c4
        type(excit_t), intent(in) :: excit

        ! T amplitudes
        real(dp), intent(inout) :: t4

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        ! Place holders
        real(dp) :: c1c3, c12c2, c22, c13

        i = excit%from_a(1)
        j = excit%from_a(2)
        k = excit%from_a(3)
        l = excit%from_b(1)

        a = excit%to_a(1)
        b = excit%to_a(2)
        c = excit%to_a(3)
        d = excit%to_b(1)



        associate(c1_a=>c_vec%o1_a, c1_b=>c_vec%o1_b, &
                c2_aa=>c_vec%o2_aa, c2_ab=>c_vec%o2_ab, c2_bb=>c_vec%o2_bb, &
                c3_aaa=>c_vec%o3_aaa, c3_aab=>c_vec%o3_aab, &
                c3_abb=>c_vec%o3_abb, c3_bbb=>c_vec%o3_bbb)

            t4 = (c4 &
                -c1_a(a,i)*c3_aab(b,c,d,j,k,l) &
                +c1_a(a,j)*c3_aab(b,c,d,i,k,l) &
                +c1_a(a,k)*c3_aab(b,c,d,j,i,l) &
                +c1_a(b,i)*c3_aab(a,c,d,j,k,l) &
                +c1_a(c,i)*c3_aab(b,a,d,j,k,l) &
                -c1_a(b,j)*c3_aab(a,c,d,i,k,l) &
                -c1_a(c,j)*c3_aab(b,a,d,i,k,l) &
                -c1_a(b,k)*c3_aab(a,c,d,j,i,l) &
                -c1_a(c,k)*c3_aab(b,a,d,j,i,l) &
                -c1_b(d,l)*c3_aaa(a,b,c,i,j,k) &
                +2*(c1_a(a,i)*c1_b(d,l)*c2_aa(b,c,j,k) &
                -c1_a(a,j)*c1_b(d,l)*c2_aa(b,c,i,k) &
                -c1_a(a,k)*c1_b(d,l)*c2_aa(b,c,j,i) &
                -c1_a(b,i)*c1_b(d,l)*c2_aa(a,c,j,k) &
                -c1_a(c,i)*c1_b(d,l)*c2_aa(b,a,j,k) &
                +c1_a(b,j)*c1_b(d,l)*c2_aa(a,c,i,k) &
                +c1_a(c,j)*c1_b(d,l)*c2_aa(b,a,i,k) &
                +c1_a(b,k)*c1_b(d,l)*c2_aa(a,c,j,i) &
                +c1_a(c,k)*c1_b(d,l)*c2_aa(b,a,j,i) &
                +c1_a(a,i)*c1_a(b,j)*c2_ab(c,d,k,l) &
                -c1_a(a,j)*c1_a(b,i)*c2_ab(c,d,k,l) &
                -c1_a(a,k)*c1_a(b,j)*c2_ab(c,d,i,l) &
                -c1_a(a,i)*c1_a(b,k)*c2_ab(c,d,j,l) &
                -c1_a(c,i)*c1_a(b,j)*c2_ab(a,d,k,l) &
                -c1_a(a,i)*c1_a(c,j)*c2_ab(b,d,k,l) &
                +c1_a(a,j)*c1_a(b,k)*c2_ab(c,d,i,l) &
                +c1_a(a,k)*c1_a(b,i)*c2_ab(c,d,j,l) &
                +c1_a(c,j)*c1_a(b,i)*c2_ab(a,d,k,l) &
                +c1_a(a,j)*c1_a(c,i)*c2_ab(b,d,k,l) &
                +c1_a(c,k)*c1_a(b,j)*c2_ab(a,d,i,l) &
                +c1_a(a,k)*c1_a(c,j)*c2_ab(b,d,i,l) &
                +c1_a(c,i)*c1_a(b,k)*c2_ab(a,d,j,l) &
                +c1_a(a,i)*c1_a(c,k)*c2_ab(b,d,j,l) &
                -c1_a(c,j)*c1_a(b,k)*c2_ab(a,d,i,l) &
                -c1_a(a,j)*c1_a(c,k)*c2_ab(b,d,i,l) &
                -c1_a(c,k)*c1_a(b,i)*c2_ab(a,d,j,l) &
                -c1_a(a,k)*c1_a(c,i)*c2_ab(b,d,j,l)) &
                -c2_aa(a,b,i,j)*c2_ab(c,d,k,l) &
                +c2_aa(a,b,k,j)*c2_ab(c,d,i,l) &
                +c2_aa(a,b,i,k)*c2_ab(c,d,j,l) &
                +c2_aa(c,b,i,j)*c2_ab(a,d,k,l) &
                +c2_aa(a,c,i,j)*c2_ab(b,d,k,l) &
                -c2_aa(c,b,k,j)*c2_ab(a,d,i,l) &
                -c2_aa(a,c,k,j)*c2_ab(b,d,i,l) &
                -c2_aa(c,b,i,k)*c2_ab(a,d,j,l) &
                -c2_aa(a,c,i,k)*c2_ab(b,d,j,l) &
                -6*(c1_a(a,i)*c1_a(b,j)*c1_a(c,k)*c1_b(d,l) &
                -c1_a(a,j)*c1_a(b,i)*c1_a(c,k)*c1_b(d,l) &
                -c1_a(a,k)*c1_a(b,j)*c1_a(c,i)*c1_b(d,l) &
                -c1_a(a,i)*c1_a(b,k)*c1_a(c,j)*c1_b(d,l) &
                +c1_a(a,j)*c1_a(b,k)*c1_a(c,i)*c1_b(d,l) &
                +c1_a(a,k)*c1_a(b,i)*c1_a(c,j)*c1_b(d,l)))

        end associate

    end subroutine analyze_t4_aaab

    subroutine analyze_t4_aabb(c_vec, excit , c4, t4)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use const, only: dp
        use excitations, only: excit_t
        use ext_cor_types, only: vec3_t

        ! CI coefficients
        type(vec3_t), intent(in) :: c_vec
        real(dp), intent(in) :: c4
        type(excit_t), intent(in) :: excit

        ! T amplitudes
        real(dp), intent(inout) :: t4

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        ! Place holders
        real(dp) :: c1c3, c12c2, c22, c13


        i = excit%from_a(1)
        j = excit%from_a(2)
        k = excit%from_b(1)
        l = excit%from_b(2)

        a = excit%to_a(1)
        b = excit%to_a(2)
        c = excit%to_b(1)
        d = excit%to_b(2)

        associate(c1_a=>c_vec%o1_a, c1_b=>c_vec%o1_b, &
                c2_aa=>c_vec%o2_aa, c2_ab=>c_vec%o2_ab, c2_bb=>c_vec%o2_bb, &
                c3_aaa=>c_vec%o3_aaa, c3_aab=>c_vec%o3_aab, &
                c3_abb=>c_vec%o3_abb, c3_bbb=>c_vec%o3_bbb)

            c1c3 = (-c1_a(a,i)*c3_abb(b,c,d,j,k,l) &
                +c1_a(a,j)*c3_abb(b,c,d,i,k,l) &
                +c1_a(b,i)*c3_abb(a,c,d,j,k,l) &
                -c1_a(b,j)*c3_abb(a,c,d,i,k,l) &
                -c1_b(d,l)*c3_aab(a,b,c,i,j,k) &
                +c1_b(d,k)*c3_aab(a,b,c,i,j,l) &
                +c1_b(c,l)*c3_aab(a,b,d,i,j,k) &
                -c1_b(c,k)*c3_aab(a,b,d,i,j,l))

            c12c2 = 2*(c1_a(a,i)*c1_a(b,j)*c2_bb(c,d,k,l) &
                -c1_a(a,j)*c1_a(b,i)*c2_bb(c,d,k,l) &
                +c1_a(a,i)*c1_b(c,k)*c2_ab(b,d,j,l) &
                -c1_a(a,j)*c1_b(c,k)*c2_ab(b,d,i,l) &
                -c1_a(a,i)*c1_b(c,l)*c2_ab(b,d,j,k) &
                -c1_a(b,i)*c1_b(c,k)*c2_ab(a,d,j,l) &
                -c1_a(a,i)*c1_b(d,k)*c2_ab(b,c,j,l) &
                +c1_a(a,j)*c1_b(c,l)*c2_ab(b,d,i,k) &
                +c1_a(b,j)*c1_b(c,k)*c2_ab(a,d,i,l) &
                +c1_a(a,j)*c1_b(d,k)*c2_ab(b,c,i,l) &
                +c1_a(b,i)*c1_b(c,l)*c2_ab(a,d,j,k) &
                +c1_a(a,i)*c1_b(d,l)*c2_ab(b,c,j,k) &
                +c1_a(b,i)*c1_b(d,k)*c2_ab(a,c,j,l) &
                -c1_a(b,j)*c1_b(c,l)*c2_ab(a,d,i,k) &
                -c1_a(a,j)*c1_b(d,l)*c2_ab(b,c,i,k) &
                -c1_a(b,j)*c1_b(d,k)*c2_ab(a,c,i,l) &
                -c1_a(b,i)*c1_b(d,l)*c2_ab(a,c,j,k) &
                +c1_a(b,j)*c1_b(d,l)*c2_ab(a,c,i,k) &
                +c1_b(c,k)*c1_b(d,l)*c2_aa(a,b,i,j) &
                -c1_b(c,l)*c1_b(d,k)*c2_aa(a,b,i,j))

            c22 = (-c2_ab(a,c,i,k)*c2_ab(b,d,j,l) &
                +c2_ab(a,c,j,k)*c2_ab(b,d,i,l) &
                +c2_ab(a,c,i,l)*c2_ab(b,d,j,k) &
                +c2_ab(b,c,i,k)*c2_ab(a,d,j,l) &
                -c2_ab(a,c,j,l)*c2_ab(b,d,i,k) &
                -c2_ab(b,c,j,k)*c2_ab(a,d,i,l) &
                -c2_ab(b,c,i,l)*c2_ab(a,d,j,k) &
                +c2_ab(b,c,j,l)*c2_ab(a,d,i,k) &
                -c2_aa(a,b,i,j)*c2_bb(c,d,k,l))

            c13 = (-6*(c1_a(a,i)*c1_a(b,j)*c1_b(c,k)*c1_b(d,l) &
                -c1_a(a,j)*c1_a(b,i)*c1_b(c,k)*c1_b(d,l) &
                -c1_a(a,i)*c1_a(b,j)*c1_b(c,l)*c1_b(d,k) &
                +c1_a(a,j)*c1_a(b,i)*c1_b(c,l)*c1_b(d,k)))

        end associate

        ! Debuggin
        !print '(8i3)', a, b, c, d, i, j, k, l
        !if (c4_aabb(a,b,c,d,i,j,k,l) /= 0.0d0) then
        !    print '(f13.10)', c4_aabb(a,b,c,d,i,j,k,l)
        !    print '(f13.10)', c1c3
        !    print '(f13.10)', c12c2
        !    print '(f13.10)', c22
        !    print '(f13.10)', c13
        !endif


        t4=c4 + c1c3 + c12c2 + c22 + c13
        !print '(f13.10)', t4_aabb(a,b,c,d,i,j,k,l)

    end subroutine analyze_t4_aabb

    subroutine analyze_t4_abbb(c_vec, excit, c4, t4)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use const, only: dp
        use excitations, only: excit_t
        use ext_cor_types, only: vec3_t

        ! CI coefficients
        type(vec3_t), intent(in) :: c_vec
        real(dp), intent(in) :: c4
        type(excit_t), intent(in) :: excit

        ! T amplitudes
        real(dp), intent(inout) :: t4

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        ! Place holders
        real(dp) :: c1c3, c12c2, c22, c13

        i = excit%from_a(1)
        j = excit%from_b(1)
        k = excit%from_b(2)
        l = excit%from_b(3)

        a = excit%to_a(1)
        b = excit%to_b(1)
        c = excit%to_b(2)
        d = excit%to_b(3)

        associate(c1_a=>c_vec%o1_a, c1_b=>c_vec%o1_b, &
                c2_aa=>c_vec%o2_aa, c2_ab=>c_vec%o2_ab, c2_bb=>c_vec%o2_bb, &
                c3_aaa=>c_vec%o3_aaa, c3_aab=>c_vec%o3_aab, &
                c3_abb=>c_vec%o3_abb, c3_bbb=>c_vec%o3_bbb)

            t4 = (c4 &
                -c1_a(a,i)*c3_bbb(b,c,d,j,k,l) &
                -c1_b(d,l)*c3_abb(a,b,c,i,j,k) &
                +c1_b(d,j)*c3_abb(a,b,c,i,l,k) &
                +c1_b(d,k)*c3_abb(a,b,c,i,j,l) &
                +c1_b(b,l)*c3_abb(a,d,c,i,j,k) &
                +c1_b(c,l)*c3_abb(a,b,d,i,j,k) &
                -c1_b(b,j)*c3_abb(a,d,c,i,l,k) &
                -c1_b(c,j)*c3_abb(a,b,d,i,l,k) &
                -c1_b(b,k)*c3_abb(a,d,c,i,j,l) &
                -c1_b(c,k)*c3_abb(a,b,d,i,j,l) &
                +2*(c1_b(c,k)*c1_b(d,l)*c2_ab(a,b,i,j) &
                -c1_b(c,j)*c1_b(d,l)*c2_ab(a,b,i,k) &
                -c1_b(c,k)*c1_b(d,j)*c2_ab(a,b,i,l) &
                -c1_b(c,l)*c1_b(d,k)*c2_ab(a,b,i,j) &
                -c1_b(b,k)*c1_b(d,l)*c2_ab(a,c,i,j) &
                -c1_b(c,k)*c1_b(b,l)*c2_ab(a,d,i,j) &
                +c1_b(c,l)*c1_b(d,j)*c2_ab(a,b,i,k) &
                +c1_b(c,j)*c1_b(d,k)*c2_ab(a,b,i,l) &
                +c1_b(b,j)*c1_b(d,l)*c2_ab(a,c,i,k) &
                +c1_b(c,j)*c1_b(b,l)*c2_ab(a,d,i,k) &
                +c1_b(b,k)*c1_b(d,j)*c2_ab(a,c,i,l) &
                +c1_b(c,k)*c1_b(b,j)*c2_ab(a,d,i,l) &
                +c1_b(b,l)*c1_b(d,k)*c2_ab(a,c,i,j) &
                +c1_b(c,l)*c1_b(b,k)*c2_ab(a,d,i,j) &
                -c1_b(b,l)*c1_b(d,j)*c2_ab(a,c,i,k) &
                -c1_b(c,l)*c1_b(b,j)*c2_ab(a,d,i,k) &
                -c1_b(b,j)*c1_b(d,k)*c2_ab(a,c,i,l) &
                -c1_b(c,j)*c1_b(b,k)*c2_ab(a,d,i,l) &
                +c1_a(a,i)*c1_b(b,j)*c2_bb(c,d,k,l) &
                -c1_a(a,i)*c1_b(b,k)*c2_bb(c,d,j,l) &
                -c1_a(a,i)*c1_b(b,l)*c2_bb(c,d,k,j) &
                -c1_a(a,i)*c1_b(c,j)*c2_bb(b,d,k,l) &
                -c1_a(a,i)*c1_b(d,j)*c2_bb(c,b,k,l) &
                +c1_a(a,i)*c1_b(c,k)*c2_bb(b,d,j,l) &
                +c1_a(a,i)*c1_b(d,k)*c2_bb(c,b,j,l) &
                +c1_a(a,i)*c1_b(c,l)*c2_bb(b,d,k,j) &
                +c1_a(a,i)*c1_b(d,l)*c2_bb(c,b,k,j)) &
                -c2_ab(a,b,i,j)*c2_bb(c,d,k,l) &
                +c2_ab(a,b,i,k)*c2_bb(c,d,j,l) &
                +c2_ab(a,b,i,l)*c2_bb(c,d,k,j) &
                +c2_ab(a,c,i,j)*c2_bb(b,d,k,l) &
                +c2_ab(a,d,i,j)*c2_bb(c,b,k,l) &
                -c2_ab(a,c,i,k)*c2_bb(b,d,j,l) &
                -c2_ab(a,d,i,k)*c2_bb(c,b,j,l) &
                -c2_ab(a,c,i,l)*c2_bb(b,d,k,j) &
                -c2_ab(a,d,i,l)*c2_bb(c,b,k,j) &
                -6*(c1_a(a,i)*c1_b(b,j)*c1_b(c,k)*c1_b(d,l) &
                -c1_a(a,i)*c1_b(b,k)*c1_b(c,j)*c1_b(d,l) &
                -c1_a(a,i)*c1_b(b,l)*c1_b(c,k)*c1_b(d,j) &
                -c1_a(a,i)*c1_b(b,j)*c1_b(c,l)*c1_b(d,k) &
                +c1_a(a,i)*c1_b(b,k)*c1_b(c,l)*c1_b(d,j) &
                +c1_a(a,i)*c1_b(b,l)*c1_b(c,j)*c1_b(d,k)))

        end associate

    end subroutine analyze_t4_abbb

    subroutine analyze_t4_bbbb(c_vec, excit, c4, t4)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use const, only: dp
        use excitations, only: excit_t
        use ext_cor_types, only: vec3_t

        ! CI coefficients
        type(vec3_t), intent(in) :: c_vec
        real(dp), intent(in) :: c4
        type(excit_t), intent(in) :: excit

        ! T amplitudes
        real(dp), intent(inout) :: t4

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        ! Place holders
        real(dp) :: c1c3, c12c2, c22, c13

        i = excit%from_b(1)
        j = excit%from_b(2)
        k = excit%from_b(3)
        l = excit%from_b(4)

        a = excit%to_b(1)
        b = excit%to_b(2)
        c = excit%to_b(3)
        d = excit%to_b(4)

        associate(c1_a=>c_vec%o1_a, c1_b=>c_vec%o1_b, &
                c2_aa=>c_vec%o2_aa, c2_ab=>c_vec%o2_ab, c2_bb=>c_vec%o2_bb, &
                c3_aaa=>c_vec%o3_aaa, c3_aab=>c_vec%o3_aab, &
                c3_abb=>c_vec%o3_abb, c3_bbb=>c_vec%o3_bbb)

            t4 = &
                (c4 &
                -c1_b(a,i)*c3_bbb(b,c,d,j,k,l) &
                +c1_b(a,j)*c3_bbb(b,c,d,i,k,l) &
                +c1_b(a,k)*c3_bbb(b,c,d,j,i,l) &
                +c1_b(a,l)*c3_bbb(b,c,d,j,k,i) &
                +c1_b(b,i)*c3_bbb(a,c,d,j,k,l) &
                +c1_b(c,i)*c3_bbb(b,a,d,j,k,l) &
                +c1_b(d,i)*c3_bbb(b,c,a,j,k,l) &
                -c1_b(b,j)*c3_bbb(a,c,d,i,k,l) &
                -c1_b(c,j)*c3_bbb(b,a,d,i,k,l) &
                -c1_b(d,j)*c3_bbb(b,c,a,i,k,l) &
                -c1_b(b,k)*c3_bbb(a,c,d,j,i,l) &
                -c1_b(c,k)*c3_bbb(b,a,d,j,i,l) &
                -c1_b(d,k)*c3_bbb(b,c,a,j,i,l) &
                -c1_b(b,l)*c3_bbb(a,c,d,j,k,i) &
                -c1_b(c,l)*c3_bbb(b,a,d,j,k,i) &
                -c1_b(d,l)*c3_bbb(b,c,a,j,k,i) &
                +2*(c1_b(a,i)*c1_b(b,j)*c2_bb(c,d,k,l) & !1
                -c1_b(a,k)*c1_b(b,j)*c2_bb(c,d,i,l) & !(ik)
                -c1_b(a,l)*c1_b(b,j)*c2_bb(c,d,k,i) & !(il)
                -c1_b(a,j)*c1_b(b,i)*c2_bb(c,d,k,l) & !(ij)
                -c1_b(a,i)*c1_b(b,k)*c2_bb(c,d,j,l) & !(jk)
                -c1_b(a,i)*c1_b(b,l)*c2_bb(c,d,k,j) & !(jl)
                -c1_b(c,i)*c1_b(b,j)*c2_bb(a,d,k,l) & !(ac)
                -c1_b(d,i)*c1_b(b,j)*c2_bb(c,a,k,l) & !(ad)
                -c1_b(a,i)*c1_b(c,j)*c2_bb(b,d,k,l) & !(bc)
                -c1_b(a,i)*c1_b(d,j)*c2_bb(c,b,k,l) & !(bd)
                +c1_b(c,i)*c1_b(d,j)*c2_bb(a,b,k,l) & !(ac)(bd)
                +c1_b(c,k)*c1_b(b,j)*c2_bb(a,d,i,l) & !(ac)(ik)
                +c1_b(d,k)*c1_b(b,j)*c2_bb(c,a,i,l) & !(ad)(ik)
                +c1_b(a,k)*c1_b(c,j)*c2_bb(b,d,i,l) & !(bc)(ik)
                +c1_b(a,k)*c1_b(d,j)*c2_bb(c,b,i,l) & !(bd)(ik)
                +c1_b(c,l)*c1_b(b,j)*c2_bb(a,d,k,i) & !(ac)(il)
                +c1_b(d,l)*c1_b(b,j)*c2_bb(c,a,k,i) & !(ad)(il)
                +c1_b(a,l)*c1_b(c,j)*c2_bb(b,d,k,i) & !(bc)(il)
                +c1_b(a,l)*c1_b(d,j)*c2_bb(c,b,k,i) & !(bd)(il)
                +c1_b(c,j)*c1_b(b,i)*c2_bb(a,d,k,l) & !(ac)(ij)
                +c1_b(d,j)*c1_b(b,i)*c2_bb(c,a,k,l) & !(ad)(ij)
                +c1_b(a,j)*c1_b(c,i)*c2_bb(b,d,k,l) & !(bc)(ij)
                +c1_b(a,j)*c1_b(d,i)*c2_bb(c,b,k,l) & !(bd)(ij)
                +c1_b(c,i)*c1_b(b,k)*c2_bb(a,d,j,l) & !(ac)(jk)
                +c1_b(d,i)*c1_b(b,k)*c2_bb(c,a,j,l) & !(ad)(jk)
                +c1_b(a,i)*c1_b(c,k)*c2_bb(b,d,j,l) & !(bc)(jk)
                +c1_b(a,i)*c1_b(d,k)*c2_bb(c,b,j,l) & !(bd)(jk)
                +c1_b(c,i)*c1_b(b,l)*c2_bb(a,d,k,j) & !(ac)(jl)
                +c1_b(d,i)*c1_b(b,l)*c2_bb(c,a,k,j) & !(ad)(jl)
                +c1_b(a,i)*c1_b(c,l)*c2_bb(b,d,k,j) & !(bc)(jl)
                +c1_b(a,i)*c1_b(d,l)*c2_bb(c,b,k,j) & !(bd)(jl)
                +c1_b(a,j)*c1_b(b,k)*c2_bb(c,d,i,l) & !(ijk)
                +c1_b(a,k)*c1_b(b,i)*c2_bb(c,d,j,l) & !(ikj)
                +c1_b(a,j)*c1_b(b,l)*c2_bb(c,d,k,i) & !(ijl)
                +c1_b(a,l)*c1_b(b,i)*c2_bb(c,d,k,j) & !(ilj)
                -c1_b(a,l)*c1_b(b,k)*c2_bb(c,d,i,j) & !(kilj)
                -c1_b(c,k)*c1_b(d,j)*c2_bb(a,b,i,l) & !(ac)(bd)(ik)
                -c1_b(c,l)*c1_b(d,j)*c2_bb(a,b,k,i) & !(ac)(bd)(il)
                -c1_b(c,j)*c1_b(d,i)*c2_bb(a,b,k,l) & !(ac)(bd)(ij)
                -c1_b(c,i)*c1_b(d,k)*c2_bb(a,b,j,l) & !(ac)(bd)(jk)
                -c1_b(c,i)*c1_b(d,l)*c2_bb(a,b,k,j) & !(ac)(bd)(jl)
                -c1_b(c,j)*c1_b(b,k)*c2_bb(a,d,i,l) & !(ac)(ijk)
                -c1_b(d,j)*c1_b(b,k)*c2_bb(c,a,i,l) & !(ad)(ijk)
                -c1_b(a,j)*c1_b(c,k)*c2_bb(b,d,i,l) & !(bc)(ijk)
                -c1_b(a,j)*c1_b(d,k)*c2_bb(c,b,i,l) & !(bd)(ijk)
                -c1_b(c,k)*c1_b(b,i)*c2_bb(a,d,j,l) & !(ac)(ikj)
                -c1_b(d,k)*c1_b(b,i)*c2_bb(c,a,j,l) & !(ad)(ikj)
                -c1_b(a,k)*c1_b(c,i)*c2_bb(b,d,j,l) & !(bc)(ikj)
                -c1_b(a,k)*c1_b(d,i)*c2_bb(c,b,j,l) & !(bd)(ikj)
                -c1_b(c,j)*c1_b(b,l)*c2_bb(a,d,k,i) & !(ac)(ijl)
                -c1_b(d,j)*c1_b(b,l)*c2_bb(c,a,k,i) & !(ad)(ijl)
                -c1_b(a,j)*c1_b(c,l)*c2_bb(b,d,k,i) & !(bc)(ijl)
                -c1_b(a,j)*c1_b(d,l)*c2_bb(c,b,k,i) & !(bd)(ijl)
                -c1_b(c,l)*c1_b(b,i)*c2_bb(a,d,k,j) & !(ac)(ilj)
                -c1_b(d,l)*c1_b(b,i)*c2_bb(c,a,k,j) & !(ad)(ilj)
                -c1_b(a,l)*c1_b(c,i)*c2_bb(b,d,k,j) & !(bc)(ilj)
                -c1_b(a,l)*c1_b(d,i)*c2_bb(c,b,k,j) & !(bd)(ilj)
                +c1_b(c,j)*c1_b(d,k)*c2_bb(a,b,i,l) & !(ac)(bd)(ijk)
                +c1_b(c,k)*c1_b(d,i)*c2_bb(a,b,j,l) & !(ac)(bd)(ikj)
                +c1_b(c,j)*c1_b(d,l)*c2_bb(a,b,k,i) & !(ac)(bd)(ijl)
                +c1_b(c,l)*c1_b(d,i)*c2_bb(a,b,k,j) & !(ac)(bd)(ilj)
                +c1_b(c,l)*c1_b(b,k)*c2_bb(a,d,i,j) & !(ac)(kilj)
                +c1_b(d,l)*c1_b(b,k)*c2_bb(c,a,i,j) & !(ad)(kilj)
                +c1_b(a,l)*c1_b(c,k)*c2_bb(b,d,i,j) & !(bc)(kilj)
                +c1_b(a,l)*c1_b(d,k)*c2_bb(c,b,i,j) & !(bd)(kilj)
                -c1_b(c,l)*c1_b(d,k)*c2_bb(a,b,i,j) & !(ac)(bd)(kilj)
                -c1_b(c,k)*c1_b(b,l)*c2_bb(a,d,i,j) & !(ac)(ik)(jl)
                -c1_b(d,k)*c1_b(b,l)*c2_bb(c,a,i,j) & !(ad)(ik)(jl)
                -c1_b(a,k)*c1_b(c,l)*c2_bb(b,d,i,j) & !(bc)(ik)(jl)
                -c1_b(a,k)*c1_b(d,l)*c2_bb(c,b,i,j) & !(bd)(ik)(jl)
                +c1_b(c,k)*c1_b(d,l)*c2_bb(a,b,i,j) & !(ac)(bd)(ik)(jl)
                +c1_b(a,k)*c1_b(b,l)*c2_bb(c,d,i,j)) & !(ik)(jl)
                -c2_bb(a,b,i,j)*c2_bb(c,d,k,l) &
                +c2_bb(a,b,k,j)*c2_bb(c,d,i,l) &
                +c2_bb(a,b,l,j)*c2_bb(c,d,k,i) &
                +c2_bb(a,b,i,k)*c2_bb(c,d,j,l) &
                +c2_bb(a,b,i,l)*c2_bb(c,d,k,j) &
                +c2_bb(c,b,i,j)*c2_bb(a,d,k,l) &
                +c2_bb(d,b,i,j)*c2_bb(c,a,k,l) &
                -c2_bb(a,b,k,l)*c2_bb(c,d,i,j) &
                -c2_bb(c,b,k,j)*c2_bb(a,d,i,l) &
                -c2_bb(c,b,l,j)*c2_bb(a,d,k,i) &
                -c2_bb(c,b,i,k)*c2_bb(a,d,j,l) &
                -c2_bb(c,b,i,l)*c2_bb(a,d,k,j) &
                -c2_bb(d,b,k,j)*c2_bb(c,a,i,l) &
                -c2_bb(d,b,l,j)*c2_bb(c,a,k,i) &
                -c2_bb(d,b,i,k)*c2_bb(c,a,j,l) &
                -c2_bb(d,b,i,l)*c2_bb(c,a,k,j) &
                +c2_bb(c,b,k,l)*c2_bb(a,d,i,j) &
                +c2_bb(d,b,k,l)*c2_bb(c,a,i,j) &
                -6*(c1_b(a,i)*c1_b(b,j)*c1_b(c,k)*c1_b(d,l) &
                -c1_b(a,j)*c1_b(b,i)*c1_b(c,k)*c1_b(d,l) &
                -c1_b(a,k)*c1_b(b,j)*c1_b(c,i)*c1_b(d,l) &
                -c1_b(a,l)*c1_b(b,j)*c1_b(c,k)*c1_b(d,i) &
                -c1_b(a,i)*c1_b(b,k)*c1_b(c,j)*c1_b(d,l) &
                -c1_b(a,i)*c1_b(b,l)*c1_b(c,k)*c1_b(d,j) &
                -c1_b(a,i)*c1_b(b,j)*c1_b(c,l)*c1_b(d,k) &
                +c1_b(a,j)*c1_b(b,k)*c1_b(c,i)*c1_b(d,l) &
                +c1_b(a,k)*c1_b(b,i)*c1_b(c,j)*c1_b(d,l) &
                +c1_b(a,j)*c1_b(b,l)*c1_b(c,k)*c1_b(d,i) &
                +c1_b(a,l)*c1_b(b,i)*c1_b(c,k)*c1_b(d,j) &
                +c1_b(a,k)*c1_b(b,j)*c1_b(c,l)*c1_b(d,i) &
                +c1_b(a,l)*c1_b(b,j)*c1_b(c,i)*c1_b(d,k) &
                +c1_b(a,i)*c1_b(b,k)*c1_b(c,l)*c1_b(d,j) &
                +c1_b(a,i)*c1_b(b,l)*c1_b(c,j)*c1_b(d,k) &
                +c1_b(a,j)*c1_b(b,i)*c1_b(c,l)*c1_b(d,k) &
                +c1_b(a,k)*c1_b(b,l)*c1_b(c,i)*c1_b(d,j) &
                +c1_b(a,l)*c1_b(b,k)*c1_b(c,j)*c1_b(d,i) &
                -c1_b(a,j)*c1_b(b,k)*c1_b(c,l)*c1_b(d,i) &
                -c1_b(a,j)*c1_b(b,l)*c1_b(c,i)*c1_b(d,k) &
                -c1_b(a,k)*c1_b(b,l)*c1_b(c,j)*c1_b(d,i) &
                -c1_b(a,k)*c1_b(b,i)*c1_b(c,l)*c1_b(d,j) &
                -c1_b(a,l)*c1_b(b,i)*c1_b(c,j)*c1_b(d,k) &
                -c1_b(a,l)*c1_b(b,k)*c1_b(c,i)*c1_b(d,j)))

        end associate

    end subroutine analyze_t4_bbbb

end module cluster_analysis
