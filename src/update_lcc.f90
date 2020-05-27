module update_lcc

    ! Module holding the update routines analog to
    ! update_cc, but for the lambda  CC state

    implicit none

contains

    subroutine update_l2(sys, run, cc)

        use const, only: p
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        real(p), pointer :: t(:) => null()
        real(p), pointer :: l(:) => null()
        real(p), pointer :: lh(:) => null()

        integer :: n0, n1, n2 ,n3
        integer :: k1, k2, k3, k4
        integer :: k1a, k1b, k2a, k2b, k2c
        integer :: iroot

        iroot = 0

        n0 = sys%froz
        n1 = sys%occ_a
        n2 = sys%occ_b
        n3 = sys%orbs
        k1 = n1 - n0
        k2 = n2 - n0
        k3 = n3 - n1
        k4 = n3 - n2

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)

        t => cc%t_vec
        l => cc%l_vec
        lh => cc%lh_vec

        lh = 0.0_p

        associate(fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
                intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab, &
                h1a=>cc%hbar%a, h1b=>cc%hbar%b, h2a=>cc%hbar%aa, h2b=>cc%hbar%ab, &
                h2c=>cc%hbar%bb)

            call compute_l_hbar_disconnected(N0,N1,N2,N3,iroot, &
                H1A,H1B,H2A,H2B,H2C, &
                l(K1A:K1B-1),l(K1B:K2A-1),l(K2A:K2B-1),l(K2B:K2C-1),l(K2C:cc%l_size), &
                LH(K1A:K1B-1),LH(K1B:K2A-1),LH(K2A:K2B-1),LH(K2B:K2C-1),LH(K2C:cc%l_size))

            call L1A_update(N0,N1,N2,N3,LH(K1A), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L1B_update(N0,N1,N2,N3,LH(K1B), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L2A_update(N0,N1,N2,N3,LH(K2A), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L2B_update(N0,N1,N2,N3,LH(K2B), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            call L2C_update(N0,N1,N2,N3,LH(K2C), &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                H1A,H1B,H2A,H2B,H2C, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
                l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

            !if(iroot.ne.0)LH=LH-ECor*L
            call left_jacobi_update(N0,N1,N2,N3,run%shift,H1A,H1B,cc%en_cor, &
                l(K1A:K1B-1),l(K1B:K2A-1),l(K2A:K2B-1),l(K2B:K2C-1),l(K2C:cc%l_size), &
                LH(K1A:K1B-1),LH(K1B:K2A-1),LH(K2A:K2B-1),LH(K2B:K2C-1),LH(K2C:cc%l_size))

        end associate

    end subroutine update_l2

    subroutine compute_l_hbar_disconnected(N0,N1,N2,N3,iroot, &
            H1A,H1B,H2A,H2B,H2C,l1A,l1B,l2A,l2B,l2C, &
            V1A,V1B,V2A,V2B,V2C)

        use const, only: p

        integer, intent(in) :: n0, n1, n2, n3
        integer, intent(in) :: iroot

        integer :: a, b
        integer :: i, j
        real(p) :: H1A(N0+1:N3,N0+1:N3)
        real(p) :: H1B(N0+1:N3,N0+1:N3)
        real(p) :: H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
        real(p) :: H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
        real(p) :: H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
        real(p) :: l1A(N1+1:N3,N0+1:N1)
        real(p) :: l1B(N2+1:N3,N0+1:N2)
        real(p) :: l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
        real(p) :: l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
        real(p) :: l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
        real(p) :: V1A(N1+1:N3,N0+1:N1)
        real(p) :: V1B(N2+1:N3,N0+1:N2)
        real(p) :: V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
        real(p) :: V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
        real(p) :: V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)

        do b=N1+1,N3
            do a=N1+1,N3
                do i=N0+1,N1
                    do j=N0+1,N1
                        V2A(b,a,j,i)= &
                            -H1A(b,i)*l1A(a,j) &
                            +H1A(a,i)*l1A(b,j) &
                            +H1A(b,j)*l1A(a,i) &
                            -H1A(a,j)*l1A(b,i)
                    enddo
                enddo
            enddo
        enddo

        do i=N0+1,N1
            do j=N0+1,N2
                do a=N1+1,N3
                    do b=N2+1,N3
                        V2B(b,a,j,i)=+H1B(b,j)*l1A(a,i) &
                            +H1A(a,i)*l1B(b,j)
                    enddo
                enddo
            enddo
        enddo

        do i=N0+1,N2
            do j=N0+1,N2
                do a=N2+1,N3
                    do b=N2+1,N3
                        V2C(b,a,j,i)=-H1B(b,i)*l1B(a,j) &
                            +H1B(a,i)*l1B(b,j) &
                            +H1B(b,j)*l1B(a,i) &
                            -H1B(a,j)*l1B(b,i)
                    enddo
                enddo
            enddo
        enddo

        if (iroot /= 0) return

        V1A(n1+1:n3, n0+1:n1) = H1A(n1+1:n3, n0+1:n1)
        V1B(n1+1:n3, n0+1:n1) = H1B(n1+1:n3, n0+1:n1)

        do i=N0+1,N1
            do j=N0+1,N1
                do a=N1+1,N3
                    do b=N1+1,N3
                        V2A(b,a,j,i)=V2A(b,a,j,i)+H2A(b,a,j,i)
                    enddo
                enddo
            enddo
        enddo

        do i=N0+1,N1
            do j=N0+1,N2
                do a=N1+1,N3
                    do b=N2+1,N3
                        V2B(b,a,j,i)=V2B(b,a,j,i)+H2B(b,a,j,i)
                    enddo
                enddo
            enddo
        enddo

        do i=N0+1,N2
            do j=N0+1,N2
                do a=N2+1,N3
                    do b=N2+1,N3
                        V2C(b,a,j,i)=V2C(b,a,j,i)+H2C(b,a,j,i)
                    enddo
                enddo
            enddo
        enddo

    end subroutine compute_l_hbar_disconnected

    subroutine left_jacobi_update(N0,N1,N2,N3,shift,H1A,H1B,ECor, &
            l1A,l1B,l2A,l2B,l2C, &
            V1A,V1B,V2A,V2B,V2C)

        use const, only: p


        integer, intent(in) :: n0, n1, n2, n3

        integer :: a, b
        integer :: i, j
        real(p) :: shift,PP,ECor
        real(p) :: H1A(N0+1:N3,N0+1:N3)
        real(p) :: H1B(N0+1:N3,N0+1:N3)
        real(p) :: l1A(N1+1:N3,N0+1:N1)
        real(p) :: l1B(N2+1:N3,N0+1:N2)
        real(p) :: l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
        real(p) :: l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
        real(p) :: l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
        real(p) :: V1A(N1+1:N3,N0+1:N1)
        real(p) :: V1B(N2+1:N3,N0+1:N2)
        real(p) :: V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
        real(p) :: V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
        real(p) :: V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)


        do i=N0+1,N1
            do a=N1+1,N3
                PP=H1A(a,a)-H1A(i,i)-ECor+shift
                l1A(a,i)=l1A(a,i)-V1A(a,i)/PP
            enddo
        enddo

        do i=N0+1,N2
            do a=N2+1,N3
                PP=H1B(a,a)-H1B(i,i)-ECor+shift
                l1B(a,i)=l1B(a,i)-V1B(a,i)/PP
            enddo
        enddo
        do i=N0+1,N1
            do j=N0+1,N1
                do a=N1+1,N3
                    do b=N1+1,N3
                        PP=H1A(b,b)+H1A(a,a)-H1A(j,j)-H1A(i,i)-ECor+shift
                        l2A(b,a,j,i)= l2A(b,a,j,i)-V2A(b,a,j,i)/PP
                    enddo
                enddo
            enddo
        enddo

        do i=N0+1,N1
            do j=N0+1,N2
                do a=N1+1,N3
                    do b=N2+1,N3
                        PP=H1B(b,b)+H1A(a,a)-H1B(j,j)-H1A(i,i)-ECor+shift
                        l2B(b,a,j,i)= l2B(b,a,j,i)-V2B(b,a,j,i)/PP
                    enddo
                enddo
            enddo
        enddo

        do i=N0+1,N2
            do j=N0+1,N2
                do a=N2+1,N3
                    do b=N2+1,N3
                        PP=H1B(b,b)+H1B(a,a)-H1B(j,j)-H1B(i,i)-ECor+shift
                        l2C(b,a,j,i)= l2C(b,a,j,i)-V2C(b,a,j,i)/PP
                    enddo
                enddo
            enddo
        enddo

    end subroutine left_jacobi_update


end module update_lcc
