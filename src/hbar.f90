module hbar_gen

    ! This module contains code to generate and manage
    ! the similarity transformed Hamiltonian

    implicit none

contains

    subroutine hbar2(sys, cc)

        use const, only: p
        use printing, only: io, print_date
        use errors, only: stop_all
        use system, only: sys_t
        use cc_types, only: cc_t, init_hbar

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout), target :: cc

        real(p), pointer :: t(:) => null()

        integer :: n0, n1, n2, n3
        integer :: k1, k2, k3, k4
        integer :: k1a, k1b, k2a, k2b, k2c
        integer :: i, j, k, l
        integer :: a, b, c, d

        real(p), allocatable :: v0a2a(:,:)
        real(p), allocatable :: v0a2c(:,:)
        real(p), allocatable :: v0a4a(:,:,:,:)
        real(p), allocatable :: v0a4c(:,:,:,:)
        real(p), allocatable :: v0a4e(:,:,:,:)
        real(p), allocatable :: v1a1a(:,:)
        real(p), allocatable :: v1a3a(:,:,:,:)
        real(p), allocatable :: v1a3c(:,:,:,:)
        real(p), allocatable :: v1b1b(:,:)
        real(p), allocatable :: v1b3b(:,:,:,:)
        real(p), allocatable :: v1b3d(:,:,:,:)
        real(p), allocatable :: v2a0a(:,:)
        real(p), allocatable :: v2a2a(:,:,:,:)
        real(p), allocatable :: v2a2c(:,:,:,:)
        real(p), allocatable :: v2b2b(:,:,:,:)
        real(p), allocatable :: v2c0a(:,:)
        real(p), allocatable :: v2c2a(:,:,:,:)
        real(p), allocatable :: v2c2c(:,:,:,:)
        real(p), allocatable :: v3a1a(:,:,:,:)
        real(p), allocatable :: v3b1b(:,:,:,:)
        real(p), allocatable :: v3c1a(:,:,:,:)
        real(p), allocatable :: v3d1b(:,:,:,:)
        real(p), allocatable :: v4a0a(:,:,:,:)
        real(p), allocatable :: v4c0a(:,:,:,:)
        real(p), allocatable :: v4e0a(:,:,:,:)

        ! Compatibility layer
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

        ! Load T1 and T2
        ! cc%pos(6) corresponds to the beginning of T3A
        t => cc%t_vec(1:cc%pos(6)-1)

        call print_date('  (H_N e^(T_1 + T_2))_C generation started on')

        ! Initialize hbar arrays. We don't need 3-body HBar
        call init_hbar(sys, cc, 2)

        associate(fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
                intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)

            allocate(V0A2A(N0+1:N1,N0+1:N1))
            V0A2A=0.0d0

            call HBar0A2A(N0,N1,N2,N3,V0A2A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    cc%hbar%a(j,i)=V0A2A(j,i)+FockR(j,i)
                enddo
            enddo
            deallocate(V0A2A)

            allocate(V1A1A(N1+1:N3,N0+1:N1))
            V1A1A=0.0d0

            call HBar1A1A(N0,N1,N2,N3,V1A1A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do a=N1+1,N3
                    cc%hbar%a(a,i)=V1A1A(a,i)+FockR(a,i)
                enddo
            enddo
            deallocate(V1A1A)

            allocate(V2A0A(N1+1:N3,N1+1:N3))
            V2A0A=0.0d0

            call HBar2A0A(N0,N1,N2,N3,V2A0A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do a=N1+1,N3
                do b=N1+1,N3
                    cc%hbar%a(b,a)=V2A0A(b,a)+FockR(b,a)
                enddo
            enddo
            deallocate(V2A0A)


            allocate(V0A2C(N0+1:N2,N0+1:N2))
            V0A2C=0.0d0

            call HBar0A2C(N0,N1,N2,N3,V0A2C, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do j=N0+1,N2
                    cc%hbar%b(j,i)=V0A2C(j,i)+FockB(j,i)
                enddo
            enddo
            deallocate(V0A2C)

            allocate(V1B1B(N2+1:N3,N0+1:N2))
            V1B1B=0.0d0

            call HBar1B1B(N0,N1,N2,N3,V1B1B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do a=N2+1,N3
                    cc%hbar%b(a,i)=V1B1B(a,i)+FockB(a,i)
                enddo
            enddo
            deallocate(V1B1B)

            allocate(V2C0A(N2+1:N3,N2+1:N3))
            V2C0A=0.0d0

            call HBar2C0A(N0,N1,N2,N3,V2C0A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do a=N2+1,N3
                do b=N2+1,N3
                    cc%hbar%b(b,a)=V2C0A(b,a)+FockB(b,a)
                enddo
            enddo
            deallocate(V2C0A)

            write(io, '(a)') '    One-body matrix done'

            allocate(V0A4A(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
            V0A4A=0.0d0

            call HBar0A4A(N0,N1,N2,N3,V0A4A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do k=N0+1,N1
                        do l=N0+1,N1
                            cc%hbar%aa(j,i,l,k)=V0A4A(l,k,j,i)+IntR(j,i,l,k)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V0A4A)

            allocate(V1A3A(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            V1A3A=0.0d0

            call HBar1A3A0(N0,N1,N2,N3,V1A3A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do k=N0+1,N1
                        do a=N1+1,N3
                            cc%hbar%aa(a,i,k,j)=V1A3A(a,k,j,i)+IntR(a,i,k,j)
                            cc%hbar%aa(i,a,k,j)=-cc%hbar%aa(a,i,k,j)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1A3A)

            allocate(V1A3A(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            V1A3A=0.0d0

            call HBar1A3A(N0,N1,N2,N3,V1A3A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do k=N0+1,N1
                        do a=N1+1,N3
                            cc%hbar%aa(k,j,i,a)=V1A3A(a,k,j,i)+IntR(k,j,i,a)
                            cc%hbar%aa(k,j,a,i)=-cc%hbar%aa(k,j,i,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1A3A)

            do i=N0+1,N1
                do j=N0+1,N1
                    do a=N1+1,N3
                        do b=N1+1,N3
                            cc%hbar%aa(b,a,j,i)=IntR(b,a,j,i)
                        enddo
                    enddo
                enddo
            enddo

            allocate(V2A2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            V2A2A=0.0d0

            call HBar2A2A(N0,N1,N2,N3,V2A2A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do a=N1+1,N3
                        do b=N1+1,N3
                            cc%hbar%aa(b,i,j,a)=V2A2A(b,a,j,i)+IntR(b,i,j,a)
                            cc%hbar%aa(b,i,a,j)=-cc%hbar%aa(b,i,j,a)
                            cc%hbar%aa(i,b,j,a)=-cc%hbar%aa(b,i,j,a)
                            cc%hbar%aa(i,b,a,j)=cc%hbar%aa(b,i,j,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V2A2A)

            allocate(V3A1A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
            V3A1A=0.0d0

            call HBar3A1A0(N0,N1,N2,N3,V3A1A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do a=N1+1,N3
                    do b=N1+1,N3
                        do c=N1+1,N3
                            cc%hbar%aa(c,b,i,a)=V3A1A(c,b,a,i)+IntR(c,b,i,a)
                            cc%hbar%aa(c,b,a,i)=-cc%hbar%aa(c,b,i,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3A1A)

            allocate(V3A1A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
            V3A1A=0.0d0

            call HBar3A1A(N0,N1,N2,N3,V3A1A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do a=N1+1,N3
                    do b=N1+1,N3
                        do c=N1+1,N3
                            cc%hbar%aa(b,i,c,a)=V3A1A(c,b,a,i)+IntR(b,i,c,a)
                            cc%hbar%aa(i,b,c,a)=-cc%hbar%aa(b,i,c,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3A1A)

            allocate(V4A0A(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
            V4A0A=0.0d0

            call HBar4A0A(N0,N1,N2,N3,V4A0A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do a=N1+1,N3
                do b=N1+1,N3
                    do c=N1+1,N3
                        do d=N1+1,N3
                            cc%hbar%aa(d,c,b,a)=V4A0A(d,c,b,a)+IntR(d,c,b,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V4A0A)

            allocate(V0A4C(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
            V0A4C=0.0d0

            call HBar0A4C(N0,N1,N2,N3,V0A4C, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do k=N0+1,N2
                        do l=N0+1,N2
                            cc%hbar%ab(k,i,l,j)=V0A4C(l,k,j,i)+IntM(k,i,l,j)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V0A4C)

            allocate(V1A3C(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
            V1A3C=0.0d0

            call HBar1A3C0(N0,N1,N2,N3,V1A3C, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N2
                    do k=N0+1,N2
                        do a=N1+1,N3
                            cc%hbar%ab(j,a,k,i)=V1A3C(a,k,j,i)+IntM(j,a,k,i)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1A3C)

            allocate(V1A3C(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
            V1A3C=0.0d0

            call HBar1A3C(N0,N1,N2,N3,V1A3C, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N2
                    do k=N0+1,N2
                        do a=N1+1,N3
                            cc%hbar%ab(j,i,k,a)=V1A3C(a,k,j,i)+IntM(j,i,k,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1A3C)

            allocate(V1B3B(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            V1B3B=0.0d0

            call HBar1B3B0(N0,N1,N2,N3,V1B3B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do k=N0+1,N2
                        do a=N2+1,N3
                            cc%hbar%ab(a,i,k,j)=V1B3B(a,k,j,i)+IntM(a,i,k,j)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1B3B)

            allocate(V1B3B(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            V1B3B=0.0d0

            call HBar1B3B(N0,N1,N2,N3,V1B3B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do k=N0+1,N2
                        do a=N2+1,N3
                            cc%hbar%ab(k,i,a,j)=V1B3B(a,k,j,i)+IntM(k,i,a,j)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1B3B)

            allocate(V2A2C(N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
            V2A2C=0.0d0

            call HBar2A2C(N0,N1,N2,N3,V2A2C, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do j=N0+1,N2
                    do a=N1+1,N3
                        do b=N1+1,N3
                            cc%hbar%ab(j,b,i,a)=V2A2C(b,a,j,i)+IntM(j,b,i,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V2A2C)

            do i=N0+1,N1
                do j=N0+1,N2
                    do a=N1+1,N3
                        do b=N2+1,N3
                            cc%hbar%ab(b,a,j,i)=IntM(b,a,j,i)
                        enddo
                    enddo
                enddo
            enddo

            allocate(V2B2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            V2B2B=0.0d0

            call HBar2B2B0(N0,N1,N2,N3,V2B2B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N2
                    do a=N1+1,N3
                        do b=N2+1,N3
                            cc%hbar%ab(b,i,j,a)=V2B2B(b,a,j,i)+IntM(b,i,j,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V2B2B)

            allocate(V2B2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            V2B2B=0.0d0

            call HBar2B2B(N0,N1,N2,N3,V2B2B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N2
                    do a=N1+1,N3
                        do b=N2+1,N3
                            cc%hbar%ab(j,a,b,i)=V2B2B(b,a,j,i)+IntM(j,a,b,i)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V2B2B)

            allocate(V2C2A(N2+1:N3,N2+1:N3,N0+1:N1,N0+1:N1))
            V2C2A=0.0d0

            call HBar2C2A(N0,N1,N2,N3,V2C2A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do j=N0+1,N1
                    do a=N2+1,N3
                        do b=N2+1,N3
                            cc%hbar%ab(a,i,b,j)=V2C2A(b,a,j,i)+IntM(a,i,b,j)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V2C2A)

            allocate(V3B1B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
            V3B1B=0.0d0

            call HBar3B1B0(N0,N1,N2,N3,V3B1B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do a=N1+1,N3
                    do b=N1+1,N3
                        do c=N2+1,N3
                            cc%hbar%ab(c,b,i,a)=V3B1B(c,b,a,i)+IntM(c,b,i,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3B1B)

            allocate(V3B1B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
            V3B1B=0.0d0

            call HBar3B1B(N0,N1,N2,N3,V3B1B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do a=N1+1,N3
                    do b=N1+1,N3
                        do c=N2+1,N3
                            cc%hbar%ab(i,b,c,a)=V3B1B(c,b,a,i)+IntM(i,b,c,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3B1B)

            allocate(V3C1A(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
            V3C1A=0.0d0

            call HBar3C1A0(N0,N1,N2,N3,V3C1A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do a=N1+1,N3
                    do b=N2+1,N3
                        do c=N2+1,N3
                            cc%hbar%ab(c,a,b,i)=V3C1A(c,b,a,i)+IntM(c,a,b,i)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3C1A)

            allocate(V3C1A(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
            V3C1A=0.0d0

            call HBar3C1A(N0,N1,N2,N3,V3C1A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N1
                do a=N1+1,N3
                    do b=N2+1,N3
                        do c=N2+1,N3
                            cc%hbar%ab(b,i,c,a)=V3C1A(c,b,a,i)+IntM(b,i,c,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3C1A)

            allocate(V4C0A(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
            V4C0A=0.0d0
            call HBar4C0A(N0,N1,N2,N3,V4C0A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do a=N1+1,N3
                do b=N1+1,N3
                    do c=N2+1,N3
                        do d=N2+1,N3
                            cc%hbar%ab(d,b,c,a)=V4C0A(d,c,b,a)+IntM(d,b,c,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V4C0A)

            allocate(V0A4E(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
            V0A4E=0.0d0

            call HBar0A4E(N0,N1,N2,N3,V0A4E, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do j=N0+1,N2
                    do k=N0+1,N2
                        do l=N0+1,N2
                            cc%hbar%bb(j,i,l,k)=V0A4E(l,k,j,i)+IntB(j,i,l,k)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V0A4E)

            allocate(V1B3D(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            V1B3D=0.0d0

            call HBar1B3D0(N0,N1,N2,N3,V1B3D, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do j=N0+1,N2
                    do k=N0+1,N2
                        do a=N2+1,N3
                            cc%hbar%bb(a,i,k,j)=V1B3D(a,k,j,i)+IntB(a,i,k,j)
                            cc%hbar%bb(i,a,k,j)=-cc%hbar%bb(a,i,k,j)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1B3D)

            allocate(V1B3D(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            V1B3D=0.0d0

            call HBar1B3D(N0,N1,N2,N3,V1B3D, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do j=N0+1,N2
                    do k=N0+1,N2
                        do a=N2+1,N3
                            cc%hbar%bb(k,j,i,a)=V1B3D(a,k,j,i)+IntB(k,j,i,a)
                            cc%hbar%bb(k,j,a,i)=-cc%hbar%bb(k,j,i,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V1B3D)

            do i=N0+1,N2
                do j=N0+1,N2
                    do a=N2+1,N3
                        do b=N2+1,N3
                            cc%hbar%bb(b,a,j,i)=IntB(b,a,j,i)
                        enddo
                    enddo
                enddo
            enddo

            allocate(V2C2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            V2C2C=0.0d0

            call HBar2C2C(N0,N1,N2,N3,V2C2C, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do j=N0+1,N2
                    do a=N2+1,N3
                        do b=N2+1,N3
                            cc%hbar%bb(b,i,j,a)=V2C2C(b,a,j,i)+IntB(b,i,j,a)
                            cc%hbar%bb(b,i,a,j)=-cc%hbar%bb(b,i,j,a)
                            cc%hbar%bb(i,b,j,a)=-cc%hbar%bb(b,i,j,a)
                            cc%hbar%bb(i,b,a,j)=cc%hbar%bb(b,i,j,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V2C2C)

            allocate(V3D1B(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
            V3D1B=0.0d0

            call HBar3D1B0(N0,N1,N2,N3,V3D1B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do a=N2+1,N3
                    do b=N2+1,N3
                        do c=N2+1,N3
                            cc%hbar%bb(c,b,i,a)=V3D1B(c,b,a,i)+IntB(c,b,i,a)
                            cc%hbar%bb(c,b,a,i)=-cc%hbar%bb(c,b,i,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3D1B)

            allocate(V3D1B(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
            V3D1B=0.0d0

            call HBar3D1B(N0,N1,N2,N3,V3D1B, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do i=N0+1,N2
                do a=N2+1,N3
                    do b=N2+1,N3
                        do c=N2+1,N3
                            cc%hbar%bb(b,i,c,a)=V3D1B(c,b,a,i)+IntB(b,i,c,a)
                            cc%hbar%bb(i,b,c,a)=-cc%hbar%bb(b,i,c,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V3D1B)

            allocate(V4E0A(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
            V4E0A=0.0d0

            call HBar4E0A(N0,N1,N2,N3,V4E0A, &
                K1,K2,K3,K4, &
                FockR,FockB,IntR,IntB,IntM, &
                t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
            do a=N2+1,N3
                do b=N2+1,N3
                    do c=N2+1,N3
                        do d=N2+1,N3
                            cc%hbar%bb(d,c,b,a)=V4E0A(d,c,b,a)+IntB(d,c,b,a)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(V4E0A)

            call print_date('  (H_N e^(T_1 + T_2))_C generation ended on')

        end associate
    end subroutine hbar2

    subroutine add_twobody_hbar(sys, cc)

        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc

        cc%hbar%aa(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) = &
            sys%ints%v_aa(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a)

        cc%hbar%ab(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) = &
            sys%ints%v_ab(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a)

        cc%hbar%bb(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) = &
            sys%ints%v_bb(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b)

    end subroutine add_twobody_hbar

end module hbar_gen
