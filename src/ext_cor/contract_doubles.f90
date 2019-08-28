module contract_doubles_ext_cor

    use const, only: p

    implicit none

contains

    subroutine drive_doubles_contraction(sys, cc, v2a, v2b, v2c)

        use const, only: dp
        use cc_types, only: cc_t
        use system, only: sys_t

        use errors, only: stop_all
        use printing, only: io

        use utils, only: get_wall_time


        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in), target :: cc

        real(p), allocatable, intent(out) :: v2a(:,:,:,:)
        real(p), allocatable, intent(out) :: v2b(:,:,:,:)
        real(p), allocatable, intent(out) :: v2c(:,:,:,:)

        real(dp) :: start_time, end_time

        real(p), pointer :: t1a(:,:) => null()
        real(p), pointer :: t1b(:,:) => null()

        real(p), pointer :: t2a(:,:,:,:) => null()
        real(p), pointer :: t2b(:,:,:,:) => null()
        real(p), pointer :: t2c(:,:,:,:) => null()

        real(p), pointer :: t3a(:,:,:,:,:,:) => null()
        real(p), pointer :: t3b(:,:,:,:,:,:) => null()
        real(p), pointer :: t3c(:,:,:,:,:,:) => null()
        real(p), pointer :: t3d(:,:,:,:,:,:) => null()

        real(p) :: shift

        ! Compatibility vars
        ! [TODO] all this has to be removed
        integer :: n0, n1, n2 ,n3, m1, m2
        integer :: k1, k2, k3, k4
        integer :: k5, k6 ,k7, k8, k9, k0

        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d

        integer :: i, i0, i1

        shift = 0.0_p

        ! Compatibility layer
        n0 = sys%froz
        n1 = sys%occ_a
        n2 = sys%occ_b
        n3 = sys%orbs
        m1 = sys%act_occ_b
        m2 = sys%act_unocc_a

        ! K1 = # of occ alpha
        k1 = sys%occ_a - sys%froz
        ! K3 = # of unocc alpha
        k3 = sys%orbs - sys%occ_a
        ! K2 = # of occ beta
        k2 = sys%occ_b - sys%froz
        ! K4 = # of unocc beta
        k4 = sys%orbs - sys%occ_b

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)
        k3a = cc%pos(6)
        k3b = cc%pos(7)
        k3c = cc%pos(8)
        k3d = cc%pos(9)


        write(io, '(12x,a)') '=> Allocating arrays'
        t1a(sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(1):cc%pos(2)-1)

        t1b(sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b) &
            => cc%t_vec(cc%pos(2):cc%pos(3)-1)



        t2a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(3):cc%pos(4)-1)

        t2b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(4):cc%pos(5)-1)

        t2c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
            => cc%t_vec(cc%pos(5):cc%pos(6)-1)



        t3a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(6):cc%pos(7)-1)

        t3b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(7):cc%pos(8)-1)

        t3c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(8):cc%pos(9)-1)

        t3d(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
            => cc%t_vec(cc%pos(9):cc%pos(10)-1)

        associate(fahh=>sys%ints%fahh, fahp=>sys%ints%fahp, fapp=>sys%ints%fapp, &
                fbhh=>sys%ints%fbhh, fbhp=>sys%ints%fbhp, fbpp=>sys%ints%fbpp, &
                vahhhh=>sys%ints%vahhhh, vahhhp=>sys%ints%vahhhp, vahhpp=>sys%ints%vahhpp, &
                vahphp=>sys%ints%vahphp, vahppp=>sys%ints%vahppp, &
                vbhhhh=>sys%ints%vbhhhh, vbhhhp=>sys%ints%vbhhhp, vbhhph=>sys%ints%vbhhph, &
                vbhhpp=>sys%ints%vbhhpp, vbhphp=>sys%ints%vbhphp, vbhpph=>sys%ints%vbhpph, &
                vbphph=>sys%ints%vbphph, vbhppp=>sys%ints%vbhppp, vbphpp=>sys%ints%vbphpp, &
                vchhhh=>sys%ints%vchhhh, vchhhp=>sys%ints%vchhhp, vchhpp=>sys%ints%vchhpp, &
                vchphp=>sys%ints%vchphp, vchppp=>sys%ints%vchppp, &
                vaappp=>sys%ints%vaappp, vbappp=>sys%ints%vbappp, vbpapp=>sys%ints%vbpapp, &
                vcappp=>sys%ints%vcappp, &
                fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
                intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)


                allocate(V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
                V2A=0.0d0
                allocate(V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
                V2B=0.0d0
                allocate(V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
                V2C=0.0d0

                write(io, '(12x,a)') '=> <ijab| [V_N, (T4 - C4)]D | phi> alpha spin'
                start_time = get_wall_time()
                call t2A_disconnected(n0,n1,n2,n3, &
                    k1,k2,k3,k4,v2a,v2b,v2c, &
                    fockr, fockb, intr,intb,intm, &
                    t1a, t1b, t2a, t2b, t2c, &
                    t3a, t3b, t3c, t3d)
                end_time = get_wall_time()
                write(io, '(16x,a,f10.2,a)') '=> Took', end_time - start_time, ' seconds'

                write(io, '(12x,a)') '=> <ijab| [V_N, (T4 - C4)]D | phi> beta spin'
                start_time = get_wall_time()
                call t2B_disconnected(n0,n1,n2,n3, &
                    k1,k2,k3,k4,v2b,v2c, &
                    fockr, fockb, intr,intb,intm, &
                    t1a, t1b, t2a, t2b, t2c, &
                    t3a, t3b, t3c, t3d)
                end_time = get_wall_time()
                write(io, '(16x,a,f10.2,a)') '=> Took', end_time - start_time, ' seconds'

                write(io, '(12x,a)') '=> <ijab| (V_N, (T4 - C4))C | phi> alpha-alpha spin'
                start_time = get_wall_time()
                call t2A_update(N0,N1,N2,N3,V2A, &
                    K1,K2,K3,K4, &
                    FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,&
                    VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,&
                    VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,&
                    VBPHPH,VBHPPP,VBPHPP,&
                    VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,&
                    t1a, t1b, t2a, t2b, t2c, &
                    t3a, t3b, t3c, t3d)
                end_time = get_wall_time()
                write(io, '(16x,a,f10.2,a)') '=> Took', end_time - start_time, ' seconds'

                write(io, '(12x,a)') '=> <ijab| (V_N, (T4 - C4))C | phi> alpha-beta spin'
                start_time = get_wall_time()
                call t2B_update(N0,N1,N2,N3,V2B, &
                    K1,K2,K3,K4, &
                    FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,&
                    VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,&
                    VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,&
                    VBPHPH,VBHPPP,VBPHPP,&
                    VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,&
                    t1a, t1b, t2a, t2b, t2c, &
                    t3a, t3b, t3c, t3d)
                end_time = get_wall_time()
                write(io, '(16x,a,f10.2,a)') '=> Took', end_time - start_time, ' seconds'

                write(io, '(12x,a)') '=> <ijab| (V_N, (T4 - C4))C | phi> beta-beta spin'
                start_time = get_wall_time()
                call t2C_update(N0,N1,N2,N3,V2C, &
                    K1,K2,K3,K4, &
                    FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,&
                    VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,&
                    VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,&
                    VBPHPH,VBHPPP,VBPHPP,&
                    VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,&
                    t1a, t1b, t2a, t2b, t2c, &
                    t3a, t3b, t3c, t3d)
                end_time = get_wall_time()
                write(io, '(16x,a,f10.2,a)') '=> Took', end_time - start_time, ' seconds'

            end associate

        end subroutine drive_doubles_contraction


        subroutine t2A_update(N0,N1,N2,N3,V2A, &
                K1,K2,K3,K4,&
                FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
                VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
                VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
                VBPHPH,VBHPPP,VBPHPP, &
                VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
                t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D)

            use cc_utils, only: reorder_stripe, sum_stripe

            integer :: n0, n1, n2, n3
            integer :: k1, k2 ,k3, k4
            integer :: i1, i2, i3
            real(kind=8) :: FAHH(N0+1:N1,N0+1:N1)
            real(kind=8) :: FAHP(N1+1:N3,N0+1:N1)
            real(kind=8) :: FAPP(N1+1:N3,N1+1:N3)
            real(kind=8) :: FBHH(N0+1:N2,N0+1:N2)
            real(kind=8) :: FBHP(N2+1:N3,N0+1:N2)
            real(kind=8) :: FBPP(N2+1:N3,N2+1:N3)
            real(kind=8) :: VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1)
            real(kind=8) :: VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1)
            real(kind=8) :: VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3)
            real(kind=8) :: VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3)
            real(kind=8) :: VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2)
            real(kind=8) :: VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2)

            real(kind=8) :: t1A(N1+1:N3,N0+1:N1)
            real(kind=8) :: t1B(N2+1:N3,N0+1:N2)
            real(kind=8) :: t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) ::t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
            real(kind=8) :: t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
            real(kind=8) :: t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)

            real(kind=8) :: V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)

            real(kind=8),allocatable::B1(:,:)
            real(kind=8),allocatable::B2(:,:)
            real(kind=8),allocatable::C1(:,:,:)
            real(kind=8),allocatable::C2(:,:,:)
            real(kind=8),allocatable::D1(:,:,:,:)
            real(kind=8),allocatable::D2(:,:,:,:)
            real(kind=8),allocatable::F1(:,:,:,:,:,:)
            real(kind=8),allocatable::F2(:,:,:,:,:,:)

            real(kind=8), allocatable :: S1(:,:,:,:)
            real(kind=8), allocatable :: S25(:,:,:,:)
            real(kind=8), allocatable :: S29(:,:,:,:)
            real(kind=8), allocatable :: Q9(:,:)
            real(kind=8), allocatable :: S27(:,:,:,:)
            real(kind=8), allocatable :: S39(:,:,:,:)
            real(kind=8), allocatable :: Q3(:,:)
            real(kind=8), allocatable :: Q12(:,:)
            real(kind=8), allocatable :: Q5(:,:)
            real(kind=8), allocatable :: S14(:,:,:,:)
            real(kind=8), allocatable :: S32(:,:,:,:)
            real(kind=8), allocatable :: S17(:,:,:,:)
            real(kind=8), allocatable :: Q7(:,:)
            real(kind=8), allocatable :: Q8(:,:)
            real(kind=8), allocatable :: S21(:,:,:,:)
            real(kind=8), allocatable :: S23(:,:,:,:)
            real(kind=8), allocatable :: S3(:,:,:,:)
            real(kind=8), allocatable :: Q1(:,:)
            real(kind=8), allocatable :: Q10(:,:)
            real(kind=8), allocatable :: S8(:,:,:,:)
            real(kind=8), allocatable :: Q6(:,:)
            real(kind=8), allocatable :: S6(:,:,:,:)
            real(kind=8), allocatable :: S35(:,:,:,:)
            real(kind=8), allocatable :: Q11(:,:)
            real(kind=8), allocatable :: Q4(:,:)
            real(kind=8), allocatable :: Q2(:,:)
            real(kind=8), allocatable :: Z2(:,:,:,:)
            real(kind=8), allocatable :: X1(:,:,:,:)
            real(kind=8), allocatable :: Z26(:,:,:,:)
            real(kind=8), allocatable :: X2(:,:,:,:)
            real(kind=8), allocatable :: Z40(:,:,:,:)
            real(kind=8), allocatable :: Z30(:,:,:,:)
            real(kind=8), allocatable :: X3(:,:)
            real(kind=8), allocatable :: Z31(:,:,:,:)
            real(kind=8), allocatable :: Z28(:,:,:,:)
            real(kind=8), allocatable :: X4(:,:)
            real(kind=8), allocatable :: Z11(:,:,:,:)
            real(kind=8), allocatable :: X5(:,:)
            real(kind=8), allocatable :: Z38(:,:,:,:)
            real(kind=8), allocatable :: Z18(:,:,:,:)
            real(kind=8), allocatable :: Z22(:,:,:,:)
            real(kind=8), allocatable :: Z24(:,:,:,:)
            real(kind=8), allocatable :: X6(:,:,:,:)
            real(kind=8), allocatable :: Z4(:,:,:,:)
            real(kind=8), allocatable :: Z7(:,:,:,:)
            real(kind=8), allocatable :: Z36(:,:,:,:)
            real(kind=8), allocatable :: X7(:,:)
            real(kind=8), allocatable :: Z12(:,:,:,:)

            allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'1342',VAHHPP,D1)
            allocate(S1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
            I1=K3*K1*K1
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,D1,t1A,S1)
            deallocate(D1)

            allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'2341',S1,D1)
            allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(6,shape(t3A),size(t3A),'451236',t3A,F2)
            allocate(Z2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1*K3*K3
            I3=K3*K1*K1
            call EGEMM(I1,I2,I3,D1,F2,Z2)
            deallocate(D1)
            deallocate(F2)

            V2A=V2A-0.500*Z2
            call sum_stripe(4,shape(V2A),size(V2A),'1243', 0.500,V2A,Z2)
            deallocate(Z2)

            allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'4231',S1,D1)
            allocate(S25(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1*K1*K1
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,D1,t1A,S25)
            deallocate(D1)

            allocate(X1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
            X1=0.0d0
            call sum_stripe(4,shape(X1),size(X1),'3124', 1.000,X1,S25)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'2431',S1,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(S29(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1*K1
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S29)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S29),size(S29),'3124',S29,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Z30(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,D1,B2,Z30)
            deallocate(D1)
            deallocate(B2)

            call sum_stripe(4,shape(V2A),size(V2A),'2134', 1.000,V2A,Z30)
            call sum_stripe(4,shape(V2A),size(V2A),'2143',-1.000,V2A,Z30)
            deallocate(Z30)
            deallocate(S29)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'2431',S1,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q9(N0+1:N1,N0+1:N1))
            I1=K1*K1
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q9)
            deallocate(D1)
            deallocate(B2)

            allocate(X3(N0+1:N1,N0+1:N1))
            X3=0.0d0
            X3=X3+Q9
            deallocate(Q9)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'3421',S1,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(S27(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1*K1
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S27)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S27),size(S27),'3124',S27,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Z28(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,D1,B2,Z28)
            deallocate(D1)
            deallocate(B2)

            V2A=V2A+Z28
            call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,V2A,Z28)
            deallocate(Z28)
            deallocate(S27)

            allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S25),size(S25),'3214',S25,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(S39(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1*K1*K1
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,D1,B2,S39)
            deallocate(D1)
            deallocate(B2)

            allocate(X2(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            X2=0.0d0
            call sum_stripe(4,shape(X2),size(X2),'2134', 1.000,X2,S39)
            deallocate(S39)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3142',VBHHPP,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q3(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q3)
            deallocate(D1)
            deallocate(B2)

            allocate(X4(N0+1:N1,N1+1:N3))
            X4=0.0d0
            X4=X4+Q3

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q12(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,Q3,B2,Q12)
            deallocate(B2)

            allocate(X5(N1+1:N3,N1+1:N3))
            X5=0.0d0
            call sum_stripe(2,shape(X5),size(X5),'21', 1.000,X5,Q12)
            deallocate(Q12)

            allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'3421',VAHHPP,D1)
            allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(t2A),size(t2A),'3412',t2A,D2)
            allocate(Q5(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K3*K1*K1
            call EGEMM(I1,I2,I3,D1,D2,Q5)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X5),size(X5),'21',-0.500,X5,Q5)
            deallocate(Q5)

            allocate(S14(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1*K1
            I2=K1*K1
            I3=K3*K3
            call EGEMM(I1,I2,I3,VAHHPP,t2A,S14)

            call sum_stripe(4,shape(X1),size(X1),'3412',-0.500,X1,S14)

            allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(t2A),size(t2A),'3412',t2A,D2)
            allocate(Z26(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1
            I2=K3*K3
            I3=K1*K1
            call EGEMM(I1,I2,I3,X1,D2,Z26)
            deallocate(D2)

            V2A=V2A-0.500*Z26
            deallocate(Z26)
            deallocate(X1)

            allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S14),size(S14),'4312',S14,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(S32(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1*K1*K1
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,D1,B2,S32)
            deallocate(D1)
            deallocate(B2)

            call sum_stripe(4,shape(X2),size(X2),'2134',-0.500,X2,S32)
            deallocate(S32)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Z40(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,X2,B2,Z40)
            deallocate(B2)

            V2A=V2A-Z40
            deallocate(Z40)
            deallocate(X2)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'4231',VAHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(S17(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
            I1=K3*K1
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S17)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(S17),size(S17),'3412',S17,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(Z18(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
            I1=K1*K3
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,Z18)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,V2A,Z18)
            call sum_stripe(4,shape(V2A),size(V2A),'1324', 1.000,V2A,Z18)
            deallocate(Z18)
            deallocate(S17)

            allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3412',VBHHPP,D1)
            allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(t2B),size(t2B),'3412',t2B,D2)
            allocate(Q7(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K4*K1*K2
            call EGEMM(I1,I2,I3,D1,D2,Q7)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X5),size(X5),'21', 1.000,X5,Q7)
            deallocate(Q7)

            allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3124',VBHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(Q8(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3*K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Q8)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X3),size(X3),'21',-1.000,X3,Q8)
            deallocate(Q8)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4231',VBHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(S21(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
            I1=K4*K2
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S21)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(S21),size(S21),'3412',S21,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(Z22(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
            I1=K1*K3
            I2=K1*K3
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Z22)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(V2A),size(V2A),'2314',-1.000,V2A,Z22)
            call sum_stripe(4,shape(V2A),size(V2A),'1324', 1.000,V2A,Z22)
            call sum_stripe(4,shape(V2A),size(V2A),'2413', 1.000,V2A,Z22)
            call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,V2A,Z22)
            deallocate(Z22)
            deallocate(S21)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'4231',VCHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(S23(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
            I1=K4*K2
            I2=K1*K3
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S23)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(S23),size(S23),'3412',S23,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(Z24(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
            I1=K1*K3
            I2=K1*K3
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Z24)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,V2A,Z24)
            call sum_stripe(4,shape(V2A),size(V2A),'1324', 1.000,V2A,Z24)
            deallocate(Z24)
            deallocate(S23)

            allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'4123',VAHHPP,D1)
            allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(6,shape(t3A),size(t3A),'412356',t3A,F2)
            allocate(S3(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1*K1*K3
            I3=K3*K3*K1
            call EGEMM(I1,I2,I3,D1,F2,S3)
            deallocate(D1)
            deallocate(F2)

            allocate(X6(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            X6=0.0d0
            call sum_stripe(4,shape(X6),size(X6),'2341', 1.000,X6,S3)
            deallocate(S3)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'3142',VAHHPP,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q1(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q1)
            deallocate(D1)
            deallocate(B2)

            X4=X4+Q1

            allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(6,shape(t3A),size(t3A),'412356',t3A,F2)
            allocate(Z11(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I2=K1*K1*K3*K3
            I3=K3*K1
            call EGEMM2(I2,I3,X4,F2,Z11)
            deallocate(F2)

            V2A=V2A+Z11
            deallocate(Z11)
            deallocate(X4)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q10(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,Q1,B2,Q10)
            deallocate(B2)

            call sum_stripe(2,shape(X5),size(X5),'21', 1.000,X5,Q10)
            deallocate(Q10)

            allocate(Z38(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
            I1=K3
            I2=K1*K1*K3
            I3=K3
            call EGEMM(I1,I2,I3,X5,t2A,Z38)

            call sum_stripe(4,shape(V2A),size(V2A),'2341',-1.000,V2A,Z38)
            call sum_stripe(4,shape(V2A),size(V2A),'1342', 1.000,V2A,Z38)
            deallocate(Z38)
            deallocate(X5)

            allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3124',VBHHPP,D1)
            allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(6,shape(t3B),size(t3B),'412356',t3B,F2)
            allocate(S8(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1*K1*K3
            I3=K3*K4*K2
            call EGEMM(I1,I2,I3,D1,F2,S8)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(X6),size(X6),'2341',-2.000,X6,S8)
            deallocate(S8)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Z4(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,X6,B2,Z4)
            deallocate(B2)

            V2A=V2A+0.500*Z4
            call sum_stripe(4,shape(V2A),size(V2A),'2134',-0.500,V2A,Z4)
            deallocate(Z4)
            deallocate(X6)

            allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'3124',VAHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(Q6(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3*K3*K1
            call EGEMM(I1,I2,I3,D1,D2,Q6)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X3),size(X3),'21',-0.500,X3,Q6)
            deallocate(Q6)

            allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'2341',VBHHPP,D1)
            allocate(S6(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
            I1=K4*K1*K2
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,D1,t1A,S6)
            deallocate(D1)

            allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'2341',S6,D1)
            allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(6,shape(t3B),size(t3B),'451236',t3B,F2)
            allocate(Z7(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1*K3*K3
            I3=K4*K1*K2
            call EGEMM(I1,I2,I3,D1,F2,Z7)
            deallocate(D1)
            deallocate(F2)

            V2A=V2A+Z7
            call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,V2A,Z7)
            deallocate(Z7)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'2431',S6,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(S35(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            I1=K1*K1
            I2=K1*K3
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S35)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S35),size(S35),'3124',S35,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Z36(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,D1,B2,Z36)
            deallocate(D1)
            deallocate(B2)

            V2A=V2A+Z36
            call sum_stripe(4,shape(V2A),size(V2A),'2134',-1.000,V2A,Z36)
            call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,V2A,Z36)
            call sum_stripe(4,shape(V2A),size(V2A),'2143', 1.000,V2A,Z36)
            deallocate(Z36)
            deallocate(S35)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'2431',S6,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q11(N0+1:N1,N0+1:N1))
            I1=K1*K1
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q11)
            deallocate(D1)
            deallocate(B2)

            X3=X3-Q11
            deallocate(Q11)

            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(Z31(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1*K3*K3
            I3=K1
            call EGEMM(I1,I2,I3,X3,D2,Z31)
            deallocate(D2)

            V2A=V2A-Z31
            call sum_stripe(4,shape(V2A),size(V2A),'1243', 1.000,V2A,Z31)
            deallocate(Z31)
            deallocate(X3)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'3142',VCHHPP,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q4(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q4)
            deallocate(D1)
            deallocate(B2)

            allocate(X7(N0+1:N2,N2+1:N3))
            X7=0.0d0
            X7=X7+Q4
            deallocate(Q4)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4231',VBHHPP,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q2(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q2)
            deallocate(D1)
            deallocate(B2)

            X7=X7+Q2
            deallocate(Q2)

            allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(6,shape(t3B),size(t3B),'412356',t3B,F2)
            allocate(Z12(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I2=K1*K1*K3*K3
            I3=K4*K2
            call EGEMM2(I2,I3,X7,F2,Z12)
            deallocate(F2)

            V2A=V2A+Z12
            deallocate(Z12)
            deallocate(X7)

        end subroutine t2A_update

        subroutine t2B_update(N0,N1,N2,N3,V2B, &
                K1,K2,K3,K4,&
                FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
                VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
                VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
                VBPHPH,VBHPPP,VBPHPP, &
                VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
                t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D)

            use cc_utils, only: reorder_stripe, sum_stripe

            integer :: n0, n1, n2, n3
            integer :: k1, k2 ,k3, k4
            integer :: i1, i2, i3
            real(kind=8) :: FAHH(N0+1:N1,N0+1:N1)
            real(kind=8) :: FAHP(N1+1:N3,N0+1:N1)
            real(kind=8) :: FAPP(N1+1:N3,N1+1:N3)
            real(kind=8) :: FBHH(N0+1:N2,N0+1:N2)
            real(kind=8) :: FBHP(N2+1:N3,N0+1:N2)
            real(kind=8) :: FBPP(N2+1:N3,N2+1:N3)
            real(kind=8) :: VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1)
            real(kind=8) :: VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1)
            real(kind=8) :: VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3)
            real(kind=8) :: VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3)
            real(kind=8) :: VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2)
            real(kind=8) :: VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2)

            real(kind=8) :: t1A(N1+1:N3,N0+1:N1)
            real(kind=8) :: t1B(N2+1:N3,N0+1:N2)
            real(kind=8) :: t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) ::t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
            real(kind=8) :: t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
            real(kind=8) :: t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)

            real(kind=8) :: V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)

            real(kind=8),allocatable::B1(:,:)
            real(kind=8),allocatable::B2(:,:)
            real(kind=8),allocatable::C1(:,:,:)
            real(kind=8),allocatable::C2(:,:,:)
            real(kind=8),allocatable::D1(:,:,:,:)
            real(kind=8),allocatable::D2(:,:,:,:)
            real(kind=8),allocatable::F1(:,:,:,:,:,:)
            real(kind=8),allocatable::F2(:,:,:,:,:,:)

            real(kind=8), allocatable :: S1(:,:,:,:)
            real(kind=8), allocatable :: S41(:,:,:,:)
            real(kind=8), allocatable :: Q13(:,:)
            real(kind=8), allocatable :: S6(:,:,:,:)
            real(kind=8), allocatable :: S45(:,:,:,:)
            real(kind=8), allocatable :: S47(:,:,:,:)
            real(kind=8), allocatable :: Q15(:,:)
            real(kind=8), allocatable :: S49(:,:,:,:)
            real(kind=8), allocatable :: S65(:,:,:,:)
            real(kind=8), allocatable :: S16(:,:,:,:)
            real(kind=8), allocatable :: S61(:,:,:,:)
            real(kind=8), allocatable :: Q19(:,:)
            real(kind=8), allocatable :: S21(:,:,:,:)
            real(kind=8), allocatable :: Q5(:,:)
            real(kind=8), allocatable :: Q6(:,:)
            real(kind=8), allocatable :: S25(:,:,:,:)
            real(kind=8), allocatable :: Q7(:,:)
            real(kind=8), allocatable :: Q8(:,:)
            real(kind=8), allocatable :: S29(:,:,:,:)
            real(kind=8), allocatable :: S55(:,:,:,:)
            real(kind=8), allocatable :: S32(:,:,:,:)
            real(kind=8), allocatable :: S34(:,:,:,:)
            real(kind=8), allocatable :: Q10(:,:)
            real(kind=8), allocatable :: Q11(:,:)
            real(kind=8), allocatable :: Q12(:,:)
            real(kind=8), allocatable :: S39(:,:,:,:)
            real(kind=8), allocatable :: S3(:,:,:,:)
            real(kind=8), allocatable :: Q1(:,:)
            real(kind=8), allocatable :: Q14(:,:)
            real(kind=8), allocatable :: S8(:,:,:,:)
            real(kind=8), allocatable :: Q2(:,:)
            real(kind=8), allocatable :: Q16(:,:)
            real(kind=8), allocatable :: Q17(:,:)
            real(kind=8), allocatable :: S11(:,:,:,:)
            real(kind=8), allocatable :: S59(:,:,:,:)
            real(kind=8), allocatable :: S52(:,:,:,:)
            real(kind=8), allocatable :: S13(:,:,:,:)
            real(kind=8), allocatable :: Q3(:,:)
            real(kind=8), allocatable :: Q18(:,:)
            real(kind=8), allocatable :: S18(:,:,:,:)
            real(kind=8), allocatable :: Q4(:,:)
            real(kind=8), allocatable :: Q20(:,:)
            real(kind=8), allocatable :: Q9(:,:)
            real(kind=8), allocatable :: Z2(:,:,:,:)
            real(kind=8), allocatable :: X1(:,:,:,:)
            real(kind=8), allocatable :: Z42(:,:,:,:)
            real(kind=8), allocatable :: X2(:,:)
            real(kind=8), allocatable :: Z43(:,:,:,:)
            real(kind=8), allocatable :: Z7(:,:,:,:)
            real(kind=8), allocatable :: X3(:,:,:,:)
            real(kind=8), allocatable :: Z48(:,:,:,:)
            real(kind=8), allocatable :: X4(:,:,:,:)
            real(kind=8), allocatable :: Z66(:,:,:,:)
            real(kind=8), allocatable :: Z17(:,:,:,:)
            real(kind=8), allocatable :: X5(:,:)
            real(kind=8), allocatable :: Z63(:,:,:,:)
            real(kind=8), allocatable :: X6(:,:,:,:)
            real(kind=8), allocatable :: Z22(:,:,:,:)
            real(kind=8), allocatable :: X7(:,:)
            real(kind=8), allocatable :: Z24(:,:,:,:)
            real(kind=8), allocatable :: X8(:,:,:,:)
            real(kind=8), allocatable :: Z26(:,:,:,:)
            real(kind=8), allocatable :: X9(:,:)
            real(kind=8), allocatable :: Z28(:,:,:,:)
            real(kind=8), allocatable :: Z33(:,:,:,:)
            real(kind=8), allocatable :: X10(:,:)
            real(kind=8), allocatable :: Z5(:,:,:,:)
            real(kind=8), allocatable :: X11(:,:)
            real(kind=8), allocatable :: Z10(:,:,:,:)
            real(kind=8), allocatable :: Z12(:,:,:,:)

            allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'1342',VAHHPP,D1)
            allocate(S1(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
            I1=K3*K1*K1
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,D1,t1A,S1)
            deallocate(D1)

            allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'2341',S1,D1)
            allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(6,shape(t3B),size(t3B),'562134',t3B,F2)
            allocate(Z2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1
            I2=K2*K3*K4
            I3=K3*K1*K1
            call EGEMM(I1,I2,I3,D1,F2,Z2)
            deallocate(D1)
            deallocate(F2)

            V2B=V2B+0.500*Z2
            deallocate(Z2)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'2431',S1,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
            allocate(S41(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            I1=K1*K1
            I2=K2*K4
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S41)
            deallocate(D1)
            deallocate(D2)

            allocate(X1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
            X1=0.0d0
            call sum_stripe(4,shape(X1),size(X1),'2314', 1.000,X1,S41)
            deallocate(S41)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S1),size(S1),'2431',S1,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q13(N0+1:N1,N0+1:N1))
            I1=K1*K1
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q13)
            deallocate(D1)
            deallocate(B2)

            allocate(X2(N0+1:N1,N0+1:N1))
            X2=0.0d0
            X2=X2+Q13
            deallocate(Q13)

            allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'2341',VBHHPP,D1)
            allocate(S6(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
            I1=K4*K1*K2
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,D1,t1A,S6)
            deallocate(D1)

            allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'2341',S6,D1)
            allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(6,shape(t3C),size(t3C),'461235',t3C,F2)
            allocate(Z7(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1
            I2=K2*K3*K4
            I3=K4*K1*K2
            call EGEMM(I1,I2,I3,D1,F2,Z7)
            deallocate(D1)
            deallocate(F2)

            V2B=V2B-Z7
            deallocate(Z7)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'2431',S6,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(S45(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            I1=K1*K1
            I2=K2*K4
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S45)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(X1),size(X1),'2314',-1.000,X1,S45)
            deallocate(S45)

            allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'4231',S6,D1)
            allocate(S47(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
            I1=K1*K1*K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,D1,t1B,S47)
            deallocate(D1)

            allocate(X3(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
            X3=0.0d0
            call sum_stripe(4,shape(X3),size(X3),'3124', 1.000,X3,S47)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'2431',S6,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q15(N0+1:N1,N0+1:N1))
            I1=K1*K1
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q15)
            deallocate(D1)
            deallocate(B2)

            X2=X2-Q15
            deallocate(Q15)

            allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(4,shape(S6),size(S6),'3421',S6,D1)
            allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
            allocate(S49(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
            I1=K1*K2
            I2=K2*K3
            I3=K4*K1
            call EGEMM(I1,I2,I3,D1,D2,S49)
            deallocate(D1)
            deallocate(D2)

            allocate(X4(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
            X4=0.0d0
            call sum_stripe(4,shape(X4),size(X4),'2314', 1.000,X4,S49)
            deallocate(S49)

            allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
            call reorder_stripe(4,shape(S47),size(S47),'3214',S47,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(S65(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
            I1=K1*K2*K2
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,D1,B2,S65)
            deallocate(D1)
            deallocate(B2)

            call sum_stripe(4,shape(X4),size(X4),'2134', 1.000,X4,S65)
            deallocate(S65)

            allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'1342',VCHHPP,D1)
            allocate(S16(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
            I1=K4*K2*K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,D1,t1B,S16)
            deallocate(D1)

            allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S16),size(S16),'2341',S16,D1)
            allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(6,shape(t3C),size(t3C),'451236',t3C,F2)
            allocate(Z17(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
            I1=K2
            I2=K1*K3*K4
            I3=K4*K2*K2
            call EGEMM(I1,I2,I3,D1,F2,Z17)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(V2B),size(V2B),'1243', 0.500,V2B,Z17)
            deallocate(Z17)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S16),size(S16),'3421',S16,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(S61(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
            I1=K2*K2
            I2=K1*K3
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S61)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,X4,S61)
            deallocate(S61)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S16),size(S16),'2431',S16,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q19(N0+1:N2,N0+1:N2))
            I1=K2*K2
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q19)
            deallocate(D1)
            deallocate(B2)

            allocate(X5(N0+1:N2,N0+1:N2))
            X5=0.0d0
            X5=X5+Q19
            deallocate(Q19)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'4231',VAHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(S21(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
            I1=K3*K1
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S21)
            deallocate(D1)
            deallocate(D2)

            allocate(X6(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            X6=0.0d0
            call sum_stripe(4,shape(X6),size(X6),'3412', 1.000,X6,S21)
            deallocate(S21)

            allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'3124',VAHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(Q5(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3*K3*K1
            call EGEMM(I1,I2,I3,D1,D2,Q5)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X2),size(X2),'21',-0.500,X2,Q5)
            deallocate(Q5)

            allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'3421',VAHHPP,D1)
            allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(t2A),size(t2A),'3412',t2A,D2)
            allocate(Q6(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K3*K1*K1
            call EGEMM(I1,I2,I3,D1,D2,Q6)
            deallocate(D1)
            deallocate(D2)

            allocate(X7(N1+1:N3,N1+1:N3))
            X7=0.0d0
            call sum_stripe(2,shape(X7),size(X7),'21', 1.000,X7,Q6)
            deallocate(Q6)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4231',VBHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(S25(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
            I1=K4*K2
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S25)
            deallocate(D1)
            deallocate(D2)

            allocate(X8(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            X8=0.0d0
            call sum_stripe(4,shape(X8),size(X8),'3412', 1.000,X8,S25)
            deallocate(S25)

            allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3412',VBHHPP,D1)
            allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(t2B),size(t2B),'3412',t2B,D2)
            allocate(Q7(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K4*K1*K2
            call EGEMM(I1,I2,I3,D1,D2,Q7)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X7),size(X7),'21',-2.000,X7,Q7)
            deallocate(Q7)

            allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3421',VBHHPP,D1)
            allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(t2B),size(t2B),'3421',t2B,D2)
            allocate(Q8(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K3*K1*K2
            call EGEMM(I1,I2,I3,D1,D2,Q8)
            deallocate(D1)
            deallocate(D2)

            allocate(X9(N2+1:N3,N2+1:N3))
            X9=0.0d0
            call sum_stripe(2,shape(X9),size(X9),'21', 1.000,X9,Q8)
            deallocate(Q8)

            allocate(S29(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
            I1=K1*K2
            I2=K1*K2
            I3=K3*K4
            call EGEMM(I1,I2,I3,VBHHPP,t2B,S29)

            call sum_stripe(4,shape(X3),size(X3),'3412', 1.000,X3,S29)

            allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder_stripe(4,shape(t2B),size(t2B),'3412',t2B,D2)
            allocate(Z48(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1*K2
            I2=K3*K4
            I3=K1*K2
            call EGEMM(I1,I2,I3,X3,D2,Z48)
            deallocate(D2)

            V2B=V2B+Z48
            deallocate(Z48)
            deallocate(X3)

            allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
            call reorder_stripe(4,shape(S29),size(S29),'4312',S29,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(S55(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
            I1=K1*K2*K2
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,D1,B2,S55)
            deallocate(D1)
            deallocate(B2)

            call sum_stripe(4,shape(X4),size(X4),'2134', 1.000,X4,S55)
            deallocate(S55)

            allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4132',VBHHPP,D1)
            allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
            allocate(S32(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
            I1=K3*K2
            I2=K2*K3
            I3=K4*K1
            call EGEMM(I1,I2,I3,D1,D2,S32)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S32),size(S32),'3412',S32,D1)
            allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3214',t2B,D2)
            allocate(Z33(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
            I1=K2*K3
            I2=K1*K4
            I3=K3*K2
            call EGEMM(I1,I2,I3,D1,D2,Z33)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(V2B),size(V2B),'1423', 1.000,V2B,Z33)
            deallocate(Z33)
            deallocate(S32)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3142',VBHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(S34(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
            I1=K3*K1
            I2=K1*K3
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S34)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(X6),size(X6),'3412', 1.000,X6,S34)
            deallocate(S34)

            allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
            allocate(Z22(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
            I1=K1*K3
            I2=K2*K4
            I3=K3*K1
            call EGEMM(I1,I2,I3,X6,D2,Z22)
            deallocate(D2)

            call sum_stripe(4,shape(V2B),size(V2B),'1324', 1.000,V2B,Z22)
            deallocate(Z22)
            deallocate(X6)

            allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3124',VBHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(Q10(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3*K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Q10)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X2),size(X2),'21',-1.000,X2,Q10)
            deallocate(Q10)

            allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
            allocate(Z43(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1
            I2=K2*K3*K4
            I3=K1
            call EGEMM(I1,I2,I3,X2,D2,Z43)
            deallocate(D2)

            V2B=V2B+Z43
            deallocate(Z43)
            deallocate(X2)

            allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'3412',VCHHPP,D1)
            allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
            allocate(Q11(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K4*K2*K2
            call EGEMM(I1,I2,I3,D1,D2,Q11)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X9),size(X9),'21', 0.500,X9,Q11)
            deallocate(Q11)

            allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'3124',VCHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(Q12(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K4*K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Q12)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X5),size(X5),'21',-0.500,X5,Q12)
            deallocate(Q12)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'4231',VCHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(S39(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
            I1=K4*K2
            I2=K1*K3
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S39)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(X8),size(X8),'3412', 1.000,X8,S39)
            deallocate(S39)

            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(Z26(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
            I1=K1*K3
            I2=K2*K4
            I3=K4*K2
            call EGEMM(I1,I2,I3,X8,D2,Z26)
            deallocate(D2)

            call sum_stripe(4,shape(V2B),size(V2B),'1324', 1.000,V2B,Z26)
            deallocate(Z26)
            deallocate(X8)

            allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'3124',VAHHPP,D1)
            allocate(F2(N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(6,shape(t3B),size(t3B),'523146',t3B,F2)
            allocate(S3(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1*K2*K4
            I3=K3*K3*K1
            call EGEMM(I1,I2,I3,D1,F2,S3)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(X1),size(X1),'2341',-0.500,X1,S3)
            deallocate(S3)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'3142',VAHHPP,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q1(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q1)
            deallocate(D1)
            deallocate(B2)

            allocate(X10(N0+1:N1,N1+1:N3))
            X10=0.0d0
            X10=X10+Q1

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q14(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,Q1,B2,Q14)
            deallocate(B2)

            call sum_stripe(2,shape(X7),size(X7),'21',-2.000,X7,Q14)
            deallocate(Q14)

            allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3124',VBHHPP,D1)
            allocate(F2(N0+1:N2,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(6,shape(t3C),size(t3C),'413256',t3C,F2)
            allocate(S8(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1*K2*K4
            I3=K3*K4*K2
            call EGEMM(I1,I2,I3,D1,F2,S8)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,X1,S8)
            deallocate(S8)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4231',VBHHPP,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q2(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q2)
            deallocate(D1)
            deallocate(B2)

            allocate(X11(N0+1:N2,N2+1:N3))
            X11=0.0d0
            X11=X11+Q2

            allocate(B1(N2+1:N3,N0+1:N2))
            call reorder_stripe(2,shape(Q2),size(Q2),'21',Q2,B1)
            allocate(Q16(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,B1,t1B,Q16)
            deallocate(B1)

            call sum_stripe(2,shape(X5),size(X5),'21',-1.000,X5,Q16)
            deallocate(Q16)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q17(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,Q2,B2,Q17)
            deallocate(B2)

            call sum_stripe(2,shape(X9),size(X9),'21', 1.000,X9,Q17)
            deallocate(Q17)

            allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'1342',VBHHPP,D1)
            allocate(S11(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
            I1=K3*K1*K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,D1,t1B,S11)
            deallocate(D1)

            allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S11),size(S11),'2341',S11,D1)
            allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(6,shape(t3B),size(t3B),'452136',t3B,F2)
            allocate(Z12(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
            I1=K2
            I2=K1*K3*K4
            I3=K3*K1*K2
            call EGEMM(I1,I2,I3,D1,F2,Z12)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(V2B),size(V2B),'1243',-1.000,V2B,Z12)
            deallocate(Z12)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S11),size(S11),'3421',S11,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
            allocate(S59(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
            I1=K2*K2
            I2=K1*K3
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S59)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,X4,S59)
            deallocate(S59)

            allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
            call reorder_stripe(4,shape(S11),size(S11),'2431',S11,D1)
            allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3214',t2B,D2)
            allocate(S52(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
            I1=K2*K1
            I2=K1*K4
            I3=K3*K2
            call EGEMM(I1,I2,I3,D1,D2,S52)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(X1),size(X1),'2413', 1.000,X1,S52)
            deallocate(S52)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Z42(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
            I1=K1*K2*K4
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,X1,B2,Z42)
            deallocate(B2)

            call sum_stripe(4,shape(V2B),size(V2B),'2134', 1.000,V2B,Z42)
            deallocate(Z42)
            deallocate(X1)

            allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4123',VBHHPP,D1)
            allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(6,shape(t3B),size(t3B),'512346',t3B,F2)
            allocate(S13(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
            I1=K2
            I2=K1*K2*K3
            I3=K3*K4*K1
            call EGEMM(I1,I2,I3,D1,F2,S13)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,X4,S13)
            deallocate(S13)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3142',VBHHPP,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q3(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q3)
            deallocate(D1)
            deallocate(B2)

            X10=X10+Q3

            allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(6,shape(t3B),size(t3B),'521346',t3B,F2)
            allocate(Z5(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I2=K1*K2*K3*K4
            I3=K3*K1
            call EGEMM2(I2,I3,X10,F2,Z5)
            deallocate(F2)

            V2B=V2B+Z5
            deallocate(Z5)
            deallocate(X10)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q18(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,Q3,B2,Q18)
            deallocate(B2)

            call sum_stripe(2,shape(X7),size(X7),'21',-2.000,X7,Q18)
            deallocate(Q18)

            allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'2134',t2B,D2)
            allocate(Z24(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
            I1=K3
            I2=K1*K2*K4
            I3=K3
            call EGEMM(I1,I2,I3,X7,D2,Z24)
            deallocate(D2)

            call sum_stripe(4,shape(V2B),size(V2B),'1342', 0.500,V2B,Z24)
            deallocate(Z24)
            deallocate(X7)

            allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'4123',VCHHPP,D1)
            allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(6,shape(t3C),size(t3C),'412356',t3C,F2)
            allocate(S18(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
            I1=K2
            I2=K1*K2*K3
            I3=K4*K4*K2
            call EGEMM(I1,I2,I3,D1,F2,S18)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(X4),size(X4),'2341', 0.500,X4,S18)
            deallocate(S18)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Z66(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1*K2*K3
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,X4,B2,Z66)
            deallocate(B2)

            V2B=V2B+Z66
            deallocate(Z66)
            deallocate(X4)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'3142',VCHHPP,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q4(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q4)
            deallocate(D1)
            deallocate(B2)

            X11=X11+Q4

            allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            call reorder_stripe(6,shape(t3C),size(t3C),'412356',t3C,F2)
            allocate(Z10(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I2=K1*K2*K3*K4
            I3=K4*K2
            call EGEMM2(I2,I3,X11,F2,Z10)
            deallocate(F2)

            V2B=V2B+Z10
            deallocate(Z10)
            deallocate(X11)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q20(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,Q4,B2,Q20)
            deallocate(B2)

            call sum_stripe(2,shape(X9),size(X9),'21', 1.000,X9,Q20)
            deallocate(Q20)

            allocate(Z28(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
            I1=K4
            I2=K1*K2*K3
            I3=K4
            call EGEMM(I1,I2,I3,X9,t2B,Z28)

            call sum_stripe(4,shape(V2B),size(V2B),'2341',-1.000,V2B,Z28)
            deallocate(Z28)
            deallocate(X9)

            allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4123',VBHHPP,D1)
            allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
            allocate(Q9(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K3*K4*K1
            call EGEMM(I1,I2,I3,D1,D2,Q9)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X5),size(X5),'21',-1.000,X5,Q9)
            deallocate(Q9)

            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
            allocate(Z63(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
            I1=K2
            I2=K1*K3*K4
            I3=K2
            call EGEMM(I1,I2,I3,X5,D2,Z63)
            deallocate(D2)

            call sum_stripe(4,shape(V2B),size(V2B),'1243', 1.000,V2B,Z63)
            deallocate(Z63)
            deallocate(X5)

        end subroutine t2B_update

        subroutine t2C_update(N0,N1,N2,N3,V2C, &
                K1,K2,K3,K4,&
                FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
                VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
                VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
                VBPHPH,VBHPPP,VBPHPP, &
                VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
                t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D)

            use cc_utils, only: reorder_stripe, sum_stripe

            integer :: n0, n1, n2, n3
            integer :: k1, k2 ,k3, k4
            integer :: i1, i2, i3
            real(kind=8) :: shift
            real(kind=8) :: FAHH(N0+1:N1,N0+1:N1)
            real(kind=8) :: FAHP(N1+1:N3,N0+1:N1)
            real(kind=8) :: FAPP(N1+1:N3,N1+1:N3)
            real(kind=8) :: FBHH(N0+1:N2,N0+1:N2)
            real(kind=8) :: FBHP(N2+1:N3,N0+1:N2)
            real(kind=8) :: FBPP(N2+1:N3,N2+1:N3)
            real(kind=8) :: VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1)
            real(kind=8) :: VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1)
            real(kind=8) :: VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3)
            real(kind=8) :: VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1)
            real(kind=8) :: VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3)
            real(kind=8) :: VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2)
            real(kind=8) :: VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2)

            real(kind=8) :: t1A(N1+1:N3,N0+1:N1)
            real(kind=8) :: t1B(N2+1:N3,N0+1:N2)
            real(kind=8) :: t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) ::t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
            real(kind=8) :: t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
            real(kind=8) :: t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)

            real(kind=8) :: V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)

            real(kind=8),allocatable::B1(:,:)
            real(kind=8),allocatable::B2(:,:)
            real(kind=8),allocatable::C1(:,:,:)
            real(kind=8),allocatable::C2(:,:,:)
            real(kind=8),allocatable::D1(:,:,:,:)
            real(kind=8),allocatable::D2(:,:,:,:)
            real(kind=8),allocatable::F1(:,:,:,:,:,:)
            real(kind=8),allocatable::F2(:,:,:,:,:,:)

            real(kind=8), allocatable :: Q1(:,:)
            real(kind=8), allocatable :: Q2(:,:)
            real(kind=8), allocatable :: Q9(:,:)
            real(kind=8), allocatable :: Q10(:,:)
            real(kind=8), allocatable :: Q3(:,:)
            real(kind=8), allocatable :: S8(:,:,:,:)
            real(kind=8), allocatable :: S29(:,:,:,:)
            real(kind=8), allocatable :: S33(:,:,:,:)
            real(kind=8), allocatable :: Q11(:,:)
            real(kind=8), allocatable :: S31(:,:,:,:)
            real(kind=8), allocatable :: S39(:,:,:,:)
            real(kind=8), allocatable :: Q6(:,:)
            real(kind=8), allocatable :: Q7(:,:)
            real(kind=8), allocatable :: S20(:,:,:,:)
            real(kind=8), allocatable :: S36(:,:,:,:)
            real(kind=8), allocatable :: S23(:,:,:,:)
            real(kind=8), allocatable :: S3(:,:,:,:)
            real(kind=8), allocatable :: S27(:,:,:,:)
            real(kind=8), allocatable :: S5(:,:,:,:)
            real(kind=8), allocatable :: S10(:,:,:,:)
            real(kind=8), allocatable :: Q4(:,:)
            real(kind=8), allocatable :: Q12(:,:)
            real(kind=8), allocatable :: S15(:,:,:,:)
            real(kind=8), allocatable :: Q8(:,:)
            real(kind=8), allocatable :: S13(:,:,:,:)
            real(kind=8), allocatable :: Q5(:,:)
            real(kind=8), allocatable :: X1(:,:)
            real(kind=8), allocatable :: Z1(:,:,:,:)
            real(kind=8), allocatable :: X2(:,:)
            real(kind=8), allocatable :: Z2(:,:,:,:)
            real(kind=8), allocatable :: X3(:,:)
            real(kind=8), allocatable :: Z25(:,:,:,:)
            real(kind=8), allocatable :: X4(:,:)
            real(kind=8), allocatable :: Z26(:,:,:,:)
            real(kind=8), allocatable :: Z9(:,:,:,:)
            real(kind=8), allocatable :: X5(:,:,:,:)
            real(kind=8), allocatable :: Z30(:,:,:,:)
            real(kind=8), allocatable :: X6(:,:,:,:)
            real(kind=8), allocatable :: Z40(:,:,:,:)
            real(kind=8), allocatable :: Z34(:,:,:,:)
            real(kind=8), allocatable :: Z32(:,:,:,:)
            real(kind=8), allocatable :: Z24(:,:,:,:)
            real(kind=8), allocatable :: Z4(:,:,:,:)
            real(kind=8), allocatable :: Z28(:,:,:,:)
            real(kind=8), allocatable :: X7(:,:,:,:)
            real(kind=8), allocatable :: Z6(:,:,:,:)
            real(kind=8), allocatable :: Z16(:,:,:,:)
            real(kind=8), allocatable :: Z14(:,:,:,:)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'4231',VAHHPP,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q1(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q1)
            deallocate(D1)
            deallocate(B2)

            allocate(X1(N0+1:N1,N1+1:N3))
            X1=0.0d0
            X1=X1+Q1
            deallocate(Q1)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4231',VBHHPP,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
            allocate(Q2(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q2)
            deallocate(D1)
            deallocate(B2)

            allocate(X2(N0+1:N2,N2+1:N3))
            X2=0.0d0
            X2=X2+Q2

            allocate(B1(N2+1:N3,N0+1:N2))
            call reorder_stripe(2,shape(Q2),size(Q2),'21',Q2,B1)
            allocate(Q9(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,B1,t1B,Q9)
            deallocate(B1)

            allocate(X3(N0+1:N2,N0+1:N2))
            X3=0.0d0
            call sum_stripe(2,shape(X3),size(X3),'21', 1.000,X3,Q9)
            deallocate(Q9)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q10(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,Q2,B2,Q10)
            deallocate(B2)

            allocate(X4(N2+1:N3,N2+1:N3))
            X4=0.0d0
            call sum_stripe(2,shape(X4),size(X4),'21', 1.000,X4,Q10)
            deallocate(Q10)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3142',VBHHPP,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q3(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q3)
            deallocate(D1)
            deallocate(B2)

            X1=X1+Q3
            deallocate(Q3)

            allocate(F2(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(6,shape(t3C),size(t3C),'631245',t3C,F2)
            allocate(Z1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I2=K2*K2*K4*K4
            I3=K3*K1
            call EGEMM2(I2,I3,X1,F2,Z1)
            deallocate(F2)

            V2C=V2C+Z1
            deallocate(Z1)
            deallocate(X1)

            allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'1342',VCHHPP,D1)
            allocate(S8(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
            I1=K4*K2*K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,D1,t1B,S8)
            deallocate(D1)

            allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S8),size(S8),'2341',S8,D1)
            allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(6,shape(t3D),size(t3D),'451236',t3D,F2)
            allocate(Z9(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2*K4*K4
            I3=K4*K2*K2
            call EGEMM(I1,I2,I3,D1,F2,Z9)
            deallocate(D1)
            deallocate(F2)

            V2C=V2C-0.500*Z9
            call sum_stripe(4,shape(V2C),size(V2C),'1243', 0.500,V2C,Z9)
            deallocate(Z9)

            allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S8),size(S8),'4231',S8,D1)
            allocate(S29(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2*K2*K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,D1,t1B,S29)
            deallocate(D1)

            allocate(X5(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
            X5=0.0d0
            call sum_stripe(4,shape(X5),size(X5),'3124', 1.000,X5,S29)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S8),size(S8),'2431',S8,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(S33(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2*K2
            I2=K2*K4
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S33)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S33),size(S33),'3124',S33,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Z34(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,D1,B2,Z34)
            deallocate(D1)
            deallocate(B2)

            call sum_stripe(4,shape(V2C),size(V2C),'2134', 1.000,V2C,Z34)
            call sum_stripe(4,shape(V2C),size(V2C),'2143',-1.000,V2C,Z34)
            deallocate(Z34)
            deallocate(S33)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S8),size(S8),'2431',S8,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q11(N0+1:N2,N0+1:N2))
            I1=K2*K2
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q11)
            deallocate(D1)
            deallocate(B2)

            X3=X3-Q11
            deallocate(Q11)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S8),size(S8),'3421',S8,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(S31(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2*K2
            I2=K2*K4
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S31)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S31),size(S31),'3124',S31,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Z32(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,D1,B2,Z32)
            deallocate(D1)
            deallocate(B2)

            V2C=V2C+Z32
            call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,V2C,Z32)
            deallocate(Z32)
            deallocate(S31)

            allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S29),size(S29),'3214',S29,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(S39(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2*K2*K2
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,D1,B2,S39)
            deallocate(D1)
            deallocate(B2)

            allocate(X6(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            X6=0.0d0
            call sum_stripe(4,shape(X6),size(X6),'2134', 1.000,X6,S39)
            deallocate(S39)

            allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'3421',VBHHPP,D1)
            allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(t2B),size(t2B),'3421',t2B,D2)
            allocate(Q6(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K3*K1*K2
            call EGEMM(I1,I2,I3,D1,D2,Q6)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X4),size(X4),'21', 1.000,X4,Q6)
            deallocate(Q6)

            allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'3421',VCHHPP,D1)
            allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
            allocate(Q7(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K4*K2*K2
            call EGEMM(I1,I2,I3,D1,D2,Q7)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X4),size(X4),'21',-0.500,X4,Q7)
            deallocate(Q7)

            allocate(S20(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2*K2
            I2=K2*K2
            I3=K4*K4
            call EGEMM(I1,I2,I3,VCHHPP,t2C,S20)

            call sum_stripe(4,shape(X5),size(X5),'3412',-0.500,X5,S20)

            allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
            allocate(Z30(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2
            I2=K4*K4
            I3=K2*K2
            call EGEMM(I1,I2,I3,X5,D2,Z30)
            deallocate(D2)

            V2C=V2C-0.500*Z30
            deallocate(Z30)
            deallocate(X5)

            allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S20),size(S20),'4312',S20,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(S36(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2*K2*K2
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,D1,B2,S36)
            deallocate(D1)
            deallocate(B2)

            call sum_stripe(4,shape(X6),size(X6),'2134',-0.500,X6,S36)
            deallocate(S36)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Z40(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,X6,B2,Z40)
            deallocate(B2)

            V2C=V2C-Z40
            deallocate(Z40)
            deallocate(X6)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'4231',VCHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(S23(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
            I1=K4*K2
            I2=K2*K4
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,S23)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S23),size(S23),'3412',S23,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(Z24(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
            I1=K2*K4
            I2=K2*K4
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Z24)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(V2C),size(V2C),'1423',-1.000,V2C,Z24)
            call sum_stripe(4,shape(V2C),size(V2C),'1324', 1.000,V2C,Z24)
            deallocate(Z24)
            deallocate(S23)

            allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'1342',VBHHPP,D1)
            allocate(S3(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
            I1=K3*K1*K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,D1,t1B,S3)
            deallocate(D1)

            allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S3),size(S3),'2341',S3,D1)
            allocate(F2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(6,shape(t3C),size(t3C),'463125',t3C,F2)
            allocate(Z4(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2*K4*K4
            I3=K3*K1*K2
            call EGEMM(I1,I2,I3,D1,F2,Z4)
            deallocate(D1)
            deallocate(F2)

            V2C=V2C+Z4
            call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,V2C,Z4)
            deallocate(Z4)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S3),size(S3),'3421',S3,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
            allocate(S27(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2*K2
            I2=K2*K4
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S27)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(4,shape(S27),size(S27),'3124',S27,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Z28(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,D1,B2,Z28)
            deallocate(D1)
            deallocate(B2)

            V2C=V2C+Z28
            call sum_stripe(4,shape(V2C),size(V2C),'2134',-1.000,V2C,Z28)
            call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,V2C,Z28)
            call sum_stripe(4,shape(V2C),size(V2C),'2143', 1.000,V2C,Z28)
            deallocate(Z28)
            deallocate(S27)

            allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4123',VBHHPP,D1)
            allocate(F2(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(6,shape(t3C),size(t3C),'613245',t3C,F2)
            allocate(S5(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2*K2*K4
            I3=K3*K4*K1
            call EGEMM(I1,I2,I3,D1,F2,S5)
            deallocate(D1)
            deallocate(F2)

            allocate(X7(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
            X7=0.0d0
            call sum_stripe(4,shape(X7),size(X7),'2341', 1.000,X7,S5)
            deallocate(S5)

            allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'4123',VCHHPP,D1)
            allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(6,shape(t3D),size(t3D),'412356',t3D,F2)
            allocate(S10(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2*K2*K4
            I3=K4*K4*K2
            call EGEMM(I1,I2,I3,D1,F2,S10)
            deallocate(D1)
            deallocate(F2)

            call sum_stripe(4,shape(X7),size(X7),'2341',-0.500,X7,S10)
            deallocate(S10)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Z6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,X7,B2,Z6)
            deallocate(B2)

            V2C=V2C-Z6
            call sum_stripe(4,shape(V2C),size(V2C),'2134', 1.000,V2C,Z6)
            deallocate(Z6)
            deallocate(X7)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'3142',VCHHPP,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q4(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q4)
            deallocate(D1)
            deallocate(B2)

            X2=X2+Q4

            allocate(F2(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            call reorder_stripe(6,shape(t3D),size(t3D),'412356',t3D,F2)
            allocate(Z2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I2=K2*K2*K4*K4
            I3=K4*K2
            call EGEMM2(I2,I3,X2,F2,Z2)
            deallocate(F2)

            V2C=V2C+Z2
            deallocate(Z2)
            deallocate(X2)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
            allocate(Q12(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,Q4,B2,Q12)
            deallocate(B2)

            call sum_stripe(2,shape(X4),size(X4),'21', 1.000,X4,Q12)
            deallocate(Q12)

            allocate(Z26(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
            I1=K4
            I2=K2*K2*K4
            I3=K4
            call EGEMM(I1,I2,I3,X4,t2C,Z26)

            call sum_stripe(4,shape(V2C),size(V2C),'2341',-1.000,V2C,Z26)
            call sum_stripe(4,shape(V2C),size(V2C),'1342', 1.000,V2C,Z26)
            deallocate(Z26)
            deallocate(X4)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4231',VBHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
            allocate(S15(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
            I1=K4*K2
            I2=K2*K4
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S15)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S15),size(S15),'3412',S15,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(Z16(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
            I1=K2*K4
            I2=K2*K4
            I3=K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Z16)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(V2C),size(V2C),'2314',-1.000,V2C,Z16)
            call sum_stripe(4,shape(V2C),size(V2C),'1324', 1.000,V2C,Z16)
            call sum_stripe(4,shape(V2C),size(V2C),'2413', 1.000,V2C,Z16)
            call sum_stripe(4,shape(V2C),size(V2C),'1423',-1.000,V2C,Z16)
            deallocate(Z16)
            deallocate(S15)

            allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VCHHPP),size(VCHHPP),'3124',VCHHPP,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(Q8(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K4*K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Q8)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X3),size(X3),'21', 0.500,X3,Q8)
            deallocate(Q8)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder_stripe(4,shape(VAHHPP),size(VAHHPP),'4231',VAHHPP,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
            allocate(S13(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
            I1=K3*K1
            I2=K2*K4
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,S13)
            deallocate(D1)
            deallocate(D2)

            allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(S13),size(S13),'3412',S13,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
            allocate(Z14(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
            I1=K2*K4
            I2=K2*K4
            I3=K3*K1
            call EGEMM(I1,I2,I3,D1,D2,Z14)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(4,shape(V2C),size(V2C),'2314',-0.500,V2C,Z14)
            call sum_stripe(4,shape(V2C),size(V2C),'1324', 0.500,V2C,Z14)
            call sum_stripe(4,shape(V2C),size(V2C),'2413', 0.500,V2C,Z14)
            call sum_stripe(4,shape(V2C),size(V2C),'1423',-0.500,V2C,Z14)
            deallocate(Z14)
            deallocate(S13)

            allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(VBHHPP),size(VBHHPP),'4123',VBHHPP,D1)
            allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
            allocate(Q5(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K3*K4*K1
            call EGEMM(I1,I2,I3,D1,D2,Q5)
            deallocate(D1)
            deallocate(D2)

            call sum_stripe(2,shape(X3),size(X3),'21', 1.000,X3,Q5)
            deallocate(Q5)

            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
            allocate(Z25(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2*K4*K4
            I3=K2
            call EGEMM(I1,I2,I3,X3,D2,Z25)
            deallocate(D2)

            V2C=V2C+Z25
            call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,V2C,Z25)
            deallocate(Z25)
            deallocate(X3)

        end subroutine t2C_update


        subroutine t2A_disconnected(N0,N1,N2,N3,K1,K2,K3,K4, &
                V2A, V2B, V2C, &
                FockR,FockB,IntR,IntB,IntM, &
                t1A,t1B,t2A,t2B,t2C, &
                t3A,t3B,t3C,t3D)

            use cc_utils, only: reorder_stripe

            integer :: n0,n1,n2,n3
            integer :: k1,k2,k3,k4
            integer :: i,j,a,b
            integer :: m, n, e,f
            integer :: i1, i2, i3
            real(kind=8) :: shift,PP,Coeleft
            real(kind=8) :: FockR(N3,N3)
            real(kind=8) :: FockB(N3,N3)
            real(kind=8) :: IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
            real(kind=8) :: IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
            real(kind=8) :: IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
            real(kind=8) :: t1A(N1+1:N3,N0+1:N1)
            real(kind=8) :: t1B(N2+1:N3,N0+1:N2)
            real(kind=8) :: t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
            real(kind=8) :: t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
            real(kind=8) :: t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)

            real(kind=8),allocatable::accum(:,:)

            real(kind=8) :: e2a, e2b, e2c
            real(kind=8) :: e1a1a, e1a1b, e1b1b
            real(kind=8) :: etot

            real(kind=8),allocatable::B1(:,:)
            real(kind=8),allocatable::B2(:,:)
            real(kind=8),allocatable::D1(:,:,:,:)
            real(kind=8),allocatable::D2(:,:,:,:)
            real(kind=8),allocatable::F2(:,:,:,:,:,:)
            real(kind=8),allocatable::H2(:,:,:,:,:,:,:,:)

            real(kind=8),allocatable::Q1(:,:)
            real(kind=8),allocatable::Q2(:,:)
            real(kind=8),allocatable::Q3(:,:)
            real(kind=8),allocatable::Q24(:,:)
            real(kind=8),allocatable::Q25(:,:)
            real(kind=8),allocatable::Q26(:,:)
            real(kind=8),allocatable::Q27(:,:)
            real(kind=8),allocatable::Q28(:,:)
            real(kind=8),allocatable::Q29(:,:)
            real(kind=8),allocatable::Q30(:,:)
            real(kind=8),allocatable::Q31(:,:)
            real(kind=8),allocatable::Q32(:,:)
            real(kind=8),allocatable::Q33(:,:)
            real(kind=8),allocatable::Q34(:,:)
            real(kind=8),allocatable::Q35(:,:)
            real(kind=8),allocatable::Q36(:,:)
            real(kind=8),allocatable::Q37(:,:)
            real(kind=8),allocatable::Q38(:,:)
            real(kind=8),allocatable::Q39(:,:)
            real(kind=8),allocatable::Q40(:,:)
            real(kind=8),allocatable::Q41(:,:)
            real(kind=8),allocatable::Q42(:,:)
            real(kind=8),allocatable::Q43(:,:)
            real(kind=8),allocatable::X1(:,:)
            real(kind=8),allocatable::X2(:,:)
            real(kind=8),allocatable::X3(:,:)

            allocate(accum(N1+1:N3,N0+1:N1))
            accum = 0.0d0

            allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
            allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
            !call reorder451236(N1,N3,N1,N3,N1,N3,N0,N1,N0,N1,N0,N1, &
            !    N0,N1,N0,N1,N1,N3,N1,N3,N1,N3,N0,N1,t3A,F2)
            call reorder_stripe(6,shape(t3a),size(t3a),'451236',t3a,f2)
            allocate(Q1(N1+1:N3,N0+1:N1))
            I2=K1*K3
            I3=K3*K3*K1*K1
            call EGEMM2(I2,I3,D1,F2,Q1)
            deallocate(D1)
            deallocate(F2)

            allocate(X1(N1+1:N3,N0+1:N1))
            X1=0.0d0
            X1=X1+0.250*Q1
            deallocate(Q1)

            allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
            allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
            !call reorder451236(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1, &
            !    N0,N2,N0,N1,N2,N3,N1,N3,N1,N3,N0,N1,t3B,F2)
            call reorder_stripe(6,shape(t3b),size(t3b),'451236',t3b,f2)
            allocate(Q2(N1+1:N3,N0+1:N1))
            I2=K1*K3
            I3=K3*K4*K1*K2
            call EGEMM2(I2,I3,D1,F2,Q2)
            deallocate(D1)
            deallocate(F2)

            X1=X1+Q2
            deallocate(Q2)

            allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
            allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
            !call reorder451236(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1, &
            !    N0,N2,N0,N2,N2,N3,N2,N3,N1,N3,N0,N1,t3C,F2)
            call reorder_stripe(6,shape(t3c),size(t3c),'451236',t3c,f2)
            allocate(Q3(N1+1:N3,N0+1:N1))
            I2=K1*K3
            I3=K4*K4*K2*K2
            call EGEMM2(I2,I3,D1,F2,Q3)
            deallocate(D1)
            deallocate(F2)

            X1=X1+0.250*Q3
            accum = accum + x1
            deallocate(Q3)
            deallocate(X1)

            allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
            allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1, &
                N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
            allocate(Q24(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K3*K1*K1
            call EGEMM(I1,I2,I3,D1,D2,Q24)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N1+1:N3,N1+1:N3))
            call reorder21(N1,N3,N1,N3, &
                N1,N3,N1,N3,Q24,B1)
            allocate(B2(N1+1:N3,N0+1:N1))
            call reorder12(N1,N3,N0,N1, &
                N1,N3,N0,N1,t1A,B2)
            allocate(Q25(N0+1:N1,N1+1:N3))
            I1=K3
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,B1,B2,Q25)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q24)

            allocate(X2(N1+1:N3,N0+1:N1))
            X2=0.0d0
            call sum21(N1,N3,N0,N1,X2,Q25,-0.500)
            deallocate(Q25)

            allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
            allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1, &
                N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
            allocate(Q26(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3*K3*K1
            call EGEMM(I1,I2,I3,D1,D2,Q26)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N0+1:N1,N0+1:N1))
            call reorder21(N0,N1,N0,N1, &
                N0,N1,N0,N1,Q26,B1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q27(N1+1:N3,N0+1:N1))
            I1=K1
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,B1,B2,Q27)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q26)

            X2=X2-0.500*Q27
            deallocate(Q27)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q28(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q28)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1, &
                N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
            allocate(Q29(N0+1:N1,N1+1:N3))
            I2=K3*K1
            I3=K3*K1
            call EGEMM2(I2,I3,Q28,D2,Q29)
            deallocate(D2)

            call sum21(N1,N3,N0,N1,X2,Q29, 1.000)
            deallocate(Q29)

            allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
            allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
            allocate(Q30(N1+1:N3,N1+1:N3))
            I1=K3
            I2=K3
            I3=K4*K1*K2
            call EGEMM(I1,I2,I3,D1,D2,Q30)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N1+1:N3,N1+1:N3))
            call reorder21(N1,N3,N1,N3, &
                N1,N3,N1,N3,Q30,B1)
            allocate(B2(N1+1:N3,N0+1:N1))
            call reorder12(N1,N3,N0,N1, &
                N1,N3,N0,N1,t1A,B2)
            allocate(Q31(N0+1:N1,N1+1:N3))
            I1=K3
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,B1,B2,Q31)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q30)

            call sum21(N1,N3,N0,N1,X2,Q31,-1.000)
            deallocate(Q31)

            allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
            allocate(Q32(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3*K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Q32)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N0+1:N1,N0+1:N1))
            call reorder21(N0,N1,N0,N1, &
                N0,N1,N0,N1,Q32,B1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q33(N1+1:N3,N0+1:N1))
            I1=K1
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,B1,B2,Q33)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q32)

            X2=X2-Q33
            deallocate(Q33)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q34(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q34)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
            allocate(Q35(N0+1:N1,N1+1:N3))
            I2=K3*K1
            I3=K4*K2
            call EGEMM2(I2,I3,Q34,D2,Q35)
            deallocate(D2)
            deallocate(Q34)

            call sum21(N1,N3,N0,N1,X2,Q35, 1.000)
            deallocate(Q35)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder2413(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q36(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q36)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1, &
                N0,N1,N1,N3,N0,N1,N1,N3,t2A,D2)
            allocate(Q37(N0+1:N1,N1+1:N3))
            I2=K3*K1
            I3=K3*K1
            call EGEMM2(I2,I3,Q36,D2,Q37)
            deallocate(D2)

            call sum21(N1,N3,N0,N1,X2,Q37, 1.000)
            deallocate(Q37)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q38(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q38)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N2,N2,N3,N0,N1,N1,N3,t2B,D2)
            allocate(Q39(N0+1:N1,N1+1:N3))
            I2=K3*K1
            I3=K4*K2
            call EGEMM2(I2,I3,Q38,D2,Q39)
            deallocate(D2)
            deallocate(Q38)

            call sum21(N1,N3,N0,N1,X2,Q39, 1.000)
            deallocate(Q39)
            accum = accum + x2
            deallocate(X2)

            allocate(B1(N1+1:N3,N0+1:N1))
            call reorder21(N0,N1,N1,N3, &
                N1,N3,N0,N1,Q28,B1)
            allocate(B2(N1+1:N3,N0+1:N1))
            call reorder12(N1,N3,N0,N1, &
                N1,N3,N0,N1,t1A,B2)
            allocate(Q40(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,B1,B2,Q40)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q28)

            allocate(B1(N0+1:N1,N0+1:N1))
            call reorder21(N0,N1,N0,N1, &
                N0,N1,N0,N1,Q40,B1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q41(N1+1:N3,N0+1:N1))
            I1=K1
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,B1,B2,Q41)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q40)

            allocate(X3(N1+1:N3,N0+1:N1))
            X3=0.0d0
            X3=X3-Q41
            deallocate(Q41)

            allocate(B1(N1+1:N3,N0+1:N1))
            call reorder21(N0,N1,N1,N3, &
                N1,N3,N0,N1,Q36,B1)
            allocate(B2(N1+1:N3,N0+1:N1))
            call reorder12(N1,N3,N0,N1, &
                N1,N3,N0,N1,t1A,B2)
            allocate(Q42(N0+1:N1,N0+1:N1))
            I1=K1
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,B1,B2,Q42)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q36)

            allocate(B1(N0+1:N1,N0+1:N1))
            call reorder21(N0,N1,N0,N1, &
                N0,N1,N0,N1,Q42,B1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q43(N1+1:N3,N0+1:N1))
            I1=K1
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,B1,B2,Q43)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q42)

            X3=X3-Q43
            deallocate(Q43)
            accum = accum + x3
            deallocate(X3)

            ! [TODO] scaling stuff should go somewhere independend
            ! Scaling of T_2
            call v_t2(n0, n1, n2, n3, &
                intr, intm, intb, &
                t2a, t2b, t2c, &
                e2a, e2b, e2c)

            ! Scaling of T_1^2
            call v_t1_t1(n0, n1, n2 ,n3, &
                intr, intm, intb, &
                t1a, t1b, &
                e1a1a, e1a1b, e1b1b)

            etot = e2a + e2b + e2c + e1a1a + e1a1b + e1b1b

            v2a = v2a + etot * t2a
            v2b = v2b + etot * t2b
            v2c = v2c + etot * t2c

            ! All energy pieces times T_1A^2
            forall(b=n1+1:n3, a=n1+1:n3, j=n0+1:n1, i=n0+1:n1)
                v2a(b,a,j,i) = v2a(b,a,j,i) &
                    + etot * (t1a(b,j) * t1a(a,i) - t1a(b,i) * t1a(a,j))
            end forall

            ! All energy pieces times T_1A * T_1B
            forall(b=n2+1:n3, a=n1+1:n3, j=n0+1:n2, i=n0+1:n1)
                v2b(b,a,j,i) = v2b(b,a,j,i) &
                    + etot * t1b(b,j) * t1a(a,i)
            end forall

            ! All energy pieces times T_1B * T_1B
            forall(b=n2+1:n3, a=n2+1:n3, j=n0+1:n2, i=n0+1:n2)
                v2c(b,a,j,i) = v2c(b,a,j,i) &
                    + etot * (t1b(b,j) * t1b(a,i) - t1b(b,i) * t1b(a,j))
            end forall


            ! Projection of singles A and outer product T_1A
            forall(b=n1+1:n3, a=n1+1:n3, j=n0+1:n1, i=n0+1:n1)
                v2a(b,a,j,i) = v2a(b,a,j,i) &
                    + accum(b,j) * t1a(a,i) &
                    - accum(a,j) * t1a(b,i) &
                    - accum(b,i) * t1a(a,j) &
                    + accum(a,i) * t1a(b,j)
            end forall

            ! Projection of singles A and outer product T_1B
            forall(b=n1+1:n3, a=n1+1:n3, j=n0+1:n1, i=n0+1:n1)
                v2b(b,a,j,i) = v2b(b,a,j,i) &
                    + accum(a,i) * t1b(b,j)
            end forall

            deallocate(accum)

        end subroutine t2A_disconnected

        subroutine t2B_disconnected(N0,N1,N2,N3,K1,K2,K3,K4, &
                V2B, V2C, &
                FockR,FockB,IntR,IntB,IntM, &
                t1A,t1B,t2A,t2B,t2C, &
                t3A,t3B,t3C,t3D)

            use cc_utils, only: reorder_stripe

            integer :: n0,n1,n2,n3
            integer :: k1,k2,k3,k4
            integer :: i,j,a,b
            integer :: m, n, e,f
            integer :: i1, i2, i3
            real(kind=8) :: shift,PP,Coeleft
            real(kind=8) :: FockR(N3,N3)
            real(kind=8) :: FockB(N3,N3)
            real(kind=8) :: IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
            real(kind=8) :: IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
            real(kind=8) :: IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
            real(kind=8) :: t1A(N1+1:N3,N0+1:N1)
            real(kind=8) :: t1B(N2+1:N3,N0+1:N2)
            real(kind=8) :: t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
            real(kind=8) :: t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
            real(kind=8) :: t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
            real(kind=8) :: t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
            real(kind=8) :: t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
            real(kind=8) :: t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
            real(kind=8) :: V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
            real(kind=8) :: V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)

            real(kind=8),allocatable::accum(:,:)

            real(kind=8) :: e2a, e2b, e2c
            real(kind=8) :: e1a1a, e1a1b, e1b1b
            real(kind=8) :: etot

            real(kind=8),allocatable::B1(:,:)
            real(kind=8),allocatable::B2(:,:)
            real(kind=8),allocatable::D1(:,:,:,:)
            real(kind=8),allocatable::D2(:,:,:,:)
            real(kind=8),allocatable::F2(:,:,:,:,:,:)
            real(kind=8),allocatable::H2(:,:,:,:,:,:,:,:)

            real(kind=8),allocatable::Q1(:,:)
            real(kind=8),allocatable::Q2(:,:)
            real(kind=8),allocatable::Q3(:,:)
            real(kind=8),allocatable::Q18(:,:)
            real(kind=8),allocatable::Q19(:,:)
            real(kind=8),allocatable::Q20(:,:)
            real(kind=8),allocatable::Q21(:,:)
            real(kind=8),allocatable::Q28(:,:)
            real(kind=8),allocatable::Q29(:,:)
            real(kind=8),allocatable::Q30(:,:)
            real(kind=8),allocatable::Q31(:,:)
            real(kind=8),allocatable::Q32(:,:)
            real(kind=8),allocatable::Q33(:,:)
            real(kind=8),allocatable::Q34(:,:)
            real(kind=8),allocatable::Q35(:,:)
            real(kind=8),allocatable::Q36(:,:)
            real(kind=8),allocatable::Q37(:,:)
            real(kind=8),allocatable::Q38(:,:)
            real(kind=8),allocatable::Q39(:,:)
            real(kind=8),allocatable::Q40(:,:)
            real(kind=8),allocatable::Q41(:,:)
            real(kind=8),allocatable::Q42(:,:)
            real(kind=8),allocatable::Q43(:,:)
            real(kind=8),allocatable::X1(:,:)
            real(kind=8),allocatable::X2(:,:)
            real(kind=8),allocatable::X3(:,:)


            allocate(accum(N2+1:N3,N0+1:N2))
            accum = 0.0d0

            allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N0,N1,N1,N3,N1,N3,IntR,D1)
            allocate(F2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
            !call reorder562314(N2,N3,N1,N3,N1,N3,N0,N2,N0,N1,N0,N1, &
            !    N0,N1,N0,N1,N1,N3,N1,N3,N2,N3,N0,N2,t3B,F2)
            call reorder_stripe(6,shape(t3b),size(t3b),'562314',t3b,f2)
            allocate(Q1(N2+1:N3,N0+1:N2))
            I2=K2*K4
            I3=K3*K3*K1*K1
            call EGEMM2(I2,I3,D1,F2,Q1)
            deallocate(D1)
            deallocate(F2)

            allocate(X1(N1+1:N3,N0+1:N1))
            X1=0.0d0
            X1=X1+0.250*Q1
            deallocate(Q1)

            allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N0,N1,N2,N3,N1,N3,IntM,D1)
            allocate(F2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
            !call reorder461325(N2,N3,N2,N3,N1,N3,N0,N2,N0,N2,N0,N1, &
            !    N0,N2,N0,N1,N2,N3,N1,N3,N2,N3,N0,N2,t3C,F2)
            call reorder_stripe(6,shape(t3c),size(t3b),'461325',t3c,f2)
            allocate(Q2(N2+1:N3,N0+1:N2))
            I2=K2*K4
            I3=K3*K4*K1*K2
            call EGEMM2(I2,I3,D1,F2,Q2)
            deallocate(D1)
            deallocate(F2)

            X1=X1+Q2
            deallocate(Q2)

            allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
            allocate(F2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
            !call reorder451236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2, &
            !    N0,N2,N0,N2,N2,N3,N2,N3,N2,N3,N0,N2,t3D,F2)
            call reorder_stripe(6,shape(t3d),size(t3d),'451236',t3d,f2)
            allocate(Q3(N2+1:N3,N0+1:N2))
            I2=K2*K4
            I3=K4*K4*K2*K2
            call EGEMM2(I2,I3,D1,F2,Q3)
            deallocate(D1)
            deallocate(F2)

            X1=X1+0.250*Q3
            deallocate(Q3)
            accum = accum + x1
            deallocate(X1)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
            call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N1,N3,N0,N1,N1,N3,IntR,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q18(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q18)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
            allocate(Q19(N0+1:N2,N2+1:N3))
            I2=K4*K2
            I3=K3*K1
            call EGEMM2(I2,I3,Q18,D2,Q19)
            deallocate(D2)
            deallocate(Q18)

            allocate(X2(N1+1:N3,N0+1:N1))
            X2=0.0d0
            call sum21(N1,N3,N0,N1,X2,Q19, 1.000)
            deallocate(Q19)

            allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                N0,N1,N1,N3,t1A,B2)
            allocate(Q20(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K3*K1
            call EGEMM1(I1,I3,D1,B2,Q20)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2, &
                N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
            allocate(Q21(N0+1:N2,N2+1:N3))
            I2=K4*K2
            I3=K4*K2
            call EGEMM2(I2,I3,Q20,D2,Q21)
            deallocate(D2)

            call sum21(N1,N3,N0,N1,X2,Q21, 1.000)
            deallocate(Q21)

            allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
            call reorder2134(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
            allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
            call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
            allocate(Q28(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K3*K1*K2
            call EGEMM(I1,I2,I3,D1,D2,Q28)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N2+1:N3,N2+1:N3))
            call reorder21(N2,N3,N2,N3, &
                N2,N3,N2,N3,Q28,B1)
            allocate(B2(N2+1:N3,N0+1:N2))
            call reorder12(N2,N3,N0,N2, &
                N2,N3,N0,N2,t1B,B2)
            allocate(Q29(N0+1:N2,N2+1:N3))
            I1=K4
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,B1,B2,Q29)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q28)

            call sum21(N1,N3,N0,N1,X2,Q29,-1.000)
            deallocate(Q29)

            allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder1432(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N1,N2,N3,N1,N3,N0,N2,IntM,D1)
            allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
            allocate(Q30(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K3*K4*K1
            call EGEMM(I1,I2,I3,D1,D2,Q30)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N0+1:N2,N0+1:N2))
            call reorder21(N0,N2,N0,N2, &
                N0,N2,N0,N2,Q30,B1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q31(N2+1:N3,N0+1:N2))
            I1=K2
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,B1,B2,Q31)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q30)

            X2=X2-Q31
            deallocate(Q31)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
            call reorder2413(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N2,N3,N0,N1,N1,N3,IntM,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q32(N0+1:N1,N1+1:N3))
            I1=K3*K1
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q32)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
            call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1, &
                N0,N1,N1,N3,N0,N2,N2,N3,t2B,D2)
            allocate(Q33(N0+1:N2,N2+1:N3))
            I2=K4*K2
            I3=K3*K1
            call EGEMM2(I2,I3,Q32,D2,Q33)
            deallocate(D2)
            deallocate(Q32)

            call sum21(N1,N3,N0,N1,X2,Q33, 1.000)
            deallocate(Q33)

            allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder2143(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
            allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
            call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2, &
                N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
            allocate(Q34(N2+1:N3,N2+1:N3))
            I1=K4
            I2=K4
            I3=K4*K2*K2
            call EGEMM(I1,I2,I3,D1,D2,Q34)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N2+1:N3,N2+1:N3))
            call reorder21(N2,N3,N2,N3, &
                N2,N3,N2,N3,Q34,B1)
            allocate(B2(N2+1:N3,N0+1:N2))
            call reorder12(N2,N3,N0,N2, &
                N2,N3,N0,N2,t1B,B2)
            allocate(Q35(N0+1:N2,N2+1:N3))
            I1=K4
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,B1,B2,Q35)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q34)

            call sum21(N1,N3,N0,N1,X2,Q35,-0.500)
            deallocate(Q35)

            allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
            allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2, &
                N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
            allocate(Q36(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K4*K4*K2
            call EGEMM(I1,I2,I3,D1,D2,Q36)
            deallocate(D1)
            deallocate(D2)

            allocate(B1(N0+1:N2,N0+1:N2))
            call reorder21(N0,N2,N0,N2, &
                N0,N2,N0,N2,Q36,B1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q37(N2+1:N3,N0+1:N2))
            I1=K2
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,B1,B2,Q37)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q36)

            X2=X2-0.500*Q37
            deallocate(Q37)

            allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3, &
                N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q38(N0+1:N2,N2+1:N3))
            I1=K4*K2
            I3=K4*K2
            call EGEMM1(I1,I3,D1,B2,Q38)
            deallocate(D1)
            deallocate(B2)

            allocate(D2(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
            call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2, &
                N0,N2,N2,N3,N0,N2,N2,N3,t2C,D2)
            allocate(Q39(N0+1:N2,N2+1:N3))
            I2=K4*K2
            I3=K4*K2
            call EGEMM2(I2,I3,Q38,D2,Q39)
            deallocate(D2)

            call sum21(N1,N3,N0,N1,X2,Q39, 1.000)
            deallocate(Q39)
            accum = accum + x2
            deallocate(X2)

            allocate(B1(N2+1:N3,N0+1:N2))
            call reorder21(N0,N2,N2,N3, &
                N2,N3,N0,N2,Q20,B1)
            allocate(B2(N2+1:N3,N0+1:N2))
            call reorder12(N2,N3,N0,N2, &
                N2,N3,N0,N2,t1B,B2)
            allocate(Q40(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,B1,B2,Q40)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q20)

            allocate(B1(N0+1:N2,N0+1:N2))
            call reorder21(N0,N2,N0,N2, &
                N0,N2,N0,N2,Q40,B1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q41(N2+1:N3,N0+1:N2))
            I1=K2
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,B1,B2,Q41)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q40)

            allocate(X3(N1+1:N3,N0+1:N1))
            X3=0.0d0
            X3=X3-Q41
            deallocate(Q41)

            allocate(B1(N2+1:N3,N0+1:N2))
            call reorder21(N0,N2,N2,N3, &
                N2,N3,N0,N2,Q38,B1)
            allocate(B2(N2+1:N3,N0+1:N2))
            call reorder12(N2,N3,N0,N2, &
                N2,N3,N0,N2,t1B,B2)
            allocate(Q42(N0+1:N2,N0+1:N2))
            I1=K2
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,B1,B2,Q42)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q38)

            allocate(B1(N0+1:N2,N0+1:N2))
            call reorder21(N0,N2,N0,N2, &
                N0,N2,N0,N2,Q42,B1)
            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                N0,N2,N2,N3,t1B,B2)
            allocate(Q43(N2+1:N3,N0+1:N2))
            I1=K2
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,B1,B2,Q43)
            deallocate(B1)
            deallocate(B2)
            deallocate(Q42)

            X3=X3-Q43
            deallocate(Q43)
            accum = accum + x3
            deallocate(X3)

            ! Projection of singles B and outer product T_1B
            forall(b=n2+1:n3, a=n2+1:n3, j=n0+1:n2, i=n0+1:n2)
                v2c(b,a,j,i) = v2c(b,a,j,i) &
                    + accum(b,j) * t1b(a,i) &
                    - accum(a,j) * t1b(b,i) &
                    - accum(b,i) * t1b(a,j) &
                    + accum(a,i) * t1b(b,j)
            end forall

            ! Projection of singles B and outer product T_1A
            forall(b=n2+1:n3, a=n1+1:n3, j=n0+1:n2, i=n0+1:n1)
                v2b(b,a,j,i) = v2b(b,a,j,i) &
                    + accum(b,j) * t1a(a,i)
            end forall


            deallocate(accum)

        end subroutine t2B_disconnected

        subroutine v_t1_t1(froz, occ_a, occ_b, orbs, &
                v_aa, v_ab, v_bb, &
                t1a, t1b, &
                e1a1a, e1a1b, e1b1b)

            integer, intent(in) :: froz, occ_a, occ_b, orbs
            real(p), intent(in) :: v_aa(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
            real(p), intent(in) :: v_ab(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
            real(p), intent(in) :: v_bb(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
            real(p), intent(in) :: t1a(occ_a+1:orbs,froz+1:occ_a)
            real(p), intent(in) :: t1b(occ_b+1:orbs,froz+1:occ_b)
            real(p), intent(inout) :: e1a1a, e1a1b, e1b1b

            integer :: m, n, e, f

            ! Calculate e2a,e2b,e2c
            e1a1a = 0.0_p
            e1a1b = 0.0_p
            e1b1b = 0.0_p

            do m=froz+1,occ_a
                do n=froz+1,occ_a
                    do e=occ_a+1,orbs
                        do f=occ_a+1,orbs
                            e1a1a = e1a1a + 0.50_p * v_aa(e,f,m,n) * t1a(f,n) * t1a(e,m)
                        enddo
                    enddo
                enddo
            enddo

            do m=froz+1,occ_b
                do n=froz+1,occ_b
                    do e=occ_b+1,orbs
                        do f=occ_b+1,orbs
                            e1b1b = e1b1b + 0.50_p * v_bb(e,f,m,n) * t1b(f,n) * t1b(e,m)
                        enddo
                    enddo
                enddo
            enddo

            do m=froz+1,occ_a
                do n=froz+1,occ_b
                    do e=occ_a+1,orbs
                        do f=occ_b+1,orbs
                            e1a1b = e1a1b + v_ab(e,f,m,n) * t1a(e,m) * t1b(f,n)
                        enddo
                    enddo
                enddo
            enddo

        end subroutine v_t1_t1

        subroutine v_t2(froz, occ_a, occ_b, orbs, &
                v_aa, v_ab, v_bb, &
                t2a, t2b, t2c, &
                e2a, e2b, e2c)

            integer, intent(in) :: froz, occ_a, occ_b, orbs
            real(p), intent(in) :: v_aa(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
            real(p), intent(in) :: v_ab(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
            real(p), intent(in) :: v_bb(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
            real(p), intent(in) :: t2a(occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a)
            real(p), intent(in) :: t2b(occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_b,froz+1:occ_a)
            real(p), intent(in) :: t2c(occ_b+1:orbs,occ_b+1:orbs,froz+1:occ_b,froz+1:occ_b)
            real(p), intent(inout) :: e2a, e2b, e2c

            integer :: m, n, e, f

            ! Calculate e2a,e2b,e2c
            e2a = 0.0_p
            e2b = 0.0_p
            e2c = 0.0_p

            do m=froz+1,occ_a
                do n=froz+1,occ_a
                    do e=occ_a+1,orbs
                        do f=occ_a+1,orbs
                            e2a = e2a + 0.25_p * v_aa(e,f,m,n) * t2a(f,e,n,m)
                        enddo
                    enddo
                enddo
            enddo

            do m=froz+1,occ_b
                do n=froz+1,occ_b
                    do e=occ_b+1,orbs
                        do f=occ_b+1,orbs
                            e2c = e2c + 0.25_p * v_bb(e,f,m,n) * t2c(f,e,n,m)
                        enddo
                    enddo
                enddo
            enddo

            do m=froz+1,occ_a
                do n=froz+1,occ_b
                    do e=occ_a+1,orbs
                        do f=occ_b+1,orbs
                            e2b = e2b + v_ab(e,f,m,n) * t2b(f,e,n,m)
                        enddo
                    enddo
                enddo
            enddo

        end subroutine v_t2

    end module contract_doubles_ext_cor
