module energy

    ! This module is responsible for calculating energies
    ! including correlation energies, correction energies,
    ! Hartre--Fock energies, etc.

    implicit none

contains

    function calc_hf_energy(sys, e1int, e2int ) result(hf_energy)

        ! Calculate Hartree--Fock energy

        ! In:
        !   sys: molecular system data
        !   e1int: onebody integrals (Z)
        !   e2int: twobody integrals (V)

        ! Out:
        !   hf_energy: Hartree--Fock energy (without nuclear repulsion)

        use const, only: p
        use system, only: sys_t

        real(p) :: hf_energy
        type(sys_t), intent(in) :: sys
        real(p), allocatable, intent(in) :: e1int(:,:)
        real(p), allocatable, intent(in) :: e2int(:,:,:,:)

        integer :: i, j

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, orbs=>sys%orbs)
            ! Calculate reference energy
            hf_energy = 0.0_p
            do i=1,occ_a
                hf_energy = hf_energy + e1int(i,i)
            enddo
            do i=1,occ_b
                hf_energy = hf_energy + e1int(i,i)
            enddo

            do i=1,occ_a
                do j=1,occ_b
                    hf_energy = hf_energy + e2int(i,j,i,j)
                enddo
            enddo

            do i=1,occ_a
                do j=1,occ_a
                    hf_energy = hf_energy + 0.5_p * (e2int(i,j,i,j)-e2int(i,j,j,i))
                enddo
            enddo

            do i=1,occ_b
                do j=1,occ_b
                    hf_energy = hf_energy + 0.5_p * (e2int(i,j,i,j)-e2int(i,j,j,i))
                enddo
            enddo

        end associate

    end function calc_hf_energy

    function calculate_left_energy(sys, cc) result(energy)

        ! Calculate the energy of a Lambda CC state

        ! In:
        !   sys: molecular system information
        !   cc: CC vectors

        ! Out:
        !   energy: Lambda CC energy

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        real(p) :: energy
        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in) :: cc

        integer :: i

        energy = 0.0_p

        do i=1, cc%l_size
            energy = energy + cc%lh_vec(i) * cc%lh_vec(i)
        enddo

        energy = dsqrt(energy) + cc%en_cor

    end function calculate_left_energy

    function calculate_unsorted_energy(sys, cc) result(energy)

        ! Calculate the CC correlation energy.

        ! In:
        !    sys: system data
        !    cc: coupled cluster data (amplitudes are needed)

        ! Out:
        !    energy: system's correlation energy

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        real(p) :: energy
        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in) :: cc

        real(p), pointer :: t1a(:,:) => null()
        real(p), pointer :: t1b(:,:) => null()
        real(p), pointer :: t2a(:,:,:,:) => null()
        real(p), pointer :: t2b(:,:,:,:) => null()
        real(p), pointer :: t2c(:,:,:,:) => null()

        real(p) :: e1a,e1b,e2a,e2b,e2c,e1a1a,e1b1b,e1a1b

        integer :: m, n, e, f

        t1a(sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a) => cc%t_vec(cc%pos(1):cc%pos(2) - 1)
        t1b(sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b) => cc%t_vec(cc%pos(2):cc%pos(3) - 1)
        t2a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) => &
            cc%t_vec(cc%pos(3):cc%pos(4) - 1)
        t2b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) => &
            cc%t_vec(cc%pos(4):cc%pos(5) - 1)
        t2c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) => &
            cc%t_vec(cc%pos(5):cc%pos(6) - 1)

        ! Calculate e2a,e2b,e2c
        e2a = 0.0_p
        e2b = 0.0_p
        e2c = 0.0_p
        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_a
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_a+1,sys%orbs
                        e2a = e2a + 0.25_p * sys%ints%v_aa(e,f,m,n) * t2a(f,e,n,m)
                    enddo
                enddo
            enddo
        enddo

        do m=sys%froz+1,sys%occ_b
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_b+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e2c = e2c + 0.25_p * sys%ints%v_bb(e,f,m,n) * t2c(f,e,n,m)
                    enddo
                enddo
            enddo
        enddo

        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e2b = e2b + sys%ints%v_ab(e,f,m,n) * t2b(f,e,n,m)
                    enddo
                enddo
            enddo
        enddo

        !  calculate e1a,e1b
        e1a=0.0_p
        e1b=0.0_p
        do e= sys%occ_a+1,sys%orbs
            do m= sys%froz+1,sys%occ_a
                e1a = e1a + sys%ints%f_a(m,e) * t1a(e,m)
            enddo
        enddo
        do e= sys%occ_b+1,sys%orbs
            do m= sys%froz+1,sys%occ_b
                e1b = e1b + sys%ints%f_b(m,e) * t1b(e,m)
            enddo
        enddo

        ! caluculate e1a1a,e1a1b,e1b1b
        e1a1a=0.0_p
        e1a1b=0.0_p
        e1b1b=0.0_p
        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_a
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_a+1,sys%orbs
                        e1a1a = e1a1a + 0.50_p * sys%ints%v_aa(e,f,m,n) * t1a(f,n)*t1a(e,m)
                        ! [TMPDEBUG]
                        !print '(4i4,3es24.10)', e, f, m, n,sys%ints%v_aa(e,f,m,n), t1a(f,n), t1a(e,m)
                    enddo
                enddo
            enddo
        enddo
        do m=sys%froz+1,sys%occ_b
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_b+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e1b1b = e1b1b + 0.50_p * sys%ints%v_bb(e,f,m,n) * t1b(f,n)*t1b(e,m)
                    enddo
                enddo
            enddo
        enddo
        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e1a1b = e1a1b + sys%ints%v_ab(e,f,m,n) * t1a(e,m)*t1b(f,n)
                    enddo
                enddo
            enddo
        enddo

        ! [TMPDEBUG]
        !print *, 'e1a', e1a
        !print *, 'e1b', e1b
        !print *, 'e2a', e2a
        !print *, 'e2b', e2b
        !print *, 'e2c', e2c
        !print *, 'e1a1a', e1a1a
        !print *, 'e1a1b', e1a1b
        !print *, 'e1b1b', e1b1b
        !print *, 'v*t2', e2a + e2b + e2c
        energy = e1a + e1b + e2a + e2b + e2c + e1a1a + e1a1b + e1b1b

    end function calculate_unsorted_energy

    function calculate_sorted_energy(sys, cc) result(energy)

        ! Calculate the CC correlation energy using sorted integrals.

        ! In:
        !    sys: system data
        !    cc: coupled cluster data (amplitudes are needed)

        ! Out:
        !    energy: system's correlation energy

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        real(p) :: energy
        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in) :: cc

        real(p), pointer :: t1A(:,:) => null()
        real(p), pointer :: t1B(:,:) => null()
        real(p), pointer :: t2A(:,:,:,:) => null()
        real(p), pointer :: t2B(:,:,:,:) => null()
        real(p), pointer :: t2C(:,:,:,:) => null()

        real(p) :: e1a,e1b,e2a,e2b,e2c,e1a1a,e1b1b,e1a1b

        integer :: m, n, e, f

        t1a(sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a) => cc%t_vec(cc%pos(1):cc%pos(2) - 1)
        t1b(sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b) => cc%t_vec(cc%pos(2):cc%pos(3) - 1)
        t2a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) => &
            cc%t_vec(cc%pos(3):cc%pos(4) - 1)
        t2b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) => &
            cc%t_vec(cc%pos(4):cc%pos(5) - 1)
        t2c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) => &
            cc%t_vec(cc%pos(5):cc%pos(6) - 1)

        ! Calculate e2a,e2b,e2c
        e2a = 0.0_p
        e2b = 0.0_p
        e2c = 0.0_p

        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_a
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_a+1,sys%orbs
                        e2a=e2a+0.25_p*sys%ints%vahhpp(f,e,n,m)*t2a(f,e,n,m)
                    enddo
                enddo
            enddo
        enddo
        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e2b=e2b+sys%ints%vbhhpp(f,e,n,m)*t2b(f,e,n,m)
                    enddo
                enddo
            enddo
        enddo
        do m=sys%froz+1,sys%occ_b
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_b+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e2c=e2c+0.25_p*sys%ints%vchhpp(f,e,n,m)*t2c(f,e,n,m)
                    enddo
                enddo
            enddo
        enddo
        !  calculate e1a,e1b
        e1a=0.0_p
        e1b=0.0_p
        do e= sys%occ_a+1,sys%orbs
            do m= sys%froz+1,sys%occ_a
                e1a=e1a+sys%ints%fahp(e,m)*t1a(e,m)
            enddo
        enddo
        do e= sys%occ_b+1,sys%orbs
            do m= sys%froz+1,sys%occ_b
                e1b=e1b+sys%ints%fbhp(e,m)*t1b(e,m)
            enddo
        enddo

        ! caluculate e1a1a,e1a1b,e1b1b
        e1a1a=0.0_p
        e1a1b=0.0_p
        e1b1b=0.0_p
        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_a
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_a+1,sys%orbs
                        e1a1a=e1a1a+0.5_p*sys%ints%vahhpp(f,e,n,m)*t1a(f,n)*t1a(e,m)
                    enddo
                enddo
            enddo
        enddo
        do m=sys%froz+1,sys%occ_a
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_a+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e1a1b=e1a1b+sys%ints%vbhhpp(f,e,n,m)*t1a(e,m)*t1b(f,n)
                    enddo
                enddo
            enddo
        enddo
        do m=sys%froz+1,sys%occ_b
            do n=sys%froz+1,sys%occ_b
                do e=sys%occ_b+1,sys%orbs
                    do f=sys%occ_b+1,sys%orbs
                        e1b1b=e1b1b+0.5_p*sys%ints%vchhpp(f,e,n,m)*t1b(f,n)*t1b(e,m)
                    enddo
                enddo
            enddo
        enddo

        energy = e1a + e1b + e2a + e2b + e2c + e1a1a + e1a1b + e1b1b

    end function calculate_sorted_energy

end module energy