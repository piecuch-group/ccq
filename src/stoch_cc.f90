module stoch_cc

    implicit none

contains

    subroutine update_stoch_t3(sys, run, cc)

        ! Main update of stochastic CC(P;Q) calculations. This routine will call the
        ! required components to generate the intermediate similarity transformed
        ! Hamiltonian and to convert to and from the Slater determinant representation
        ! of cluster operators.

        ! In:
        !    sys: molecular system being studied.
        !    run: runtime variables and information.
        ! In/Out:
        !    cc: coupled-cluster data (including T vector, energies, etc.) Especifically,
        !        this routine will return the updated T vector.

        use const, only: p
        use checking, only: check_allocate, check_deallocate
        use system, only: sys_t, run_t
        use cc_types, only: cc_t, init_hbar, dealloc_hbar, hbar_t

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        real(p), allocatable :: t3_amps_in(:), t3_amps_out(:)
        real(p), allocatable :: t3_vec_tmp(:)
        integer :: ierr

        ! Allocate temporary amplitude arrays
        allocate(t3_amps_in(cc%stoch%dets_size), stat=ierr)
        call check_allocate('t3_amps_in', cc%stoch%dets_size, ierr)
        allocate(t3_amps_out(cc%stoch%dets_size), stat=ierr)
        call check_allocate('t3_amps_out', cc%stoch%dets_size, ierr)
        t3_amps_out = 0.0_p

        ! Transform T amps into determinantal space
        call t3sp_to_t3det(sys, cc, cc%stoch%f_ref, cc%t_vec, t3_amps_in)

        ! Update similarity transformed Hamiltonian (hbar) (with only diagonal triples)
        ! hbar will only live for as long as the stochastic update runs.
        call init_hbar(sys, cc, 3)

        ! [TODO] invoke actual contraction
        !call update_stoch_hbar(sys, cc, run)
        call contract_h_slater(sys, cc, t3_amps_in, t3_amps_out)
        call contract_hbar_slater(sys, cc, t3_amps_in, t3_amps_out)
        call dealloc_hbar(cc)


        ! Transform determinantal space T into spin integrated T
        allocate(t3_vec_tmp(cc%pos(6):cc%pos(10)-1), stat=ierr)
        call check_allocate('t3_vec_tmp', cc%pos(10) - cc%pos(6), ierr)
        t3_vec_tmp = 0.0_p

        call t3det_to_t3sp(sys, cc, cc%stoch%f_ref, t3_vec_tmp, t3_amps_out)
        !call update_jacobi(sys, cc, t3_vec_tmp)
        call update_stoch_t1_t2(sys, cc, t3_vec_tmp)


        deallocate(t3_vec_tmp, stat=ierr)
        call check_deallocate('t3_vec_tmp', ierr)
        deallocate(t3_amps_in, stat=ierr)
        call check_deallocate('t3_amps_in', ierr)
        deallocate(t3_amps_out, stat=ierr)
        call check_deallocate('t3_amps_out', ierr)

    end subroutine update_stoch_t3

    subroutine contract_h_slater(sys, cc, t3_amps_in, t3_amps_out)

        ! Contract the similarity transform Hamiltonian with T_3 in the Slater
        ! determinant space.

        ! In:
        ! Out:

        use const, only: p, i0
        use determinants, only: decode_det
        use excitations, only: excit_t, get_excitation_level, get_excitation
        use hmat, only: slater0_eham, slater1_eham, get_v
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in) :: cc
        real(p), allocatable, intent(in) :: t3_amps_in(:)
        real(p), allocatable, intent(inout) :: t3_amps_out(:)

        integer :: idet, jdet
        type(excit_t) :: excit
        integer :: nexcit
        integer :: occ_list(sys%nel)

        real(p) :: h_element


        do idet=1, cc%stoch%dets_size
            do jdet=1, cc%stoch%dets_size

                ! Get excitation level
                nexcit = get_excitation_level(cc%stoch%dets(:,idet), cc%stoch%dets(:,jdet))

                ! Skip if idet and jdet differ by more than 3 excitations. Normally,
                ! this would be constrained to 2 if the Hamiltonian contains up to two-body
                ! terms only. However, because we are using a similarity transformed
                ! Hamiltonian, it contains up to three-body terms.
                if (nexcit > 2) cycle

                ! If excitation possible, decode.
                ! [TODO] this double loop is going to become expensive with so much logic
                ! in in. Lets try to optimize this in a better way in the future.
                excit = get_excitation(sys%nel, sys%basis, cc%stoch%dets(:,idet), cc%stoch%dets(:,jdet))
                call decode_det(sys%basis, cc%stoch%dets(:,idet), occ_list)

                if (nexcit == 0) then
                    ! Call 0 difference Slater rules
                    h_element = slater0_eham(sys, occ_list)

                elseif (nexcit == 1) then
                    ! Call 1 difference Slater rules
                    h_element = slater1_eham(sys, occ_list, &
                        excit%from_orb(1), excit%to_orb(1), excit%perm)

                elseif (nexcit == 2) then
                    ! Call 2 difference Slater rules
                    h_element = get_v(sys%ints%v_aa, sys%ints%v_ab, sys%ints%v_bb, &
                        excit%from_orb(1), excit%from_orb(2), &
                        excit%to_orb(1), excit%to_orb(2))

                    if (excit%perm) h_element = -h_element

                endif

                t3_amps_out(idet) = t3_amps_out(idet) + h_element * t3_amps_in(jdet)

            enddo
        enddo

    end subroutine contract_h_slater

    subroutine contract_hbar_slater(sys, cc, t3_amps_in, t3_amps_out)

        ! Contract the similarity transform Hamiltonian with T_3 in the Slater
        ! determinant space.

        ! In:
        ! Out:

        use const, only: p, i0
        use determinants, only: decode_det_full
        use excitations, only: excit_t, get_excitation_level, get_excitation
        use energy, only: calculate_unsorted_energy
        use hmat, only: slater0_simham, slater1_simham, slater2_simham, slater3_simham
        use system, only: sys_t
        use cc_types, only: cc_t

        !use debug_tools, only: debug_t_vector

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in) :: cc
        real(p), allocatable, intent(in) :: t3_amps_in(:)
        real(p), allocatable, intent(inout) :: t3_amps_out(:)

        integer :: idet, jdet
        type(excit_t) :: excit
        integer :: nexcit
        integer :: occ_list(sys%nel)
        integer :: unocc_list(sys%nvirt)

        real(p) :: h_element
        real(p) :: cc_energy

        ! We need the current energy to be able to calculate the update.

        ! <D_I | (barH_N * T_3)_C | D_0> = <D_I | barH_N | D_J> <D_J | T_3 | D_0>
        !       + <D_I | T_3 | D_0> <D_0 | barH_N | D_0>

        ! Here |D_I> and |D_J> are triply excited determinants and <D_0 | barH_N | D_0> is
        ! the energy of the current hbar.
        cc_energy = calculate_unsorted_energy(sys, cc)

        ! [TMPDEBUG]
        !call debug_t_vector(sys, cc)
        !occ_list = (/1,2,3,4/)
        !unocc_list = (/5,6,7,8/)
        occ_list = (/4,5,6,7/)
        unocc_list = (/1,2,3,8/)
        h_element = slater0_simham(sys, cc%hbar, occ_list, unocc_list)
        print *, cc_energy, h_element
        do
            print *, 'Occ list:'
            read(*,*) occ_list
            print *, 'Unocc list:'
            read(*,*) unocc_list
            print '(a,4i4)', 'occs: ', occ_list
            print '(a,4i4)', 'unoccs: ', unocc_list
            h_element = slater0_simham(sys, cc%hbar, occ_list, unocc_list)
            print *, cc_energy, h_element
        enddo
        call exit(0)

        do idet=1, cc%stoch%dets_size

            do jdet=1, cc%stoch%dets_size

                ! Get excitation level
                nexcit = get_excitation_level(cc%stoch%dets(:,idet), cc%stoch%dets(:,jdet))

                ! Skip if idet and jdet differ by more than 3 excitations. Normally,
                ! this would be constrained to 2 if the Hamiltonian contains up to two-body
                ! terms only. However, because we are using a similarity transformed
                ! Hamiltonian, it contains up to three-body terms.
                if (nexcit > 3) cycle

                ! If excitation possible, decode.
                ! [TODO] this double loop is going to become expensive with so much logic
                ! in in. Lets try to optimize this in a better way in the future.
                excit = get_excitation(sys%nel, sys%basis, cc%stoch%dets(:,idet), cc%stoch%dets(:,jdet))
                call decode_det_full(sys%basis, cc%stoch%dets(:,idet), occ_list, unocc_list)

                if (nexcit == 0) then
                    ! Call 0 difference Slater rules
                    h_element = slater0_simham(sys, cc%hbar, occ_list, unocc_list) - cc_energy

                elseif (nexcit == 1) then
                    ! Call 1 difference Slater rules
                    h_element = slater1_simham(sys, cc%hbar, occ_list, unocc_list, &
                        excit%from_orb(1), excit%to_orb(1), excit%perm)

                elseif (nexcit == 2) then
                    ! Call 2 difference Slater rules
                    h_element = slater2_simham(sys, cc%hbar, occ_list, &
                        excit%from_orb(1), excit%from_orb(2), &
                        excit%to_orb(1), excit%to_orb(2), excit%perm)

                elseif (nexcit == 3) then
                    ! Call 3 difference Slater rules
                    h_element = slater3_simham(cc%hbar, &
                        excit%from_orb(1), excit%from_orb(2), excit%from_orb(3), &
                        excit%to_orb(1), excit%to_orb(2), excit%to_orb(3), excit%perm)

                endif

                t3_amps_out(idet) = t3_amps_out(idet) + h_element * t3_amps_in(jdet)
            enddo

        enddo

    end subroutine contract_hbar_slater

    subroutine mask_t3a(sys, cc)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(inout) :: cc

        real(p), pointer :: t3a(:,:,:,:,:,:) => null()

        integer :: i, j, k, a, b, c

        ! Point to the right piece of T vec
        t3a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(6):cc%pos(7)-1)

        do i=sys%froz+1, sys%occ_a-2
            do j=i+1, sys%occ_a-1
                do k=j+1, sys%occ_a
                    do a=sys%occ_a+1, sys%orbs-2
                        do b=a+1, sys%orbs-1
                            do c=b+1, sys%orbs
                                t3a(c,b,a,k,j,i) = t3a(c,b,a,k,j,i) * cc%stoch%o3(a,b,c,i,j,k)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        !do i=sys%froz+1, sys%occ_a
        !    do j=sys%froz+1, sys%occ_a
        !        do k=sys%froz+1, sys%occ_a
        !            do a=sys%occ_a+1, sys%orbs
        !                do b=sys%occ_a+1, sys%orbs
        !                    do c=sys%occ_a+1, sys%orbs
        !                        if (cc%stoch%o3(a,b,c,i,j,k) == 0) then
        !                            t3a(c,b,a,k,j,i) = 0.0_p
        !                        endif
        !                    enddo
        !                enddo
        !            enddo
        !        enddo
        !    enddo
        !enddo

    end subroutine mask_t3a

    subroutine mask_t3b(sys, cc)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(inout) :: cc

        real(p), pointer :: t3b(:,:,:,:,:,:) => null()

        integer :: i, j, k, a, b, c

        ! Point to the right piece of T vec
        t3b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(7):cc%pos(8)-1)

        do i=sys%froz+1, sys%occ_a-1
            do j=i+1, sys%occ_a
                do k=sys%froz+1, sys%occ_b
                    do a=sys%occ_a+1, sys%orbs-1
                        do b=a+1, sys%orbs-1
                            do c=sys%occ_b+1, sys%orbs
                                t3b(c,b,a,k,j,i) = t3b(c,b,a,k,j,i) * cc%stoch%o3(a,b,c,i,j,k)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        !do i=sys%froz+1, sys%occ_a
        !    do j=sys%froz+1, sys%occ_a
        !        do k=sys%froz+1, sys%occ_b
        !            do a=sys%occ_a+1, sys%orbs
        !                do b=sys%occ_a+1, sys%orbs
        !                    do c=sys%occ_b+1, sys%orbs
        !                        if (cc%stoch%o3(a,b,c,i,j,k) == 0) then
        !                            t3b(c,b,a,k,j,i) = 0.0_p
        !                        endif
        !                    enddo
        !                enddo
        !            enddo
        !        enddo
        !    enddo
        !enddo

    end subroutine mask_t3b

    subroutine mask_t3c(sys, cc)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(inout) :: cc

        real(p), pointer :: t3c(:,:,:,:,:,:) => null()

        integer :: i, j, k, a, b, c

        ! Point to the right piece of T vec
        t3c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(8):cc%pos(9)-1)

        do i=sys%froz+1, sys%occ_a
            do j=sys%froz+1, sys%occ_b-1
                do k=j+1, sys%occ_b
                    do a=sys%occ_a+1, sys%orbs
                        do b=sys%occ_b+1, sys%orbs-1
                            do c=b+1, sys%orbs
                                t3c(c,b,a,k,j,i) = t3c(c,b,a,k,j,i) * cc%stoch%o3(a,b,c,i,j,k)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        !do i=sys%froz+1, sys%occ_a
        !    do j=sys%froz+1, sys%occ_b
        !        do k=sys%froz+1, sys%occ_b
        !            do a=sys%occ_a+1, sys%orbs
        !                do b=sys%occ_b+1, sys%orbs
        !                    do c=sys%occ_b+1, sys%orbs
        !                        if (cc%stoch%o3(a,b,c,i,j,k) == 0) then
        !                            t3c(c,b,a,k,j,i) = 0.0_p
        !                        endif
        !                    enddo
        !                enddo
        !            enddo
        !        enddo
        !    enddo
        !enddo


    end subroutine mask_t3c

    subroutine mask_t3d(sys, cc)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(inout) :: cc

        real(p), pointer :: t3d(:,:,:,:,:,:) => null()

        integer :: i, j, k, a, b, c

        ! Point to the right piece of T vec
        t3d(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
            => cc%t_vec(cc%pos(9):cc%pos(10)-1)

        do i=sys%froz+1, sys%occ_b-2
            do j=i+1, sys%occ_b-1
                do k=j+1, sys%occ_b
                    do a=sys%occ_b+1, sys%orbs-2
                        do b=a+1, sys%orbs-1
                            do c=b+1, sys%orbs
                                t3d(c,b,a,k,j,i) = t3d(c,b,a,k,j,i) * cc%stoch%o3(a,b,c,i,j,k)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        !do i=sys%froz+1, sys%occ_b
        !    do j=sys%froz+1, sys%occ_b
        !        do k=sys%froz+1, sys%occ_b
        !            do a=sys%occ_b+1, sys%orbs
        !                do b=sys%occ_b+1, sys%orbs
        !                    do c=sys%occ_b+1, sys%orbs
        !                        if (cc%stoch%o3(a,b,c,i,j,k) == 0) then
        !                            t3d(c,b,a,k,j,i) = 0.0_p
        !                        endif
        !                    enddo
        !                enddo
        !            enddo
        !        enddo
        !    enddo
        !enddo

    end subroutine mask_t3d

    subroutine update_proj_t3(sys, cc, t3_confs, t3_amps_in, t3_amps_out)

        use const, only: p, i0
        use excitations, only: excit_t, get_excitation_level, get_excitation
        use hmat, only: get_v
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        integer(i0), intent(in) :: t3_confs(:,:)
        real(p), intent(in) :: t3_amps_in
        real(p), intent(in) :: t3_amps_out

        !  Local variables
        type(excit_t) :: excit
        integer :: iconf, jconf
        integer :: nexcit

        integer :: i, j, k, a, b, c

        integer :: cnt
        real(p) :: h_element

        ! Read configurations
        do iconf=1, size(t3_confs,2)
            do jconf=1, size(t3_confs,2)

                nexcit = get_excitation_level(t3_confs(:,iconf), t3_confs(:,jconf))
                if (nexcit > 3) cycle

                excit = get_excitation(sys%nel, sys%basis, t3_confs(:,iconf), t3_confs(:,jconf))
                i = excit%from_orb(1)
                j = excit%from_orb(2)
                k = excit%from_orb(3)
                a = excit%to_orb(1)
                b = excit%to_orb(2)
                c = excit%to_orb(3)

                select case (nexcit)
                case (3)
                    i = i
                    !t3_amps_out(c, b, a, k, j, i) = t3_amps_out(c, b, a, k, j, i) + &
                    !    slater3_simham(sys, i, j, k, a, b, c, excit%perm) * &
                    !    t3_amps_int(c, b, a, k, i, j)


                end select

            enddo
        enddo


    end subroutine update_proj_t3

    subroutine t3sp_to_t3det(sys, cc, f_ref, t3_target, t3_amps)

        use const, only: i0, p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in) :: cc
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)
        real(p), allocatable, intent(in) :: t3_target(:)
        real(p), intent(inout) :: t3_amps(:)

        real(p), pointer :: t3(:,:,:,:,:,:) => null()
        integer :: idet
        integer :: i, j, k, a, b, c
        logical :: perm
        real(p) :: lhs_coe


        do idet=1, cc%stoch%dets_size

            call t3_point(sys, f_ref, cc%stoch%dets, idet, cc%pos, t3_target, t3, i, j, k, a, b, c, perm)
            t3_amps(idet) = t3(c,b,a,k,j,i)
            if (perm) t3_amps(idet) = -t3_amps(idet)

        enddo

    end subroutine t3sp_to_t3det

    subroutine t3det_to_t3sp(sys, cc, f_ref, t3_target, t3_amps)

        use const, only: i0, p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in) :: cc
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)
        real(p), allocatable, intent(inout) :: t3_target(:)
        real(p), intent(in) :: t3_amps(:)

        real(p), pointer :: t3(:,:,:,:,:,:) => null()
        integer :: idet
        integer :: i, j, k, a, b, c
        logical :: perm
        real(p) :: lhs_coe


        do idet=1, cc%stoch%dets_size

            call t3_point(sys, f_ref, cc%stoch%dets, idet, cc%pos, t3_target, t3, i, j, k, a, b, c, perm)
            t3(c,b,a,k,j,i) = t3_amps(idet)
            if (perm) t3(c,b,a,k,j,i) = -t3(c,b,a,k,j,i)

        enddo

    end subroutine t3det_to_t3sp

    subroutine t3_point(sys, f_ref, dets, idet, pos, t3_target, t3_pointer, &
            i, j, k, a, b, c, perm)

        use const, only: i0, p
        use excitations, only: excit_t, get_excitation_spin_integrate
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)
        integer(i0), intent(in) :: dets(:,:)
        integer, intent(in) :: idet
        integer, intent(in) :: pos(:)
        real(p), allocatable, target, intent(in) :: t3_target(:)
        real(p), pointer, intent(inout) :: t3_pointer(:,:,:,:,:,:)
        integer, intent(inout) :: i, j, k, a, b, c
        logical, intent(out) :: perm

        type(excit_t) :: excit

        excit = get_excitation_spin_integrate(sys%nel, sys%basis, f_ref, dets(:,idet))
        perm = excit%perm

        select case(excit%nexcit_alpha)
        case (3)
            t3_pointer(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
                sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
                => t3_target(pos(6):pos(7)-1)
            i = excit%from_a(1)
            j = excit%from_a(2)
            k = excit%from_a(3)
            a = excit%to_a(1)
            b = excit%to_a(2)
            c = excit%to_a(3)

        case (2)
            t3_pointer(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
                sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
                => t3_target(pos(7):pos(8)-1)
            i = excit%from_a(1)
            j = excit%from_a(2)
            k = excit%from_b(1)
            a = excit%to_a(1)
            b = excit%to_a(2)
            c = excit%to_b(1)

        case (1)
            t3_pointer(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
                sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
                => t3_target(pos(8):pos(9)-1)
            i = excit%from_a(1)
            j = excit%from_b(1)
            k = excit%from_b(2)
            a = excit%to_a(1)
            b = excit%to_b(1)
            c = excit%to_b(2)

        case (0)
            t3_pointer(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
                sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
                => t3_target(pos(9):pos(10)-1)
            i = excit%from_b(1)
            j = excit%from_b(2)
            k = excit%from_b(3)
            a = excit%to_b(1)
            b = excit%to_b(2)
            c = excit%to_b(3)

        end select

    end subroutine t3_point

    subroutine update_jacobi(sys, cc, t3_vec_tmp)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(inout) :: cc
        real(p), allocatable, target, intent(inout) :: t3_vec_tmp(:)

        integer :: n0, n1, n2, n3
        integer :: k1, k2 ,k3 ,k4
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d
        real(p), pointer :: t3(:,:,:,:,:,:) => null()
        real(p), pointer :: v3(:,:,:,:,:,:) => null()

        integer :: indx


        ! aaa case
        v3(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => t3_vec_tmp(cc%pos(6):cc%pos(7)-1)

        t3(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(6):cc%pos(7)-1)

        call jacobi_update_a(sys, t3, v3)

        ! aab
        v3(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => t3_vec_tmp(cc%pos(7):cc%pos(8)-1)

        t3(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(7):cc%pos(8)-1)

        call jacobi_update_b(sys, t3, v3)

        ! abb
        v3(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
            => t3_vec_tmp(cc%pos(8):cc%pos(9)-1)

        t3(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(8):cc%pos(9)-1)

        call jacobi_update_c(sys, t3, v3)

        ! bbb
        v3(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
            => t3_vec_tmp(cc%pos(9):cc%pos(10)-1)

        t3(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
            => cc%t_vec(cc%pos(9):cc%pos(10)-1)

        call jacobi_update_d(sys, t3, v3)


    end subroutine update_jacobi

    subroutine update_stoch_t1_t2(sys, cc, t3_vec_tmp)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(inout) :: cc
        real(p), allocatable, target, intent(inout) :: t3_vec_tmp(:)

        integer :: n0, n1, n2, n3
        integer :: k1, k2 ,k3 ,k4
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d
        real(p), pointer :: t(:) => null()
        real(p), pointer :: v3(:,:,:,:,:,:) => null()

        integer :: indx

        n0 = sys%froz
        n1 = sys%occ_a
        n2 = sys%occ_b
        n3 = sys%orbs

        k1 = sys%occ_a - sys%froz
        k2 = sys%occ_b - sys%froz
        k3 = sys%orbs - sys%occ_a
        k4 = sys%orbs - sys%occ_b

        t => cc%t_vec

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)
        k3a = cc%pos(6)
        k3b = cc%pos(7)
        k3c = cc%pos(8)
        k3d = cc%pos(9)

        !do indx=cc%pos(6),cc%pos(10)-1
        !    if (isnan(t3_vec_tmp(indx))) then
        !        print *, indx, t3_vec_tmp(indx)
        !    endif
        !enddo


        !v3(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
        !    sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
        !    => t3_vec_tmp(cc%pos(6):cc%pos(7)-1)

        !call t3A_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,0.0_p,V3, &
        !    sys%ints%f_a,sys%ints%f_b,sys%ints%v_aa,sys%ints%v_bb,sys%ints%v_ab, &
        !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)

        !v3(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
        !    sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
        !    => t3_vec_tmp(cc%pos(7):cc%pos(8)-1)

        !call t3B_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,0.0_p,V3, &
        !    sys%ints%f_a,sys%ints%f_b,sys%ints%v_aa,sys%ints%v_bb,sys%ints%v_ab, &
        !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)

        !v3(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
        !    sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
        !    => t3_vec_tmp(cc%pos(8):cc%pos(9)-1)

        !call t3C_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,0.0_p,V3, &
        !    sys%ints%f_a,sys%ints%f_b,sys%ints%v_aa,sys%ints%v_bb,sys%ints%v_ab, &
        !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)

        !v3(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
        !    sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
        !    => t3_vec_tmp(cc%pos(9):cc%pos(10)-1)

        !call t3D_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,0.0_p,V3, &
        !    sys%ints%f_a,sys%ints%f_b,sys%ints%v_aa,sys%ints%v_bb,sys%ints%v_ab, &
        !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)

    end subroutine update_stoch_t1_t2

    subroutine jacobi_update_a(sys, t3a, v3a)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        real(p), pointer :: t3a(:,:,:,:,:,:)
        real(p), pointer :: v3a(:,:,:,:,:,:)

        integer :: i, j, k, a, b, c
        real(p) :: coeleft

        do i=sys%froz+1,sys%occ_a-2
            do j=i+1,sys%occ_a-1
                do k=j+1,sys%occ_a
                    do a=sys%occ_a+1,sys%orbs-2
                        do b=a+1,sys%orbs-1
                            do c=b+1,sys%orbs

                                coeleft = sys%ints%f_a(c,c) + sys%ints%f_a(b,b) + sys%ints%f_a(a,a) -&
                                    sys%ints%f_a(k,k) - sys%ints%f_a(j,j) - sys%ints%f_a(i,i)

                                t3a(c,b,a,k,j,i)=t3a(c,b,a,k,j,i)-v3a(c,b,a,k,j,i)/coeleft
                                t3a(c,b,a,k,i,j)=-t3a(c,b,a,k,j,i)
                                t3a(c,b,a,i,j,k)=-t3a(c,b,a,k,j,i)
                                t3a(c,b,a,i,k,j)= t3a(c,b,a,k,j,i)
                                t3a(c,b,a,j,k,i)=-t3a(c,b,a,k,j,i)
                                t3a(c,b,a,j,i,k)= t3a(c,b,a,k,j,i)
                                t3a(c,a,b,k,j,i)=-t3a(c,b,a,k,j,i)
                                t3a(c,a,b,k,i,j)= t3a(c,b,a,k,j,i)
                                t3a(c,a,b,i,j,k)= t3a(c,b,a,k,j,i)
                                t3a(c,a,b,i,k,j)=-t3a(c,b,a,k,j,i)
                                t3a(c,a,b,j,k,i)= t3a(c,b,a,k,j,i)
                                t3a(c,a,b,j,i,k)=-t3a(c,b,a,k,j,i)
                                t3a(a,b,c,k,j,i)=-t3a(c,b,a,k,j,i)
                                t3a(a,b,c,k,i,j)= t3a(c,b,a,k,j,i)
                                t3a(a,b,c,i,j,k)= t3a(c,b,a,k,j,i)
                                t3a(a,b,c,i,k,j)=-t3a(c,b,a,k,j,i)
                                t3a(a,b,c,j,k,i)= t3a(c,b,a,k,j,i)
                                t3a(a,b,c,j,i,k)=-t3a(c,b,a,k,j,i)
                                t3a(a,c,b,k,j,i)= t3a(c,b,a,k,j,i)
                                t3a(a,c,b,k,i,j)=-t3a(c,b,a,k,j,i)
                                t3a(a,c,b,i,j,k)=-t3a(c,b,a,k,j,i)
                                t3a(a,c,b,i,k,j)= t3a(c,b,a,k,j,i)
                                t3a(a,c,b,j,k,i)=-t3a(c,b,a,k,j,i)
                                t3a(a,c,b,j,i,k)= t3a(c,b,a,k,j,i)
                                t3a(b,c,a,k,j,i)=-t3a(c,b,a,k,j,i)
                                t3a(b,c,a,k,i,j)= t3a(c,b,a,k,j,i)
                                t3a(b,c,a,i,j,k)= t3a(c,b,a,k,j,i)
                                t3a(b,c,a,i,k,j)=-t3a(c,b,a,k,j,i)
                                t3a(b,c,a,j,k,i)= t3a(c,b,a,k,j,i)
                                t3a(b,c,a,j,i,k)=-t3a(c,b,a,k,j,i)
                                t3a(b,a,c,k,j,i)= t3a(c,b,a,k,j,i)
                                t3a(b,a,c,k,i,j)=-t3a(c,b,a,k,j,i)
                                t3a(b,a,c,i,j,k)=-t3a(c,b,a,k,j,i)
                                t3a(b,a,c,i,k,j)= t3a(c,b,a,k,j,i)
                                t3a(b,a,c,j,k,i)=-t3a(c,b,a,k,j,i)
                                t3a(b,a,c,j,i,k)= t3a(c,b,a,k,j,i)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine jacobi_update_a

    subroutine jacobi_update_b(sys, t3b, v3b)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        real(p), pointer :: t3b(:,:,:,:,:,:)
        real(p), pointer :: v3b(:,:,:,:,:,:)

        integer :: i, j, k, a, b, c
        real(p) :: coeleft

        do i=sys%froz+1,sys%occ_a-1
            do j=i+1,sys%occ_a
                do k=sys%froz+1,sys%occ_b
                    do a=sys%occ_a+1,sys%orbs-1
                        do b=a+1,sys%orbs
                            do c=sys%occ_b+1,sys%orbs

                                coeleft = sys%ints%f_b(c,c) + sys%ints%f_a(b,b) + sys%ints%f_a(a,a) -&
                                    sys%ints%f_b(k,k) - sys%ints%f_a(j,j) - sys%ints%f_a(i,i)
                                t3b(c,b,a,k,j,i)=t3b(c,b,a,k,j,i)-v3b(c,b,a,k,j,i)/coeleft
                                t3b(c,b,a,k,i,j)=-t3b(c,b,a,k,j,i)
                                t3b(c,a,b,k,j,i)=-t3b(c,b,a,k,j,i)
                                t3b(c,a,b,k,i,j)= t3b(c,b,a,k,j,i)

                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine jacobi_update_b

    subroutine jacobi_update_c(sys, t3c, v3c)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        real(p), pointer :: t3c(:,:,:,:,:,:)
        real(p), pointer :: v3c(:,:,:,:,:,:)

        integer :: i, j, k, a, b, c
        real(p) :: coeleft

        do i=sys%froz+1,sys%occ_a
            do j=sys%froz+1,sys%occ_b - 1
                do k=j+1,sys%occ_b
                    do a=sys%occ_a+1,sys%orbs
                        do b=sys%occ_b+1,sys%orbs-1
                            do c=b+1,sys%orbs

                                coeleft = sys%ints%f_b(c,c) + sys%ints%f_b(b,b) + sys%ints%f_a(a,a) -&
                                    sys%ints%f_b(k,k) - sys%ints%f_b(j,j) - sys%ints%f_a(i,i)
                                t3c(c,b,a,k,j,i)=t3c(c,b,a,k,j,i)-v3c(c,b,a,k,j,i)/coeleft
                                t3c(c,b,a,j,k,i)=-t3c(c,b,a,k,j,i)
                                t3c(b,c,a,k,j,i)=-t3c(c,b,a,k,j,i)
                                t3c(b,c,a,j,k,i)= t3c(c,b,a,k,j,i)

                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine jacobi_update_c

    subroutine jacobi_update_d(sys, t3d, v3d)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        real(p), pointer :: t3d(:,:,:,:,:,:)
        real(p), pointer :: v3d(:,:,:,:,:,:)

        integer :: i, j, k, a, b, c
        real(p) :: coeleft

        do i=sys%froz+1,sys%occ_b-2
            do j=i+1,sys%occ_b-1
                do k=j+1,sys%occ_b
                    do a=sys%occ_b+1,sys%orbs-2
                        do b=a+1,sys%orbs-1
                            do c=b+1,sys%orbs

                                coeleft = sys%ints%f_b(c,c) + sys%ints%f_b(b,b) + sys%ints%f_b(a,a) -&
                                    sys%ints%f_b(k,k) - sys%ints%f_b(j,j) - sys%ints%f_b(i,i)

                                t3d(c,b,a,k,j,i)=t3d(c,b,a,k,j,i)-v3d(c,b,a,k,j,i)/coeleft
                                t3d(c,b,a,k,i,j)=-t3d(c,b,a,k,j,i)
                                t3d(c,b,a,i,j,k)=-t3d(c,b,a,k,j,i)
                                t3d(c,b,a,i,k,j)= t3d(c,b,a,k,j,i)
                                t3d(c,b,a,j,k,i)=-t3d(c,b,a,k,j,i)
                                t3d(c,b,a,j,i,k)= t3d(c,b,a,k,j,i)
                                t3d(c,a,b,k,j,i)=-t3d(c,b,a,k,j,i)
                                t3d(c,a,b,k,i,j)= t3d(c,b,a,k,j,i)
                                t3d(c,a,b,i,j,k)= t3d(c,b,a,k,j,i)
                                t3d(c,a,b,i,k,j)=-t3d(c,b,a,k,j,i)
                                t3d(c,a,b,j,k,i)= t3d(c,b,a,k,j,i)
                                t3d(c,a,b,j,i,k)=-t3d(c,b,a,k,j,i)
                                t3d(a,b,c,k,j,i)=-t3d(c,b,a,k,j,i)
                                t3d(a,b,c,k,i,j)= t3d(c,b,a,k,j,i)
                                t3d(a,b,c,i,j,k)= t3d(c,b,a,k,j,i)
                                t3d(a,b,c,i,k,j)=-t3d(c,b,a,k,j,i)
                                t3d(a,b,c,j,k,i)= t3d(c,b,a,k,j,i)
                                t3d(a,b,c,j,i,k)=-t3d(c,b,a,k,j,i)
                                t3d(a,c,b,k,j,i)= t3d(c,b,a,k,j,i)
                                t3d(a,c,b,k,i,j)=-t3d(c,b,a,k,j,i)
                                t3d(a,c,b,i,j,k)=-t3d(c,b,a,k,j,i)
                                t3d(a,c,b,i,k,j)= t3d(c,b,a,k,j,i)
                                t3d(a,c,b,j,k,i)=-t3d(c,b,a,k,j,i)
                                t3d(a,c,b,j,i,k)= t3d(c,b,a,k,j,i)
                                t3d(b,c,a,k,j,i)=-t3d(c,b,a,k,j,i)
                                t3d(b,c,a,k,i,j)= t3d(c,b,a,k,j,i)
                                t3d(b,c,a,i,j,k)= t3d(c,b,a,k,j,i)
                                t3d(b,c,a,i,k,j)=-t3d(c,b,a,k,j,i)
                                t3d(b,c,a,j,k,i)= t3d(c,b,a,k,j,i)
                                t3d(b,c,a,j,i,k)=-t3d(c,b,a,k,j,i)
                                t3d(b,a,c,k,j,i)= t3d(c,b,a,k,j,i)
                                t3d(b,a,c,k,i,j)=-t3d(c,b,a,k,j,i)
                                t3d(b,a,c,i,j,k)=-t3d(c,b,a,k,j,i)
                                t3d(b,a,c,i,k,j)= t3d(c,b,a,k,j,i)
                                t3d(b,a,c,j,k,i)=-t3d(c,b,a,k,j,i)
                                t3d(b,a,c,j,i,k)= t3d(c,b,a,k,j,i)
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine jacobi_update_d

end module stoch_cc
