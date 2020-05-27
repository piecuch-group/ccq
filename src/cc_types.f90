module cc_types

    ! This module holds the types defining all CC-related data.
    ! For exmaple, T vector, L vector, Moments, ACC parameters, etc.

    use const, only: p, sp, int_32, i0
    use ext_cor_types, only: ext_cor_t

    implicit none

    ! ACC parameters for scaling square powers of T
    type acc_t
        real(sp) :: t2t2_t2(5)
        real(sp) :: t3_t2(2)
        real(sp) :: t1t3_t2(4)
        real(sp) :: t2t2_t3(3)
        real(sp) :: t2t3_t3(5)

        real(p), allocatable :: t2_mc(:)
    end type acc_t

    ! Similarity transformed Hamiltonian
    ! The indices are layout such that the first half corresponds
    ! to the ket, and the second half, to the bra. For example,
    ! hbar%aa(a,i,k,j) -> <jk|hbar|ia>.
    type hbar_t
        real(p), allocatable :: a(:,:)
        real(p), allocatable :: b(:,:)
        real(p), allocatable :: aa(:,:,:,:)
        real(p), allocatable :: ab(:,:,:,:)
        real(p), allocatable :: bb(:,:,:,:)
        real(p), allocatable :: aaa(:,:,:,:,:,:)
        real(p), allocatable :: aab(:,:,:,:,:,:)
        real(p), allocatable :: abb(:,:,:,:,:,:)
        real(p), allocatable :: bbb(:,:,:,:,:,:)
    end type hbar_t

    ! [TODO] this is temporary
    type stoch_t
        ! Masks
        integer(int_32), allocatable :: o3(:,:,:,:,:,:)
        integer(int_32), allocatable :: o4(:,:,:,:,:,:,:,:)

        ! Stochastic determinant list
        integer(i0), allocatable :: dets(:,:)

        ! Number of loaded determinants
        integer :: dets_size

        ! Reference det
        integer(i0), allocatable :: f_ref(:)
    end type stoch_t

    ! Main CC data
    type cc_t
        ! CC vector
        real(p), allocatable :: t_vec(:)
        integer :: t_size
        integer :: pos(20)

        ! LCC vector
        real(p), allocatable :: l_vec(:)
        real(p), allocatable :: lh_vec(:)
        integer :: l_size

        ! Similarity transformed Hamiltonian data
        type(hbar_t) :: hbar

        ! ACC data
        type(acc_t) :: acc

        ! Externally corrected data
        type(ext_cor_t) :: ext_cor

        ! Stochastic CC data
        type(stoch_t) :: stoch

        ! Correlation energy
        real(p) :: en_cor
        real(p) :: mm_en_cor_a
        real(p) :: mm_en_cor_b
        real(p) :: mm_en_cor_c
        real(p) :: mm_en_cor_d
    end type cc_t

contains

    subroutine init_hbar(sys, cc, rank)

        ! Initialize similiarity transformed Hamiltonian arrays.

        ! In:
        !    sys: system's information
        !    rank: maximum rank of the similarity transformed Hamiltonian
        ! Out:
        ! [TODO] we probably need only the cc%hbar part of CC.
        !    cc: allocated hbar arrays

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        integer, intent(in) :: rank

        ! Allocate arrays over the whole space of excitations
        allocate(cc%hbar%a(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
        allocate(cc%hbar%b(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))

        allocate(cc%hbar%aa(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, &
            sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
        allocate(cc%hbar%ab(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, &
            sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
        allocate(cc%hbar%bb(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, &
            sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))

        ! [TODO] add array allocation checking

        cc%hbar%a = 0.0_p
        cc%hbar%b = 0.0_p
        cc%hbar%aa = 0.0_p
        cc%hbar%ab = 0.0_p
        cc%hbar%bb = 0.0_p

        if (rank > 2 ) then
            allocate(cc%hbar%aaa(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, sys%froz+1:sys%orbs,&
                sys%froz+1:sys%orbs,sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
            allocate(cc%hbar%aab(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, sys%froz+1:sys%orbs,&
                sys%froz+1:sys%orbs,sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
            allocate(cc%hbar%abb(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, sys%froz+1:sys%orbs,&
                sys%froz+1:sys%orbs,sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
            allocate(cc%hbar%bbb(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, sys%froz+1:sys%orbs,&
                sys%froz+1:sys%orbs,sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))

            cc%hbar%aaa = 0.0_p
            cc%hbar%aab = 0.0_p
            cc%hbar%abb = 0.0_p
            cc%hbar%bbb = 0.0_p
        endif

    end subroutine init_hbar

    subroutine dealloc_hbar(cc)

        ! Deinitialize similarity transformed Hamiltonian arrays

        ! In/Out:
        !    cc: allocated hbar arrays

        type(cc_t), intent(inout) :: cc

        deallocate(cc%hbar%a)
        deallocate(cc%hbar%b)
        deallocate(cc%hbar%aa)
        deallocate(cc%hbar%ab)
        deallocate(cc%hbar%bb)

        if (allocated(cc%hbar%aaa)) deallocate(cc%hbar%aaa)
        if (allocated(cc%hbar%aab)) deallocate(cc%hbar%aab)
        if (allocated(cc%hbar%abb)) deallocate(cc%hbar%abb)
        if (allocated(cc%hbar%bbb)) deallocate(cc%hbar%bbb)

    end subroutine dealloc_hbar

    subroutine init_p_space(sys, p_mask)

        ! [TODO] clean p_space work

        use const, only: tmp_unit
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(stoch_t), intent(inout) :: p_mask

        integer :: i, j, k, a, b, c
        integer :: ios

        allocate(p_mask%o3(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a))

        p_mask%o3 = 0

        ! [TODO] this is only temporary
        open(tmp_unit, file="p_space", status="old")
        do
            read(tmp_unit, *, iostat=ios) c, b, a, k, j, i
            if (ios /= 0) exit

            p_mask%o3(c,b,a,k,j,i) = 1
        enddo
        !p_mask%o3 = 1

        close(tmp_unit)

    end subroutine init_p_space

    subroutine init_p_space_slater(sys, filename, rank, stoch)

        ! [TODO] clean p_space work

        use const, only: tmp_unit, i0, max_p_space
        use checking, only: check_allocate
        use determinants, only: gen_f_ref, encode_det
        use excitations, only: get_excitation_level
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        character(len=*), intent(in) :: filename
        type(stoch_t), intent(inout) :: stoch
        integer, intent(in) :: rank

        ! [TODO] make this variable more clear.
        ! Might be temporary due to the i/o of walkers eventually changing
        integer :: id_walkers(2)

        integer :: excit_rank

        integer(i0) :: f(sys%basis%string_len)
        integer :: occ_list(sys%nel)
        integer :: cnt = 0
        integer :: ierr, ios

        ! Generate the reference determinant
        ! [TODO] expand this to other kinds of dets (i.e. not HF)
        allocate(stoch%f_ref(sys%basis%string_len), stat=ierr)
        call check_allocate('stoch%f_ref', sys%basis%string_len, ierr)
        call gen_f_ref(sys, stoch%f_ref)

        ! Allocate determinants matrix. Will hold the P space from QMC simulations
        allocate(stoch%dets(sys%basis%string_len, max_p_space), stat=ierr)
        call check_allocate('stoch%dets', sys%basis%string_len * max_p_space, ierr)

        ! Read walkers file with the corresponding QMC generated determinant list
        ! [TODO] improve the way the lists are communicated. Maybe HDF5?
        open(tmp_unit, file=trim(filename), status='old')
        do
            read(tmp_unit, *, iostat=ios) id_walkers, occ_list
            if (ios /= 0) exit

            call encode_det(sys%basis, occ_list, f)
            excit_rank = get_excitation_level(stoch%f_ref, f)

            if (excit_rank /= rank) cycle

            cnt = cnt + 1
            stoch%dets(:,cnt) = f
        enddo

        close(tmp_unit)

        ! Read p_space mask
        ! [TODO] this should be temporary only
        call init_p_space(sys, stoch)


        ! Set the P space size
        stoch%dets_size = cnt

    end subroutine init_p_space_slater


end module cc_types
