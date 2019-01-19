module cc_types

    use const, only: p, sp, dp
    use ext_cor_types, only: ext_cor_t

    type acc_t
        real(sp) :: t2t2_t2(5)
        real(sp) :: t3_t2(2)
        real(sp) :: t1t3_t2(4)
        real(sp) :: t2t2_t3(3)
        real(sp) :: t2t3_t3(5)

        real(dp), allocatable :: t2_mc(:)
    end type acc_t


    type hbar_t
        real(p), allocatable :: a(:,:)
        real(p), allocatable :: b(:,:)
        real(p), allocatable :: aa(:,:,:,:)
        real(p), allocatable :: ab(:,:,:,:)
        real(p), allocatable :: bb(:,:,:,:)
    end type hbar_t

    type cc_t
        ! CC vector
        real(p), allocatable :: t_vec(:)
        integer :: t_size
        integer :: pos(20)

        ! LCC vector
        real(p), allocatable :: l_vec(:)
        real(p), allocatable :: lh_vec(:)
        integer :: l_size

        ! Hbar data
        type(hbar_t) :: hbar

        ! ACC data
        type(acc_t) :: acc

        ! Externally corrected data
        type(ext_cor_t) :: ext_cor

        ! Correlation energy
        real(p) :: en_cor
        real(p) :: mm_en_cor_a
        real(p) :: mm_en_cor_b
        real(p) :: mm_en_cor_c
        real(p) :: mm_en_cor_d
    end type cc_t

contains

    subroutine init_hbar(sys, cc)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc

        allocate(cc%hbar%a(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
        allocate(cc%hbar%b(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
        allocate(cc%hbar%aa(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, &
            sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
        allocate(cc%hbar%ab(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, &
            sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))
        allocate(cc%hbar%bb(sys%froz+1:sys%orbs,sys%froz+1:sys%orbs, &
            sys%froz+1:sys%orbs,sys%froz+1:sys%orbs))

        cc%hbar%a = 0.0_p
        cc%hbar%b = 0.0_p
        cc%hbar%aa = 0.0_p
        cc%hbar%ab = 0.0_p
        cc%hbar%bb = 0.0_p

    end subroutine init_hbar

    subroutine dealloc_hbar(cc)

        type(cc_t), intent(inout) :: cc

        deallocate(cc%hbar%a)
        deallocate(cc%hbar%b)
        deallocate(cc%hbar%aa)
        deallocate(cc%hbar%ab)
        deallocate(cc%hbar%bb)

    end subroutine dealloc_hbar



end module cc_types
