module ext_cor_types

    use const, only: i0, p

    type ext_cor_t
        integer :: doubles_nconf
        integer(i0), allocatable :: doubles_conf(:,:)
        real(p), allocatable :: doubles_proj(:)

        ! [TODO] refactor this such that vec3_t is being used instead
        real(p), allocatable :: t1a(:,:)
        real(p), allocatable :: t1b(:,:)

        real(p), allocatable :: t2a(:,:,:,:)
        real(p), allocatable :: t2b(:,:,:,:)
        real(p), allocatable :: t2c(:,:,:,:)

        ! [TODO] this should be an Hbar eventually
        ! contains the intermediates required to update T3T1 terms
        ! in the externally corrected methods
        real(p), allocatable :: x1(:,:,:,:)
        real(p), allocatable :: x2(:,:,:,:)
        real(p), allocatable :: x3(:,:,:,:,:,:)
        real(p), allocatable :: x4(:,:,:,:,:,:)
        real(p), allocatable :: x5(:,:,:,:,:,:)
        real(p), allocatable :: x6(:,:,:,:)
        real(p), allocatable :: x7(:,:,:,:)
        real(p), allocatable :: x8(:,:,:,:,:,:)
        real(p), allocatable :: x9(:,:,:,:)
        real(p), allocatable :: x10(:,:,:,:)
        real(p), allocatable :: x11(:,:,:,:,:,:)
        real(p), allocatable :: x12(:,:,:,:)
        real(p), allocatable :: x13(:,:,:,:)
        real(p), allocatable :: x14(:,:,:,:,:,:)

    end type ext_cor_t

    type vec3_t

        real(p), allocatable :: o1_a(:,:)
        real(p), allocatable :: o1_b(:,:)

        real(p), allocatable :: o2_aa(:,:,:,:)
        real(p), allocatable :: o2_ab(:,:,:,:)
        real(p), allocatable :: o2_bb(:,:,:,:)

        real(p), allocatable :: o3_aaa(:,:,:,:,:,:)
        real(p), allocatable :: o3_aab(:,:,:,:,:,:)
        real(p), allocatable :: o3_abb(:,:,:,:,:,:)
        real(p), allocatable :: o3_bbb(:,:,:,:,:,:)

    end type vec3_t

contains

    subroutine alloc_vec3_t(sys, vec3)

        ! Initialize a vector with up to three-body components

        ! In:
        !    sys: system information

        ! In/Out:
        !    vec3: vector with up to three-body components

        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(vec3_t), intent(in out) :: vec3

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            allocate(vec3%o1_a(occ_a+1:total,froz+1:occ_a))
            allocate(vec3%o1_b(occ_b+1:total,froz+1:occ_b))

            allocate(vec3%o2_aa(occ_a+1:total,occ_a+1:total,froz+1:occ_a,froz+1:occ_a))
            allocate(vec3%o2_ab(occ_a+1:total,occ_b+1:total,froz+1:occ_a,froz+1:occ_b))
            allocate(vec3%o2_bb(occ_b+1:total,occ_b+1:total,froz+1:occ_b,froz+1:occ_b))

            allocate(vec3%o3_aaa(occ_a+1:total,occ_a+1:total,occ_a+1:total, &
                 froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
            allocate(vec3%o3_aab(occ_a+1:total,occ_a+1:total,occ_b+1:total, &
                 froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
            allocate(vec3%o3_abb(occ_a+1:total,occ_b+1:total,occ_b+1:total, &
                 froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
            allocate(vec3%o3_bbb(occ_b+1:total,occ_b+1:total,occ_b+1:total, &
                 froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))

            vec3%o1_a = 0.0_p
            vec3%o1_b = 0.0_p
            vec3%o2_aa = 0.0_p
            vec3%o2_ab = 0.0_p
            vec3%o2_bb = 0.0_p
            vec3%o3_aaa = 0.0_p
            vec3%o3_aab = 0.0_p
            vec3%o3_abb = 0.0_p
            vec3%o3_bbb = 0.0_p

        end associate

    end subroutine alloc_vec3_t

    subroutine dealloc_vec3_t(vec3)

        ! Deallocate a vector with up to three-body components

        ! In/Out:
        !    vec3: vector with up to three-body components

        type(vec3_t), intent(in out) :: vec3

        deallocate(vec3%o1_a)
        deallocate(vec3%o1_b)

        deallocate(vec3%o2_aa)
        deallocate(vec3%o2_ab)
        deallocate(vec3%o2_bb)

        deallocate(vec3%o3_aaa)
        deallocate(vec3%o3_aab)
        deallocate(vec3%o3_abb)
        deallocate(vec3%o3_bbb)

    end subroutine dealloc_vec3_t

    subroutine alloc_out_arrays(sys, ext_cor)

        ! Initialize the arrays containing the pre-computed intermediates
        ! required for the Jacobi iterations of externally corrected CC methods

        ! In:
        !    sys: system information

        ! In/Out:
        !    ext_cor: external correction data

        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(ext_cor_t), intent(in out) :: ext_cor

        if (.not. allocated(ext_cor%t1a)) then
            allocate(ext_cor%t1a(sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a))
            ext_cor%t1a = 0.0_p
        endif

        if (.not. allocated(ext_cor%t1b)) then
            allocate(ext_cor%t1b(sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b))
            ext_cor%t1b = 0.0_p
        endif

        if (.not. allocated(ext_cor%t2a)) then
            allocate(ext_cor%t2a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
                 sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a))
            ext_cor%t2a = 0.0_p
        endif

        if (.not. allocated(ext_cor%t2b)) then
            allocate(ext_cor%t2b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
                 sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a))
            ext_cor%t2b = 0.0_p
        endif

        if (.not. allocated(ext_cor%t2c)) then
            allocate(ext_cor%t2c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
                 sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b))
            ext_cor%t2c = 0.0_p
        endif

    end subroutine alloc_out_arrays

end module ext_cor_types
