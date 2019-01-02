module ext_cor

    implicit none

contains

    subroutine alloc_vec3_t(sys, vec3)

        use const, only: dp
        use ext_cor_types, only: vec3_t
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(vec3_t), intent(inout) :: vec3

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

            vec3%o1_a = 0.0_dp
            vec3%o1_b = 0.0_dp
            vec3%o2_aa = 0.0_dp
            vec3%o2_ab = 0.0_dp
            vec3%o2_bb = 0.0_dp
            vec3%o3_aaa = 0.0_dp
            vec3%o3_aab = 0.0_dp
            vec3%o3_abb = 0.0_dp
            vec3%o3_bbb = 0.0_dp

        end associate

    end subroutine alloc_vec3_t

    subroutine dealloc_vec3_t(vec3)

        use ext_cor_types, only: vec3_t

        type(vec3_t) :: vec3

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


end module ext_cor
