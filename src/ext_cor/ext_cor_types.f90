module ext_cor_types

    use const, only: dp

    type excit_t
        real(dp) :: coef

        integer :: excit_sign
        integer :: rank_a
        integer :: rank_b
        integer :: from_a(4), to_a(4)
        integer :: from_b(4), to_b(4)

    end type excit_t

    type c_vec_t

        real(dp), allocatable :: c1_a(:,:)
        real(dp), allocatable :: c1_b(:,:)

        real(dp), allocatable :: c2_aa(:,:,:,:)
        real(dp), allocatable :: c2_ab(:,:,:,:)
        real(dp), allocatable :: c2_bb(:,:,:,:)

        real(dp), allocatable :: c3_aaa(:,:,:,:,:,:)
        real(dp), allocatable :: c3_aab(:,:,:,:,:,:)
        real(dp), allocatable :: c3_abb(:,:,:,:,:,:)
        real(dp), allocatable :: c3_bbb(:,:,:,:,:,:)

    end type c_vec_t

end module ext_cor_types
