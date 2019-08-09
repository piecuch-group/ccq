module ext_cor_types

    use const, only: i0, dp

    type ext_cor_t
        integer :: doubles_nconf
        integer(i0), allocatable :: doubles_conf(:,:)
        real(dp), allocatable :: doubles_proj(:)

        real(dp), allocatable :: t2a(:,:,:,:)
        real(dp), allocatable :: t2b(:,:,:,:)
        real(dp), allocatable :: t2c(:,:,:,:)
    end type ext_cor_t

    type vec3_t

        real(dp), allocatable :: o1_a(:,:)
        real(dp), allocatable :: o1_b(:,:)

        real(dp), allocatable :: o2_aa(:,:,:,:)
        real(dp), allocatable :: o2_ab(:,:,:,:)
        real(dp), allocatable :: o2_bb(:,:,:,:)

        real(dp), allocatable :: o3_aaa(:,:,:,:,:,:)
        real(dp), allocatable :: o3_aab(:,:,:,:,:,:)
        real(dp), allocatable :: o3_abb(:,:,:,:,:,:)
        real(dp), allocatable :: o3_bbb(:,:,:,:,:,:)

    end type vec3_t

end module ext_cor_types
