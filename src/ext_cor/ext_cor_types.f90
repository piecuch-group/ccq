module ext_cor_types

    use const, only: i0, p

    type ext_cor_t
        integer :: doubles_nconf
        integer(i0), allocatable :: doubles_conf(:,:)
        real(p), allocatable :: doubles_proj(:)

        real(p), allocatable :: t2a(:,:,:,:)
        real(p), allocatable :: t2b(:,:,:,:)
        real(p), allocatable :: t2c(:,:,:,:)
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

end module ext_cor_types
