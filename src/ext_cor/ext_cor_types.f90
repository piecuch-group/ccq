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

end module ext_cor_types
