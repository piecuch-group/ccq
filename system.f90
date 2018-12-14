module system

    use const, only: dp, sp

    type acc_t
        real(sp) :: t2t2_t2(5)
        real(sp) :: t3_t2(2)
        real(sp) :: t1t3_t2(4)
        real(sp) :: t2t2_t3(3)
        real(sp) :: t2t3_t3(5)
    end type acc_t

    type ints_t
        real(dp), allocatable :: f_a(:,:)
        real(dp), allocatable :: f_b(:,:)
        real(dp), allocatable :: v_aa(:,:,:,:)
        real(dp), allocatable :: v_ab(:,:,:,:)
        real(dp), allocatable :: v_bb(:,:,:,:)
    end type ints_t

    type sys_t
        ! Molecular system information
        integer :: froz
        integer :: occ_a
        integer :: occ_b
        integer :: orbs
        integer :: mult

        ! Active partitioning
        integer :: act_occ_a
        integer :: act_occ_b
        integer :: act_unocc_a
        integer :: act_unocc_b

        ! Integrals
        type(ints_t) :: ints

        real(dp) :: en_repul
        real(dp) :: en_ref
    end type sys_t

    type cc_t
        ! CC vector
        real(dp), allocatable :: t_vec(:)
        integer :: t_size
        integer :: pos(20)


        ! ACC data
        type(acc_t) :: acc

        ! Correlation energy
        real(dp) :: en_cor
    end type cc_t

    type run_t
        ! Solver information
        real(dp) :: shift
        real(dp) :: tol
        integer :: diis_space
        logical :: restart
        integer :: max_iter
        character(len=255) :: label

        ! Coupled-cluster level information
        integer :: act_ind_t
        integer :: act_ind_q
        character(len=20) :: calc_type
        logical :: ext_cor

        ! Compat layer
        character(len=6) :: lvl
        character(len=6) :: lvl_t
        character(len=6) :: lvl_q

        ! I/O information
        character(len=255) :: config_file
        character(len=255) :: output_file
        character(len=255) :: onebody_file
        character(len=255) :: twobody_file
        character(len=255) :: bin_file
    end type run_t


end module system
