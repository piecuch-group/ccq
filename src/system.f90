module system

    ! Module holding the system types. These include
    ! sys_t which holds the molecular system information,
    ! run_t, which contains information about the runtime configuration
    ! ints_t, which hold the molecular integrals, etc.

    use const, only: int_32, p, sp, line_len, c_len
    use basis_types, only: basis_t

    type ints_t
        ! Bare integrals
        real(p), allocatable :: e1int(:,:)
        real(p), allocatable :: e2int(:,:,:,:)


        ! Unsorted integrals for older CC codes and CCSDTQ derivatives
        real(p), allocatable :: f_a(:,:)
        real(p), allocatable :: f_b(:,:)
        real(p), allocatable :: v_aa(:,:,:,:)
        real(p), allocatable :: v_ab(:,:,:,:)
        real(p), allocatable :: v_bb(:,:,:,:)

        ! Sorted integrals for faster CC calculations
        ! ------------------------------------------
        ! Main integrals
        real(p), allocatable :: fahh(:,:)
        real(p), allocatable :: fahp(:,:)
        real(p), allocatable :: fapp(:,:)
        real(p), allocatable :: fbhh(:,:)
        real(p), allocatable :: fbhp(:,:)
        real(p), allocatable :: fbpp(:,:)
        real(p), allocatable :: vahhhh(:,:,:,:)
        real(p), allocatable :: vahhhp(:,:,:,:)
        real(p), allocatable :: vahhpp(:,:,:,:)
        real(p), allocatable :: vahphp(:,:,:,:)
        real(p), allocatable :: vahppp(:,:,:,:)
        real(p), allocatable :: vbhhhh(:,:,:,:)
        real(p), allocatable :: vbhhhp(:,:,:,:)
        real(p), allocatable :: vbhhph(:,:,:,:)
        real(p), allocatable :: vbhhpp(:,:,:,:)
        real(p), allocatable :: vbhphp(:,:,:,:)
        real(p), allocatable :: vbhpph(:,:,:,:)
        real(p), allocatable :: vbphph(:,:,:,:)
        real(p), allocatable :: vbhppp(:,:,:,:)
        real(p), allocatable :: vbphpp(:,:,:,:)
        real(p), allocatable :: vchhhh(:,:,:,:)
        real(p), allocatable :: vchhhp(:,:,:,:)
        real(p), allocatable :: vchhpp(:,:,:,:)
        real(p), allocatable :: vchphp(:,:,:,:)
        real(p), allocatable :: vchppp(:,:,:,:)
        ! Active space integrals integrals. Truncated to within the active space.
        real(p), allocatable :: vaappp(:,:,:,:)
        real(p), allocatable :: vbappp(:,:,:,:)
        real(p), allocatable :: vbpapp(:,:,:,:)
        real(p), allocatable :: vcappp(:,:,:,:)

    end type ints_t

    type sys_t
        ! Molecular system information
        integer :: froz
        integer :: occ_a
        integer :: occ_b
        integer :: orbs
        integer :: mult

        character(len=5) :: point_group = ''

        ! Spin orbital molecular information
        integer :: froz_spin
        integer :: nel
        integer :: nvirt
        integer :: nalpha
        integer :: nbeta
        integer :: nvirt_alpha
        integer :: nvirt_beta
        type(basis_t) :: basis

        ! Active partitioning
        integer :: act_occ
        integer :: act_unocc

        integer :: act_occ_a
        integer :: act_occ_b
        integer :: act_unocc_a
        integer :: act_unocc_b

        ! Integrals
        type(ints_t) :: ints

        ! Initial energies
        integer, allocatable :: orbital_syms(:)
        real(p), allocatable :: orbital_energies(:)
        real(p) :: en_repul
        real(p) :: en_ref

    end type sys_t

    type config_t
        character(len=c_len) :: filename
        character(len=line_len) :: lines(200)
        integer :: file_size = 0

        logical :: echo = .false.
    end type config_t

    type run_t
        ! Solver information
        real(p) :: shift
        real(p) :: tol
        integer :: diis_space
        logical :: restart
        integer :: max_iter
        logical :: rhf

        ! Parallel
        integer(int_32) :: num_threads = 1_int_32

        ! Calculation ID
        character(len=c_len) :: label
        character(len=37) :: uuid

        ! Coupled-cluster level information
        integer :: act_ind_t
        integer :: act_ind_q
        character(len=20) :: calc_type
        logical :: ext_cor
        ! Use singles and doubles
        logical :: ext_cor_sd

        ! Calc type specifics
        logical :: sorted_ints
        logical :: hbar
        logical :: lcc
        logical :: mm_23
        logical :: stoch

        ! Compatibility layer
        character(len=6) :: lvl
        logical :: lvl_t
        logical :: lvl_q

        ! I/O information
        type(config_t) :: config
        logical :: keep_bin

        character(len=c_len) :: h5_master_file

        ! External correcion I/O
        character(len=c_len) :: ext_cor_file
        logical :: ext_cor_file_h5

        character(len=c_len) :: sym_file

        character(len=c_len) :: output_file
        character(len=c_len) :: onebody_file
        character(len=c_len) :: twobody_file
        character(len=c_len) :: bin_file

        character(len=c_len) :: fcidump

    end type run_t

end module system
