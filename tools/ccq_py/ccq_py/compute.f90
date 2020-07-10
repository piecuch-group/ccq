module compute

    ! python wrapper around ccq

    use const, only: p, int_32
    use system, only: sys_t, run_t
    use cc_types, only: cc_t

    implicit none

    type(sys_t), private :: sys
    type(run_t), private :: run
    type(cc_t), private :: cc

    real(kind=8), allocatable, public :: t_vec(:)

    real(kind=8), allocatable, public :: onebody(:,:)
    real(kind=8), allocatable, public :: twobody(:,:,:,:)

contains

    subroutine set_ints(norbs, onebody_in, twobody_in, en_repul)

        integer(int_32), intent(in) :: norbs
        real(kind=8), intent(in) :: onebody_in(norbs, norbs)
        real(kind=8), intent(in) :: twobody_in(norbs, norbs, norbs, norbs)
        real(kind=8), intent(in) :: en_repul

        if (.not. allocated(onebody)) onebody = onebody_in
        if (.not. allocated(twobody)) twobody = twobody_in
        sys%en_repul = en_repul

    end subroutine set_ints

    subroutine configure(froz, nel, nvirt, &
            onebody, twobody, &
            rhf, tol, calc_type)

        ! [TODO] extend this api

        ! Configure a ccq calculation

        use parser, only: set_default_options, get_calc_macros
        use printing, only: init_print

        use symmetry, only: read_sym

        integer(int_32), intent(in) :: froz, nel, nvirt
        character(len=500), intent(in) :: onebody, twobody
        logical, intent(in) :: rhf
        real(kind=8), intent(in) :: tol
        character(len=500), intent(in) :: calc_type

        run%calc_type = trim(calc_type)

        ! [TODO] do something better for the temporary naming of log files
        call init_print("ccq_py_out_tmp.log")
        call set_default_options(sys, run, cc)
        call get_calc_macros(sys, run, cc, calc_type)


        ! Set system data
        sys%froz = int(froz)
        sys%nel = int(nel)
        sys%occ_a = sys%nel / 2
        sys%occ_b = sys%occ_a - (sys%mult - 1) / 2
        sys%nvirt = int(nvirt)
        sys%orbs = sys%occ_a + int(nvirt) / 2

        run%tol = tol
        run%rhf = rhf

        run%onebody_file = trim(onebody)
        run%twobody_file = trim(twobody)

        call read_sym(run%sym_file, sys%orbital_syms, sys%point_group, sys%orbs)

    end subroutine configure


    subroutine run_calculation(hf_energy, correlation_energy, total_energy)

        ! Main routine. This is the entry point for all calculations

        ! In:
        !    sys: molecular system information as set in this module
        !    run: runtime information as set in this module

        ! In/Out:
        !    hf_energy: Hartee--Fock energy
        !    correlation_energy: correlation energy
        !    total_energy: total energy (i.e. HF plus correlation)

        use calc_driver, only: init_vecs, clean_system
        use cc_types, only: init_p_space_slater
        use printing, only: print_calc_params, print_date, print_summary

        use integrals, only: load_ints, load_sorted_ints
        use energy, only: calc_hf_energy, calc_orbital_energy

        use basis_types, only: init_basis_strings
        use excitations, only: init_excitations

        use cc_utils, only: get_t_sizes, get_t_sizes_act, open_t4_files

        use hdf5_io, only: init_h5_file

        use omp_lib, only: omp_set_num_threads


        use external_correction, only: external_correction_driver
        use solver, only: solve_cc, solve_lcc
        use hbar_gen, only: hbar2
        use mm_correct, only: crcc23

        real(kind=8), intent(out) :: hf_energy
        real(kind=8), intent(out) :: correlation_energy
        real(kind=8), intent(out) :: total_energy

        logical(kind=8) :: cc_failed = .false.

        ! Start main wall clock
        call print_date('  ccq started on:')
        ! [TODO] improve calculation type passing maybe use procedure pointers?
        if (run%sorted_ints) then
            call get_t_sizes_act(sys, cc)
        else
            call get_t_sizes(sys, cc, run%calc_type)
        endif

        if (.not. allocated(onebody) .and. .not. allocated(twobody)) then
            ! Load integrals
            call load_ints(sys, run)
            ! [TODO] everything should be sorted in the future
            call load_sorted_ints(sys)
        else
            call process_ints()
            deallocate(onebody, twobody)
        endif


        ! Calculate initial energies
        call calc_orbital_energy(sys, sys%ints%e1int, sys%ints%e2int)
        sys%en_ref = calc_hf_energy(sys, sys%ints%e1int, sys%ints%e2int) + sys%en_repul


        ! Initialize determinant and excitation systems
        call init_basis_strings(sys%basis)
        call init_excitations(sys%basis)

        ! Initialize HDF5 master file
        call init_h5_file(run%h5_master_file)

        ! Initialize CC vectors
        if (run%lvl_q) call open_t4_files(sys, run)
        call init_vecs(run, cc)

        ! Original p space init
        ! [TODO] clean this
        !if (run%stoch) call init_p_space(sys, cc%stoch)
        if (run%stoch) call init_p_space_slater(sys, 'p_space_det', 3, cc%stoch)

        ! Initialize OpenMP threads
        call omp_set_num_threads(run%num_threads)



        ! Print calculation options parameters
        call print_calc_params(sys, run, cc)

        ! Externally corrected CC methods
        ! -------------------------------
        if (run%ext_cor) then
            call external_correction_driver(sys, run, cc)
        endif

        ! Solve coupled cluster
        ! ---------------------
        call solve_cc(sys, run, cc, cc_failed)

        ! Hbar generation
        ! ---------------
        ! [TODO] improve naming of hbar2
        if (run%hbar) then
            call hbar2(sys, cc)
        endif

        ! Solve left coupled cluster
        if (run%lcc) then
            call solve_lcc(sys, run, cc, cc_failed)
        endif

        ! Calculate MM correction
        if (run%mm_23) then
            call crcc23(sys, cc)
        endif

        !print *, run%calc_type
        !print *, run%lvl_t
        !print *, cc%t_size
        !print *, size(cc%t_vec)
        !t_vec = cc%t_vec

        ! Wrap up
        call clean_system(sys, run, cc, cc_failed)
        !call print_summary(sys, run, cc)

        call print_date('  ccq finished on:')

        hf_energy = sys%en_ref
        correlation_energy = cc%en_cor
        total_energy = sys%en_ref + cc%en_cor


    end subroutine run_calculation


    subroutine print_config()

        ! Print the current configuration. To be used with python

        use printing, only: print_calc_params

        call print_calc_params(sys, run, cc)

    end subroutine print_config

    subroutine process_ints()

        ! Load one- and two-body integrals from a file/files.

        ! In:
        !    run: runtime configuration and data

        ! In/Out:
        !    sys: molecular system data. On return, the
        !         integrals will be loaded on sys%ints

        use integrals, only: gen_fock_operator

        use checking, only: check_allocate
        use errors, only: stop_all
        use utils, only: count_file_lines

        integer :: i, j, a, b
        integer :: onebody_lines, orbs

        integer :: ierr


        ! Initialize onebody electronic integrals array
        sys%ints%e1int = onebody


        ! Initialize twobody electronic integrals array
        sys%ints%e2int = twobody


        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, orbs=>sys%orbs)

            if (.not. allocated(sys%ints%f_a)) &
                allocate(sys%ints%f_a(orbs, orbs))

            if (.not. allocated(sys%ints%f_b)) &
                allocate(sys%ints%f_b(orbs, orbs))

            if (.not. allocated(sys%ints%v_aa)) &
                allocate(sys%ints%v_aa(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs))

            if (.not. allocated(sys%ints%v_ab)) &
                allocate(sys%ints%v_ab(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs))

            if (.not. allocated(sys%ints%v_bb)) &
                allocate(sys%ints%v_bb(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs))

            ! Antisymmetrize two-body matrix
            do i=froz+1,orbs
                do j=froz+1,orbs
                    do a=froz+1,orbs
                        do b=froz+1,orbs
                            sys%ints%v_ab(i,j,a,b) = sys%ints%e2int(i,j,a,b)
                            sys%ints%v_aa(i,j,a,b) = sys%ints%e2int(i,j,a,b) - sys%ints%e2int(i,j,b,a)
                            sys%ints%v_bb(i,j,a,b) = sys%ints%e2int(i,j,a,b) - sys%ints%e2int(i,j,b,a)
                        enddo
                    enddo
                enddo
            enddo

            ! Generate fock matrix
            call gen_fock_operator(sys, sys%ints%e1int, sys%ints%e2int)

        end associate

    end subroutine process_ints

end module compute
