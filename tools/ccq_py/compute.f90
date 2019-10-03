module compute

    ! python wrapper around ccq

    use const, only: p
    use system, only: sys_t, run_t
    use cc_types, only: cc_t

    implicit none

    type(sys_t), private :: sys
    type(run_t), private :: run
    type(cc_t), private :: cc

    real(kind=8), allocatable, public :: t_vec(:)

contains

    subroutine configure(froz, nel, nvirt, &
            onebody, twobody, &
            rhf, tol, calc_type)

        ! [TODO] extend this api

        ! Configure a ccq calculation

        use parser, only: set_default_options, get_calc_macros
        use printing, only: init_print

        integer, intent(in) :: froz, nel, nvirt
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
        sys%froz = froz / 2
        sys%nel = nel
        sys%occ_a = sys%nel / 2 + sys%froz
        sys%occ_b = sys%occ_a - (sys%mult - 1) / 2
        sys%nvirt = nvirt
        sys%orbs = sys%froz + sys%occ_a + nvirt / 2

        run%tol = tol
        run%rhf = rhf

        run%onebody_file = trim(onebody)
        run%twobody_file = trim(twobody)


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

        use calc_driver, only: init_system, clean_system
        use printing, only: print_calc_params, print_date, print_summary

        use external_correction, only: ext_cor_driver
        use solver, only: solve_cc, solve_lcc
        use hbar_gen, only: hbar2
        use mm_correct, only: crcc23

        real(kind=8), intent(out) :: hf_energy
        real(kind=8), intent(out) :: correlation_energy
        real(kind=8), intent(out) :: total_energy

        logical(kind=8) :: cc_failed = .false.

        ! Start main wall clock
        call print_date('  ccq started on:')
        call init_system(sys, run, cc)


        ! Print calculation options parameters
        call print_calc_params(sys, run, cc)

        ! Externally corrected CC methods
        ! -------------------------------
        if (run%ext_cor) then
            call ext_cor_driver(sys, run, cc)
        endif

        ! Solve coupled cluster
        ! ---------------------
        call solve_cc(sys, run, cc, cc_failed)

        ! Hbar generation
        ! ---------------
        ! [TODO] improve naming of hbar2
        if (run%hbar) then
            call hbar2(sys, run, cc)
        endif

        ! Solve left coupled cluster
        if (run%lcc) then
            call solve_lcc(sys, run, cc, cc_failed)
        endif

        ! Calculate MM correction
        if (run%mm_23) then
            call crcc23(sys, run, cc)
        endif

        print *, run%calc_type
        print *, run%lvl_t
        print *, cc%t_size
        print *, size(cc%t_vec)
        t_vec = cc%t_vec

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

end module compute
