program main

    use const, only: t_unit
    use integrals, only: load_ints, unload_ints
    use system, only: sys_t, run_t, cc_t, ints_t
    use parser, only: get_opts, get_config
    use printing, only: init_print, print_header, print_calc_params, print_config, close_print
    use solver, only: solve_cc

    implicit none

    type(sys_t) :: sys
    type(run_t) :: run
    type(cc_t) :: cc

    ! Load configurations
    call get_opts(sys, run)
    call get_config(sys, run, cc)

    ! Initialize printing
    call init_print(run)



    call print_header()
    if (run%config%echo) call print_config(run%config)

    call load_ints(sys, run)
    call print_calc_params(sys, run, cc)


    ! Store T vector in binary file

    ! Solve coupled cluster
    call solve_cc(sys, run, cc)



    call unload_ints(sys)

    call close_print()


end program
