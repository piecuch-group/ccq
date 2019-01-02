program main

    use calc_driver, only: run_calcs
    use system, only: sys_t, run_t, cc_t, ints_t
    use parser, only: get_opts, get_config
    use printing, only: init_print, print_header, print_config, close_print

    implicit none

    type(sys_t) :: sys
    type(run_t) :: run
    type(cc_t) :: cc

    ! Load configurations
    call get_opts(sys, run)
    call get_config(sys, run, cc)

    ! Initialize printing
    call init_print(run)

    call print_header(run%uuid)
    if (run%config%echo) call print_config(run%config)

    ! Run calculations
    call run_calcs(sys, run, cc)


    call close_print()


end program
