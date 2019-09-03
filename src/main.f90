! ccq - A coupled-cluster code with up to quadruply excited cluster components

! Developed in the Piecuch group at the Michigan State University.

program main

    ! ccq main program. This is the point of entrance when running
    ! directly from the shell.

    use calc_driver, only: run_calcs
    use system, only: sys_t, run_t
    use cc_types, only: cc_t
    use parser, only: get_opts, get_config
    use printing, only: init_print, print_header, print_config, close_print

    implicit none

    type(sys_t) :: sys
    type(run_t) :: run
    type(cc_t) :: cc

    ! Parse and load configurations
    call get_opts(sys, run)
    call get_config(sys, run, cc)

    ! Initialize printing system
    call init_print(run)

    ! Print settings, configurations, host and compilationn
    ! formation to the output file
    call print_header(run)
    if (run%config%echo) call print_config(run%config)

    ! Run CC calculations
    call run_calcs(sys, run, cc)

    ! Close output file
    call close_print()

end program
