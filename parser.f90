module parser
!    use printing
    implicit none

    contains

        subroutine get_opts(sys, run)

            use system, only: sys_t, run_t
            use printing, only: print_help, abort_cc

            type(sys_t), intent(inout) :: sys
            type(run_t), intent(inout) :: run

            integer :: arg_cnt
            integer :: i
            character(len=50) :: val_next = ''

            character(len=255) :: tmp_arg

            ! Initialize
            run%config_file = ''
            run%output_file = ''

            arg_cnt = command_argument_count()

            if (arg_cnt < 1) then
                call print_help()
            endif

            do i=1, arg_cnt
                call get_command_argument(i, tmp_arg)

                if (tmp_arg(1:1) == '-') then
                    select case (tmp_arg)
                        case ('--output')
                            val_next = '--output'

                        case ('-h', '--help')
                            call print_help()

                        case default
                            call print_help()

                    end select
                else

                    if (i == arg_cnt) then
                        if (trim(run%config_file) == '') then
                            run%config_file = tmp_arg
                            cycle
                        else
                            call abort_cc('Filename not provided!')
                        endif
                    endif

                    select case (val_next)
                        case ('--output')
                            run%output_file = tmp_arg

                    end select
                    val_next = ''
                endif

            enddo


        end subroutine get_opts

        subroutine get_config(sys, run, cc)

            use const, only: sp, dp, config_unit
            use system, only: sys_t, run_t, cc_t

            use printing, only: abort_cc

            type(sys_t), intent(inout) :: sys
            type(run_t), intent(inout) :: run
            type(cc_t), intent(inout) :: cc

            integer :: nfroz, nocc_spin, nunocc_spin
            integer :: act_occ, act_unocc
            logical :: restart
            integer :: itol

            character(len=255) :: line
            character(len=255) :: option
            character(len=255) :: val


            logical :: t_exists
            integer :: indx
            integer :: ios


            inquire(file=trim(run%config_file), exist=t_exists)
            if (.not. t_exists) call abort_cc('Config file not found')

            open(unit=config_unit,file=trim(run%config_file),status='old')

            ! Default values
            ! [TODO] find a better place for this
            nfroz = 0
            nocc_spin = -1
            nunocc_spin = -1
            sys%mult = 0

            ! CC defaults
            sys%act_occ_a = 0
            sys%act_occ_b = 0
            sys%act_unocc_a = 0
            sys%act_unocc_b = 0

            run%act_ind_t = 0
            run%act_ind_q = 0

            cc%acc%t2t2_t2 = 1.0_sp
            cc%acc%t3_t2 = 1.0_sp
            cc%acc%t1t3_t2 = 1.0_sp
            cc%acc%t2t2_t3 = 1.0_sp
            cc%acc%t2t3_t3 = 1.0_sp


            ! Runtime defaults
            run%ext_cor = .false.
            run%shift = 0.0_dp
            run%restart = .false.
            run%diis_space = 5
            run%max_iter = 60
            run%tol = 1.0e-4_dp
            run%label = ''
            run%onebody_file = 'onebody.inp'
            run%twobody_file = 'twobody.inp'
            run%bin_file = 'output.bin'
            run%calc_type = 'CCSD'
            run%lvl = 'CCSD'


            do
                read(config_unit, '(a)', iostat=ios) line
                if (ios /= 0) exit

                ! Skip comments
                indx = scan(line, '#') + scan(line, '!')
                if (indx /= 0) cycle

                indx = scan(line, '=')
                if (indx == 0 ) then
                    select case (line)
                    case ('restart', 'rest')
                        restart = .true.

                    end select
                else
                    option = trim(line(1:indx-1))
                    val = trim(line(indx+1:))

                    select case (option)
                    ! System options
                    case ('core', 'nfroz', 'frozen', 'froz')
                        read(val, *) nfroz

                    case ('nel', 'electrons')
                        read(val, *) nocc_spin

                    case ('nvir', 'virtuals')
                        read(val, *) nunocc_spin

                    case ('multiplicity', 'mult')
                        read(val, *) sys%mult

                    case ('act_occ')
                        read(val, *) act_occ

                    case ('act_unocc')
                        read(val, *) act_unocc

                    case ('act_ind_t')
                        read(val, *) run%act_ind_t

                    case ('act_ind_q')
                        read(val, *) run%act_ind_q

                    ! ACC options
                    case ('t2t2_t2')
                        read(val, *) cc%acc%t2t2_t2

                    case ('t3_t2')
                        read(val, *) cc%acc%t3_t2

                    case ('t1t3_t2')
                        read(val, *) cc%acc%t1t3_t2

                    case ('t2t2_t3')
                        read(val, *) cc%acc%t2t2_t3

                    case ('t2t3_t3')
                        read(val, *) cc%acc%t2t3_t3

                    ! Development options
                    case ('lvl_t')
                        run%lvl_t = val

                    case ('lvl_q')
                        run%lvl_q = val

                    ! Run configuration options
                    case ('max_iterations', 'max_iter')
                        read(val, *) run%max_iter

                    case ('diis_space', 'diis')
                        read(val, *) run%diis_space

                    case ('tolerance', 'tol', 'itol')
                        read(val, '(i10)', iostat=ios) itol
                        if (ios /= 0) then
                            read(val, *) run%tol
                        else
                            if (itol > 0) print '(a)', 'WARNING: tolerance exponent is positive'
                            run%tol = 1.0_dp ** itol
                        endif

                    case ('calc_type')

                        select case (val)

                        case ('dev')
                            run%calc_type = 'dev'

                        case ('CCSD')
                            run%calc_type = 'CCSD'
                            sys%act_occ_a = -1
                            sys%act_occ_b = -1
                            sys%act_unocc_a= -1
                            sys%act_unocc_b= -1
                            run%act_ind_t = 0
                            run%act_ind_q = 0

                            run%lvl_t = ''
                            run%lvl_q = ''

                        case ('CCSDT')
                            run%calc_type = 'CCSDT'
                            sys%act_occ_a = -1
                            sys%act_occ_b = -1
                            sys%act_unocc_a= -1
                            sys%act_unocc_b= -1
                            run%act_ind_t = 3
                            run%act_ind_q = 0

                            run%lvl_t = 'ccsdt'
                            run%lvl_q = ''

                        case ('CCSDTQ')
                            run%calc_type = 'CCSDTQ'
                            sys%act_occ_a = -1
                            sys%act_occ_b = -1
                            sys%act_unocc_a= -1
                            sys%act_unocc_b= -1
                            run%act_ind_t = 3
                            run%act_ind_q = 3

                            run%lvl_t = 'ccsdt'
                            run%lvl_q = 'ccsdtq'

                        end select

                    case ('label')
                        run%label = val

                    case ('onebody')
                        run%onebody_file = val

                    case ('twobody')
                        run%twobody_file = val

                    case ('output_bin')
                        run%bin_file = val

                    end select
                endif

            enddo


            ! Update final numbers
            ! Convert spin orbital number to spatial
            sys%froz = nfroz / 2
            sys%occ_a = (nfroz + nocc_spin) / 2
            sys%occ_b = sys%occ_a - (sys%mult - 1) / 2
            sys%orbs = (nfroz + nocc_spin + nunocc_spin) / 2
            sys%act_occ_a = act_occ + sys%occ_a - sys%occ_b
            sys%act_occ_b = act_occ
            sys%act_unocc_a = act_unocc
            sys%act_unocc_b = act_unocc + sys%occ_a - sys%occ_b

            ! Checks
            if (nocc_spin == -1 .or. nunocc_spin == -1) then
                call abort_cc('ERROR: nel and nvir keywords must be given.')
            endif

            inquire(file=trim(run%onebody_file), exist=t_exists)
            if (.not. t_exists) call abort_cc('Onebody integral file not found')

            inquire(file=trim(run%twobody_file), exist=t_exists)
            if (.not. t_exists) call abort_cc('Twobody integral file not found')


        end subroutine get_config

end module parser
