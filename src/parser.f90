module parser
!    use printing
    implicit none

        interface
            subroutine gen_uuid(uuid) bind(c)
                use, intrinsic :: iso_c_binding, only: c_char
                implicit none
                character(kind=c_char) :: uuid
            end subroutine gen_uuid
        end interface

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
            call gen_uuid(run%uuid)
            ! [TODO] solve this
            run%uuid = trim(run%uuid(1:36))
            run%config%filename = ''
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
                        if (trim(run%config%filename) == '') then
                            run%config%filename = tmp_arg
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
            use system, only: sys_t, run_t, cc_t, config_t

            use printing, only: io, abort_cc

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
            integer :: indx, l_indx
            integer :: ios


            call load_config_file(run%config)
            call set_default_options(sys, run, cc)


            ! Default values
            ! [TODO] find a better place for this
            nfroz = 0
            nocc_spin = -1
            nunocc_spin = -1
            act_occ = 0
            act_unocc = 0


            do l_indx=1, run%config%file_size
                line = run%config%lines(l_indx)

                ! Skip comments
                indx = scan(line, '#') + scan(line, '!')
                if (indx /= 0) cycle

                indx = scan(line, '=')
                if (indx == 0 ) then
                    select case (line)
                    case ('restart', 'rest')
                        run%restart = .true.

                    case ('print_config')
                        run%config%echo = .true.

                    end select
                else
                    option = trim(adjustl(line(1:indx-1)))
                    val = trim(adjustl(line(indx+1:)))

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
                        read(val, *,iostat=ios) run%lvl_t
                        if (ios /= 0) call abort_cc('CONFIGURATION ERROR: lvl_t must logical')

                    case ('lvl_q')
                        read(val, *, iostat=ios) run%lvl_q
                        if (ios /= 0) call abort_cc('CONFIGURATION ERROR: lvl_q must logical')

                    ! Run configuration options
                    case ('ext_cor')
                        read(val, *, iostat=ios) run%ext_cor
                        if (ios /= 0) call abort_cc('CONFIGURATION ERROR: ext_cor must logical')

                    case ('shift')
                        read(val, *) run%shift

                    case ('max_iterations', 'max_iter')
                        read(val, *) run%max_iter

                    case ('diis_space', 'diis')
                        read(val, *) run%diis_space

                    case ('tolerance', 'tol', 'itol')
                        read(val, '(i10)', iostat=ios) itol
                        if (ios /= 0) then
                            read(val, *) run%tol
                        else
                            if (itol > 0) print '(a)', 'CONFIGURATION WARNING: tolerance exponent is positive'
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

                            run%lvl_t = .false.
                            run%lvl_q = .false.

                        case ('CCSDT')
                            run%calc_type = 'CCSDT'
                            sys%act_occ_a = -1
                            sys%act_occ_b = -1
                            sys%act_unocc_a= -1
                            sys%act_unocc_b= -1
                            run%act_ind_t = 0
                            run%act_ind_q = 0

                            run%lvl_t = .true.
                            run%lvl_q = .false.

                        case ('CCSDTQ')
                            run%calc_type = 'CCSDTQ'
                            sys%act_occ_a = -1
                            sys%act_occ_b = -1
                            sys%act_unocc_a= -1
                            sys%act_unocc_b= -1
                            run%act_ind_t = 3
                            run%act_ind_q = 3

                            run%lvl_t = .true.
                            run%lvl_q = .true.

                        end select

                    case ('label')
                        run%label = val

                    case ('onebody')
                        run%onebody_file = val

                    case ('twobody')
                        run%twobody_file = val

                    case ('keep_bin')
                        read(val, *) run%keep_bin
                    case ('output_log')
                        run%output_file = val

                    case ('output_bin')
                        run%bin_file = val

                    case ('ext_cor_file')
                        run%ext_cor_file = val

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

            if (trim(run%label) /= '') then
                run%bin_file = 'tvec_'//trim(run%label)//'.bin'
            endif

            ! Error checks
            if (nocc_spin == -1 .or. nunocc_spin == -1) then
                call abort_cc('CONFIGURATION ERROR: nel and nvir keywords must be given.')
            endif

            inquire(file=trim(run%onebody_file), exist=t_exists)
            if (.not. t_exists) call abort_cc('CONFIGURATION ERROR: Onebody integral file not found')

            inquire(file=trim(run%twobody_file), exist=t_exists)
            if (.not. t_exists) call abort_cc('CONFIGURATION ERROR: Twobody integral file not found')

            inquire(file=trim(run%bin_file), exist=t_exists)
            if (t_exists .and. .not. run%restart) &
                write(io, '(a)') 'WARNING: '//trim(run%bin_file)//' already exists'


        end subroutine get_config

        subroutine set_default_options(sys, run, cc)

            use const, only: sp, dp
            use system, only: sys_t, run_t, cc_t

            type(sys_t), intent(inout) :: sys
            type(run_t), intent(inout) :: run
            type(cc_t), intent(inout) :: cc

            ! System
            sys%mult = 0

            ! Run
            run%act_ind_t = 0
            run%act_ind_q = 0
            run%keep_bin = .true.
            run%lvl_t = .false.
            run%lvl_q = .false.
            run%ext_cor = .false.
            run%shift = 0.0_dp
            run%restart = .false.
            run%diis_space = 5
            run%max_iter = 60
            run%tol = 1.0e-4_dp
            run%label = ''
            run%onebody_file = 'onebody.inp'
            run%twobody_file = 'twobody.inp'
            run%bin_file = 'tvec_'//trim(run%uuid)//'.bin'
            run%calc_type = 'CCSD'

            ! CC
            cc%acc%t2t2_t2 = 1.0_sp
            cc%acc%t3_t2 = 1.0_sp
            cc%acc%t1t3_t2 = 1.0_sp
            cc%acc%t2t2_t3 = 1.0_sp
            cc%acc%t2t3_t3 = 1.0_sp

        end subroutine set_default_options

        subroutine load_config_file(config)

            use const, only: line_len, config_unit
            use printing, only: abort_cc
            use system, only: config_t

            type(config_t), intent(inout) :: config

            character(len=line_len) :: line
            integer :: ios
            logical :: t_exists

            associate(filename=>config%filename,lines=>config%lines,file_size=>config%file_size)

                inquire(file=trim(filename), exist=t_exists)
                if (.not. t_exists) &
                    call abort_cc('CONFIGURATION ERROR: configuration file not found')

                open(unit=config_unit,file=trim(filename),status='old')

                do
                    read(config_unit, '(a)', iostat=ios) line
                    if (ios == -1) exit
                    if (ios /= 0) &
                        call abort_cc("CONFIGURATION ERROR: could not read configuration file")

                    ! Load config file into memory
                    file_size = file_size + 1
                    if (file_size > size(config%lines,1)) &
                        call abort_cc("CONFIGURATION ERROR: configuration file too large")
                    lines(file_size) = line
                enddo

            end associate

            close(config_unit)

        end subroutine load_config_file

    end module parser
