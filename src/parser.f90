module parser

    ! Module for parsing ccq input files and setting options

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

        ! Read command line options
        ! In:
        !    command line arguments from system shell
        ! In/Out:
        !    sys: system information
        !    run: runtime information

        use system, only: sys_t, run_t
        use printing, only: print_help
        use errors, only: stop_all

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(inout) :: run

        integer :: arg_cnt
        integer :: i
        character(len=50) :: val_next = ''

        character(len=255) :: tmp_arg

        ! Initialize uuid
        call gen_uuid(run%uuid)

        ! [TODO] create a better interface maybe
        run%uuid = trim(run%uuid(1:36))
        run%config%filename = ''
        run%output_file = ''
        run%h5_master_file = 'master_'//trim(run%uuid)//'.h5'

        arg_cnt = command_argument_count()

        ! Print help and exit if no input file is given
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
                        call stop_all('get_opts', 'Input filename not provided!')
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

        ! Read the input config file. [TODO] probably convert this to a better
        ! tech. Either AOTUS, f2py, or forpy

        ! In/Out:
        !     run: runtime configuration (specifically the config file path)
        ! Out:
        !     sys: system information
        !     cc: coupled-cluster information

        use const, only: sp, dp, config_unit, line_len
        use system, only: sys_t, run_t, config_t
        use cc_types, only: cc_t

        use printing, only: io
        use errors, only: stop_all

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(inout) :: run
        type(cc_t), intent(inout) :: cc


        integer :: nfroz, nocc_spin, nunocc_spin
        integer :: act_occ, act_unocc
        logical :: restart
        integer :: itol

        character(len=line_len) :: line
        character(len=line_len) :: option
        character(len=line_len) :: val


        logical :: t_exists
        integer :: indx, l_indx
        integer :: ios


        ! Load input config file and store it in memory (to be printed
        ! on the log if wanted)
        call load_config_file(run%config)

        ! Set default options and parameters
        call set_default_options(sys, run, cc)

        ! Get options and parameters required for each particular
        ! calculation type.
        call get_calc_macros(sys, run, cc)


        ! Initial values for the molecular system
        ! [TODO] find a better place for this
        nfroz = 0
        nocc_spin = -1
        nunocc_spin = -1
        act_occ = 0
        act_unocc = 0

        ! Parse config file
        do l_indx=1, run%config%file_size
            line = run%config%lines(l_indx)

            ! Skip comments
            indx = scan(line, '#') + scan(line, '!')
            if (indx /= 0) cycle
            ! Skip empty lines
            if (trim(line) == '') cycle

            ! Find equal sign and separate keyword from value
            indx = scan(line, '=')

            ! Load flag keywords (i.e. keywords that if present
            ! represent a true value, if missing false)
            if (indx == 0 ) then
                select case (line)
                case ('restart', 'rest')
                    run%restart = .true.

                case ('print_config', 'echo')
                    run%config%echo = .true.

                end select

            else

                ! Parse options
                option = trim(adjustl(line(1:indx-1)))
                val = trim(adjustl(line(indx+1:)))

                ! Molecular system parameters.
                ! All calues are loaded locally in order to be able to check
                ! for errors. [TODO] improve
                select case (option)
                case ('core', 'nfroz', 'frozen', 'froz')
                    ! This is in spin orbital form
                    read(val, *) nfroz

                case ('nel', 'electrons')
                    read(val, *) nocc_spin

                case ('nvir', 'nvirt', 'virtuals')
                    read(val, *) nunocc_spin

                case ('multiplicity', 'mult')
                    read(val, *) sys%mult

                end select

                ! Load specific options based on calculation type.
                ! run%calc_type should have been loaded in
                ! get_calc_macros.
                select case (run%calc_type)
                case ('CCSD', 'CCSDT', 'CCSDTQ', 'stoch-CC')
                    call get_run_opts(run, option, val)

                case ('CADFCIQMC', 'DCSD-MC')
                    call get_run_opts(run, option, val)
                    call get_ext_cor_opts(sys, run, cc, option, val)

                case ('ACC')
                    call get_run_opts(run, option, val)
                    call get_acc_opts(sys, run, cc, option, val)

                case ('CCSDt', 'CC(t;3)')
                    call get_run_opts(run, option, val)
                    call get_act_opts(sys, run, cc, option, val)

                case ('dev')
                    ! This is a special macro that lets one set every
                    ! available option. Use at your own discretion.
                    call get_run_opts(run, option, val)
                    call get_ext_cor_opts(sys, run, cc, option, val)
                    call get_act_opts(sys, run, cc, option, val)
                    call get_acc_opts(sys, run, cc, option, val)

                case default
                    ! If no calc_type is given, stop.
                    call stop_all('get_config', 'CONFIGURATION ERROR: calc_type keyword not found')

                end select
            endif

        enddo

        ! Update final numbers
        ! Convert spin orbital number to spatial
        ! [TODO] move this to a better place
        sys%froz = nfroz / 2
        sys%occ_a = (nfroz + nocc_spin) / 2
        sys%occ_b = sys%occ_a - (sys%mult - 1) / 2
        sys%orbs = (nfroz + nocc_spin + nunocc_spin) / 2

        ! Set active space
        sys%act_occ_b = max(sys%occ_b - sys%act_occ, sys%froz)
        sys%act_occ_a = sys%act_occ_b + sys%occ_a - sys%occ_b
        sys%act_unocc_a = min(sys%occ_a + sys%act_unocc, sys%orbs)
        sys%act_unocc_b = sys%act_unocc_a + sys%occ_a - sys%occ_b

        sys%nel = sys%occ_a + sys%occ_b
        sys%basis%nbasis = 2 * sys%orbs
        sys%nvirt = sys%basis%nbasis - sys%nel


        ! Error checks
        ! [TODO] write a routine for this
        if (nocc_spin == -1 .or. nunocc_spin == -1) then
            call stop_all('get_config', 'CONFIGURATION ERROR: nel and nvirt keywords must be given.')
        endif

        inquire(file=trim(run%onebody_file), exist=t_exists)
        if (.not. t_exists) call stop_all('get_config', 'CONFIGURATION ERROR: Onebody integral file not found')

        inquire(file=trim(run%twobody_file), exist=t_exists)
        if (.not. t_exists) call stop_all('get_config', 'CONFIGURATION ERROR: Twobody integral file not found')
        if (run%restart) then
            inquire(file=trim(run%bin_file), exist=t_exists)
            if (.not. t_exists) call stop_all('get_config', 'CONFIGURATION ERROR: Twobody integral file not found')
        endif

        if (trim(run%label) /= '') then
            if (run%restart) &
                call stop_all('get_config', 'CONFIGURATION ERROR: Restart requires setting output_bin')
            run%bin_file = 'tvec_'//trim(run%label)//'.bin'
        endif

        inquire(file=trim(run%bin_file), exist=t_exists)
        if (t_exists .and. .not. run%restart) &
            write(io, '(a)') 'WARNING: '//trim(run%bin_file)//' already exists'


        if (trim(run%calc_type) == 'CCSDt' &
            .and. sys%act_occ == -1 .and. sys%act_unocc == -1 ) then
            call stop_all('get_config', 'CONFIGURATION ERROR: Active space methods require an active space')
        endif


    end subroutine get_config


    subroutine get_calc_macros(sys, run, cc)

        ! Get calculation type from input file and set default
        ! configurations and parameters
        !
        ! In/Out:
        !     run: runtime information. Specifically the config file
        ! Out:
        !     sys: system information
        !     run: runtime information
        !     cc: CC information

        use const, only: sp, dp, line_len
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(inout) :: run
        type(cc_t), intent(inout) :: cc

        character(len=line_len) :: line
        character(len=line_len) :: option
        character(len=line_len) :: val

        integer :: indx, l_indx

        ! Loop over the config file lines searchinf for calc_type
        do l_indx=1, run%config%file_size
            line = run%config%lines(l_indx)

            ! Skip comments
            indx = scan(line, '#') + scan(line, '!')
            if (indx /= 0) cycle
            ! Skip empty lines
            if (trim(line) == '') cycle
            ! Skip lines without equal sign
            indx = scan(line, '=')
            if (indx == 0 ) cycle

            option = trim(adjustl(line(1:indx-1)))
            val = trim(adjustl(line(indx+1:)))

            select case (option)

            case ('calc_type')

                select case (val)

                case ('dev')
                    run%calc_type = 'dev'

                case ('CADFCIQMC')
                    run%calc_type = 'CADFCIQMC'
                    run%act_ind_t = 0
                    run%act_ind_q = 0

                    run%ext_cor = .true.
                    run%lvl_t = .true.
                    run%lvl_q = .false.

                case ('DCSD-MC')
                    run%calc_type = 'DCSD-MC'
                    run%act_ind_t = 0
                    run%act_ind_q = 0

                    cc%acc%t2t2_t2 = (/1.0_sp, 0.0_sp, 0.5_sp, 0.5_sp, 0.0_sp/)
                    run%ext_cor = .true.
                    run%lvl_t = .true.
                    run%lvl_q = .false.

                case ('CCSD')
                    run%calc_type = 'CCSD'
                    run%act_ind_t = 0
                    run%act_ind_q = 0

                    run%lvl_t = .false.
                    run%lvl_q = .false.

                case ('CCSDt')
                    run%calc_type = 'CCSDt'
                    run%sorted_ints = .true.
                    sys%act_occ = -1
                    sys%act_unocc= -1
                    run%act_ind_t = 1
                    run%act_ind_q = 0

                    run%lvl_t = .true.
                    run%lvl_q = .false.

                case ('stoch-CC')
                    run%calc_type = 'stoch-CC'
                    run%act_ind_t = 0
                    run%act_ind_q = 0

                    run%lvl_t = .true.
                    run%lvl_q = .false.
                    run%stoch = .true.

                case ('CCT3', 'CCt3', 'CC(t;3)')
                    run%calc_type = 'CC(t;3)'
                    run%sorted_ints = .true.
                    run%hbar = .true.
                    run%lcc = .true.
                    run%mm_23 = .true.
                    sys%act_occ = -1
                    sys%act_unocc= -1
                    run%act_ind_t = 1
                    run%act_ind_q = 0

                    run%lvl_t = .true.
                    run%lvl_q = .false.

                case ('CCSDT')
                    run%calc_type = 'CCSDT'
                    run%act_ind_t = 0
                    run%act_ind_q = 0

                    run%lvl_t = .true.
                    run%lvl_q = .false.

                case ('CCSDTQ')
                    run%calc_type = 'CCSDTQ'
                    run%act_ind_t = 0
                    run%act_ind_q = 0

                    run%lvl_t = .true.
                    run%lvl_q = .true.

                end select
            end select

        enddo

    end subroutine get_calc_macros

    subroutine get_acc_opts(sys, run, cc, option, val)

        ! Get ACC-type methods specific configuration parameters
        !
        ! In:
        !     option: option keyword from the config file
        !     val: option's value
        ! Out:
        !     sys: system information
        !     run: runtime information
        !     cc: CC information

        use const, only: sp, dp
        use errors, only: stop_all
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(inout) :: run
        type(cc_t), intent(inout) :: cc
        character(len=*), intent(in) :: option
        character(len=*), intent(in) :: val

        integer :: ios

        ! ACC options
        select case (option)
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
        end select

    end subroutine get_acc_opts

    subroutine get_act_opts(sys, run, cc, option, val)

        ! Get active-space-type calculation configuration parameters
        !
        ! In:
        !     option: option keyword from the config file
        !     val: option's value
        ! Out:
        !     sys: system information
        !     run: runtime information
        !     cc: CC information

        use const, only: sp, dp
        use errors, only: stop_all
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(inout) :: run
        type(cc_t), intent(inout) :: cc
        character(len=*), intent(in) :: option
        character(len=*), intent(in) :: val

        integer :: ios

        ! [TODO] by-pass this if full calcs
        select case (option)
        case ('act_occ')
            read(val, *) sys%act_occ

        case ('act_unocc')
            read(val, *) sys%act_unocc

        case ('act_ind_t')
            read(val, *) run%act_ind_t

        case ('act_ind_q')
            read(val, *) run%act_ind_q

        case ('lvl_t')
            read(val, *,iostat=ios) run%lvl_t
            if (ios /= 0) call stop_all('get_act_opts', 'CONFIGURATION ERROR: lvl_t must logical')

        case ('lvl_q')
            read(val, *, iostat=ios) run%lvl_q
            if (ios /= 0) call stop_all('get_act_opts', 'CONFIGURATION ERROR: lvl_q must logical')
        end select

    end subroutine get_act_opts

    subroutine get_ext_cor_opts(sys, run, cc, option, val)

        ! Get externally correction configuration parameters
        !
        ! In:
        !     option: option keyword from the config file
        !     val: option's value
        ! Out:
        !     sys: system information
        !     run: runtime information
        !     cc: CC information

        use const, only: sp, dp
        use errors, only: stop_all
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(inout) :: run
        type(cc_t), intent(inout) :: cc
        character(len=*), intent(in) :: option
        character(len=*), intent(in) :: val

        integer :: ios

        ! [TODO] by-pass this if full calcs
        select case (option)
        case ('ext_cor')
            if (run%calc_type == 'dev') then
                read(val, *, iostat=ios) run%ext_cor
                if (ios /= 0) call stop_all('get_ext_cor_opts', 'CONFIGURATION ERROR: ext_cor must logical')
            endif
        case ('ext_cor_file')
            run%ext_cor_file = val
        case ('ext_cor_sd')
            read(val, *, iostat=ios) run%ext_cor_sd
            if (ios /= 0) call stop_all('get_ext_cor_opts', 'CONFIGURATION ERROR: ext_cor_sd must logical')
        end select

    end subroutine get_ext_cor_opts

    subroutine get_run_opts(run, option, val)

        ! Get generic runtime information options
        !
        ! In:
        !     option: option keyword from the config file
        !     val: option's value
        ! Out:
        !     run: runtime information

        use const, only: sp, p
        use errors, only: stop_all
        use system, only: run_t

        type(run_t), intent(inout) :: run
        character(len=*), intent(in) :: option
        character(len=*), intent(in) :: val

        integer :: itol
        integer :: ios

        ! Run configuration options
        select case (option)
        case ('sorted_ints')
            read(val, *, iostat=ios) run%sorted_ints
            if (ios /= 0) call stop_all('get_run_opts', 'CONFIGURATION ERROR: rhf must logical')

        case ('rhf')
            read(val, *, iostat=ios) run%rhf
            if (ios /= 0) call stop_all('get_run_opts', 'CONFIGURATION ERROR: rhf must logical')

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
                run%tol = 10.0_p ** itol
            endif


        case ('label')
            run%label = val

        case ('symfile', 'sym_file')
            run%sym_file = val

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
        end select


    end subroutine get_run_opts


    subroutine load_config_file(config)

        ! Load config file into memory
        !
        ! In/Out:
        !    config: configuration data, including input filename,
        !            input file contents, etc. (see system.f90)

        use const, only: line_len, config_unit
        use errors, only: stop_all
        use system, only: config_t

        type(config_t), intent(inout) :: config

        character(len=line_len) :: line
        integer :: ios
        logical :: t_exists

        associate(filename=>config%filename,lines=>config%lines,file_size=>config%file_size)

            inquire(file=trim(filename), exist=t_exists)
            if (.not. t_exists) &
                call stop_all('load_config_file', 'CONFIGURATION ERROR: configuration file not found')

            open(unit=config_unit,file=trim(filename),status='old')

            do
                read(config_unit, '(a)', iostat=ios) line
                if (ios == -1) exit
                if (ios /= 0) &
                    call stop_all('load_config_file', "CONFIGURATION ERROR: could not read configuration file")

                ! Load config file into memory
                file_size = file_size + 1
                if (file_size > size(config%lines,1)) &
                    call stop_all('load_config_file', "CONFIGURATION ERROR: configuration file too large")
                lines(file_size) = line
            enddo

        end associate

        close(config_unit)

    end subroutine load_config_file

    subroutine set_default_options(sys, run, cc)

        ! Set default options and configuration parameters
        !
        ! In/Out:
        !    sys: system information
        !    run: runtime information
        !    cc: cc information

        use const, only: sp, dp
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(inout) :: run
        type(cc_t), intent(inout) :: cc

        ! System
        sys%mult = 0
        sys%act_occ_b = 0
        sys%act_unocc_a = 0

        ! Run
        run%sorted_ints = .false.
        run%hbar = .false.
        run%lcc = .false.
        run%mm_23 = .false.
        run%act_ind_t = 0
        run%act_ind_q = 0
        run%lvl_t = .false.
        run%lvl_q = .false.

        run%rhf = .false.
        run%keep_bin = .true.
        run%shift = 0.0_dp
        run%restart = .false.
        run%diis_space = 5
        run%max_iter = 60
        run%tol = 1.0e-4_dp
        run%label = ''
        run%sym_file = 'sym.inp'
        run%onebody_file = 'onebody.inp'
        run%twobody_file = 'twobody.inp'
        run%bin_file = 'tvec_'//trim(run%uuid)//'.bin'
        !run%calc_type = 'CCSD'
        ! Externally corrected
        run%ext_cor = .false.
        run%ext_cor_sd = .true.

        ! CC
        cc%acc%t2t2_t2 = 1.0_sp
        cc%acc%t3_t2 = 1.0_sp
        cc%acc%t1t3_t2 = 1.0_sp
        cc%acc%t2t2_t3 = 1.0_sp
        cc%acc%t2t3_t3 = 1.0_sp

    end subroutine set_default_options

end module parser
