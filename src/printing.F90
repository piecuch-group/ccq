module printing

    use, intrinsic :: iso_fortran_env, only: output_unit

    implicit none

    integer :: io = output_unit

contains

    subroutine init_print(run)

        use const, only: log_unit
        use system, only: run_t

        type(run_t), intent(in) :: run

        if (trim(run%output_file) /= '' .and. trim(run%output_file) /= 'stdout') then
            open(log_unit, file=trim(run%output_file), status='unknown')
            io = log_unit
        endif

    end subroutine init_print

    subroutine close_print()

        if (io /= output_unit) close(io)

    end subroutine close_print

    subroutine print_header(uuid)

        character(len=255) :: hostname
        character(len=255) :: cmd
        character(len=255) :: cwd
        character(len=255) :: user
        character(len=37), intent(in) :: uuid
        character(len=30) :: date


        write(io,'(a)') 'ccq Coupled-Cluster Program'
        write(io,'(a/)') '==========================='

        write(io, '(2x,a)') 'ccq repository online: <https://gitlab.msu.edu/piecuch-group/ccq/>'
        write(io, '(2x,a/)') "Program written in Piecuch's group at MSU: <https://www2.chemistry.msu.edu/faculty/piecuch/>"

        write(io, '(2x,a)') "Authors:"
        write(io, '(4x,a)') "Nicholas P. Bauman, J. Emiliano Deustua, Ilias Magoulas"
        write(io, '(4x,a/)') "Piotr Piecuch, and Jun Shen."

        write(io,'(a)') 'Compilation information'
        write(io,'(a)') '-----------------------'

#if defined (COMP_TIME) && defined (COMP_HOST)
        write(io,'(2x,a20,1x,a)') 'Hostname', &
            COMP_HOST

        write(io,'(2x,a20,1x,a)') 'Date', &
            COMP_TIME
#endif

#ifdef __VERSION__
        write(io,'(2x,a20,1x,2a)') &
            'Compiler', 'gfortran ', __VERSION__
#endif

#ifdef __INTEL_COMPILER
        write(io,'(2x,a20,1x,a,i6)') &
            'Compiler', 'ifort ', __INTEL_COMPILER
#endif

#ifdef FLAGS
        write(io,'(2x,a20,1x,a)') &
            'Flags', FLAGS
#endif

#ifdef VERSION
        write(io,'(2x,a20,1x,a)') 'Git SHA', &
            VERSION
#endif

        write(io,'(/a)') 'Host information'
        write(io,'(a)') '----------------'
        call hostnm(hostname)
        call get_command(cmd)
        call getcwd(cwd)
        call get_environment_variable("USER", user)
        call fdate(date)
        write(io,'(2x,a20,1x,a)') 'Hostname', trim(hostname)
        write(io,'(2x,a20,1x,a)') 'Command', trim(cmd)
        write(io,'(2x,a20,1x,a)') 'Working dir', trim(cwd)
        write(io,'(2x,a20,1x,a)') 'User', trim(user)
        write(io,'(2x,a20,1x,a)') 'Date', trim(date)
        write(io,'(2x,a20,1x,a/)') 'UUID', trim(uuid(1:36))

        call flush(io)

    end subroutine print_header

    subroutine print_calc_params(sys, run, cc)

        use const, only: dp
        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(in) :: cc

        write(io,'(a)') 'System information'
        write(io,'(a)') '------------------'
        if (trim(run%label) /= '') then
            write(io,'(2x,a27,2x,a)') 'Label', trim(run%label)
        endif
        write(io,'(2x,a27,2x,i16)') 'Number of electrons', sys%nel
        write(io,'(2x,a27,2x,i16)') 'Number of virtuals', sys%nvirt
        write(io,'(2x,a27,2x,i16)') 'Frozen orbitals', sys%froz
        write(io,'(2x,a27,2x,i16)') 'Occupied orbitals (alpha)',sys%occ_a
        write(io,'(2x,a27,2x,i16)') 'Occupied orbitals (beta)', sys%occ_b
        write(io,'(2x,a27,2x,i16/)') 'Total orbitals', sys%orbs

        write(io,'(2x,a)') 'Active space partitioning:'
        write(io,'(2x,a27,2x,i16)') 'Occupied (alpha)', sys%act_occ_a
        write(io,'(2x,a27,2x,i16)') 'Occupied (beta)', sys%act_occ_b
        write(io,'(2x,a27,2x,i16)') 'Unoccupied (alpha)', sys%act_unocc_a
        write(io,'(2x,a27,2x,i16)') 'Unoccupied (beta)', sys%act_unocc_b


        write(io,'(/a)') 'CC settings'
        write(io,'(a)')  '-----------'
        !      write(io,'(2x,a27,2x,i16)') 'Number of excited states', nroot
        write(io,'(2x,a27,2x,a16)') 'Calculation type', trim(run%calc_type)
        write(io,'(2x,a27,2x,es16.2)') 'Convergence tolerance', run%tol
        write(io,'(2x,a27,2x,i16)') 'Max. iterations', run%max_iter
        write(io,'(2x,a27,2x,i16)') 'DIIS space', run%diis_space
        write(io,'(2x,a27,2x,f16.4)') 'Shift energy', run%shift
        write(io,'(2x,a27,2x,l16)') 'Restart', run%restart
        write(io,'(2x,a27,2x,l16/)') 'Restricted CC', run%rhf

        write(io,'(2x,a27,2x,i16)') 'Active triples indices', run%act_ind_t
        write(io,'(2x,a27,2x,i16/)') 'Active quadruples indices', run%act_ind_q

        if (run%ext_cor) then
            write(io,'(2x,a)') 'External correction parameters:'
            write(io,'(2x,a27,2x,l16/)') 'Use singles and doubles', run%ext_cor_sd
        endif


        write(io,'(2x,a)') 'ACC parameters:'
        write(io,'(2x,a27,2x,5f6.2)') 'T2^2 -> T2 =', cc%acc%t2t2_t2
        write(io,'(2x,a27,2x,2f6.2)') 'T3 -> T2 =', cc%acc%t3_t2
        write(io,'(2x,a27,2x,4f6.2)') 'T1*T3 -> T2 =', cc%acc%t1t3_t2
        write(io,'(2x,a27,2x,3f6.2)') 'T2^2 -> T3 =', cc%acc%t2t2_t3
        write(io,'(2x,a27,2x,5f6.2)') 'T2*T3 -> T3 =', cc%acc%t2t3_t3

        write(io,'(/a)') 'Starting energies (Eh)'
        write(io,'(a)')  '----------------------'
        write(io,'(2x,a27,2x,f16.10)') 'Nuclear repulsion', sys%en_repul
        write(io,'(2x,a27,2x,f16.10/)') 'Reference (HF)', sys%en_ref

        call flush(io)

    end subroutine print_calc_params

    subroutine print_cct3(sys, cc)

        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in) :: cc

        write(io,'(/a20,2a18)') 'Method', 'Correlation (Eh)', 'Total Energy (Eh)'
        write(io,'(10x,46("-"))')
        write(io,'(a20,2f18.12)') 'CCSDt', cc%en_cor, sys%en_ref + cc%en_cor
        write(io,'(a20,2f18.12)') 'CC(t;3),A', cc%en_cor + cc%mm_en_cor_a, sys%en_ref + cc%en_cor + cc%mm_en_cor_a
        write(io,'(a20,2f18.12)') 'CC(t;3),B', cc%en_cor + cc%mm_en_cor_b, sys%en_ref + cc%en_cor + cc%mm_en_cor_b
        write(io,'(a20,2f18.12)') 'CC(t;3),C', cc%en_cor + cc%mm_en_cor_c, sys%en_ref + cc%en_cor + cc%mm_en_cor_c
        write(io,'(a20,2f18.12)') 'CC(t;3)', cc%en_cor + cc%mm_en_cor_d, sys%en_ref + cc%en_cor + cc%mm_en_cor_d

        call flush(io)

    end subroutine print_cct3

    subroutine print_summary(sys, run, cc)

        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(in) :: cc

        write(io,'(/a)') 'Calculation Summary (Eh)'
        write(io,'(a)')  '------------------------'
        write(io,'(a20,f18.12)') 'Reference', sys%en_ref
        write(io,'(a20,f18.12)') 'Correlation', cc%en_cor
        write(io,'(a20,f18.12)') trim(run%calc_type), sys%en_ref + cc%en_cor

        call flush(io)

    end subroutine print_summary


    subroutine print_date(note)

        character(len=*), intent(in) :: note
        character(len=30) :: date

        call fdate(date)
        write (io,'(/a/)') trim(note)//' '//trim(date)

        call flush(io)

    end subroutine print_date

    subroutine print_iter_head()

        ! Print the iteration table header

        write(io,'(/2x,a4,3(a15),a16)') 'It.',  'E (Corr)', 'dE', 'Residuum', 'Wall Time'
        write(io,'(2x,65("-"))')
        call flush(io)

    end subroutine print_iter_head

    subroutine print_iteration(iter, ecor, energy_diff, res, prev_time, new_time)

        use const, only: dp

        integer, intent(in) :: iter
        real(dp), intent(in) :: ecor
        real(dp), intent(in) :: energy_diff
        real(dp), intent(in) :: res
        real(dp), intent(in) :: prev_time, new_time
        real(dp) :: cputime

        real(dp) :: nsec
        integer :: nmin

        nsec=new_time - prev_time
        nmin=int(nsec) / 60
        nsec=nsec-real(nmin, dp)*60.0_dp

        write(io,'(2x,i4,3(f15.10),i5,'' min'',f5.1,'' s'')') iter,ecor,energy_diff, res,nmin,nsec

        call flush(io)

    end subroutine print_iteration

    subroutine print_help()

        ! Print the help dialog
        print '(a)', 'Usage: ccq [OPTIONS] FILE'
        print '(a)', 'Coupled-cluster program that performs calculations with up to quadruply'
        print '(a/)', 'excited cluster components.'

        print '(a)', 'Options:'
        print '(a8,a16,4x,a)', '-o,', '--output', "output file, where the calculation's information will be written"
        print '(a8,a16,4x,a)', '-h,', '--help', "print this help menu"

        print '(/a)', 'ccq repository online: <https://gitlab.msu.edu/piecuch-group/ccq/>'
        print '(a)', "Program written in Piecuch's group at MSU: <https://www2.chemistry.msu.edu/faculty/piecuch/>"

        call exit()

    end subroutine print_help

    subroutine print_config(config)

        use system, only: config_t

        type(config_t), intent(in) :: config

        integer :: i

        write(io, '(a)') 'Configuration file'
        write(io, '(a/)') '------------------'

        do i=1, config%file_size
            write(io, '(a)') trim(config%lines(i))
        enddo

        write(io, '(/)')

    end subroutine print_config

end module printing
