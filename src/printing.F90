module printing

    ! This module deals with printing and outputting information

    use, intrinsic :: iso_fortran_env, only: output_unit

    implicit none

    integer :: io = output_unit

contains

    subroutine init_print(output_file)

        ! Initialize printing

        ! In:
        !   run: information about this run's configuration

        use const, only: log_unit
        use system, only: run_t

        character(len=*) :: output_file

        ! Open output file if it is provided in the configuration file
        if (trim(output_file) /= '' .and. trim(output_file) /= 'stdout') then
            open(log_unit, file=trim(output_file), status='unknown')
            io = log_unit
        endif

    end subroutine init_print

    subroutine close_print()

        ! Close io file on exit

        if (io /= output_unit) close(io)

    end subroutine close_print

    subroutine print_compilation_info()

        ! Print ccq information. This includes information about
        ! the compilation, the host, authors, etc.

        ! In:
        !   run: runtime configuration data

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

#ifdef CPPFLAGS
        write(io,'(2x,a20,1x,a)') &
            'CPP Flags', CPPFLAGS
#endif

#ifdef FLAGS
        write(io,'(2x,a20,1x,a)') &
            'Flags', FLAGS
#endif

#ifdef VERSION
        write(io,'(2x,a20,1x,a)') 'Git SHA', &
            VERSION
#endif
        call flush(io)

    end subroutine print_compilation_info

    subroutine print_host_info(run)

        ! Print ccq information. This includes information about
        ! the compilation, the host, authors, etc.

        ! In:
        !   run: runtime configuration data

        use system, only: run_t

        type(run_t), intent(in) :: run

        character(len=255) :: hostname
        character(len=255) :: cmd
        character(len=255) :: cwd
        character(len=255) :: user
        character(len=30) :: date

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
        write(io,'(2x,a20,1x,a)') 'UUID', trim(run%uuid(1:36))
        write(io,'(2x,a20,1x,i0/)') 'Threads', run%num_threads

        call flush(io)

    end subroutine print_host_info

    subroutine print_calc_params(sys, run, cc)

        ! Print calculation parameters. This includes
        ! molecular system data, CC settings, and other
        ! parameters and configurations related to the QM
        ! calculations.

        ! In:
        !   sys: molecular system data
        !   run: runtime configuration data
        !   cc: CC data, including vectors

        use system, only: sys_t, run_t
        use cc_types, only: cc_t

        use symmetry, only: reverse_map_irrep

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(in) :: cc

        character(len=5) :: irrep
        character(len=1) :: occ
        integer :: idx

        write(io,'(a)') 'System information'
        write(io,'(a)') '------------------'
        if (trim(run%label) /= '') &
            write(io,'(2x,a27,2x,a)') 'Label', trim(run%label)
        write(io,'(2x,a27,2x,i0)') 'Number of electrons', sys%nel
        write(io,'(2x,a27,2x,i0)') 'Number of virtuals', sys%nvirt
        write(io,'(2x,a27,2x,i0)') 'Frozen orbitals', sys%froz
        write(io,'(2x,a27,2x,i0)') 'Occupied orbitals (alpha)',sys%occ_a
        write(io,'(2x,a27,2x,i0)') 'Occupied orbitals (beta)', sys%occ_b
        write(io,'(2x,a27,2x,i0/)') 'Total orbitals', sys%orbs

        write(io,'(2x,a)') 'Active space partitioning:'
        write(io,'(2x,a27,2x,i0)') 'Occupied (alpha)', sys%act_occ_a
        write(io,'(2x,a27,2x,i0)') 'Occupied (beta)', sys%act_occ_b
        write(io,'(2x,a27,2x,i0)') 'Unoccupied (alpha)', sys%act_unocc_a
        write(io,'(2x,a27,2x,i0)') 'Unoccupied (beta)', sys%act_unocc_b


        if (trim(sys%point_group) /= '') &
            write(io,'(/2x,a27,2x,a)') 'Point group', trim(sys%point_group)


        write(io,'(/a)') 'CC settings'
        write(io,'(a)')  '-----------'
        !      write(io,'(2x,a27,2x,i16)') 'Number of excited states', nroot
        write(io,'(2x,a27,2x,a)') 'Calculation type', trim(run%calc_type)
        write(io,'(2x,a27,2x,es10.2)') 'Convergence tolerance', run%tol
        write(io,'(2x,a27,2x,i0)') 'Max iterations', run%max_iter
        write(io,'(2x,a27,2x,i0)') 'DIIS space', run%diis_space
        write(io,'(2x,a27,2x,f8.4)') 'Shift energy', run%shift
        write(io,'(2x,a27,2x,l)') 'Restart', run%restart
        write(io,'(2x,a27,2x,l/)') 'Restricted CC', run%rhf

        write(io,'(2x,a27,2x,i0)') 'Active triples indices', run%act_ind_t
        write(io,'(2x,a27,2x,i0/)') 'Active quadruples indices', run%act_ind_q

        if (run%ext_cor) then
            write(io,'(2x,a)') 'External correction settings:'
            write(io,'(2x,a27,2x,l)') 'Use singles and doubles', run%ext_cor_sd
            write(io,'(2x,a27,2x,l)') 'Read NECI HDF5 POPSFILE', run%ext_cor_file_h5
            write(io,'(2x,a27,2x,a/)') 'External determinant list', trim(run%ext_cor_file)
        endif


        write(io,'(2x,a)') 'ACC parameters:'
        write(io,'(2x,a27,2x,5f6.2)') 'T2^2 -> T2 =', cc%acc%t2t2_t2
        write(io,'(2x,a27,2x,2f6.2)') 'T3 -> T2 =', cc%acc%t3_t2
        write(io,'(2x,a27,2x,4f6.2)') 'T1*T3 -> T2 =', cc%acc%t1t3_t2
        write(io,'(2x,a27,2x,3f6.2)') 'T2^2 -> T3 =', cc%acc%t2t2_t3
        write(io,'(2x,a27,2x,5f6.2)') 'T2*T3 -> T3 =', cc%acc%t2t3_t3

        write(io,'(/a)') 'Molecular orbital basis'
        write(io,'(a)')  '-----------------------'
        write(io,'(2x,a27,2x,f16.10)') 'Nuclear repulsion (Eh)', sys%en_repul
        write(io,'(2x,a27,2x,f16.10)') 'Reference (Eh)', sys%en_ref

        write(io,'(/2x,a10,2x,a10,a15,a10)') 'MO Index', 'Symmetry', 'Energy (Eh)', 'Occupied'
        write(io,'(2x,47("-"))')
        do idx=1, sys%orbs
            irrep = reverse_map_irrep(sys%orbital_syms(idx), sys%point_group)
            if (idx <= sys%occ_a) then
                occ = '*'
            else
                occ = ''
            endif
            write(io,'(2x,i10,2x,i5,a5,f15.8,2x,a)') idx, sys%orbital_syms(idx), trim(irrep), sys%orbital_energies(idx), occ
        enddo

        write(io, '(a)') ''

        call flush(io)

    end subroutine print_calc_params

    subroutine print_cct3(sys, cc)

        ! Print the summary of CC(t;3) calculations

        ! In:
        !   sys: molecular system data
        !   cc: CC data, including vectors, energy, etc.

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

        ! Print ccq calculation summary

        ! In:
        !   sys: molecular system data
        !   run: runtime configuratoin data
        !   cc: CC data, including vectors, energy, etc.

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

    subroutine print_date(msg)

        ! Print the date of now plus a message

        ! In:
        !   msg: message to be printed next to the date

        character(len=*), intent(in) :: msg
        character(len=30) :: date

        call fdate(date)
        write (io,'(/a/)') trim(msg)//' '//trim(date)

        call flush(io)

    end subroutine print_date

    subroutine print_iter_head()

        ! Print the iteration table header at the
        ! beginning of the iterative procedure

        write(io,'(/2x,a4,3(a15),a16)') 'It.',  'E (Corr)', 'dE', 'Residuum', 'Wall Time'
        write(io,'(2x,65("-"))')
        call flush(io)

    end subroutine print_iter_head

    subroutine print_iteration(iter, ecor, energy_diff, res, prev_time, new_time)

        ! Print the results of the current Jacobi iteration

        ! In:
        !   iter: iteration number
        !   ecor: correlation energy
        !   energy_diff: correlation energy difference
        !                between iterations
        !   res: residuum
        !   prev_time: time of the previous iteration
        !   new_time: time of the current iteration.
        !             This and prev_time are used to
        !             calculate the iteration's duration

        use const, only: p

        integer, intent(in) :: iter
        real(p), intent(in) :: ecor
        real(p), intent(in) :: energy_diff
        real(p), intent(in) :: res
        real(p), intent(in) :: prev_time, new_time

        real(p) :: nsec
        integer :: nmin

        ! Convert time in seconds to time in min:sec
        nsec = new_time - prev_time
        nmin = int(nsec) / 60
        nsec = nsec-real(nmin, p)*60.0_p

        write(io,'(2x,i4,3(f15.10),i5,'' min'',f5.1,'' s'')') iter, ecor, energy_diff, res, nmin, nsec

        call flush(io)

    end subroutine print_iteration

    subroutine print_help()

        ! Print the help dialog if no options or input file is given

        print '(a)', 'Usage: ccq [OPTIONS] FILE'
        print '(a)', 'Coupled-cluster program that performs calculations with up to quadruply'
        print '(a/)', 'excited cluster components.'

        print '(a)', 'Options:'
        print '(4x,a8,a16,4x,a)', '-o,', '--output', "output file, where the calculation's information will be written"
        print '(4x,a8,a16,4x,a)', '-h,', '--help', "print this help menu"

        print '(/a)', 'ccq repository online: <https://gitlab.msu.edu/piecuch-group/ccq/>'
        print '(a)', "Program written in Piecuch's group at MSU: <https://www2.chemistry.msu.edu/faculty/piecuch/>"

        call exit(1)

    end subroutine print_help

    subroutine print_config(config)

        ! Echo configuration file to the output file

        ! In:
        !   config: configuration file split into lines

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

    ! [TODO] write a couple routines to write down the largest T amplitudes,
    ! R amplitudes, L amplitudes

end module printing
