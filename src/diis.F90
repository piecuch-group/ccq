module diis

    ! This module provides all functions and subroutines
    ! needed for performing a DIIS extrapolation during
    ! iterative solving of CC methods.

    implicit none

contains

    subroutine calc_diis(run, conv)

        ! Perform a DIIS extrapolation

        ! In:
        !   run: runtime information

        ! In/Out:
        !   conv: convergence information. This includes
        !         the CC vector and the location of the
        !         past iterations. On return, an extrapolated
        !         vector is loaded to the vec_ptr

        use const, only: p
        use errors, only: stop_all
        use system, only: run_t
        use cc_types, only: cc_t
        use solver_types, only: conv_t

#ifdef ENABLE_HDF5
        use hdf5_io, only: get_chunk_daxpy
#endif

        ! Input vars
        type(run_t), intent(in) :: run
        type(conv_t), intent(inout) :: conv

        ! Local vars
        integer, allocatable :: ipiv(:)
        integer :: info
        integer :: idx, idy
        integer :: max_t_size

        real(p), allocatable :: B(:,:), C(:)

#ifndef ENABLE_HDF5
        real(p), allocatable :: vec_aux(:)
#endif

        real(p) :: accum
        ! Define external functions
        ! [TODO] should use module
        real(p) :: ddot, axpy


        allocate(B(run%diis_space+1,run%diis_space+1))

        ! Construct error B matrix
        call construct_b_mat(conv, run%diis_space, B)

        allocate(ipiv(run%diis_space+1))
        allocate(c(run%diis_space+1))

        C=0.0_p
        C(run%diis_space+1) = -1.0_p

        ! Solve system of equations
        call dgesv(run%diis_space+1, 1, B, run%diis_space+1, ipiv, C, run%diis_space+1, info)
        if (info /= 0) call stop_all('calc_diis', 'DIIS error in DGESV.')

        conv%vec_ptr(1:conv%vec_size) = 0.0_p

#ifdef ENABLE_HDF5
        call get_chunk_daxpy(conv%filename, conv%iter_dset_name, conv%vec_ptr, C, run%diis_space)
#else

        allocate(vec_aux(conv%vec_size))
        do idx=1, run%diis_space
            call read_vec(conv, run%diis_space, idx, vec_aux)

            call daxpy(conv%vec_size, C(idx), vec_aux, 1, conv%vec_ptr, 1)
        enddo
        deallocate(vec_aux)

#endif

        deallocate(ipiv, c, b)

    end subroutine calc_diis

    function residuum(conv, iter, diis_space)

        ! Calculate the residuum by taking the norm
        ! of the difference between the last two CC
        ! vectors

        ! In:
        !   conv: current and previous CC vectors

        ! Out:
        !   residuum: norm of the difference between the
        !             last two vectors

        use const, only: p
        use solver_types, only: conv_t

#ifdef ENABLE_HDF5
        use hdf5_io, only: get_chunk_residuum
#endif


        type(conv_t), intent(in) :: conv
        integer, intent(in) :: iter
        integer, intent(in) :: diis_space

        real(p) :: residuum
        integer :: indx1, indx2

#ifndef ENABLE_HDF5
        real(p), allocatable :: v1(:), v2(:)
        real(p) :: ddot
#endif


#ifdef ENABLE_HDF5

        indx1 = mod(iter - 1, diis_space + 1)
        if (indx1 == 0) indx1 = diis_space + 1

        indx2 = mod(iter, diis_space + 1)
        if (indx2 == 0) indx2 = diis_space + 1


        residuum = get_chunk_residuum(conv%filename, conv%iter_dset_name, &
             indx1, indx2)
#else

        ! Allocate auxiliary vectors
        allocate(v1(conv%vec_size))
        allocate(v2(conv%vec_size))

        ! Make sure that we start doing the difference after
        ! the second iteration. Otherwise consider 0.0 for
        ! the starting vector
        if (iter == 1) then
            v1 = 0.0_p
        else
            call read_vec(conv, diis_space, iter - 1, v1)
        endif

        call read_vec(conv, diis_space, iter, v2)
        v1 = v1 - v2

        residuum = ddot(conv%vec_size, v1, 1, v1, 1)
        residuum = dsqrt(residuum)

        deallocate(v1)
        deallocate(v2)

#endif

        ! Return the square root of the ddot product
        residuum = dsqrt(residuum)

    end function residuum

    subroutine construct_b_mat(conv, diis_space, B)

        ! Construct the error matrix B

        ! In:
        !   conv: CC vectors and data from the current
        !         iterative procedure

        ! In/Out:
        !   B: error matrix

        use const, only: p, c_len, tmp_unit
        use solver_types, only: conv_t

#ifdef ENABLE_HDF5
        use hdf5_io, only: get_chunk_diff_dot_in_mat
#endif

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: diis_space
        real(p), allocatable, intent(inout) :: B(:,:)

        integer :: idx, idy

#ifndef ENABLE_HDF5
        real(p), allocatable :: v1(:), v2(:), v3(:)
        real(p) :: accum
        real(p) :: ddot
#endif

        ! Initialize B matrix
        B = 0.0_p

#ifdef ENABLE_HDF5

        call get_chunk_diff_dot_in_mat(conv%filename, conv%iter_dset_name, &
            B, diis_space)

#else

        allocate(v1(conv%vec_size))
        allocate(v2(conv%vec_size))
        allocate(v3(conv%vec_size))

        do idx=1, diis_space

            call read_vec(conv, diis_space, idx, v1)
            call read_vec(conv, diis_space, idx+1, v2)

            v3 = v2 - v1

            do idy=idx, diis_space

                call read_vec(conv, diis_space, idy, v1)
                call read_vec(conv, diis_space, idy+1, v2)

                v2 = v2 - v1

                accum = 0.0_p
                accum = ddot(conv%vec_size, v3, 1, v2, 1)
                B(idx, idy) = B(idx, idy) + accum

            enddo
        enddo

        deallocate(v1)
        deallocate(v2)
        deallocate(v3)

#endif

        ! Fill the lower triangular part of the B
        ! matrix
        do idx=1, diis_space
            do idy=idx+1, diis_space
                B(idy, idx) = B(idx, idy)
            enddo
        enddo

        ! Set B matrix boundaries
        B(:,diis_space+1) = -1.0_p
        B(diis_space+1,:) = -1.0_p

    end subroutine construct_b_mat

#ifndef ENABLE_HDF5
    subroutine read_vec(conv, diis_space, iter, vec)

        ! Read CC vector when HDF5 is disabled

        ! In:
        !   conv: convergence data, including CC vector
        !         location, etc.

        ! In/Out:
        !   vec: vector containing the data of the iter-th 
        !        binary file

        use const, only: p, c_len, tmp_unit
        use solver_types, only: conv_t
        use errors, only: stop_all

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: diis_space
        integer, intent(in) :: iter
        real(p), allocatable, intent(inout) :: vec(:)


        integer :: indx_rec
        character(len=c_len) :: filename

        ! Check whether vec is allocated. Abort otherwise
        if (.not. allocated(vec)) call stop_all('read_vec', 'vec not allocated')

        ! Calculate index
        indx_rec = mod(iter, diis_space + 1)
        if (indx_rec == 0) indx_rec = diis_space + 1

        ! Obtain filename
        write(filename, '(a,i0,a)') trim(conv%iter_dset_name)//"-", &
            indx_rec, ".bin"

        ! Load vector from disk
        open(tmp_unit, file=trim(filename), form='unformatted', status='old')
        read(tmp_unit) vec
        close(tmp_unit)

    end subroutine read_vec
#endif

    subroutine write_vecs(conv, iter, diis_space)

        ! Write CC vectors to disk

        ! In:
        !   conv: CC vectors and convergence data
        !   iter: current Jacobi iteration
        !   diis_space: dimension of the DIIS space

        ! Out:
        !   CC vector written to disk

        use const, only: tmp_unit, p, c_len
        use solver_types, only: conv_t
#ifdef ENABLE_HDF5
        use hdf5_io, only: write_column_in_mat, write_vector
#endif

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: iter
        integer, intent(in) :: diis_space

        character(len=c_len) :: filename
        integer :: indx_rec, i_chunk, i


#ifdef ENABLE_HDF5
        ! Write latest vector in its corresponding dataset
        call write_vector(conv%filename, conv%vec_dset_name, &
             conv%vec_ptr(1:conv%vec_size), conv%vec_size)


        ! Write vector in the iteration history
        indx_rec = mod(iter, diis_space + 1)
        if (indx_rec == 0) indx_rec = diis_space + 1
        call write_column_in_mat(conv%filename, conv%iter_dset_name, &
            conv%vec_ptr(1:conv%vec_size), conv%vec_size, indx_rec)
#else
        ! Write latest vector
        open(tmp_unit, file=trim(conv%vec_dset_name)//".bin", &
            form='unformatted', status='unknown')
        write(tmp_unit) conv%vec_ptr(1:conv%vec_size)
        close(tmp_unit)

        ! Write vector in the iteration history
        indx_rec = mod(iter, diis_space + 1)
        if (indx_rec == 0) indx_rec = diis_space + 1

        write(filename, '(a,i0,a)') trim(conv%iter_dset_name)//"-", &
            indx_rec, ".bin"

        open(tmp_unit, file=trim(filename), &
            form='unformatted', status='unknown')
        write(tmp_unit) conv%vec_ptr(1:conv%vec_size)
        close(tmp_unit)

#endif

    end subroutine write_vecs

    subroutine init_vecs(conv, diis_space)

        ! Initialize HDF5 dataset for the iteration
        ! history vectors

        ! In:
        !   conv: CC vector and convergence data
        !   diis_space: dimension of the DIIS space

        ! Out:
        !   HDF5 dataset written to the master
        !   file. If HDF5 is disabled, this routine
        !   doesn't do anything.

        use solver_types, only: conv_t
#ifdef ENABLE_HDF5
        use hdf5_io, only: init_dset
#endif

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: diis_space

#ifdef ENABLE_HDF5
        call init_dset(conv%filename, conv%iter_dset_name, [conv%vec_size, diis_space + 1])
#endif

    end subroutine init_vecs

    subroutine clean_up_files(conv, diis_space)

        ! Remove DIIS generated files after
        ! convergence is reached.


        ! In:
        !   conv: CC vector and converfence data

        ! Out:
        !   all DIIS generated files are removed

        use const, only: tmp_unit, c_len
        use solver_types, only: conv_t

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: diis_space

        integer :: idx
        character(len=c_len) :: filename

#ifndef ENABLE_HDF5

        do idx=1, diis_space
            write(filename, '(a,i0,a)') trim(conv%iter_dset_name)//"-", &
                idx, ".bin"

            open(tmp_unit, file=trim(filename), status='unknown')
            close(tmp_unit, status='delete')
        enddo

#endif

    end subroutine clean_up_files

end module diis