module diis
    implicit none

    integer :: chunks = 0
    integer :: chunk_size = 0
    integer, parameter :: max_len = (2**30/8)

contains

    subroutine calc_diis(run, conv)

        use const, only: p, t_vecs_unit
        use errors, only: stop_all
        use hdf5_io, only: get_chunk_daxpy
        use system, only: run_t
        use cc_types, only: cc_t
        use solver_types, only: conv_t

        ! Input vars
        type(run_t), intent(in) :: run
        type(conv_t), intent(inout) :: conv

        ! Local vars
        integer, allocatable :: ipiv(:)
        integer :: info
        integer :: indx, indy
        integer :: max_t_size

        real(p), allocatable :: vec_aux_1(:), vec_aux_2(:), vec_aux_3(:)
        real(p), allocatable :: B(:,:), C(:)

        real(p) :: accum
        real(p) ddot, axpy


        allocate(B(run%diis_space+1,run%diis_space+1))

        call construct_b_mat(conv, run%diis_space, B)

        allocate(ipiv(run%diis_space+1))
        allocate(c(run%diis_space+1))

        C=0.0_p
        C(run%diis_space+1) = -1.0_p

        ! Solve system of equations
        call dgesv(run%diis_space+1, 1, B, run%diis_space+1, ipiv, C, run%diis_space+1, info)
        if (info /= 0) call stop_all('calc_diis', 'DIIS error in DGESV.')

        conv%vec_ptr(1:conv%vec_size) = 0.0_p
        call get_chunk_daxpy(conv%filename, conv%iter_dset_name, conv%vec_ptr, C, run%diis_space)

        deallocate(ipiv, c, b)

    end subroutine calc_diis

    subroutine construct_b_mat(conv, diis_space, B)

        use const, only: p
        use hdf5_io, only: get_chunk_diff_dot_in_mat
        use solver_types, only: conv_t

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: diis_space
        real(p), allocatable, intent(inout) :: B(:,:)

        integer :: idx, idy

        ! Allocate
        B=0.0_p

        call get_chunk_diff_dot_in_mat(conv%filename, conv%iter_dset_name, &
            B, diis_space)

        ! Fill the lower triangular part
        do idx=1, diis_space
            do idy=idx+1, diis_space
                B(idy, idx) = B(idx, idy)
            enddo
        enddo

        ! Set DIIS matrix boundaries
        B(:,diis_space+1) = -1.0_p
        B(diis_space+1,:) = -1.0_p

    end subroutine construct_b_mat

    subroutine write_vecs(conv, iter, diis_space)

        use const, only: t_vecs_unit, p
        use hdf5_io, only: write_column_in_mat, write_vector
        use solver_types, only: conv_t

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: iter
        integer, intent(in) :: diis_space

        integer :: indx_rec, i_chunk, i

        ! Write latest vector in its corresponding dataset
        call write_vector(conv%filename, conv%vec_dset_name, &
             conv%vec_ptr(1:conv%vec_size), conv%vec_size)


        ! Write vector in the iteration history
        indx_rec = mod(iter, diis_space + 1)
        if (indx_rec == 0) indx_rec = diis_space + 1
        call write_column_in_mat(conv%filename, conv%iter_dset_name, &
            conv%vec_ptr(1:conv%vec_size), conv%vec_size, indx_rec)


    end subroutine write_vecs

    subroutine init_vecs(conv, diis_space)

        use solver_types, only: conv_t
        use hdf5_io, only: init_dset

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: diis_space

        call init_dset(conv%filename, conv%iter_dset_name, [conv%vec_size, diis_space + 1])

    end subroutine init_vecs


end module diis
