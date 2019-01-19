module diis
    implicit none

    integer :: chunks = 0
    integer :: chunk_size = 0
    integer, parameter :: max_len = (2**30/8)

contains

    subroutine calc_diis(run, conv)

        use const, only: p, t_vecs_unit
        use printing, only: abort_cc
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

        ! Allocate
        allocate(B(run%diis_space+1,run%diis_space+1))
        B=0.0_p
        allocate(vec_aux_1(conv%vec_size))
        allocate(vec_aux_2(conv%vec_size))
        allocate(vec_aux_3(conv%vec_size))

        max_t_size = conv%vec_size

        do indx = 1, run%diis_space

            ! Generate difference vector
            !read(t_vecs_unit, rec=indx) vec_aux_1
            !read(t_vecs_unit, rec=indx+1) vec_aux_2
            call read_vecs(conv, indx,  vec_aux_1)
            call read_vecs(conv, indx+1,  vec_aux_2)

            vec_aux_3 = vec_aux_2 - vec_aux_1

            do indy = 1, run%diis_space

                ! Generate difference vector
                !read(t_vecs_unit, rec=indy) vec_aux_1
                !read(t_vecs_unit, rec=indy+1) vec_aux_2
                call read_vecs(conv, indy,  vec_aux_1)
                call read_vecs(conv, indy+1,  vec_aux_2)

                vec_aux_2 = vec_aux_2 - vec_aux_1

                accum = 0.0_p
                accum = ddot(max_t_size, vec_aux_3, 1, vec_aux_2, 1)

                B(indx,indy) = B(indx,indy) + accum
            enddo
        enddo

        deallocate(vec_aux_2, vec_aux_3)

        ! Set DIIS matrix boundaries
        b(:,run%diis_space+1) = -1.0_p
        b(run%diis_space+1,:) = -1.0_p

        !allocate(L(iDIIS+1),C(iDIIS+1))
        allocate(ipiv(run%diis_space+1))
        allocate(c(run%diis_space+1))

        c=0.0_p
        c(run%diis_space+1) = -1.0_p

        ! Solve system of equations
        call dgesv(run%diis_space+1, 1, B, run%diis_space+1, ipiv, c, run%diis_space+1, info)

        if (info /= 0) call abort_cc('DIIS error.')

        conv%vec_ptr(1:max_t_size) = 0.0_p
        do indx = 1, run%diis_space
            !read(t_vecs_unit, rec=indx) vec_aux_1
            call read_vecs(conv, indx, vec_aux_1)

            call daxpy(max_t_size, C(indx), vec_aux_1, 1, conv%vec_ptr, 1)

        enddo

        deallocate(vec_aux_1)
        deallocate(ipiv, c, b)

    end subroutine calc_diis

    subroutine write_vecs(conv, iter, diis_space)

        use const, only: t_vecs_unit
        use solver_types, only: conv_t

        type(conv_t), intent(in) :: conv
        integer, intent(in) :: iter
        integer, intent(in) :: diis_space

        integer :: indx_rec, i_chunk, i

        indx_rec = mod(iter, diis_space + 1)
        if (indx_rec == 0) indx_rec = diis_space + 1

        if (chunks == 0) then
            write(conv%vecs_unit, rec=indx_rec) conv%vec_ptr(1:conv%vec_size)
        else
            do i_chunk=0, chunks
                if (i_chunk < chunks) then
                    write(conv%vecs_unit, rec=((indx_rec - 1) * chunks) + i_chunk + 1) &
                        conv%vec_ptr(i_chunk*max_len+1:(i_chunk+1)*max_len)
                else
                    write(conv%vecs_unit, rec=((indx_rec - 1) * chunks) + i_chunk + 1) &
                        conv%vec_ptr(i_chunk*max_len+1:i_chunk*max_len + chunk_size)
                endif

            enddo
        endif


    end subroutine write_vecs

    subroutine read_vecs(conv, indx,  vec)

        use const, only: p
        use solver_types, only: conv_t


        type(conv_t), intent(in) :: conv
        integer, intent(in) :: indx
        real(p), allocatable, intent(inout) :: vec(:)

        integer :: i_chunk


        if (chunks == 0) then
            read(conv%vecs_unit, rec=indx) vec
        else
            do i_chunk=0, chunks
                if (i_chunk < chunks) then
                    read(conv%vecs_unit, rec=((indx - 1) * chunks) + i_chunk + 1) &
                        vec(i_chunk*max_len+1:(i_chunk+1)*max_len)
                else
                    read(conv%vecs_unit, rec=((indx - 1) * chunks) + i_chunk + 1) &
                        vec(i_chunk*max_len+1:i_chunk*max_len + chunk_size)
                endif
            enddo
        endif

    end subroutine read_vecs

    subroutine init_vecs(conv)

        use solver_types, only: conv_t

        type(conv_t), intent(in) :: conv

        chunks = int(conv%vec_size / max_len)
        chunk_size = conv%vec_size - max_len * chunks

        if (chunks == 0) then
            open(conv%vecs_unit, file='iter_vecs.bin', form='unformatted', recl=conv%vec_size*8, access='direct')
        else
            open(conv%vecs_unit, file='iter_vecs.bin', form='unformatted', recl=max_len*8, access='direct')
        endif


    end subroutine init_vecs


end module diis
