subroutine reorder_stripe(rank, mat_shape, mat_size, permutation_string, A, B)

    use const, only: p
    use cc_utils, only: gen_perm_array

    implicit none

    integer, intent(in) :: rank
    integer, intent(in) :: mat_shape(rank)
    integer, intent(in) :: mat_size
    character(len=*), intent(in) :: permutation_string

    real(p), intent(in) :: A(mat_size)
    real(p), intent(in out) :: B(mat_size)

    integer :: permutation(rank)

    integer :: identity(rank)

    integer :: stride_a(rank)
    integer :: stride_a_inner
    integer :: size_outer, size_inner
    integer :: offset_a, offset_b

    integer :: i, j, j_tmp
    integer :: current_index

    call gen_perm_array(permutation_string, permutation)

    do i=1, rank
        identity(i) = i
    enddo

    if (all(identity == permutation)) then
        B = A
        return
    endif

    stride_a(1) = 1
    do i=2, rank
        stride_a(i) = stride_a(i-1) * mat_shape(i-1)
    enddo
    !print *, 'size a',  size(a)
    !print *, 'stide_a', stride_a
    !print *, 'mat_shape', mat_shape

    size_outer = 1
    do i=1, rank
        if (i /= permutation(1)) size_outer = size_outer * mat_shape(i)
    enddo
    !print *, 'size_outer', size_outer

    ! Size of the fastest changing index
    size_inner = mat_shape(permutation(1))
    ! Size of the stride corresponding to this index in the A matrix
    stride_a_inner = stride_a(permutation(1))

    !$omp parallel default(none), &
    !$omp shared(size_outer, rank, mat_shape, permutation, stride_a, &
    !$omp size_inner, stride_a_inner, a, b), &
    !$omp private(i, j, j_tmp, offset_a, offset_b, current_index)

    !$omp do
    do j=0, size_outer-1
        offset_a = 0

        j_tmp = j

        do i=2, rank
            current_index = modulo(j_tmp, mat_shape(permutation(i)))
            j_tmp = j_tmp / mat_shape(permutation(i))
            offset_a = offset_a + (current_index * stride_a(permutation(i)))
        enddo

        offset_b = j * size_inner

        do i=0, size_inner-1
            b(offset_b + i + 1) = a(offset_a + (i * stride_a_inner) + 1)
        enddo


    enddo
    !$omp end do
    !$omp end parallel


end subroutine reorder_stripe


subroutine reorder_shift(rank, a_shape, a_size, b_shape, b_size, b_shifts, permutation_string, A, B)

    use const, only: p
    use cc_utils, only: gen_perm_array

    integer, intent(in) :: rank
    integer, intent(in) :: a_shape(rank)
    integer, intent(in) :: a_size
    integer, intent(in) :: b_shape(rank)
    integer, intent(in) :: b_size
    integer, intent(in) :: b_shifts(rank)
    character(len=*), intent(in) :: permutation_string

    real(p), intent(in) :: A(a_size)
    real(p), intent(inout) :: B(b_size)


    !integer :: identity(rank)
    integer :: permutation(rank)

    integer :: new_shape(rank)
    integer :: shifts(rank)

    integer :: stride_a(rank)

    integer :: stride_a_inner
    integer :: size_outer, size_inner
    integer :: shift_inner
    integer :: offset_a, offset_b

    integer :: i, j, j_tmp
    integer :: rank_indx

    call gen_perm_array(permutation_string, permutation)

    !identity = [ (i, i=1, rank) ]

    !if (all(identity == permutation)) then
    !    B = A
    !    return
    !endif

    call permute_array(rank, permutation, b_shape, new_shape)
    call permute_array(rank, permutation, b_shifts, shifts)

    ! Stride sizes per dimension
    stride_a(1) = 1
    do i=2, rank
        stride_a(i) = stride_a(i-1) * a_shape(i-1)
    enddo

    !print *, '---------------------------------'
    !print *, "a_shape:  ", a_shape
    !print *, "b_shape:  ", b_shape
    !print *, "b_shifts: ", b_shifts
    !print *, "stride_a: ", stride_a
    !print *, "shifts:   ", shifts
    !print *, '---------------------------------'

    ! The idea here is to reshape the N-rank array into a 2-rank array
    ! where the inner_size corresponds to the left index (fastest changing)
    ! and outer_size, to the right index (slowest chaging)

    ! Size of the fastest changing index
    size_inner = new_shape(permutation(1))
    ! Size of the remaining dimension
    size_outer = product(new_shape) / size_inner
    ! Size of the stride corresponding to this index in the A matrix
    stride_a_inner = stride_a(permutation(1))
    shift_inner = shifts(permutation(1))

    !$omp parallel default(none), &
    !$omp shared(rank, new_shape, shifts, permutation, &
    !$omp size_outer, size_inner, shift_inner, &
    !$omp stride_a, stride_a_inner, &
    !$omp a, b), &
    !$omp private(i, j, j_tmp, offset_a, offset_b, rank_indx)

    !$omp do
    do j=0, size_outer-1
        offset_a = 0

        j_tmp = j

        do i=2, rank
            rank_indx = modulo(j_tmp, new_shape(permutation(i)))
            j_tmp = j_tmp / new_shape(permutation(i))
            offset_a = offset_a &
                + (rank_indx * stride_a(permutation(i))) &
                + (shifts(permutation(i)) * stride_a(permutation(i)))
        enddo
        offset_a = offset_a + (shift_inner * stride_a_inner)

        offset_b = j * size_inner
        do i=0, size_inner-1
            b(offset_b + i + 1) = a(offset_a + (i * stride_a_inner) + 1)
        enddo


    enddo
    !$omp end do

    !$omp end parallel


end subroutine reorder_shift

subroutine sum_stripe(rank, mat_shape, mat_size, perm_str, beta, A, B)

    use const, only: p, sp
    use cc_utils, only: gen_perm_array

    integer, intent(in) :: rank
    integer, intent(in) :: mat_shape(rank)
    integer, intent(in) :: mat_size
    character(len=*), intent(in) :: perm_str
    real(kind=4), intent(in) :: beta
    real(kind=8), intent(in out) :: A(mat_size)
    real(kind=8), intent(in) :: B(mat_size)

    integer :: perm(rank)

    integer :: ident(rank)

    integer :: stride_a(rank)
    integer :: stride_a_inner
    integer :: size_outer, size_inner
    integer :: offset_a, offset_b

    integer :: i, j, j_tmp
    integer :: current_index

    call gen_perm_array(perm_str, perm)

    do i=1, rank
        ident(i) = i
    enddo

    if (all(ident == perm)) then
        A = A + beta * B
        return
    endif

    stride_a(1) = 1
    do i=2, rank
        stride_a(i) = stride_a(i-1) * mat_shape(i-1)
    enddo
    !print *, 'size a',  size(a)
    !print *, 'stide_a', stride_a
    !print *, 'mat_shape', mat_shape

    size_outer = 1
    do i=1, rank
        if (i /= perm(1)) size_outer = size_outer * mat_shape(i)
    enddo
    !print *, 'size_outer', size_outer


    size_inner = mat_shape(perm(1))

    !$omp parallel default(none), &
    !$omp shared(size_outer, rank, mat_shape, perm, stride_a, size_inner, a, b, beta), &
    !$omp private(i, j, j_tmp, offset_a, offset_b, current_index, stride_a_inner)

    !$omp do
    do j=0, size_outer-1
        offset_a = 0

        j_tmp = j

        do i=2, rank

            current_index = modulo(j_tmp, mat_shape(perm(i)))
            j_tmp = j_tmp / mat_shape(perm(i))
            offset_a = offset_a + (current_index * stride_a(perm(i)))

        enddo

        offset_b = j * size_inner
        stride_a_inner = stride_a(perm(1))

        do i=0, size_inner-1
            a(offset_a + (i * stride_a_inner) + 1) = a(offset_a + (i * stride_a_inner) + 1) &
                + beta * b(offset_b + i + 1)
        enddo


    enddo
    !$omp end do

    !$omp end parallel


end subroutine sum_stripe

subroutine permute_array(rank, permutation, A, B)
    integer, intent(in) :: rank
    integer, intent(in) :: permutation(rank)
    integer, intent(in) :: A(rank)
    integer, intent(in out) :: B(rank)

    integer :: i

    do i=1, rank
        B(permutation(i)) = A(i)
    end do
end subroutine permute_array

!subroutine inverse_permute_array(rank, permutation, A, B)
!    integer, intent(in) :: rank
!    integer, intent(in) :: permutation(rank)
!    integer, intent(in) :: A(rank)
!    integer, intent(in out) :: B(rank)
!
!    integer :: i
!
!    do i=1, rank
!        B(i) = A(permutation(i))
!    end do
!end subroutine inverse_permute_array
