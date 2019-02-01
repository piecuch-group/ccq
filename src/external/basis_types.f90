! Module borrowed and adapted from HANDE
! https://github.com/hande-qmc/hande

module basis_types

    use const, only: i0

    type basis_t
        ! Number of basis functions.
        ! For the Hubbard model this is equal to twice the number of sites as there are
        ! 2 spin orbitals per site, for the Heisenberg model to the number of sites,
        ! for UEG equal to twice the number of k-points within the energy cutoff and for
        ! read in (e.g. molecular) systems the number of single-particle states read in.
        integer :: nbasis

        ! We commonly store the many-particle basis functions (e.g. spin
        ! products for the Heisenberg model, determinants for fermions) as a bit
        ! string.  The bit string is stored in an array of i0 integers.
        ! string_len gives the size of this array.
        ! The alpha and beta orbitals are interleaved and string_len is
        ! ceiling(nbasis/i0_length).
        !
        ! Note it's much more efficient to do operations on 32-bit or 64-bit
        ! integers than individual bits (or, indeed smaller integers) on modern
        ! architectures, so there's no need to cry about the bit type being
        ! removed from proposed F2008 standard or the wasted space at the end of
        ! the bit array.
        integer :: string_len

        ! The QMC algorithms implemented essentially sample a tensor of arbitrary rank.
        ! For example, FCIQMC samples a vector (rank 1) and DMQMC a matrix (rank 2).
        ! It is sometimes convenient to store/do operations on a bit string
        ! formed from concatenating the bit strings of the individual tensor
        ! labels.  Then length of this array is given by tensor_label_len.
        ! tensor_label_len = (rank of tensor) * string_len.
        ! NOTE: this must be set before running a QMC algorithm where the rank is not 1.
        integer :: tensor_label_len

        ! Bit masks to reveal the list of alpha basis functions and beta functions occupied
        ! in a Slater determinant.
        integer(i0) :: alpha_mask, beta_mask

        ! A determinant is stored in the array f(nbasis).  A basis function is occupied
        ! in the determinant if the relevant bit is set.  The relevant bit is given by
        ! bit_element, the element of the array which contains the bit corresponding to
        ! the basis function, and bit_position, which contains the position of the bit
        ! within the given element.  bit_lookup(:,i) gives the (/ bit_position,
        ! bit_element /) of the i-th basis function.
        ! Note fortran numbers bits starting from 0.
        integer, allocatable :: bit_lookup(:,:) ! (2, nbasis)

        ! The reverse lookup to bit_lookup.
        ! basis_lookup(i,j) gives the basis function corresponding to
        ! the i-th bit in the j-th element of a determinant array.
        integer, allocatable :: basis_lookup(:,:) ! (0:i0_end, string_len)

        ! excit_mask(:,i) is a bit field with bits corresponding to all orbitals with
        ! a higher index than i set.
        integer(i0), allocatable :: excit_mask(:,:) ! (string_len, nbasis)

    end type basis_t

contains

        subroutine init_basis_strings(b)

            ! Initialise the string information in a basis_t object for
            ! converting a bit-string representation of a list of orbitials to/from the
            ! integer list.

            ! In/Out:
            !   b: basis_t object to be set.  On input b%nbasis must be set.
            !      On output the bit string look-up tables are also set.

            use const, only: i0_end, i0_length
            use checking, only: check_allocate

            type(basis_t), intent(inout) :: b

            integer :: i, bit_element, bit_pos, ierr

            b%string_len = ceiling(real(b%nbasis)/i0_length)

            b%tensor_label_len = b%string_len

            ! Lookup arrays.
            allocate(b%bit_lookup(2,b%nbasis), stat=ierr)
            call check_allocate('b%bit_lookup',2*b%nbasis,ierr)
            allocate(b%basis_lookup(0:i0_end,b%string_len), stat=ierr)
            call check_allocate('b%basis_lookup',i0_length*b%string_len,ierr)
            b%basis_lookup = 0

            do i = 1, b%nbasis
                bit_pos = mod(i, i0_length) - 1
                if (bit_pos == -1) bit_pos = i0_end
                bit_element = (i+i0_end)/i0_length
                b%bit_lookup(:,i) = (/ bit_pos, bit_element /)
                b%basis_lookup(bit_pos, bit_element) = i
            end do

            ! Bit masks...
            ! Alpha basis functions are in the even bits.  alpha_mask = 01010101...
            ! Beta basis functions are in the odd bits.    beta_mask  = 10101010...
            b%alpha_mask = 0_i0
            b%beta_mask = 0_i0
            do i = 0, i0_end
                if (mod(i,2)==0) then
                    b%alpha_mask = ibset(b%alpha_mask,i)
                else
                    b%beta_mask = ibset(b%beta_mask,i)
                end if
            end do

        end subroutine init_basis_strings

        subroutine dealloc_basis_t(b)

            ! In/Out:
            !    b: basis_t object to be deallocated.

            use checking, only: check_deallocate

            type(basis_t), intent(inout) :: b
            integer :: ierr

            if (allocated(b%bit_lookup)) then
                deallocate(b%bit_lookup, stat=ierr)
                call check_deallocate('b%bit_lookup', ierr)
            end if
            if (allocated(b%basis_lookup)) then
                deallocate(b%basis_lookup, stat=ierr)
                call check_deallocate('b%basis_lookup', ierr)
            end if

        end subroutine dealloc_basis_t

end module basis_types
