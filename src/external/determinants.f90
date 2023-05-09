! Module borrowed and adapted from HANDE
! https://github.com/hande-qmc/hande

module determinants

! Generation, inspection and manipulation of Slater determinants.

use const, only: i0, i0_end, i0_length, p, dp

implicit none

! --- FCIQMC info ---

! A handy type for containing a lot of information about a determinant.
! This is convenient for passing around different amounts of info when
! we need consistent interfaces.
! Not all compenents are necessarily allocated: only those needed at the time.
type det_info_t
    ! bit representation of determinant.
    integer(i0), pointer :: f(:)  => NULL()  ! (string_len)
    ! List of occupied spin-orbitals.
    integer, pointer :: occ_list(:)  => NULL()  ! (nel)
    ! List of occupied alpha/beta spin-orbitals
    integer, pointer :: occ_list_alpha(:), occ_list_beta(:) !(nel) WARNING: don't assume otherwise.
    ! List of unoccupied alpha/beta spin-orbitals
    integer, pointer :: unocc_list_alpha(:), unocc_list_beta(:)
    ! Number of unoccupied orbitals with each spin and symmetry.
    ! The first index maps to spin using (Ms+3)/2, where Ms=-1 is spin-down and
    ! Ms=1 is spin-up.
    integer, pointer :: symunocc(:,:) ! (2,sym0_tot:sym_max_tot)
    ! \sum_i F_i - F_0, where F_i is the single-particle eigenvalue of the i-th occupied orbital
    ! and F_0 is the corresponding sum for the reference determinant.
    ! Initialize this as a signalling nan just in case
    real(p) :: fock_sum = huge(1.0_dp)
    ! TODO when appropriate more universal fortran support is available, use some sort of NaN above.

    ! Pointer (never allocated) to corresponding elements in particle_t%dat array.
    real(p), pointer :: data(:) => NULL()
end type det_info_t


contains

    subroutine gen_f_ref(sys, f_ref)

        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        integer(i0), intent(out) :: f_ref(sys%basis%string_len)

        integer :: ref_occ_list(sys%nel)
        integer :: i

        do i=1, sys%nel
            ref_occ_list(i) = i
        enddo

        call encode_det(sys%basis, ref_occ_list, f_ref)

    end subroutine gen_f_ref

!--- Initialisation and finalisation of det_info_t objects ---

    subroutine alloc_det_info_t(sys, det_info, allocate_bit_strings)

        ! Allocate the components of a det_info_t variable.

        ! In:
        !    sys: system to be studied (which defines the length of the
        !        det_info_t components).
        !    allocate_bit_strings (optional): if true (default), allocate the
        !        bit string attributes.  If false, then the bit string attributes
        !        can be used to point to already allocated bit strings.
        !        If set to false, the programmer *must* set allocated_bit_strings to
        !        false when calling dealloc_det_info_t.
        ! Out:
        !    det_info: det_info variable with components allocated to the
        !        appropriate sizes.

        use checking, only: check_allocate
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        logical, intent(in), optional :: allocate_bit_strings
        type(det_info_t), intent(inout) :: det_info
        logical :: alloc_f
        integer :: ierr

        ! Bit strings...
        if (present(allocate_bit_strings)) then
            alloc_f = allocate_bit_strings
        else
            alloc_f = .true.
        end if
        if (alloc_f) then
            allocate(det_info%f(sys%basis%string_len), stat=ierr)
            call check_allocate('det_info%f',sys%basis%string_len,ierr)
        end if

        ! Components for occupied basis functions...
        allocate(det_info%occ_list(sys%nel), stat=ierr)
        call check_allocate('det_info%occ_list',sys%nel,ierr)
        allocate(det_info%occ_list_alpha(sys%nel), stat=ierr)
        call check_allocate('det_info%occ_list_alpha',sys%nalpha,ierr)
        allocate(det_info%occ_list_beta(sys%nel), stat=ierr)
        call check_allocate('det_info%occ_list_beta',sys%nbeta,ierr)

        ! Components for unoccupied basis functions...
        allocate(det_info%unocc_list_alpha(sys%nvirt), stat=ierr)
        call check_allocate('det_info%unocc_list_alpha',sys%nvirt_alpha,ierr)
        allocate(det_info%unocc_list_beta(sys%nvirt), stat=ierr)
        call check_allocate('det_info%unocc_list_beta',sys%nvirt_beta,ierr)

        ! Components for symmetry summary of unoccupied basis functions...
        !allocate(det_info%symunocc(2,sys%sym0_tot:sys%sym_max_tot), stat=ierr)
        !call check_allocate('det_info%symunocc', 2*sys%nsym_tot, ierr)

    end subroutine alloc_det_info_t

    subroutine dealloc_det_info_t(det_info, allocated_bit_strings)

        ! Deallocate the components of a det_info_t variable.

        ! In:
        !    allocated_bit_strings (optional): if true (default), the
        !        bit string attributes are allocated and must be deallocated.
        !        If false, then the bit string attributes were just used to
        !        point to already allocated bit strings and don't need to be
        !        deallocated.  This *must* correspond to the alloc_bit_strings
        !        argument given to alloc_det_info_t.
        ! Out:
        !    det_info: det_info variable with all components deallocated.

        use checking, only: check_deallocate

        logical, intent(in), optional :: allocated_bit_strings
        type(det_info_t), intent(inout) :: det_info
        integer :: ierr

        logical :: alloc_f

        if (present(allocated_bit_strings)) then
            alloc_f = allocated_bit_strings
        else
            alloc_f = .true.
        end if

        if (alloc_f) then
            deallocate(det_info%f, stat=ierr)
            !call check_deallocate('det_info%f',ierr)
        end if
        deallocate(det_info%occ_list, stat=ierr)
        call check_deallocate('det_info%occ_list',ierr)
        deallocate(det_info%occ_list_alpha, stat=ierr)
        call check_deallocate('det_info%occ_list_alpha',ierr)
        deallocate(det_info%occ_list_beta, stat=ierr)
        call check_deallocate('det_info%occ_list_beta',ierr)
        deallocate(det_info%unocc_list_alpha, stat=ierr)
        call check_deallocate('det_info%unocc_list_alpha',ierr)
        deallocate(det_info%unocc_list_beta, stat=ierr)
        call check_deallocate('det_info%unocc_list_beta',ierr)
        !deallocate(det_info%symunocc, stat=ierr)
        !call check_deallocate('det_info%symunocc',ierr)

    end subroutine dealloc_det_info_t

!--- Encode determinant bit strings ---

    pure subroutine encode_det(basis_set, occ_list, bit_list)

        ! In:
        !    basis_set: information about the single-particle basis.
        !    occ_list(nel): integer list of occupied orbitals in the Slater determinant.
        ! Out:
        !    bit_list(string_len): a bit string representation of the occupied
        !        orbitals.   The first element contains the first i0_length basis
        !        functions, the second element the next i0_length and so on.  A basis
        !        function is ocupied if the relevant bit is set.

        use basis_types, only: basis_t

        type(basis_t), intent(in) :: basis_set
        integer, intent(in) :: occ_list(:)
        integer(i0), intent(out) :: bit_list(basis_set%string_len)
        integer :: i, orb, bit_pos, bit_element

        bit_list = 0
        do i = 1, size(occ_list)
            orb = occ_list(i)
            bit_pos = basis_set%bit_lookup(1,orb)
            bit_element = basis_set%bit_lookup(2,orb)
            bit_list(bit_element) = ibset(bit_list(bit_element), bit_pos)
        end do

    end subroutine encode_det

!--- Decode determinant bit strings ---

    pure subroutine decode_det(basis_set, f, occ_list)

        ! In:
        !    basis_set: information about the single-particle basis.
        !    f(:): bit string representation of the Slater
        !        determinant.
        ! Out:
        !    occ_list(:): integer list of occupied orbitals in the Slater
        !        determinant. (min size: number of electrons.)

        ! This algorithm has a look over Nbits/256 rather than Nbits, and so
        ! is most likely dominated by O(N_el) scaling.

        use basis_types, only: basis_t
        use bit_table_256_m, only: bit_table_256

        type(basis_t), intent(in) :: basis_set
        integer(i0), intent(in) :: f(basis_set%string_len)
        integer, intent(out) :: occ_list(:)

        ! The lookup table contains the list of bits set for all possible integers contained in a given number of bits.
        ! Number of bits in integers in the lookup table (assume a power of 2!).
        integer(i0), parameter :: field_size = ubound(bit_table_256, dim=1)
        ! Number of such bit chunks in integers of kind i0.
        integer, parameter :: nfields = i0_length/field_size
        ! Bit mask to extract a chunk containing field_size bits.
        integer(i0), parameter :: mask = 2**field_size - 1

        integer :: iel, ifield, nfound, nbits_seen
        integer(i0) :: offset, field

        ! WARNING: we assume that the basis functions 1,2,..., correspond to bits 0,1,...
        ! in the first integer of f and so on (i.e. basis_set%separate_strings is false).

        nfound = 0
        nbits_seen = 0
        outer: do iel = 1, basis_set%string_len
            offset = 0
            do ifield = 1, nfields
                ! Inspect one byte at a time.
                field = iand(mask, ishft(f(iel), -offset))
                associate(in_field=>bit_table_256(0,field))
                    ! 1-based index in lookup table, which matches the orbitals indexing scheme.
                    occ_list(nfound+1:nfound+in_field) = bit_table_256(1:in_field, field) + nbits_seen
                    nfound = nfound + in_field
                end associate
                offset = offset + field_size
                nbits_seen = nbits_seen + field_size
            end do
            if (nfound == size(occ_list)) exit outer
        end do outer

    end subroutine decode_det

    pure subroutine decode_det_occ(sys, f, d)

        ! Decode determinant bit string into integer list containing the
        ! occupied orbitals.
        !
        ! In:
        !    sys: system being studied (contains required basis information).
        !    f(string_len): bit string representation of the Slater
        !        determinant.
        ! Out:
        !    d: det_info_t variable.  The following components are set:
        !        occ_list: integer list of occupied spin-orbitals in the
        !            Slater determinant.

        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        integer(i0), intent(in) :: f(sys%basis%string_len)
        type(det_info_t), intent(inout) :: d

        call decode_det(sys%basis, f, d%occ_list)

    end subroutine decode_det_occ

    pure subroutine decode_det_full(basis_set, f, occ_list, unocc_list)

        ! Decode determinant bit string into integer lists containing the
        ! occupied and unoccupied orbitals.
        !
        ! In:
        !    basis_set: information about the single-particle basis.
        !    f(:): bit string representation of the Slater
        !        determinant.
        ! Out:
        !    occ_list(:): integer list of occupied orbitals in the Slater
        !        determitant. (size: number of electrons)
        !    unocc_list(:): integer list of unoccupied orbitals in the Slater
        !        determitant. (size: number of orbitals - number of electrons)

        use basis_types, only: basis_t

        type(basis_t), intent(in) :: basis_set
        integer(i0), intent(in) :: f(basis_set%string_len)
        integer, intent(out) :: occ_list(:), unocc_list(:)
        integer :: i, j, iocc, iunocc, orb, last_basis_ind

        ! A bit too much to do the chunk-based decoding of the occupied list and then fill
        ! in the remaining information.  We only use this in Hubbard model calculations in
        ! k-space, so for now just do a (slow) bit-wise inspection.

        iocc = 0
        iunocc = 0
        orb = 0

        do i = 1, basis_set%string_len - 1
            ! Manual unrolling allows us to avoid 2 mod statements
            ! and some branching.
            do j = 0, i0_end
                ! Test alpha orbital.
                orb = orb + 1
                if (btest(f(i), j)) then
                    iocc = iocc + 1
                    occ_list(iocc) = orb
                else
                    iunocc = iunocc + 1
                    unocc_list(iunocc) = orb
                end if
            end do
        end do

        ! Deal with the last element in the determinant bit array separately.
        ! Note that decoding a bit string is surprisingly slow (or, more
        ! importantly, adds up when doing billions of times).
        ! Treating the last element as a special case rather than having an if
        ! statement in the above loop results a speedup of the Hubbard k-space
        ! FCIQMC calculations of 1.5%.
        last_basis_ind = basis_set%nbasis - i0_length*(basis_set%string_len-1) - 1
        do j = 0, last_basis_ind
            ! Test alpha orbital.
            orb = orb + 1
            if (btest(f(i), j)) then
                iocc = iocc + 1
                occ_list(iocc) = orb
            else
                iunocc = iunocc + 1
                unocc_list(iunocc) = orb
            end if
        end do

    end subroutine decode_det_full

    pure subroutine decode_det_spinocc_spinunocc(sys, f, d)

        ! Decode determinant bit string into integer lists containing the
        ! occupied and unoccupied orbitals.
        !
        ! We return the lists for alpha and beta electrons separately.
        !
        ! In:
        !    f(string_len): bit string representation of the Slater
        !        determinant.
        ! Out:
        !    d: det_info_t variable.  The following components are set:
        !        occ_list: integer list of occupied spin-orbitals in the
        !            Slater determinant.
        !        occ_list_alpha: integer list of occupied alpha
        !            spin-orbitals in the Slater determinant.
        !        occ_list_beta: integer list of occupied beta
        !            spin-orbitals in the Slater determinant.
        !        unocc_list_alpha: integer list of unoccupied alpha
        !            spin-orbitals in the Slater determinant.
        !        unocc_list_beta: integer list of unoccupied beta
        !            spin-orbitals in the Slater determinant.

        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        integer(i0), intent(in) :: f(sys%basis%string_len)
        type(det_info_t), intent(inout) :: d
        integer :: i, j, iocc, iocc_a, iocc_b, iunocc_a, iunocc_b, orb, last_basis_ind

        ! A bit too much to do the chunk-based decoding of the occupied list and then fill
        ! in the remaining information.  We only use this in Hubbard model calculations in
        ! k-space, so for now just do a (slow) bit-wise inspection.

        iocc = 0
        iocc_a = 0
        iocc_b = 0
        iunocc_a = 0
        iunocc_b = 0
        orb = 0

        do i = 1, sys%basis%string_len - 1
            ! Manual unrolling allows us to avoid 2 mod statements
            ! and some branching.
            do j = 0, i0_end, 2
                ! Test alpha orbital.
                orb = orb + 1
                if (btest(f(i), j)) then
                    iocc = iocc + 1
                    iocc_a = iocc_a + 1
                    d%occ_list(iocc) = orb
                    d%occ_list_alpha(iocc_a) = orb
                else
                    iunocc_a = iunocc_a + 1
                    d%unocc_list_alpha(iunocc_a) = orb
                end if
                ! Test beta orbital.
                orb = orb + 1
                if (btest(f(i), j+1)) then
                    iocc = iocc + 1
                    iocc_b = iocc_b + 1
                    d%occ_list(iocc) = orb
                    d%occ_list_beta(iocc_b) = orb
                else
                    iunocc_b = iunocc_b + 1
                    d%unocc_list_beta(iunocc_b) = orb
                end if
            end do
        end do

        ! Deal with the last element in the determinant bit array separately.
        ! Note that decoding a bit string is surprisingly slow (or, more
        ! importantly, adds up when doing billions of times).
        ! Treating the last element as a special case rather than having an if
        ! statement in the above loop results a speedup of the Hubbard k-space
        ! FCIQMC calculations of 1.5%.
        last_basis_ind = sys%basis%nbasis - i0_length*(sys%basis%string_len-1) - 1
        do j = 0, last_basis_ind, 2
            ! Test alpha orbital.
            orb = orb + 1
            if (btest(f(i), j)) then
                iocc = iocc + 1
                iocc_a = iocc_a + 1
                d%occ_list(iocc) = orb
                d%occ_list_alpha(iocc_a) = orb
            else
                iunocc_a = iunocc_a + 1
                d%unocc_list_alpha(iunocc_a) = orb
            end if
            ! Test beta orbital.
            orb = orb + 1
            if (btest(f(i), j+1)) then
                iocc = iocc + 1
                iocc_b = iocc_b + 1
                d%occ_list(iocc) = orb
                d%occ_list_beta(iocc_b) = orb
            else
                iunocc_b = iunocc_b + 1
                d%unocc_list_beta(iunocc_b) = orb
            end if
        end do

    end subroutine decode_det_spinocc_spinunocc

!--- Extract information from bit strings ---

    pure function spin_orb_list(orb_list) result(ms)

        ! In:
        !    basis_fns: list of single-particle basis functions.
        !    orb_list: list of orbitals (e.g. determinant).
        ! Returns:
        !    Ms: total spin of the determinant in units of electron spin (1/2).

        integer :: ms
        integer, intent(in) :: orb_list(:)

        integer :: i

        ms = 0
        do i = lbound(orb_list, dim=1), ubound(orb_list, dim=1)
            ms = ms + mod(orb_list(i), 2)
        end do

    end function spin_orb_list

!--- Output ---

    subroutine update_sys_spin_info(cdet, sys, nalpha, nbeta)

        ! Determine the spin polarisation from a given determinant and set
        ! system spin polarisation accordingly.

        ! In:
        !    cdet: det_info_t object with occ_list set, from which the total ms
        !       derives.
        ! In/Out:
        !    sys: sys_t object. On output spin polarisation (nalpha, nvirt ..)
        !       will be correctly set.

        use bit_utils, only: count_set_bits, count_even_set_bits
        use system, only: sys_t

        type(det_info_t), intent(in) :: cdet
        type(sys_t), intent(inout) :: sys
        integer, intent(inout) :: nalpha, nbeta

        nalpha = sum(count_even_set_bits(cdet%f))
        nbeta = sys%nel - nalpha
        !sys%nvirt_alpha = sys%basis%nbasis/2 - sys%nalpha
        !sys%nvirt_beta = sys%basis%nbasis/2 - sys%nbeta

    end subroutine update_sys_spin_info

end module determinants
