!> \file dictionary_m.f90
!! \brief Module file for dictionary_t

!> Dictionary type that uses strings for the keys and values
!!
!! Design:
!!  - djb2 hash function (D. J. Bernstein, see http://www.cse.yorku.ca/~oz/hash.html)
!!  - The strings are all "character(len=:), allocatable" variables
!!  - There is no linked list nor pointers, only allocatable arrays for the dynamic data structure
!!  - set rewrites existing entries without complaining

module det_hash

    use const, only: i0

    implicit none

    private

    public :: dictionary_t, murmurhash_bit_string, hash_exist, get_val

    !> Single entry in the dictionary
    type entry_t
        integer(i0), allocatable :: key(:)
        integer :: value
    end type entry_t

    !> A bucket contains several entries
    type bucket_t
        type(entry_t), allocatable :: entries(:)
        integer :: current_size = 0
        integer :: current_idx = 0
    contains
        procedure :: find
    end type bucket_t

    !> The dictionary contains dict_size buckets (defined at run time)
    type dictionary_t
        type(bucket_t), allocatable :: buckets(:)
        integer :: dict_size = 0
    contains
        procedure :: djb2
        procedure :: set
        procedure :: get
        procedure :: init
        procedure :: show
    end type dictionary_t

    integer, parameter :: BUCKET_EMPTY = -2
    integer, parameter :: BUCKET_ENTRY_NOT_FOUND = -4

    interface
        ! Interfaces to hashing algorithms written in C.
        function MurmurHash2(key, N, seed) result(hash) bind(c, name='MurmurHash2')
            ! In:
            !    key: data to be hashed.
            !    seed: random(ish!) number to seed the hash.
            ! In/Out:
            !    N: number of bytes in data.
            ! Returns:
            !    32-bit hash of data.
            use, intrinsic:: iso_c_binding
            integer(c_int32_t) :: hash
            type(c_ptr), intent(in), value :: key
            integer(c_int), intent(in), value :: N
            integer(c_int32_t), intent(in), value :: seed
        end function MurmurHash2
    end interface

contains

    !function murmurhash_bit_string(f, N, seed) result(hash)
    function murmurhash_bit_string(f, N) result(hash)

        ! Wrapper around MurmurHash2.

        ! In:
        !    f: bit string.
        !    N: length of bits in bit string.  NOTE: we hash in multiples of 32 bits.
        ! Returns:
        !    Hash of f using the MurmurHash2 algorithm.

        use, intrinsic:: iso_c_binding
        use const, only: i0

        integer :: hash
        integer, intent(in) :: N
        integer(i0), intent(in), target :: f(N)
        integer(c_int32_t), parameter :: seed = 10
        type(c_ptr) :: key
        integer(c_int) :: nbytes

        ! Pass MurmurHash2 a multiple of 32-bits.
        ! Note that DET_SIZE must currently be 32 or 64 bits, so this is safe!
        ! The size parameter used in Murmurhash is in bytes...
        nbytes = ceiling(real(N)/32)*4

        ! Unfortunately it seems c_loc is not required to be pure by the
        ! F2003 standards! :-(
        key = c_loc(f(1))

        hash = MurmurHash2(key, nbytes, seed)

    end function murmurhash_bit_string


    ! Testing
    ! [TMPDEBUG]
    function hash_exist(s, dict_size, dict) result(exists)

        logical :: exists

        integer(i0), intent(in) :: s(:)
        integer, intent(in) :: dict_size
        type(dictionary_t), intent(in) :: dict

        integer :: string_len
        integer :: hash
        integer :: r
        integer :: i

        integer :: b_idx

        exists = .true.

        string_len = size(s)
        hash = int(murmurhash_bit_string(s, string_len))
        r = modulo(hash, dict_size) + 1

        if (dict%buckets(r)%current_size == 0) then
            exists = .false.
            return
        end if

        exists = .false.
        do i = 1, dict%buckets(r)%current_size
            if (all(dict%buckets(r)%entries(i)%key == s)) then
                exists = .true.
                exit
            end if
        end do

    end function hash_exist

    ! Testing
    ! [TMPDEBUG]
    function get_val(s, dict_size, dict) result(val)

        integer :: val

        integer(i0), intent(in) :: s(:)
        integer, intent(in) :: dict_size
        type(dictionary_t), intent(in) :: dict

        integer :: string_len
        integer :: hash
        integer :: r
        integer :: i

        integer :: b_idx

        string_len = size(s)
        hash = int(murmurhash_bit_string(s, string_len))
        r = modulo(hash, dict_size) + 1

        if (dict%buckets(r)%current_size == 0) then
            val = 0
            return
        end if

        val = 0
        do i = 1, dict%buckets(r)%current_size
            if (all(dict%buckets(r)%entries(i)%key == s)) then
                val = dict%buckets(r)%entries(i)%value
                return
            end if
        end do

    end function get_val

    !> djb2 hash function
    !!
    !! \param this the dictionary_t object
    !! \param s a string
    !!
    !! \return the hash value between 0 and dict_size-1
    function djb2(this, s) result(r)
    class(dictionary_t), intent(in) :: this
        integer(i0), intent(in) :: s(:)
        integer :: string_len
        integer :: hash
        integer :: r

        string_len = size(s)
        hash = int(murmurhash_bit_string(s, string_len))
        r = modulo(hash, this%dict_size) + 1

    end function djb2

    !> Add or replace an entry in the dictionary
    !!
    !! \param this the dictionary_t object
    !! \param k the key
    !! \param v the value
    subroutine set(this, k, v)
    class(dictionary_t), intent(inout) :: this
        integer(i0), intent(in) :: k(:)
        integer :: v

        type(bucket_t) :: tmp_bucket

        integer :: h, i, b_idx

        h = this%djb2(k)

        b_idx = this%buckets(h)%find(k)

        if (b_idx == BUCKET_EMPTY) then
            ! allocate bucket for 1 entry
            ! also, means we can take the first entry
            allocate(this%buckets(h)%entries(1))
            this%buckets(h)%current_size = 1
            this%buckets(h)%current_idx = 1
            b_idx = 1
            this%buckets(h)%entries(1)%key = k
            this%buckets(h)%entries(1)%value = v
            ! the values are registered, exit
            return
        end if

        if (b_idx == BUCKET_ENTRY_NOT_FOUND) then
            ! copy and grow bucket entries

            allocate(tmp_bucket%entries(this%buckets(h)%current_size + 1))
            tmp_bucket%current_size = this%buckets(h)%current_size + 1
            tmp_bucket%current_idx = this%buckets(h)%current_idx + 1

            do i = 1, this%buckets(h)%current_size
                tmp_bucket%entries(i)%key = this%buckets(h)%entries(i)%key
                tmp_bucket%entries(i)%value = this%buckets(h)%entries(i)%value
            end do

            deallocate(this%buckets(h)%entries)
            allocate(this%buckets(h)%entries, source=tmp_bucket%entries)
            deallocate(tmp_bucket%entries)

            this%buckets(h)%current_size = tmp_bucket%current_size
            this%buckets(h)%current_idx = tmp_bucket%current_idx
            b_idx = this%buckets(h)%current_idx
        end if

        if (b_idx > 0) then
            this%buckets(h)%entries(b_idx)%key = k
            this%buckets(h)%entries(b_idx)%value = v
        end if

    end subroutine set

    !> Initialize a dictionary object
    !!
    !! \param this the dictionary_t object
    !! \param dict_size the size of the hash table
    subroutine init(this, dict_size)
    class(dictionary_t), intent(out) :: this
        integer, intent(in) :: dict_size

        allocate(this%buckets(dict_size))
        this%dict_size = dict_size

    end subroutine init

    !> Display the content of a dictionary
    !!
    !! \param this the dictionary_t object
    subroutine show(this)
    class(dictionary_t), intent(in) :: this

        integer :: i, j, s
        integer :: n

        n = 0
        do i = 1, this%dict_size
            s = this%buckets(i)%current_idx
            if (s > 0) then
                write(*,*) 'bucket   : ', i, ' size ', s
                do j = 1, s
                    write(*,*) 'key      : ', this%buckets(i)%entries(j)%key
                    write(*,*) 'value    : ', this%buckets(i)%entries(j)%value
                end do
            end if
        end do

    end subroutine show

    !> Find the "in-bucket" index for a given key
    !!
    !! Negative return values correspond to module-defined return codes.
    !!
    !! \param this the bucket_t object
    !! \param k the key
    !!
    !! \return the index (1-based) of the key in the bucket or a return code
    function find(this, k) result(r)
    class(bucket_t), intent(in) :: this
        integer(i0), intent(in) :: k(:)
        integer :: r

        integer :: i

        if (this%current_size == 0) then
            r = BUCKET_EMPTY
            return
        end if

        r = BUCKET_ENTRY_NOT_FOUND
        do i = 1, this%current_size
            if (all(this%entries(i)%key == k)) then
                r = i
                exit
            end if
        end do

    end function find

    !> Fetch an entry in the dictionary.
    !!
    !! \param this the dictionary_t object
    !! \param k the key
    !!
    !! \return the value if found, an empty string else
    function get(this, k) result(r)
    class(dictionary_t), intent(in) :: this
        integer(i0), intent(in) :: k(:)

        integer :: r

        integer :: h, b_idx

        integer :: string_len

        h = this%djb2(k)

        b_idx = this%buckets(h)%find(k)

        if ( (b_idx == BUCKET_EMPTY) .or. &
            (b_idx == BUCKET_ENTRY_NOT_FOUND) ) then
            r = 0
            return
        end if

        if (b_idx>0) then
            r = this%buckets(h)%entries(b_idx)%value
        end if

    end function get

end module det_hash
