module hdf5_io


    use const, only: int_32


#ifdef ENABLE_HDF5

    use hdf5, only: hid_t, hsize_t

    implicit none

    integer(int_32), parameter :: size_2gb = 268435456
    integer(int_32), parameter :: size_4gb = 536870912
    integer(int_32), parameter :: size_chunk = size_2gb
    ! HDF5 kinds equivalent to the kinds defined in const.  Set in
    ! hdf5_kinds_init.
    type hdf5_kinds_t
        integer(hid_t) :: i32
        integer(hid_t) :: i64
        integer(hid_t) :: i0
        integer(hid_t) :: sp
        integer(hid_t) :: dp
    end type hdf5_kinds_t

#else
    implicit none
#endif

contains

#ifdef ENABLE_HDF5

    subroutine hdf5_kinds_init(kinds)

        ! Out:
        !    kinds: HDF5 kinds corresponding to integer and real kinds used in the QMC algorithms.

        ! WARNING: Take care.  If called before h5open_f is called, then it will return garbage.
        !          Further, the kind values are *not* constant between closing and then re-opening
        !          the HDF5 library.  Learn from my (painful) experiences...

        use const, only: int_32, int_64, i0, sp, dp
        use hdf5, only: H5_INTEGER_KIND, H5_REAL_KIND, h5kind_to_type

        type(hdf5_kinds_t), intent(out) :: kinds

        ! Convert our non-standard kinds to something HDF5 understands.
        kinds%i32 = h5kind_to_type(int(int_32, kind=int_32), H5_INTEGER_KIND)
        kinds%i64 = h5kind_to_type(int(int_64, kind=int_32), H5_INTEGER_KIND)
        kinds%i0 = h5kind_to_type(int(i0, kind=int_32), H5_INTEGER_KIND)
        kinds%sp = h5kind_to_type(int(sp, kind=int_32), H5_REAL_KIND)
        kinds%dp = h5kind_to_type(int(dp, kind=int_32), H5_REAL_KIND)

    end subroutine hdf5_kinds_init

    subroutine write_calc_data(run, cc)

        use const, only: int_32, int_64
        use system, only: run_t
        use cc_types, only: cc_t

        use hdf5, only: h5open_f, h5fcreate_f, h5gcreate_f, &
            h5gclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_TRUNC_F

        type(run_t), intent(in) :: run
        type(cc_t), intent(in) :: cc

        integer(hid_t) :: fid, gid
        integer(int_32) :: ierr

        ! Init hdf5
        call h5open_f(ierr)
        call h5fcreate_f("data_"//trim(run%uuid)//".h5", H5F_ACC_TRUNC_F, fid, ierr)

        ! Create vecs group
        call h5gcreate_f(fid, "vecs", gid, ierr)


        ! Write CC vectors
        call write_dataset_1d_real_dp(gid, "t_vec", cc%t_vec)
        if (run%lcc) call write_dataset_1d_real_dp(gid, "l_vec", cc%l_vec)


        call h5gclose_f(gid, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine write_calc_data

    subroutine init_dset(filename, dset_name, dims)

        ! Initialize dataset in a HDF5 file

        ! In:
        !   filename: name of the already initialized HDF5 file
        !   dset_name: name of the dataset
        !   dims: dimensions of the dataset (a.k.a. shape)

        ! Out:
        !   dataset initialized in HDF5 file

        use const, only: int_32, int_64
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, &
            h5dcreate_f, &
            h5screate_simple_f, &
            h5dclose_f, h5sclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDWR_F


        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        integer, intent(in) :: dims(:)

        integer(int_32) :: rank
        integer(hid_t) :: fid
        integer(hid_t) :: dspace_id, dset_id
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        rank = int(size(dims), int_32)

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in R/W mode
        call h5fopen_f(trim(filename), H5F_ACC_RDWR_F, fid, ierr)

        ! Create rank 1 simple space
        call h5screate_simple_f(rank, int(dims, hid_t), dspace_id, ierr)
        call h5dcreate_f(fid, trim(dset_name), kinds%dp, dspace_id, dset_id, ierr)

        ! Close dataset, dataspace, and file
        call h5dclose_f(dset_id, ierr)
        call h5sclose_f(dspace_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine init_dset

    subroutine init_h5_file(filename)

        use const, only: int_32, int_64

        use hdf5, only: h5open_f, h5fcreate_f, &
            h5fclose_f, h5close_f, &
            H5F_ACC_TRUNC_F

        character(len=*), intent(in) :: filename

        integer(hid_t) :: fid
        integer(int_32) :: ierr

        ! Init hdf5
        call h5open_f(ierr)
        call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, fid, ierr)
        call h5fclose_f(fid, ierr)
        call h5close_f(ierr)

    end subroutine init_h5_file

    subroutine write_vector(filename, dset_name, vector, vector_size)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
            h5dwrite_f, &
            h5dclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDWR_F


        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        real(dp), intent(in) :: vector(:)
        integer, intent(in) :: vector_size

        integer(hid_t) :: fid
        integer(hid_t) :: dset_id
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in R/W mode
        call h5fopen_f(trim(filename), H5F_ACC_RDWR_F, fid, ierr)

        ! Open dataset
        call h5dopen_f(fid, trim(dset_name), dset_id, ierr)

        ! Write to dataset
        call h5dwrite_f(dset_id, kinds%dp, vector, [int(vector_size, hsize_t)], ierr)


        ! Close dataset, dataspace, and file
        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine write_vector

    subroutine write_matrix(filename, dset_name, matrix, matrix_shape, matrix_size)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
            h5dwrite_f, &
            h5dclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDWR_F


        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        integer, intent(in) :: matrix_size
        real(dp), intent(in) :: matrix(matrix_size)
        integer, intent(in) :: matrix_shape(:)

        integer(int_32) :: rank

        integer(hid_t) :: fid
        integer(hid_t) :: dset_id
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        rank = int(size(matrix_shape), int_32)

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in R/W mode
        call h5fopen_f(trim(filename), H5F_ACC_RDWR_F, fid, ierr)

        ! Open dataset
        call h5dopen_f(fid, trim(dset_name), dset_id, ierr)

        ! Write to dataset
        call h5dwrite_f(dset_id, kinds%dp, matrix, int(matrix_shape, hsize_t), ierr)


        ! Close dataset, dataspace, and file
        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine write_matrix

    subroutine write_vector_from_scratch(filename, dset_name, vector, vector_size)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5

        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        real(dp), intent(in) :: vector(:)
        integer, intent(in) :: vector_size

        integer(hid_t) :: fid
        integer(hid_t) :: dset_id, dspace_id
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in R/W mode
        call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, fid, ierr)

        call h5screate_simple_f(1_int_32, [int(vector_size, hsize_t)], dspace_id, ierr)

        call h5dcreate_f(fid, dset_name, H5T_NATIVE_DOUBLE, dspace_id, &
            dset_id, ierr)

        ! Write to dataset
        call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vector, [int(vector_size, hsize_t)], ierr)

        ! Close dataset, dataspace, and file
        call h5sclose_f(dspace_id, ierr)
        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine write_vector_from_scratch

    subroutine write_mat_from_scratch(filename, dset_name, mat, mat_dims, mat_size)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5

        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        integer, intent(in) :: mat_size
        real(dp), intent(in) :: mat(mat_size)
        integer, intent(in) :: mat_dims(:)

        integer(hid_t) :: fid
        integer(hid_t) :: dset_id, dspace_id
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds
        integer(hsize_t), allocatable :: mat_dims_in(:)
        integer(int_32) :: rank

        rank = int(size(mat_dims), int_32)
        allocate(mat_dims_in(rank))
        mat_dims_in = int(mat_dims, hsize_t)

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in R/W mode
        call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, fid, ierr)

        call h5screate_simple_f(rank, mat_dims_in, dspace_id, ierr)

        call h5dcreate_f(fid, dset_name, kinds%dp, dspace_id, &
            dset_id, ierr)

        ! Write to dataset
        call h5dwrite_f(dset_id, kinds%dp, mat, mat_dims_in, ierr)


        ! Close dataset, dataspace, and file
        call h5sclose_f(dspace_id, ierr)
        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

        deallocate(mat_dims_in)

    end subroutine write_mat_from_scratch

    subroutine write_column_in_mat(filename, dset_name, vector, col_size, col_indx)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
            h5dwrite_f, h5dget_space_f, &
            h5screate_simple_f, h5sselect_hyperslab_f, &
            h5sclose_f, h5dclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDWR_F, H5S_SELECT_SET_F


        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        real(dp), intent(in) :: vector(:)
        integer, intent(in) :: col_size
        integer, intent(in) :: col_indx

        integer(int_32), parameter :: rank = 2_int_32
        integer(int_32), parameter :: rank_1d = 1_int_32
        integer(hid_t) :: fid
        integer(hid_t) :: dspace_id, dset_id
        integer(hid_t) :: memspace
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds


        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in R/W mode
        call h5fopen_f(trim(filename), H5F_ACC_RDWR_F, fid, ierr)

        ! Open dataset
        call h5dopen_f(fid, trim(dset_name), dset_id, ierr)
        call h5dget_space_f(dset_id, dspace_id, ierr)

        ! Create in-memory datasapce. Because it is a vector, it is rank-1
        call h5screate_simple_f(rank_1d, [int(col_size, hsize_t)], memspace, ierr)

        ! Select the file's hyperslab
        call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
            [0_hsize_t, int(col_indx - 1, hsize_t)], &
            [int(col_size, hsize_t), 1_hsize_t], ierr)

        call h5dwrite_f(dset_id, kinds%dp, vector, [int(col_size, hsize_t)], ierr, &
            memspace, dspace_id)


        ! Close dataset, dataspace, and file
        call h5sclose_f(dspace_id, ierr)
        call h5sclose_f(memspace, ierr)

        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine write_column_in_mat


    subroutine read_popsfile_h5(filename, dets, coefs, list_size)

        use const, only: int_32, int_64, i0, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
             h5dread_f, h5dget_space_f, &
             h5sget_simple_extent_dims_f, h5sget_simple_extent_ndims_f, &
             h5sclose_f, h5dclose_f, h5fclose_f, h5close_f, &
             H5F_ACC_RDONLY_F

        character(len=*), intent(in) :: filename
        integer(i0), allocatable, intent(in out) :: dets(:,:)
        real(dp), allocatable, intent(in out) :: coefs(:,:)
        integer, intent(out) :: list_size

        integer(hid_t) :: fid
        integer(hid_t) :: dspace_id, dset_id

        integer(int_32) :: rank
        integer(hsize_t), allocatable :: dims(:)
        integer(hsize_t), allocatable :: maxdims(:)

        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in RO mode
        call h5fopen_f(trim(filename), H5F_ACC_RDONLY_F, fid, ierr)



        ! Read ilut matrix
        ! Open dataset
        call h5dopen_f(fid, "/wavefunction/ilut", dset_id, ierr)

        ! Open data space
        call h5dget_space_f(dset_id, dspace_id, ierr)
        call h5sget_simple_extent_ndims_f(dspace_id, rank, ierr)

        ! Allocate dims and ndims
        allocate(dims(rank), maxdims(rank))
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr)

        allocate(dets(int(dims(1)), int(dims(2))))

        call h5sclose_f(dspace_id, ierr)

        ! Read ilut array
        call h5dread_f(dset_id, kinds%i64, dets, dims, ierr)

        list_size = dims(2)

        deallocate(dims, maxdims)

        ! Close dataset, dataspace, and file
        call h5dclose_f(dset_id, ierr)




        ! Read sgns matrix
        ! Open dataset
        call h5dopen_f(fid, "/wavefunction/sgns", dset_id, ierr)

        ! Open data space
        call h5dget_space_f(dset_id, dspace_id, ierr)
        call h5sget_simple_extent_ndims_f(dspace_id, rank, ierr)

        ! Allocate dims and ndims
        allocate(dims(rank), maxdims(rank))
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr)

        call h5sclose_f(dspace_id, ierr)

        allocate(coefs(int(dims(1)), int(dims(2))))

        ! Read ilut array
        call h5dread_f(dset_id, kinds%dp, coefs, dims, ierr)
        deallocate(dims, maxdims)

        ! Close dataset, dataspace, and file
        call h5dclose_f(dset_id, ierr)




        ! Close dataset, dataspace, and file
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)


    end subroutine read_popsfile_h5

    subroutine read_column_in_mat(filename, dset_name, vector, col_size, col_indx)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
            h5dread_f, h5dget_space_f, &
            h5screate_simple_f, h5sselect_hyperslab_f, &
            h5sclose_f, h5dclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDONLY_F, H5S_SELECT_SET_F


        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        real(dp), intent(inout) :: vector(:)
        integer, intent(in) :: col_size
        integer, intent(in) :: col_indx

        integer(int_32), parameter :: rank = 2_int_32
        integer(int_32), parameter :: rank_1d = 1_int_32
        integer(hid_t) :: fid
        integer(hid_t) :: dspace_id, dset_id
        integer(hid_t) :: memspace
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in RO mode
        call h5fopen_f(trim(filename), H5F_ACC_RDONLY_F, fid, ierr)

        ! Open dataset
        call h5dopen_f(fid, trim(dset_name), dset_id, ierr)
        call h5dget_space_f(dset_id, dspace_id, ierr)

        ! Create in-memory datasapce. Because it is a vector, it is rank-1
        call h5screate_simple_f(rank_1d, [int(col_size, hsize_t)], memspace, ierr)

        ! Select the file's hyperslab
        call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
            [0_hsize_t, int(col_indx - 1, hsize_t)], &
            [int(col_size, hsize_t), 1_hsize_t], ierr)

        call h5dread_f(dset_id, kinds%dp, vector, [int(col_size, hsize_t)], ierr, &
            memspace, dspace_id)

        ! Close dataset, dataspace, and file
        call h5sclose_f(dspace_id, ierr)
        call h5sclose_f(memspace, ierr)

        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine read_column_in_mat

    subroutine get_chunk_daxpy(filename, dset_name, vector, C, diis_space)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
            h5dget_space_f, h5dread_f, &
            h5sget_simple_extent_dims_f, h5screate_simple_f, &
            h5sselect_hyperslab_f, &
            h5sclose_f, h5dclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDONLY_F, H5S_SELECT_SET_F


        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        real(dp), intent(inout) :: vector(:)
        real(dp), intent(in) :: C(:)
        integer, intent(in) :: diis_space


        integer :: col_indx
        integer(int_32), parameter :: rank = 2_int_32
        integer(int_32), parameter :: rank_1d = 1_int_32
        integer(hsize_t) :: cur_dims(rank), max_dims(rank)
        integer(hid_t) :: fid
        integer(hid_t) :: dspace_id, dset_id
        integer(hid_t) :: memspace
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        real(dp), allocatable :: v1(:), v2(:)
        integer(hsize_t) :: offset(rank)
        integer(hsize_t) :: element_count(rank)
        integer(hsize_t) :: mod_size

        integer(int_32) :: full_chunks
        integer :: idx


        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in RO mode
        call h5fopen_f(trim(filename), H5F_ACC_RDONLY_F, fid, ierr)

        ! Open dataset
        call h5dopen_f(fid, trim(dset_name), dset_id, ierr)
        call h5dget_space_f(dset_id, dspace_id, ierr)

        ! Get current dimensions
        call h5sget_simple_extent_dims_f(dspace_id, cur_dims, max_dims, ierr)

        full_chunks = int(cur_dims(1) / size_chunk, int_32)
        if (full_chunks > 0) then

            allocate(v1(size_chunk))

            ! Create in-memory datasapce. Because it is a vector, it is rank-1
            call h5screate_simple_f(rank_1d, [int(size_chunk, hsize_t)], memspace, ierr)

            element_count = [int(size_chunk, hsize_t), 1_hsize_t]
            do idx=1, full_chunks

                do col_indx=1, diis_space
                    ! Position x v1
                    offset = [int((idx - 1) * size_chunk, hsize_t), int(col_indx - 1, hsize_t)]
                    call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                        offset, &
                        element_count, ierr)

                    call h5dread_f(dset_id, kinds%dp, v1, [int(size_chunk, hsize_t)], ierr, &
                        memspace, dspace_id)

                    call daxpy(size_chunk, C(col_indx), v1, 1, &
                        vector( &
                        ((idx - 1) * size_chunk) + 1: &
                        idx * size_chunk), &
                        1)

                enddo
            enddo

        endif

        ! Get the rest
        mod_size = modulo(cur_dims(1), int(size_chunk, hsize_t))

        if (mod_size /= 0) then

            allocate(v1(mod_size))
            allocate(v2(mod_size))

            ! Create in-memory datasapce. Because it is a vector, it is rank-1
            call h5screate_simple_f(rank_1d, [int(mod_size, hsize_t)], memspace, ierr)

            element_count = [int(mod_size, hsize_t), 1_hsize_t]

            do col_indx=1, diis_space

                ! Position x v1
                offset = [int(full_chunks * size_chunk, hsize_t), int(col_indx - 1, hsize_t)]
                call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                    offset, &
                    element_count, ierr)

                call h5dread_f(dset_id, kinds%dp, v1, [int(size_chunk, hsize_t)], ierr, &
                    memspace, dspace_id)

                v2(1:mod_size) = vector((full_chunks * size_chunk) + 1:(full_chunks * size_chunk) + mod_size)

                call daxpy(mod_size, C(col_indx), v1, 1, v2, 1)
                vector((full_chunks * size_chunk) + 1:(full_chunks * size_chunk) + mod_size) = v2(1:mod_size)
            enddo

            deallocate(v1)
            deallocate(v2)

        endif

        ! Close dataset, dataspace, and file
        call h5sclose_f(dspace_id, ierr)
        call h5sclose_f(memspace, ierr)

        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine get_chunk_daxpy

    subroutine get_chunk_diff_dot_in_mat(filename, dset_name, B, diis_space)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
            h5dget_space_f, h5dread_f, &
            h5sget_simple_extent_dims_f, h5screate_simple_f, &
            h5sselect_hyperslab_f, &
            h5sclose_f, h5dclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDONLY_F, H5S_SELECT_SET_F


        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        real(dp), intent(inout) :: B(:,:)
        integer, intent(in) :: diis_space

        integer(int_32), parameter :: rank = 2_int_32
        integer(int_32), parameter :: rank_1d = 1_int_32
        integer(hsize_t) :: cur_dims(rank), max_dims(rank)
        integer(hid_t) :: fid
        integer(hid_t) :: dspace_id, dset_id
        integer(hid_t) :: memspace
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        integer :: col_indx, col_indy

        real(dp), allocatable :: v1(:), v2(:), v3(:)
        integer(hsize_t) :: offset(rank)
        integer(hsize_t) :: element_count(rank)
        integer(hsize_t) :: mod_size

        integer(int_32) :: full_chunks
        integer :: idx


        real(dp) :: ddot


        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in RO mode
        call h5fopen_f(trim(filename), H5F_ACC_RDONLY_F, fid, ierr)

        ! Open dataset
        call h5dopen_f(fid, trim(dset_name), dset_id, ierr)
        call h5dget_space_f(dset_id, dspace_id, ierr)

        ! Get current dimensions
        call h5sget_simple_extent_dims_f(dspace_id, cur_dims, max_dims, ierr)

        full_chunks = int(cur_dims(1) / size_chunk, int_32)
        if (full_chunks > 0) then

            allocate(v1(size_chunk))
            allocate(v2(size_chunk))
            allocate(v3(size_chunk))

            ! Create in-memory datasapce. Because it is a vector, it is rank-1
            call h5screate_simple_f(rank_1d, [int(size_chunk, hsize_t)], memspace, ierr)

            element_count = [int(size_chunk, hsize_t), 1_hsize_t]
            do idx=1, full_chunks

                do col_indx=1, diis_space
                    ! Position x v1
                    offset = [int((idx - 1) * size_chunk, hsize_t), int(col_indx - 1, hsize_t)]
                    call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                        offset, &
                        element_count, ierr)

                    call h5dread_f(dset_id, kinds%dp, v1, [int(size_chunk, hsize_t)], ierr, &
                        memspace, dspace_id)

                    ! Position x v2
                    offset = [int((idx - 1) * size_chunk, hsize_t), int(col_indx, hsize_t)]
                    call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                        offset, &
                        element_count, ierr)

                    call h5dread_f(dset_id, kinds%dp, v2, [int(size_chunk, hsize_t)], ierr, &
                        memspace, dspace_id)

                    ! Diff in the x position
                    v3 = v2 - v1

                    ! Diagonal part
                    !B(col_indx, col_indx) = B(col_indx, col_indx) + &
                    !    ddot(int(size_chunk, int_64), v3, 1, v3, 1)

                    do col_indy=1, diis_space
                        ! Position y v1
                        offset = [int((idx - 1) * size_chunk, hsize_t), int(col_indy - 1, hsize_t)]
                        call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                            offset, &
                            element_count, ierr)

                        call h5dread_f(dset_id, kinds%dp, v1,  [int(size_chunk, hsize_t)], ierr, &
                            memspace, dspace_id)

                        ! Position y v2
                        offset = [int((idx - 1) * size_chunk, hsize_t), int(col_indy, hsize_t)]
                        call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                            offset, &
                            element_count, ierr)

                        call h5dread_f(dset_id, kinds%dp, v2,  [int(size_chunk, hsize_t)], ierr, &
                            memspace, dspace_id)

                        ! Diff in the y position
                        v2 = v2 - v1


                        ! Off-diagonal parts
                        B(col_indx, col_indy) = B(col_indx, col_indx) + &
                            ddot(int(size_chunk, int_64), v3, 1, v2, 1)

                    enddo

                enddo

            enddo

            deallocate(v1)
            deallocate(v2)
            deallocate(v3)

        endif


        ! Get the rest
        mod_size = modulo(cur_dims(1), int(size_chunk, hsize_t))

        if (mod_size /= 0) then

            allocate(v1(mod_size))
            allocate(v2(mod_size))
            allocate(v3(mod_size))

            ! Create in-memory datasapce. Because it is a vector, it is rank-1
            call h5screate_simple_f(rank_1d, [int(mod_size, hsize_t)], memspace, ierr)

            element_count = [int(mod_size, hsize_t), 1_hsize_t]

            do col_indx=1, diis_space
                ! Position x v1
                offset = [int(full_chunks * size_chunk, hsize_t), int(col_indx - 1, hsize_t)]
                call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                    offset, &
                    element_count, ierr)

                call h5dread_f(dset_id, kinds%dp, v1, [int(mod_size, hsize_t)], ierr, &
                    memspace, dspace_id)

                ! Position x v2
                offset = [int(full_chunks * size_chunk, hsize_t), int(col_indx, hsize_t)]
                call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                    offset, &
                    element_count, ierr)

                call h5dread_f(dset_id, kinds%dp, v2,  [int(mod_size, hsize_t)], ierr, &
                    memspace, dspace_id)

                ! Diff in the x position
                v3 = v2 - v1

                ! Diagonal part
                B(col_indx, col_indx) = B(col_indx, col_indx) + &
                    ddot(mod_size, v3, 1, v3, 1)

                do col_indy=col_indx+1, diis_space
                    ! Position y v1
                    offset = [int(full_chunks * size_chunk, hsize_t), int(col_indy - 1, hsize_t)]
                    call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                        offset, &
                        element_count, ierr)

                    call h5dread_f(dset_id, kinds%dp, v1,  [int(mod_size, hsize_t)], ierr, &
                        memspace, dspace_id)

                    ! Position y v2
                    offset = [int(full_chunks * size_chunk, hsize_t), int(col_indy, hsize_t)]
                    call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                        offset, &
                        element_count, ierr)

                    call h5dread_f(dset_id, kinds%dp, v2,  [int(mod_size, hsize_t)], ierr, &
                        memspace, dspace_id)

                    ! Diff in the y position
                    v2 = v2 - v1

                    ! Off-diagonal parts
                    B(col_indx, col_indy) = B(col_indx, col_indy) + &
                        ddot(int(mod_size, int_64), v3, 1, v2, 1)

                enddo
            enddo


            deallocate(v1)
            deallocate(v2)
            deallocate(v3)

        endif

        ! Close dataset, dataspace, and file
        call h5sclose_f(dspace_id, ierr)
        call h5sclose_f(memspace, ierr)

        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end subroutine get_chunk_diff_dot_in_mat

    function get_chunk_residuum(filename, dset_name, indx1, &
            indx2) result(residuum)

        use const, only: int_32, int_64, dp
        use system, only: run_t

        use hdf5, only: h5open_f, h5fopen_f, h5dopen_f, &
            h5dget_space_f, h5dread_f, &
            h5sget_simple_extent_dims_f, h5screate_simple_f, &
            h5sselect_hyperslab_f, &
            h5sclose_f, h5dclose_f, h5fclose_f, h5close_f, &
            H5F_ACC_RDONLY_F, H5S_SELECT_SET_F


        real(dp) :: residuum
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        integer, intent(in) :: indx1, indx2

        integer(int_32), parameter :: rank = 2_int_32
        integer(int_32), parameter :: rank_1d = 1_int_32
        integer(hsize_t) :: cur_dims(rank), max_dims(rank)
        integer(hid_t) :: fid
        integer(hid_t) :: dspace_id, dset_id
        integer(hid_t) :: memspace
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        real(dp), allocatable :: v1(:), v2(:)
        integer(hsize_t) :: offset(rank)
        integer(hsize_t) :: element_count(rank)
        integer(hsize_t) :: mod_size

        integer(int_32) :: full_chunks
        integer :: idx


        real(dp) :: ddot


        residuum = 0.0_dp

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call h5open_f(ierr)
        call hdf5_kinds_init(kinds)

        ! Open file in RO mode
        call h5fopen_f(trim(filename), H5F_ACC_RDONLY_F, fid, ierr)

        ! Open dataset
        call h5dopen_f(fid, trim(dset_name), dset_id, ierr)
        call h5dget_space_f(dset_id, dspace_id, ierr)

        ! Get current dimensions
        call h5sget_simple_extent_dims_f(dspace_id, cur_dims, max_dims, ierr)

        full_chunks = int(cur_dims(1) / size_chunk, int_32)
        if (full_chunks > 0) then

            allocate(v1(size_chunk))
            allocate(v2(size_chunk))

            ! Create in-memory datasapce. Because it is a vector, it is rank-1
            call h5screate_simple_f(rank_1d, [int(size_chunk, hsize_t)], memspace, ierr)

            element_count = [int(size_chunk, hsize_t), 1_hsize_t]
            do idx=1, full_chunks

                ! Vec 1
                offset = [int((idx - 1) * size_chunk, hsize_t), int(indx1 - 1, hsize_t)]
                call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                    offset, &
                    element_count, ierr)

                call h5dread_f(dset_id, kinds%dp, v1, [int(size_chunk, hsize_t)], ierr, &
                    memspace, dspace_id)


                ! Vec 2
                offset = [int((idx - 1) * size_chunk, hsize_t), int(indx2 -1, hsize_t)]
                call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                    offset, &
                    element_count, ierr)

                call h5dread_f(dset_id, kinds%dp, v2,  [int(size_chunk, hsize_t)], ierr, &
                    memspace, dspace_id)

                ! Diff in the y position
                v2 = v2 - v1

                residuum = residuum + ddot(int(size_chunk, int_64), v2, 1, v2, 1)

            enddo

            deallocate(v1)
            deallocate(v2)

        endif


        ! Get the rest
        mod_size = int(modulo(cur_dims(1), int(size_chunk, hsize_t)), hsize_t)

        if (mod_size /= 0) then

            allocate(v1(mod_size))
            allocate(v2(mod_size))

            ! Create in-memory datasapce. Because it is a vector, it is rank-1
            call h5screate_simple_f(rank_1d, [int(mod_size, hsize_t)], memspace, ierr)

            element_count = [int(mod_size, hsize_t), 1_hsize_t]

            ! Vec 1
            offset = [int(full_chunks * size_chunk, hsize_t), int(indx1 - 1, hsize_t)]
            call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                offset, &
                element_count, ierr)

            call h5dread_f(dset_id, kinds%dp, v1, [int(mod_size, hsize_t)], ierr, &
                memspace, dspace_id)

            ! Vec 2
            offset = [int(full_chunks * size_chunk, hsize_t), int(indx2 - 1, hsize_t)]
            call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
                offset, &
                element_count, ierr)

            call h5dread_f(dset_id, kinds%dp, v2,  [int(mod_size, hsize_t)], ierr, &
                memspace, dspace_id)

            ! Diff in the x position
            v2 = v2 - v1

            residuum = residuum + ddot(int(mod_size, int_64), v2, 1, v2, 1)

            deallocate(v1)
            deallocate(v2)

        endif

        ! Close dataset, dataspace, and file
        call h5sclose_f(dspace_id, ierr)
        call h5sclose_f(memspace, ierr)

        call h5dclose_f(dset_id, ierr)
        call h5fclose_f(fid, ierr)

        call h5close_f(ierr)

    end function get_chunk_residuum

    subroutine write_dataset_1d_real_dp(fid, dset, arr)

        use const, only: int_32, int_64, dp

        use hdf5, only: h5screate_simple_f, h5dcreate_f, &
            h5dwrite_f, h5dclose_f, h5sclose_f

        integer(hid_t), intent(in) :: fid
        character(len=*), intent(in) :: dset
        real(dp), allocatable, intent(in) :: arr(:)

        integer(hid_t) :: dspace_id, dset_id
        integer(int_32) :: ierr
        type(hdf5_kinds_t) :: kinds

        integer(int_32), parameter :: rank = 1_int_32

        ! According to HANDE this has to be called everytime. HDF5 is tricky...
        call hdf5_kinds_init(kinds)

        ! Create rank 1 simple space
        call h5screate_simple_f(rank, shape(arr, kind=int_64), dspace_id, ierr)
        call h5dcreate_f(fid, dset, kinds%dp, dspace_id, dset_id, ierr)


        ! Write dataset
        call h5dwrite_f(dset_id, kinds%dp, arr, shape(arr, kind=int_64), ierr)

        ! Close dataset and dataspace
        call h5dclose_f(dset_id, ierr)
        call h5sclose_f(dspace_id, ierr)

    end subroutine write_dataset_1d_real_dp


    subroutine write_ext_cor_vecs(run, cc)

        use cc_types, only: cc_t
        use system, only: run_t

        type(run_t), intent(in) :: run
        type(cc_t), intent(in) :: cc

        ! Write externally corrected methods to HDF5, just in case
        call init_dset(run%h5_master_file, 'ext_cor_t', [cc%t_size])
        call write_vector(run%h5_master_file, 'ext_cor_t', cc%t_vec, cc%t_size)

        call init_dset(run%h5_master_file, 'ext_cor_vt4a', shape(cc%ext_cor%t2a))
        call write_matrix(run%h5_master_file, 'ext_cor_vt4a', cc%ext_cor%t2a, &
            shape(cc%ext_cor%t2a), size(cc%ext_cor%t2a))

        call init_dset(run%h5_master_file, 'ext_cor_vt4b', shape(cc%ext_cor%t2b))
        call write_matrix(run%h5_master_file, 'ext_cor_vt4b', cc%ext_cor%t2b, &
            shape(cc%ext_cor%t2b), size(cc%ext_cor%t2b))

        call init_dset(run%h5_master_file, 'ext_cor_vt4c', shape(cc%ext_cor%t2c))
        call write_matrix(run%h5_master_file, 'ext_cor_vt4c', cc%ext_cor%t2c, &
            shape(cc%ext_cor%t2c), size(cc%ext_cor%t2c))

    end subroutine write_ext_cor_vecs

#else

    ! ----- Dummy stubs -----

    subroutine init_dset(filename, dset_name, dims)

        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dset_name
        integer, intent(in) :: dims(:)

        character(len=200) :: dummy_filename, dummy_dset_name
        integer :: i

        dummy_filename = filename
        dummy_dset_name = dset_name
        i = dims(1)


    end subroutine init_dset

    subroutine init_h5_file(filename)

        character(len=*), intent(in) :: filename

        character(len=200) :: dummy_filename

        dummy_filename = filename

    end subroutine init_h5_file

    subroutine write_ext_cor_vecs(run, cc)

        use cc_types, only: cc_t
        use system, only: run_t

        type(run_t), intent(in) :: run
        type(cc_t), intent(in) :: cc

        integer :: dummy_diis_space, dummy_t_size

        dummy_diis_space = run%diis_space
        dummy_t_size = cc%t_size

    end subroutine write_ext_cor_vecs

    subroutine read_popsfile_h5(filename, dets, coefs, list_size)

        use const, only: i0, dp

        use errors, only: stop_all

        character(len=*), intent(in) :: filename
        integer(i0), allocatable, intent(in out) :: dets(:,:)
        real(dp), allocatable, intent(in out) :: coefs(:,:)
        integer, intent(out) :: list_size

        call stop_all('read_popsfile_h5', 'ERROR: compilation has no HDF5 support')

    end subroutine read_popsfile_h5

#endif

end module hdf5_io
