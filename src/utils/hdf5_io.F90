module hdf5_io

#ifdef ENABLE_HDF5

    use hdf5, only: hid_t

    implicit none

    ! HDF5 kinds equivalent to the kinds defined in const.  Set in
    ! hdf5_kinds_init.
    type hdf5_kinds_t
        integer(hid_t) :: i32
        integer(hid_t) :: i64
        integer(hid_t) :: i0
        integer(hid_t) :: sp
        integer(hid_t) :: dp
    end type hdf5_kinds_t

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

        subroutine write_calc_data(sys, run, cc)

            use const, only: int_32, int_64
            use system, only: sys_t, run_t
            use cc_types, only: cc_t

            use hdf5, only: h5open_f, h5fcreate_f, h5gcreate_f, &
                h5gclose_f, h5fclose_f, h5close_f, &
                H5F_ACC_TRUNC_F

            type(sys_t), intent(in) :: sys
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
#else

        ! Use this if no HDF5 is set

        subroutine write_calc_data(sys, run, cc)

            use const, only: t_unit, l_unit
            use system, only: sys_t, run_t
            use cc_types, only: cc_t

            type(sys_t), intent(in) :: sys
            type(run_t), intent(in) :: run
            type(cc_t), intent(in) :: cc


            ! Open files
            open(t_unit,file="t_vec_"//trim(run%uuid)//".bin",form='unformatted')
            write(t_unit) cc%t_vec
            close(t_unit)

            if (run%lcc) then
                open(l_unit,file="l_vec_"//trim(run%uuid)//".bin",form='unformatted')
                write(l_unit) cc%l_vec
                close(l_unit)
            endif

        end subroutine write_calc_data

#endif

end module hdf5_io
