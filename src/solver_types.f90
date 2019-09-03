module solver_types

    ! Module defining the solver types. This is
    ! used as an abstraction for the various methods

    use const, only: p, c_len

    type conv_t

        ! Filename for disk storing. Usually HDF5 master file
        character(len=c_len) :: filename

        ! Name of the dataset for the main vector
        character(len=c_len) :: vec_dset_name

        ! Name of the dataset for the DIIS iteration history
        character(len=c_len) :: iter_dset_name

        real(p) :: en_cor ! Correlation energy
        logical :: failed ! Status of the calculation
        real(p) :: res ! Residuum
        real(p) :: conv(3) = 1.0_p ! Convergence status history

        ! [TODO] Vector units. These might need to be deprecated
        integer :: vec_unit
        integer :: vecs_unit

        ! Pointer to the target vector and the vector size
        integer :: vec_size
        real(p), pointer :: vec_ptr(:) => null()
    end type conv_t

end module solver_types