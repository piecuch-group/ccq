module solver_types

    use const, only: p

    type conv_t
        real(p) :: en_cor
        logical :: failed
        real(p) :: res
        real(p) :: conv(3) = 1.0_p

        integer :: vec_unit
        integer :: vecs_unit

        integer :: vec_size
        real(p), pointer :: vec_ptr(:) => null()
    end type conv_t

end module solver_types
