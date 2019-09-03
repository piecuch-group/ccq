subroutine egemm(k1,k2,k3,A,B,C)

    ! BLAS DGEMM wrapper.

    ! In:
    !   k1, k2, k3: matrix dimensions. k3 is the dimension
    !               being contracted
    !   A, B: input matrices

    ! In/Out:
    !   C: dgemm result

    ! [TODO] This has to become a module.
    ! Also, add  compatibility with sp?
    use const, only: dp, int_64

    implicit none

    integer :: k1,k2,k3
    integer(int_64) :: k1_loc,k2_loc,k3_loc
    real(dp) :: A(K3,K1),B(K3,K2),C(K2,K1)

    k1_loc = int(k1, kind=int_64)
    k2_loc = int(k2, kind=int_64)
    k3_loc = int(k3, kind=int_64)

    call dgemm('t', 'n', k2_loc, k1_loc, k3_loc, 1.0_dp, b, k3_loc, a, k3_loc, 0.0_dp, c, k2_loc)

end subroutine egemm

subroutine egemm1(k1,k3,A,B,C)

    ! BLAS DGEMV wrappeer.

    ! In:
    !   k1, k3: matrix dimensions. k3 is the dimension
    !               being contracted
    !   A: input matrix
    !   B: input vector

    ! In/Out:
    !   C: dgemv result.

    use const, only: dp, int_64

    implicit none

    integer :: k1,k3
    integer(int_64) :: k1_loc,k3_loc
    real(dp) :: A(K3,K1),B(K3),C(K1)

    k1_loc = int(k1, kind=int_64)
    k3_loc = int(k3, kind=int_64)

    call dgemv('t', k3_loc, k1_loc, 1.0_dp, a, k3_loc, b, 1, 0.0_dp, c, 1)

end subroutine egemm1

subroutine egemm2(K2,K3,A,B,C)

    use const, only: dp, int_64

    implicit none

    integer :: k2,k3
    integer(int_64) :: k2_loc,k3_loc
    real(dp) :: A(K3),B(K3,K2),C(K2)

    k2_loc = int(k2, kind=int_64)
    k3_loc = int(k3, kind=int_64)

    call dgemv('t', k3_loc, k2_loc, 1.0_dp, b, k3_loc, a, 1, 0.0_dp, c, 1)

end subroutine egemm2