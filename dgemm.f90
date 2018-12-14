subroutine egemm(K1,K2,K3,A,B,C)
    use const, only: dp

    implicit none

    integer :: K1,K2,K3
    real(dp) :: A(K3,K1),B(K3,K2),C(K2,K1)

    call dgemm('t', 'n', K2, K1, K3, 1.0_dp, B, K3, A, K3, 0.0_dp, C, K2)

end subroutine egemm

subroutine egemm1(K1,K3,A,B,C)
    use const, only: dp

    implicit none

    integer :: K1,K3
    real(dp) :: A(K3,K1),B(K3),C(K1)

    call dgemv('t', K3, K1, 1.0_dp, A, K3, B, 1, 0.0_dp, C, 1)

end subroutine egemm1

subroutine egemm2(K2,K3,A,B,C)
    use const, only: dp

    implicit none

    integer :: K2,K3
    real(dp) :: A(K3),B(K3,K2),C(K2)

    call dgemv('t', K3, K2, 1.0_dp, B, K3, A, 1, 0.0_dp, C, 1)

end subroutine egemm2
