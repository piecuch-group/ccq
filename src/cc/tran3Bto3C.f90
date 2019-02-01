subroutine tran3bto3c(n0,n1,n2,n3,t3b,t3c)

    use const, only: p


    implicit none

    integer, intent(in) :: n0, n1, n2, n3

    real(p), intent(in) :: t3b(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1)
    real(p), intent(inout) :: t3c(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1)
    integer :: i,j,k,a,b,c

    do i=n0+1,n1
        do j=n0+1,n2-1
            do k=j+1,n2
                do a=n1+1,n3
                    do b=n2+1,n3-1
                        do c=b+1,n3
                            t3c(c,b,a,k,j,i)= t3b(a,c,b,i,k,j)
                            t3c(c,b,a,j,k,i)=-t3c(c,b,a,k,j,i)
                            t3c(b,c,a,k,j,i)=-t3c(c,b,a,k,j,i)
                            t3c(b,c,a,j,k,i)= t3c(c,b,a,k,j,i)
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo

end subroutine tran3bto3c
