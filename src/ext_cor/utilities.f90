module utilities

    ! Module containing extra auxiliar functions

    implicit none

    contains
        function binom_coeff(n, r) result(c)

            ! Compute a binomial coefficient
            ! [TODO] there might be a standard function for this.

            integer, intent(in) :: n, r

            integer(kind=8) :: n_8, r_8

            integer :: c

            if (r > n) then
                c = 0
                return
            endif

            n_8 = int(n, kind=8)
            r_8 = int(r, kind=8)

            if (r > (n - r)) then
                c = int(factorial(n_8 , r_8),kind=4) / int(factorial(n_8 - r_8, int(1,kind=8)), kind=4)
            else
                c = int(factorial(n_8, (n_8 - r_8)),kind=4) / int(factorial(r_8, int(1,kind=8)),kind=4)
            endif

            !c = factorial(n_8) / (factorial(r_8)*factorial(n_8-r_8))

        end function

        recursive function factorial(x, lvl) result(fact)

            ! Compute x! recursively
            !
            ! In:
            !   x: input number
            !   lvl: if lvl = 1, a normal factorial is computed. Other wise the product
            !       equals x * (x-1) * (x-2) * ... * lvl + 1
            ! Out:
            !   fact: x!

            integer(kind=8), intent(in) :: x
            integer(kind=8), intent(in) :: lvl

            integer(kind=8) :: fact

            if (x == lvl) then
                fact = 1
            else
                fact = x * factorial(x - 1, lvl)
            endif

        end function factorial

        subroutine t3_aab_to_t3_abb(t3_aab, t3_abb)

            ! Proper transfer of t3_aab to t3_abb for closed-shell systems

            use sys_data

            ! Indices
            integer :: i, j, k
            integer :: a, b, c

            ! t3_aab and t3_abb amplitudes
            real(kind=8), allocatable, intent(in) :: t3_aab(:,:,:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: t3_abb(:,:,:,:,:,:)

            ! Transfer t3_aab values to t3_abb
            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do k=j+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_b+1,total
                                do c=b+1,total

                                    t3_abb(a,b,c,i,j,k) = t3_aab(b,c,a,j,k,i)

                               enddo
                            enddo
                       enddo
                   enddo
               enddo
           enddo

        end subroutine t3_aab_to_t3_abb

        subroutine t4_aaab_to_t4_abbb(t4_aaab, t4_abbb)

            ! Proper transfer of t4_aaab to t4_abbb for closed-shell systems

            use sys_data

            ! Indices
            integer :: i, j, k, l
            integer :: a, b, c, d

            ! t4_aaab and t4_abbb amplitudes
            real(kind=8), allocatable, intent(in) :: t4_aaab(:,:,:,:,:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: t4_abbb(:,:,:,:,:,:,:,:)

            ! Transfer t4_aaab values to t4_abbb
            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do k=j+1,occ_b
                        do l=k+1,occ_b
                            do a=occ_a+1,total
                                do b=occ_b+1,total
                                    do c=b+1,total
                                        do d=c+1,total

                                            t4_abbb(a,b,c,d,i,j,k,l) = t4_aaab(b,c,d,a,j,k,l,i)

                                       enddo
                                    enddo
                               enddo
                           enddo
                       enddo
                   enddo
               enddo
           enddo

        end subroutine t4_aaab_to_t4_abbb

end module utilities
