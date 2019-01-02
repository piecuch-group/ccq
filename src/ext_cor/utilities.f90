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

        subroutine t3_aab_to_t3_abb(sys, t3_aab, t3_abb)

            ! Proper transfer of t3_aab to t3_abb for closed-shell systems
            use const, only: dp
            use system, only: sys_t

            ! Indices
            type(sys_t), intent(in) :: sys
            integer :: i, j, k
            integer :: a, b, c

            ! t3_aab and t3_abb amplitudes
            real(dp), allocatable, intent(in) :: t3_aab(:,:,:,:,:,:)
            real(dp), allocatable, intent(inout) :: t3_abb(:,:,:,:,:,:)

            associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

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
            end associate

        end subroutine t3_aab_to_t3_abb

        subroutine t4_aaab_to_t4_abbb(sys, t4_aaab, t4_abbb)

            ! Proper transfer of t4_aaab to t4_abbb for closed-shell systems

            use const, only: dp
            use system, only: sys_t

            ! Indices
            type(sys_t), intent(in) :: sys
            integer :: i, j, k, l
            integer :: a, b, c, d

            ! t4_aaab and t4_abbb amplitudes
            real(dp), allocatable, intent(in) :: t4_aaab(:,:,:,:,:,:,:,:)
            real(dp), allocatable, intent(inout) :: t4_abbb(:,:,:,:,:,:,:,:)

            associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

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
            end associate

        end subroutine t4_aaab_to_t4_abbb

        subroutine antisymmetrize(sys, c_vec)

            ! Antisymmetrize arrays
            !
            ! In:
            !   sys: system information
            ! In/Out:
            !   c_vec: CI or CC coefficient/amplitude arrays. On entry, arrays are not antisymmetrized
            !          on output arrays are antisymmetrized

            use ext_cor_types, only: vec3_t
            use system, only: sys_t

            type(sys_t), intent(in) :: sys
            type(vec3_t), intent(inout) :: c_vec

            integer :: a, b, c
            integer :: i, j, k

            associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

                do i=froz+1, occ_a
                    do j=i+1, occ_a
                        do a=occ_a+1, total
                            do b=a+1, total
                                c_vec%o2_aa(a,b,j,i)=-c_vec%o2_aa(a,b,i,j) !(ij)
                                c_vec%o2_aa(b,a,i,j)=-c_vec%o2_aa(a,b,i,j) !(ab)
                                c_vec%o2_aa(b,a,j,i)=c_vec%o2_aa(a,b,i,j) !(ab)(ij)
                            enddo
                        enddo
                    enddo
                enddo

                do i=froz+1, occ_b
                    do j=i+1, occ_b
                        do a=occ_b+1, total
                            do b=a+1, total
                                c_vec%o2_bb(a,b,j,i)=-c_vec%o2_bb(a,b,i,j) !(ij)
                                c_vec%o2_bb(b,a,i,j)=-c_vec%o2_bb(a,b,i,j) !(ab)
                                c_vec%o2_bb(b,a,j,i)=c_vec%o2_bb(a,b,i,j) !(ab)(ij)
                            enddo
                        enddo
                    enddo
                enddo

                do i=froz+1, occ_a
                    do j=i+1, occ_a
                        do k=j+1, occ_a
                            do a=occ_a+1, total
                                do b=a+1, total
                                    do c=b+1, total
                                        c_vec%o3_aaa(a,b,c,j,i,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ij)
                                        c_vec%o3_aaa(a,b,c,k,j,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ik)
                                        c_vec%o3_aaa(a,b,c,i,k,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(jk)
                                        c_vec%o3_aaa(a,b,c,j,k,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(ijk)
                                        c_vec%o3_aaa(a,b,c,k,i,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(ikj)
                                        c_vec%o3_aaa(b,a,c,i,j,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ab)
                                        c_vec%o3_aaa(b,a,c,j,i,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ij)
                                        c_vec%o3_aaa(b,a,c,k,j,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ik)
                                        c_vec%o3_aaa(b,a,c,i,k,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(jk)
                                        c_vec%o3_aaa(b,a,c,j,k,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ijk)
                                        c_vec%o3_aaa(b,a,c,k,i,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ab)(ikj)
                                        c_vec%o3_aaa(c,b,a,i,j,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ac)
                                        c_vec%o3_aaa(c,b,a,j,i,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ij)
                                        c_vec%o3_aaa(c,b,a,k,j,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ik)
                                        c_vec%o3_aaa(c,b,a,i,k,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(jk)
                                        c_vec%o3_aaa(c,b,a,j,k,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ijk)
                                        c_vec%o3_aaa(c,b,a,k,i,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(ac)(ikj)
                                        c_vec%o3_aaa(a,c,b,i,j,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(bc)
                                        c_vec%o3_aaa(a,c,b,j,i,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ij)
                                        c_vec%o3_aaa(a,c,b,k,j,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ik)
                                        c_vec%o3_aaa(a,c,b,i,k,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(jk)
                                        c_vec%o3_aaa(a,c,b,j,k,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ijk)
                                        c_vec%o3_aaa(a,c,b,k,i,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(bc)(ikj)
                                        c_vec%o3_aaa(b,c,a,i,j,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(abc)
                                        c_vec%o3_aaa(b,c,a,j,i,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ij)
                                        c_vec%o3_aaa(b,c,a,k,j,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ik)
                                        c_vec%o3_aaa(b,c,a,i,k,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(jk)
                                        c_vec%o3_aaa(b,c,a,j,k,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ijk)
                                        c_vec%o3_aaa(b,c,a,k,i,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(abc)(ikj)
                                        c_vec%o3_aaa(c,a,b,i,j,k)=c_vec%o3_aaa(a,b,c,i,j,k) !(acb)
                                        c_vec%o3_aaa(c,a,b,j,i,k)=-c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ij)
                                        c_vec%o3_aaa(c,a,b,k,j,i)=-c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ik)
                                        c_vec%o3_aaa(c,a,b,i,k,j)=-c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(jk)
                                        c_vec%o3_aaa(c,a,b,j,k,i)=c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ijk)
                                        c_vec%o3_aaa(c,a,b,k,i,j)=c_vec%o3_aaa(a,b,c,i,j,k) !(acb)(ikj)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

                do i=froz+1, occ_a
                    do j=i+1, occ_a
                        do k=froz+1, occ_b
                            do a=occ_a+1, total
                                do b=a+1, total
                                    do c=occ_b+1, total
                                        c_vec%o3_aab(a,b,c,j,i,k)=-c_vec%o3_aab(a,b,c,i,j,k) !(ij)
                                        c_vec%o3_aab(b,a,c,i,j,k)=-c_vec%o3_aab(a,b,c,i,j,k) !(ab)
                                        c_vec%o3_aab(b,a,c,j,i,k)=c_vec%o3_aab(a,b,c,i,j,k) !(ab)(ij)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

                do i=froz+1, occ_a
                    do j=froz+1, occ_b
                        do k=j+1, occ_b
                            do a=occ_a+1, total
                                do b=occ_b+1, total
                                    do c=b+1, total
                                        c_vec%o3_abb(a,b,c,i,k,j)=-c_vec%o3_abb(a,b,c,i,j,k) !(jk)
                                        c_vec%o3_abb(a,c,b,i,j,k)=-c_vec%o3_abb(a,b,c,i,j,k) !(bc)
                                        c_vec%o3_abb(a,c,b,i,k,j)=c_vec%o3_abb(a,b,c,i,j,k) !(bc)(jk)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

                do i=froz+1, occ_b
                    do j=i+1, occ_b
                        do k=j+1, occ_b
                            do a=occ_b+1, total
                                do b=a+1, total
                                    do c=b+1, total
                                        c_vec%o3_bbb(a,b,c,j,i,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ij)
                                        c_vec%o3_bbb(a,b,c,k,j,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ik)
                                        c_vec%o3_bbb(a,b,c,i,k,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(jk)
                                        c_vec%o3_bbb(a,b,c,j,k,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(ijk)
                                        c_vec%o3_bbb(a,b,c,k,i,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(ikj)
                                        c_vec%o3_bbb(b,a,c,i,j,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ab)
                                        c_vec%o3_bbb(b,a,c,j,i,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ij)
                                        c_vec%o3_bbb(b,a,c,k,j,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ik)
                                        c_vec%o3_bbb(b,a,c,i,k,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(jk)
                                        c_vec%o3_bbb(b,a,c,j,k,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ijk)
                                        c_vec%o3_bbb(b,a,c,k,i,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ab)(ikj)
                                        c_vec%o3_bbb(c,b,a,i,j,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ac)
                                        c_vec%o3_bbb(c,b,a,j,i,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ij)
                                        c_vec%o3_bbb(c,b,a,k,j,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ik)
                                        c_vec%o3_bbb(c,b,a,i,k,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(jk)
                                        c_vec%o3_bbb(c,b,a,j,k,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ijk)
                                        c_vec%o3_bbb(c,b,a,k,i,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(ac)(ikj)
                                        c_vec%o3_bbb(a,c,b,i,j,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(bc)
                                        c_vec%o3_bbb(a,c,b,j,i,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ij)
                                        c_vec%o3_bbb(a,c,b,k,j,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ik)
                                        c_vec%o3_bbb(a,c,b,i,k,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(jk)
                                        c_vec%o3_bbb(a,c,b,j,k,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ijk)
                                        c_vec%o3_bbb(a,c,b,k,i,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(bc)(ikj)
                                        c_vec%o3_bbb(b,c,a,i,j,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(abc)
                                        c_vec%o3_bbb(b,c,a,j,i,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ij)
                                        c_vec%o3_bbb(b,c,a,k,j,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ik)
                                        c_vec%o3_bbb(b,c,a,i,k,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(jk)
                                        c_vec%o3_bbb(b,c,a,j,k,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ijk)
                                        c_vec%o3_bbb(b,c,a,k,i,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(abc)(ikj)
                                        c_vec%o3_bbb(c,a,b,i,j,k)=c_vec%o3_bbb(a,b,c,i,j,k) !(acb)
                                        c_vec%o3_bbb(c,a,b,j,i,k)=-c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ij)
                                        c_vec%o3_bbb(c,a,b,k,j,i)=-c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ik)
                                        c_vec%o3_bbb(c,a,b,i,k,j)=-c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(jk)
                                        c_vec%o3_bbb(c,a,b,j,k,i)=c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ijk)
                                        c_vec%o3_bbb(c,a,b,k,i,j)=c_vec%o3_bbb(a,b,c,i,j,k) !(acb)(ikj)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

            end associate

        end subroutine antisymmetrize

    end module utilities
