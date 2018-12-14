module utils

    implicit none

    contains

        subroutine count_t_spaces(froz, occ_a, occ_b, orbs, actocc, actunocc, &
                t_trunc, t_order, t_pos, t_size)

            integer, intent(in) :: froz, occ_a, occ_b, orbs
            integer, intent(in) :: actocc, actunocc
            integer, intent(in) :: t_trunc
            integer, intent(inout) :: t_order
            integer, allocatable, intent(inout) :: t_pos(:)
            integer, intent(inout) :: t_size

            integer :: nocc_a, nocc_b, nunocc_a, nunocc_b
            integer :: actocc_a, actocc_b, actunocc_a, actunocc_b
            integer :: non_actocc, non_actunocc

            nocc_a = occ_a - froz
            nocc_b = occ_b - froz
            nunocc_a = orbs - occ_a
            nunocc_b = orbs - occ_b
            non_actocc = actocc - froz
            non_actunocc = orbs - actunocc
            actocc_a = occ_a - actocc
            actocc_b = occ_b - actocc
            actunocc_a = actunocc - occ_a
            actunocc_b = actunocc - occ_b

            t_order = 16
            allocate(t_pos(t_order))
            ! T1 alpha size
            ! K1A
            t_pos(1) = 1

            ! T1 beta size
            ! K1B
            t_pos(2)=t_pos(1)+nocc_a*nunocc_a

            ! T2 alpha-alpha size
            ! K2A
            t_pos(3)=t_pos(2)+nocc_b*nunocc_b

            ! T2 alpha-beta size
            ! K2B
            t_pos(4)=t_pos(3)+nocc_a*nocc_a*nunocc_a*nunocc_a

            ! T2 beta-beta size
            ! K2C
            t_pos(5)=t_pos(4)+nocc_a*nocc_b*nunocc_a*nunocc_b

            ! T3 sizes
            ! K3A
            t_pos(6)=t_pos(5)+nocc_b*nocc_b*nunocc_b*nunocc_b
            ! K3B1
            t_pos(7)=t_pos(6)+nunocc_a*nunocc_a*actunocc_a*nocc_a*nocc_a*actocc_a  !1**1**
            ! K3B2
            t_pos(8)=t_pos(7)+nunocc_b*nunocc_a*actunocc_a*nocc_b*nocc_a*actocc_a  !1**1**
            ! K3B3
            t_pos(9)=t_pos(8)+actunocc_b*non_actunocc*non_actunocc*actocc_b*non_actocc*non_actocc  !001001
            ! K3B4
            t_pos(10)=t_pos(9)+actunocc_b*non_actunocc*non_actunocc*nocc_b*nocc_a*actocc_a  !1**001
            ! K3C1
            t_pos(11)=t_pos(10)+nunocc_b*nunocc_a*actunocc_a*actocc_b*non_actocc*non_actocc  !0011**
            ! K3C2
            t_pos(12)=t_pos(11)+nunocc_b*actunocc_b*nunocc_a*nocc_b*actocc_b*nocc_a  !*1**1*
            ! K3C3
            t_pos(13)=t_pos(12)+non_actunocc*non_actunocc*actunocc_a*non_actocc*non_actocc*actocc_a  !100100
            ! K3C4
            t_pos(14)=t_pos(13)+non_actunocc*non_actunocc*actunocc_a*nocc_b*actocc_b*nocc_a  !*1*100
            ! K3D
            t_pos(15)=t_pos(14)+nunocc_b*actunocc_b*nunocc_a*non_actocc*non_actocc*actocc_a  !100*1*
            ! End
            t_pos(16)=t_pos(15)+nunocc_b*nunocc_b*actunocc_b*nocc_b*nocc_b*actocc_b  !1**1**

            t_size = t_pos(t_order) - 1

            t_order = t_order - 1

        end subroutine count_t_spaces

        function residuum(iter, diis_space, t_size)

            use const, only: dp, t_vecs_unit

            integer, intent(in) :: iter
            integer, intent(in) :: diis_space
            integer, intent(in) :: t_size

            real(dp) :: residuum
            real(dp), allocatable :: t_aux(:,:)
            real(dp) :: ddot
            integer :: i, indx_rec


            allocate(t_aux(t_size, 2))

            do i=0, 1

                indx_rec = mod(iter - i, diis_space + 1)
                if (indx_rec == 0) indx_rec = diis_space + 1

                if (iter - i == 0) then
                    t_aux(:,i+1) = 0
                else
                    read(t_vecs_unit, rec=indx_rec) t_aux(:,i+1)
                endif

            enddo

            t_aux(:,1) = t_aux(:,2) - t_aux(:,1)
            residuum = ddot(t_size, t_aux(:,1), 1, t_aux(:,1), 1)

            deallocate(t_aux)

            residuum = dsqrt(residuum)

        end function residuum

        subroutine antisymmetrize_p_space(froz, occ_a, occ_b, orbs, p_space)

            integer, intent(in) :: froz, occ_a, occ_b, orbs
            integer, intent(inout) :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

            integer :: a, b, c, i, j, k

            do i=froz+1, occ_a-2
                do j=i+1, occ_a-1
                    do k=j+1, occ_a
                        do a=occ_a+1, orbs-2
                            do b=a+1, orbs-1
                                do c=b+1, orbs
                                    print '(6i3)', c,b,a,k,j,i
                                    p_space(c,b,a,k,i,j) = p_space(c,b,a,k,j,i)
                                    p_space(c,b,a,i,j,k) = p_space(c,b,a,k,j,i)
                                    p_space(c,b,a,i,k,j) = p_space(c,b,a,k,j,i)
                                    p_space(c,b,a,j,k,i) = p_space(c,b,a,k,j,i)
                                    p_space(c,b,a,j,i,k) = p_space(c,b,a,k,j,i)
                                    p_space(c,a,b,k,j,i) = p_space(c,b,a,k,j,i)
                                    p_space(c,a,b,k,i,j) = p_space(c,b,a,k,j,i)
                                    p_space(c,a,b,i,j,k) = p_space(c,b,a,k,j,i)
                                    p_space(c,a,b,i,k,j) = p_space(c,b,a,k,j,i)
                                    p_space(c,a,b,j,k,i) = p_space(c,b,a,k,j,i)
                                    p_space(c,a,b,j,i,k) = p_space(c,b,a,k,j,i)
                                    p_space(a,b,c,k,j,i) = p_space(c,b,a,k,j,i)
                                    p_space(a,b,c,k,i,j) = p_space(c,b,a,k,j,i)
                                    p_space(a,b,c,i,j,k) = p_space(c,b,a,k,j,i)
                                    p_space(a,b,c,i,k,j) = p_space(c,b,a,k,j,i)
                                    p_space(a,b,c,j,k,i) = p_space(c,b,a,k,j,i)
                                    p_space(a,b,c,j,i,k) = p_space(c,b,a,k,j,i)
                                    p_space(a,c,b,k,j,i) = p_space(c,b,a,k,j,i)
                                    p_space(a,c,b,k,i,j) = p_space(c,b,a,k,j,i)
                                    p_space(a,c,b,i,j,k) = p_space(c,b,a,k,j,i)
                                    p_space(a,c,b,i,k,j) = p_space(c,b,a,k,j,i)
                                    p_space(a,c,b,j,k,i) = p_space(c,b,a,k,j,i)
                                    p_space(a,c,b,j,i,k) = p_space(c,b,a,k,j,i)
                                    p_space(b,c,a,k,j,i) = p_space(c,b,a,k,j,i)
                                    p_space(b,c,a,k,i,j) = p_space(c,b,a,k,j,i)
                                    p_space(b,c,a,i,j,k) = p_space(c,b,a,k,j,i)
                                    p_space(b,c,a,i,k,j) = p_space(c,b,a,k,j,i)
                                    p_space(b,c,a,j,k,i) = p_space(c,b,a,k,j,i)
                                    p_space(b,c,a,j,i,k) = p_space(c,b,a,k,j,i)
                                    p_space(b,a,c,k,j,i) = p_space(c,b,a,k,j,i)
                                    p_space(b,a,c,k,i,j) = p_space(c,b,a,k,j,i)
                                    p_space(b,a,c,i,j,k) = p_space(c,b,a,k,j,i)
                                    p_space(b,a,c,i,k,j) = p_space(c,b,a,k,j,i)
                                    p_space(b,a,c,j,k,i) = p_space(c,b,a,k,j,i)
                                    p_space(b,a,c,j,i,k) = p_space(c,b,a,k,j,i)
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

        end subroutine antisymmetrize_p_space

    end module utils
