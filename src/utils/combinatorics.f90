module combinatorics

    implicit none

contains

    subroutine combinations(items, r, combs)

        integer, intent(in) :: items(:)
        integer, intent(in) :: r
        integer, allocatable, intent(out) :: combs(:,:)

        integer, allocatable :: inds(:)
        integer :: n
        integer :: i, j, k, indx
        integer :: n_combs

        n = size(items)

        n_combs = ncr(n, r)

        allocate(combs(r, n_combs))

        allocate(inds(n))

        do i=1, r
            inds(i) = i
        enddo

        !write(*,*) (inds(k), k=1, r)
        indx = 1
        do k=1, r
            combs(k,indx) = items(inds(k))
        enddo

        outer: do
            do i=r, 1, -1
                if (inds(i) /= i + n - r) exit
                if (i == 1) exit outer
            enddo
            inds(i) = inds(i) + 1
            do j=i+1, r
                inds(j) = inds(j-1) + 1
            enddo

            !write(*,*) (inds(k), k=1, r)
            indx = indx + 1
            do k=1, r
                combs(k,indx) = items(inds(k))
            enddo
        enddo outer

        deallocate(inds)

    end subroutine combinations


    function ncr(n, r) result(res)
        integer, intent(in) :: n, r
        integer :: res

        integer :: i
        integer :: accu

        accu = 1
        do i=(n-r)+1, n
            accu = accu * i
        enddo
        res = accu

        accu = 1
        do i=1, r
            accu = accu * i
        enddo
        res = res / accu

    end function ncr


    function factorial(n) result(res)

        integer, intent(in) :: n
        integer :: i, res

        res = 1
        do i=2, n
            res = res * i
        enddo

    end function factorial

end module combinatorics
