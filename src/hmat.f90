module hmat

    ! Module for calculating matrix elements of the Hamiltonian and the
    ! similarity transformed Hamiltonian in the Slater determinant space.

    implicit none

contains

    function get_z(z_a, z_b, i, a) result(z_int)

        ! Get one-body matrix element <i|z|a>

        ! In:
        !    ints: system's integrals
        !    i: ith spin-orbital
        !    a: ath spin-orbital
        ! Out:
        !    z_int: one-body operator matrix element

        use const, only: p

        real(p) :: z_int
        real(p), allocatable, intent(in) :: z_a(:,:), z_b(:,:)
        integer, intent(in) :: i, a

        ! Spatial orbitals
        integer :: i_sp, a_sp

        i_sp = int((i + 1) / 2)
        a_sp = int((a + 1) / 2)

        ! Choose spin case
        if (mod(i, 2) == 0) then
            z_int = z_b(a_sp, i_sp)
        else
            z_int = z_a(a_sp, i_sp)
        endif

    end function get_z


    function get_v(v_aa, v_ab, v_bb, i, j, a, b) result(v_int)

        ! Get two-body matrix element <ij|v|ab>

        ! In:
        !    ints: system's integrals
        !    i: ith spin-orbital
        !    j: jth spin-orbital
        !    a: ath spin-orbital
        !    b: bth spin-orbital
        ! Out:
        !    v_int: two-body operator matrix element

        use const, only: p
        use system, only: ints_t

        real(p) :: v_int
        real(p), allocatable, intent(in) :: v_aa(:,:,:,:), v_ab(:,:,:,:), v_bb(:,:,:,:)
        integer, intent(in) :: i, j, a, b
        integer :: dod
        integer :: i_sp, j_sp, a_sp, b_sp

        ! Spatial orbitals
        i_sp = int((i + 1) / 2)
        j_sp = int((j + 1) / 2)
        a_sp = int((a + 1) / 2)
        b_sp = int((b + 1) / 2)

        ! Total spin
        dod = mod(i,2) + mod(j,2) + mod(a,2) + mod(b,2)

        ! All spins are the same
        if (dod == 4) then
            v_int = v_aa(b_sp, a_sp, j_sp, i_sp)

        else if (dod == 0) then
            v_int = v_bb(b_sp, a_sp, j_sp, i_sp)

        else if (dod == 1 .or. dod == 3) then
            v_int = 0.0_p

        else if (mod(i, 2) == mod(j, 2)) then
            ! Two spins from spins are the same
            v_int = 0.0_p

        else if (mod(i, 2) == mod(a, 2)) then
            ! Bra and ket indices match spin
            v_int = v_ab(b_sp, a_sp, j_sp, i_sp)

        else if (mod(i, 2) == mod(b, 2)) then
            ! Bra and ket indices are flipped
            v_int = -v_ab(a_sp, b_sp, j_sp, i_sp)
        endif

    end function get_v

    ! [TMPDEBUG]
    function get_g(g_aaa, g_aab, g_abb, g_bbb, i, j, k, a, b, c) result(g_int)
    !function get_g(g_aaa, g_aab, g_abb, g_bbb, k, i, j, a, c, b) result(g_int)

        ! Get three-body matrix element <ijk|v|abc>

        ! In:
        !    ints: system's integrals
        !    i: ith spin-orbital
        !    j: jth spin-orbital
        !    k: jth spin-orbital
        !    a: ath spin-orbital
        !    b: bth spin-orbital
        !    c: bth spin-orbital

        ! Out:
        !    g_int: three-body operator matrix element

        use const, only: p

        real(p) :: g_int
        real(p), allocatable, intent(in) :: g_aaa(:,:,:,:,:,:), g_aab(:,:,:,:,:,:), &
            g_abb(:,:,:,:,:,:), g_bbb(:,:,:,:,:,:)
        integer, intent(in) :: i, j, k, a, b, c

        integer :: i_sp, j_sp, k_sp, a_sp, b_sp, c_sp
        integer :: inds(6)
        integer :: dod

        ! Spatial orbitals
        i_sp = int((i + 1) / 2)
        j_sp = int((j + 1) / 2)
        k_sp = int((k + 1) / 2)
        a_sp = int((a + 1) / 2)
        b_sp = int((b + 1) / 2)
        c_sp = int((c + 1) / 2)

        dod = mod(i, 2) + mod(j, 2) + mod(k, 2) + mod(a, 2) + mod(b, 2) + mod(c, 2)
        !print '(6i4)', i,j,k,a,b,c
        !print '(a,i4)', 'dod', dod
        ! [TODO] optimize this
        if (dod == 0) then
            !g_int = g_bbb(i_sp, j_sp, k_sp, a_sp, b_sp, c_sp)
            g_int = g_bbb(c_sp, b_sp, a_sp, k_sp, j_sp, i_sp)

        elseif (dod == 6) then
            g_int = g_aaa(c_sp, b_sp, a_sp, k_sp, j_sp, i_sp)

        elseif (dod == 2 .and. mod(i,2) + mod(j,2) + mod(k,2) == 1) then
            inds = (/i,j,k,a,b,c/)
            g_int = calc_perm_g_abb(g_abb, inds)

        elseif (dod == 4 .and. mod(i,2) + mod(j,2) + mod(k,2) == 2) then
            inds = (/i,j,k,a,b,c/)
            g_int = calc_perm_g_aab(g_aab, inds)

        elseif (mod(dod, 2) /= 0) then
            g_int = 0.0_p

        endif

        !if (i==1.and.j==1.and.k==6.and.a==1.and.b==1.and.c==6) call exit()

        !g_int = -g_int

        !g_int = 0.0_p

    end function get_g


    function slater0_eham(sys, occ_list) result(h_element)

        ! Calculate diagonal elements of the Hamiltonian.
        ! <D_I | H_N | D_I> = <D_I | H | D_I> - <D_0 | H | D_0>

        ! In:
        !    sys: system's information
        !    occ_list: occupied orbitals

        ! Out:
        !    h_element: Hamiltonian element

        use const, only: p
        use system, only: sys_t

        real(p) :: h_element
        type(sys_t), intent(in) :: sys
        integer, intent(in) :: occ_list(:)

        integer :: i, j
        integer :: iel, jel

        h_element = 0.0_p

        ! Calculate diagonal elements
        do iel=1, sys%nel
            i = occ_list(iel)
            h_element = h_element + get_z(sys%ints%e1int, sys%ints%e1int, i, i)

            !do jel=iel+1, sys%nel
            do jel=1, sys%nel
                j = occ_list(jel)
                h_element = h_element + get_v(sys%ints%v_aa, sys%ints%v_ab, sys%ints%v_bb, &
                    i, j, i, j) / 2.0_p
            enddo

        enddo

        h_element = h_element - (sys%en_ref-sys%en_repul)

    end function slater0_eham


    function slater1_eham(sys, occ_list, i, a, perm) result(h_element)

        ! Calculate elements of the Hamiltonian with one difference
        ! <D_I | H_N | D_J> = <D_I | H_N * E_a^i | D_I>

        ! In:
        !    sys: system's information
        !    occ_list: occupied orbitals
        !    i, a: from (i) and to (a) orbitals of the single excitation
        !    perm: permutation parity

        ! Out:
        !    h_element: Hamiltonian element

        use const, only: p
        use system, only: sys_t

        real(p) :: h_element
        type(sys_t), intent(in) :: sys
        integer, intent(in) :: occ_list(:)
        integer, intent(in) :: i, a
        logical, intent(in) :: perm

        integer :: iel

        h_element = get_z(sys%ints%e1int, sys%ints%e1int, i, a)

        do iel=1, sys%nel
            h_element = h_element + get_v(sys%ints%v_aa, sys%ints%v_ab, sys%ints%v_bb, &
                i, occ_list(iel), a, occ_list(iel))
        enddo

        if (perm) h_element = -h_element

    end function slater1_eham

    ! ---- Similarity transformed Hamiltonian barH_N ----

    function slater0_simham(sys, hbar, occ_list, unocc_list) result(h_element)

        ! Calculate elements of the similarity transformed Hamiltonian

        ! In:
        !    sys: system's information
        !    occ_list: occupied orbitals

        ! Out:
        !    h_element: Hamiltonian element

        use const, only: p
        use system, only: sys_t
        use cc_types, only: hbar_t

        real(p) :: h_element
        type(sys_t), intent(in) :: sys
        type(hbar_t), intent(in) :: hbar
        integer, intent(in) :: occ_list(:), unocc_list(:)


        integer :: i, j, k
        integer :: iel, jel, kel

        ! [TMPDEBUG]
        real(p) :: z_val, v_val, g_val


        ! Hold matrix element value
        h_element = 0.0_p


        do iel=1, sys%nel
            i = occ_list(iel)
            ! <i|z|i>
            z_val =  get_z(hbar%a, hbar%b, i, i)
            h_element = h_element + z_val
            if (z_val /= 0.0_p) print '(i4,es24.10)', i, z_val

            !do jel=iel+1, sys%nel
            do jel=1, sys%nel
                j = occ_list(jel)
                ! <ij|v|ij>
                v_val = get_v(hbar%aa, hbar%ab, hbar%bb, i, j, i, j)
                h_element = h_element + get_v(hbar%aa, hbar%ab, hbar%bb, i, j, i, j) / 2.0_p
                if (v_val /= 0.0_p) print '(2i4,es24.10)', i, j, v_val

                do kel=1, sys%nvirt
                    k = unocc_list(kel)
                    ! <ijk|g|ijk>
                    g_val = get_g(hbar%aaa, hbar%aab, hbar%abb, hbar%bbb, &
                        i, j, k, i, j, k) / 4.0_p
                    h_element = h_element + g_val
                    if (g_val /= 0.0_p) print '(3i4,es24.10)', i,j,k, g_val
                enddo

            enddo

        enddo

        !h_element = h_element

    end function slater0_simham


    function slater1_simham(sys, hbar, occ_list, unocc_list, i, a, perm) result(h_element)

        ! Calculate elements of the similarity transformed Hamiltonian with one
        ! difference.
        ! <D_I | simH_N | D_J> = <D_I | simH_N * E_a^i | D_I>

        ! In:
        !    sys: system's information
        !    occ_list: occupied orbitals
        !    i, a: from (i) and to (a) orbitals of the single excitation
        !    perm: permutation parity

        ! Out:
        !    h_element: Hamiltonian element

        use const, only: p
        use system, only: sys_t
        use cc_types, only: hbar_t

        real(p) :: h_element
        type(sys_t), intent(in) :: sys
        type(hbar_t), intent(in) :: hbar
        integer, intent(in) :: occ_list(:), unocc_list(:)
        integer, intent(in) :: i, a
        logical, intent(in) :: perm

        integer :: jel, kel


        ! <i|z|a>
        h_element = get_z(hbar%a, hbar%b, i, a)

        do jel=1, sys%nel
            ! <ij|v|aj>
            h_element = h_element + get_v(hbar%aa, hbar%ab, hbar%bb, &
                i, occ_list(jel), a, occ_list(jel))

            do kel=1, sys%nvirt
                ! <ijk|g|ajk>
                h_element = h_element + get_g(hbar%aaa, hbar%aab, hbar%abb, hbar%bbb, &
                    i, occ_list(jel), unocc_list(kel), &
                    a, occ_list(jel), unocc_list(kel)) / 2.0_p
            enddo

        enddo

        if (perm) h_element = -h_element

    end function slater1_simham


    function slater2_simham(sys, hbar, occ_list, unocc_list, i, j, a, b, perm) result(h_element)

        ! Calculate elements of the similarity transformed Hamiltonian with two
        ! difference.
        ! <D_I | simH_N | D_J> = <D_I | simH_N * E_a^i E_b^j | D_I>

        ! In:
        !    sys: system's information
        !    occ_list: occupied orbitals
        !    i, j, a, b: from (ij) and to (ab) orbitals of the single excitation
        !    perm: permutation parity

        ! Out:
        !    h_element: Hamiltonian element

        use const, only: p
        use system, only: sys_t
        use cc_types, only: hbar_t

        real(p) :: h_element
        type(sys_t), intent(in) :: sys
        type(hbar_t), intent(in) :: hbar
        integer, intent(in) :: occ_list(:), unocc_list(:)
        integer, intent(in) :: i, j, a, b
        logical, intent(in) :: perm

        integer :: kel

        ! <ij|v|ab>
        h_element = get_v(hbar%aa, hbar%ab, hbar%bb, i, j, a, b)

        do kel=1, sys%nvirt
            ! <ijk|g|abk>
            h_element = h_element + get_g(hbar%aaa, hbar%aab, hbar%abb, hbar%bbb, &
                i, j, unocc_list(kel), &
                a, b, unocc_list(kel))
        enddo

        if (perm) h_element = -h_element

    end function slater2_simham


    function slater3_simham(sys, hbar, i, j, k, a, b, c, perm) result(h_element)

        ! Calculate elements of the similarity transformed Hamiltonian with three
        ! difference.
        ! <D_I | simH_N | D_J> = <D_I | simH_N * E_a^i E_b^j E_c^k | D_I>

        ! In:
        !    sys: system's information
        !    occ_list: occupied orbitals
        !    i, a: from (i) and to (a) orbitals of the single excitation
        !    perm: permutation parity

        ! Out:
        !    h_element: Hamiltonian element

        use const, only: p
        use system, only: sys_t
        use cc_types, only: hbar_t

        real(p) :: h_element
        type(sys_t), intent(in) :: sys
        type(hbar_t), intent(in) :: hbar
        integer, intent(in) :: i, j, k, a, b, c
        logical, intent(in) :: perm

        integer :: iel, jel

        ! <ijk|g|abc>
        h_element = get_g(hbar%aaa, hbar%aab, hbar%abb, hbar%bbb, &
            i, j, k, a, b, c)

        if (perm) h_element = -h_element

    end function slater3_simham


    function calc_perm_g(g, inds) result(val)
    !pure function calc_perm_g(g, inds) result(val)

        ! This is only temporary. Not optimized.

        use const, only: p

        real(p) :: val
        real(p), intent(in) :: g(:,:,:,:,:,:)
        integer, intent(in) :: inds(6)

        logical :: perm
        integer :: indx, cnt
        integer :: diff, tmp
        integer :: ex_1, ex_2
        integer :: inds_tmp(6)

        ! [TODO] optimize this

        ex_1 = 0
        ex_2 = 0
        cnt = 0
        do indx=1, 3
            diff = mod(inds(indx),2 ) - mod(inds(indx + 3), 2)
            cnt = cnt + abs(diff)
            if (ex_1 == 0 .and. diff /= 0) ex_1 = indx + 3
            if (ex_2 == 0 .and. diff /= 0) ex_2 = indx + 3
        enddo

        cnt = cnt/2

        inds_tmp = inds
        if (ex_1 /= 0 .and. ex_2 /= 0) then
            inds_tmp(ex_1) = inds(ex_2)
            inds_tmp(ex_2) = inds(ex_1)
        endif

        do indx=1, 6
            inds_tmp(indx) = int((inds_tmp(indx) + 1) / 2)
        enddo

        perm = (-1)**cnt == -1
        val = g(inds_tmp(1), inds_tmp(2), inds_tmp(3), inds_tmp(4), inds_tmp(5), inds_tmp(6))
        if (perm) val = -val

    end function calc_perm_g

    pure function calc_perm_g_abb(g, inds) result(val)

        ! This is only temporary. Not optimized.

        use const, only: p

        real(p) :: val
        real(p), intent(in) :: g(:,:,:,:,:,:)
        integer, intent(in) :: inds(6)

        integer :: i, perm
        integer :: bra_indx, ket_indx
        integer :: inds_tmp(6)

        perm = 1
        ! Find alpha in bra
        do i=1, 3
            if (mod(inds(i), 2) == 1) then
                ket_indx = i
            endif
            inds_tmp(i) = inds(i)
        enddo

        ! If in another position that 1 swap and keep parity count
        if (ket_indx /= 1) then
            inds_tmp(1) = inds(ket_indx)
            inds_tmp(ket_indx) = inds(1)
            perm = -1
        endif


        ! Find alpha in ket
        do i=4, 6
            if (mod(inds(i), 2) == 1) then
                bra_indx = i
            endif
            inds_tmp(i) = inds(i)
        enddo

        ! If in another position that 4 swap and keep parity count
        if (bra_indx /= 4) then
            inds_tmp(4) = inds(bra_indx)
            inds_tmp(bra_indx) = inds(4)
            perm = -perm
        endif

        ! Convert to spatial orbitals
        do i=1, 6
            inds_tmp(i) = int((inds_tmp(i) + 1) / 2)
        enddo

        val = perm * g(inds_tmp(6), inds_tmp(5), inds_tmp(4), inds_tmp(3), inds_tmp(2), inds_tmp(1))

    end function calc_perm_g_abb

    pure function calc_perm_g_aab(g, inds) result(val)

        ! This is only temporary. Not optimized.

        use const, only: p

        real(p) :: val
        real(p), intent(in) :: g(:,:,:,:,:,:)
        integer, intent(in) :: inds(6)

        integer :: i, perm
        integer :: bra_indx, ket_indx
        integer :: inds_tmp(6)

        perm = 1
        ! Find beta in bra
        do i=1, 3
            if (mod(inds(i), 2) == 0) then
                ket_indx = i
            endif
            inds_tmp(i) = inds(i)
        enddo

        ! If in another position that 1 swap and keep parity count
        if (ket_indx /= 3) then
            inds_tmp(3) = inds(ket_indx)
            inds_tmp(ket_indx) = inds(3)
            perm = -1
        endif


        ! Find alpha in ket
        do i=4, 6
            if (mod(inds(i), 2) == 0) then
                bra_indx = i
            endif
            inds_tmp(i) = inds(i)
        enddo

        ! If in another position that 4 swap and keep parity count
        if (bra_indx /= 6) then
            inds_tmp(6) = inds(bra_indx)
            inds_tmp(bra_indx) = inds(6)
            perm = -perm
        endif

        ! Convert to spatial orbitals
        do i=1, 6
            inds_tmp(i) = int((inds_tmp(i) + 1) / 2)
        enddo

        val = perm * g(inds_tmp(6), inds_tmp(5), inds_tmp(4), inds_tmp(3), inds_tmp(2), inds_tmp(1))

    end function calc_perm_g_aab

end module hmat
