module proc_pointers

    implicit none

    abstract interface
        subroutine update_clusters(sys, run, cc)

            use system, only: sys_t, run_t, cc_t

            type(sys_t), intent(in), target :: sys
            type(run_t), intent(in) :: run
            type(cc_t), intent(inout) :: cc
        endsubroutine update_clusters
    end interface

    procedure(update_clusters), pointer :: update_clusters_ptr => null()
end module proc_pointers
