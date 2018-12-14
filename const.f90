module const

    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)


    integer, parameter :: log_unit = 140
    integer, parameter :: config_unit = 141
    integer, parameter :: tmp_unit = 142
    integer, parameter :: cnf_unit = 147
    integer, parameter :: t_unit = 389
    integer, parameter :: t_vecs_unit = 390
    integer, parameter :: l_vecs_unit = 433
    integer, parameter :: l_unit = 482
    integer, parameter :: hbar_unit = 488
    integer, parameter :: end_unit = 489

    ! Adapt for Ilias old scheme
    integer, parameter :: ta = 29
    integer, parameter :: tb = 30
    integer, parameter :: tc = 31
    integer, parameter :: td = 32
    integer, parameter :: te = 33

end module const
