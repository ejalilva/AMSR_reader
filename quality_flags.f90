module quality_flags
    implicit none

    integer, parameter :: PRECIP_BIT = 1
    integer, parameter :: SNOW_BIT = 2
    integer, parameter :: FROZEN_BIT = 3

    ! Define band indices for clarity
    integer, parameter :: TB10V = 1, TB10H = 2, TB19V = 3, TB19H = 4, TB37V = 7, TB37H = 8

contains
    function check_precipitation(tb6v, tb6h, tb11v) result(is_precip)
        real, intent(in) :: tb6v, tb6h, tb11v
        logical :: is_precip
        real :: pol6, grad

        if (tb6v /= 0.0) then
            pol6 = (tb6v - tb6h) / tb6v
        else
            pol6 = 0.0
        endif
        grad = (tb6v - tb11v) / tb6v

        is_precip = (pol6 > 0.2) .and. (grad > 0.05)
    end function

    function check_snow(tb19v, tb19h, tb37v, tb37h) result(is_snow)
        real, intent(in) :: tb19v, tb19h, tb37v, tb37h
        logical :: is_snow
        real :: pol19, pol37, grad

        if (tb19v /= 0.0) then
            pol19 = (tb19v - tb19h) / tb19v
        else
            pol19 = 0.0
        endif
        if (tb37v /= 0.0) then
            pol37 = (tb37v - tb37h) / tb37v
        else
            pol37 = 0.0
        endif
        grad = (tb19v - tb37v) / tb19v

        is_snow = (pol37 < 0.1) .and. (grad > 0.01) .and. (pol19 > pol37)
    end function

    function check_frozen_ground(tb10v, tb10h, tb19v, tb36v) result(is_frozen)
        real, intent(in) :: tb10v, tb10h, tb19v, tb36v
        logical :: is_frozen
        real :: pol10

        if ((tb10v + tb10h) /= 0.0) then
            pol10 = (tb10v - tb10h) / (tb10v + tb10h)
        else
            pol10 = 0.0
        endif
        is_frozen = (pol10 < 0.035) .and. (tb19v - tb36v > 8.0)
    end function

    subroutine set_quality_flags(tb_data, dims, qc_flags)
        real, intent(in) :: tb_data(:,:,:)
        integer, intent(in) :: dims(3)
        integer(1), intent(out) :: qc_flags(dims(1), dims(2))
        integer :: i, j

        qc_flags = 0

        do j = 1, dims(2)
            do i = 1, dims(1)
                if (check_precipitation(tb_data(i,j,6), tb_data(i,j,7), tb_data(i,j,9))) then
                    qc_flags(i,j) = ibset(qc_flags(i,j), PRECIP_BIT-1)
                endif
                if (check_snow(tb_data(i,j,TB19V), tb_data(i,j,TB19H), &
                               tb_data(i,j,TB37V), tb_data(i,j,TB37H))) then
                    qc_flags(i,j) = ibset(qc_flags(i,j), SNOW_BIT-1)
                endif
                if (check_frozen_ground(tb_data(i,j,TB10V), tb_data(i,j,TB10H), &
                                        tb_data(i,j,TB19V), tb_data(i,j,TB37V))) then
                    qc_flags(i,j) = ibset(qc_flags(i,j), FROZEN_BIT-1)
                endif
            end do
        end do
    end subroutine

end module

