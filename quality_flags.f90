module quality_flags
    implicit none
    
    ! Define bit positions
    integer, parameter :: PRECIP_BIT = 1
    integer, parameter :: SNOW_BIT = 2  
    integer, parameter :: FROZEN_BIT = 3

contains

    function check_precipitation(tb36v, tb36h, tb89v) result(is_precip)
        real, intent(in) :: tb36v, tb36h, tb89v
        logical :: is_precip
        
        ! Simple precipitation test based on brightness temperatures
        is_precip = (tb36v > 250.0) .and. (tb36h > 240.0) .and. (tb89v > 270.0)
    end function
    
    subroutine set_quality_flags(tb_data, dims, qc_flags)
        real, intent(in) :: tb_data(:,:,:)  ! 3D array of brightness temperatures
        integer, intent(in) :: dims(3)
        integer(1), intent(out) :: qc_flags(dims(1), dims(2))
        integer :: i, j
        
        qc_flags = 0  ! Initialize flags
        
        do j = 1, dims(2)
            do i = 1, dims(1)
                ! Check precipitation
                if (check_precipitation(tb_data(i,j,6), tb_data(i,j,7), tb_data(i,j,9))) then
                    qc_flags(i,j) = ibset(qc_flags(i,j), PRECIP_BIT-1)
                endif
            end do
        end do
    end subroutine

end module
