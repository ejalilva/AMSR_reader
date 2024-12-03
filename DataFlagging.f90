module DataFlagging
    implicit none
    private
    public :: flag_data

    ! Define constants for flag codes (bit positions)
    integer, parameter :: FLAG_SNOW = 1            ! 1st bit
    integer, parameter :: FLAG_FROZEN_GROUND = 2  ! 2nd bit
    integer, parameter :: FLAG_OCEAN = 4          ! 3rd bit
    integer, parameter :: FLAG_PRECIPITATION = 8  ! 4th bit

contains

    ! Main subroutine to apply all flags
    subroutine flag_data(tb_data, channels, flag_array)
        real, intent(in) :: tb_data(:,:,:)
        character(len=*), intent(in) :: channels(:)
        integer, intent(out) :: flag_array(:,:)
        integer :: i, j, snow_band, frozen_band, ocean_band, precip_band

        ! Determine band indices based on channel names
        snow_band = find_channel_index(channels, "Brightness Temperature (res10,89.0GHz,V)")
        frozen_band = find_channel_index(channels, "Brightness Temperature (res10,37.0GHz,H)")
        ocean_band = find_channel_index(channels, "Brightness Temperature (res10,10.7GHz,V)")
        precip_band = find_channel_index(channels, "Brightness Temperature (res10,23.8GHz,H)")

        ! Initialize the flag array
        flag_array = 0

        ! Loop through each pixel
        do i = 1, size(flag_array, 1)
            do j = 1, size(flag_array, 2)

                ! Call individual flagging subroutines for each condition
                call flag_snow(tb_data, snow_band, i, j, flag_array(i, j))
                call flag_frozen_ground(tb_data, frozen_band, i, j, flag_array(i, j))
                call flag_ocean(tb_data, ocean_band, i, j, flag_array(i, j))
                call flag_precipitation(tb_data, precip_band, i, j, flag_array(i, j))

            end do
        end do
    end subroutine flag_data

    ! Subroutine to find the index of a channel
    integer function find_channel_index(channels, target_channel)
        character(len=*), intent(in) :: channels(:)
        character(len=*), intent(in) :: target_channel
        integer :: k

        find_channel_index = -1
        do k = 1, size(channels)
            if (trim(channels(k)) == trim(target_channel)) then
                find_channel_index = k
                exit
            end if
        end do
    end function find_channel_index

    ! Flag for snow using the specified band index
    subroutine flag_snow(tb_data, band, i, j, flag)
        real, intent(in) :: tb_data(:,:,:)
        integer, intent(in) :: band, i, j
        integer, intent(inout) :: flag

        if (band > 0 .and. tb_data(i, j, band) > 200.0) then
            flag = ior(flag, FLAG_SNOW)
        end if
    end subroutine flag_snow

    ! Flag for frozen ground using the specified band index
    subroutine flag_frozen_ground(tb_data, band, i, j, flag)
        real, intent(in) :: tb_data(:,:,:)
        integer, intent(in) :: band, i, j
        integer, intent(inout) :: flag

        if (band > 0 .and. tb_data(i, j, band) < 250.0) then
            flag = ior(flag, FLAG_FROZEN_GROUND)
        end if
    end subroutine flag_frozen_ground

    ! Flag for ocean using the specified band index
    subroutine flag_ocean(tb_data, band, i, j, flag)
        real, intent(in) :: tb_data(:,:,:)
        integer, intent(in) :: band, i, j
        integer, intent(inout) :: flag

        if (band > 0 .and. tb_data(i, j, band) < 150.0) then
            flag = ior(flag, FLAG_OCEAN)
        end if
    end subroutine flag_ocean

    ! Flag for precipitation using the specified band index
    subroutine flag_precipitation(tb_data, band, i, j, flag)
        real, intent(in) :: tb_data(:,:,:)
        integer, intent(in) :: band, i, j
        integer, intent(inout) :: flag

        ! Example: Detect large differences between H and V polarization
        if (band > 0 .and. abs(tb_data(i, j, band) - tb_data(i, j, band + 1)) > 20.0) then
            flag = ior(flag, FLAG_PRECIPITATION)
        end if
    end subroutine flag_precipitation

end module DataFlagging

