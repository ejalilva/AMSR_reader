program read_amsr
use netcdf
use hdf5
implicit none

integer :: ncid, error, x_dimid, y_dimid
integer(hid_t) :: file_id, dataset_id, dataspace_id
real, allocatable :: data(:,:,:), lat89(:,:), lon89(:,:), lat(:,:), lon(:,:)
integer(hsize_t), dimension(2) :: dims, dims89
integer :: i
character(len=256) :: filename
character(len=100) :: channels(10)
integer, dimension(12) :: varids

channels = (/"Brightness Temperature (res10,10.7GHz,H)", &
           "Brightness Temperature (res10,10.7GHz,V)", &
           "Brightness Temperature (res10,18.7GHz,H)", &
           "Brightness Temperature (res10,18.7GHz,V)", &
           "Brightness Temperature (res10,23.8GHz,H)", &
           "Brightness Temperature (res10,23.8GHz,V)", &
           "Brightness Temperature (res10,36.5GHz,H)", &
           "Brightness Temperature (res10,36.5GHz,V)", &
           "Brightness Temperature (res10,89.0GHz,H)", &
           "Brightness Temperature (res10,89.0GHz,V)"/)

! Initialize HDF5 and open file
call h5open_f(error)
call get_command_argument(1, filename)
call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)

! Get dimensions for res10 data
call h5dopen_f(file_id, trim(channels(1)), dataset_id, error)
call h5dget_space_f(dataset_id, dataspace_id, error)
call h5sget_simple_extent_dims_f(dataspace_id, dims, dims, error)
call h5dclose_f(dataset_id, error)

! Get dimensions for 89A data
call h5dopen_f(file_id, "Latitude of Observation Point for 89A", dataset_id, error)
call h5dget_space_f(dataset_id, dataspace_id, error)
call h5sget_simple_extent_dims_f(dataspace_id, dims89, dims89, error)
call h5dclose_f(dataset_id, error)

! Allocate arrays
allocate(data(dims(1), dims(2), 10))
allocate(lat89(dims89(1), dims89(2)), lon89(dims89(1), dims89(2)))
allocate(lat(dims(1), dims(2)), lon(dims(1), dims(2)))

! Read lat/lon
call h5dopen_f(file_id, "Latitude of Observation Point for 89A", dataset_id, error)
call h5dread_f(dataset_id, H5T_NATIVE_REAL, lat89, dims89, error)
call h5dclose_f(dataset_id, error)

call h5dopen_f(file_id, "Longitude of Observation Point for 89A", dataset_id, error)
call h5dread_f(dataset_id, H5T_NATIVE_REAL, lon89, dims89, error)
call h5dclose_f(dataset_id, error)

! Resample lat/lon using zoom
call zoom_2d(lat89, dims89, lat, dims)
call zoom_2d(lon89, dims89, lon, dims)

! Read all channels
do i = 1, 10
   call h5dopen_f(file_id, trim(channels(i)), dataset_id, error)
   call h5dread_f(dataset_id, H5T_NATIVE_REAL, data(:,:,i), dims, error)
   call h5dclose_f(dataset_id, error)
end do

call h5fclose_f(file_id, error)
call h5close_f(error)

! Create NetCDF file
call check(nf90_create('output.nc', NF90_CLOBBER, ncid))

! Define dimensions
call check(nf90_def_dim(ncid, "x", int(dims(1)), x_dimid))
call check(nf90_def_dim(ncid, "y", int(dims(2)), y_dimid))

! Define variables
do i = 1, 10
   call check(nf90_def_var(ncid, trim(channels(i)), NF90_FLOAT, (/x_dimid,y_dimid/), varids(i)))
end do
call check(nf90_def_var(ncid, "lat", NF90_FLOAT, (/x_dimid,y_dimid/), varids(11)))
call check(nf90_def_var(ncid, "lon", NF90_FLOAT, (/x_dimid,y_dimid/), varids(12)))

call check(nf90_enddef(ncid))

! Write data
call check(nf90_put_var(ncid, varids(11), lat))
call check(nf90_put_var(ncid, varids(12), lon))
do i = 1, 10
   call check(nf90_put_var(ncid, varids(i), data(:,:,i)))
end do

call check(nf90_close(ncid))

deallocate(data, lat89, lon89, lat, lon)

contains

subroutine zoom_2d(input, dims_in, output, dims_out) 
   implicit none
   integer(hsize_t), intent(in) :: dims_in(2), dims_out(2)
   real, intent(in) :: input(dims_in(1), dims_in(2))
   real, intent(out) :: output(dims_out(1), dims_out(2))
   
   real :: x_scale, y_scale, x, y
   integer :: i, j, x1, x2, y1, y2
   real :: dx, dy
   real :: c11, c12, c21, c22
   real :: f1, f2
   
   ! Compute scaling factors
   x_scale = real(dims_in(1) - 1) / real(dims_out(1) - 1)
   y_scale = real(dims_in(2) - 1) / real(dims_out(2) - 1)
   
   do j = 1, dims_out(2)
       do i = 1, dims_out(1)
           ! Get input coordinates
           x = 1.0 + (i-1) * x_scale
           y = 1.0 + (j-1) * y_scale
           
           ! Get surrounding points
           x1 = int(x)
           x2 = min(x1 + 1, int(dims_in(1)))
           y1 = int(y)
           y2 = min(y1 + 1, int(dims_in(2)))
           
           ! Get interpolation weights
           dx = x - x1
           dy = y - y1
           
           ! Get corner values
           c11 = input(x1, y1)
           c12 = input(x1, y2)
           c21 = input(x2, y1)
           c22 = input(x2, y2)
           
           ! Bilinear interpolation
           f1 = (1.0-dx)*c11 + dx*c21
           f2 = (1.0-dx)*c12 + dx*c22
           output(i,j) = (1.0-dy)*f1 + dy*f2
       end do
   end do
   
end subroutine

subroutine check(status)
   integer, intent(in) :: status
   if (status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop "Stopped"
   end if
end subroutine check

end program

