program find_ekman_params

  use iso_fortran_env, only : int32, real32
  use mod_drifter, only : ekman_params_tile

  implicit none

  integer(int32), parameter :: tile_size = 5.0
  integer(int32), parameter :: Ntiles_lon = 360.0 / tile_size
  integer(int32), parameter :: Ntiles_lat = 180.0 / tile_size

  integer(int32), parameter :: Nobs_min = 1000

  integer(int32) :: i, j, i0, j0
  integer(int32) ::  Nobs
  character(len=128) :: path0, pin1, pin2, pout
  real(real32), allocatable :: dvel(:, :)
  real(real32), allocatable :: wind(:, :)
  real(real32) :: ek_params_0(Ntiles_lon, Ntiles_lat, 2)
 !----------------------------------------------------------

 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = 'data/drifters/'
  pin2 = 'data/corrections/'
  pout = 'data/drifters/'
 !----------------------------------------------------------

 ! Find the Ekman parameters (first estimate)
 ! drift_vel(k, :) = [1.0 * n0, drec % lon, drec % lat, drec % ve, drec % vn]
 !----------------------------------------------------------
  open(21, file = trim(pin1)//'drifters_filtered_tiled.dat', form = 'unformatted')
  do i = 1, Ntiles_lon
    do j = 1, Ntiles_lat
      read(21) i0, j0, Nobs
      print *, i0, j0, Nobs
      if (Nobs > 0) then
        allocate(dvel(1:Nobs, 7))
        read(21) dvel
      end if
      if (Nobs >= Nobs_min) then
        !ek_params_0(i, j, :) = ekman_params_tile(dvel)
      end if
      if (allocated(dvel)) deallocate(dvel)
    end do
  end do
  close(21) 
 !----------------------------------------------------------

 !----------------------------------------------------------
  open(31, file = trim(pout)//'ekman_params_0.dat', form = 'unformatted')
  write(31) ek_params_0
  close(31)
 !----------------------------------------------------------

end program find_ekman_params
