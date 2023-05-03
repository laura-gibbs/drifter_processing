program compute_currents_mean

  use iso_fortran_env, only : int32, real32
  use mod_drifter, only : wind2ekman

  implicit none

  integer(int32), parameter :: tile_size = 5.0
  integer(int32), parameter :: Ntiles_lon = 360.0 / tile_size
  integer(int32), parameter :: Ntiles_lat = 180.0 / tile_size

  real(real32), parameter :: flg = -1.9e19
  real(real32), parameter :: flg0p1 = flg * 0.1

 ! Input
  real(real32) :: ek_params(Ntiles_lon, Ntiles_lat, 2)
  real(real32), allocatable :: dvel(:, :)

 ! Output
  real(real32), allocatable :: dvel_ek(:, :)
  real(real32), allocatable  :: dvel_mn_geo(:, :)

 ! Local
  integer(int32) :: i, j
  integer(int32) :: i0, j0, Nobs
  character(len=128) :: path0, pin1, pin2, pout
 !----------------------------------------------------------

 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = 'data/drifters/'
  pout = 'data/drifters/'
 !----------------------------------------------------------

 !----------------------------------------------------------
  open(21, file = trim(pin1)//'ekman_params.dat', form = 'unformatted')
  read(21) ek_params
  close(21)
 !----------------------------------------------------------
 
 ! Find the Ekman parameters (first estimate)
 ! drift_vel(k, :) = [1.0 * n0, drec % lon, drec % lat, drec % ve, drec % vn]
 !----------------------------------------------------------
  open(21, file = trim(pin1)//'drifters_filtered_tiled.dat', form = 'unformatted')
  open(31, file = trim(pout)//'drifters_ekman_tiled.dat', form = 'unformatted')
  open(32, file = trim(pout)//'drifters_mean_geo_tiled.dat', form = 'unformatted')
  do i = 1, Ntiles_lon
    do j = 1, Ntiles_lat
      read(21) i0, j0, Nobs
      print *, i0, j0, Nobs
      write(31) i0, j0, Nobs
      write(32) i0, j0, Nobs
      if (Nobs > 0) then
        allocate(dvel(Nobs, 7))
        allocate(dvel_ek(Nobs, 5))
        allocate(dvel_mn_geo(Nobs, 5))
        read(21) dvel
        dvel_ek = dvel(:, 1:5)
        dvel_mn_geo= dvel(:, 1:5)
        dvel_ek(:, 4:5) = wind2ekman(dvel(:, 6:7), ek_params(i, j, :), dvel(:, 3))
        dvel_mn_geo(:, 4:5) = dvel(:, 4:5) - dvel_ek(:, 4:5)
        write(31) dvel_ek
        write(32) dvel_mn_geo
      end if
    end do
  end do
  close(21) 
  close(31) 
  close(32) 
 !----------------------------------------------------------

end program compute_currents_mean
