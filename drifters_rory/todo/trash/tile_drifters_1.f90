program tile_drifters

  use iso_fortran_env, only : int32, real32

  implicit none

  real(real32), parameter :: tile_size = 5.0

  integer(int32), parameter :: Ntiles_lon = 360.0 / tile_size
  integer(int32), parameter :: Ntiles_lat = 180.0 / tile_size

  integer(int32) :: i, j
  integer(int32) :: i0, j0
  integer(int32) ::  drft_cnt
  integer(int32) ::  id, Nlocs
  character(len=128) :: path0, pin1, pout
  real(real32), allocatable :: drift_vel(:, :)
  real(real32) :: tile_lon(Ntiles_lon)
  real(real32) :: tile_lat(Ntiles_lat)
  integer(int32) :: tile_cnt(Ntiles_lon, Ntiles_lat)
 !----------------------------------------------------------

 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = 'data/drifters/'
  pout = 'data/drifters/'
 !----------------------------------------------------------

 !----------------------------------------------------------
  tile_lon = [(tile_size * (i - 0.5), i = 1, Ntiles_lon)]
  tile_lat = [(tile_size * (j - 0.5) - 90.0, j = 1, Ntiles_lat)]
 !----------------------------------------------------------

 !       drift_vel(k, :) = [1.0 * n0, drec % lon, drec % lat, drec % ve, drec % vn]
 !----------------------------------------------------------
  tile_cnt = 0
  open(21, file = trim(pin1)//'drifters_filtered.dat', form = 'unformatted')

  read(21) drft_cnt

  do i = 1, drft_cnt

    print *, 'processing drifter', i, 'of', drft_cnt

    read(21) id, Nlocs

    allocate(drift_vel(Nlocs, 5))

    read(21)drift_vel

    do j = 1, Nlocs
      i0 = minloc(abs(tile_lon - drift_vel(j, 2)), 1)
      j0 = minloc(abs(tile_lat - drift_vel(j, 3)), 1)
      tile_cnt(i0, j0) = tile_cnt(i0, j0) + 1
    end do

    deallocate(drift_vel)

  end do
  close(21)
  print *, 'maximum drifter obs per tile:', maxval(maxval(tile_cnt, 2), 1)
 !----------------------------------------------------------

end program tile_drifters
